module Balatro.Bot.Runner

open System.IO
open System.Text.Json

type Outcome =
    | Won
    | Lost
    | Aborted of string

type RunResult =
    { Outcome: Outcome
      Won: bool
      Ante: int
      Round: int
      Plays: int
      Discards: int
      ExactPredictions: int
      Mismatches: int
      Purchases: int }

let winAnte = 8

let maxDecisions = 3000

// Beating the ante-8 boss sets G.GAME.won and raises the ante to 9; a won flag carried over from an earlier run is not trusted
let isWon (startWon: bool) (g: Api.GameState) =
    g.State <> Api.State.GameOver && g.AnteNum > winAnte && (startWon || g.Won)

let outcomeText outcome =
    match outcome with
    | Won -> "won"
    | Lost -> "lost"
    | Aborted reason -> "aborted: " + reason

let runOne
    (client: Api.Client)
    (deck: Api.Deck)
    (stake: Api.Stake)
    (seed: string)
    (outDir: string)
    : Result<RunResult, Api.ApiError> =
    Directory.CreateDirectory outDir |> ignore
    File.Delete(Path.Combine(outDir, "trace.jsonl"))
    File.Delete(Path.Combine(outDir, "trace.txt"))

    let emit (json: string) (text: string) =
        File.AppendAllLines(Path.Combine(outDir, "trace.jsonl"), [ json ])
        File.AppendAllLines(Path.Combine(outDir, "trace.txt"), [ text ])
        printfn "%s" text

    let finish outcome (g: Api.GameState) plays discards exact purchases =
        emit (JsonSerializer.Serialize {| outcome = outcomeText outcome |}) ("outcome: " + outcomeText outcome)

        Ok
            { Outcome = outcome
              Won = (outcome = Won)
              Ante = g.AnteNum
              Round = g.RoundNum
              Plays = plays
              Discards = discards
              ExactPredictions = exact
              Mismatches = plays - exact
              Purchases = purchases }

    let rec loop startWon seq (g: Api.GameState) plays discards exact purchases =
        if isWon startWon g then
            finish Won g plays discards exact purchases
        elif g.State = Api.State.GameOver then
            finish Lost g plays discards exact purchases
        elif seq > maxDecisions then
            finish (Aborted(sprintf "%d decisions made" maxDecisions)) g plays discards exact purchases
        else
            try
                let action, decision = Policy.decide seq g
                emit (Decision.toJson decision) (Decision.toText decision)

                let next apiCall =
                    apiCall |> Result.bind (fun s -> loop startWon (seq + 1) s plays discards exact purchases)

                match action with
                | Policy.Stop -> finish (Aborted decision.Rule) g plays discards exact purchases
                | Policy.Play cards ->
                    Api.play client cards
                    |> Result.bind (fun after ->
                        // Round.Chips is cumulative for the blind, so one play scores the difference
                        let outcome: Decision.Outcome =
                            { Seq = seq
                              Predicted = decision.Predicted.Value
                              Actual = float (after.Round.Chips - g.Round.Chips) }

                        emit (Decision.outcomeToJson outcome) (Decision.outcomeToText outcome)
                        let hit = if outcome.Predicted = outcome.Actual then 1 else 0
                        loop startWon (seq + 1) after (plays + 1) discards (exact + hit) purchases)
                | Policy.Discard cards ->
                    Api.discard client cards
                    |> Result.bind (fun after -> loop startWon (seq + 1) after plays (discards + 1) exact purchases)
                | Policy.SelectBlind -> next (Api.select client)
                | Policy.CashOut -> next (Api.cashOut client)
                | Policy.NextRound -> next (Api.nextRound client)
                | Policy.BuyCard index ->
                    Api.buy client (Api.BuyCard index)
                    |> Result.bind (fun after -> loop startWon (seq + 1) after plays discards exact (purchases + 1))
                | Policy.UsePlanet index -> next (Api.useConsumable client index [])
                | Policy.ReorderJokers order -> next (Api.rearrange client (Api.RearrangeJokers order))
                | Policy.SkipPack -> next (Api.pack client Api.SkipPack)
            with ex ->
                finish (Aborted(ex.GetType().Name + ": " + ex.Message)) g plays discards exact purchases

    // A start sent while the game is in GAME_OVER fails, so go through the menu first
    Api.menu client
    |> Result.bind (fun _ -> Api.start client deck stake (Some seed))
    |> Result.bind (fun g -> loop g.Won 1 g 0 0 0 0)
