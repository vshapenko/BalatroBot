module Balatro.Bot.Runner

open System.IO

type RunResult =
    { Won: bool
      Ante: int
      Round: int
      Plays: int
      Discards: int
      ExactPredictions: int }

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

    let finish (g: Api.GameState) plays discards exact =
        Ok
            { Won = g.Won
              Ante = g.AnteNum
              Round = g.RoundNum
              Plays = plays
              Discards = discards
              ExactPredictions = exact }

    let rec loop seq (g: Api.GameState) plays discards exact =
        if g.Won then
            finish g plays discards exact
        else
            let action, decision = Policy.decide seq g
            emit (Decision.toJson decision) (Decision.toText decision)
            let next apiCall = apiCall |> Result.bind (fun s -> loop (seq + 1) s plays discards exact)

            match action with
            | Policy.Stop -> finish g plays discards exact
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
                    loop (seq + 1) after (plays + 1) discards (exact + hit))
            | Policy.Discard cards ->
                Api.discard client cards
                |> Result.bind (fun after -> loop (seq + 1) after plays (discards + 1) exact)
            | Policy.SelectBlind -> next (Api.select client)
            | Policy.CashOut -> next (Api.cashOut client)
            | Policy.NextRound -> next (Api.nextRound client)
            | Policy.SkipPack -> next (Api.pack client Api.SkipPack)

    // A start sent while the game is in GAME_OVER fails, so go through the menu first
    Api.menu client
    |> Result.bind (fun _ -> Api.start client deck stake (Some seed))
    |> Result.bind (fun g -> loop 1 g 0 0 0)
