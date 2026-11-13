module Balatro.Bot.Runner

open System.Collections.Generic
open System.IO
open System.Text.Json
open System.Text.Json.Nodes

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
      Purchases: int
      Sales: int
      Rerolls: int
      Packs: int }

let winAnte = 8

let maxDecisions = 3000

// Beating the ante-8 boss sets G.GAME.won and raises the ante to 9; a won flag carried over from an earlier run is not trusted
let isWon (startWon: bool) (g: Api.GameState) =
    g.State <> Api.State.GameOver && g.AnteNum > winAnte && (startWon || g.Won)

let allHandDebuffed (g: Api.GameState) =
    not g.Hand.Cards.IsEmpty && g.Hand.Cards |> List.forall (fun c -> c.State.Debuff)

// the reply to a sale still shows the hand debuffed for about 0.1 s while Verdant Leaf is being lifted
let private settle client (after: Api.GameState) =
    let rec reread tries (g: Api.GameState) =
        if tries = 0 || g.State <> Api.State.SelectingHand || not (allHandDebuffed g) then
            Ok g
        else
            System.Threading.Thread.Sleep 100
            Api.gamestate client |> Result.bind (reread (tries - 1))

    reread 20 after

let oneLine (s: string) =
    s.Replace("\r\n", " ").Replace('\n', ' ').Replace('\r', ' ')

let outcomeText outcome =
    match outcome with
    | Won -> "won"
    | Lost -> "lost"
    | Aborted reason -> "aborted: " + reason

let context (seq: int) (g: Api.GameState) : string * string =
    let blinds = g.Blinds.Value
    let blind = [ blinds.Small; blinds.Big; blinds.Boss ] |> List.find (fun b -> b.Status = Api.BlindStatus.Current)
    let edition (c: Api.Card) = c.Modifier.Edition |> Option.map (sprintf "%A")

    let jokers = JsonArray()

    for c in g.Jokers.Cards do
        let j = JsonObject()
        j.Add("label", JsonValue.Create c.Label)
        j.Add("effect", JsonValue.Create c.Value.Effect)
        j.Add("edition", edition c |> Option.map (fun e -> JsonValue.Create e :> JsonNode) |> Option.toObj)
        j.Add("debuffed", JsonValue.Create c.State.Debuff)
        jokers.Add j

    let o = JsonObject()
    o.Add("kind", JsonValue.Create "context")
    o.Add("seq", JsonValue.Create seq)

    o.Add(
        "blind",
        JsonObject(
            [ KeyValuePair("name", JsonValue.Create blind.Name :> JsonNode)
              KeyValuePair("effect", JsonValue.Create blind.Effect :> JsonNode)
              KeyValuePair("score", JsonValue.Create blind.Score :> JsonNode) ]
        )
    )

    o.Add("jokers", jokers)

    let joker (c: Api.Card) =
        [ Some c.Label
          Some(oneLine c.Value.Effect)
          edition c
          (if c.State.Debuff then Some "debuffed" else None) ]
        |> List.choose id
        |> String.concat ", "
        |> sprintf "[%s]"

    let effect = if blind.Effect = "" then "" else ": " + blind.Effect

    let text =
        sprintf
            "#%d context: blind %s (target %d)%s; jokers: %s"
            seq
            blind.Name
            blind.Score
            effect
            (if g.Jokers.Cards.IsEmpty then "none" else g.Jokers.Cards |> List.map joker |> String.concat " ")

    o.ToJsonString(), text

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

    let finish outcome (g: Api.GameState) plays discards exact purchases sales rerolls packs =
        emit (JsonSerializer.Serialize {| kind = "result"; outcome = outcomeText outcome |}) ("outcome: " + outcomeText outcome)

        Ok
            { Outcome = outcome
              Won = (outcome = Won)
              Ante = g.AnteNum
              Round = g.RoundNum
              Plays = plays
              Discards = discards
              ExactPredictions = exact
              Mismatches = plays - exact
              Purchases = purchases
              Sales = sales
              Rerolls = rerolls
              Packs = packs }

    let rec loop startWon seq (g: Api.GameState) plays discards exact purchases sales rerolls packs =
        if isWon startWon g then
            finish Won g plays discards exact purchases sales rerolls packs
        elif g.State = Api.State.GameOver then
            finish Lost g plays discards exact purchases sales rerolls packs
        elif seq > maxDecisions then
            finish (Aborted(sprintf "%d decisions made" maxDecisions)) g plays discards exact purchases sales rerolls packs
        else
            try
                let action, decision = Policy.decide seq g
                emit (Decision.toJson decision) (Decision.toText decision)

                let next apiCall =
                    apiCall |> Result.bind (fun s -> loop startWon (seq + 1) s plays discards exact purchases sales rerolls packs)

                match action with
                | Policy.Stop -> finish (Aborted decision.Rule) g plays discards exact purchases sales rerolls packs
                | Policy.Play cards ->
                    Api.play client cards
                    |> Result.bind (fun after ->
                        // Round.Chips is cumulative for the blind, so one play scores the difference
                        let outcome: Decision.Outcome =
                            { Seq = seq
                              Predicted = decision.Predicted.Value
                              Actual = float (after.Round.Chips - g.Round.Chips) }

                        emit (Decision.outcomeToJson outcome) (Decision.outcomeToText outcome)

                        if outcome.Predicted <> outcome.Actual then
                            let json, text = context seq g
                            emit json text

                        let hit = if outcome.Predicted = outcome.Actual then 1 else 0
                        loop startWon (seq + 1) after (plays + 1) discards (exact + hit) purchases sales rerolls packs)
                | Policy.Discard cards ->
                    Api.discard client cards
                    |> Result.bind (fun after -> loop startWon (seq + 1) after plays (discards + 1) exact purchases sales rerolls packs)
                | Policy.SelectBlind -> next (Api.select client)
                | Policy.CashOut -> next (Api.cashOut client)
                | Policy.NextRound -> next (Api.nextRound client)
                | Policy.BuyCard index ->
                    Api.buy client (Api.BuyCard index)
                    |> Result.bind (fun after ->
                        loop startWon (seq + 1) after plays discards exact (purchases + 1) sales rerolls packs)
                | Policy.SellJoker index ->
                    Api.sell client (Api.SellJoker index)
                    |> Result.bind (fun after -> if g.State = Api.State.SelectingHand then settle client after else Ok after)
                    |> Result.bind (fun after ->
                        loop startWon (seq + 1) after plays discards exact purchases (sales + 1) rerolls packs)
                | Policy.Reroll ->
                    Api.reroll client
                    |> Result.bind (fun after ->
                        loop startWon (seq + 1) after plays discards exact purchases sales (rerolls + 1) packs)
                | Policy.BuyPack index ->
                    Api.buy client (Api.BuyPack index)
                    |> Result.bind (fun after ->
                        loop startWon (seq + 1) after plays discards exact purchases sales rerolls (packs + 1))
                | Policy.PickPack index -> next (Api.pack client (Api.PickCard(index, [])))
                | Policy.UsePlanet index -> next (Api.useConsumable client index [])
                | Policy.ReorderJokers order -> next (Api.rearrange client (Api.RearrangeJokers order))
                | Policy.SkipPack -> next (Api.pack client Api.SkipPack)
            with ex ->
                finish (Aborted(oneLine (ex.GetType().Name + ": " + ex.Message))) g plays discards exact purchases sales rerolls packs

    // A start sent while the game is in GAME_OVER fails, so go through the menu first
    Api.menu client
    |> Result.bind (fun _ -> Api.start client deck stake (Some seed))
    |> Result.bind (fun g -> loop g.Won 1 g 0 0 0 0 0 0 0)
