module Balatro.Bot.Oracle

open System.IO
open System.Text.Json.Nodes
open Balatro.Core
open Balatro.Core.Domain

type Trial =
    { Key: string
      Hand: string
      Predicted: float
      Actual: float }

let private symbol (card: Card) =
    let rank =
        match card.Rank with
        | Ten -> "T"
        | other -> (Rank.label other).Substring(0, 1)

    rank + (string card.Suit).Substring(0, 1)

let rec private subsets size (items: 'a list) =
    match size, items with
    | 0, _ -> [ [] ]
    | _, [] -> []
    | n, x :: rest -> (subsets (n - 1) rest |> List.map (fun s -> x :: s)) @ subsets n rest

let private bestPlay (g: Api.GameState) (seen: Set<HandType>) =
    let situation = Mapping.situation g
    let levels = Mapping.levels g

    let hand =
        g.Hand.Cards
        |> List.indexed
        |> List.choose (fun (i, c) -> Mapping.toCard c |> Option.map (fun card -> i, card))

    let scored =
        [ 1..5 ]
        |> List.collect (fun n -> subsets n hand)
        |> List.map (fun chosen ->
            let held = hand |> List.filter (fun h -> not (List.contains h chosen)) |> List.map snd
            let played = chosen |> List.map snd
            chosen |> List.map fst, played, Scoring.scoreWith situation { Played = played; Held = held; Levels = levels })

    let fresh = scored |> List.filter (fun (_, _, b) -> not (seen.Contains b.HandType))
    let pool = if fresh.IsEmpty then scored else fresh
    pool |> List.maxBy (fun (_, _, b) -> b.Total)

let runKey (client: Api.Client) (deck: Api.Deck) (stake: Api.Stake) (key: string) : Result<Trial list, Api.ApiError> =
    let rec loop (g: Api.GameState) seen trials =
        if g.State <> Api.State.SelectingHand || List.length trials = 4 then
            Ok(List.rev trials)
        else
            let indices, played, breakdown = bestPlay g seen

            Api.play client indices
            |> Result.bind (fun after ->
                let trial =
                    { Key = key
                      Hand =
                        sprintf "%s [%s]" (HandType.label breakdown.HandType) (played |> List.map symbol |> String.concat " ")
                      Predicted = breakdown.Total
                      // Round.Chips is cumulative for the blind, so one play scores the difference
                      Actual = float (after.Round.Chips - g.Round.Chips) }

                loop after (seen.Add breakdown.HandType) (trial :: trials))

    // A start sent while the game is in GAME_OVER fails, so go through the menu first
    Api.menu client
    |> Result.bind (fun _ -> Api.start client deck stake (Some "ORACLE"))
    // Ante 8 makes the blind unreachable (negative chips are refused), so the trial never ends early
    |> Result.bind (fun _ -> Api.set client [ "ante", 8 ])
    |> Result.bind (fun _ -> Api.select client)
    |> Result.bind (fun _ -> Api.add client key)
    |> Result.bind (fun g -> loop g Set.empty [])

let run (client: Api.Client) (deck: Api.Deck) (stake: Api.Stake) (keys: string list) (outDir: string) : Result<int * int, Api.ApiError> =
    Directory.CreateDirectory outDir |> ignore
    let path = Path.Combine(outDir, "oracle.jsonl")
    File.Delete path

    let record (t: Trial) =
        let exact = t.Predicted = t.Actual
        let o = JsonObject()
        o.Add("key", JsonValue.Create t.Key :> JsonNode)
        o.Add("hand", JsonValue.Create t.Hand :> JsonNode)
        o.Add("predicted", JsonValue.Create t.Predicted :> JsonNode)
        o.Add("actual", JsonValue.Create t.Actual :> JsonNode)
        o.Add("exact", JsonValue.Create exact :> JsonNode)
        File.AppendAllLines(path, [ o.ToJsonString() ])
        printfn "%s %s: predicted %.0f, actual %.0f, %s" t.Key t.Hand t.Predicted t.Actual (if exact then "exact" else "MISMATCH")
        exact

    let results =
        keys
        |> List.collect (fun key ->
            match runKey client deck stake key with
            | Ok trials -> trials |> List.map record
            | Error e ->
                printfn "%s: ERROR %A" key e
                [])

    Ok(results |> List.filter id |> List.length, results.Length)
