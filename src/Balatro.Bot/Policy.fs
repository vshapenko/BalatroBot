module Balatro.Bot.Policy

open Balatro.Core
open Balatro.Core.Domain
open Balatro.Bot.Decision

type Action =
    | Play of int list
    | SelectBlind
    | CashOut
    | NextRound
    | SkipPack
    | Stop

let rec private combos k xs =
    match k, xs with
    | 0, _ -> [ [] ]
    | _, [] -> []
    | _, x :: rest -> (combos (k - 1) rest |> List.map (fun c -> x :: c)) @ combos k rest

let private symbol (card: Card) =
    let rank =
        match card.Rank with
        | Ten -> "T"
        | other -> (Rank.label other).Substring(0, 1)

    rank + (string card.Suit).Substring(0, 1)

let private describe (breakdown: Scoring.ScoreBreakdown) (played: Card list) =
    sprintf "play %s [%s]" (HandType.label breakdown.HandType) (played |> List.map symbol |> String.concat " ")

let private decision seq (g: Api.GameState) chosen rule candidates predicted : Decision =
    { Seq = seq
      Ante = g.AnteNum
      Round = g.RoundNum
      Phase = string g.State
      Chosen = chosen
      Rule = rule
      Candidates = candidates
      Predicted = predicted }

let private passive seq (g: Api.GameState) action chosen rule =
    action, decision seq g chosen rule [] None

let private bestPlay seq (g: Api.GameState) =
    let levels = Mapping.levels g

    let hand =
        g.Hand.Cards
        |> List.indexed
        |> List.choose (fun (i, c) -> Mapping.toCard c |> Option.map (fun card -> i, card))

    let scored =
        [ 1..5 ]
        |> List.collect (fun k -> combos k hand)
        |> List.map (fun subset ->
            let played = subset |> List.map snd
            let held = hand |> List.filter (fun h -> not (List.contains h subset)) |> List.map snd
            let breakdown = Scoring.score { Played = played; Held = held; Levels = levels }
            subset |> List.map fst, played, breakdown)
        |> List.sortBy (fun (indices, _, b) -> -b.Total, indices.Length, indices)

    let candidates =
        scored
        |> List.truncate 5
        |> List.map (fun (indices, played, b) ->
            { Action = describe b played
              Value = b.Total
              Detail = sprintf "positions %A, %.0f chips x %.0f mult" indices b.Chips b.Mult })

    let indices, _, best = scored.Head

    Play indices, decision seq g candidates.Head.Action "highest immediate score" candidates (Some best.Total)

let decide (seq: int) (g: Api.GameState) : Action * Decision =
    match g.State with
    | Api.State.SelectingHand -> bestPlay seq g
    | Api.State.BlindSelect ->
        passive seq g SelectBlind "select blind" "skeleton policy only selects the blind in this phase"
    | Api.State.RoundEval -> passive seq g CashOut "cash out" "skeleton policy only cashes out in this phase"
    | Api.State.Shop -> passive seq g NextRound "next round" "skeleton policy leaves the shop without buying"
    | Api.State.SmodsBoosterOpened -> passive seq g SkipPack "skip pack" "skeleton policy skips every pack"
    | Api.State.GameOver -> passive seq g Stop "stop" "game is over"
    | other -> passive seq g Stop "stop" (sprintf "unexpected state %A, skeleton policy stops" other)
