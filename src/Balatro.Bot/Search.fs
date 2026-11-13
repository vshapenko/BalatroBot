module Balatro.Bot.Search

open System
open Balatro.Core.Domain
open Balatro.Core.Scoring

type Input =
    { Hand: (int * Card) list
      Deck: Card list
      DiscardsLeft: int
      Needed: float
      Score: Card list -> Card list -> ScoreBreakdown
      Seed: int
      Samples: int }

type Choice =
    | PlayNow of int list * Card list * ScoreBreakdown
    | DiscardNow of int list * float

let rec private combos k xs =
    match k, xs with
    | 0, _ -> [ [] ]
    | _, [] -> []
    | _, x :: rest -> (combos (k - 1) rest |> List.map (fun c -> x :: c)) @ combos k rest

let private subsets hand = [ 1..5 ] |> List.collect (fun k -> combos k hand)

let private scored (score: Card list -> Card list -> ScoreBreakdown) (hand: (int * Card) list) =
    subsets hand
    |> List.map (fun subset ->
        let played = subset |> List.map snd
        let held = hand |> List.filter (fun h -> not (List.contains h subset)) |> List.map snd
        subset |> List.map fst, played, score played held)

let plays (input: Input) : (int list * Card list * ScoreBreakdown) list =
    scored input.Score input.Hand
    |> List.sortBy (fun (indices, _, b) -> -b.Total, indices.Length, indices)

let discards (input: Input) : (int list * float) list =
    let shuffles =
        Array.init input.Samples (fun s ->
            let cards = Array.ofList input.Deck
            Random(input.Seed + s).Shuffle cards
            cards)

    let expected (subset: (int * Card) list) =
        let kept = input.Hand |> List.filter (fun h -> not (List.contains h subset)) |> List.map snd

        let totals =
            shuffles
            |> Array.map (fun shuffle ->
                let hand = kept @ (shuffle |> Array.truncate subset.Length |> List.ofArray) |> List.indexed
                scored input.Score hand |> List.map (fun (_, _, b) -> b.Total) |> List.max)

        subset |> List.map fst, Array.average totals

    subsets input.Hand
    |> Array.ofList
    |> Array.Parallel.map expected
    |> List.ofArray
    |> List.sortBy (fun (indices, value) -> -value, indices.Length, indices)

let choose (input: Input) : Choice * string =
    let indices, played, best = (plays input).Head
    let playNow = PlayNow(indices, played, best)

    if best.Total >= input.Needed then
        playNow, "clears the blind now"
    elif input.DiscardsLeft > 0 then
        match discards input with
        | (discard, value) :: _ when value > best.Total ->
            DiscardNow(discard, value),
            sprintf "expected best hand after redraw %.0f beats best hand now %.0f" value best.Total
        | _ -> playNow, "no discard is expected to improve on the best hand"
    else
        playNow, "no discards left"
