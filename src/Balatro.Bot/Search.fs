module Balatro.Bot.Search

open System
open Balatro.Core.Domain
open Balatro.Core.Scoring

type Input =
    { Hand: (int * Card) list
      Deck: Card list
      HandsLeft: int
      HandSize: int
      DiscardsLeft: int
      Needed: float
      Score: int -> int -> Card list -> Card list -> ScoreBreakdown
      Seed: int
      RedrawSamples: int
      Rollouts: int }

type Candidate =
    | PlayCards of int list * Card list * ScoreBreakdown
    | DiscardCards of int list * float

type Outlook =
    { Clears: float
      MeanHands: float
      MeanChips: float }

let rec private combos k xs =
    match k, xs with
    | 0, _ -> [ [] ]
    | _, [] -> []
    | _, x :: rest -> (combos (k - 1) rest |> List.map (fun c -> x :: c)) @ combos k rest

let private subsets hand = [ 1..5 ] |> List.collect (fun k -> combos k hand)

let private scored
    (score: int -> int -> Card list -> Card list -> ScoreBreakdown)
    (handsLeftAfter: int)
    (discardsLeft: int)
    (hand: (int * Card) list)
    =
    subsets hand
    |> List.map (fun subset ->
        let played = subset |> List.map snd
        let held = hand |> List.filter (fun h -> not (List.contains h subset)) |> List.map snd
        subset |> List.map fst, played, score handsLeftAfter discardsLeft played held)

let private shuffles (input: Input) (offset: int) (count: int) =
    Array.init count (fun s ->
        let cards = Array.ofList input.Deck
        Random(input.Seed + offset + s).Shuffle cards
        cards)

let plays (input: Input) : (int list * Card list * ScoreBreakdown) list =
    scored input.Score (input.HandsLeft - 1) input.DiscardsLeft input.Hand
    |> List.sortBy (fun (indices, _, b) -> -b.Total, indices.Length, indices)

let discards (input: Input) : (int list * float) list =
    let shuffles = shuffles input 0 input.RedrawSamples

    let expected (subset: (int * Card) list) =
        let kept = input.Hand |> List.filter (fun h -> not (List.contains h subset)) |> List.map snd

        let totals =
            shuffles
            |> Array.map (fun shuffle ->
                let hand = kept @ (shuffle |> Array.truncate subset.Length |> List.ofArray) |> List.indexed
                scored input.Score (input.HandsLeft - 1) (input.DiscardsLeft - 1) hand |> List.map (fun (_, _, b) -> b.Total) |> List.max)

        subset |> List.map fst, Array.average totals

    subsets input.Hand
    |> Array.ofList
    |> Array.Parallel.map expected
    |> List.ofArray
    |> List.sortBy (fun (indices, value) -> -value, indices.Length, indices)

let private playCandidates (input: Input) =
    let all = plays input
    let rankSum = List.sumBy (fun (c: Card) -> Rank.id c.Rank)

    all
    |> List.map (fun (_, _, b) -> b.Total)
    |> List.distinct
    |> List.truncate 3
    |> List.collect (fun total ->
        let same = all |> List.filter (fun (_, _, b) -> b.Total = total)

        let padded =
            same
            |> List.sortBy (fun (indices, played, b) ->
                -indices.Length, rankSum played - rankSum b.ScoringCards, indices)

        [ same.Head; padded.Head ])
    |> List.distinctBy (fun (indices, _, _) -> indices)
    |> List.map PlayCards

let private rollout (input: Input) (shuffle: Card[]) (candidate: Candidate) : float * int =
    let without indices =
        input.Hand
        |> List.filter (fun (i, _) -> not (List.contains i indices))
        |> List.map snd

    let refill (kept: Card list) taken =
        let count = min (input.HandSize - kept.Length) (shuffle.Length - taken)
        kept @ List.ofArray shuffle[taken .. taken + count - 1], taken + count

    let rec play (hand: Card list) chips used taken discardsLeft =
        if used >= input.HandsLeft || chips >= input.Needed || hand.IsEmpty then
            chips, used
        else
            let indexed = List.indexed hand

            let indices, _, best =
                scored input.Score (input.HandsLeft - used - 1) discardsLeft indexed
                |> List.minBy (fun (indices, _, b) -> -b.Total, -indices.Length, indices)

            let kept =
                indexed
                |> List.filter (fun (i, _) -> not (List.contains i indices))
                |> List.map snd

            let next, taken = refill kept taken
            play next (chips + best.Total) (used + 1) taken discardsLeft

    let first, chips, used, discardsLeft =
        match candidate with
        | PlayCards(indices, _, b) -> indices, b.Total, 1, input.DiscardsLeft
        | DiscardCards(indices, _) -> indices, 0.0, 0, input.DiscardsLeft - 1

    let hand, taken = refill (without first) 0
    play hand chips used taken discardsLeft

let private outlook (input: Input) (shuffles: Card[][]) (candidate: Candidate) : Outlook =
    let results = shuffles |> Array.map (fun shuffle -> rollout input shuffle candidate)
    let cleared (chips, _) = chips >= input.Needed

    { Clears = results |> Array.averageBy (fun r -> if cleared r then 1.0 else 0.0)
      MeanHands = results |> Array.averageBy (fun ((_, used) as r) -> if cleared r then float used else float input.HandsLeft)
      MeanChips = results |> Array.averageBy fst }

let evaluate (input: Input) : (Candidate * Outlook) list =
    let rollouts = shuffles input input.RedrawSamples input.Rollouts

    let discardCandidates =
        if input.DiscardsLeft > 0 then
            discards input |> List.truncate 6 |> List.map DiscardCards
        else
            []

    let order (candidate, outlook: Outlook) =
        let kind, indices =
            match candidate with
            | PlayCards(indices, _, _) -> 0, indices
            | DiscardCards(indices, _) -> 1, indices

        -outlook.Clears, outlook.MeanHands, -outlook.MeanChips, kind, indices.Length, indices

    playCandidates input @ discardCandidates
    |> Array.ofList
    |> Array.Parallel.map (fun candidate -> candidate, outlook input rollouts candidate)
    |> List.ofArray
    |> List.sortBy order

let choose (input: Input) : (Candidate * Outlook) list * string =
    let indices, played, best = (plays input).Head

    if best.Total >= input.Needed then
        [ PlayCards(indices, played, best),
          { Clears = 1.0
            MeanHands = 1.0
            MeanChips = best.Total } ],
        "clears the blind now"
    else
        let evaluated = evaluate input

        evaluated,
        sprintf "highest simulated chance to clear: %.0f%% of %d rounds" ((snd evaluated.Head).Clears * 100.0) input.Rollouts
