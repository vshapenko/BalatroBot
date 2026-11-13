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

let rec private combos k start n =
    if k = 0 then
        [ [] ]
    else
        [ for i in start .. n - k do
              for rest in combos (k - 1) (i + 1) n -> i :: rest ]

// Every set of one to five positions of a hand of n cards, by size, then in order
let private subsetIndices n =
    [| for k in 1..5 do
           for c in combos k 0 n -> Array.ofList c |]

let private split (cards: Card[]) (subset: int[]) =
    let mutable played = []
    let mutable held = []
    let mutable j = subset.Length - 1

    for i in cards.Length - 1 .. -1 .. 0 do
        if j >= 0 && subset[j] = i then
            played <- cards[i] :: played
            j <- j - 1
        else
            held <- cards[i] :: held

    played, held

let private scored
    (score: int -> int -> Card list -> Card list -> ScoreBreakdown)
    (handsLeftAfter: int)
    (discardsLeft: int)
    (hand: (int * Card) list)
    =
    let positions = hand |> List.map fst |> Array.ofList
    let cards = hand |> List.map snd |> Array.ofList

    subsetIndices cards.Length
    |> Array.map (fun subset ->
        let played, held = split cards subset
        subset |> Array.map (fun i -> positions[i]) |> List.ofArray, played, score handsLeftAfter discardsLeft played held)
    |> List.ofArray

// The subset with the highest total, then the most cards, then the first in order; and that total
let private bestSubset
    (score: int -> int -> Card list -> Card list -> ScoreBreakdown)
    (handsLeftAfter: int)
    (discardsLeft: int)
    (cards: Card[])
    (subsets: int[][])
    =
    let mutable best = 0
    let mutable bestTotal = 0.0

    for s in 0 .. subsets.Length - 1 do
        let played, held = split cards subsets[s]
        let total = (score handsLeftAfter discardsLeft played held).Total

        if s = 0 || total > bestTotal || (total = bestTotal && subsets[s].Length > subsets[best].Length) then
            best <- s
            bestTotal <- total

    subsets[best], bestTotal

let private remaining (subset: int[]) (cards: Card[]) =
    [ for i in 0 .. cards.Length - 1 do
          if not (Array.contains i subset) then
              yield cards[i] ]

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

    let positions = input.Hand |> List.map fst |> Array.ofList
    let cards = input.Hand |> List.map snd |> Array.ofList
    let tables = Array.init (cards.Length + 1) subsetIndices

    let expected (subset: int[]) =
        let kept = remaining subset cards

        let totals =
            shuffles
            |> Array.map (fun shuffle ->
                let hand = kept @ (shuffle |> Array.truncate subset.Length |> List.ofArray) |> Array.ofList
                snd (bestSubset input.Score (input.HandsLeft - 1) (input.DiscardsLeft - 1) hand tables[hand.Length]))

        subset |> Array.map (fun i -> positions[i]) |> List.ofArray, Array.average totals

    subsetIndices cards.Length
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
            let cards = Array.ofList hand

            let chosen, total =
                bestSubset input.Score (input.HandsLeft - used - 1) discardsLeft cards (subsetIndices cards.Length)

            let next, taken = refill (remaining chosen cards) taken
            play next (chips + total) (used + 1) taken discardsLeft

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
