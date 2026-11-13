module Balatro.Bot.Shop

open System
open Balatro.Core
open Balatro.Core.Domain

type Build =
    { Jokers: Jokers.Joker list
      Levels: Map<HandType, HandLevel>
      Deck: Card list
      HandSize: int
      Hands: int
      Situation: Jokers.Situation }

type Offer =
    | JokerOffer of position: int * joker: Jokers.Joker * cost: int
    | PlanetOffer of position: int * hand: HandType * name: string * cost: int

type Evaluated =
    { Offer: Offer
      Before: float
      After: float
      InterestLost: int }

let private simulate (build: Build) (shuffle: Card[]) : float =
    let score handsLeftAfter discardsLeft played held =
        Scoring.scoreWith
            { build.Situation with
                Jokers = build.Jokers
                HandsLeft = handsLeftAfter
                DiscardsLeft = discardsLeft }
            { Played = played
              Held = held
              Levels = build.Levels }

    let draw (kept: Card list) taken =
        let count = min (build.HandSize - kept.Length) (shuffle.Length - taken)
        kept @ List.ofArray shuffle[taken .. taken + count - 1], taken + count

    let rec play (hand: Card list) taken handsLeft total =
        if handsLeft = 0 then
            total
        else
            let input: Search.Input =
                { Hand = List.indexed hand
                  Deck = []
                  HandsLeft = handsLeft
                  HandSize = build.HandSize
                  DiscardsLeft = build.Situation.DiscardsLeft
                  Needed = 0.0
                  Score = score
                  Seed = 0
                  RedrawSamples = 0
                  Rollouts = 0 }

            let positions, _, best = (Search.plays input).Head

            let kept =
                hand
                |> List.indexed
                |> List.filter (fun (i, _) -> not (List.contains i positions))
                |> List.map snd

            let next, taken = draw kept taken
            play next taken (handsLeft - 1) (total + best.Total)

    let hand, taken = draw [] 0
    play hand taken build.Hands 0.0

let roundScore (seed: int) (samples: int) (build: Build) : float =
    Array.init samples (fun s ->
        let cards = Array.ofList build.Deck
        Random(seed + s).Shuffle cards
        simulate build cards)
    |> Array.average

let private interest money = min 5 (money / 5)

let cost offer =
    match offer with
    | JokerOffer(_, _, cost)
    | PlanetOffer(_, _, _, cost) -> cost

let position offer =
    match offer with
    | JokerOffer(position, _, _)
    | PlanetOffer(position, _, _, _) -> position

let private apply (build: Build) offer =
    match offer with
    | JokerOffer(_, joker, _) -> { build with Jokers = build.Jokers @ [ joker ] }
    | PlanetOffer(_, hand, _, _) ->
        let one = handLevel hand 1
        let two = handLevel hand 2
        let level = build.Levels[hand]

        { build with
            Levels =
                build.Levels
                |> Map.add
                    hand
                    { Chips = level.Chips + two.Chips - one.Chips
                      Mult = level.Mult + two.Mult - one.Mult } }

let evaluate (seed: int) (samples: int) (money: int) (build: Build) (offers: Offer list) : Evaluated list =
    let before = roundScore seed samples build

    offers
    |> List.filter (fun offer ->
        cost offer <= money
        && match offer with
           | JokerOffer _ -> build.Jokers.Length < build.Situation.JokerSlots
           | PlanetOffer _ -> true)
    |> List.map (fun offer ->
        { Offer = offer
          Before = before
          After = roundScore seed samples (apply build offer)
          InterestLost = interest money - interest (money - cost offer) })
    |> List.sortBy (fun e -> -(e.After / e.Before), cost e.Offer, position e.Offer)

let choose (evaluated: Evaluated list) : Evaluated option =
    evaluated
    |> List.tryFind (fun e -> e.After / e.Before >= 1.05 + 0.05 * float e.InterestLost)

let rec private permutations (items: int list) : int list list =
    match items with
    | [] -> [ [] ]
    | _ ->
        items
        |> List.collect (fun x -> permutations (List.filter ((<>) x) items) |> List.map (fun rest -> x :: rest))

let bestOrder (seed: int) (samples: int) (build: Build) : int list =
    let current = [ 0 .. build.Jokers.Length - 1 ]

    if build.Jokers.Length > 5 then
        current
    else
        permutations current
        |> List.map (fun order ->
            order, roundScore seed samples { build with Jokers = order |> List.map (fun i -> build.Jokers[i]) })
        |> List.reduce (fun best next -> if snd next > snd best then next else best)
        |> fst
