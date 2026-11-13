module Balatro.Bot.Shop

open System
open System.Globalization
open System.Text.RegularExpressions
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
    let score plays handsLeftAfter discardsLeft played held =
        Scoring.scoreWith
            { build.Situation with
                Jokers = build.Jokers
                HandsLeft = handsLeftAfter
                DiscardsLeft = discardsLeft
                HandPlays = plays }
            { Played = played
              Held = held
              Levels = build.Levels }

    let draw (kept: Card list) taken =
        let count = min (build.HandSize - kept.Length) (shuffle.Length - taken)
        kept @ List.ofArray shuffle[taken .. taken + count - 1], taken + count

    let rec play (hand: Card list) taken handsLeft plays total =
        if handsLeft = 0 then
            total
        else
            let input: Search.Input =
                { Hand = List.indexed hand
                  Deck = []
                  HandsLeft = handsLeft
                  HandSize = build.HandSize
                  DiscardsLeft = 0
                  Needed = 0.0
                  Score = score plays
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
            let run, round = plays |> Map.tryFind best.HandType |> Option.defaultValue (0, 0)
            play next taken (handsLeft - 1) (Map.add best.HandType (run + 1, round + 1) plays) (total + best.Total)

    let hand, taken = draw [] 0
    let plays = build.Situation.HandPlays |> Map.map (fun _ (run, _) -> run, 0)
    play hand taken build.Hands plays 0.0

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

let revalue (build: Build) : Build =
    let jokers = build.Jokers
    let money = build.Situation.Money
    let sell = jokers |> List.sumBy (fun joker -> joker.SellValue)
    let stencils = jokers |> List.filter (fun joker -> joker.Key = "j_stencil") |> List.length
    let empty = build.Situation.JokerSlots - jokers.Length

    let value (joker: Jokers.Joker) =
        match joker.Key with
        | "j_swashbuckler" -> Some(float (sell - joker.SellValue))
        | "j_abstract" -> Some(3.0 * float jokers.Length)
        // the game applies the Stencil multiplier only while a slot is empty
        | "j_stencil" -> Some(if empty > 0 then float (empty + stencils) else 0.0)
        | "j_bull" -> Some(2.0 * float money)
        | "j_bootstraps" -> Some(2.0 * float (money / 5))
        | _ -> joker.Value

    { build with Jokers = jokers |> List.map (fun joker -> { joker with Value = value joker }) }

let private decaying = set [ "j_ice_cream"; "j_popcorn"; "j_ramen" ]

let decay (build: Build) (rounds: int) : Build =
    let ahead = float rounds
    let leading = Regex(@"^([+X])(\d+(?:\.\d+)?)")

    // the engine reads these jokers from the card text, so only the text is aged
    let aged (joker: Jokers.Joker) =
        let m = leading.Match joker.Text

        let set value =
            let shown = (value: float).ToString("0.##", CultureInfo.InvariantCulture)
            { joker with Text = leading.Replace(joker.Text, m.Groups[1].Value + shown) }

        if not m.Success then
            joker
        else
            let value = Double.Parse(m.Groups[2].Value, CultureInfo.InvariantCulture)

            match joker.Key with
            | "j_ice_cream" -> set (max 0.0 (value - 5.0 * 4.0 * ahead))
            | "j_popcorn" -> set (max 0.0 (value - 4.0 * ahead))
            | "j_ramen" -> set (max 1.0 (Math.Round(value - 0.01 * 8.0 * ahead, 2)))
            | _ -> joker

    { build with Jokers = build.Jokers |> List.map aged }

let private horizon (seed: int) (samples: int) (build: Build) : float =
    if build.Jokers |> List.exists (fun joker -> decaying.Contains joker.Key) then
        [ 0; 1; 2 ] |> List.averageBy (fun rounds -> roundScore seed samples (revalue (decay build rounds)))
    else
        roundScore seed samples (revalue build)

let private apply (build: Build) offer =
    let paid = { build.Situation with Money = build.Situation.Money - cost offer }

    revalue (
        match offer with
        | JokerOffer(_, joker, _) ->
            let slot = if joker.Edition = Some Negative then 1 else 0

            { build with
                Jokers = build.Jokers @ [ joker ]
                Situation = { paid with JokerSlots = paid.JokerSlots + slot } }
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
                          Mult = level.Mult + two.Mult - one.Mult }
                Situation = paid }
    )

let evaluate (seed: int) (samples: int) (money: int) (build: Build) (offers: Offer list) : Evaluated list =
    let current = revalue build
    let before = horizon seed samples current

    offers
    |> List.filter (fun offer ->
        cost offer <= money
        && match offer with
           | JokerOffer _ -> build.Jokers.Length < build.Situation.JokerSlots
           | PlanetOffer _ -> true)
    |> List.map (fun offer ->
        { Offer = offer
          Before = before
          After = horizon seed samples (apply current offer)
          InterestLost = interest money - interest (money - cost offer) })
    |> List.sortBy (fun e -> -(e.After / e.Before), cost e.Offer, position e.Offer)

// early in a run the caller passes InterestLost without interest: joker power matters more than saving
let threshold (owned: int) (e: Evaluated) : float =
    (if owned < 3 then 1.02 else 1.05) + 0.05 * float e.InterestLost

let choose (owned: int) (evaluated: Evaluated list) : Evaluated option =
    evaluated |> List.tryFind (fun e -> e.After / e.Before >= threshold owned e)

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
        let scored =
            permutations current
            |> Array.ofList
            |> Array.Parallel.map (fun order ->
                order, roundScore seed samples { build with Jokers = order |> List.map (fun i -> build.Jokers[i]) })

        let best = scored |> Array.reduce (fun best next -> if snd next > snd best then next else best)
        let currentScore = scored |> Array.find (fun (order, _) -> order = current) |> snd

        if snd best >= 1.02 * currentScore then fst best else current

type Upgrade =
    { Sell: int
      Offer: Offer
      Before: float
      After: float
      Net: int
      Threshold: float }

let upgrades (seed: int) (samples: int) (money: int) (build: Build) (offers: Offer list) : Upgrade list =
    if build.Jokers.Length < build.Situation.JokerSlots then
        []
    else
        let current = revalue build
        let before = horizon seed samples current

        [ for offer in offers do
              match offer with
              | JokerOffer(_, _, price) ->
                  for sell, owned in List.indexed build.Jokers do
                      let net = price - owned.SellValue

                      // a Negative joker is never sold: its slot would go with it and leave no room for the purchase
                      if net <= money && owned.Edition <> Some Negative then
                          let held = money + owned.SellValue

                          let sold =
                              { current with
                                  Jokers = List.removeAt sell current.Jokers
                                  Situation = { current.Situation with Money = current.Situation.Money + owned.SellValue } }

                          yield
                              { Sell = sell
                                Offer = offer
                                Before = before
                                After = horizon seed samples (apply sold offer)
                                Net = net
                                Threshold = 1.05 + 0.05 * float (interest held - interest (held - price)) }
              | PlanetOffer _ -> () ]
        |> List.sortBy (fun u -> -(u.After / u.Before), u.Net, u.Sell)

let chooseUpgrade (upgrades: Upgrade list) : Upgrade option =
    upgrades |> List.tryFind (fun u -> u.After / u.Before >= max 1.15 u.Threshold)

let pickFromPack (seed: int) (samples: int) (build: Build) (cards: Offer list) : Evaluated option =
    evaluate seed samples 0 build cards
    |> List.tryHead
    |> Option.filter (fun e -> e.After > e.Before)
