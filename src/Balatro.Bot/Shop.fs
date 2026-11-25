module Balatro.Bot.Shop

open System
open System.Threading
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
    | TarotOffer of position: int * name: string * cost: int * deck: Card list

type Evaluated =
    { Offer: Offer
      Before: float
      After: float
      InterestLost: int }

let private counter = Regex(@"Active!|(\d+) remaining")

let private remaining (text: string) =
    let m = counter.Match text
    if m.Groups[1].Success then int m.Groups[1].Value else 0

let mutable private network: PlayNet.Net option = None

let useNetwork (net: PlayNet.Net option) = network <- net

// picks, declines, picks whose breakdown total differs from the network's total
let private counts = [| 0; 0; 0 |]

let networkCounts () = counts[0], counts[1], counts[2]

let private simulate (build: Build) (shuffle: Card[]) : float * (Card list * Scoring.ScoreBreakdown) list =
    let situation plays handsLeftAfter discardsLeft =
        // the card pays on the hands N, N+6, ... from now; the engine reads "Active!" as the paying hand
        let hand = build.Hands - 1 - handsLeftAfter

        let counted (joker: Jokers.Joker) =
            if joker.Key = "j_loyalty_card" then
                let left = ((remaining joker.Text - hand) % 6 + 6) % 6
                let shown = if left = 0 then "Active!" else $"{left} remaining"
                { joker with Text = counter.Replace(joker.Text, shown) }
            else
                joker

        { build.Situation with
            Jokers = build.Jokers |> List.map counted
            HandsLeft = handsLeftAfter
            DiscardsLeft = discardsLeft
            HandPlays = plays }

    let score plays handsLeftAfter discardsLeft played held =
        Scoring.scoreWith
            (situation plays handsLeftAfter discardsLeft)
            { Played = played
              Held = held
              Levels = build.Levels }

    let pick plays (input: Search.Input) =
        let chosen =
            network
            |> Option.bind (fun net ->
                PlayNet.choose
                    net
                    (situation plays (input.HandsLeft - 1) input.DiscardsLeft)
                    (input.Hand |> List.map snd)
                    build.Levels)

        match network, chosen with
        | Some _, Some(positions, total, _) ->
            let played = positions |> List.map (fun p -> snd input.Hand[p])

            let held =
                input.Hand
                |> List.indexed
                |> List.filter (fun (p, _) -> not (List.contains p positions))
                |> List.map (snd >> snd)

            let breakdown = input.Score (input.HandsLeft - 1) input.DiscardsLeft played held
            Interlocked.Increment(&counts[0]) |> ignore

            if breakdown.Total <> total then
                Interlocked.Increment(&counts[2]) |> ignore

            positions |> List.map (fun p -> fst input.Hand[p]), played, breakdown
        | Some _, None ->
            Interlocked.Increment(&counts[1]) |> ignore
            (Search.plays input).Head
        | None, _ -> (Search.plays input).Head

    let draw (kept: Card list) taken =
        let count = min (build.HandSize - kept.Length) (shuffle.Length - taken)
        kept @ List.ofArray shuffle[taken .. taken + count - 1], taken + count

    let without (hand: Card list) drop =
        hand |> List.indexed |> List.filter (fun (i, _) -> not (List.contains i drop)) |> List.map snd

    let lowest (hand: Card list) positions =
        positions |> List.sortBy (fun i -> Rank.id hand[i].Rank, i) |> List.truncate 5

    // the redraw sample starts n cards past the next card while enough of the shuffle remains
    let discard (hand: Card list) (best: Scoring.ScoreBreakdown) taken plays (input: Search.Input) =
        let left = shuffle.Length - taken
        let plain (c: Card) = { c with Enhancement = None }

        let unscored =
            hand
            |> List.indexed
            |> List.fold
                (fun (drop, scoring) (i, c) ->
                    match List.tryFindIndex ((=) (plain c)) scoring with
                    | Some j -> drop, List.removeAt j scoring
                    | None -> i :: drop, scoring)
                ([], best.ScoringCards |> List.map plain)
            |> fst
            |> List.rev

        let suit, count = hand |> List.countBy (fun c -> c.Suit) |> List.maxBy snd

        let offsuit =
            if count >= 3 && count < 5 then
                [ for i, c in List.indexed hand do
                      if c.Suit <> suit then
                          yield i ]
            else
                []

        let value drop =
            let kept = without hand drop
            let n = List.length drop
            let fresh = [ for j in 0 .. n - 1 -> shuffle[taken + (n + j) % left] ]

            let redrawn =
                { input with
                    Hand = List.indexed (kept @ fresh)
                    DiscardsLeft = input.DiscardsLeft - 1 }

            let _, _, b = pick plays redrawn
            b.Total

        let candidates =
            [ lowest hand unscored; lowest hand offsuit ]
            |> List.filter (fun drop -> not drop.IsEmpty && drop.Length <= left)
            |> List.distinctBy List.sort

        match candidates with
        | [] -> None
        | _ ->
            let drop, value = candidates |> List.map (fun drop -> drop, value drop) |> List.maxBy snd
            if value > best.Total then Some drop else None

    let rec play (hand: Card list) taken handsLeft discardsLeft plays total hands =
        if handsLeft = 0 || hand.IsEmpty then
            total, List.rev hands
        else
            let input: Search.Input =
                { Hand = List.indexed hand
                  Deck = []
                  HandsLeft = handsLeft
                  HandSize = build.HandSize
                  DiscardsLeft = discardsLeft
                  Needed = 0.0
                  Score = score plays
                  Seed = 0
                  RedrawSamples = 0
                  Rollouts = 0 }

            let positions, played, best = pick plays input

            match (if discardsLeft > 0 && taken < shuffle.Length then discard hand best taken plays input else None) with
            | Some drop ->
                let next, taken = draw (without hand drop) taken
                play next taken handsLeft (discardsLeft - 1) plays total hands
            | None ->
                let next, taken = draw (without hand positions) taken
                let run, round = plays |> Map.tryFind best.HandType |> Option.defaultValue (0, 0)

                play
                    next
                    taken
                    (handsLeft - 1)
                    discardsLeft
                    (Map.add best.HandType (run + 1, round + 1) plays)
                    (total + best.Total)
                    ((played, best) :: hands)

    let hand, taken = draw [] 0
    let plays = build.Situation.HandPlays |> Map.map (fun _ (run, _) -> run, 0)
    play hand taken build.Hands build.Situation.DiscardsLeft plays 0.0 []

let private simulated =
    System.Collections.Concurrent.ConcurrentDictionary<int * int * Build, (float * (Card list * Scoring.ScoreBreakdown) list)[]>()

let private rounds (seed: int) (samples: int) (build: Build) =
    let key = seed, samples, { build with Situation = { build.Situation with Money = 0 } }

    match simulated.TryGetValue key with
    | true, cached -> cached
    | _ ->
        let computed =
            Array.init samples (fun s ->
                let cards = Array.ofList build.Deck
                Random(seed + s).Shuffle cards
                simulate build cards)

        if simulated.Count > 1000 then
            simulated.Clear()

        simulated[key] <- computed
        computed

let roundScore (seed: int) (samples: int) (build: Build) : float =
    rounds seed samples build |> Array.averageBy fst

// per-round averages over the simulated hands
type Rates =
    { Straights: float
      Fours: float
      Pairs2: float
      Twos: float
      Faceless: bool
      Plays: Map<HandType, float> }

let roundStats (seed: int) (samples: int) (build: Build) : float * Rates =
    let sampled = rounds seed samples build
    let rules = JokerRegistry.rules build.Jokers
    let hands = sampled |> Array.collect (snd >> Array.ofList)
    let perRound count = float count / float samples

    let containing types =
        hands
        |> Array.filter (fun (played, _) ->
            let found = Hands.contained rules played
            types |> List.exists found.Contains)
        |> Array.length

    let scored = hands |> Array.collect (fun (_, best) -> Array.ofList best.ScoringCards)

    let score = sampled |> Array.averageBy fst

    score,
    { Straights = perRound (containing [ Straight ])
      Fours = perRound (hands |> Array.filter (fun (played, _) -> played.Length = 4) |> Array.length)
      Pairs2 = perRound (containing [ TwoPair; FullHouse ])
      Twos = perRound (scored |> Array.filter (fun card -> card.Rank = Two) |> Array.length)
      Faceless = scored |> Array.forall (fun card -> card.Rank <> Jack && card.Rank <> Queen && card.Rank <> King)
      Plays =
        hands
        |> Array.countBy (fun (_, best) -> best.HandType)
        |> Array.map (fun (handType, count) -> handType, perRound count)
        |> Map.ofArray }

let private interest money = min 5 (money / 5)

let cost offer =
    match offer with
    | JokerOffer(_, _, cost)
    | PlanetOffer(_, _, _, cost)
    | TarotOffer(_, _, cost, _) -> cost

let position offer =
    match offer with
    | JokerOffer(position, _, _)
    | PlanetOffer(position, _, _, _)
    | TarotOffer(position, _, _, _) -> position

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
            | "j_loyalty_card" ->
                let left = ((remaining joker.Text - rounds * build.Hands) % 6 + 6) % 6
                { joker with Text = counter.Replace(joker.Text, $"{left} remaining") }
            | _ -> joker

    { build with Jokers = build.Jokers |> List.map aged }

let private growing =
    set [ "j_runner"; "j_square"; "j_trousers"; "j_wee"; "j_ride_the_bus"; "j_supernova" ]

let grow (rates: Rates) (rounds: int) (build: Build) : Build =
    let ahead = float rounds

    let add rate (joker: Jokers.Joker) =
        { joker with Value = Some(Option.defaultValue 0.0 joker.Value + ahead * rate) }

    let grown (joker: Jokers.Joker) =
        match joker.Key with
        | "j_runner" -> add (15.0 * rates.Straights) joker
        | "j_square" -> add (4.0 * rates.Fours) joker
        | "j_trousers" -> add (2.0 * rates.Pairs2) joker
        | "j_wee" -> add (8.0 * rates.Twos) joker
        | "j_ride_the_bus" when rates.Faceless -> add (float build.Hands) joker
        | _ -> joker

    let plays =
        if build.Jokers |> List.exists (fun joker -> joker.Key = "j_supernova") then
            build.Situation.HandPlays
            |> Map.map (fun handType (run, round) ->
                let more = rates.Plays |> Map.tryFind handType |> Option.defaultValue 0.0
                run + int (Math.Round(ahead * more)), round)
        else
            build.Situation.HandPlays

    { build with
        Jokers = build.Jokers |> List.map grown
        Situation = { build.Situation with HandPlays = plays } }

let private horizon (seed: int) (samples: int) (build: Build) : float =
    if build.Jokers |> List.exists (fun joker -> decaying.Contains joker.Key || joker.Key = "j_loyalty_card" || growing.Contains joker.Key) then
        let score0, rates = roundStats seed samples (revalue build)

        let later =
            [ 1 .. 5 ]
            |> List.map (fun rounds -> roundScore seed samples (revalue (grow rates rounds (decay build rounds))))

        score0 :: later |> List.average
    else
        roundScore seed samples (revalue build)

let underBoss (boss: string) (build: Build) : Build =
    let debuff keep = { build with Deck = build.Deck |> List.map (fun card -> if keep card then { card with Debuffed = true } else card) }

    match boss with
    | "The Club" -> debuff (fun card -> card.Suit = Clubs)
    | "The Goad" -> debuff (fun card -> card.Suit = Spades)
    | "The Window" -> debuff (fun card -> card.Suit = Diamonds)
    | "The Head" -> debuff (fun card -> card.Suit = Hearts)
    | "The Plant" -> debuff (fun card -> card.Rank = Jack || card.Rank = Queen || card.Rank = King)
    | "The Water" -> { build with Situation = { build.Situation with DiscardsLeft = 0 } }
    | "The Needle" -> { build with Hands = 1 }
    | "The Manacle" -> { build with HandSize = build.HandSize - 1 }
    | "The Psychic" -> { build with Situation = { build.Situation with Boss = Some Jokers.Psychic } }
    | "The Eye" -> { build with Situation = { build.Situation with Boss = Some Jokers.Eye } }
    | "The Mouth" -> { build with Situation = { build.Situation with Boss = Some Jokers.Mouth } }
    | "The Hook" -> { build with Situation = { build.Situation with Boss = Some Jokers.Hook } }
    | _ -> build

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
        | TarotOffer(_, _, _, deck) -> { build with Deck = deck; Situation = paid }
    )

let private placed (value: Build -> float) (build: Build) offer =
    match offer with
    | JokerOffer(_, joker, _) when joker.Key = "j_blueprint" ->
        let bought = apply build offer
        let rest = List.take (bought.Jokers.Length - 1) bought.Jokers

        [ 0 .. rest.Length ]
        |> List.map (fun i -> value (revalue { bought with Jokers = List.insertAt i joker rest }))
        |> List.max
    | _ -> value (apply build offer)

let evaluate (seed: int) (samples: int) (money: int) (boss: string option) (build: Build) (offers: Offer list) : Evaluated list =
    let current = revalue build
    let boss = boss |> Option.filter (fun name -> underBoss name build <> build)

    let value (build: Build) =
        let normal = horizon seed samples build

        match boss with
        | Some name -> (normal + roundScore seed samples (underBoss name build)) / 2.0
        | None -> normal

    let before = value current

    offers
    |> List.filter (fun offer ->
        cost offer <= money
        && match offer with
           | JokerOffer _ -> build.Jokers.Length < build.Situation.JokerSlots
           | PlanetOffer _
           | TarotOffer _ -> true)
    |> List.map (fun offer ->
        { Offer = offer
          Before = before
          After = placed value current offer
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
        let undiscarded = { build with Situation = { build.Situation with DiscardsLeft = 0 } }

        let scored =
            permutations current
            |> Array.ofList
            |> Array.Parallel.map (fun order ->
                order, roundScore seed samples { undiscarded with Jokers = order |> List.map (fun i -> build.Jokers[i]) })

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
                                After = placed (horizon seed samples) sold offer
                                Net = net
                                Threshold = 1.05 + 0.05 * float (interest held - interest (held - price)) }
              | PlanetOffer _
              | TarotOffer _ -> () ]
        |> List.sortBy (fun u -> -(u.After / u.Before), u.Net, u.Sell)

let chooseUpgrade (upgrades: Upgrade list) : Upgrade option =
    upgrades |> List.tryFind (fun u -> u.After / u.Before >= max 1.15 u.Threshold)

let pickFromPack (seed: int) (samples: int) (boss: string option) (build: Build) (cards: Offer list) : Evaluated option =
    evaluate seed samples 0 boss build cards
    |> List.tryHead
    |> Option.filter (fun e -> e.After > e.Before)
