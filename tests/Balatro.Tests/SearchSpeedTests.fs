module Balatro.Tests.SearchSpeedTests

open System
open System.Collections.Concurrent
open System.IO
open System.Security.Cryptography
open System.Text
open System.Text.Json
open Xunit
open Balatro.Core
open Balatro.Core.Domain
open Balatro.Core.Scoring
open Balatro.Bot
open Balatro.Bot.Search
open Balatro.Tests.HandsTests

// The search as it stood before it was sped up, kept to prove the new one returns the same
module private Reference =
    let rec combos k start n =
        if k = 0 then
            [ [] ]
        else
            [ for i in start .. n - k do
                  for rest in combos (k - 1) (i + 1) n -> i :: rest ]

    let subsetIndices n =
        [| for k in 1..5 do
               for c in combos k 0 n -> Array.ofList c |]

    let split (cards: Card[]) (subset: int[]) =
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

    let scored
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

    let bestSubset
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

    let remaining (subset: int[]) (cards: Card[]) =
        [ for i in 0 .. cards.Length - 1 do
              if not (Array.contains i subset) then
                  yield cards[i] ]

    let shuffles (input: Input) (offset: int) (count: int) =
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

    let playCandidates (input: Input) =
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

    let rollout (input: Input) (shuffle: Card[]) (candidate: Candidate) : float * int =
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

    let outlook (input: Input) (shuffles: Card[][]) (candidate: Candidate) : Outlook =
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

// Bit-exact text of results, so that 0.0 and -0.0 or a last-digit difference cannot pass
let private bits (f: float) = BitConverter.DoubleToInt64Bits f

let private cardTexts = ConcurrentDictionary<Card, string>(HashIdentity.Structural)

let private cardText (c: Card) =
    cardTexts.GetOrAdd(c, fun c -> (sprintf "%A" c).Replace("\n", " "))

let private addBreakdown (sb: StringBuilder) (b: ScoreBreakdown) =
    sb.Append(HandType.label b.HandType).Append('|') |> ignore

    for c in b.ScoringCards do
        sb.Append(cardText c).Append(',') |> ignore

    sb.Append('|').Append(bits b.Chips).Append('|').Append(bits b.Mult).Append('|').Append(bits b.Total) |> ignore

    for s in b.Steps do
        sb.Append('|').Append(s.Source).Append(':').Append(bits s.Chips).Append(':').Append(bits s.Mult) |> ignore

let private addCandidate (sb: StringBuilder) (candidate: Candidate) =
    match candidate with
    | PlayCards(indices, played, b) ->
        sb.Append("P ").Append(String.Join(",", indices)).Append(' ') |> ignore

        for c in played do
            sb.Append(cardText c).Append(',') |> ignore

        sb.Append(' ') |> ignore
        addBreakdown sb b
    | DiscardCards(indices, value) -> sb.Append("D ").Append(String.Join(",", indices)).Append(' ').Append(bits value) |> ignore

let private playsText (results: (int list * Card list * ScoreBreakdown) list) =
    let sb = StringBuilder()

    for indices, played, b in results do
        sb.Append(String.Join(",", indices)).Append(' ') |> ignore

        for c in played do
            sb.Append(cardText c).Append(',') |> ignore

        sb.Append(' ') |> ignore
        addBreakdown sb b
        sb.Append('\n') |> ignore

    sb.ToString()

let private discardsText (results: (int list * float) list) =
    let sb = StringBuilder()

    for indices, value in results do
        sb.Append(String.Join(",", indices)).Append(' ').Append(bits value).Append('\n') |> ignore

    sb.ToString()

let private chooseText ((evaluated, rule): (Candidate * Outlook) list * string) =
    let sb = StringBuilder()

    for candidate, o in evaluated do
        addCandidate sb candidate
        sb.Append(' ').Append(bits o.Clears).Append(' ').Append(bits o.MeanHands).Append(' ').Append(bits o.MeanChips).Append('\n') |> ignore

    sb.Append(rule).ToString()

let private fixtureJokers =
    JsonDocument
        .Parse(File.ReadAllText(AppContext.BaseDirectory + "fixtures/jokers.json"))
        .RootElement.EnumerateArray()
    |> Seq.map (fun e -> e.GetProperty("key").GetString(), e.GetProperty("label").GetString(), e.GetProperty("effect").GetString(), e.GetProperty("sell").GetInt32())
    |> Seq.filter (fun (key, _, _, _) -> JokerRegistry.implemented.Contains key)
    |> Array.ofSeq

let private jokerOf (key: string, label: string, text: string, sell: int) : Jokers.Joker =
    { Key = key
      Name = label
      Edition = None
      Debuffed = false
      Value = Mapping.currentValue text
      SellValue = sell
      Text = text }

let private fixtureJoker key =
    fixtureJokers |> Array.find (fun (k, _, _, _) -> k = key) |> jokerOf

let private special =
    [| "j_loyalty_card"
       "j_ice_cream"
       "j_ancient"
       "j_idol"
       "j_blueprint"
       "j_brainstorm"
       "j_baseball"
       "j_vampire"
       "j_raised_fist"
       "j_splash"
       "j_four_fingers"
       "j_pareidolia"
       "j_smeared"
       "j_shortcut"
       "j_popcorn"
       "j_ramen"
       "j_photograph"
       "j_flower_pot"
       "j_seeing_double"
       "j_midas_mask"
       "j_blackboard"
       "j_supernova"
       "j_obelisk"
       "j_hanging_chad" |]

let private suitNames = [| "Spade"; "Heart"; "Club"; "Diamond" |]
let private rankNames = [| "2"; "5"; "7"; "10"; "Jack"; "Queen"; "King"; "Ace" |]

let private retext (rng: Random) (j: Jokers.Joker) =
    match j.Key with
    | "j_loyalty_card" ->
        let left = rng.Next 7
        { j with Text = (if left = 0 then "X4 Mult every 6 hands played Active!" else $"X4 Mult every 6 hands played {left} remaining") }
    | "j_ancient" -> { j with Text = $"Each played card with {suitNames[rng.Next 4]} suit gives X1.5 Mult when scored, suit changes at end of round" }
    | "j_idol" ->
        { j with Text = $"Each played {rankNames[rng.Next rankNames.Length]} of {suitNames[rng.Next 4]}s gives X2 Mult when scored Card changes every round" }
    | "j_ice_cream" -> { j with Text = $"+{rng.Next(0, 101)} Chips -5 Chips for every hand played" }
    | _ -> j

let private enhancements =
    [| Enhancement.Bonus; Enhancement.Mult; Enhancement.Wild; Enhancement.Glass; Enhancement.Steel; Enhancement.Stone; Enhancement.Gold; Enhancement.Lucky |]

let private editions = [| Foil; Holo; Polychrome; Negative |]
let private seals = [| Seal.Red; Seal.Blue; Seal.Gold; Seal.Purple |]

let private fullDeck =
    [ for suit in [ Spades; Hearts; Clubs; Diamonds ] do
          for rank in [ Two; Three; Four; Five; Six; Seven; Eight; Nine; Ten; Jack; Queen; King; Ace ] -> card rank suit ]

let private handTypes =
    [ FlushFive
      FlushHouse
      FiveOfAKind
      StraightFlush
      FourOfAKind
      FullHouse
      Flush
      Straight
      ThreeOfAKind
      TwoPair
      Pair
      HighCard ]

let private decorate (rng: Random) (c: Card) =
    { c with
        Enhancement = if rng.Next 6 = 0 then Some enhancements[rng.Next enhancements.Length] else None
        Edition = if rng.Next 8 = 0 then Some editions[rng.Next editions.Length] else None
        Seal = if rng.Next 8 = 0 then Some seals[rng.Next seals.Length] else None
        Debuffed = rng.Next 20 = 0 }

let private makeJokers (rng: Random) =
    let count = rng.Next 6

    [ for _ in 1..count ->
          let pick =
              if rng.Next 5 < 2 then
                  fixtureJoker special[rng.Next special.Length]
              else
                  jokerOf fixtureJokers[rng.Next fixtureJokers.Length]

          { retext rng pick with
              Edition = (if rng.Next 6 = 0 then Some editions[rng.Next editions.Length] else None)
              Debuffed = rng.Next 15 = 0 } ]

// A seeded random search input: a hand of minHand..maxHand cards, 0..5 jokers, random levels, plays and boss
let private makeInput (seed: int) (minHand: int) (maxHand: int) (samples: int) (rollouts: int) : Input =
    let rng = Random(seed)
    let shuffled = Array.ofList fullDeck
    rng.Shuffle shuffled
    let size = rng.Next(minHand, maxHand + 1)
    let hand = shuffled[.. size - 1] |> Array.map (decorate rng) |> List.ofArray
    let deck = shuffled[size..] |> Array.map (fun c -> if rng.Next 6 = 0 then decorate rng c else c) |> List.ofArray

    let levels = handTypes |> List.map (fun t -> t, handLevel t (rng.Next(1, 9))) |> Map.ofList

    let handPlays =
        handTypes |> List.map (fun t -> t, (rng.Next 4, rng.Next 3)) |> Map.ofList

    let boss =
        match rng.Next 20 with
        | 0 -> Some Jokers.Psychic
        | 1 -> Some Jokers.Eye
        | 2 -> Some Jokers.Mouth
        | 3 -> Some Jokers.Flint
        | 4 when size <= 6 -> Some Jokers.Hook
        | _ -> None

    let situation: Jokers.Situation =
        { Jokers = makeJokers rng
          HandsLeft = 0
          DiscardsLeft = 0
          Money = rng.Next 40
          DeckCount = deck.Length
          JokerSlots = 5
          HandPlays = handPlays
          Boss = boss
          Balance = rng.Next 12 = 0 }

    let handsLeft = rng.Next 5
    let discardsLeft = rng.Next 5

    { Hand = List.indexed hand
      Deck = deck
      HandsLeft = handsLeft
      HandSize = size
      DiscardsLeft = discardsLeft
      Needed = float (rng.Next(50, 20000))
      Score =
        fun left discards played held ->
            scoreWith
                { situation with
                    HandsLeft = left
                    DiscardsLeft = discards }
                { Played = played
                  Held = held
                  Levels = levels }
      Seed = seed
      RedrawSamples = samples
      Rollouts = rollouts }

let private digest = IncrementalHash.CreateHash HashAlgorithmName.SHA256

[<Fact>]
let ``plays returns what the reference search returns on 2000 seeded hands`` () =
    for seed in 1..2000 do
        let input = makeInput seed 5 8 0 0
        let expected = playsText (Reference.plays input)
        let actual = playsText (plays input)
        Assert.True((expected = actual), $"seed {seed}")
        digest.AppendData(Encoding.UTF8.GetBytes actual)

    let hash = Convert.ToHexString(digest.GetHashAndReset())
    Assert.Equal("BF500A0277C4B0556B200907018E1E3B", hash[..31])

[<Fact>]
let ``plays follows the positions it is given and any hand size`` () =
    for seed in 1..300 do
        let input = makeInput (seed + 40000) 5 8 0 0
        let positions = Array.init input.Hand.Length (fun i -> i * 3 - 7)
        Random(seed).Shuffle positions
        let moved = { input with Hand = input.Hand |> List.mapi (fun i (_, c) -> positions[i], c) }
        Assert.True((playsText (Reference.plays moved) = playsText (plays moved)), $"seed {seed}")

    let levels = handTypes |> List.map (fun t -> t, handLevel t 1) |> Map.ofList

    for size in [ 0; 1; 3; 16; 17; 18 ] do
        let input =
            { makeInput 50000 5 8 0 0 with
                Hand = fullDeck |> List.truncate size |> List.indexed
                Score = fun _ _ played held -> scoreWith Jokers.Situation.empty { Played = played; Held = held; Levels = levels } }

        Assert.True((playsText (Reference.plays input) = playsText (plays input)), $"size {size}")

[<Fact>]
let ``discards returns what the reference search returns`` () =
    for seed in 1..150 do
        let input = makeInput (seed + 10000) 5 6 3 0
        Assert.True((discardsText (Reference.discards input) = discardsText (discards input)), $"seed {seed}")

    for seed in 1..6 do
        let input = makeInput (seed + 20000) 8 8 2 0
        Assert.True((discardsText (Reference.discards input) = discardsText (discards input)), $"eight cards, seed {seed}")

[<Fact>]
let ``choose returns what the reference search returns`` () =
    for seed in 1..150 do
        let input = makeInput (seed + 30000) 5 6 3 5
        Assert.True((chooseText (Reference.choose input) = chooseText (choose input)), $"seed {seed}")

let private shopBuild (jokers: Jokers.Joker list) : Shop.Build =
    let g =
        match Api.decodeGameState (JsonDocument.Parse(File.ReadAllText(AppContext.BaseDirectory + "fixtures/gamestate.SHOP.json")).RootElement) with
        | Ok g -> g
        | Error e -> failwith e

    let situation = Mapping.situation g

    { Jokers = jokers
      Levels = Mapping.levels g
      Deck = g.Cards.Cards |> List.choose Mapping.toCard
      HandSize = g.Hand.Limit
      Hands = g.Round.HandsLeft
      Situation = { situation with Jokers = jokers } }

let private loyalty left =
    { fixtureJoker "j_loyalty_card" with Text = $"X4 Mult every 6 hands played {left} remaining" }

let private shopBuilds =
    [ "none", shopBuild []
      "common", shopBuild [ fixtureJoker "j_joker"; fixtureJoker "j_jolly"; fixtureJoker "j_lusty_joker" ]
      "loyal5",
      shopBuild [ loyalty 5; fixtureJoker "j_blueprint"; fixtureJoker "j_ice_cream"; fixtureJoker "j_ancient"; fixtureJoker "j_idol" ]
      "loyal2", shopBuild [ fixtureJoker "j_baseball"; loyalty 2; fixtureJoker "j_vampire"; fixtureJoker "j_photograph" ]
      "loyal0", shopBuild [ loyalty 0; fixtureJoker "j_brainstorm"; fixtureJoker "j_popcorn" ] ]

let private shopText () =
    let sb = StringBuilder()
    let lines = ResizeArray<string>()

    for name, build in shopBuilds do
        for seed in [ 67001; 67002 ] do
            let score, rates = Shop.roundStats seed 8 build
            sb.Append(name).Append(' ').Append(seed).Append(' ').Append(bits score) |> ignore

            for f in [ rates.Straights; rates.Fours; rates.Pairs2; rates.Twos ] do
                sb.Append(' ').Append(bits f) |> ignore

            sb.Append(' ').Append(rates.Faceless) |> ignore

            for KeyValue(t, f) in rates.Plays do
                sb.Append(' ').Append(HandType.label t).Append('=').Append(bits f) |> ignore

            sb.Append(' ').Append(bits (Shop.roundScore seed 8 build)) |> ignore
            sb.Append('\n') |> ignore

        lines.Add($"{name} " + Convert.ToHexString(SHA256.HashData(Encoding.UTF8.GetBytes(sb.ToString())))[..11])
        sb.Clear() |> ignore

    List.ofSeq lines

[<Fact>]
let ``simulated rounds including the Loyalty Card path give the recorded scores`` () =
    let recorded =
        [ "none 76D70C5FDE28"
          "common 59B78C5F7A02"
          "loyal5 ADCB06B8A34D"
          "loyal2 E62831FCB84A"
          "loyal0 49FB244479BA" ]

    Assert.Equal<string list>(recorded, shopText ())
