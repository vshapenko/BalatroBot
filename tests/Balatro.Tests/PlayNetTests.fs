module Balatro.Tests.PlayNetTests

open System
open System.IO
open System.Text.Json
open Xunit
open Balatro.Core
open Balatro.Core.Domain
open Balatro.Core.Jokers
open Balatro.Bot
open Balatro.Tests.HandsTests

let private net = lazy (PlayNet.load (AppContext.BaseDirectory + "nn/playnet.json"))

// Every in-scope joker key with the one-hot name the net gives it; "inert" marks a key with no score effect
let private jokerKeys =
    lazy
        (use doc = JsonDocument.Parse(File.ReadAllText(AppContext.BaseDirectory + "nn/playnet.json"))
         [ for p in doc.RootElement.GetProperty("keys").EnumerateObject() -> p.Name, p.Value.GetString() ])

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

let private deck =
    [| for suit in [ Spades; Hearts; Clubs; Diamonds ] do
           for rank in [ Two; Three; Four; Five; Six; Seven; Eight; Nine; Ten; Jack; Queen; King; Ace ] ->
               card rank suit |]

let private levelsAt (levelOf: HandType -> int) =
    handTypes |> List.map (fun t -> t, handLevel t (levelOf t)) |> Map.ofList

let private levelOne = levelsAt (fun _ -> 1)

let private score levels played held =
    Scoring.score { Played = played; Held = held; Levels = levels }

let private joker key edition value : Joker =
    { Key = key
      Name = key
      Edition = edition
      Debuffed = false
      Value = Some value
      SellValue = 0
      Text = "" }

let private situationOf hands discards (plays: (HandType * (int * int)) list) jokers : Situation =
    { Situation.empty with
        Jokers = jokers
        HandsLeft = hands
        DiscardsLeft = discards
        HandPlays = Map.ofList plays }

// The head of Search.plays; its Score sees the situation as the bot's Score does
let private bestWith (situation: Situation) (hand: Card list) levels =
    let input: Search.Input =
        { Hand = List.indexed hand
          Deck = []
          HandsLeft = situation.HandsLeft + 1
          HandSize = 8
          DiscardsLeft = situation.DiscardsLeft
          Needed = 0.0
          Score =
            fun handsLeft discardsLeft played held ->
                Scoring.scoreWith
                    { situation with HandsLeft = handsLeft; DiscardsLeft = discardsLeft }
                    { Played = played; Held = held; Levels = levels }
          Seed = 1
          RedrawSamples = 1
          Rollouts = 1 }

    let indices, _, breakdown = Search.plays input |> List.head
    indices, breakdown.Total, breakdown.HandType

let private best hand levels = bestWith Situation.empty hand levels

let private chosenWith situation hand levels =
    PlayNet.choose net.Value situation hand levels |> Option.get

let private chosen hand levels = chosenWith Situation.empty hand levels

let private disagreements (cases: (Card list * Map<HandType, HandLevel>)[]) =
    cases
    |> Array.Parallel.map (fun (hand, levels) ->
        hand, levels, PlayNet.choose net.Value Situation.empty hand levels, best hand levels)
    |> Array.filter (fun (_, _, got, expected) -> got <> Some expected)

let private enhancements =
    None
    :: [ for e in [ Enhancement.Bonus; Enhancement.Mult; Enhancement.Wild; Enhancement.Glass
                    Enhancement.Steel; Enhancement.Stone; Enhancement.Gold; Enhancement.Lucky ] -> Some e ]

let private editions = None :: [ for e in [ Foil; Holo; Polychrome; Negative ] -> Some e ]

let private seals =
    None :: [ for e in [ Seal.Red; Seal.Blue; Seal.Gold; Seal.Purple ] -> Some e ]

// The precision limits of the net, from the card and joker fields: Polychrome plus Steel passes (a Polychrome joker is
// one pass), and Glass passes; a Red Seal doubles a pass
let private passesOf (hand: Card list) (jokers: Joker list) =
    let passes (c: Card) = if c.Seal = Some Seal.Red then 2 else 1

    let weight =
        hand
        |> List.sumBy (fun c ->
            passes c * ((if c.Edition = Some Polychrome then 1 else 0) + (if c.Enhancement = Some Enhancement.Steel then 1 else 0)))

    let glass = hand |> List.sumBy (fun c -> if c.Enhancement = Some Enhancement.Glass then passes c else 0)
    weight + (jokers |> List.filter (fun j -> j.Edition = Some Polychrome) |> List.length), glass

let private breaksLimits hand jokers =
    let weight, glass = passesOf hand jokers
    weight > 8 || glass > 6

let private modified enhancement edition seal (c: Card) =
    { c with Enhancement = enhancement; Edition = edition; Seal = seal }

let private withEdition edition (c: Card) = { c with Edition = Some edition }

let private withSeal seal (c: Card) = { c with Seal = Some seal }

[<Fact>]
let ``agrees with the search on 500 random plain hands`` () =
    let random = Random(2026)

    let cases =
        Array.init 500 (fun _ ->
            let size = random.Next(1, 9)
            let shuffled = Array.copy deck
            random.Shuffle shuffled
            let levels = levelsAt (fun _ -> random.Next(1, 31))
            List.ofArray shuffled[.. size - 1], levels)

    let failures = disagreements cases

    Assert.True(failures.Length = 0, $"{failures.Length} of {cases.Length} hands disagree; first: %A{Array.truncate 5 failures}")

type ModifierAgreement(output: Xunit.Abstractions.ITestOutputHelper) =
    [<Fact>]
    member _.``agrees with the search on 1500 random modified hands`` () =
        let random = Random(2027)
        let pick (options: 'a list) = options[random.Next options.Length]

        let cases =
            Array.init 1500 (fun _ ->
                let size = random.Next(1, 9)
                let shuffled = Array.copy deck
                random.Shuffle shuffled
                let hand = [ for c in shuffled[.. size - 1] -> modified (pick enhancements) (pick editions) (pick seals) c ]
                hand, levelsAt (fun _ -> random.Next(1, 31)))

        let results =
            cases
            |> Array.Parallel.map (fun (hand, levels) ->
                hand, levels, PlayNet.choose net.Value Situation.empty hand levels, best hand levels)

        let rejected = results |> Array.filter (fun (_, _, got, _) -> got.IsNone)
        let checkedHands = results |> Array.filter (fun (_, _, got, _) -> got.IsSome)
        let loose = rejected |> Array.filter (fun (hand, _, _, _) -> not (breaksLimits hand []))
        let failures = checkedHands |> Array.filter (fun (_, _, got, expected) -> got <> Some expected)

        output.WriteLine $"checked {checkedHands.Length}, rejected {rejected.Length}"

        Assert.True(loose.Length = 0, $"{loose.Length} hands rejected inside the limits; first: %A{Array.truncate 5 loose}")
        Assert.True(checkedHands.Length >= 1000, $"only {checkedHands.Length} hands checked")
        Assert.True(failures.Length = 0, $"{failures.Length} of {checkedHands.Length} hands disagree; first: %A{Array.truncate 5 failures}")

type JokerAgreement(output: Xunit.Abstractions.ITestOutputHelper) =
    [<Fact>]
    member _.``agrees with the search on 1500 random hands with jokers`` () =
        let random = Random(2028)
        let pick (options: 'a list) = options[random.Next options.Length]
        let keys = jokerKeys.Value |> List.map fst

        let cases =
            Array.init 1500 (fun _ ->
                let size = random.Next(1, 9)
                let shuffled = Array.copy deck
                random.Shuffle shuffled
                let hand = [ for c in shuffled[.. size - 1] -> modified (pick enhancements) (pick editions) (pick seals) c ]

                let jokers =
                    [ for _ in 1 .. random.Next(0, 6) -> joker (pick keys) (pick editions) (float (random.Next(0, 41))) ]

                let plays =
                    [ for t in handTypes ->
                          let run = random.Next(0, 6)
                          t, (run, random.Next(0, run + 1)) ]

                let situation = situationOf (random.Next(0, 5)) (random.Next(0, 5)) plays jokers
                hand, situation, levelsAt (fun _ -> random.Next(1, 31)))

        let results =
            cases
            |> Array.Parallel.map (fun (hand, situation, levels) ->
                hand, situation, levels, PlayNet.choose net.Value situation hand levels, bestWith situation hand levels)

        let rejected = results |> Array.filter (fun (_, _, _, got, _) -> got.IsNone)
        let checkedCases = results |> Array.filter (fun (_, _, _, got, _) -> got.IsSome)

        let loose =
            rejected |> Array.filter (fun (hand, situation, _, _, _) -> not (breaksLimits hand situation.Jokers))

        let failures = checkedCases |> Array.filter (fun (_, _, _, got, expected) -> got <> Some expected)

        output.WriteLine $"checked {checkedCases.Length}, rejected {rejected.Length}"

        Assert.True(loose.Length = 0, $"{loose.Length} cases rejected inside the limits; first: %A{Array.truncate 5 loose}")
        Assert.True(checkedCases.Length >= 1000, $"only {checkedCases.Length} cases checked")
        Assert.True(failures.Length = 0, $"{failures.Length} of {checkedCases.Length} cases disagree; first: %A{Array.truncate 5 failures}")

// The net's choice must equal the search's head and Scoring.scoreWith of that play
let private agreeWith (situation: Situation) (hand: Card list) levels =
    let positions, total, got = chosenWith situation hand levels
    let played = positions |> List.map (fun i -> hand[i])
    let held = hand |> List.indexed |> List.filter (fun (i, _) -> not (List.contains i positions)) |> List.map snd

    let expected =
        Scoring.scoreWith situation { Played = played; Held = held; Levels = levels }

    Assert.Equal(expected.Total, total)
    Assert.Equal(expected.HandType, got)
    Assert.Equal(bestWith situation hand levels, (positions, total, got))
    positions, total, got

let private agree hand levels = agreeWith Situation.empty hand levels

[<Fact>]
let ``folds mult in played order`` () =
    let multFirst = [ withEnhancement Enhancement.Mult (card King Spades); withEnhancement Enhancement.Glass (card King Hearts) ]
    let firstPositions, firstTotal, _ = agree multFirst levelOne
    let _, secondTotal, _ = agree (List.rev multFirst) levelOne

    Assert.Equal<int list>([ 0; 1 ], firstPositions)
    Assert.Equal(360.0, firstTotal)
    Assert.Equal(240.0, secondTotal)

[<Fact>]
let ``repeats a Polychrome card with a Red Seal`` () =
    let polychrome = withEdition Polychrome (card Ace Spades)
    let hand seal = [ (if seal then withSeal Seal.Red polychrome else polychrome); card Ace Hearts ]
    let _, plain, _ = agree (hand false) levelOne
    let _, repeated, _ = agree (hand true) levelOne

    Assert.True(repeated > plain)

[<Fact>]
let ``multiplies by held Steel once, or twice with a Red Seal`` () =
    let hand steel = [ card Ace Spades; steel (card King Hearts) ]
    let steel = withEnhancement Enhancement.Steel
    let positions, once, _ = agree (hand steel) levelOne
    let _, twice, _ = agree (hand (steel >> withSeal Seal.Red)) levelOne

    Assert.Equal<int list>([ 0 ], positions)
    Assert.Equal(24.0, once)
    Assert.Equal(36.0, twice)

[<Fact>]
let ``scores a Stone with no rank or suit`` () =
    let stone = withEnhancement Enhancement.Stone
    let alone, _, _ = agree [ stone (card Ace Spades) ] levelOne
    let _, _, withKicker = agree [ card King Spades; card King Hearts; card King Clubs; card King Diamonds; stone (card Three Spades) ] levelOne
    let _, _, highCard = agree [ stone (card Two Spades); card Seven Clubs ] levelOne

    Assert.Equal<int list>([ 0 ], alone)
    Assert.Equal(FourOfAKind, withKicker)
    Assert.Equal(HighCard, highCard)

[<Fact>]
let ``counts a Wild card in a Flush`` () =
    let hand = [ card Two Hearts; card Five Hearts; card Nine Hearts; card King Hearts; withEnhancement Enhancement.Wild (card Seven Spades) ]
    let positions, _, handType = agree hand levelOne

    Assert.Equal(Flush, handType)
    Assert.Equal<int list>([ 0; 1; 2; 3; 4 ], positions)

[<Fact>]
let ``agrees at the top levels and the precision limits`` () =
    let top = handTypes |> List.map (fun t -> t, { Chips = 5000.0; Mult = 200.0 }) |> Map.ofList
    let glassPolychromeRed = card King Spades |> modified (Some Enhancement.Glass) (Some Polychrome) (Some Seal.Red)
    let rest = [ card Queen Spades; card Queen Hearts; card Seven Clubs; card Four Diamonds ]

    let glassHand =
        [ glassPolychromeRed
          withSeal Seal.Red (withEnhancement Enhancement.Glass (withEdition Polychrome (card King Hearts)))
          withSeal Seal.Red (withEnhancement Enhancement.Glass (withEdition Polychrome (card King Clubs)))
          withSeal Seal.Red (withEdition Polychrome (card Two Spades)) ]
        @ rest

    let steelHand =
        [ for c in [ card King Spades; card King Hearts; card King Clubs ] -> withSeal Seal.Red (withEnhancement Enhancement.Glass c) ]
        @ [ for c in [ card Queen Spades; card Queen Hearts; card Seven Clubs; card Four Diamonds ] ->
                withSeal Seal.Red (withEnhancement Enhancement.Steel c) ]
        @ [ card Two Spades ]

    for hand in [ glassHand; steelHand ] do
        Assert.Equal((8, 6), passesOf hand [])
        agree hand top |> ignore

[<Fact>]
let ``reaches every hand type`` () =
    let cases =
        [ FlushFive, List.replicate 5 (card Ace Spades)
          FlushHouse, List.replicate 3 (card Seven Diamonds) @ List.replicate 2 (card Four Diamonds)
          FiveOfAKind, [ card Ace Spades; card Ace Hearts; card Ace Hearts; card Ace Clubs; card Ace Diamonds ]
          StraightFlush, [ card Queen Spades; card Jack Spades; card Ten Spades; card Nine Spades; card Eight Spades ]
          FourOfAKind, [ card King Spades; card King Hearts; card King Clubs; card King Diamonds; card Three Spades ]
          FullHouse, [ card Nine Spades; card Nine Hearts; card Nine Clubs; card Four Diamonds; card Four Spades ]
          Flush, [ card Two Hearts; card Five Hearts; card Nine Hearts; card King Hearts; card Seven Hearts ]
          Straight, [ card Ace Spades; card Two Hearts; card Three Clubs; card Four Diamonds; card Five Spades ]
          ThreeOfAKind, [ card Eight Spades; card Eight Hearts; card Eight Clubs; card King Diamonds; card Two Spades ]
          TwoPair, [ card Nine Spades; card Nine Hearts; card Four Clubs; card Four Diamonds ]
          Pair, [ card Five Clubs; card Five Diamonds ]
          HighCard, [ card Seven Clubs ] ]

    for handType, hand in cases do
        let positions, total, got = chosen hand levelOne
        let played = positions |> List.map (fun i -> hand[i])
        let held = hand |> List.indexed |> List.filter (fun (i, _) -> not (List.contains i positions)) |> List.map snd
        let expected = score levelOne played held

        Assert.Equal(handType, got)
        Assert.Equal(expected.HandType, got)
        Assert.Equal(expected.Total, total)
        Assert.Equal(best hand levelOne, (positions, total, got))

[<Fact>]
let ``agrees with the search on 300 straight windows`` () =
    let random = Random(2029)
    let suits = [| Spades; Hearts; Clubs; Diamonds |]
    let ranks = [| Ace; Two; Three; Four; Five; Six; Seven; Eight; Nine; Ten; Jack; Queen; King; Ace |]

    let cases =
        Array.init 300 (fun _ ->
            let start = random.Next 10
            let sameSuit = random.Next 3 = 0
            let suit = suits[random.Next 4]
            let run = [ for r in ranks[start .. start + 4] -> card r (if sameSuit then suit else suits[random.Next 4]) ]
            let others = deck |> Array.filter (fun c -> not (List.contains c run))
            random.Shuffle others
            let hand = Array.ofList (run @ List.ofArray others[.. random.Next 4 - 1])
            random.Shuffle hand
            start, List.ofArray hand, levelsAt (fun _ -> random.Next(1, 31)))

    let results =
        cases
        |> Array.Parallel.map (fun (start, hand, levels) ->
            start, hand, levels, PlayNet.choose net.Value Situation.empty hand levels, best hand levels)

    let failures = results |> Array.filter (fun (_, _, _, got, expected) -> got <> Some expected)
    let typeOf (_, _, _, _, (_, _, handType)) = handType

    Assert.True(failures.Length = 0, $"{failures.Length} of {results.Length} hands disagree; first: %A{Array.truncate 5 failures}")
    Assert.True(results |> Array.exists (fun r -> typeOf r = Straight), "no Straight reached")
    Assert.True(results |> Array.exists (fun r -> typeOf r = StraightFlush), "no Straight Flush reached")

    Assert.True(
        results |> Array.exists (fun ((start, _, _, _, _) as r) -> start = 0 && (typeOf r = Straight || typeOf r = StraightFlush)),
        "no Ace-low straight reached"
    )

[<Fact>]
let ``breaks ties like the search`` () =
    let hand =
        [ card Queen Hearts
          card King Spades
          card Two Clubs
          card Four Diamonds
          card Six Spades
          card Nine Hearts
          card Three Clubs
          card Eight Diamonds ]

    let positions, _, _ = chosen hand levelOne
    let searched, _, _ = best hand levelOne

    Assert.Equal<int list>([ 0 ], positions)
    Assert.Equal<int list>(searched, positions)

[<Fact>]
let ``declines hands outside its scope`` () =
    let plain = [ card Ace Spades; card King Hearts ]
    let pairLevel = levelOne[Pair]
    let steelRed = card Ace Spades |> modified (Some Enhancement.Steel) None (Some Seal.Red)
    let declined hand levels = PlayNet.choose net.Value Situation.empty hand levels

    Assert.Equal(None, declined [ { card Ace Spades with Debuffed = true } ] levelOne)
    Assert.Equal(None, declined (List.init 9 (fun i -> deck[i])) levelOne)
    Assert.Equal(None, declined [] levelOne)
    Assert.Equal(None, declined plain (Map.add Pair { pairLevel with Mult = 201.0 } levelOne))
    Assert.Equal(None, declined plain (Map.remove Pair levelOne))
    Assert.Equal(None, declined (List.replicate 4 steelRed @ [ withEdition Polychrome (card King Hearts) ]) levelOne)

let private single = [ card King Spades ]

// A hand, its levels and how to build the situation around a joker list
type private Scene =
    { Cards: Card list
      Rates: Map<HandType, HandLevel>
      Make: Joker list -> Situation }

let private scene cards =
    { Cards = cards
      Rates = levelOne
      Make = situationOf 2 2 [] }

let private withLeft hands discards scene =
    { scene with Make = situationOf hands discards [] }

let private withPlays plays scene =
    { scene with Make = situationOf 2 2 plays }

let private favouring handType scene =
    { scene with Rates = levelsAt (fun t -> if t = handType then 30 else 1) }

// What the net chooses for one joker in a scene, and what that joker adds to the chosen play
let private effectOf (scene: Scene) key value =
    let situation = scene.Make [ joker key None value ]
    let positions, total, _ = agreeWith situation scene.Cards scene.Rates
    let played = positions |> List.map (fun i -> scene.Cards[i])
    let held = scene.Cards |> List.indexed |> List.filter (fun (i, _) -> not (List.contains i positions)) |> List.map snd
    let without = Scoring.scoreWith { situation with Jokers = [] } { Played = played; Held = held; Levels = scene.Rates }
    total, without.Total

let private pairs = scene [ card King Spades; card King Hearts; card Four Diamonds ]
let private noPair = scene [ card King Spades; card Seven Hearts ]
let private threes = scene [ card Eight Spades; card Eight Hearts; card Eight Clubs ]
let private noThree = scene [ card Eight Spades; card Eight Hearts ]
let private twoPairs = scene [ card Nine Spades; card Nine Hearts; card Four Clubs; card Four Diamonds ]
let private noTwoPair = scene [ card Nine Spades; card Nine Hearts; card Four Clubs ]
let private straights = scene [ card Five Spades; card Six Hearts; card Seven Clubs; card Eight Diamonds; card Nine Spades ]
let private noStraight = scene [ card Five Spades; card Six Hearts; card Seven Clubs; card Eight Diamonds ]
let private flushes = scene [ card Two Hearts; card Five Hearts; card Nine Hearts; card King Hearts; card Seven Hearts ]
let private noFlush = scene [ card Two Hearts; card Five Hearts; card Nine Hearts; card King Hearts; card Seven Spades ]
let private fours = scene [ card King Spades; card King Hearts; card King Clubs; card King Diamonds ]
let private noFour = scene [ card King Spades; card King Hearts; card King Clubs ]

// Five spades to play and three red cards to hold: some held card is never black
let private redHeld =
    scene [ card Two Spades; card Five Spades; card Nine Spades; card King Spades; card Seven Spades; card Three Hearts; card Four Diamonds; card Six Hearts ]
    |> favouring Flush

let private scenes = scene single

// A scene where the joker's effect fires and, for a conditional joker, one where it does not; each with a value
let private jokerCases: (string * (float * Scene) * (float * Scene) option)[] =
    [| "j_joker", (0.0, scenes), None
       "j_gros_michel", (0.0, scenes), None
       "j_cavendish", (0.0, scenes), None
       "j_stuntman", (0.0, scenes), None
       "j_sly", (0.0, pairs), Some(0.0, noPair)
       "j_jolly", (0.0, pairs), Some(0.0, noPair)
       "j_duo", (0.0, pairs), Some(0.0, noPair)
       "j_wily", (0.0, threes), Some(0.0, noThree)
       "j_zany", (0.0, threes), Some(0.0, noThree)
       "j_trio", (0.0, threes), Some(0.0, noThree)
       "j_mad", (0.0, twoPairs), Some(0.0, noTwoPair)
       "j_clever", (0.0, twoPairs), Some(0.0, noTwoPair)
       "j_crazy", (0.0, straights), Some(0.0, noStraight)
       "j_devious", (0.0, straights), Some(0.0, noStraight)
       "j_order", (0.0, straights), Some(0.0, noStraight)
       "j_droll", (0.0, flushes), Some(0.0, noFlush)
       "j_crafty", (0.0, flushes), Some(0.0, noFlush)
       "j_tribe", (0.0, flushes), Some(0.0, noFlush)
       "j_family", (0.0, fours), Some(0.0, noFour)
       "j_half", (0.0, scenes), Some(0.0, favouring Flush flushes)
       "j_blackboard", (0.0, scene [ card Ace Spades; card King Clubs ]), Some(0.0, redHeld)
       "j_flower_pot", (0.0, straights), Some(0.0, scene [ card Five Spades; card Six Hearts; card Seven Spades; card Eight Diamonds; card Nine Spades ])
       "j_seeing_double", (0.0, straights), Some(0.0, scene [ card Five Spades; card Six Hearts; card Seven Diamonds; card Eight Diamonds; card Nine Spades ])
       "j_banner", (0.0, withLeft 2 3 scenes), Some(0.0, withLeft 2 0 scenes)
       "j_mystic_summit", (0.0, withLeft 2 0 scenes), Some(0.0, withLeft 2 2 scenes)
       "j_acrobat", (0.0, withLeft 0 2 scenes), Some(0.0, withLeft 2 2 scenes)
       "j_supernova", (0.0, withPlays [ HighCard, (5, 1) ] scenes), None
       "j_card_sharp", (0.0, withPlays [ HighCard, (1, 1) ] scenes), Some(0.0, scenes)
       "j_abstract", (7.0, scenes), Some(0.0, scenes)
       "j_erosion", (7.0, scenes), Some(0.0, scenes)
       "j_fortune_teller", (7.0, scenes), Some(0.0, scenes)
       "j_swashbuckler", (7.0, scenes), Some(0.0, scenes)
       "j_bootstraps", (7.0, scenes), Some(0.0, scenes)
       "j_blue_joker", (7.0, scenes), Some(0.0, scenes)
       "j_stone", (7.0, scenes), Some(0.0, scenes)
       "j_bull", (7.0, scenes), Some(0.0, scenes)
       "j_drivers_license", (16.0, scenes), Some(15.0, scenes) |]

[<Fact>]
let ``scores every joker where its effect fires and where it does not`` () =
    let effectKeys = jokerKeys.Value |> List.filter (fun (_, name) -> name <> "inert") |> List.map fst
    let covered = jokerCases |> Array.map (fun (key, _, _) -> key) |> List.ofArray

    Assert.Equal<string list>(List.sort effectKeys, List.sort covered)

    for key, (value, fires), other in jokerCases do
        let total, without = effectOf fires key value
        Assert.True(total <> without, $"{key} did not fire where it should")

        match other with
        | Some(value, quiet) ->
            let total, without = effectOf quiet key value
            Assert.True((total = without), $"{key} fired where it should not")
        | None -> ()

[<Fact>]
let ``lets an inert joker occupy a slot`` () =
    let total, without = effectOf pairs "j_misprint" 0.0

    Assert.Equal(without, total)

[<Fact>]
let ``orders Foil and Holo before and after a joker effect`` () =
    let holo = joker "j_misprint" (Some Holo) 0.0
    let foil = joker "j_misprint" (Some Foil) 0.0
    let triple = joker "j_cavendish" None 0.0
    let hand = single

    let totalOf jokers =
        let _, total, _ = agreeWith (situationOf 2 2 [] jokers) hand levelOne
        total

    // chips 15, mult 1: (1 + 10) * 3 against 1 * 3 + 10
    Assert.Equal(495.0, totalOf [ holo; triple ])
    Assert.Equal(195.0, totalOf [ triple; holo ])
    // Foil adds chips, so the order does not matter
    Assert.Equal(totalOf [ foil; triple ], totalOf [ triple; foil ])
    Assert.Equal(195.0, totalOf [ foil; triple ])

[<Fact>]
let ``applies a Polychrome joker after its own effect`` () =
    let polychrome key = joker key (Some Polychrome) 0.0
    let plain key = joker key None 0.0

    let totalOf jokers =
        let _, total, _ = agreeWith (situationOf 2 2 [] jokers) single levelOne
        total

    // chips 15: (1 + 4) * 1.5 = 7.5 floors to 112; had the 1.5 come first, 1.5 + 4 = 5.5 would give 82
    Assert.Equal(112.0, totalOf [ polychrome "j_joker" ])
    // the 1.5 closes its own slot: (1 + 4) * 1.5 + 4 = 11.5, but (1 + 4 + 4) * 1.5 = 13.5
    Assert.Equal(172.0, totalOf [ polychrome "j_joker"; plain "j_joker" ])
    Assert.Equal(202.0, totalOf [ plain "j_joker"; polychrome "j_joker" ])

[<Fact>]
let ``agrees on the widest joker hand in scope`` () =
    let top = handTypes |> List.map (fun t -> t, { Chips = 5000.0; Mult = 200.0 }) |> Map.ofList
    let glass = withEnhancement Enhancement.Glass
    let steel = withEnhancement Enhancement.Steel

    let hand =
        [ glass (card King Spades)
          glass (card King Hearts)
          glass (card King Clubs)
          withEdition Polychrome (card King Diamonds)
          withEdition Polychrome (card Queen Spades)
          steel (card Queen Hearts)
          steel (card Queen Clubs)
          card Three Diamonds ]

    let jokers =
        [ joker "j_blue_joker" None 500.0
          joker "j_abstract" (Some Polychrome) 500.0
          joker "j_family" None 500.0
          joker "j_trio" (Some Polychrome) 500.0
          joker "j_drivers_license" None 500.0 ]

    let situation = situationOf 0 0 [] jokers
    let positions, total, handType = agreeWith situation hand top

    Assert.Equal((6, 3), passesOf hand jokers)
    Assert.Equal(FourOfAKind, handType)
    Assert.Equal<int list>([ 0; 1; 2; 3 ], positions)
    Assert.True(total > 1e9, $"total {total} is not large")

[<Fact>]
let ``declines jokers and situations outside its scope`` () =
    let declined jokers = PlayNet.choose net.Value (situationOf 2 2 [] jokers) single levelOne
    let one key = joker key None 0.0
    let boss = { situationOf 2 2 [] [] with Boss = Some Psychic }

    Assert.Equal(None, declined (List.replicate 6 (one "j_joker")))
    Assert.Equal(None, declined [ { one "j_joker" with Debuffed = true } ])
    Assert.Equal(None, declined [ one "j_stencil" ])
    Assert.Equal(None, declined [ joker "j_joker" None 501.0 ])
    Assert.Equal(None, PlayNet.choose net.Value boss single levelOne)
    Assert.Equal(None, PlayNet.choose net.Value { situationOf 2 2 [] [] with Balance = true } single levelOne)
    Assert.True((declined [ joker "j_joker" None 500.0 ]).IsSome)

[<Fact>]
let ``readme lists every dim once`` () =
    use doc = JsonDocument.Parse(File.ReadAllText(AppContext.BaseDirectory + "nn/playnet.json"))
    let d = doc.RootElement.GetProperty("d").GetInt32()

    let indices =
        File.ReadAllLines(Path.Combine(__SOURCE_DIRECTORY__, "../../nn/README.md"))
        |> Array.choose (fun line ->
            let cells = line.Split('|')

            if cells.Length > 2 then
                match Int32.TryParse(cells[1].Trim()) with
                | true, index -> Some index
                | _ -> None
            else
                None)

    Assert.Equal<int[]>([| 0 .. d - 1 |], Array.sort indices)
