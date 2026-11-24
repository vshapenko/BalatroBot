module Balatro.Tests.PlayNetTests

open System
open System.IO
open System.Text.Json
open Xunit
open Balatro.Core
open Balatro.Core.Domain
open Balatro.Bot
open Balatro.Tests.HandsTests

let private net = lazy (PlayNet.load (AppContext.BaseDirectory + "nn/playnet.json"))

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

let private best (hand: Card list) levels =
    let input: Search.Input =
        { Hand = List.indexed hand
          Deck = []
          HandsLeft = 1
          HandSize = 8
          DiscardsLeft = 0
          Needed = 0.0
          Score = fun _ _ played held -> score levels played held
          Seed = 1
          RedrawSamples = 1
          Rollouts = 1 }

    let indices, _, breakdown = Search.plays input |> List.head
    indices, breakdown.Total, breakdown.HandType

let private chosen hand levels =
    PlayNet.choose net.Value hand levels |> Option.get

let private disagreements (cases: (Card list * Map<HandType, HandLevel>)[]) =
    cases
    |> Array.Parallel.map (fun (hand, levels) -> hand, levels, PlayNet.choose net.Value hand levels, best hand levels)
    |> Array.filter (fun (_, _, got, expected) -> got <> Some expected)

let private enhancements =
    None
    :: [ for e in [ Enhancement.Bonus; Enhancement.Mult; Enhancement.Wild; Enhancement.Glass
                    Enhancement.Steel; Enhancement.Stone; Enhancement.Gold; Enhancement.Lucky ] -> Some e ]

let private editions = None :: [ for e in [ Foil; Holo; Polychrome; Negative ] -> Some e ]

let private seals =
    None :: [ for e in [ Seal.Red; Seal.Blue; Seal.Gold; Seal.Purple ] -> Some e ]

// The precision limits of the net, from the card fields: Polychrome plus Steel passes, and Glass passes
let private breaksLimits (hand: Card list) =
    let passes (c: Card) = if c.Seal = Some Seal.Red then 2 else 1

    let weight =
        hand
        |> List.sumBy (fun c ->
            passes c * ((if c.Edition = Some Polychrome then 1 else 0) + (if c.Enhancement = Some Enhancement.Steel then 1 else 0)))

    let glass = hand |> List.sumBy (fun c -> if c.Enhancement = Some Enhancement.Glass then passes c else 0)
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
            |> Array.Parallel.map (fun (hand, levels) -> hand, levels, PlayNet.choose net.Value hand levels, best hand levels)

        let rejected = results |> Array.filter (fun (_, _, got, _) -> got.IsNone)
        let checkedHands = results |> Array.filter (fun (_, _, got, _) -> got.IsSome)
        let loose = rejected |> Array.filter (fun (hand, _, _, _) -> not (breaksLimits hand))
        let failures = checkedHands |> Array.filter (fun (_, _, got, expected) -> got <> Some expected)

        output.WriteLine $"checked {checkedHands.Length}, rejected {rejected.Length}"

        Assert.True(loose.Length = 0, $"{loose.Length} hands rejected inside the limits; first: %A{Array.truncate 5 loose}")
        Assert.True(checkedHands.Length >= 1000, $"only {checkedHands.Length} hands checked")
        Assert.True(failures.Length = 0, $"{failures.Length} of {checkedHands.Length} hands disagree; first: %A{Array.truncate 5 failures}")

// The net's choice must equal the search's head and Scoring.score of that play
let private agree (hand: Card list) levels =
    let positions, total, got = chosen hand levels
    let played = positions |> List.map (fun i -> hand[i])
    let held = hand |> List.indexed |> List.filter (fun (i, _) -> not (List.contains i positions)) |> List.map snd
    let expected = score levels played held

    Assert.Equal(expected.Total, total)
    Assert.Equal(expected.HandType, got)
    Assert.Equal(best hand levels, (positions, total, got))
    positions, total, got

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
        Assert.False(breaksLimits hand)
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

    Assert.Equal(None, PlayNet.choose net.Value [ { card Ace Spades with Debuffed = true } ] levelOne)
    Assert.Equal(None, PlayNet.choose net.Value (List.init 9 (fun i -> deck[i])) levelOne)
    Assert.Equal(None, PlayNet.choose net.Value [] levelOne)
    Assert.Equal(None, PlayNet.choose net.Value plain (Map.add Pair { pairLevel with Mult = 201.0 } levelOne))
    Assert.Equal(None, PlayNet.choose net.Value plain (Map.remove Pair levelOne))
    Assert.Equal(None, PlayNet.choose net.Value (List.replicate 4 steelRed @ [ withEdition Polychrome (card King Hearts) ]) levelOne)

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
