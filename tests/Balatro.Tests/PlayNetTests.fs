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

[<Fact>]
let ``agrees with the search on 3000 random hands`` () =
    let random = Random(2026)

    let cases =
        Array.init 3000 (fun _ ->
            let size = random.Next(1, 9)
            let shuffled = Array.copy deck
            random.Shuffle shuffled
            let levels = levelsAt (fun _ -> random.Next(1, 31))
            List.ofArray shuffled[.. size - 1], levels)

    let failures =
        cases
        |> Array.Parallel.map (fun (hand, levels) -> hand, levels, PlayNet.choose net.Value hand levels, best hand levels)
        |> Array.filter (fun (_, _, got, expected) -> got <> Some expected)

    Assert.True(failures.Length = 0, $"{failures.Length} of {cases.Length} hands disagree; first: %A{Array.truncate 5 failures}")

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

    Assert.Equal(None, PlayNet.choose net.Value [ withEnhancement Enhancement.Glass (card Ace Spades) ] levelOne)
    Assert.Equal(None, PlayNet.choose net.Value [ { card Ace Spades with Debuffed = true } ] levelOne)
    Assert.Equal(None, PlayNet.choose net.Value (List.init 9 (fun i -> deck[i])) levelOne)
    Assert.Equal(None, PlayNet.choose net.Value [] levelOne)
    Assert.Equal(None, PlayNet.choose net.Value plain (Map.add Pair { pairLevel with Mult = 201.0 } levelOne))

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
