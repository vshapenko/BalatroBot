module Balatro.Tests.SearchTests

open System
open System.IO
open System.Text.Json
open Xunit
open Balatro.Core
open Balatro.Core.Domain
open Balatro.Bot
open Balatro.Tests.HandsTests

let private levels =
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
    |> List.map (fun handType -> handType, handLevel handType 1)
    |> Map.ofList

let private input hand deck handsLeft discardsLeft needed : Search.Input =
    { Hand = List.indexed hand
      Deck = deck
      HandsLeft = handsLeft
      HandSize = 8
      DiscardsLeft = discardsLeft
      Needed = needed
      Score =
        fun _ _ played held -> Scoring.score { Played = played; Held = held; Levels = levels }
      Seed = 1
      RedrawSamples = 12
      Rollouts = 40 }

let private fixtureHand =
    [ card King Spades
      card King Hearts
      card Jack Clubs
      card Ten Spades
      card Eight Spades
      card Eight Hearts
      card Eight Clubs
      card Seven Clubs ]

let private heartsHand =
    [ card Two Hearts
      card Five Hearts
      card Nine Hearts
      card King Hearts
      card Three Spades
      card Four Clubs
      card Seven Diamonds
      card Eight Clubs ]

let private heartsDeck =
    [ card Three Hearts
      card Four Hearts
      card Six Hearts
      card Seven Hearts
      card Eight Hearts
      card Ten Hearts
      card Jack Hearts
      card Queen Hearts ]

let private flushHand =
    [ card King Hearts
      card Ten Hearts
      card Five Hearts
      card Three Hearts
      card Two Hearts
      card Queen Spades
      card Nine Clubs
      card Eight Diamonds ]

let private flushDeck =
    [ for suit in [ Spades; Hearts; Clubs; Diamonds ] do
          for rank in [ Two; Three; Four; Five; Six; Seven; Eight; Nine; Ten; Jack; Queen; King; Ace ] ->
              card rank suit ]
    |> List.filter (fun c -> not (List.contains c flushHand))

let private bestPlayTotal (i: Search.Input) =
    let _, _, best = (Search.plays i).Head
    best.Total

[<Fact>]
let ``plays the hand that clears the blind`` () =
    match Search.choose (input fixtureHand [] 4 3 300.0) with
    | [ Search.PlayCards(indices, _, best), outlook ], rule ->
        Assert.Equal<int list>([ 0; 1; 4; 5; 6 ], indices)
        Assert.Equal(336.0, best.Total)
        Assert.Equal(1.0, outlook.Clears)
        Assert.Equal("clears the blind now", rule)
    | other -> failwithf "unexpected %A" other

[<Fact>]
let ``discards towards a flush when the deck is full of hearts`` () =
    let i = input heartsHand heartsDeck 4 1 100000.0

    match Search.choose i with
    | (Search.DiscardCards(indices, _), _) :: _, rule ->
        Assert.All(indices, fun p -> Assert.Contains(p, [ 0; 4; 5; 6; 7 ]))
        Assert.StartsWith("highest simulated chance to clear", rule)
    | other -> failwithf "unexpected %A" other

[<Fact>]
let ``keeps a made flush intact`` () =
    let i = input flushHand flushDeck 4 4 300.0

    match Search.choose i with
    | (Search.PlayCards(indices, _, _), _) :: _, _ -> Assert.All([ 0; 1; 2; 3; 4 ], fun p -> Assert.Contains(p, indices))
    | (Search.DiscardCards(indices, _), _) :: _, _ -> Assert.All(indices, fun p -> Assert.Contains(p, [ 5; 6; 7 ]))
    | other -> failwithf "unexpected %A" other

[<Fact>]
let ``a rollout never uses more hands than are left`` () =
    let i = input flushHand flushDeck 4 4 300.0

    for _, outlook in Search.evaluate i do
        Assert.True(outlook.MeanHands <= 4.0)

[<Fact>]
let ``with no discards left every candidate is a play and dead cards pad a hand`` () =
    let i = input flushHand flushDeck 4 0 100000.0

    let plays =
        Search.evaluate i
        |> List.map (fun (candidate, _) ->
            match candidate with
            | Search.PlayCards(indices, _, b) -> b.Total, indices.Length
            | Search.DiscardCards _ -> failwith "unexpected discard")

    Assert.True(
        plays
        |> List.exists (fun (total, size) -> size = 5 && plays |> List.exists (fun (t, n) -> t = total && n < 5))
    )

[<Fact>]
let ``an empty deck and one hand left cannot clear and uses one hand`` () =
    let i = input heartsHand [] 1 0 100000.0

    for _, outlook in Search.evaluate i do
        Assert.Equal(0.0, outlook.Clears)
        Assert.Equal(1.0, outlook.MeanHands)

    Assert.True(snd (Search.discards i).Head <= bestPlayTotal i)

[<Fact>]
let ``only the last simulated hand is scored with no hands left`` () =
    let i =
        { input flushHand flushDeck 3 1 100000.0 with
            Score =
                fun handsLeftAfter _ played held ->
                    let b = Scoring.score { Played = played; Held = held; Levels = levels }
                    { b with Total = if handsLeftAfter = 0 then 1000.0 else 1.0 } }

    for _, outlook in Search.evaluate i do
        Assert.Equal(1002.0, outlook.MeanChips)

[<Fact>]
let ``simulated plays are scored with the discards left at the time of the play`` () =
    let i =
        { input flushHand flushDeck 2 3 100000.0 with
            Score =
                fun _ discardsLeft played held ->
                    let b = Scoring.score { Played = played; Held = held; Levels = levels }
                    { b with Total = 100.0 * float discardsLeft } }

    for candidate, outlook in Search.evaluate i do
        match candidate with
        | Search.PlayCards _ -> Assert.Equal(600.0, outlook.MeanChips)
        | Search.DiscardCards _ -> Assert.Equal(400.0, outlook.MeanChips)

[<Fact>]
let ``the same input gives the same results`` () =
    let i = input heartsHand heartsDeck 4 1 100000.0
    Assert.True((Search.discards i = Search.discards i))
    Assert.True((Search.choose i = Search.choose i))

[<Fact>]
let ``every subset of one to five cards is a discard candidate`` () =
    Assert.Equal(218, (Search.discards (input heartsHand heartsDeck 4 1 100000.0)).Length)

[<Fact>]
let ``policy still plays the fixture hand`` () =
    let text = File.ReadAllText(AppContext.BaseDirectory + "fixtures/gamestate.SELECTING_HAND.json")

    match Api.decodeGameState (JsonDocument.Parse(text).RootElement) with
    | Ok g ->
        let action, decision = Policy.decide 1 g
        Assert.Equal(Policy.Play [ 0; 1; 4; 5; 6 ], action)
        Assert.Equal(Some 336.0, decision.Predicted)
    | Error e -> failwith e

let private goldenInput =
    let joker key : Jokers.Joker =
        { Key = key
          Name = key
          Edition = None
          Debuffed = false
          Value = None
          SellValue = 1
          Text = "" }

    let hand =
        [ card King Spades
          { card King Hearts with Seal = Some Seal.Red }
          card Queen Diamonds
          card Seven Clubs
          { card Seven Spades with Edition = Some Foil }
          card Five Hearts
          card Three Diamonds
          { card King Clubs with Enhancement = Some Enhancement.Steel } ]

    let deck =
        [ for suit in [ Spades; Hearts; Clubs; Diamonds ] do
              for rank in [ Two; Three; Four; Five; Six; Seven; Eight; Nine; Ten; Jack; Queen; King; Ace ] ->
                  card rank suit ]
        |> List.filter (fun c -> not (hand |> List.exists (fun h -> h.Rank = c.Rank && h.Suit = c.Suit)))

    let situation =
        { Jokers.Situation.empty with
            Jokers = [ joker "j_joker"; joker "j_jolly"; joker "j_baron"; joker "j_blueprint"; joker "j_hanging_chad" ]
            JokerSlots = 5
            DeckCount = 44 }

    { input hand deck 4 3 1e9 with
        Seed = 100
        RedrawSamples = 8
        Score =
            fun handsLeft discardsLeft played held ->
                Scoring.scoreWith
                    { situation with
                        HandsLeft = handsLeft
                        DiscardsLeft = discardsLeft }
                    { Played = played; Held = held; Levels = levels } }

[<Fact>]
let ``evaluate keeps its outlooks with five jokers`` () =
    let actual =
        Search.evaluate goldenInput
        |> List.map (fun (candidate, outlook) ->
            match candidate with
            | Search.PlayCards(indices, _, _) -> "play", indices, outlook.Clears, outlook.MeanHands, outlook.MeanChips
            | Search.DiscardCards(indices, _) -> "discard", indices, outlook.Clears, outlook.MeanHands, outlook.MeanChips)

    let expected =
        [ "discard", [ 2; 3; 5; 6 ], 0.0, 4.0, 9506.775
          "discard", [ 0; 2; 3; 5; 6 ], 0.0, 4.0, 8388.4
          "discard", [ 2; 6 ], 0.0, 4.0, 8293.175
          "discard", [ 2; 5 ], 0.0, 4.0, 8172.625
          "discard", [ 2; 3 ], 0.0, 4.0, 8067.225
          "discard", [ 5; 6 ], 0.0, 4.0, 8033.4
          "play", [ 4; 5; 6 ], 0.0, 4.0, 7427.0
          "play", [ 4 ], 0.0, 4.0, 6993.175
          "play", [ 2; 3; 4; 5; 6 ], 0.0, 4.0, 6822.1
          "play", [ 3; 4 ], 0.0, 4.0, 6590.5
          "play", [ 0; 1; 3; 4; 7 ], 0.0, 4.0, 5758.675 ]

    Assert.Equal<(string * int list * float * float * float) list>(expected, actual)

[<Fact>]
let ``the first discards and plays keep their order and values with five jokers`` () =
    let discards = Search.discards goldenInput |> List.truncate 20
    let plays = Search.plays goldenInput |> List.truncate 20 |> List.map (fun (indices, _, b) -> indices, b.Total)

    let expectedDiscards =
        [ [ 2; 3; 5; 6 ], 4304.5
          [ 5; 6 ], 4174.75
          [ 0; 2; 3; 5; 6 ], 4147.5
          [ 2; 3 ], 4105.625
          [ 2; 5 ], 4105.625
          [ 2; 6 ], 4105.625
          [ 3; 5 ], 4105.625
          [ 3; 6 ], 4105.625
          [ 2; 3; 5 ], 4105.625
          [ 2; 3; 6 ], 4105.625
          [ 2; 5; 6 ], 4105.625
          [ 3; 5; 6 ], 4105.625
          [ 2 ], 3499.625
          [ 3 ], 3499.625
          [ 5 ], 3499.625
          [ 6 ], 3499.625
          [ 0; 2; 5; 6 ], 3399.625
          [ 0; 2; 3; 5 ], 3368.0
          [ 0; 2; 3; 6 ], 3368.0
          [ 0; 3; 5; 6 ], 3368.0 ]

    let expectedPlays =
        [ [ 4 ], 3362.0
          [ 4; 5 ], 3362.0
          [ 4; 6 ], 3362.0
          [ 4; 5; 6 ], 3362.0
          [ 0; 1; 3; 4; 7 ], 2944.0
          [ 3; 4 ], 2773.0
          [ 2; 3; 4 ], 2773.0
          [ 3; 4; 5 ], 2773.0
          [ 3; 4; 6 ], 2773.0
          [ 2; 3; 4; 5 ], 2773.0
          [ 2; 3; 4; 6 ], 2773.0
          [ 3; 4; 5; 6 ], 2773.0
          [ 2; 3; 4; 5; 6 ], 2773.0
          [ 0; 1; 3; 4 ], 2541.0
          [ 0; 1; 2; 3; 4 ], 2541.0
          [ 0; 1; 3; 4; 5 ], 2541.0
          [ 0; 1; 3; 4; 6 ], 2541.0
          [ 0; 3; 4; 7 ], 2376.0
          [ 0; 2; 3; 4; 7 ], 2376.0
          [ 0; 3; 4; 5; 7 ], 2376.0 ]

    Assert.Equal<(int list * float) list>(expectedDiscards, discards)
    Assert.Equal<(int list * float) list>(expectedPlays, plays)
