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

let private input hand deck discardsLeft needed : Search.Input =
    { Hand = List.indexed hand
      Deck = deck
      DiscardsLeft = discardsLeft
      Needed = needed
      Score = fun played held -> Scoring.score { Played = played; Held = held; Levels = levels }
      Seed = 1
      Samples = 12 }

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

let private bestPlayTotal (i: Search.Input) =
    let _, _, best = (Search.plays i).Head
    best.Total

[<Fact>]
let ``plays the hand that clears the blind`` () =
    match Search.choose (input fixtureHand [] 3 300.0) with
    | Search.PlayNow(indices, _, best), rule ->
        Assert.Equal<int list>([ 0; 1; 4; 5; 6 ], indices)
        Assert.Equal(336.0, best.Total)
        Assert.Equal("clears the blind now", rule)
    | other -> failwithf "unexpected %A" other

[<Fact>]
let ``plays the best hand when no discards are left`` () =
    match Search.choose (input fixtureHand [] 0 100000.0) with
    | Search.PlayNow(indices, _, _), rule ->
        Assert.Equal<int list>([ 0; 1; 4; 5; 6 ], indices)
        Assert.Equal("no discards left", rule)
    | other -> failwithf "unexpected %A" other

[<Fact>]
let ``discards towards a flush when the deck is full of hearts`` () =
    let i = input heartsHand heartsDeck 1 100000.0

    match Search.choose i with
    | Search.DiscardNow(indices, value), rule ->
        Assert.All(indices, fun p -> Assert.Contains(p, [ 0; 4; 5; 6; 7 ]))
        Assert.True(value > bestPlayTotal i)
        Assert.StartsWith("expected best hand after redraw", rule)
    | other -> failwithf "unexpected %A" other

[<Fact>]
let ``the same input gives the same results`` () =
    let i = input heartsHand heartsDeck 1 100000.0
    Assert.True((Search.discards i = Search.discards i))
    Assert.True((Search.choose i = Search.choose i))

[<Fact>]
let ``every subset of one to five cards is a discard candidate`` () =
    Assert.Equal(218, (Search.discards (input heartsHand heartsDeck 1 100000.0)).Length)

[<Fact>]
let ``an empty deck never beats the best hand now`` () =
    let i = input heartsHand [] 1 100000.0
    Assert.True(snd (Search.discards i).Head <= bestPlayTotal i)

    match Search.choose i with
    | Search.PlayNow _, rule -> Assert.Equal("no discard is expected to improve on the best hand", rule)
    | other -> failwithf "unexpected %A" other

[<Fact>]
let ``policy still plays the fixture hand`` () =
    let text = File.ReadAllText(AppContext.BaseDirectory + "fixtures/gamestate.SELECTING_HAND.json")

    match Api.decodeGameState (JsonDocument.Parse(text).RootElement) with
    | Ok g ->
        let action, decision = Policy.decide 1 g
        Assert.Equal(Policy.Play [ 0; 1; 4; 5; 6 ], action)
        Assert.Equal(Some 336.0, decision.Predicted)
    | Error e -> failwith e
