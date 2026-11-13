module Balatro.Tests.ShopTests

open System
open System.IO
open System.Text.Json
open Xunit
open Balatro.Core
open Balatro.Core.Domain
open Balatro.Bot
open Balatro.Bot.Shop
open Balatro.Tests.HandsTests

let private joker key : Jokers.Joker =
    { Key = key
      Name = key
      Edition = None
      Debuffed = false
      Value = None
      SellValue = 0
      Text = "" }

let private deck =
    [ for suit in [ Spades; Hearts; Clubs; Diamonds ] do
          for rank in [ Two; Three; Four; Five; Six; Seven; Eight; Nine; Ten; Jack; Queen; King; Ace ] do
              yield card rank suit ]

let private build jokers : Build =
    { Jokers = jokers
      Levels =
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
        |> List.map (fun t -> t, handLevel t 1)
        |> Map.ofList
      Deck = deck
      HandSize = 8
      Hands = 4
      Situation = { Jokers.Situation.empty with JokerSlots = 5 } }

let private jupiter cost = PlanetOffer(1, Flush, "Jupiter", cost)

let private entry gain lost : Evaluated =
    { Offer = jupiter 3
      Before = 100.0
      After = 100.0 * gain
      InterestLost = lost }

[<Fact>]
let ``round score is deterministic and rises with a joker`` () =
    let plain = roundScore 100 4 (build [])
    Assert.Equal(plain, roundScore 100 4 (build []))
    Assert.True(roundScore 100 4 (build [ joker "j_joker" ]) > plain)

[<Fact>]
let ``evaluate keeps affordable offers that do not lower the score`` () =
    let offers =
        [ JokerOffer(0, joker "j_jolly", 3)
          jupiter 3
          JokerOffer(2, joker "j_joker", 11) ]

    let evaluated = evaluate 100 4 10 (build []) offers
    Assert.Equal<int list>([ 0; 1 ], evaluated |> List.map (fun e -> position e.Offer) |> List.sort)
    Assert.All(evaluated, fun e -> Assert.True(e.After >= e.Before))

[<Fact>]
let ``evaluate returns no joker offer when the joker slots are full`` () =
    let full = build (List.replicate 5 (joker "j_joker"))
    let evaluated = evaluate 100 2 10 full [ JokerOffer(0, joker "j_jolly", 3); jupiter 3 ]
    Assert.Equal<int list>([ 1 ], evaluated |> List.map (fun e -> position e.Offer))

[<Fact>]
let ``interest lost is the interest the purchase costs`` () =
    let lost money cost =
        (evaluate 100 1 money (build []) [ jupiter cost ]).Head.InterestLost

    Assert.Equal(1, lost 26 3)
    Assert.Equal(0, lost 30 3)
    Assert.Equal(0, lost 4 3)

[<Fact>]
let ``choose needs the gain to clear the interest adjusted threshold`` () =
    Assert.Equal(Some(entry 1.30 0), choose [ entry 1.30 0 ])
    Assert.Equal(None, choose [ entry 1.06 1 ])
    Assert.Equal(None, choose [])

[<Fact>]
let ``best order puts the additive joker before the multiplier`` () =
    Assert.Equal<int list>([ 1; 0 ], bestOrder 100 4 (build [ joker "j_duo"; joker "j_jolly" ]))

[<Fact>]
let ``policy never buys the tarot in the fixture shop`` () =
    let text = File.ReadAllText(AppContext.BaseDirectory + "fixtures/gamestate.SHOP.json")

    let g =
        match Api.decodeGameState (JsonDocument.Parse(text).RootElement) with
        | Ok g -> g
        | Error e -> failwith e

    let action, decision = Policy.decide 1 g
    Assert.True(action = Policy.BuyCard 1 || action = Policy.NextRound)
    Assert.Equal(None, decision.Predicted)

    let full = { g with Consumables = { g.Consumables with Count = g.Consumables.Limit } }
    Assert.Equal(Policy.NextRound, fst (Policy.decide 1 full))

    let jupiter = g.Shop.Cards[1]

    let earth =
        { jupiter with
            Key = "c_earth"
            Label = "Earth"
            Cost = { jupiter.Cost with Buy = 1 } }

    let rich = { g with Money = 26; Shop = { g.Shop with Cards = [ jupiter; earth ] } }
    let action, decision = Policy.decide 2 rich
    Assert.Equal(Policy.BuyCard 1, action)
    Assert.Equal("buy Earth for $1", decision.Chosen)
    Assert.Equal(decision.Chosen, decision.Candidates.Head.Action)
