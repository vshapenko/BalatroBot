module Balatro.Tests.PlanTests

open Xunit
open Balatro.Core
open Balatro.Core.Domain
open Balatro.Bot
open Balatro.Tests.HandsTests

let private ranks = [ Two; Three; Four; Five; Six; Seven; Eight; Nine; Ten; Jack; Queen; King; Ace ]

let private mixed =
    [ for suit in [ Spades; Hearts; Clubs; Diamonds ] do
          for rank in ranks do
              yield card rank suit ]

let private build jokers deck : Shop.Build =
    { Jokers = jokers
      Levels =
        [ FlushFive; FlushHouse; FiveOfAKind; StraightFlush; FourOfAKind; FullHouse; Flush; Straight; ThreeOfAKind; TwoPair; Pair; HighCard ]
        |> List.map (fun t -> t, handLevel t 1)
        |> Map.ofList
      Deck = deck
      HandSize = 8
      Hands = 4
      Situation = { Jokers.Situation.empty with JokerSlots = 5 } }

let private joker key name : Jokers.Joker =
    { Key = key
      Name = name
      Edition = None
      Debuffed = false
      Value = None
      SellValue = 0
      Text = "" }

[<Fact>]
let ``a joker-free build on a one-suit deck plans a flush in that suit`` () =
    let deck = [ for _ in 1..4 do for rank in ranks -> card rank Hearts ]
    let plan = Plan.make 100 (build [] deck)
    Assert.Equal(Flush, plan.Hand)
    Assert.Equal(Some Hearts, plan.Suit)
    Assert.Empty plan.Fans
    Assert.True(plan.Share > 0.8)

[<Fact>]
let ``Gluttonous Joker makes Clubs the plan suit and a fan`` () =
    let plan = Plan.make 100 (build [ joker "j_gluttenous_joker" "Gluttonous Joker" ] mixed)
    Assert.Equal(Some Clubs, plan.Suit)
    Assert.Contains("Gluttonous Joker", plan.Fans)

[<Fact>]
let ``the plan text names the suit, share and fans`` () =
    let flush: Plan.Plan =
        { Hand = Flush
          Suit = Some Hearts
          Share = 0.61
          Fans = [ "Flower Pot"; "Lusty Joker" ] }

    let pair: Plan.Plan = { Hand = Pair; Suit = None; Share = 0.48; Fans = [] }
    Assert.Equal("plan: Flush in Hearts (61% of simulated plays; fits Flower Pot, Lusty Joker)", Plan.text flush)
    Assert.Equal("plan: Pair (48% of simulated plays)", Plan.text pair)
