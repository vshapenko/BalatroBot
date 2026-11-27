module Balatro.Tests.ShopSpeedTests

open System
open System.IO
open System.Text.Json
open System.Threading.Tasks
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

let private build : Build =
    { Jokers =
        [ joker "j_joker"
          { joker "j_popcorn" with Text = "+20 Mult -4 Mult per round played" } ]
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

let private offers =
    [ JokerOffer(0, joker "j_jolly", 3)
      JokerOffer(1, joker "j_blueprint", 5)
      JokerOffer(2, { joker "j_ice_cream" with Text = "+100 Chips -5 Chips for every hand played" }, 5)
      JokerOffer(3, { joker "j_joker" with Value = Some 4.0 }, 4)
      PlanetOffer(4, Flush, "Jupiter", 3)
      TarotOffer(5, "The World", 3, deck |> List.mapi (fun i c -> if i < 3 then { c with Suit = Spades } else c)) ]

// a seed no other test uses, so the first call computes every round
let private evaluated () =
    evaluate 64064 2 20 (Some "The Club") build offers
    |> List.map (fun e -> position e.Offer, e.Before, e.After, e.InterestLost)

// recorded from the sequential implementation
let private expected =
    [ 2, 5544.0, 13185.666666666668, 1
      1, 5544.0, 9241.5, 1
      4, 5544.0, 6568.0, 1
      3, 5544.0, 6548.0, 1
      0, 5544.0, 6466.5, 1
      5, 5544.0, 5544.0, 1 ]

[<Fact>]
let ``evaluate returns the recorded list, concurrently and on every repeat`` () =
    let threads = Array.init 4 (fun _ -> Task.Run(fun () -> evaluated ()))
    Task.WaitAll(threads |> Array.map (fun t -> t :> Task))
    Assert.All(threads, fun t -> Assert.Equal<(int * float * float * int) list>(expected, t.Result))

    for _ in 1..20 do
        Assert.Equal<(int * float * float * int) list>(expected, evaluated ())

// recorded from the code before the offers were valued in parallel
let private decisions =
    Map.ofList
        [ "gamestate.SHOP.json",
          ("NextRound",
           "#1 ante 1 round 1 Shop: next round (939); rule: best offer The World raises the simulated round score by 2%, below the 2% threshold; valued against The Hook: mean of a normal round and the boss round; Jupiter skipped: keeps $6 for a joker while a slot is free; plan: Flush in Spades (34% of simulated plays); experts: Survival +2 (round score 916 -> 939), Economy -2 (bar +2% base, $0 interest lost), Plan +0 (off plan), Hunt +0 (no hunt); seen: The World $3, Jupiter $3, Reroll Surplus $10, Buffoon Pack $4, Jumbo Celestial Pack $6")
          "gamestate.SHOP.with_joker.json",
          ("NextRound",
           "#1 ante 1 round 1 Shop: next round (4699); rule: best offer The World raises the simulated round score by 1%, below the 2% threshold; valued against The Hook: mean of a normal round and the boss round; Jupiter skipped: keeps $6 for a joker while a slot is free; plan: Flush in Spades (34% of simulated plays); experts: Survival +1 (round score 4654 -> 4699), Economy -2 (bar +2% base, $0 interest lost), Plan +0 (off plan), Hunt +0 (no hunt); seen: The World $3, Jupiter $3, Reroll Surplus $10, Jumbo Celestial Pack $6")
          "gamestate.SMODS_BOOSTER_OPENED.json",
          ("PickPack 0",
           "#1 ante 1 round 1 SmodsBoosterOpened: pick Popcorn; rule: raises the simulated round score from 916 to 4654 (+408%); seen: Popcorn, Mad Joker") ]

let private load (path: string) =
    match Api.decodeGameState (JsonDocument.Parse(File.ReadAllText path).RootElement) with
    | Ok g -> g
    | Error e -> failwithf "%s: %s" path e

[<Fact>]
let ``shop and pack decisions on every fixture are the recorded ones`` () =
    let states =
        Directory.GetFiles(AppContext.BaseDirectory + "fixtures", "gamestate.*.json")
        |> Array.map (fun path -> Path.GetFileName path, load path)
        |> Array.filter (fun (_, g) -> g.State = Api.State.Shop || g.State = Api.State.SmodsBoosterOpened)

    Assert.Equal<string list>(decisions.Keys |> List.ofSeq, states |> Array.map fst |> Array.sort |> List.ofArray)

    for name, g in states do
        let action, decision = Policy.decideWith None 1 g
        Assert.Equal(fst decisions[name], sprintf "%A" action)
        Assert.Equal(snd decisions[name], Decision.toText decision)
