module Balatro.Tests.SkeletonTests

open System
open System.IO
open System.Text.Json
open Xunit
open Balatro.Core.Domain
open Balatro.Bot
open Balatro.Bot.Decision

let private load name =
    let text = File.ReadAllText(AppContext.BaseDirectory + "fixtures/gamestate." + name + ".json")

    match Api.decodeGameState (JsonDocument.Parse(text).RootElement) with
    | Ok g -> g
    | Error e -> failwithf "%s: %s" name e

let private play () = Policy.decide 1 (load "SELECTING_HAND")

[<Fact>]
let ``wire hand cards map to domain cards`` () =
    let cards = (load "SELECTING_HAND").Hand.Cards |> List.map Mapping.toCard
    Assert.Equal(8, cards |> List.choose id |> List.length)
    let first = cards.Head.Value
    Assert.Equal(King, first.Rank)
    Assert.Equal(Spades, first.Suit)
    Assert.False first.Debuffed
    Assert.Equal(None, first.Enhancement)

[<Fact>]
let ``levels cover all twelve hands`` () =
    let levels = Mapping.levels (load "SELECTING_HAND")
    Assert.Equal(12, levels.Count)
    Assert.Equal(10.0, levels[Pair].Chips)
    Assert.Equal(2.0, levels[Pair].Mult)

[<Fact>]
let ``policy plays the best immediate hand`` () =
    let action, decision = play ()
    Assert.Equal(Policy.Play [ 0; 1; 4; 5; 6 ], action)
    Assert.Equal(Some 336.0, decision.Predicted)
    Assert.True(decision.Candidates.Length <= 5)
    Assert.Equal(336.0, decision.Candidates.Head.Value)

[<Fact>]
let ``policy handles the other phases`` () =
    Assert.Equal(Policy.SelectBlind, fst (Policy.decide 1 (load "BLIND_SELECT")))
    Assert.Equal(Policy.CashOut, fst (Policy.decide 1 (load "ROUND_EVAL")))
    Assert.Equal(Policy.Stop, fst (Policy.decide 1 (load "GAME_OVER")))

[<Fact>]
let ``decision renders as json and text`` () =
    let _, decision = play ()
    use json = JsonDocument.Parse(Decision.toJson decision)
    Assert.Equal("decision", json.RootElement.GetProperty("kind").GetString())
    Assert.Equal(JsonValueKind.Array, json.RootElement.GetProperty("candidates").ValueKind)
    let text = Decision.toText decision
    Assert.Contains("Full House", text)
    Assert.Contains("336", text)

[<Fact>]
let ``outcome text says exact or mismatch`` () =
    Assert.Contains("exact", Decision.outcomeToText { Seq = 1; Predicted = 336.0; Actual = 336.0 })
    Assert.Contains("MISMATCH", Decision.outcomeToText { Seq = 1; Predicted = 336.0; Actual = 300.0 })

[<Fact>]
let ``blind select names the blind to select`` () =
    let small = load "BLIND_SELECT"
    let action, decision = Policy.decide 1 small
    Assert.Equal(Policy.SelectBlind, action)
    Assert.Equal("select Small Blind (target 300)", decision.Chosen)
    let b = small.Blinds.Value

    let boss =
        { small with
            Blinds =
                Some
                    { Small = { b.Small with Status = Api.BlindStatus.Defeated }
                      Big = { b.Big with Status = Api.BlindStatus.Defeated }
                      Boss = { b.Boss with Status = Api.BlindStatus.Select } } }

    let action, decision = Policy.decide 1 boss
    Assert.Equal(Policy.SelectBlind, action)
    Assert.Equal("select The Hook (target 600): Discards 2 random cards per hand played", decision.Chosen)

[<Fact>]
let ``decisions list the offers seen in the shop only`` () =
    let g = load "SHOP"
    let first = g.Shop.Cards.Head
    let _, shop = Policy.decide 1 g
    Assert.NotEmpty shop.Seen
    Assert.Equal(sprintf "%s $%d" first.Label first.Cost.Buy, shop.Seen.Head)
    Assert.Contains("; seen: ", Decision.toText shop)
    let _, hand = play ()
    Assert.Empty hand.Seen
    Assert.DoesNotContain("seen:", Decision.toText hand)

let private jokerCard (template: Api.Card) key cost =
    { template with
        Key = key
        Label = key
        Set = Api.CardSet.Joker
        Cost = { Sell = cost / 2; Buy = cost } }

let private huntShop ante owned =
    let g = load "SHOP"
    let t = g.Shop.Cards.Head

    { g with
        AnteNum = ante
        Money = 20
        Jokers = { g.Jokers with Cards = owned |> List.map (fun k -> jokerCard t k 4); Count = owned.Length }
        Shop = { g.Shop with Cards = [ jokerCard t "j_stuntman" 7; jokerCard t "j_cavendish" 4 ] } }

[<Fact>]
let ``hunting buys an X-Mult joker over a bigger flat gain`` () =
    let action, decision = Policy.decide 1 (huntShop 3 [])
    Assert.Equal(Policy.BuyCard 1, action)
    Assert.Contains("goal: find an X-Mult joker (none owned in ante 3)", decision.Rule)

[<Fact>]
let ``an owned X-Mult joker ends the hunt`` () =
    let action, decision = Policy.decide 1 (huntShop 3 [ "j_cavendish" ])
    Assert.Equal(Policy.BuyCard 0, action)
    Assert.DoesNotContain("goal:", decision.Rule)

[<Fact>]
let ``hunting rerolls down to ten dollars only in antes 2 to 4`` () =
    let empty ante =
        let g = huntShop ante []
        { g with
            Shop = { g.Shop with Cards = [] }
            Packs = { g.Packs with Cards = [] } }

    let action, decision = Policy.decide 1 (empty 3)
    Assert.Equal(Policy.Reroll, action)
    Assert.Contains("stays at or above $10 while hunting for an X-Mult joker", decision.Rule)
    Assert.NotEqual(Policy.Reroll, fst (Policy.decide 1 (empty 5)))

    let g = empty 3
    let celestial = { (load "SHOP").Packs.Cards.Head with Key = "p_celestial_normal_1"; Cost = { Sell = 2; Buy = 4 } }
    let withPack = { g with Packs = { g.Packs with Cards = [ celestial ] } }
    Assert.Equal(Policy.Reroll, fst (Policy.decide 1 withPack))
