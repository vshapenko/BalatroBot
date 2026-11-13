module Balatro.Tests.ApiTests

open System
open System.IO
open System.Text.Json
open System.Text.Json.Nodes
open Xunit
open Balatro.Bot.Api

let private text name =
    File.ReadAllText(AppContext.BaseDirectory + "fixtures/" + name)

let private load name =
    match decodeGameState (JsonDocument.Parse(text name).RootElement) with
    | Ok g -> g
    | Error e -> failwithf "%s: %s" name e

[<Fact>]
let ``each gamestate fixture decodes to its named state`` () =
    [ "BLIND_SELECT", State.BlindSelect
      "GAME_OVER", State.GameOver
      "MENU", State.Menu
      "ROUND_EVAL", State.RoundEval
      "ROUND_EVAL.with_joker", State.RoundEval
      "SELECTING_HAND", State.SelectingHand
      "SELECTING_HAND.with_joker", State.SelectingHand
      "SHOP", State.Shop
      "SHOP.with_joker", State.Shop
      "SMODS_BOOSTER_OPENED", State.SmodsBoosterOpened ]
    |> List.iter (fun (name, expected) ->
        Assert.Equal(expected, (load ("gamestate." + name + ".json")).State))

[<Fact>]
let ``selecting hand fixture`` () =
    let g = load "gamestate.SELECTING_HAND.json"
    Assert.Equal(8, g.Hand.Cards.Length)
    let first = g.Hand.Cards.Head
    Assert.Equal("S_K", first.Key)
    Assert.Equal(Some Rank.King, first.Value.Rank)
    Assert.Equal(Some Suit.Spades, first.Value.Suit)
    Assert.Equal(44, g.Cards.Count)
    let pair = g.Hands["Pair"]
    Assert.Equal(10, pair.Chips)
    Assert.Equal(2, pair.Mult)
    Assert.Equal(1, pair.Level)
    Assert.Equal(4, g.Round.HandsLeft)
    Assert.Equal(4, g.Round.DiscardsLeft)

[<Fact>]
let ``blind select fixture`` () =
    let g = load "gamestate.BLIND_SELECT.json"
    let blinds = g.Blinds.Value
    Assert.Equal(300, blinds.Small.Score)
    Assert.Equal(BlindStatus.Select, blinds.Small.Status)
    Assert.Equal("The Hook", blinds.Boss.Name)
    Assert.Equal(4, g.Money)
    Assert.Equal(Some Deck.Red, g.Deck)
    Assert.Equal(Some Stake.White, g.Stake)
    Assert.Equal(Some "FIXTURE1", g.Seed)

[<Fact>]
let ``shop fixtures`` () =
    let withJoker = load "gamestate.SHOP.with_joker.json"
    Assert.Equal(1, withJoker.Jokers.Cards.Length)
    Assert.Equal("j_popcorn", withJoker.Jokers.Cards.Head.Key)
    Assert.Equal(2, withJoker.Jokers.Cards.Head.Cost.Sell)
    let shop = load "gamestate.SHOP.json"
    Assert.Equal(2, shop.Shop.Cards.Length)
    Assert.Equal(1, shop.Vouchers.Cards.Length)
    Assert.Equal(10, shop.Vouchers.Cards.Head.Cost.Buy)
    Assert.Equal(2, shop.Packs.Cards.Length)

[<Fact>]
let ``non playing card enhancement label decodes as none`` () =
    let shop = load "gamestate.SHOP.json"
    let world = shop.Shop.Cards |> List.find (fun c -> c.Key = "c_world")
    Assert.Equal(None, world.Modifier.Enhancement)

[<Fact>]
let ``booster fixture`` () =
    let g = load "gamestate.SMODS_BOOSTER_OPENED.json"
    Assert.Equal(2, g.Pack.Cards.Length)
    Assert.Equal("j_popcorn", g.Pack.Cards.Head.Key)
    let mad = g.Pack.Cards |> List.find (fun c -> c.Key = "j_mad")
    Assert.Equal(Some Edition.Foil, mad.Modifier.Edition)

[<Fact>]
let ``error response decodes to rpc error`` () =
    match decodeResponse (text "error.BAD_REQUEST.json") with
    | Error(Rpc e) ->
        Assert.Equal(-32001, e.Code)
        Assert.Equal("BAD_REQUEST", e.Name)
        Assert.Equal("You can only play 5 cards", e.Message)
    | other -> failwithf "unexpected %A" other

[<Fact>]
let ``play request body`` () =
    let cards =
        JsonArray([| for i in [ 4; 5; 6 ] -> JsonValue.Create i :> JsonNode |]) :> JsonNode

    let body = JsonDocument.Parse(encodeRequest "play" [ "cards", cards ]).RootElement
    Assert.Equal("2.0", body.GetProperty("jsonrpc").GetString())
    Assert.Equal("play", body.GetProperty("method").GetString())

    let sent =
        [ for c in body.GetProperty("params").GetProperty("cards").EnumerateArray() -> c.GetInt32() ]

    Assert.Equal<int list>([ 4; 5; 6 ], sent)
    Assert.True(body.TryGetProperty("id") |> fst)

[<Fact>]
let ``unknown state is an error`` () =
    let e =
        JsonDocument.Parse("""{"state":"NOPE","round_num":1,"ante_num":1,"money":0}""").RootElement

    match decodeGameState e with
    | Error m -> Assert.Contains("NOPE", m)
    | Ok _ -> failwith "expected Error"

[<Fact>]
let ``add and set encode their parameters`` () =
    let added = JsonDocument.Parse(encodeRequest "add" (addParameters "j_joker")).RootElement
    Assert.Equal("add", added.GetProperty("method").GetString())
    Assert.Equal("j_joker", added.GetProperty("params").GetProperty("key").GetString())

    let set = JsonDocument.Parse(encodeRequest "set" (setParameters [ "chips", -5; "hands", 4 ])).RootElement
    Assert.Equal("set", set.GetProperty("method").GetString())
    Assert.Equal(-5, set.GetProperty("params").GetProperty("chips").GetInt32())
    Assert.Equal(4, set.GetProperty("params").GetProperty("hands").GetInt32())
