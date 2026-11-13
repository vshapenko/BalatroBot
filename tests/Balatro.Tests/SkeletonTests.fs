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
    Assert.Equal(Policy.NextRound, fst (Policy.decide 1 (load "SHOP")))
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
