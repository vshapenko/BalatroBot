module Balatro.Tests.ExpertsTests

open System
open System.IO
open System.Text.Json
open Xunit
open Balatro.Core
open Balatro.Core.Domain
open Balatro.Bot
open Balatro.Bot.Decision

let private joker key : Jokers.Joker =
    { Key = key
      Name = key
      Edition = None
      Debuffed = false
      Value = None
      SellValue = 0
      Text = "" }

let private evaluated offer before after lost : Shop.Evaluated =
    { Offer = offer
      Before = before
      After = after
      InterestLost = lost }

let private plan: Plan.Plan =
    { Hand = Flush
      Suit = Some Spades
      Share = 0.34
      Level = 1
      Fans = [] }

let private load name =
    let text = File.ReadAllText(AppContext.BaseDirectory + "fixtures/gamestate." + name + ".json")

    match Api.decodeGameState (JsonDocument.Parse(text).RootElement) with
    | Ok g -> g
    | Error e -> failwithf "%s: %s" name e

[<Fact>]
let ``judge picks the same offer as Shop.choose`` () =
    let rng = Random(57)

    for _ in 1..2000 do
        let owned = rng.Next(0, 6)

        let l =
            [ for i in 1 .. rng.Next(0, 7) do
                  let before = 1.0 + rng.NextDouble() * 99999.0
                  let after = before * (0.8 + rng.NextDouble() * 0.7)
                  yield evaluated (Shop.PlanetOffer(i, Flush, "Jupiter", 3)) before after (rng.Next(0, 7)) ]

        Assert.Equal(Shop.choose owned l, Experts.judge owned l)

[<Fact>]
let ``views carry survival, economy and plan points`` () =
    let planet = evaluated (Shop.PlanetOffer(1, Flush, "Jupiter", 3)) 1000.0 1100.0 1
    let onPlan = Experts.views 2 plan planet

    Assert.Equal<string list>([ "Survival"; "Economy"; "Plan" ], onPlan |> List.map (fun v -> v.Expert))
    Assert.Equal(10.0, onPlan[0].Points, 9)
    Assert.Equal("round score 1000 -> 1100", onPlan[0].Why)
    Assert.Equal(-7.0, onPlan[1].Points, 9)
    Assert.Equal("bar +2% base, $1 interest lost", onPlan[1].Why)
    Assert.Equal(0.0, onPlan[2].Points)
    Assert.Equal("fits Flush", onPlan[2].Why)

    let off = evaluated (Shop.JokerOffer(0, joker "j_abstract", 4)) 1000.0 1100.0 0
    let offViews = Experts.views 4 plan off
    Assert.Equal("off plan", offViews[2].Why)

[<Fact>]
let ``toText adds experts only when present`` () =
    let d: Decision =
        { Seq = 3
          Ante = 1
          Round = 2
          Phase = "Shop"
          Chosen = "buy x"
          Rule = "because"
          Candidates = []
          Seen = [ "x $3" ]
          Experts = []
          Predicted = None }

    Assert.Equal(sprintf "#%d ante %d round %d %s: %s%s; rule: %s%s" 3 1 2 "Shop" "buy x" "" "because" "; seen: x $3", toText d)

    let planet = evaluated (Shop.PlanetOffer(1, Flush, "Jupiter", 3)) 1000.0 1100.0 1
    let withExperts = toText { d with Experts = Experts.views 2 plan planet }

    Assert.Contains("; experts: Survival +10 (round score 1000 -> 1100), Economy -7 (", withExperts)
    Assert.EndsWith("; seen: x $3", withExperts)

[<Fact>]
let ``shop fixtures keep the actions of the previous rule`` () =
    Assert.Equal(Policy.NextRound, fst (Policy.decide 1 (load "SHOP")))
    Assert.Equal(Policy.NextRound, fst (Policy.decide 1 (load "SHOP.with_joker")))
