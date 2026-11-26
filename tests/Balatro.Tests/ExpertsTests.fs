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

let private never (_: Shop.Evaluated) = false

let private margin owned (e: Shop.Evaluated) = (e.After / e.Before - Shop.threshold owned e) * 100.0

[<Fact>]
let ``judge without plan or hunt points picks when Shop.choose does and the largest margin`` () =
    let rng = Random(57)

    for _ in 1..2000 do
        let owned = rng.Next(0, 6)

        let l =
            [ for i in 1 .. rng.Next(0, 7) do
                  let before = 1.0 + rng.NextDouble() * 99999.0
                  let after = before * (0.8 + rng.NextDouble() * 0.7)
                  yield evaluated (Shop.PlanetOffer(i, Pair, "Mercury", 3)) before after (rng.Next(0, 7)) ]

        let chosen = Shop.choose owned l
        let judged = Experts.judge owned plan never l
        Assert.Equal(chosen.IsSome, judged.IsSome)

        match judged with
        | Some j ->
            for e in l do
                if Shop.choose owned [ e ] |> Option.isSome then
                    Assert.True(margin owned e <= margin owned j + 1e-9)
        | None -> ()

[<Fact>]
let ``on-plan joker at +4% beats off-plan joker at +6%: 4 - 5 + 3 = 2 > 6 - 5 = 1`` () =
    let onPlan = evaluated (Shop.JokerOffer(0, joker "j_arrowhead", 4)) 1000.0 1040.0 0
    let offPlan = evaluated (Shop.JokerOffer(1, joker "j_abstract", 4)) 1000.0 1060.0 0
    Assert.Equal(Some onPlan, Experts.judge 3 plan never [ offPlan; onPlan ])

[<Fact>]
let ``hunting X-Mult joker at +2% beats plain joker at +6%: 2 - 5 + 5 = 2 > 6 - 5 = 1`` () =
    let xmult = evaluated (Shop.JokerOffer(0, joker "j_cavendish", 4)) 1000.0 1020.0 0
    let plain = evaluated (Shop.JokerOffer(1, joker "j_abstract", 4)) 1000.0 1060.0 0
    let wanted (e: Shop.Evaluated) = e = xmult
    Assert.Equal(Some xmult, Experts.judge 3 plan wanted [ plain; xmult ])
    Assert.Equal(Some plain, Experts.judge 3 plan never [ plain; xmult ])

[<Fact>]
let ``wanted X-Mult joker at +10% beats plain joker at +300% by priority`` () =
    let xmult = evaluated (Shop.JokerOffer(0, joker "j_cavendish", 4)) 1000.0 1100.0 0
    let plain = evaluated (Shop.JokerOffer(1, joker "j_abstract", 4)) 1000.0 4000.0 0
    let wanted (e: Shop.Evaluated) = e = xmult
    Assert.Equal(Some xmult, Experts.judge 3 plan wanted [ plain; xmult ])

[<Fact>]
let ``nothing passes when every total is below 0`` () =
    let a = evaluated (Shop.JokerOffer(0, joker "j_abstract", 4)) 1000.0 1030.0 0
    let b = evaluated (Shop.JokerOffer(1, joker "j_abstract", 4)) 1000.0 1000.0 0
    Assert.Equal(None, Experts.judge 3 plan never [ a; b ])

[<Fact>]
let ``views carry survival, economy, plan and hunt points`` () =
    let planet = evaluated (Shop.PlanetOffer(1, Flush, "Jupiter", 3)) 1000.0 1100.0 1
    let onPlan = Experts.views 2 plan false planet

    Assert.Equal<string list>([ "Survival"; "Economy"; "Plan"; "Hunt" ], onPlan |> List.map (fun v -> v.Expert))
    Assert.Equal(10.0, onPlan[0].Points, 9)
    Assert.Equal("round score 1000 -> 1100", onPlan[0].Why)
    Assert.Equal(-7.0, onPlan[1].Points, 9)
    Assert.Equal("bar +2% base, $1 interest lost", onPlan[1].Why)
    Assert.Equal(Experts.planWeight, onPlan[2].Points)
    Assert.Equal("fits Flush", onPlan[2].Why)
    Assert.Equal(0.0, onPlan[3].Points)
    Assert.Equal("no hunt", onPlan[3].Why)

    let off = evaluated (Shop.JokerOffer(0, joker "j_abstract", 4)) 1000.0 1100.0 0
    let offViews = Experts.views 4 plan true off
    Assert.Equal(0.0, offViews[2].Points)
    Assert.Equal("off plan", offViews[2].Why)
    Assert.Equal(Experts.huntWeight, offViews[3].Points)
    Assert.Equal("X-Mult joker wanted (takes priority)", offViews[3].Why)

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
    let withExperts = toText { d with Experts = Experts.views 2 plan false planet }

    Assert.Contains("; experts: Survival +10 (round score 1000 -> 1100), Economy -7 (", withExperts)
    Assert.EndsWith("; seen: x $3", withExperts)

[<Fact>]
let ``shop fixtures keep the actions of the previous rule`` () =
    Assert.Equal(Policy.NextRound, fst (Policy.decide 1 (load "SHOP")))
    Assert.Equal(Policy.NextRound, fst (Policy.decide 1 (load "SHOP.with_joker")))
