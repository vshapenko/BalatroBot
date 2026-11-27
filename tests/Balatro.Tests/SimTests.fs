module Balatro.Tests.SimTests

open System
open System.Diagnostics
open System.IO
open Xunit
open Xunit.Abstractions
open Balatro.Core
open Balatro.Bot

let private ok (result: Result<Api.GameState, Api.ApiError>) =
    match result with
    | Ok g -> g
    | Error e -> failwithf "%A" e

let private started seed =
    let game = Sim.create ()
    ok (game.Menu ()) |> ignore
    game, ok (game.Start Api.Deck.Red Api.Stake.White (Some seed))

let private patched (change: Sim.Run -> Sim.Run) =
    Sim.gameFrom (Some(change (Sim.newRun "S1")))

let private isBadRequest (result: Result<Api.GameState, Api.ApiError>) =
    match result with
    | Error(Api.Rpc e) -> e.Code = -32001 && e.Name = "BAD_REQUEST"
    | _ -> false

let private tempDir () =
    Path.Combine(Path.GetTempPath(), "balatro-sim-" + Guid.NewGuid().ToString("N"))

[<Fact>]
let ``a new run starts at the first blind select with the Red Deck White Stake setup`` () =
    let _, g = started "S1"
    Assert.Equal(Api.State.BlindSelect, g.State)
    Assert.Equal(1, g.AnteNum)
    Assert.Equal(4, g.Money)
    Assert.False g.Won
    Assert.Equal(52, g.Cards.Count)
    Assert.Equal(52, g.Cards.Cards.Length)
    Assert.Equal(8, g.Hand.Limit)
    Assert.Equal(4, g.Round.HandsLeft)
    Assert.Equal(4, g.Round.DiscardsLeft)
    Assert.Equal(5, g.Jokers.Limit)
    Assert.Equal(2, g.Consumables.Limit)
    Assert.Equal(12, g.Hands.Count)
    let blinds = g.Blinds.Value
    Assert.Equal(Api.BlindStatus.Select, blinds.Small.Status)
    Assert.Equal(300, blinds.Small.Score)
    Assert.Equal(450, blinds.Big.Score)
    Assert.Equal(Api.BlindStatus.Upcoming, blinds.Boss.Status)

[<Fact>]
let ``selecting the small blind deals eight cards from a draw pile of 44`` () =
    let game, _ = started "S1"
    let g = ok (game.Select ())
    Assert.Equal(Api.State.SelectingHand, g.State)
    Assert.Equal(1, g.RoundNum)
    Assert.Equal(8, g.Hand.Cards.Length)
    Assert.Equal(44, g.Cards.Cards.Length)
    Assert.Equal(Api.BlindStatus.Current, g.Blinds.Value.Small.Status)

[<Fact>]
let ``only the Red Deck on White Stake can be started`` () =
    let game = Sim.create ()
    ok (game.Menu ()) |> ignore
    Assert.True(isBadRequest (game.Start Api.Deck.Blue Api.Stake.White (Some "S1")))
    Assert.True(isBadRequest (game.Start Api.Deck.Red Api.Stake.Gold (Some "S1")))

[<Fact>]
let ``boss targets match the blind table except The Wall, The Needle and Violet Vessel`` () =
    let special = set [ "The Wall"; "The Needle"; "Violet Vessel" ]

    for seed in 1..20 do
        for ante in 1..8 do
            let name = Sim.chooseBoss (Random seed) ante Map.empty

            let g =
                (patched (fun r -> { r with Ante = ante; Boss = name; Next = 2 })).State () |> ok

            let boss = g.Blinds.Value.Boss
            Assert.Equal(name, boss.Name)

            if not (special.Contains name) then
                Assert.Equal(int (Blinds.blindRequirement Domain.Stake.White ante Blinds.Boss), boss.Score)

    let base' ante = Blinds.blindAmount Domain.Stake.White ante

    let score name =
        ((patched (fun r -> { r with Ante = 4; Boss = name; Next = 2 })).State () |> ok).Blinds.Value.Boss.Score

    Assert.Equal(int (base' 4 * 4.0), score "The Wall")
    Assert.Equal(int (base' 4), score "The Needle")
    Assert.Equal(int (base' 4 * 6.0), score "Violet Vessel")

[<Fact>]
let ``ante 8 has a showdown boss`` () =
    let showdown = set [ "Cerulean Bell"; "Verdant Leaf"; "Violet Vessel"; "Amber Acorn"; "Crimson Heart" ]

    for seed in 1..20 do
        Assert.Contains(Sim.chooseBoss (Random seed) 8 Map.empty, showdown)

[<Fact>]
let ``a cash out pays the reward, a dollar per hand left and interest`` () =
    Assert.Equal(7, Sim.payout 3 2 0 12)
    Assert.Equal(10, Sim.payout 3 2 0 40)

    let cashOut money =
        let game =
            patched (fun r ->
                { r with
                    State = Api.State.RoundEval
                    Money = money
                    Reward = 3
                    Left = 2 })

        game.CashOut () |> ok

    let low = cashOut 12
    Assert.Equal(Api.State.Shop, low.State)
    Assert.Equal(12 + 7, low.Money)
    Assert.Equal(40 + 10, (cashOut 40).Money)

[<Fact>]
let ``entering the shop stocks two cards, two packs and the voucher`` () =
    let g =
        (patched (fun r -> { r with State = Api.State.RoundEval; Reward = 3 })).CashOut () |> ok

    Assert.Equal(2, g.Shop.Cards.Length)
    Assert.Equal(2, g.Packs.Cards.Length)
    Assert.Equal(1, g.Vouchers.Cards.Length)
    Assert.StartsWith("p_buffoon_normal", g.Packs.Cards.Head.Key)
    Assert.Equal(5, g.Round.RerollCost)

[<Fact>]
let ``illegal actions return BAD_REQUEST and leave the state unchanged`` () =
    let game, _ = started "S1"
    ok (game.Select ()) |> ignore
    let before = ok (game.State ())
    Assert.True(isBadRequest (game.Play [ 0; 1; 2; 3; 4; 5 ]))
    Assert.True(isBadRequest (game.Play [ 0; 0 ]))
    Assert.True(isBadRequest (game.Play [ 9 ]))
    Assert.True(isBadRequest (game.Buy(Api.BuyCard 0)))
    Assert.Equal(before, ok (game.State ()))

    let shop = patched (fun r -> { r with State = Api.State.RoundEval; Money = 0; Reward = 0 })
    let g = ok (shop.CashOut ())
    let broke = patched (fun r -> { r with State = Api.State.Shop; Money = 0; Shop = g.Shop.Cards; Packs = g.Packs.Cards })
    let stateBefore = ok (broke.State ())
    Assert.True(isBadRequest (broke.Buy(Api.BuyCard 0)))
    Assert.True(isBadRequest (broke.Buy(Api.BuyPack 0)))
    Assert.True(isBadRequest (broke.Reroll ()))
    Assert.Equal(stateBefore, ok (broke.State ()))

[<Fact>]
let ``a play spends a hand, scores and redraws`` () =
    let game, _ = started "S1"
    let dealt = ok (game.Select ())
    let g = ok (game.Play [ 0; 1; 2; 3; 4 ])
    Assert.Equal(8, dealt.Hand.Cards.Length)
    Assert.Equal(Api.State.SelectingHand, g.State)
    Assert.Equal(3, g.Round.HandsLeft)
    Assert.Equal(8, g.Hand.Cards.Length)
    Assert.Equal(39, g.Cards.Cards.Length)
    Assert.True(g.Round.Chips > 0)
    Assert.Equal(1, g.Hands.Values |> Seq.sumBy (fun h -> h.Played))

[<Fact>]
let ``a losing last play returns GameOver with the round's chips and hands`` () =
    let game =
        patched (fun r -> { r with Ante = 8; Boss = "The Needle"; Next = 2 })

    let dealt = ok (game.Select ())
    let picked = [ 0; 1; 2; 3; 4 ]
    let card i = Mapping.toCard dealt.Hand.Cards[i] |> Option.get

    let expected =
        Scoring.scoreWith
            (Mapping.situation dealt)
            { Played = picked |> List.map card
              Held = [ 5; 6; 7 ] |> List.map card
              Levels = Mapping.levels dealt }

    let g = ok (game.Play picked)
    Assert.Equal(Api.State.GameOver, g.State)
    Assert.Equal(int expected.Total, g.Round.Chips)
    Assert.True(g.Round.Chips > 0)
    Assert.Equal(0, g.Round.HandsLeft)

[<Fact>]
let ``a seed always deals the same first boss and hand`` () =
    let game, start = started "S1"
    let dealt = ok (game.Select ())
    Assert.Equal("The Head", start.Blinds.Value.Boss.Name)
    Assert.Equal("D_5 D_3 C_4 C_2 S_J S_8 C_Q D_J", dealt.Hand.Cards |> List.map (fun c -> c.Key) |> String.concat " ")

[<Fact>]
let ``a discard spends a discard and redraws`` () =
    let game, _ = started "S1"
    ok (game.Select ()) |> ignore
    let g = ok (game.Discard [ 0; 1 ])
    Assert.Equal(3, g.Round.DiscardsLeft)
    Assert.Equal(4, g.Round.HandsLeft)
    Assert.Equal(8, g.Hand.Cards.Length)
    Assert.Equal(42, g.Cards.Cards.Length)

let private bossRound name (change: Sim.Run -> Sim.Run) =
    let game = patched (fun r -> change { r with Boss = name; Next = 2 })
    game, ok (game.Select ())

[<Fact>]
let ``The Needle leaves one hand, The Water no discards, The Manacle a hand of seven`` () =
    let _, needle = bossRound "The Needle" id
    Assert.Equal(1, needle.Round.HandsLeft)
    let _, water = bossRound "The Water" id
    Assert.Equal(0, water.Round.DiscardsLeft)
    let _, manacle = bossRound "The Manacle" id
    Assert.Equal(7, manacle.Hand.Limit)
    Assert.Equal(7, manacle.Hand.Cards.Length)

[<Fact>]
let ``suit and face bosses debuff exactly their cards`` () =
    let debuffed name =
        let _, g = bossRound name id
        g.Hand.Cards |> List.filter (fun c -> c.State.Debuff)

    let all name = (snd (bossRound name id)).Hand.Cards

    Assert.All(debuffed "The Club", fun c -> Assert.Equal(Some Api.Suit.Clubs, c.Value.Suit))
    Assert.All(debuffed "The Head", fun c -> Assert.Equal(Some Api.Suit.Hearts, c.Value.Suit))

    Assert.All(
        debuffed "The Plant",
        fun c -> Assert.Contains(c.Value.Rank.Value, [ Api.Rank.Jack; Api.Rank.Queen; Api.Rank.King ])
    )

    Assert.Equal((all "The Goad" |> List.filter (fun c -> c.Value.Suit = Some Api.Suit.Spades)).Length, (debuffed "The Goad").Length)
    Assert.Empty(debuffed "The Hook")

[<Fact>]
let ``Verdant Leaf debuffs the hand until a joker is sold`` () =
    let game, g = bossRound "Verdant Leaf" id
    Assert.All(g.Hand.Cards, fun c -> Assert.True c.State.Debuff)
    let seeded = ok (game.State ())
    Assert.Equal(g, seeded)

    let joker =
        SimContent.jokers
        |> List.find (fun j -> j.Key = "j_joker")
        |> fun info -> SimContent.jokerCard info None

    let table: SimJokers.Table =
        { Money = 4
          Deck = []
          DrawPile = 0
          JokerSlots = 5
          Jokers = []
          HandPlays = Map.empty
          Skips = 0 }

    let withJoker =
        patched (fun r ->
            { r with
                Boss = "Verdant Leaf"
                Next = 2
                Jokers = [ SimJokers.create (Random 1) table joker ] })

    ok (withJoker.Select ()) |> ignore
    let after = ok (withJoker.Sell(Api.SellJoker 0))
    Assert.All(after.Hand.Cards, fun c -> Assert.False c.State.Debuff)

let private wholeRun seed dir =
    let watch = Stopwatch.StartNew()
    let result = Runner.runOne None (Sim.create ()) Api.Deck.Red Api.Stake.White seed dir
    result, watch.Elapsed

let private endsWonOrLost (output: ITestOutputHelper) seed =
    match wholeRun seed (tempDir ()) with
    | Ok r, elapsed ->
        output.WriteLine(sprintf "%s: %A ante %d round %d in %.1f s" seed r.Outcome r.Ante r.Round elapsed.TotalSeconds)

        match r.Outcome with
        | Runner.Aborted reason -> failwithf "aborted: %s" reason
        | _ -> ()
    | Error e, _ -> failwithf "%A" e

let private fastSamples run =
    Policy.shopSamples <- (4, 8)

    try
        run ()
    finally
        Policy.shopSamples <- (8, 20)

// whole runs are slow in a Debug build; filter them out with Category!=WholeRun
[<CollectionDefinition("WholeRun", DisableParallelization = true)>]
type WholeRunCollection() = class end

[<Collection("WholeRun")>]
[<Trait("Category", "WholeRun")>]
type WholeRunSim2(output: ITestOutputHelper) =
    [<Fact>]
    member _.``a whole run ends won or lost`` () = fastSamples (fun () -> endsWonOrLost output "SIM2")

[<Collection("WholeRun")>]
[<Trait("Category", "WholeRun")>]
type WholeRunSim3(output: ITestOutputHelper) =
    [<Fact>]
    member _.``a whole run ends won or lost`` () = fastSamples (fun () -> endsWonOrLost output "SIM3")

[<Collection("WholeRun")>]
[<Trait("Category", "WholeRun")>]
type SameSeed() =
    [<Fact>]
    member _.``two runs with the same seed write the same trace`` () =
        fastSamples (fun () ->
            let dirs = [ tempDir (); tempDir () ]

            let results =
                dirs
                |> List.map (fun dir -> System.Threading.Tasks.Task.Run(fun () -> fst (wholeRun "SIM1" dir)))
                |> List.map (fun t -> t.Result)

            for result in results do
                match result with
                | Ok r ->
                    match r.Outcome with
                    | Runner.Aborted reason -> failwithf "aborted: %s" reason
                    | _ -> ()
                | Error e -> failwithf "%A" e

            let text dir = File.ReadAllText(Path.Combine(dir, "trace.txt"))
            Assert.Equal(text dirs[0], text dirs[1]))
