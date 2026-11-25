module Balatro.Tests.PlayNetPolicyTests

open System
open System.Diagnostics
open System.IO
open System.Text.Json
open Xunit
open Balatro.Core
open Balatro.Core.Domain
open Balatro.Bot
open Balatro.Tests.HandsTests

let private net = lazy (PlayNet.load (AppContext.BaseDirectory + "nn/playnet.json"))

let private load name =
    let text = File.ReadAllText(AppContext.BaseDirectory + "fixtures/gamestate." + name + ".json")

    match Api.decodeGameState (JsonDocument.Parse(text).RootElement) with
    | Ok g -> g
    | Error e -> failwithf "%s: %s" name e

let private clearing (g: Api.GameState) =
    let blind =
        [ g.Blinds.Value.Small; g.Blinds.Value.Big; g.Blinds.Value.Boss ]
        |> List.find (fun b -> b.Status = Api.BlindStatus.Current)

    { g with Round = { g.Round with Chips = blind.Score - 1 } }

[<Fact>]
let ``network play on the hand fixture keeps the action and tags the rule`` () =
    let g = load "SELECTING_HAND"
    let action, decision = Policy.decideWith (Some net.Value) 1 g
    Assert.Equal(fst (Policy.decide 1 g), action)

    Assert.True(
        decision.Rule.Contains "network agrees"
        || decision.Rule.Contains "network out of scope"
        || decision.Rule.Contains "network differs: "
    )

[<Fact>]
let ``network pick that clears the blind agrees with the search`` () =
    let g = clearing (load "SELECTING_HAND")
    let action, decision = Policy.decideWith (Some net.Value) 1 g
    Assert.Equal(fst (Policy.decide 1 g), action)
    Assert.StartsWith("clears the blind now; network pick (network agrees)", decision.Rule)

[<Fact>]
let ``a joker the network declines is reported out of scope`` () =
    let g = load "SELECTING_HAND"
    let t = g.Hand.Cards.Head

    let vampire =
        { t with
            Key = "j_vampire"
            Label = "j_vampire"
            Set = Api.CardSet.Joker }

    let g = { g with Jokers = { g.Jokers with Cards = [ vampire ]; Count = 1 } }
    let action, decision = Policy.decideWith (Some net.Value) 1 g
    Assert.Equal(fst (Policy.decide 1 g), action)
    Assert.Contains("network out of scope", decision.Rule)

[<Fact>]
let ``timing of the network choice`` () =
    let g = load "SELECTING_HAND"
    let cards = g.Hand.Cards |> List.choose Mapping.toCard
    let n = net.Value
    let run () = PlayNet.choose n (Mapping.situation g) cards (Mapping.levels g) |> ignore
    run ()
    let sw = Stopwatch.StartNew()

    for _ in 1..20 do
        run ()

    printfn "PlayNet.choose mean %.3f ms over 20 calls" (sw.Elapsed.TotalMilliseconds / 20.0)

    // after a warm-up, so that the steady state is not mixed with the first calls
    for _ in 1..20 do
        run ()

    let times =
        Array.init 60 (fun _ ->
            let sw = Stopwatch.StartNew()
            run ()
            sw.Elapsed.TotalMilliseconds)
        |> Array.sort

    printfn "PlayNet.choose steady state median %.3f ms, min %.3f ms over 60 calls" times[30] times[0]

[<Fact>]
let ``shop round score with the network equals the search score and counts picks`` () =
    let deck =
        [ for suit in [ Spades; Hearts; Clubs; Diamonds ] do
              for rank in [ Two; Three; Four; Five; Six; Seven; Eight; Nine; Ten; Jack; Queen; King; Ace ] do
                  yield card rank suit ]

    let build slots : Shop.Build =
        { Jokers = []
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
          Situation = { Jokers.Situation.empty with JokerSlots = slots } }

    // the slot count is not scored, but it keeps the second call off the cached first one
    let reference = Shop.roundScore 100 4 (build 5)

    let withNet =
        try
            Shop.useNetwork (Some net.Value)
            Shop.roundScore 100 4 (build 6)
        finally
            Shop.useNetwork None

    let picks, _, differs = Shop.networkCounts ()
    Assert.Equal(reference, withNet)
    Assert.True(picks > 0)
    Assert.Equal(0, differs)
