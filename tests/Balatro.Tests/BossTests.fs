module Balatro.Tests.BossTests

open System
open System.IO
open System.Text.Json
open Xunit
open Balatro.Core.Domain
open Balatro.Core.Jokers
open Balatro.Core.Scoring
open Balatro.Bot
open Balatro.Tests.HandsTests

let private levels =
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
    |> List.map (fun handType -> handType, handLevel handType 1)
    |> Map.ofList

let private jokerJoker: Joker =
    { Key = "j_joker"
      Name = "Joker"
      Edition = None
      Debuffed = false
      Value = None
      SellValue = 0
      Text = "" }

let private run boss plays jokers played : ScoreBreakdown =
    scoreWith
        { Situation.empty with
            Jokers = jokers
            Boss = boss
            HandPlays = Map.ofList plays }
        { Played = played
          Held = []
          Levels = levels }

let private kk = [ card King Spades; card King Hearts ]

let private fiveCards =
    kk @ [ card Two Clubs; card Five Diamonds; card Nine Spades ]

let private flush =
    [ card Two Spades; card Five Spades; card Seven Spades; card Nine Spades; card Jack Spades ]

[<Fact>]
let ``The Psychic scores nothing for fewer than five cards`` () =
    let result = run (Some Psychic) [] [] kk
    Assert.Equal(0.0, result.Total)
    Assert.Equal(Pair, result.HandType)

    Assert.Equal<ScoreStep list>([ { Source = "Not allowed (The Psychic)"; Chips = 0.0; Mult = 0.0 } ], result.Steps)

[<Fact>]
let ``The Psychic allows five played cards`` () =
    Assert.Equal(60.0, (run (Some Psychic) [] [] fiveCards).Total)

[<Fact>]
let ``The Psychic still scores nothing with a joker`` () =
    Assert.Equal(0.0, (run (Some Psychic) [] [ jokerJoker ] kk).Total)

[<Fact>]
let ``The Eye rejects a hand type already played this round`` () =
    Assert.Equal(0.0, (run (Some Eye) [ Pair, (1, 1) ] [] kk).Total)

[<Fact>]
let ``The Eye allows a hand type played only in earlier rounds`` () =
    Assert.Equal(60.0, (run (Some Eye) [ Pair, (5, 0) ] [] kk).Total)

[<Fact>]
let ``The Eye allows a hand type when only another was played this round`` () =
    Assert.Equal(60.0, (run (Some Eye) [ Flush, (1, 1) ] [] kk).Total)

[<Fact>]
let ``The Mouth allows the first hand of the round`` () =
    Assert.Equal(60.0, (run (Some Mouth) [] [] kk).Total)

[<Fact>]
let ``The Mouth allows the hand type already played this round`` () =
    Assert.Equal(60.0, (run (Some Mouth) [ Pair, (1, 1) ] [] kk).Total)

[<Fact>]
let ``The Mouth rejects another hand type`` () =
    let result = run (Some Mouth) [ Flush, (1, 1) ] [] kk
    Assert.Equal(0.0, result.Total)
    Assert.Equal<ScoreStep list>([ { Source = "Not allowed (The Mouth)"; Chips = 0.0; Mult = 0.0 } ], result.Steps)

[<Fact>]
let ``The Flint halves a Pair`` () =
    Assert.Equal(25.0, (run (Some Flint) [] [] kk).Total)

[<Fact>]
let ``The Flint halves base values before jokers`` () =
    Assert.Equal(125.0, (run (Some Flint) [] [ jokerJoker ] kk).Total)

[<Fact>]
let ``The Flint halves a Flush`` () =
    Assert.Equal(102.0, (run (Some Flint) [] [] flush).Total)

[<Fact>]
let ``The Flint halves a High Card`` () =
    Assert.Equal(13.0, (run (Some Flint) [] [] [ card King Spades ]).Total)

[<Fact>]
let ``The Flint adds one step after the base values`` () =
    let steps = (run (Some Flint) [] [] kk).Steps
    Assert.Equal<string list>([ "Pair"; "The Flint"; "King of Spades"; "King of Hearts" ], steps |> List.map _.Source)

[<Fact>]
let ``no boss leaves the score and steps unchanged`` () =
    let result = run None [] [] kk
    let plain = score { Played = kk; Held = []; Levels = levels }
    Assert.Equal(60.0, result.Total)
    Assert.Equal<ScoreStep list>(plain.Steps, result.Steps)

let private load name =
    let text = File.ReadAllText(AppContext.BaseDirectory + "fixtures/gamestate." + name + ".json")

    match Api.decodeGameState (JsonDocument.Parse(text).RootElement) with
    | Ok g -> g
    | Error e -> failwithf "%s: %s" name e

let private withBoss name (state: Api.GameState) =
    let blinds = state.Blinds.Value

    { state with
        Blinds =
            Some
                { Small = { blinds.Small with Status = Api.BlindStatus.Defeated }
                  Big = { blinds.Big with Status = Api.BlindStatus.Defeated }
                  Boss = { blinds.Boss with Name = name; Status = Api.BlindStatus.Current } } }

let private withChicot debuff (state: Api.GameState) =
    let template = (load "SELECTING_HAND.with_joker").Jokers.Cards.Head

    let chicot =
        { template with
            Key = "j_chicot"
            Label = "Chicot"
            State = { template.State with Debuff = debuff } }

    { state with Jokers = { state.Jokers with Cards = [ chicot ] } }

[<Fact>]
let ``no boss rule while the small blind is current`` () =
    Assert.Equal(None, Mapping.boss (load "SELECTING_HAND"))

[<Fact>]
let ``The Psychic is read from the current boss blind`` () =
    let state = load "SELECTING_HAND" |> withBoss "The Psychic"
    Assert.Equal(Some Psychic, Mapping.boss state)
    Assert.Equal(Some Psychic, (Mapping.situation state).Boss)

[<Fact>]
let ``Chicot disables the boss rule`` () =
    Assert.Equal(None, Mapping.boss (load "SELECTING_HAND" |> withBoss "The Psychic" |> withChicot false))

[<Fact>]
let ``a debuffed Chicot does not disable the boss rule`` () =
    Assert.Equal(Some Psychic, Mapping.boss (load "SELECTING_HAND" |> withBoss "The Psychic" |> withChicot true))

[<Fact>]
let ``a boss without a scoring rule gives no boss`` () =
    Assert.Equal(None, Mapping.boss (load "SELECTING_HAND" |> withBoss "The Hook"))

let private pairAtLevel3 (state: Api.GameState) =
    let pair = { state.Hands["Pair"] with Level = 3; Chips = 40; Mult = 4 }
    { state with Hands = state.Hands |> Map.add "Pair" pair }

[<Fact>]
let ``The Arm lowers every hand above level 1 by one level`` () =
    let state = load "SELECTING_HAND" |> withBoss "The Arm" |> pairAtLevel3
    let table = Mapping.levels state
    Assert.Equal({ Chips = 25.0; Mult = 3.0 }, table[Pair])
    Assert.Equal({ Chips = 5.0; Mult = 1.0 }, table[HighCard])

[<Fact>]
let ``another boss keeps the wire hand values`` () =
    let state = load "SELECTING_HAND" |> withBoss "The Hook" |> pairAtLevel3
    Assert.Equal({ Chips = 40.0; Mult = 4.0 }, (Mapping.levels state)[Pair])

[<Fact>]
let ``The Flint is not mapped because the modded game does not halve`` () =
    let g = load "SELECTING_HAND" |> withBoss "The Flint"
    Assert.Equal(None, Mapping.boss g)
