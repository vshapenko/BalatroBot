module Balatro.Tests.ScoringTests

open System
open System.Globalization
open Xunit
open Balatro.Core
open Balatro.Core.Domain
open Balatro.Core.Scoring
open Balatro.Tests.HandsTests

let private levelsAt level =
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
    |> List.map (fun handType -> handType, handLevel handType level)
    |> Map.ofList

let private scoreOf level played held =
    score
        { Played = played
          Held = held
          Levels = levelsAt level }

let private withEdition edition (c: Card) = { c with Edition = Some edition }
let private withSeal seal (c: Card) = { c with Seal = Some seal }
let private debuffed (c: Card) = { c with Debuffed = true }

let private five = card Five Spades

[<Fact>]
let ``Three eights at level 1 score 162`` () =
    let result = scoreOf 1 [ card Eight Spades; card Eight Hearts; card Eight Clubs ] []
    Assert.Equal(ThreeOfAKind, result.HandType)
    Assert.Equal(162.0, result.Total)

[<Fact>]
let ``Level 1 Flush of 2 3 4 5 7 hearts`` () =
    let played =
        [ card Two Hearts
          card Three Hearts
          card Four Hearts
          card Five Hearts
          card Seven Hearts ]

    let result = scoreOf 1 played []
    Assert.Equal(Flush, result.HandType)
    Assert.Equal(56.0, result.Chips)
    Assert.Equal(4.0, result.Mult)
    Assert.Equal(224.0, result.Total)

[<Fact>]
let ``Plain high card`` () =
    Assert.Equal(10.0, (scoreOf 1 [ five ] []).Total)

[<Fact>]
let ``Bonus card adds 30 chips`` () =
    Assert.Equal(40.0, (scoreOf 1 [ five |> withEnhancement Enhancement.Bonus ] []).Total)

[<Fact>]
let ``Mult card adds 4 mult`` () =
    Assert.Equal(50.0, (scoreOf 1 [ five |> withEnhancement Enhancement.Mult ] []).Total)

[<Fact>]
let ``Glass card doubles mult`` () =
    Assert.Equal(20.0, (scoreOf 1 [ five |> withEnhancement Enhancement.Glass ] []).Total)

[<Fact>]
let ``Stone card adds 50 chips and no rank chips`` () =
    let stone = card King Clubs |> withEnhancement Enhancement.Stone
    let result = scoreOf 1 [ stone ] []
    Assert.Equal(HighCard, result.HandType)
    Assert.Equal(55.0, result.Total)

[<Fact>]
let ``Steel card held multiplies mult by 1.5`` () =
    let steel = card King Hearts |> withEnhancement Enhancement.Steel
    Assert.Equal(15.0, (scoreOf 1 [ five ] [ steel ]).Total)

[<Fact>]
let ``Foil adds 50 chips`` () =
    Assert.Equal(60.0, (scoreOf 1 [ five |> withEdition Foil ] []).Total)

[<Fact>]
let ``Holo adds 10 mult`` () =
    Assert.Equal(110.0, (scoreOf 1 [ five |> withEdition Holo ] []).Total)

[<Fact>]
let ``Polychrome multiplies mult by 1.5`` () =
    Assert.Equal(15.0, (scoreOf 1 [ five |> withEdition Polychrome ] []).Total)

[<Fact>]
let ``Red Seal retriggers a scoring card`` () =
    Assert.Equal(15.0, (scoreOf 1 [ five |> withSeal Seal.Red ] []).Total)

[<Fact>]
let ``Red Seal retriggers a held Steel card`` () =
    let steel = card King Hearts |> withEnhancement Enhancement.Steel |> withSeal Seal.Red
    let result = scoreOf 1 [ five ] [ steel ]
    Assert.Equal(2.25, result.Mult)
    Assert.Equal(22.0, result.Total)

[<Fact>]
let ``Debuffed scoring card contributes nothing`` () =
    let result = scoreOf 1 [ card Nine Spades |> debuffed; card Nine Hearts ] []
    Assert.Equal(Pair, result.HandType)
    Assert.Equal(2, result.ScoringCards.Length)
    Assert.Equal(38.0, result.Total)

[<Fact>]
let ``Level 3 Pair`` () =
    let result = scoreOf 3 [ card Five Spades; card Five Hearts ] []
    Assert.Equal(50.0, result.Chips)
    Assert.Equal(4.0, result.Mult)
    Assert.Equal(200.0, result.Total)

[<Fact>]
let ``Steps of a Pair of Kings with a Glass Red Seal King`` () =
    let glassKing = card King Spades |> withEnhancement Enhancement.Glass |> withSeal Seal.Red
    let result = scoreOf 1 [ glassKing; card King Hearts ] []

    let step source chips mult : ScoreStep =
        { Source = source
          Chips = chips
          Mult = mult }

    let expected =
        [ step "Pair" 10.0 2.0
          step "King of Spades" 20.0 2.0
          step "Glass" 20.0 4.0
          step "Red Seal retrigger" 20.0 4.0
          step "King of Spades" 30.0 4.0
          step "Glass" 30.0 8.0
          step "King of Hearts" 40.0 8.0 ]

    Assert.Equal<ScoreStep list>(expected, result.Steps)
    Assert.Equal(320.0, result.Total)

let private pick (rng: Random) (options: 'a list) =
    if rng.Next 4 = 0 then Some options[rng.Next options.Length] else None

let private decorate (rng: Random) (c: Card) =
    { c with
        Enhancement =
            pick
                rng
                [ Enhancement.Bonus
                  Enhancement.Mult
                  Enhancement.Wild
                  Enhancement.Glass
                  Enhancement.Steel
                  Enhancement.Stone
                  Enhancement.Gold
                  Enhancement.Lucky ]
        Edition = pick rng [ Foil; Holo; Polychrome; Negative ]
        Seal = pick rng [ Seal.Red; Seal.Blue; Seal.Gold; Seal.Purple ]
        Debuffed = rng.Next 20 = 0 }

let private randomCase (rng: Random) =
    let keys = Set.toList JokerRegistry.implemented

    let cards =
        [ for suit in [ Spades; Hearts; Clubs; Diamonds ] do
              for rank in [ Two; Three; Four; Five; Six; Seven; Eight; Nine; Ten; Jack; Queen; King; Ace ] ->
                  card rank suit ]
        |> List.sortBy (fun _ -> rng.Next())
        |> List.map (decorate rng)

    let playedCount = 1 + rng.Next 5
    let heldCount = rng.Next 6

    let jokers: Jokers.Joker list =
        [ for _ in 1 .. rng.Next 6 ->
              let key = keys[rng.Next keys.Length]

              { Key = key
                Name = key
                Edition = pick rng [ Foil; Holo; Polychrome; Negative ]
                Debuffed = rng.Next 10 = 0
                Value = Some(float (rng.Next 11))
                SellValue = 1 + rng.Next 9
                Text = "" } ]

    let situation =
        { Jokers.Situation.empty with
            Jokers = jokers
            HandsLeft = rng.Next 5
            DiscardsLeft = rng.Next 4
            Money = rng.Next 30
            DeckCount = 10 + rng.Next 35
            JokerSlots = 5 }

    situation,
    { Played = cards |> List.take playedCount
      Held = cards |> List.skip playedCount |> List.take heldCount
      Levels = levelsAt (1 + rng.Next 4) }

let private digest (b: ScoreBreakdown) =
    let number (x: float) = x.ToString("R", CultureInfo.InvariantCulture)

    let steps =
        b.Steps
        |> List.map (fun s -> $"{s.Source}={number s.Chips}x{number s.Mult}")
        |> String.concat ";"

    let cards = b.ScoringCards |> List.map (fun c -> string (Rank.id c.Rank)) |> String.concat ","
    $"{HandType.label b.HandType}|{number b.Total}|{number b.Chips}|{number b.Mult}|{cards}|{steps}"

// One FNV-1a hash per 50 cases, so a mismatch points at a range of cases
let private caseHashes () =
    let rng = Random 20260101

    [ for _ in 1..10 ->
          Seq.fold
              (fun hash _ ->
                  let situation, input = randomCase rng

                  digest (scoreWith situation input)
                  |> Seq.fold (fun h (c: char) -> (h ^^^ uint64 c) * 1099511628211UL) hash)
              14695981039346656037UL
              (seq { 1..50 })
          |> fun h -> h.ToString "x16" ]

[<Fact>]
let ``500 seeded random scores equal those of the implementation before the speed-up`` () =
    // generated from the code at the parent commit: total, chips, mult, scoring cards and every step
    let expected =
        [ "e8f627b27794d035"
          "b7037ddc7343b7f2"
          "443eefb5ebb42b3e"
          "29396441fe8469ae"
          "6b11f3f0a8aefbfd"
          "6edde6ef122dd486"
          "08460346542252c1"
          "f5759f2d00dc61f7"
          "0eee93faf6b29caf"
          "5f1f72f601c49f97" ]

    Assert.Equal<string list>(expected, caseHashes ())
