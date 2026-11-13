module Balatro.Tests.ScoringTests

open Xunit
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
