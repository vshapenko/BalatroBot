module Balatro.Tests.JokerEngineTests

open System
open System.IO
open System.Text.Json
open Xunit
open Balatro.Core.Domain
open Balatro.Core.Hands
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

let private joker key name : Joker =
    { Key = key
      Name = name
      Edition = None
      Debuffed = false
      Value = None
      SellValue = 0 }

let private withEdition edition (j: Joker) = { j with Edition = Some edition }
let private withValue value (j: Joker) = { j with Value = Some value }
let private debuffed (j: Joker) = { j with Debuffed = true }

let private jokerJoker = joker "j_joker" "Joker"
let private baseball = joker "j_baseball" "Baseball Card"
let private mime = joker "j_mime" "Mime"
let private blueprint = joker "j_blueprint" "Blueprint"
let private brainstorm = joker "j_brainstorm" "Brainstorm"
let private photograph = joker "j_photograph" "Photograph"

let private run (jokers: Joker list) played held : ScoreBreakdown =
    scoreWith
        { Situation.empty with Jokers = jokers }
        { Played = played
          Held = held
          Levels = levels }

let private total jokers played held = (run jokers played held).Total

let private kk = [ card King Spades; card King Hearts ]

[<Fact>]
let ``Joker adds 4 mult`` () =
    Assert.Equal(180.0, total [ jokerJoker ] kk [])

[<Fact>]
let ``Foil Joker adds 50 chips before its mult`` () =
    Assert.Equal(480.0, total [ jokerJoker |> withEdition Foil ] kk [])

[<Fact>]
let ``Holo Joker adds 10 mult`` () =
    Assert.Equal(480.0, total [ jokerJoker |> withEdition Holo ] kk [])

[<Fact>]
let ``Polychrome Joker multiplies after its own effect`` () =
    Assert.Equal(270.0, total [ jokerJoker |> withEdition Polychrome ] kk [])

[<Fact>]
let ``Debuffed Joker does nothing`` () =
    Assert.Equal(60.0, total [ jokerJoker |> debuffed ] kk [])
    Assert.Equal(60.0, total [ jokerJoker |> debuffed |> withEdition Foil ] kk [])

[<Fact>]
let ``Sly Joker counts a Pair inside a Full House`` () =
    let played =
        [ card King Spades
          card King Hearts
          card King Clubs
          card Queen Spades
          card Queen Hearts ]

    Assert.Equal(560.0, total [ joker "j_sly" "Sly Joker" ] played [])

[<Fact>]
let ``Greedy Joker adds 3 mult per scoring Diamond`` () =
    Assert.Equal(150.0, total [ joker "j_greedy_joker" "Greedy Joker" ] [ card King Diamonds; card King Spades ] [])

[<Fact>]
let ``Photograph doubles mult on the first scoring face card`` () =
    Assert.Equal(120.0, total [ photograph ] kk [])

[<Fact>]
let ``Hanging Chad retriggers the first scoring card twice`` () =
    Assert.Equal(100.0, total [ joker "j_hanging_chad" "Hanging Chad" ] kk [])

[<Fact>]
let ``Baron and Mime on held Kings`` () =
    let baron = joker "j_baron" "Baron"
    let queens = [ card Queen Spades; card Queen Hearts ]
    let held = [ card King Clubs ]
    Assert.Equal(90.0, total [ baron ] queens held)
    Assert.Equal(135.0, total [ mime; baron ] queens held)

[<Fact>]
let ``Mime retriggers a held Steel card`` () =
    let steel = card Five Clubs |> withEnhancement Enhancement.Steel
    Assert.Equal(135.0, total [ mime ] [ card Queen Spades; card Queen Hearts ] [ steel ])

[<Fact>]
let ``Baseball Card multiplies for Uncommon jokers in joker order`` () =
    Assert.Equal(90.0, total [ baseball; mime ] kk [])
    Assert.Equal(270.0, total [ jokerJoker; baseball; mime ] kk [])
    Assert.Equal(210.0, total [ mime; baseball; jokerJoker ] kk [])

[<Fact>]
let ``Blueprint copies the joker on its right`` () =
    Assert.Equal(300.0, total [ blueprint; jokerJoker ] kk [])
    Assert.Equal(420.0, total [ blueprint; blueprint; jokerJoker ] kk [])
    Assert.Equal(60.0, total [ blueprint ] kk [])

[<Fact>]
let ``Brainstorm copies the leftmost joker`` () =
    Assert.Equal(300.0, total [ jokerJoker; brainstorm ] kk [])

[<Fact>]
let ``Green Joker adds its counter plus the increment of this hand`` () =
    Assert.Equal(180.0, total [ joker "j_green_joker" "Green Joker" |> withValue 3.0 ] kk [])

[<Fact>]
let ``Vampire strips enhancements before the cards score`` () =
    let played = [ card King Spades |> withEnhancement Enhancement.Bonus; card King Hearts ]
    Assert.Equal(66.0, total [ joker "j_vampire" "Vampire" |> withValue 1.0 ] played [])

[<Fact>]
let ``Four Fingers makes a four card flush inside five cards`` () =
    let hearts = [ card Three Hearts; card Six Hearts; card Nine Hearts; card Queen Hearts ]
    let played = card Two Spades :: hearts
    let result = run [ joker "j_four_fingers" "Four Fingers" ] played []
    Assert.Equal(Flush, result.HandType)
    Assert.Equal<Card list>(hearts, result.ScoringCards)
    // Flush 35 chips + 3 + 6 + 9 + 10, times 4
    Assert.Equal(252.0, result.Total)

[<Fact>]
let ``Shortcut allows a straight with gaps`` () =
    let played =
        [ card Two Spades
          card Four Hearts
          card Six Clubs
          card Eight Diamonds
          card Ten Spades ]

    let result = run [ joker "j_shortcut" "Shortcut" ] played []
    Assert.Equal(Straight, result.HandType)
    // Straight 30 chips + 2 + 4 + 6 + 8 + 10, times 4
    Assert.Equal(240.0, result.Total)

[<Fact>]
let ``Smeared Joker makes Hearts and Diamonds one suit`` () =
    let played =
        [ card Two Hearts
          card Five Hearts
          card Eight Hearts
          card Jack Diamonds
          card King Diamonds ]

    let result = run [ joker "j_smeared" "Smeared Joker" ] played []
    Assert.Equal(Flush, result.HandType)
    // Flush 35 chips + 2 + 5 + 8 + 10 + 10, times 4
    Assert.Equal(280.0, result.Total)

[<Fact>]
let ``Splash makes every played card score`` () =
    let played = kk @ [ card Two Clubs; card Five Diamonds; card Nine Spades ]
    let result = run [ joker "j_splash" "Splash" ] played []
    Assert.Equal(Pair, result.HandType)
    Assert.Equal(5, result.ScoringCards.Length)
    // Pair 10 chips + 10 + 10 + 2 + 5 + 9, times 2
    Assert.Equal(92.0, result.Total)

[<Fact>]
let ``Pareidolia makes every card a face card for Photograph`` () =
    let played = [ card Seven Spades; card Seven Hearts ]
    let rule = joker "j_pareidolia" "Pareidolia"
    // Pair 10 chips + 7 + 7, first 7 doubles the mult
    Assert.Equal(96.0, total [ rule; photograph ] played [])
    Assert.Equal(48.0, total [ photograph ] played [])

[<Fact>]
let ``A Full House contains Pair, Three of a Kind, Two Pair and Full House`` () =
    let played =
        [ card King Spades
          card King Hearts
          card King Clubs
          card Queen Spades
          card Queen Hearts ]

    let types = contained HandRules.none played
    Assert.True(Set.isSubset (set [ Pair; ThreeOfAKind; TwoPair; FullHouse ]) types)

[<Fact>]
let ``Steps list one entry per contribution in stage order`` () =
    let result = run [ jokerJoker |> withEdition Foil; baseball; mime ] kk []

    let expected =
        [ "Pair"
          "King of Spades"
          "King of Hearts"
          "Joker"
          "Joker"
          "Baseball Card" ]

    Assert.Equal<string list>(expected, result.Steps |> List.map (fun step -> step.Source))
    Assert.Equal(720.0, result.Total)

let private fixtureEffect key =
    JsonDocument
        .Parse(File.ReadAllText(AppContext.BaseDirectory + "fixtures/jokers.json"))
        .RootElement.EnumerateArray()
    |> Seq.find (fun element -> element.GetProperty("key").GetString() = key)
    |> fun element -> element.GetProperty("effect").GetString()

[<Fact>]
let ``currentValue reads the counter of a joker text`` () =
    [ "j_green_joker", Some 0.0
      "j_stencil", Some 5.0
      "j_bull", Some 462.0
      "j_constellation", Some 1.0
      "j_cloud_9", Some 4.0
      "j_joker", None ]
    |> List.iter (fun (key, expected) -> Assert.Equal(expected, Mapping.currentValue (fixtureEffect key)))
