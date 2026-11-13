module Balatro.Tests.JokersCounterTests

open System
open System.IO
open System.Text.Json
open Xunit
open Balatro.Core.Domain
open Balatro.Core.Hands
open Balatro.Core.Jokers
open Balatro.Core.Scoring
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

let private joker key : Joker =
    { Key = key
      Name = key
      Edition = None
      Debuffed = false
      Value = None
      SellValue = 0
      Text = "" }

let private withValue value (j: Joker) = { j with Value = Some value }
let private withText text (j: Joker) = { j with Text = text }

let private totalIn situation (jokers: Joker list) played held =
    (scoreWith
        { situation with Jokers = jokers }
        { Played = played
          Held = held
          Levels = levels })
        .Total

let private total jokers played held = totalIn Situation.empty jokers played held

let private fixtureEffect key =
    JsonDocument
        .Parse(File.ReadAllText(AppContext.BaseDirectory + "fixtures/jokers.json"))
        .RootElement.EnumerateArray()
    |> Seq.find (fun element -> element.GetProperty("key").GetString() = key)
    |> fun element -> element.GetProperty("effect").GetString()

let private fromFixture key = joker key |> withText (fixtureEffect key)

// Pair of Kings: 10 + 10 + 10 chips, mult 2
let private kk = [ card King Spades; card King Hearts ]

[<Fact>]
let ``Ride the Bus adds its counter plus one when no scoring card is a face card`` () =
    let sevens = [ card Seven Spades; card Seven Hearts ]
    // chips 10 + 7 + 7, mult 2 + (2 + 1)
    Assert.Equal(120.0, total [ joker "j_ride_the_bus" |> withValue 2.0 ] sevens [])
    // chips 24, mult 2 + (0 + 1)
    Assert.Equal(72.0, total [ joker "j_ride_the_bus" ] sevens [])

[<Fact>]
let ``Ride the Bus resets when a scoring card is a face card`` () =
    Assert.Equal(60.0, total [ joker "j_ride_the_bus" |> withValue 2.0 ] kk [])

[<Fact>]
let ``Runner adds 15 chips for a hand that contains a Straight`` () =
    let straight =
        [ card Two Spades
          card Three Hearts
          card Four Clubs
          card Five Diamonds
          card Six Spades ]

    // chips 30 + 20 + (15 + 15), mult 4
    Assert.Equal(320.0, total [ joker "j_runner" |> withValue 15.0 ] straight [])

[<Fact>]
let ``Runner keeps its counter when the hand has no Straight`` () =
    // chips 30 + 15, mult 2
    Assert.Equal(90.0, total [ joker "j_runner" |> withValue 15.0 ] kk [])

[<Fact>]
let ``Square Joker adds 4 chips when exactly 4 cards are played`` () =
    let played = kk @ [ card Five Clubs; card Six Diamonds ]
    // chips 30 + (4 + 4), mult 2
    Assert.Equal(76.0, total [ joker "j_square" |> withValue 4.0 ] played [])

[<Fact>]
let ``Square Joker keeps its counter when the hand has another size`` () =
    // chips 30 + 4, mult 2
    Assert.Equal(68.0, total [ joker "j_square" |> withValue 4.0 ] kk [])

[<Fact>]
let ``Spare Trousers adds 2 mult for a Two Pair`` () =
    let played = kk @ [ card Five Clubs; card Five Diamonds ]
    // chips 20 + 10 + 10 + 5 + 5, mult 2 + (2 + 2)
    Assert.Equal(300.0, total [ joker "j_trousers" |> withValue 2.0 ] played [])

[<Fact>]
let ``Spare Trousers counts the Two Pair inside a Full House`` () =
    let played = [ card King Spades; card King Hearts; card King Clubs; card Five Clubs; card Five Diamonds ]
    // chips 40 + 30 + 10, mult 4 + (0 + 2)
    Assert.Equal(480.0, total [ joker "j_trousers" ] played [])

[<Fact>]
let ``Spare Trousers keeps its counter for a hand without a Two Pair`` () =
    // chips 30, mult 2 + 2
    Assert.Equal(120.0, total [ joker "j_trousers" |> withValue 2.0 ] kk [])

let private obelisk = joker "j_obelisk" |> withValue 1.8

[<Fact>]
let ``Obelisk gains 0.2 when another hand type has been played as often`` () =
    let plays = Map.ofList [ Pair, (1, 1); HighCard, (2, 0) ]
    let situation = { Situation.empty with HandPlays = plays }
    // this hand is the second Pair, tied with High Card: x(1.8 + 0.2) on 30 chips, mult 2
    Assert.Equal(120.0, totalIn situation [ obelisk ] kk [])

[<Fact>]
let ``Obelisk resets when the played hand is the most played one`` () =
    let plays = Map.ofList [ Pair, (5, 0); HighCard, (1, 0) ]
    let situation = { Situation.empty with HandPlays = plays }
    Assert.Equal(60.0, totalIn situation [ obelisk ] kk [])

[<Theory>]
[<InlineData("j_ceremonial")>]
[<InlineData("j_red_card")>]
[<InlineData("j_flash")>]
let ``A mult counter adds its value`` (key: string) =
    // chips 30, mult 2 + 8
    Assert.Equal(300.0, total [ joker key |> withValue 8.0 ] kk [])
    Assert.Equal(60.0, total [ joker key |> withValue 0.0 ] kk [])

[<Theory>]
[<InlineData("j_castle")>]
[<InlineData("j_wee")>]
let ``A chips counter adds its value`` (key: string) =
    // chips 30 + 30, mult 2
    Assert.Equal(120.0, total [ joker key |> withValue 30.0 ] kk [])
    Assert.Equal(60.0, total [ joker key |> withValue 0.0 ] kk [])

[<Theory>]
[<InlineData("j_constellation")>]
[<InlineData("j_madness")>]
[<InlineData("j_hologram")>]
[<InlineData("j_campfire")>]
[<InlineData("j_glass")>]
[<InlineData("j_lucky_cat")>]
[<InlineData("j_hit_the_road")>]
[<InlineData("j_yorick")>]
[<InlineData("j_caino")>]
let ``An x counter multiplies by its value`` (key: string) =
    Assert.Equal(90.0, total [ joker key |> withValue 1.5 ] kk [])
    Assert.Equal(60.0, total [ joker key |> withValue 1.0 ] kk [])

[<Fact>]
let ``Ice Cream, Popcorn and Ramen read their value from the card text`` () =
    // chips 30 + 100, mult 2
    Assert.Equal(260.0, total [ fromFixture "j_ice_cream" ] kk [])
    // chips 30, mult 2 + 20
    Assert.Equal(660.0, total [ fromFixture "j_popcorn" ] kk [])
    // chips 30, mult 2 times 2
    Assert.Equal(120.0, total [ fromFixture "j_ramen" ] kk [])

[<Fact>]
let ``Ice Cream follows the value in a decayed text`` () =
    let decayed = joker "j_ice_cream" |> withText "+85 Chips -5 Chips for every hand played"
    // chips 30 + 85, mult 2
    Assert.Equal(230.0, total [ decayed ] kk [])

[<Fact>]
let ``Loyalty Card pays x4 on the hand its text shows as active`` () =
    Assert.Equal(60.0, total [ fromFixture "j_loyalty_card" ] kk [])

    let active = joker "j_loyalty_card" |> withText "X4 Mult every 6 hands played Active!"
    Assert.Equal(240.0, total [ active ] kk [])

[<Fact>]
let ``Midas Mask turns scoring face cards into Gold cards and drops their enhancement`` () =
    let played = [ card King Spades |> withEnhancement Enhancement.Bonus; card King Hearts ]
    // Bonus King: chips 10 + 40 + 10, mult 2
    Assert.Equal(120.0, total [] played [])
    // Gold King: chips 10 + 10 + 10, mult 2
    Assert.Equal(60.0, total [ joker "j_midas_mask" ] played [])

[<Fact>]
let ``Midas Mask leaves cards that are not face cards alone`` () =
    let played = [ card Seven Spades |> withEnhancement Enhancement.Mult; card Seven Hearts ]
    // chips 10 + 7 + 7, mult 2 + 4
    Assert.Equal(144.0, total [ joker "j_midas_mask" ] played [])

[<Fact>]
let ``Jokers without a score effect leave the total unchanged and count as implemented`` () =
    [ "j_space"; "j_dna"; "j_todo_list"; "j_sixth_sense" ]
    |> List.iter (fun key ->
        Assert.Equal(60.0, total [ joker key ] kk [])
        Assert.Contains(key, Balatro.Core.JokerRegistry.implemented))

[<Fact>]
let ``Ramen multiplies by the value the game holds, not the rounded text`` () =
    let ramen text = joker "j_ramen" |> withText text
    // chips 30 + 270, mult 2 + 36 gives 11400 before Ramen
    let around ramen =
        total [ joker "j_castle" |> withValue 270.0; joker "j_ceremonial" |> withValue 36.0; ramen ] kk []

    Assert.Equal(16757.0, around (ramen "X1.47 Mult, loses X0.01 Mult per card discarded"))
    Assert.Equal(22800.0, around (ramen "X2 Mult, loses X0.01 Mult per card discarded"))
