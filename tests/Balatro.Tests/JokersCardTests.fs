module Balatro.Tests.JokersCardTests

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

let private fixtureText key =
    use document = JsonDocument.Parse(File.ReadAllText(AppContext.BaseDirectory + "fixtures/jokers.json"))

    document.RootElement.EnumerateArray()
    |> Seq.find (fun entry -> entry.GetProperty("key").GetString() = key)
    |> fun entry -> entry.GetProperty("effect").GetString()

let private totalIn (situation: Situation) (jokers: Joker list) played held =
    (scoreWith
        { situation with Jokers = jokers }
        { Played = played
          Held = held
          Levels = levels })
        .Total

let private total jokers played held = totalIn Situation.empty jokers played held

let private debuffed (c: Card) = { c with Debuffed = true }

let private pair rank first second = [ card rank first; card rank second ]

[<Fact>]
let ``Lusty Joker adds 3 mult per scoring Heart`` () =
    Assert.Equal(160.0, total [ joker "j_lusty_joker" ] (pair Ace Hearts Spades) [])
    Assert.Equal(64.0, total [ joker "j_lusty_joker" ] (pair Ace Spades Clubs) [])
    Assert.Equal(60.0, total [ joker "j_lusty_joker" ] (pair King Spades Clubs @ [ card Seven Hearts ]) [])

[<Fact>]
let ``Wrathful Joker adds 3 mult per scoring Spade`` () =
    Assert.Equal(150.0, total [ joker "j_wrathful_joker" ] (pair King Spades Hearts) [])
    Assert.Equal(60.0, total [ joker "j_wrathful_joker" ] (pair King Hearts Diamonds) [])

[<Fact>]
let ``Gluttonous Joker adds 3 mult per scoring Club`` () =
    Assert.Equal(150.0, total [ joker "j_gluttenous_joker" ] (pair Queen Clubs Hearts) [])
    Assert.Equal(60.0, total [ joker "j_gluttenous_joker" ] (pair Queen Hearts Diamonds) [])

[<Fact>]
let ``Fibonacci adds 8 mult on Ace 2 3 5 and 8`` () =
    Assert.Equal(468.0, total [ joker "j_fibonacci" ] (pair Eight Spades Hearts) [])
    Assert.Equal(144.0, total [ joker "j_fibonacci" ] [ card Ace Spades ] [])
    Assert.Equal(36.0, total [ joker "j_fibonacci" ] (pair Four Spades Hearts) [])

[<Fact>]
let ``Scary Face adds 30 chips on face cards`` () =
    Assert.Equal(180.0, total [ joker "j_scary_face" ] (pair King Spades Hearts) [])
    Assert.Equal(56.0, total [ joker "j_scary_face" ] (pair Nine Spades Hearts) [])

[<Fact>]
let ``Smiley adds 5 mult on face cards`` () =
    Assert.Equal(360.0, total [ joker "j_smiley" ] (pair King Spades Hearts) [])
    Assert.Equal(56.0, total [ joker "j_smiley" ] (pair Nine Spades Hearts) [])

[<Fact>]
let ``Even Steven adds 4 mult on even ranks`` () =
    Assert.Equal(180.0, total [ joker "j_even_steven" ] (pair Four Spades Hearts) [])
    Assert.Equal(48.0, total [ joker "j_even_steven" ] (pair Seven Spades Hearts) [])
    Assert.Equal(60.0, total [ joker "j_even_steven" ] (pair King Spades Hearts) [])

[<Fact>]
let ``Odd Todd adds 31 chips on odd ranks including Ace`` () =
    Assert.Equal(172.0, total [ joker "j_odd_todd" ] (pair Seven Spades Hearts) [])
    Assert.Equal(47.0, total [ joker "j_odd_todd" ] [ card Ace Spades ] [])
    Assert.Equal(36.0, total [ joker "j_odd_todd" ] (pair Four Spades Hearts) [])

[<Fact>]
let ``Scholar adds 20 chips and 4 mult on Aces`` () =
    Assert.Equal(720.0, total [ joker "j_scholar" ] (pair Ace Spades Hearts) [])
    Assert.Equal(60.0, total [ joker "j_scholar" ] (pair King Spades Hearts) [])

[<Fact>]
let ``Walkie Talkie adds 10 chips and 4 mult on 10 and 4`` () =
    Assert.Equal(500.0, total [ joker "j_walkie_talkie" ] (pair Ten Spades Hearts) [])
    Assert.Equal(380.0, total [ joker "j_walkie_talkie" ] (pair Four Spades Hearts) [])
    Assert.Equal(40.0, total [ joker "j_walkie_talkie" ] (pair Five Spades Hearts) [])

[<Fact>]
let ``Arrowhead adds 50 chips per scoring Spade`` () =
    Assert.Equal(160.0, total [ joker "j_arrowhead" ] (pair King Spades Hearts) [])
    Assert.Equal(60.0, total [ joker "j_arrowhead" ] (pair King Hearts Diamonds) [])

[<Fact>]
let ``Onyx Agate adds 7 mult per scoring Club`` () =
    Assert.Equal(270.0, total [ joker "j_onyx_agate" ] (pair King Clubs Hearts) [])
    Assert.Equal(60.0, total [ joker "j_onyx_agate" ] (pair King Hearts Diamonds) [])

[<Fact>]
let ``Triboulet doubles mult on Kings and Queens`` () =
    Assert.Equal(240.0, total [ joker "j_triboulet" ] (pair King Spades Hearts) [])
    Assert.Equal(30.0, total [ joker "j_triboulet" ] [ card Queen Spades ] [])
    Assert.Equal(60.0, total [ joker "j_triboulet" ] (pair Jack Spades Hearts) [])

[<Fact>]
let ``Ancient Joker multiplies by 1.5 per scoring card of the suit in its text`` () =
    let ancient = { joker "j_ancient" with Text = fixtureText "j_ancient" }
    Assert.Equal(90.0, total [ ancient ] (pair King Diamonds Hearts) [])
    Assert.Equal(135.0, total [ ancient ] (pair King Diamonds Diamonds) [])
    Assert.Equal(60.0, total [ ancient ] (pair King Spades Hearts) [])

[<Fact>]
let ``Ancient Joker follows the suit in its text`` () =
    let text = (fixtureText "j_ancient").Replace("Diamond suit", "Spade suit")
    let ancient = { joker "j_ancient" with Text = text }
    Assert.Equal(90.0, total [ ancient ] (pair King Spades Hearts) [])
    Assert.Equal(60.0, total [ ancient ] (pair King Diamonds Hearts) [])

[<Fact>]
let ``The Idol doubles mult on the exact rank and suit in its text`` () =
    let idol = { joker "j_idol" with Text = fixtureText "j_idol" }
    Assert.Equal(96.0, total [ idol ] (pair Seven Hearts Spades) [])
    Assert.Equal(48.0, total [ idol ] (pair Seven Spades Diamonds) [])
    Assert.Equal(60.0, total [ idol ] (pair King Hearts Spades) [])

[<Fact>]
let ``The Idol follows the card in its text`` () =
    let text = (fixtureText "j_idol").Replace("7 of Hearts", "Queen of Spades")
    let idol = { joker "j_idol" with Text = text }
    Assert.Equal(120.0, total [ idol ] (pair Queen Spades Hearts) [])
    Assert.Equal(60.0, total [ idol ] (pair Queen Hearts Diamonds) [])

[<Fact>]
let ``Sock and Buskin retriggers face cards`` () =
    Assert.Equal(100.0, total [ joker "j_sock_and_buskin" ] (pair King Spades Hearts) [])
    Assert.Equal(56.0, total [ joker "j_sock_and_buskin" ] (pair Nine Spades Hearts) [])

[<Fact>]
let ``Hack retriggers ranks 2 to 5`` () =
    Assert.Equal(52.0, total [ joker "j_hack" ] (pair Four Spades Hearts) [])
    Assert.Equal(60.0, total [ joker "j_hack" ] (pair Five Spades Hearts) [])
    Assert.Equal(44.0, total [ joker "j_hack" ] (pair Six Spades Hearts) [])

[<Fact>]
let ``Seltzer retriggers every scoring card`` () =
    Assert.Equal(92.0, total [ joker "j_selzer" ] (pair Nine Spades Hearts) [])
    Assert.Equal(25.0, total [ joker "j_selzer" ] [ card King Spades ] [])

[<Fact>]
let ``Dusk retriggers every scoring card only when no hands are left`` () =
    let lastHand = { Situation.empty with HandsLeft = 0 }
    let earlier = { Situation.empty with HandsLeft = 1 }
    Assert.Equal(92.0, totalIn lastHand [ joker "j_dusk" ] (pair Nine Spades Hearts) [])
    Assert.Equal(56.0, totalIn earlier [ joker "j_dusk" ] (pair Nine Spades Hearts) [])

[<Fact>]
let ``Raised Fist adds double the rank value of the lowest held card`` () =
    let raised = [ joker "j_raised_fist" ]
    let ace = [ card Ace Spades ]
    Assert.Equal(176.0, total raised ace [ card King Spades; card Five Hearts; card Nine Diamonds ])
    Assert.Equal(368.0, total raised ace [ card Ace Hearts ])
    Assert.Equal(16.0, total raised ace [])

[<Fact>]
let ``Raised Fist triggers once on the last of tied lowest cards`` () =
    let raised = [ joker "j_raised_fist" ]
    let ace = [ card Ace Spades ]
    Assert.Equal(176.0, total raised ace [ card Five Hearts; card Five Diamonds ])
    Assert.Equal(176.0, total raised ace [ card Five Hearts; card Five Hearts ])
    Assert.Equal(16.0, total raised ace [ card Five Hearts; debuffed (card Five Diamonds) ])
    Assert.Equal(176.0, total raised ace [ debuffed (card Five Hearts); card Five Diamonds ])

[<Fact>]
let ``Raised Fist skips Stone cards and does nothing for a debuffed lowest card`` () =
    let raised = [ joker "j_raised_fist" ]
    let ace = [ card Ace Spades ]
    Assert.Equal(336.0, total raised ace [ card Two Spades |> withEnhancement Enhancement.Stone; card King Hearts ])
    Assert.Equal(16.0, total raised ace [ debuffed (card Three Hearts); card Nine Spades ])

[<Fact>]
let ``Shoot the Moon adds 13 mult per held Queen`` () =
    let moon = [ joker "j_shoot_the_moon" ]
    let ace = [ card Ace Spades ]
    Assert.Equal(432.0, total moon ace [ card Queen Spades; card Queen Hearts; card Five Diamonds ])
    Assert.Equal(16.0, total moon ace [ card King Spades ])
    Assert.Equal(16.0, total moon ace [ debuffed (card Queen Spades) ])

[<Fact>]
let ``Jokers without a score effect leave the total unchanged and count as implemented`` () =
    let keys =
        [ "j_8_ball"
          "j_business"
          "j_ticket"
          "j_rough_gem"
          "j_reserved_parking"
          "j_bloodstone"
          "j_hiker" ]

    for key in keys do
        Assert.Equal(52.0, total [ joker key ] (pair Eight Diamonds Hearts) [ card King Hearts ])
        Assert.Contains(key, Balatro.Core.JokerRegistry.implemented)
