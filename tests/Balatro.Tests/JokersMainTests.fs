module Balatro.Tests.JokersMainTests

open System
open System.IO
open System.Text.Json
open Xunit
open Balatro.Core
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

let private joker key : Joker =
    { Key = key
      Name = key
      Edition = None
      Debuffed = false
      Value = None
      SellValue = 0
      Text = "" }

let private withValue value (j: Joker) = { j with Value = Some value }

let private totalIn (situation: Situation) (jokers: Joker list) played held =
    (scoreWith
        { situation with Jokers = jokers }
        { Played = played
          Held = held
          Levels = levels })
        .Total

let private total jokers played held =
    totalIn Situation.empty jokers played held

let private debuffed (c: Card) = { c with Debuffed = true }

let private kk = [ card King Spades; card King Hearts ]
let private threeKings = [ card King Spades; card King Hearts; card King Clubs ]
let private fourKings = [ card King Spades; card King Hearts; card King Clubs; card King Diamonds ]
let private twoPair = [ card King Spades; card King Hearts; card Queen Spades; card Queen Hearts ]

let private straight =
    [ card Five Spades; card Six Hearts; card Seven Clubs; card Eight Diamonds; card Nine Spades ]

let private flush =
    [ card Two Spades; card Five Spades; card Seven Spades; card Nine Spades; card Jack Spades ]

let private highCard = [ card King Spades; card Five Hearts ]

[<Fact>]
let ``Mult jokers by hand type add their mult when the hand contains the type`` () =
    Assert.Equal(300.0, total [ joker "j_jolly" ] kk [])
    Assert.Equal(660.0, total [ joker "j_jolly" ] threeKings [])
    Assert.Equal(900.0, total [ joker "j_zany" ] threeKings [])
    Assert.Equal(720.0, total [ joker "j_mad" ] twoPair [])
    Assert.Equal(1040.0, total [ joker "j_crazy" ] straight [])
    Assert.Equal(952.0, total [ joker "j_droll" ] flush [])

[<Fact>]
let ``Mult jokers by hand type do nothing when the hand lacks the type`` () =
    Assert.Equal(15.0, total [ joker "j_jolly" ] highCard [])
    Assert.Equal(60.0, total [ joker "j_zany" ] kk [])
    Assert.Equal(60.0, total [ joker "j_mad" ] kk [])
    Assert.Equal(60.0, total [ joker "j_crazy" ] kk [])
    Assert.Equal(60.0, total [ joker "j_droll" ] kk [])

[<Fact>]
let ``Chip jokers by hand type add their chips when the hand contains the type`` () =
    Assert.Equal(480.0, total [ joker "j_wily" ] threeKings [])
    Assert.Equal(280.0, total [ joker "j_clever" ] twoPair [])
    Assert.Equal(660.0, total [ joker "j_devious" ] straight [])
    Assert.Equal(592.0, total [ joker "j_crafty" ] flush [])

[<Fact>]
let ``Chip jokers by hand type do nothing when the hand lacks the type`` () =
    Assert.Equal(60.0, total [ joker "j_wily" ] kk [])
    Assert.Equal(60.0, total [ joker "j_clever" ] kk [])
    Assert.Equal(60.0, total [ joker "j_devious" ] kk [])
    Assert.Equal(60.0, total [ joker "j_crafty" ] kk [])

[<Fact>]
let ``Family jokers multiply when the hand contains their type`` () =
    Assert.Equal(120.0, total [ joker "j_duo" ] kk [])
    Assert.Equal(360.0, total [ joker "j_duo" ] threeKings [])
    Assert.Equal(540.0, total [ joker "j_trio" ] threeKings [])
    Assert.Equal(2800.0, total [ joker "j_family" ] fourKings [])
    Assert.Equal(780.0, total [ joker "j_order" ] straight [])
    Assert.Equal(544.0, total [ joker "j_tribe" ] flush [])

[<Fact>]
let ``Family jokers do nothing when the hand lacks their type`` () =
    Assert.Equal(15.0, total [ joker "j_duo" ] highCard [])
    Assert.Equal(60.0, total [ joker "j_trio" ] kk [])
    Assert.Equal(180.0, total [ joker "j_family" ] threeKings [])
    Assert.Equal(60.0, total [ joker "j_order" ] kk [])
    Assert.Equal(60.0, total [ joker "j_tribe" ] kk [])

[<Fact>]
let ``Half Joker counts the played cards, not the scoring ones`` () =
    Assert.Equal(660.0, total [ joker "j_half" ] kk [])
    Assert.Equal(660.0, total [ joker "j_half" ] (kk @ [ card Five Clubs ]) [])
    Assert.Equal(60.0, total [ joker "j_half" ] (kk @ [ card Five Clubs; card Seven Diamonds ]) [])

[<Fact>]
let ``Fixed jokers always act`` () =
    Assert.Equal(510.0, total [ joker "j_gros_michel" ] kk [])
    Assert.Equal(180.0, total [ joker "j_cavendish" ] kk [])
    Assert.Equal(560.0, total [ joker "j_stuntman" ] kk [])

[<Fact>]
let ``Blackboard needs every held card to be a Spade or a Club`` () =
    let blackboard = joker "j_blackboard"
    Assert.Equal(180.0, total [ blackboard ] kk [ card Five Spades; card Seven Clubs ])
    Assert.Equal(180.0, total [ blackboard ] kk [])
    Assert.Equal(60.0, total [ blackboard ] kk [ card Five Spades; card Seven Hearts ])

[<Fact>]
let ``Blackboard counts Wild cards and debuffed black cards but not Stone cards`` () =
    let blackboard = joker "j_blackboard"
    let wildHeart = card Five Hearts |> withEnhancement Enhancement.Wild
    let stone = card Five Spades |> withEnhancement Enhancement.Stone
    Assert.Equal(180.0, total [ blackboard ] kk [ wildHeart ])
    Assert.Equal(180.0, total [ blackboard ] kk [ debuffed (card Five Spades) ])
    Assert.Equal(60.0, total [ blackboard ] kk [ debuffed wildHeart ])
    Assert.Equal(60.0, total [ blackboard ] kk [ stone ])

[<Fact>]
let ``Flower Pot needs the scoring cards to cover all four suits`` () =
    let flowerPot = joker "j_flower_pot"
    let spread = [ card King Spades; card King Hearts; card Queen Clubs; card Queen Diamonds ]
    Assert.Equal(360.0, total [ flowerPot ] spread [])
    Assert.Equal(60.0, total [ flowerPot ] kk [])
    Assert.Equal(120.0, total [ flowerPot ] [ card King Spades; card King Hearts; card Queen Clubs; card Queen Spades ] [])

[<Fact>]
let ``Flower Pot lets a Wild card fill the missing suit and counts debuffed plain cards`` () =
    let flowerPot = joker "j_flower_pot"
    let wildQueen = card Queen Spades |> withEnhancement Enhancement.Wild
    let withWild = [ card King Spades; card King Hearts; card Queen Clubs; wildQueen ]
    let withDebuffedDiamond = [ card King Spades; card King Hearts; card Queen Clubs; debuffed (card Queen Diamonds) ]
    let withDebuffedWild = [ card King Spades; card King Hearts; card Queen Clubs; debuffed wildQueen ]
    Assert.Equal(360.0, total [ flowerPot ] withWild [])
    Assert.Equal(300.0, total [ flowerPot ] withDebuffedDiamond [])
    Assert.Equal(100.0, total [ flowerPot ] withDebuffedWild [])

[<Fact>]
let ``Seeing Double needs a scoring Club and a scoring card of another suit`` () =
    let seeingDouble = joker "j_seeing_double"
    Assert.Equal(120.0, total [ seeingDouble ] [ card King Clubs; card King Hearts ] [])
    Assert.Equal(60.0, total [ seeingDouble ] kk [])
    Assert.Equal(60.0, total [ seeingDouble ] [ card King Clubs; card King Clubs ] [])
    Assert.Equal(60.0, total [ seeingDouble ] (kk @ [ card Five Clubs ]) [])

[<Fact>]
let ``Seeing Double lets a Wild card stand in for the missing suit`` () =
    let seeingDouble = joker "j_seeing_double"
    let wildHeart = card King Hearts |> withEnhancement Enhancement.Wild
    Assert.Equal(120.0, total [ seeingDouble ] [ card King Spades; wildHeart ] [])
    Assert.Equal(120.0, total [ seeingDouble ] [ card King Clubs; wildHeart ] [])

[<Fact>]
let ``Banner adds 30 chips per remaining discard`` () =
    Assert.Equal(240.0, totalIn { Situation.empty with DiscardsLeft = 3 } [ joker "j_banner" ] kk [])
    Assert.Equal(60.0, totalIn { Situation.empty with DiscardsLeft = 0 } [ joker "j_banner" ] kk [])

[<Fact>]
let ``Mystic Summit adds mult only with no discards left`` () =
    Assert.Equal(510.0, totalIn { Situation.empty with DiscardsLeft = 0 } [ joker "j_mystic_summit" ] kk [])
    Assert.Equal(60.0, totalIn { Situation.empty with DiscardsLeft = 2 } [ joker "j_mystic_summit" ] kk [])

[<Fact>]
let ``Acrobat multiplies only on the final hand`` () =
    Assert.Equal(180.0, totalIn { Situation.empty with HandsLeft = 0 } [ joker "j_acrobat" ] kk [])
    Assert.Equal(60.0, totalIn { Situation.empty with HandsLeft = 2 } [ joker "j_acrobat" ] kk [])

[<Fact>]
let ``Supernova adds the run plays of the hand type including this play`` () =
    let played = { Situation.empty with HandPlays = Map.ofList [ Pair, (4, 1) ] }
    Assert.Equal(210.0, totalIn played [ joker "j_supernova" ] kk [])
    Assert.Equal(90.0, total [ joker "j_supernova" ] kk [])

[<Fact>]
let ``Card Sharp multiplies when the hand type was already played this round`` () =
    let situation plays = { Situation.empty with HandPlays = Map.ofList plays }
    Assert.Equal(180.0, totalIn (situation [ Pair, (3, 1) ]) [ joker "j_card_sharp" ] kk [])
    Assert.Equal(60.0, totalIn (situation [ Pair, (3, 0) ]) [ joker "j_card_sharp" ] kk [])
    Assert.Equal(60.0, total [ joker "j_card_sharp" ] kk [])

[<Fact>]
let ``Additive jokers add the value of their text`` () =
    [ "j_abstract", 9.0, 330.0
      "j_blue_joker", 88.0, 236.0
      "j_erosion", 8.0, 300.0
      "j_fortune_teller", 3.0, 150.0
      "j_stone", 75.0, 210.0
      "j_bull", 462.0, 984.0
      "j_swashbuckler", 7.0, 270.0
      "j_bootstraps", 158.0, 4800.0 ]
    |> List.iter (fun (key, value, expected) -> Assert.Equal(expected, total [ joker key |> withValue value ] kk []))

[<Fact>]
let ``Additive jokers with a value of zero do nothing`` () =
    [ "j_blue_joker"; "j_erosion"; "j_fortune_teller"; "j_stone"; "j_bull"; "j_swashbuckler"; "j_bootstraps" ]
    |> List.iter (fun key -> Assert.Equal(60.0, total [ joker key |> withValue 0.0 ] kk []))

[<Fact>]
let ``Multiplying jokers multiply by the value of their text`` () =
    [ "j_stencil", 5.0, 300.0
      "j_steel_joker", 2.0, 120.0
      "j_throwback", 1.5, 90.0 ]
    |> List.iter (fun (key, value, expected) -> Assert.Equal(expected, total [ joker key |> withValue value ] kk []))

[<Fact>]
let ``Multiplying jokers with a value of one do nothing`` () =
    [ "j_stencil"; "j_steel_joker"; "j_throwback" ]
    |> List.iter (fun key -> Assert.Equal(60.0, total [ joker key |> withValue 1.0 ] kk []))

[<Fact>]
let ``Driver's License multiplies from 16 enhanced cards`` () =
    Assert.Equal(180.0, total [ joker "j_drivers_license" |> withValue 16.0 ] kk [])
    Assert.Equal(60.0, total [ joker "j_drivers_license" |> withValue 15.0 ] kk [])

let private fixtureEffect key =
    JsonDocument
        .Parse(File.ReadAllText(AppContext.BaseDirectory + "fixtures/jokers.json"))
        .RootElement.EnumerateArray()
    |> Seq.find (fun element -> element.GetProperty("key").GetString() = key)
    |> fun element -> element.GetProperty("effect").GetString()

[<Fact>]
let ``Value jokers read their live card text`` () =
    [ "j_stencil", 300.0
      "j_steel_joker", 60.0
      "j_abstract", 150.0
      "j_blue_joker", 236.0
      "j_erosion", 60.0
      "j_fortune_teller", 60.0
      "j_stone", 60.0
      "j_bull", 984.0
      "j_swashbuckler", 60.0
      "j_throwback", 60.0
      "j_bootstraps", 4800.0
      "j_drivers_license", 60.0 ]
    |> List.iter (fun (key, expected) ->
        let text = fixtureEffect key

        let fromText =
            { joker key with
                Value = Mapping.currentValue text
                Text = text }

        Assert.Equal(expected, total [ fromText ] kk []))

let private noScoreKeys =
    [ "j_misprint"
      "j_superposition"
      "j_seance"
      "j_vagabond"
      "j_matador"
      "j_credit_card"
      "j_marble"
      "j_chaos"
      "j_delayed_grat"
      "j_egg"
      "j_burglar"
      "j_faceless"
      "j_riff_raff"
      "j_cloud_9"
      "j_rocket"
      "j_luchador"
      "j_gift"
      "j_turtle_bean"
      "j_to_the_moon"
      "j_hallucination"
      "j_juggler"
      "j_drunkard"
      "j_golden"
      "j_diet_cola"
      "j_trading"
      "j_mr_bones"
      "j_troubadour"
      "j_certificate"
      "j_ring_master"
      "j_merry_andy"
      "j_invisible"
      "j_satellite"
      "j_cartomancer"
      "j_astronomer"
      "j_burnt"
      "j_chicot"
      "j_perkeo"
      "j_mail"
      "j_oops" ]

[<Fact>]
let ``Jokers without a score effect leave the total unchanged and count as implemented`` () =
    noScoreKeys
    |> List.iter (fun key ->
        Assert.Equal(60.0, total [ joker key ] kk [])
        Assert.Contains(key, JokerRegistry.implemented))
