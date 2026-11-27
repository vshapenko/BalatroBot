module Balatro.Tests.SimJokersTests

open System
open System.IO
open System.Text.Json
open System.Text.RegularExpressions
open Xunit
open Balatro.Core.Domain
open Balatro.Bot
open Balatro.Bot.SimJokers

let private playing rank suit : Card =
    { Rank = rank
      Suit = suit
      Enhancement = None
      Edition = None
      Seal = None
      Debuffed = false }

let private fixtures =
    JsonDocument
        .Parse(File.ReadAllText(AppContext.BaseDirectory + "fixtures/jokers.json"))
        .RootElement.EnumerateArray()
    |> Seq.map (fun e ->
        { Api.Card.Id = 0
          Api.Card.Key = e.GetProperty("key").GetString()
          Api.Card.Set = Api.CardSet.Joker
          Api.Card.Label = e.GetProperty("label").GetString()
          Api.Card.Value =
            { Api.CardValue.Suit = None
              Rank = None
              Effect = e.GetProperty("effect").GetString() }
          Api.Card.Modifier =
            { Api.CardModifier.Eternal = false
              Rental = false
              Perishable = None
              Seal = None
              Edition = None
              Enhancement = None }
          Api.Card.State =
            { Api.CardState.Hidden = false
              Debuff = false
              Highlight = false }
          Api.Card.Cost =
            { Api.CardCost.Sell = e.GetProperty("sell").GetInt32()
              Buy = e.GetProperty("buy").GetInt32() } })
    |> List.ofSeq

let private fixture key = fixtures |> List.find (fun c -> c.Key = key)

let private ranks =
    [ Two; Three; Four; Five; Six; Seven; Eight; Nine; Ten; Jack; Queen; King; Ace ]

let private fullDeck =
    [ for suit in [ Spades; Hearts; Clubs; Diamonds ] do
          for rank in ranks -> playing rank suit ]

let private table =
    { Money = 4
      Deck = fullDeck
      DrawPile = 44
      JokerSlots = 5
      Jokers = []
      HandPlays = Map.empty
      Skips = 0 }

let private on key = { table with Jokers = [ key ] }

let private run (table: Table) key events =
    let rng = Random 1

    events
    |> List.fold
        (fun (state, paid) event ->
            match state with
            | None -> None, paid
            | Some current ->
                let next, dollars = step rng table event current
                next, paid + dollars)
        (Some(create rng table (fixture key)), 0)

let private shown (table: Table) key events =
    run table key events |> fst |> Option.map (card table)

let private valueOf (table: Table) key events =
    shown table key events |> Option.bind (fun c -> (Mapping.joker c).Value)

let private textOf (table: Table) key events =
    shown table key events |> Option.map (fun c -> c.Value.Effect)

let private paid (table: Table) key events = run table key events |> snd

let private hand cards = HandPlayed(Pair, cards, cards, [])

let private ace = playing Ace Spades
let private king = playing King Hearts
let private two = playing Two Clubs

let private repeat n event = List.replicate n event

[<Fact>]
let ``Green Joker after 3 hands and 1 discard shows +2`` () =
    let events = repeat 3 (hand [ ace ]) @ [ Discarded [ ace ] ]
    Assert.Equal(Some 2.0, valueOf (on "j_green_joker") "j_green_joker" events)

[<Fact>]
let ``Green Joker never goes below 0`` () =
    Assert.Equal(Some 0.0, valueOf (on "j_green_joker") "j_green_joker" (repeat 2 (Discarded [ ace ])))

[<Fact>]
let ``Ride the Bus shows +1 after a hand without a face card and 0 after a King`` () =
    let t = on "j_ride_the_bus"
    Assert.Equal(Some 1.0, valueOf t "j_ride_the_bus" [ hand [ ace ] ])
    Assert.Equal(Some 0.0, valueOf t "j_ride_the_bus" [ hand [ ace ]; hand [ king ] ])

[<Fact>]
let ``Ice Cream shows +5 Chips after 19 hands and is destroyed by the 20th`` () =
    let t = on "j_ice_cream"
    Assert.StartsWith("+5 Chips", (textOf t "j_ice_cream" (repeat 19 (hand [ ace ]))).Value)
    Assert.Equal(None, textOf t "j_ice_cream" (repeat 20 (hand [ ace ])))

[<Fact>]
let ``Popcorn shows +4 Mult after 4 rounds and is destroyed after 5`` () =
    let t = on "j_popcorn"
    Assert.StartsWith("+4 Mult", (textOf t "j_popcorn" (repeat 4 (RoundWon(false, 0)))).Value)
    Assert.Equal(None, textOf t "j_popcorn" (repeat 5 (RoundWon(false, 0))))

[<Fact>]
let ``Ramen shows X1.6 after 40 discarded cards and is destroyed at 100`` () =
    let t = on "j_ramen"
    let eight = Discarded(List.replicate 8 ace)
    Assert.StartsWith("X1.6 Mult", (textOf t "j_ramen" (repeat 5 eight)).Value)
    Assert.StartsWith("X1.01 Mult", (textOf t "j_ramen" (repeat 99 (Discarded [ ace ]))).Value)
    Assert.Equal(None, textOf t "j_ramen" (repeat 100 (Discarded [ ace ])))

[<Fact>]
let ``Loyalty Card is Active after 5 hands and shows 5 remaining after 6`` () =
    let t = on "j_loyalty_card"
    Assert.Contains("5 remaining", (textOf t "j_loyalty_card" []).Value)
    Assert.Contains("1 remaining", (textOf t "j_loyalty_card" (repeat 4 (hand [ ace ]))).Value)
    Assert.Contains("Active!", (textOf t "j_loyalty_card" (repeat 5 (hand [ ace ]))).Value)
    Assert.Contains("5 remaining", (textOf t "j_loyalty_card" (repeat 6 (hand [ ace ]))).Value)

[<Fact>]
let ``Constellation shows X1.3 after 3 planets and ignores tarots`` () =
    let t = on "j_constellation"
    let planets = repeat 3 (ConsumableUsed Api.CardSet.Planet)
    Assert.Equal(Some 1.3, valueOf t "j_constellation" planets)
    Assert.Equal(Some 1.3, valueOf t "j_constellation" (planets @ [ ConsumableUsed Api.CardSet.Tarot ]))

[<Fact>]
let ``Hologram shows X1.5 after 2 added cards`` () =
    Assert.Equal(Some 1.5, valueOf (on "j_hologram") "j_hologram" [ CardsAdded [ ace; king ] ])

[<Fact>]
let ``Campfire shows X1.5 after 2 sales and X1 after a boss round`` () =
    let t = on "j_campfire"
    let sales = [ Sold "j_joker"; Sold "c_mercury" ]
    Assert.Equal(Some 1.5, valueOf t "j_campfire" sales)
    Assert.Equal(Some 1.5, valueOf t "j_campfire" (sales @ [ RoundWon(false, 0) ]))
    Assert.Equal(Some 1.0, valueOf t "j_campfire" (sales @ [ RoundWon(true, 0) ]))

[<Fact>]
let ``Obelisk grows by 0.2 on a hand that is not the most played and resets on the most played`` () =
    let t = { on "j_obelisk" with HandPlays = Map.ofList [ Pair, 3 ] }
    Assert.Equal(Some 1.2, valueOf t "j_obelisk" [ HandPlayed(HighCard, [ ace ], [ ace ], []) ])
    Assert.Equal(Some 1.4, valueOf t "j_obelisk" (repeat 2 (HandPlayed(HighCard, [ ace ], [ ace ], []))))

    Assert.Equal(
        Some 1.0,
        valueOf t "j_obelisk" [ HandPlayed(HighCard, [ ace ], [ ace ], []); HandPlayed(Pair, [ ace ], [ ace ], []) ]
    )

[<Fact>]
let ``Runner shows +15 Chips after a Straight and Square +4 after 4 cards`` () =
    Assert.Equal(Some 15.0, valueOf (on "j_runner") "j_runner" [ HandPlayed(Straight, [ ace ], [ ace ], []) ])
    Assert.Equal(Some 0.0, valueOf (on "j_runner") "j_runner" [ hand [ ace ] ])

    Assert.Equal(
        Some 4.0,
        valueOf (on "j_square") "j_square" [ HandPlayed(HighCard, List.replicate 4 ace, [ ace ], []) ]
    )

[<Fact>]
let ``Spare Trousers shows +2 Mult after a Two Pair and a Full House gives +4`` () =
    let events = [ HandPlayed(TwoPair, [ ace ], [ ace ], []); HandPlayed(FullHouse, [ ace ], [ ace ], []) ]
    Assert.Equal(Some 4.0, valueOf (on "j_trousers") "j_trousers" events)

[<Fact>]
let ``Wee Joker shows +16 Chips after two scored 2s and skips a Stone card`` () =
    let stone = { two with Enhancement = Some Enhancement.Stone }
    Assert.Equal(Some 16.0, valueOf (on "j_wee") "j_wee" [ HandPlayed(Pair, [ two; two ], [ two; two ], []) ])
    Assert.Equal(Some 0.0, valueOf (on "j_wee") "j_wee" [ HandPlayed(Pair, [ stone ], [ stone ], []) ])

[<Fact>]
let ``Vampire shows X1.2 after 2 scored enhanced cards`` () =
    let bonus = { ace with Enhancement = Some Enhancement.Bonus }
    let events = [ HandPlayed(Pair, [ bonus; bonus ], [ bonus; bonus ], []) ]
    Assert.Equal(Some 1.2, valueOf (on "j_vampire") "j_vampire" events)

[<Fact>]
let ``Lucky Cat grows only on scored Lucky cards`` () =
    let lucky = { ace with Enhancement = Some Enhancement.Lucky }
    let value = valueOf (on "j_lucky_cat") "j_lucky_cat" (repeat 200 (hand [ lucky ])) |> Option.get
    Assert.InRange(value, 1.0 + 0.25 * 20.0, 1.0 + 0.25 * 80.0)
    Assert.Equal(Some 1.0, valueOf (on "j_lucky_cat") "j_lucky_cat" (repeat 200 (hand [ ace ])))

[<Fact>]
let ``Flash Card shows +4 Mult after 2 rerolls and Red Card +3 Mult after a skipped pack`` () =
    Assert.Equal(Some 4.0, valueOf (on "j_flash") "j_flash" (repeat 2 Rerolled))
    Assert.Equal(Some 3.0, valueOf (on "j_red_card") "j_red_card" [ PackSkipped ])

[<Fact>]
let ``Madness shows X1.5 after a small blind and ignores a boss blind`` () =
    Assert.Equal(Some 1.5, valueOf (on "j_madness") "j_madness" [ BlindSelected false ])
    Assert.Equal(Some 1.0, valueOf (on "j_madness") "j_madness" [ BlindSelected true ])

[<Fact>]
let ``Ceremonial Dagger adds double the sell value of the joker to its right`` () =
    let t = { table with Jokers = [ "j_ceremonial"; "j_blueprint" ] }
    Assert.Equal(Some 10.0, valueOf t "j_ceremonial" [ BlindSelected true ])
    Assert.Equal(Some 0.0, valueOf (on "j_ceremonial") "j_ceremonial" [ BlindSelected true ])

[<Fact>]
let ``Glass Joker shows X1.75 after a Glass card breaks and Caino X3 after 2 faces are destroyed`` () =
    let glass = { ace with Enhancement = Some Enhancement.Glass }
    Assert.Equal(Some 1.75, valueOf (on "j_glass") "j_glass" [ CardsDestroyed [ glass; ace ] ])
    Assert.Equal(Some 3.0, valueOf (on "j_caino") "j_caino" [ CardsDestroyed [ king; king; ace ] ])

[<Fact>]
let ``Fortune Teller shows +2 after 2 tarots and Satellite pays 2 after 2 planets`` () =
    let tarots = repeat 2 (ConsumableUsed Api.CardSet.Tarot)
    let planets = repeat 2 (ConsumableUsed Api.CardSet.Planet)
    Assert.Equal(Some 2.0, valueOf (on "j_fortune_teller") "j_fortune_teller" tarots)
    Assert.Equal(2, paid (on "j_satellite") "j_satellite" (planets @ [ RoundWon(false, 0) ]))

[<Fact>]
let ``Hit the Road shows X2 after 2 Jacks and resets when the round ends`` () =
    let jack = playing Jack Clubs
    let t = on "j_hit_the_road"
    Assert.Equal(Some 2.0, valueOf t "j_hit_the_road" [ Discarded [ jack; jack; ace ] ])
    Assert.Equal(Some 1.0, valueOf t "j_hit_the_road" [ Discarded [ jack; jack ]; RoundWon(false, 0) ])

[<Fact>]
let ``Yorick shows X2 after 23 discarded cards with 23 more to go`` () =
    let t = on "j_yorick"
    Assert.Equal(Some 1.0, valueOf t "j_yorick" [ Discarded(List.replicate 22 ace) ])
    Assert.Equal(Some 2.0, valueOf t "j_yorick" [ Discarded(List.replicate 23 ace) ])
    Assert.Contains("[23]", (textOf t "j_yorick" [ Discarded(List.replicate 23 ace) ]).Value)
    Assert.Contains("[1]", (textOf t "j_yorick" [ Discarded(List.replicate 22 ace) ]).Value)

[<Fact>]
let ``Castle shows +6 Chips after 2 discarded cards of its suit`` () =
    let suit =
        Regex.Match((textOf (on "j_castle") "j_castle" []).Value, @"discarded (\w+) card").Groups[1].Value

    let matching = fullDeck |> List.find (fun c -> string c.Suit = suit + "s")
    let other = fullDeck |> List.find (fun c -> string c.Suit <> suit + "s")
    Assert.Equal(Some 6.0, valueOf (on "j_castle") "j_castle" [ Discarded [ matching; matching; other; other ] ])

[<Fact>]
let ``Blue Joker with 40 cards left shows +80 Chips`` () =
    Assert.Equal(Some 80.0, valueOf { on "j_blue_joker" with DrawPile = 40 } "j_blue_joker" [])

[<Fact>]
let ``Bull with 23 dollars shows +46 Chips and Bootstraps +8 Mult`` () =
    Assert.Equal(Some 46.0, valueOf { on "j_bull" with Money = 23 } "j_bull" [])
    Assert.Equal(Some 8.0, valueOf { on "j_bootstraps" with Money = 23 } "j_bootstraps" [])

[<Fact>]
let ``Stencil with 2 other jokers in 5 slots shows X3`` () =
    let t = { table with Jokers = [ "j_stencil"; "j_joker"; "j_sly" ] }
    Assert.Equal(Some 3.0, valueOf t "j_stencil" [])

[<Fact>]
let ``Abstract Joker with 3 jokers shows +9 Mult`` () =
    let t = { table with Jokers = [ "j_abstract"; "j_joker"; "j_sly" ] }
    Assert.Equal(Some 9.0, valueOf t "j_abstract" [])

[<Fact>]
let ``Swashbuckler adds the sell values of the other jokers`` () =
    let t = { table with Jokers = [ "j_swashbuckler"; "j_joker"; "j_caino" ] }
    Assert.Equal(Some 11.0, valueOf t "j_swashbuckler" [])

[<Fact>]
let ``Swashbuckler sums the SimContent sell value of the other jokers for all 150 jokers`` () =
    Assert.Equal(150, SimContent.jokers.Length)

    for info in SimContent.jokers do
        let t = { table with Jokers = [ "j_swashbuckler"; info.Key ] }
        Assert.Equal(Some(float (SimContent.jokerCard info None).Cost.Sell), valueOf t "j_swashbuckler" [])

[<Fact>]
let ``deck counts show Steel X1.4, Stone +50, Erosion +8, Driver 16 and Throwback X1.5`` () =
    let enhance enhancement n =
        fullDeck |> List.truncate n |> List.map (fun c -> { c with Enhancement = Some enhancement })

    let rest n = fullDeck |> List.skip n
    let steel = { on "j_steel_joker" with Deck = enhance Enhancement.Steel 2 @ rest 2 }
    Assert.Equal(Some 1.4, valueOf steel "j_steel_joker" [])
    Assert.Equal(Some 50.0, valueOf { on "j_stone" with Deck = enhance Enhancement.Stone 2 @ rest 2 } "j_stone" [])
    Assert.Equal(Some 8.0, valueOf { on "j_erosion" with Deck = fullDeck |> List.skip 2 } "j_erosion" [])
    let driver = { on "j_drivers_license" with Deck = enhance Enhancement.Gold 16 @ rest 16 }
    Assert.Equal(Some 16.0, valueOf driver "j_drivers_license" [])
    Assert.Equal(Some 1.5, valueOf { on "j_throwback" with Skips = 2 } "j_throwback" [])

[<Fact>]
let ``Golden Joker pays 4 at round end`` () =
    Assert.Equal(4, paid (on "j_golden") "j_golden" [ RoundWon(false, 0) ])
    Assert.Equal(0, paid (on "j_golden") "j_golden" [ hand [ ace ] ])

[<Fact>]
let ``Egg sell value rises by 3 per round`` () =
    let sells n =
        (shown (on "j_egg") "j_egg" (repeat n (RoundWon(false, 0)))).Value.Cost.Sell

    Assert.Equal(2, sells 0)
    Assert.Equal(5, sells 1)
    Assert.Equal(11, sells 3)

[<Fact>]
let ``Gift Card sell value rises by 1 per round`` () =
    Assert.Equal(5, (shown (on "j_gift") "j_gift" (repeat 2 (RoundWon(false, 0)))).Value.Cost.Sell)

[<Fact>]
let ``Rocket pays 1 then 3 once a boss is beaten and 3 after`` () =
    let t = on "j_rocket"
    Assert.Equal(1, paid t "j_rocket" [ RoundWon(false, 0) ])
    Assert.Equal(1 + 3, paid t "j_rocket" [ RoundWon(false, 0); RoundWon(true, 0) ])
    Assert.Equal(1 + 3 + 3, paid t "j_rocket" [ RoundWon(false, 0); RoundWon(true, 0); RoundWon(false, 0) ])
    Assert.Contains("Earn $3", (textOf t "j_rocket" [ RoundWon(true, 0) ]).Value)

[<Fact>]
let ``Delayed Gratification pays 2 for each of 3 unused discards only when none was used`` () =
    let t = on "j_delayed_grat"
    Assert.Equal(6, paid t "j_delayed_grat" [ RoundWon(false, 3) ])
    Assert.Equal(0, paid t "j_delayed_grat" [ Discarded [ ace ]; RoundWon(false, 2) ])
    let used = [ BlindSelected false; Discarded [ ace ]; RoundWon(false, 2) ]
    Assert.Equal(6, paid t "j_delayed_grat" (used @ [ BlindSelected false; RoundWon(false, 3) ]))

[<Fact>]
let ``To the Moon pays 4 extra interest on 23 dollars and 5 on 100`` () =
    Assert.Equal(4, paid { on "j_to_the_moon" with Money = 23 } "j_to_the_moon" [ RoundWon(false, 0) ])
    Assert.Equal(5, paid { on "j_to_the_moon" with Money = 100 } "j_to_the_moon" [ RoundWon(false, 0) ])

[<Fact>]
let ``Cloud 9 pays 4 for the 9s of a full deck`` () =
    Assert.Equal(4, paid (on "j_cloud_9") "j_cloud_9" [ RoundWon(false, 0) ])
    Assert.Contains("$4", (textOf (on "j_cloud_9") "j_cloud_9" []).Value)

[<Fact>]
let ``Faceless Joker pays 5 for 3 faces in one discard and 0 for 2`` () =
    let t = on "j_faceless"
    Assert.Equal(5, paid t "j_faceless" [ Discarded [ king; king; king ] ])
    Assert.Equal(0, paid t "j_faceless" [ Discarded [ king; king; ace ] ])
    let pareidolia = { t with Jokers = [ "j_faceless"; "j_pareidolia" ] }
    Assert.Equal(5, paid pareidolia "j_faceless" [ Discarded [ ace; two; ace ] ])

[<Fact>]
let ``Trading Card pays 3 for a single card on the first discard only`` () =
    let t = on "j_trading"
    Assert.Equal(3, paid t "j_trading" [ Discarded [ ace ] ])
    Assert.Equal(3, paid t "j_trading" [ Discarded [ ace ]; Discarded [ ace ] ])
    Assert.Equal(0, paid t "j_trading" [ Discarded [ ace; ace ] ])
    Assert.Equal(6, paid t "j_trading" [ Discarded [ ace ]; BlindSelected false; Discarded [ ace ] ])

[<Fact>]
let ``Mail-In Rebate pays 5 per discarded card of the shown rank`` () =
    let t = on "j_mail"
    let shownRank = Regex.Match((textOf t "j_mail" []).Value, @"discarded (\w+),").Groups[1].Value
    let matching = fullDeck |> List.filter (fun c -> Rank.label c.Rank = shownRank) |> List.truncate 3
    Assert.Equal(15, paid t "j_mail" [ Discarded matching ])

[<Fact>]
let ``Business Card pays 2 per face card about half the time`` () =
    let total = paid (on "j_business") "j_business" (repeat 400 (hand [ king ]))
    Assert.InRange(total, 2 * 140, 2 * 260)
    Assert.Equal(0, paid (on "j_business") "j_business" (repeat 50 (hand [ ace ])))

[<Fact>]
let ``Reserved Parking pays 1 per held face card about half the time`` () =
    let events = [ HandPlayed(Pair, [ ace ], [ ace ], List.replicate 400 king) ]
    let total = paid (on "j_reserved_parking") "j_reserved_parking" events
    Assert.InRange(total, 140, 260)

[<Fact>]
let ``Rough Gem pays 2 for 2 scored Diamonds and Golden Ticket 4 for a Gold card`` () =
    let diamond = playing Five Diamonds
    let gold = { ace with Enhancement = Some Enhancement.Gold }
    let diamonds = [ diamond; diamond; ace ]
    Assert.Equal(2, paid (on "j_rough_gem") "j_rough_gem" [ HandPlayed(Pair, diamonds, diamonds, []) ])
    Assert.Equal(4, paid (on "j_ticket") "j_ticket" [ HandPlayed(Pair, [ gold; ace ], [ gold; ace ], []) ])

[<Fact>]
let ``Matador pays 8 when a scored card is debuffed`` () =
    let debuffed = { ace with Debuffed = true }
    Assert.Equal(8, paid (on "j_matador") "j_matador" [ HandPlayed(Pair, [ debuffed ], [ debuffed ], []) ])
    Assert.Equal(0, paid (on "j_matador") "j_matador" [ hand [ ace ] ])

[<Fact>]
let ``To Do List pays 4 when its hand is played and keeps one visible hand`` () =
    let t = on "j_todo_list"
    let target = Regex.Match((textOf t "j_todo_list" []).Value, @"is a ([A-Za-z ]+),").Groups[1].Value
    let hands = [ HighCard; Pair; TwoPair; ThreeOfAKind; Straight; Flush; FullHouse; FourOfAKind; StraightFlush ]
    Assert.Contains(target, hands |> List.map HandType.label)
    let played = hands |> List.find (fun h -> HandType.label h = target)
    Assert.Equal(4, paid t "j_todo_list" [ HandPlayed(played, [ ace ], [ ace ], []) ])

    let seen = (hands |> List.map (fun h -> HandType.label h)) |> Set.ofList

    for round in 1..200 do
        let events = repeat round (RoundWon(false, 0))
        Assert.Contains(Regex.Match((textOf t "j_todo_list" events).Value, @"is a ([A-Za-z ]+),").Groups[1].Value, seen)

[<Fact>]
let ``Gros Michel and Cavendish can be destroyed at round end`` () =
    let survives key n = (run (on key) key (repeat n (RoundWon(false, 0))) |> fst).IsSome
    Assert.False(survives "j_gros_michel" 100)
    Assert.True(survives "j_cavendish" 100)

[<Fact>]
let ``The Idol text always parses to a rank and suit of a deck card over 200 rounds`` () =
    let deck = [ playing Seven Hearts; playing King Clubs; playing Two Spades ]
    let t = { on "j_idol" with Deck = deck }
    let pairs = deck |> List.map (fun c -> Rank.label c.Rank, string c.Suit) |> Set.ofList
    let rng = Random 3
    let mutable state = create rng t (fixture "j_idol")
    let seen = Collections.Generic.HashSet<string * string>()

    for _ in 1..200 do
        let text = (card t state).Value.Effect
        let m = Regex.Match(text, @"played (\w+) of (\w+) gives")
        Assert.True m.Success
        let pair = m.Groups[1].Value, m.Groups[2].Value
        Assert.Contains(pair, pairs)
        seen.Add pair |> ignore
        state <- step rng t (RoundWon(false, 0)) state |> fst |> Option.get

    Assert.Equal(3, seen.Count)

[<Fact>]
let ``Ancient Joker suit changes every round and always parses to a suit`` () =
    let t = on "j_ancient"
    let rng = Random 5

    let suitOf state =
        Regex.Match((card t state).Value.Effect, @"with (\w+) suit").Groups[1].Value

    let mutable state = create rng t (fixture "j_ancient")

    for _ in 1..50 do
        let before = suitOf state
        state <- step rng t (RoundWon(false, 0)) state |> fst |> Option.get
        Assert.Contains(suitOf state, [ "Spade"; "Heart"; "Club"; "Diamond" ])
        Assert.NotEqual<string>(before, suitOf state)

let private templateExceptions =
    set [ "j_bull"; "j_bootstraps"; "j_idol"; "j_ancient"; "j_castle"; "j_mail"; "j_todo_list" ]

[<Fact>]
let ``all 150 jokers render their fixture text right after create`` () =
    let rng = Random 11

    for joker in fixtures |> List.filter (fun j -> not (templateExceptions.Contains j.Key)) do
        let t = on joker.Key
        let state = create rng t joker
        let rendered = card t state
        Assert.Equal<string>(joker.Value.Effect, rendered.Value.Effect)
        Assert.Equal(joker.Cost.Sell, rendered.Cost.Sell)

[<Fact>]
let ``random jokers differ from the fixture text only in their target`` () =
    let rng = Random 11

    let masks =
        [ "j_idol", @"played \w+ of \w+ gives"
          "j_ancient", @"with \w+ suit"
          "j_castle", @"discarded \w+ card"
          "j_mail", @"discarded \w+,"
          "j_todo_list", @"is a [A-Za-z ]+," ]

    for key, mask in masks do
        let joker = fixture key
        let t = on key
        let rendered = (card t (create rng t joker)).Value.Effect
        Assert.Equal<string>(Regex.Replace(joker.Value.Effect, mask, "#"), Regex.Replace(rendered, mask, "#"))

[<Fact>]
let ``a joker without rules keeps its text and pays nothing`` () =
    let events = [ hand [ ace ]; Discarded [ ace ]; RoundWon(true, 3); Rerolled; Sold "j_joker"; PackSkipped ]
    let state, dollars = run (on "j_joker") "j_joker" events
    Assert.Equal(0, dollars)
    Assert.Equal<string>((fixture "j_joker").Value.Effect, (card (on "j_joker") state.Value).Value.Effect)
