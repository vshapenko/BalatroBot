module Balatro.Tests.ShopTests

open System
open System.IO
open System.Text.Json
open Xunit
open Balatro.Core
open Balatro.Core.Domain
open Balatro.Bot
open Balatro.Bot.Shop
open Balatro.Tests.HandsTests

let private joker key : Jokers.Joker =
    { Key = key
      Name = key
      Edition = None
      Debuffed = false
      Value = None
      SellValue = 0
      Text = "" }

let private deck =
    [ for suit in [ Spades; Hearts; Clubs; Diamonds ] do
          for rank in [ Two; Three; Four; Five; Six; Seven; Eight; Nine; Ten; Jack; Queen; King; Ace ] do
              yield card rank suit ]

let private build jokers : Build =
    { Jokers = jokers
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
      Situation = { Jokers.Situation.empty with JokerSlots = 5 } }

let private jupiter cost = PlanetOffer(1, Flush, "Jupiter", cost)

let private aces = List.replicate 52 (card Ace Spades)

let private withSituation f (b: Build) = { b with Situation = f b.Situation }

let private plays round : Map<HandType, int * int> =
    [ FlushFive; FullHouse; Flush; Straight; ThreeOfAKind; TwoPair; Pair; HighCard ]
    |> List.map (fun t -> t, (0, round))
    |> Map.ofList

let private valueOf key (b: Build) =
    (b.Jokers |> List.find (fun j -> j.Key = key)).Value

let private entry gain lost : Evaluated =
    { Offer = jupiter 3
      Before = 100.0
      After = 100.0 * gain
      InterestLost = lost }

[<Fact>]
let ``round score is deterministic and rises with a joker`` () =
    let plain = roundScore 100 4 (build [])
    Assert.Equal(plain, roundScore 100 4 (build []))
    Assert.True(roundScore 100 4 (build [ joker "j_joker" ]) > plain)

[<Fact>]
let ``evaluate keeps affordable offers that do not lower the score`` () =
    let offers =
        [ JokerOffer(0, joker "j_jolly", 3)
          jupiter 3
          JokerOffer(2, joker "j_joker", 11) ]

    let evaluated = evaluate 100 4 10 (build []) offers
    Assert.Equal<int list>([ 0; 1 ], evaluated |> List.map (fun e -> position e.Offer) |> List.sort)
    Assert.All(evaluated, fun e -> Assert.True(e.After >= e.Before))

[<Fact>]
let ``evaluate returns no joker offer when the joker slots are full`` () =
    let full = build (List.replicate 5 (joker "j_joker"))
    let evaluated = evaluate 100 2 10 full [ JokerOffer(0, joker "j_jolly", 3); jupiter 3 ]
    Assert.Equal<int list>([ 1 ], evaluated |> List.map (fun e -> position e.Offer))

[<Fact>]
let ``interest lost is the interest the purchase costs`` () =
    let lost money cost =
        (evaluate 100 1 money (build []) [ jupiter cost ]).Head.InterestLost

    Assert.Equal(1, lost 26 3)
    Assert.Equal(0, lost 30 3)
    Assert.Equal(0, lost 4 3)

[<Fact>]
let ``choose needs the gain to clear the interest adjusted threshold`` () =
    Assert.Equal(Some(entry 1.30 0), choose [ entry 1.30 0 ])
    Assert.Equal(None, choose [ entry 1.06 1 ])
    Assert.Equal(None, choose [])

[<Fact>]
let ``best order puts the additive joker before the multiplier`` () =
    Assert.Equal<int list>([ 1; 0 ], bestOrder 100 4 (build [ joker "j_duo"; joker "j_jolly" ]))

[<Fact>]
let ``best order keeps the current order unless the gain is at least 2 percent`` () =
    let same = build [ joker "j_joker"; joker "j_joker" ]
    Assert.Equal<int list>([ 0; 1 ], bestOrder 100 4 same)

    let mixed = build [ joker "j_duo"; joker "j_jolly" ]
    Assert.Equal<int list>(bestOrder 100 4 mixed, bestOrder 100 4 mixed)

[<Fact>]
let ``revalue computes the build dependent jokers as the game does`` () =
    let other = { joker "j_joker" with SellValue = 3 }
    let one key = build [ { joker key with SellValue = 2; Value = Some 99.0 }; other ]
    let rich money = withSituation (fun s -> { s with Money = money }) (one "j_bull")

    Assert.Equal(Some 3.0, valueOf "j_swashbuckler" (revalue (one "j_swashbuckler")))
    Assert.Equal(Some 6.0, valueOf "j_abstract" (revalue (one "j_abstract")))
    // card.lua:4203-4206: card_limit - #jokers + number of Joker Stencils = 5 - 2 + 1
    Assert.Equal(Some 4.0, valueOf "j_stencil" (revalue (one "j_stencil")))
    Assert.Equal(Some 20.0, valueOf "j_bull" (revalue (rich 10)))
    Assert.Equal(Some 4.0, valueOf "j_bootstraps" (revalue (withSituation (fun s -> { s with Money = 13 }) (one "j_bootstraps"))))
    Assert.Equal(Some 99.0, valueOf "j_steel_joker" (revalue (one "j_steel_joker")))

[<Fact>]
let ``a bought joker and the money it costs change the build dependent values`` () =
    let bull = { joker "j_bull" with Value = Some 99.0 }
    let owned = withSituation (fun s -> { s with Money = 10 }) (build [ bull ])
    let offer = JokerOffer(0, joker "j_joker", 4)
    let evaluated = (evaluate 100 2 10 owned [ offer ]).Head

    let expected =
        withSituation (fun s -> { s with Money = 6 }) (build [ { bull with Value = Some 12.0 }; joker "j_joker" ])

    Assert.Equal(roundScore 100 2 expected, evaluated.After)
    Assert.Equal(roundScore 100 2 (build [ { bull with Value = Some 20.0 } ] |> withSituation (fun s -> { s with Money = 10 })), evaluated.Before)

[<Fact>]
let ``the simulated round spends the discards before it plays`` () =
    let discards = withSituation (fun s -> { s with DiscardsLeft = 4 })
    Assert.Equal(roundScore 100 4 (discards (build [])), roundScore 100 4 (discards (build [ joker "j_banner" ])))

[<Fact>]
let ``the simulated round keeps its own play counts`` () =
    let sharp = build [ joker "j_card_sharp" ] |> withSituation (fun s -> { s with HandPlays = plays 0 })
    let carried = sharp |> withSituation (fun s -> { s with HandPlays = plays 1 })
    let repeating = { sharp with Deck = aces }
    let none = { repeating with Jokers = [] }

    Assert.Equal(roundScore 100 2 sharp, roundScore 100 2 carried)
    Assert.Equal(roundScore 100 2 repeating, roundScore 100 2 { repeating with Situation = carried.Situation })
    // Flush Five of Aces: (160 + 55) chips x 16 mult; Card Sharp x3 from the second play on
    Assert.Equal(13760.0, roundScore 100 2 none)
    Assert.Equal(34400.0, roundScore 100 2 repeating)

    // Supernova adds the run count including the play being scored: 17 + 18 + 19 + 20
    let nova = { none with Jokers = [ joker "j_supernova" ] }
    Assert.Equal(15910.0, roundScore 100 2 nova)

[<Fact>]
let ``a joker offer needs a free slot whatever its edition`` () =
    let negative = { joker "j_jolly" with Edition = Some Negative }
    let offers = [ JokerOffer(0, negative, 3); JokerOffer(1, joker "j_jolly", 3) ]
    let full = build (List.replicate 5 (joker "j_joker"))
    Assert.Empty(evaluate 100 2 10 full offers)

    let free = build (List.replicate 4 (joker "j_joker"))
    Assert.Equal<int list>([ 0; 1 ], evaluate 100 2 10 free offers |> List.map (fun e -> position e.Offer) |> List.sort)

[<Fact>]
let ``a joker stencil is valued by the empty slots the build has`` () =
    let stencil = { joker "j_stencil" with Value = Some 99.0 }
    let four = List.replicate 4 (joker "j_joker")
    Assert.Equal(Some 0.0, valueOf "j_stencil" (revalue (build (stencil :: four))))

    let negative = { joker "j_jolly" with Edition = Some Negative }
    let owned = build (stencil :: List.replicate 3 (joker "j_joker"))
    let evaluated = (evaluate 100 2 10 owned [ JokerOffer(0, negative, 3) ]).Head

    // 6 slots - 5 jokers + 1 Stencil
    let expected =
        { build ({ stencil with Value = Some 2.0 } :: List.replicate 3 (joker "j_joker") @ [ negative ]) with
            Situation = { Jokers.Situation.empty with JokerSlots = 6; Money = -3 } }

    Assert.Equal(roundScore 100 2 expected, evaluated.After)

[<Fact>]
let ``policy never buys the tarot in the fixture shop`` () =
    let text = File.ReadAllText(AppContext.BaseDirectory + "fixtures/gamestate.SHOP.json")

    let g =
        match Api.decodeGameState (JsonDocument.Parse(text).RootElement) with
        | Ok g -> g
        | Error e -> failwith e

    let action, decision = Policy.decide 1 g
    Assert.True(action = Policy.BuyCard 1 || action = Policy.NextRound)
    Assert.Equal(None, decision.Predicted)

    let full = { g with Consumables = { g.Consumables with Count = g.Consumables.Limit } }
    Assert.Equal(Policy.NextRound, fst (Policy.decide 1 full))

    let jupiter = g.Shop.Cards[1]

    let earth =
        { jupiter with
            Key = "c_earth"
            Label = "Earth"
            Cost = { jupiter.Cost with Buy = 1 } }

    let rich = { g with Money = 26; Shop = { g.Shop with Cards = [ jupiter; earth ] } }
    let action, decision = Policy.decide 2 rich
    Assert.Equal(Policy.BuyCard 1, action)
    Assert.Equal("buy Earth for $1", decision.Chosen)
    Assert.Equal(decision.Chosen, decision.Candidates.Head.Action)

[<Fact>]
let ``policy passes the shop with full joker slots and says so`` () =
    let text = File.ReadAllText(AppContext.BaseDirectory + "fixtures/gamestate.SHOP.json")

    let g =
        match Api.decodeGameState (JsonDocument.Parse(text).RootElement) with
        | Ok g -> g
        | Error e -> failwith e

    let plain = { g.Shop.Cards[1] with Set = Api.CardSet.Joker; Key = "j_joker"; Label = "Joker" }
    let owned = [ for i in 1..5 -> { plain with Id = 100 + i } ]

    let full =
        { g with
            Money = 20
            Jokers = { g.Jokers with Count = 5; Cards = owned }
            Shop = { g.Shop with Cards = [ plain ] } }

    match Policy.decide 1 full with
    | Policy.NextRound, decision -> Assert.Equal("joker slots are full", decision.Rule)
    | Policy.ReorderJokers _, _ -> ()
    | other, _ -> failwithf "unexpected %A" other

let private upgrade gain : Upgrade =
    { Sell = 0
      Offer = JokerOffer(0, joker "j_duo", 8)
      Before = 100.0
      After = 100.0 * gain
      Net = 6
      Threshold = 1.05 }

let private load name =
    let text = File.ReadAllText(AppContext.BaseDirectory + "fixtures/gamestate." + name + ".json")

    match Api.decodeGameState (JsonDocument.Parse(text).RootElement) with
    | Ok g -> g
    | Error e -> failwith e

let private tarotShop (g: Api.GameState) money =
    { g with
        Money = money
        Shop = { g.Shop with Cards = [ g.Shop.Cards[0] ] } }

let private withoutPacks (g: Api.GameState) =
    { g with Packs = { g.Packs with Count = 0; Cards = [] } }

[<Fact>]
let ``upgrades need full joker slots and a net cost within the money`` () =
    let owned = { joker "j_joker" with SellValue = 2 }
    let offers = [ JokerOffer(0, joker "j_duo", 8) ]
    Assert.Empty(upgrades 100 2 10 (build (List.replicate 4 owned)) offers)

    let full = build (List.replicate 5 owned)
    let found = upgrades 100 2 10 full offers
    Assert.Equal(5, found.Length)
    Assert.All(found, fun u -> Assert.Equal(6, u.Net))
    Assert.Empty(upgrades 100 2 5 full offers)

[<Fact>]
let ``choose upgrade needs a gain of 15 percent`` () =
    Assert.Equal(Some(upgrade 1.20), chooseUpgrade [ upgrade 1.20 ])
    Assert.Equal(None, chooseUpgrade [ upgrade 1.10 ])
    Assert.Equal(None, chooseUpgrade [])

[<Fact>]
let ``choose upgrade also needs the gain to clear the buy threshold after the sale`` () =
    let dear gain = { upgrade gain with Threshold = 1.20 }
    Assert.Equal(None, chooseUpgrade [ dear 1.19 ])
    Assert.Equal(Some(dear 1.21), chooseUpgrade [ dear 1.21 ])

[<Fact>]
let ``an upgrade carries the buy threshold of the money held after the sale`` () =
    let owned = List.replicate 4 (joker "j_joker") @ [ { joker "j_joker" with SellValue = 3 } ]
    let found = upgrades 100 2 12 (build owned) [ JokerOffer(0, joker "j_duo", 11) ]
    Assert.Equal(1.20, (found |> List.find (fun u -> u.Sell = 4)).Threshold, 9)

[<Fact>]
let ``a negative joker is never sold for an upgrade`` () =
    let negative = { joker "j_joker" with Edition = Some Negative; SellValue = 2 }
    let six = withSituation (fun s -> { s with JokerSlots = 6 }) (build (List.replicate 5 { joker "j_joker" with SellValue = 2 } @ [ negative ]))
    let found = upgrades 100 2 10 six [ JokerOffer(0, joker "j_duo", 8) ]
    Assert.Equal(5, found.Length)
    Assert.DoesNotContain(found, fun u -> u.Sell = 5)

[<Fact>]
let ``pick from pack takes the planet that raises the score most`` () =
    let ranks = [ Two; Three; Four; Five; Six; Seven; Eight; Nine; Ten; Jack; Queen; King; Ace ]
    let flushes = { build [] with Deck = [ for rank in ranks -> card rank Spades ] }
    let pack = [ PlanetOffer(0, Pair, "Mercury", 0); PlanetOffer(1, Flush, "Jupiter", 0) ]
    Assert.Equal(1, position (pickFromPack 100 4 flushes pack).Value.Offer)

[<Fact>]
let ``pick from pack skips a joker with full slots and cards that change nothing`` () =
    let full = build (List.replicate 5 (joker "j_joker"))
    Assert.Equal(None, pickFromPack 100 2 full [ JokerOffer(0, joker "j_duo", 0) ])

    let repeating = { build [] with Deck = aces }
    Assert.Equal(None, pickFromPack 100 2 repeating [ PlanetOffer(0, Pair, "Mercury", 0) ])

[<Fact>]
let ``the shop seed is fixed within a visit and differs between rounds`` () =
    let g = load "SHOP"
    Assert.Equal(10100, Policy.shopSeed g)
    Assert.NotEqual(Policy.shopSeed g, Policy.shopSeed { g with RoundNum = 2 })
    Assert.NotEqual(Policy.shopSeed g, Policy.shopSeed { g with AnteNum = 2 })

    let rule seq = (snd (Policy.decide seq (tarotShop g 40))).Rule
    Assert.Equal(rule 1, rule 77)

[<Fact>]
let ``policy buys the cheapest pack that keeps the interest`` () =
    let g = load "SHOP"
    let action, decision = Policy.decide 1 (tarotShop g 40)
    Assert.Equal(Policy.BuyPack 0, action)
    Assert.Equal("buy Buffoon Pack for $4", decision.Chosen)

    Assert.Equal(Policy.NextRound, fst (Policy.decide 1 (tarotShop g 28)))

[<Fact>]
let ``policy rerolls only above the interest cap and at a low price`` () =
    let g = load "SHOP"
    let action, decision = Policy.decide 1 (tarotShop (withoutPacks g) 31)
    Assert.Equal(Policy.Reroll, action)
    Assert.Contains("$26", decision.Rule)

    let dear = tarotShop (withoutPacks g) 40
    Assert.Equal(Policy.NextRound, fst (Policy.decide 1 { dear with Round = { dear.Round with RerollCost = 8 } }))

[<Fact>]
let ``policy picks from an opened pack and names both round scores`` () =
    match Policy.decide 1 (load "SMODS_BOOSTER_OPENED") with
    | Policy.PickPack index, decision ->
        Assert.InRange(index, 0, 1)
        Assert.Matches(@"from \d+ to \d+", decision.Rule)
    | Policy.SkipPack, _ -> ()
    | other, _ -> failwithf "unexpected %A" other

[<Fact>]
let ``policy never sells an eternal joker for an upgrade`` () =
    let g = load "SHOP"
    let plain = { g.Shop.Cards[1] with Set = Api.CardSet.Joker; Key = "j_joker"; Label = "Joker" }
    let idle = { plain with Key = "j_credit_card"; Label = "Credit Card" }
    let duo = { plain with Key = "j_duo"; Label = "Duo"; Cost = { plain.Cost with Buy = 5 } }

    let state (weakest: Api.Card) =
        { g with
            Money = 30
            Jokers = { g.Jokers with Count = 5; Cards = weakest :: [ for i in 1..4 -> { plain with Id = 100 + i } ] }
            Shop = { g.Shop with Cards = [ duo ] } }

    Assert.Equal(Policy.SellJoker 0, fst (Policy.decide 1 (state idle)))

    let eternal = { idle with Modifier = { idle.Modifier with Eternal = true } }
    Assert.NotEqual(Policy.SellJoker 0, fst (Policy.decide 1 (state eternal)))

[<Fact>]
let ``policy falls back to a celestial pack when no buffoon pack is affordable`` () =
    let g = load "SHOP"
    let celestialOnly = { g with Packs = { g.Packs with Count = 1; Cards = [ g.Packs.Cards[1] ] } }
    let action, decision = Policy.decide 1 (tarotShop celestialOnly 40)
    Assert.Equal(Policy.BuyPack 0, action)
    Assert.Equal("buy Jumbo Celestial Pack for $6", decision.Chosen)

[<Fact>]
let ``policy skips a pack whose first card is a spectral card`` () =
    let g = load "SMODS_BOOSTER_OPENED"
    let spectral = { g.Pack.Cards[0] with Set = Api.CardSet.Spectral }
    let action, decision = Policy.decide 1 { g with Pack = { g.Pack with Cards = spectral :: g.Pack.Cards.Tail } }
    Assert.Equal(Policy.SkipPack, action)
    Assert.Contains("cannot handle safely", decision.Rule)

let private jokerCard key label =
    let shop = load "SHOP"
    { shop.Shop.Cards[1] with Set = Api.CardSet.Joker; Key = key; Label = label }

let private eternal (card: Api.Card) =
    { card with Modifier = { card.Modifier with Eternal = true } }

let private verdantLeaf debuffed (jokers: Api.Card list) =
    let g = load "SELECTING_HAND"
    let blinds = g.Blinds.Value

    { g with
        Blinds =
            Some
                { blinds with
                    Small = { blinds.Small with Status = Api.BlindStatus.Defeated }
                    Boss = { blinds.Boss with Name = "Verdant Leaf"; Status = Api.BlindStatus.Current } }
        Hand =
            { g.Hand with
                Cards = g.Hand.Cards |> List.map (fun c -> { c with State = { c.State with Debuff = debuffed } }) }
        Jokers = { g.Jokers with Count = jokers.Length; Cards = jokers } }

[<Fact>]
let ``policy sells the joker that costs least to lift Verdant Leaf`` () =
    let plain = jokerCard "j_joker" "Joker"
    let idle = jokerCard "j_credit_card" "Credit Card"

    let action, decision = Policy.decide 1 (verdantLeaf true [ plain; idle ])
    Assert.Equal(Policy.SellJoker 1, action)
    Assert.Equal("sell Credit Card to lift Verdant Leaf", decision.Chosen)
    Assert.Contains("all cards until a joker is sold", decision.Rule)
    Assert.Matches(@"round score at \d+", decision.Rule)

    Assert.Equal(Policy.SellJoker 0, fst (Policy.decide 1 (verdantLeaf true [ plain; eternal idle ])))

[<Fact>]
let ``policy plays on under Verdant Leaf when no joker may be sold or the cards are not debuffed`` () =
    let plain = jokerCard "j_joker" "Joker"
    let idle = jokerCard "j_credit_card" "Credit Card"

    let isSale action =
        match action with
        | Policy.SellJoker _ -> true
        | _ -> false

    Assert.False(isSale (fst (Policy.decide 1 (verdantLeaf true [ eternal plain; eternal idle ]))))
    Assert.False(isSale (fst (Policy.decide 1 (verdantLeaf false [ plain; idle ]))))

[<Fact>]
let ``a sticker adds its dollars per round to the buy threshold`` () =
    let plain = jokerCard "j_joker" "Joker"
    let rental = { plain with Modifier = { plain.Modifier with Rental = true } }
    let perishable = { plain with Modifier = { plain.Modifier with Perishable = Some 5 } }

    let bought card gain =
        (choose [ { entry gain 0 with InterestLost = Policy.stickerLoss card } ]).IsSome

    Assert.False(bought rental 1.15)
    Assert.True(bought rental 1.25)
    Assert.False(bought perishable 1.12)
    Assert.True(bought perishable 1.20)
    Assert.True(bought plain 1.06)

[<Fact>]
let ``policy names the sticker of the joker it buys`` () =
    let g = load "SHOP"
    let duo = jokerCard "j_duo" "Duo"
    let rental = { duo with Cost = { duo.Cost with Buy = 1 }; Modifier = { duo.Modifier with Rental = true } }
    let action, decision = Policy.decide 1 { g with Money = 10; Shop = { g.Shop with Cards = [ rental ] } }
    Assert.Equal(Policy.BuyCard 0, action)
    Assert.Contains("Rental sticker adds 15%", decision.Rule)

[<Fact>]
let ``interest no longer matters in the last ante`` () =
    let g = load "SHOP"
    let atAnte ante money = tarotShop { g with AnteNum = ante } money

    let action, decision = Policy.decide 1 (atAnte 8 10)
    Assert.Equal(Policy.BuyPack 0, action)
    Assert.Contains("last ante", decision.Rule)
    Assert.NotEqual(Policy.BuyPack 0, fst (Policy.decide 1 (atAnte 7 10)))

    let action, decision = Policy.decide 1 (withoutPacks (atAnte 8 6))
    Assert.Equal(Policy.Reroll, action)
    Assert.Contains("last ante", decision.Rule)

[<Fact>]
let ``the runner waits while every hand card is still debuffed`` () =
    let all = verdantLeaf true []
    Assert.True(Runner.allHandDebuffed all)

    let one =
        { all with
            Hand =
                { all.Hand with
                    Cards =
                        { all.Hand.Cards.Head with State = { all.Hand.Cards.Head.State with Debuff = false } }
                        :: all.Hand.Cards.Tail } }

    Assert.False(Runner.allHandDebuffed one)

// live play gives these jokers no value: only the text carries it
let private decaying key text : Jokers.Joker = { joker key with Text = text }

let private iceCream value =
    decaying "j_ice_cream" $"+%g{value} Chips -5 Chips for every hand played"

let private popcorn value =
    decaying "j_popcorn" $"+%g{value} Mult -4 Mult per round played"

let private ramen value =
    decaying "j_ramen" $"X%g{value} Mult, loses X0.01 Mult per card discarded"

[<Fact>]
let ``decay lowers Ice Cream, Popcorn and Ramen by what the next rounds take`` () =
    let aged = decay (build [ iceCream 100.0; popcorn 20.0; ramen 2.0; joker "j_joker" ]) 2

    Assert.StartsWith("+60 Chips", aged.Jokers[0].Text)
    Assert.StartsWith("+12 Mult", aged.Jokers[1].Text)
    Assert.StartsWith("X1.84 Mult", aged.Jokers[2].Text)
    Assert.Equal("", aged.Jokers[3].Text)

[<Fact>]
let ``decay stops Ice Cream at zero`` () =
    let aged = decay (build [ iceCream 30.0 ]) 2
    Assert.StartsWith("+0 Chips", aged.Jokers.Head.Text)

[<Fact>]
let ``a Popcorn offer is worth less than plain mult when it is about to run out`` () =
    let gain offered =
        let e = evaluate 100 8 10 (build []) [ JokerOffer(0, offered, 5) ] |> List.exactlyOne
        e.After / e.Before

    let plain = { joker "j_joker" with Value = Some 4.0 }
    Assert.True(gain (popcorn 6.0) < gain plain)
    Assert.True(gain (popcorn 20.0) > gain plain)

[<Fact>]
let ``a build without decaying jokers is scored as one round`` () =
    let owned = build [ joker "j_joker" ]
    let e = evaluate 100 8 10 owned [ jupiter 3 ] |> List.exactlyOne
    Assert.Equal(roundScore 100 8 (revalue owned), e.Before)
