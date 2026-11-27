module Balatro.Tests.SimContentTests

open System
open Xunit
open Balatro.Core
open Balatro.Bot
open Balatro.Bot.SimContent
open Balatro.Tests.HandsTests

let private view =
    { Owned = Set.empty
      Deck = []
      Played = Set.empty
      GrosMichelExtinct = false }

let private draws = 20000

let private share (hits: int) (total: int) = 100.0 * float hits / float total

let private near (expected: float) (tolerance: float) (actual: float) =
    Assert.InRange(actual, expected - tolerance, expected + tolerance)

let private planetKeys =
    [ "c_mercury"
      "c_venus"
      "c_earth"
      "c_mars"
      "c_jupiter"
      "c_saturn"
      "c_uranus"
      "c_neptune"
      "c_pluto"
      "c_planet_x"
      "c_ceres"
      "c_eris" ]

let private rareHands = [ "c_planet_x"; "c_ceres"; "c_eris" ]

let private shop seed =
    let rng = Random(seed: int)
    List.init draws (fun _ -> shopCards rng view) |> List.concat

[<Fact>]
let ``shop slots are jokers, tarots and planets at 20 4 4`` () =
    let cards = shop 1
    let kind set = cards |> List.filter (fun c -> c.Set = set) |> List.length
    near 71.4 1.5 (share (kind Api.CardSet.Joker) cards.Length)
    near 14.3 1.5 (share (kind Api.CardSet.Tarot) cards.Length)
    near 14.3 1.5 (share (kind Api.CardSet.Planet) cards.Length)

let private shopJokers =
    shop 2 |> List.filter (fun c -> c.Set = Api.CardSet.Joker)

[<Fact>]
let ``shop jokers are 70 percent common, 25 uncommon, 5 rare`` () =
    let rarity (c: Api.Card) =
        (jokers |> List.find (fun j -> j.Key = c.Key)).Rarity

    let count r =
        shopJokers |> List.filter (fun c -> rarity c = r) |> List.length

    near 70.0 1.5 (share (count Common) shopJokers.Length)
    near 25.0 1.5 (share (count Uncommon) shopJokers.Length)
    near 5.0 1.5 (share (count Rare) shopJokers.Length)
    Assert.Equal(0, count Legendary)

[<Fact>]
let ``joker editions follow the game's odds`` () =
    let rng = Random 3
    let drawn =
        List.init 100000 (fun _ -> (packContents rng view "p_buffoon_normal_1") |> List.map (fun c -> c.Modifier.Edition))
        |> List.concat

    let count e = drawn |> List.filter ((=) e) |> List.length
    near 2.0 0.15 (share (count (Some Api.Edition.Foil)) drawn.Length)
    near 1.4 0.15 (share (count (Some Api.Edition.Holo)) drawn.Length)
    near 0.3 0.15 (share (count (Some Api.Edition.Polychrome)) drawn.Length)
    near 0.3 0.15 (share (count (Some Api.Edition.Negative)) drawn.Length)

let private enhancedDeck enhancement =
    [ withEnhancement enhancement (card Domain.Rank.Two Domain.Suit.Hearts) ]

let private keys v = pool v |> List.map (fun j -> j.Key) |> Set.ofList

[<Fact>]
let ``the pool drops locked jokers and legendaries`` () =
    let locked =
        [ "j_ticket"; "j_certificate"; "j_throwback"; "j_rough_gem"; "j_bloodstone"; "j_arrowhead"
          "j_onyx_agate"; "j_glass"; "j_wee"; "j_merry_andy"; "j_idol"; "j_seeing_double"
          "j_hit_the_road"; "j_duo"; "j_trio"; "j_order"; "j_tribe"; "j_stuntman"; "j_invisible"
          "j_brainstorm"; "j_satellite"; "j_shoot_the_moon"; "j_astronomer"; "j_bootstraps"
          "j_caino"; "j_chicot"; "j_perkeo"; "j_triboulet"; "j_yorick" ]

    let open' = keys view
    Assert.Empty(locked |> List.filter open'.Contains)
    Assert.Equal(117, open'.Count)

[<Fact>]
let ``the pool gates Steel, Stone and Lucky Cat on the deck`` () =
    let gated = [ "j_steel_joker", Domain.Enhancement.Steel; "j_stone", Domain.Enhancement.Stone; "j_lucky_cat", Domain.Enhancement.Lucky ]

    for key, enhancement in gated do
        Assert.DoesNotContain(key, keys view)
        Assert.Contains(key, keys { view with Deck = enhancedDeck enhancement })

    Assert.DoesNotContain("j_steel_joker", keys { view with Deck = enhancedDeck Domain.Enhancement.Stone })

[<Fact>]
let ``the pool swaps Gros Michel for Cavendish once extinct`` () =
    Assert.Contains("j_gros_michel", keys view)
    Assert.DoesNotContain("j_cavendish", keys view)
    let extinct = keys { view with GrosMichelExtinct = true }
    Assert.DoesNotContain("j_gros_michel", extinct)
    Assert.Contains("j_cavendish", extinct)

[<Fact>]
let ``no owned key is offered and nothing repeats within a call`` () =
    let rng = Random 4
    let owned = set [ "j_joker"; "c_mercury"; "c_pluto"; "c_fool"; "j_greedy_joker"; "c_soul" ]
    let v = { view with Owned = owned }

    let nonStandard =
        [ "p_arcana_mega_1"; "p_celestial_jumbo_1"; "p_spectral_jumbo_1"; "p_buffoon_mega_1"; "p_celestial_normal_1" ]

    for _ in 1..2000 do
        let batches = shopCards rng v :: (nonStandard |> List.map (packContents rng v))

        for cards in batches do
            let ks = cards |> List.map (fun c -> c.Key)
            Assert.Equal(ks.Length, (List.distinct ks).Length)
            Assert.Empty(ks |> List.filter owned.Contains)

[<Fact>]
let ``costs add the edition surcharge and sell for half`` () =
    let info = { (List.head jokers) with Cost = 5 }

    for edition, buy, sell in
        [ None, 5, 2
          Some Api.Edition.Foil, 7, 3
          Some Api.Edition.Holo, 8, 4
          Some Api.Edition.Polychrome, 10, 5
          Some Api.Edition.Negative, 10, 5 ] do
        let c = jokerCard info edition
        Assert.Equal(buy, c.Cost.Buy)
        Assert.Equal(sell, c.Cost.Sell)

[<Fact>]
let ``consumables and vouchers carry the base prices`` () =
    Assert.Equal(3, (consumableCard "c_hermit").Cost.Buy)
    Assert.Equal(3, (consumableCard "c_neptune").Cost.Buy)
    Assert.Equal(4, (consumableCard "c_ankh").Cost.Buy)
    Assert.Equal(10, (voucher (Random 5) Set.empty).Cost.Buy)
    Assert.Equal(3, (jokers |> List.find (fun j -> j.Key = "j_jolly")).Cost)

[<Fact>]
let ``the first shop opens with a normal Buffoon pack`` () =
    let rng = Random 6

    for _ in 1..500 do
        let cards = packs rng true
        Assert.Equal(2, cards.Length)
        Assert.StartsWith("p_buffoon_normal_", cards.Head.Key)

[<Fact>]
let ``pack kinds follow the weights`` () =
    let rng = Random 7
    let both = List.init draws (fun _ -> packs rng false)
    let drawn = both |> List.map List.head |> List.map (fun c -> c.Key)
    let second = both |> List.map (fun cs -> cs[1].Key)
    let kind (k: string) = k.Split('_')[1]
    let count k = drawn |> List.filter (fun key -> kind key = k) |> List.length

    for k, expected in [ "arcana", 29.0; "celestial", 29.0; "standard", 29.0; "buffoon", 8.7; "spectral", 4.3 ] do
        near expected 1.5 (share (count k) draws)
        near expected 1.5 (share (second |> List.filter (fun key -> kind key = k) |> List.length) draws)

    let size (c: string) = drawn |> List.filter (fun key -> key.Contains c) |> List.length
    near 61.5 1.5 (share (size "_normal_") draws)
    near 30.7 1.5 (share (size "_jumbo_") draws)
    near 7.7 1.5 (share (size "_mega_") draws)

[<Fact>]
let ``pack contents and picks match the table`` () =
    let rng = Random 8

    for key, count, choose in
        [ "p_arcana_normal_1", 3, 1
          "p_arcana_jumbo_2", 5, 1
          "p_arcana_mega_1", 5, 2
          "p_celestial_normal_3", 3, 1
          "p_celestial_jumbo_1", 5, 1
          "p_celestial_mega_2", 5, 2
          "p_standard_normal_4", 3, 1
          "p_standard_jumbo_1", 5, 1
          "p_standard_mega_1", 5, 2
          "p_buffoon_normal_2", 2, 1
          "p_buffoon_jumbo_1", 4, 1
          "p_buffoon_mega_1", 4, 2
          "p_spectral_normal_1", 2, 1
          "p_spectral_jumbo_1", 4, 1
          "p_spectral_mega_1", 4, 2 ] do
        Assert.Equal(count, (packContents rng view key).Length)
        Assert.Equal(choose, packChoose key)

[<Fact>]
let ``pack cards have the Set and prices of their kind`` () =
    let rng = Random 9
    let sets = packs rng false |> List.map (fun c -> c.Set)
    Assert.All(sets, fun s -> Assert.Equal(Api.CardSet.Booster, s))

    for key, price, label in
        [ "p_celestial_jumbo_1", 6, "Jumbo Celestial Pack"; "p_buffoon_normal_1", 4, "Buffoon Pack"; "p_standard_mega_2", 8, "Mega Standard Pack" ] do
        let found =
            Seq.initInfinite (fun _ -> packs rng false)
            |> Seq.take 5000
            |> Seq.concat
            |> Seq.tryFind (fun c -> c.Key = key)

        Assert.Equal(Some(price, label), found |> Option.map (fun c -> c.Cost.Buy, c.Label))

[<Fact>]
let ``celestial packs hold planets and gate the rare hands on play`` () =
    let rng = Random 10

    let contents v =
        List.init 3000 (fun _ -> packContents rng v "p_celestial_mega_1") |> List.concat

    let all = contents view

    Assert.All(
        all,
        fun c ->
            if c.Key <> "c_black_hole" then
                Assert.Equal(Api.CardSet.Planet, c.Set)
                Assert.Contains(c.Key, planetKeys)
    )

    Assert.Empty(all |> List.filter (fun c -> List.contains c.Key rareHands))

    let seen = { view with Played = set [ Domain.FiveOfAKind; Domain.FlushHouse; Domain.FlushFive ] }
    let keysSeen = contents seen |> List.map (fun c -> c.Key) |> Set.ofList
    Assert.True(rareHands |> List.forall keysSeen.Contains)

[<Fact>]
let ``bot-facing keys and labels have the expected forms`` () =
    let rng = Random 11
    Assert.Equal(12, planetKeys.Length)

    for key in planetKeys do
        let c = consumableCard key
        Assert.Equal(Api.CardSet.Planet, c.Set)

    let info = jokers |> List.find (fun j -> j.Key = "j_green_joker")
    let mapped = Mapping.joker (jokerCard info None)
    Assert.Equal(info.Key, mapped.Key)
    Assert.Equal(info.Label, mapped.Name)
    Assert.Equal(Some 0.0, mapped.Value)

    let standard = List.init 300 (fun _ -> packContents rng view "p_standard_mega_1") |> List.concat
    Assert.All(standard, fun c -> Assert.True((Mapping.toCard c).IsSome))
    Assert.All(standard, fun c -> Assert.Equal("Base Card", c.Label))
    Assert.Equal("Voucher", (voucher rng Set.empty).Set.ToString())

[<Fact>]
let ``standard cards use suit and rank letters and the enhancement odds`` () =
    let rng = Random 12
    let cards = List.init 6000 (fun _ -> packContents rng view "p_standard_jumbo_1") |> List.concat
    Assert.All(cards, fun c -> Assert.Matches("^[HDCS]_[2-9TJQKA]$", c.Key))
    let enhanced = cards |> List.filter (fun c -> c.Modifier.Enhancement.IsSome)
    near 40.0 1.5 (share enhanced.Length cards.Length)
    Assert.All(enhanced, fun c -> Assert.Equal(Api.CardSet.Enhanced, c.Set))
    let sealed' = cards |> List.filter (fun c -> c.Modifier.Seal.IsSome)
    near 20.0 1.5 (share sealed'.Length cards.Length)
    let foil = cards |> List.filter (fun c -> c.Modifier.Edition = Some Api.Edition.Foil)
    near 4.0 1.5 (share foil.Length cards.Length)

[<Fact>]
let ``vouchers skip the redeemed ones`` () =
    let rng = Random 13
    let redeemed = set [ "v_overstock_norm"; "v_hone" ]

    for _ in 1..500 do
        let v = voucher rng redeemed
        Assert.StartsWith("v_", v.Key)
        Assert.DoesNotContain(v.Key, redeemed)
        Assert.Equal(Api.CardSet.Voucher, v.Set)

[<Fact>]
let ``rarity counts match the game's table`` () =
    let count r = jokers |> List.filter (fun j -> j.Rarity = r) |> List.length
    Assert.Equal(150, jokers.Length)
    Assert.Equal(61, count Common)
    Assert.Equal(64, count Uncommon)
    Assert.Equal(20, count Rare)
    Assert.Equal(5, count Legendary)
