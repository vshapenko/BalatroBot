module Balatro.Bot.SimContent

open System
open System.Reflection
open System.Text.Json
open Balatro.Core

type Rarity =
    | Common
    | Uncommon
    | Rare
    | Legendary

type JokerInfo =
    { Key: string
      Label: string
      Rarity: Rarity
      Cost: int
      Effect: string }

type View =
    { Owned: Set<string>
      Deck: Domain.Card list
      Played: Set<Domain.HandType>
      GrosMichelExtinct: bool }

let private rares =
    set
        [ "j_dna"
          "j_vagabond"
          "j_baron"
          "j_obelisk"
          "j_baseball"
          "j_ancient"
          "j_campfire"
          "j_blueprint"
          "j_wee"
          "j_hit_the_road"
          "j_duo"
          "j_trio"
          "j_family"
          "j_order"
          "j_tribe"
          "j_stuntman"
          "j_invisible"
          "j_brainstorm"
          "j_drivers_license"
          "j_burnt" ]

let private legendaries =
    set [ "j_caino"; "j_triboulet"; "j_yorick"; "j_chicot"; "j_perkeo" ]

let private locked =
    set
        [ "j_ticket"
          "j_certificate"
          "j_throwback"
          "j_rough_gem"
          "j_bloodstone"
          "j_arrowhead"
          "j_onyx_agate"
          "j_glass"
          "j_wee"
          "j_merry_andy"
          "j_idol"
          "j_seeing_double"
          "j_hit_the_road"
          "j_duo"
          "j_trio"
          "j_order"
          "j_tribe"
          "j_stuntman"
          "j_invisible"
          "j_brainstorm"
          "j_satellite"
          "j_shoot_the_moon"
          "j_astronomer"
          "j_bootstraps" ]

let private rarityOf key =
    if legendaries.Contains key then Legendary
    elif rares.Contains key then Rare
    elif JokerRegistry.uncommon.Contains key then Uncommon
    else Common

let private fixtureEditionCosts =
    Map [ "j_burglar", 6; "j_glass", 6; "j_jolly", 3; "j_order", 8; "j_supernova", 5; "j_swashbuckler", 4 ]

let private load () =
    use stream = Assembly.GetExecutingAssembly().GetManifestResourceStream "jokers.json"
    use doc = JsonDocument.Parse stream

    [ for row in doc.RootElement.EnumerateArray() ->
          let key = row.GetProperty("key").GetString()

          { Key = key
            Label = row.GetProperty("label").GetString()
            Rarity = rarityOf key
            Cost =
              fixtureEditionCosts
              |> Map.tryFind key
              |> Option.defaultWith (fun () -> row.GetProperty("buy").GetInt32())
            Effect = row.GetProperty("effect").GetString() } ]

let jokers: JokerInfo list = load ()

let private tarots =
    [ "c_fool", "The Fool"
      "c_magician", "The Magician"
      "c_high_priestess", "The High Priestess"
      "c_empress", "The Empress"
      "c_emperor", "The Emperor"
      "c_heirophant", "The Hierophant"
      "c_lovers", "The Lovers"
      "c_chariot", "The Chariot"
      "c_justice", "Justice"
      "c_hermit", "The Hermit"
      "c_wheel_of_fortune", "The Wheel of Fortune"
      "c_strength", "Strength"
      "c_hanged_man", "The Hanged Man"
      "c_death", "Death"
      "c_temperance", "Temperance"
      "c_devil", "The Devil"
      "c_tower", "The Tower"
      "c_star", "The Star"
      "c_moon", "The Moon"
      "c_sun", "The Sun"
      "c_judgement", "Judgement"
      "c_world", "The World" ]

let private planets =
    [ "c_mercury", "Mercury", None
      "c_venus", "Venus", None
      "c_earth", "Earth", None
      "c_mars", "Mars", None
      "c_jupiter", "Jupiter", None
      "c_saturn", "Saturn", None
      "c_uranus", "Uranus", None
      "c_neptune", "Neptune", None
      "c_pluto", "Pluto", None
      "c_planet_x", "Planet X", Some Domain.FiveOfAKind
      "c_ceres", "Ceres", Some Domain.FlushHouse
      "c_eris", "Eris", Some Domain.FlushFive ]

let private spectrals =
    [ "c_familiar", "Familiar"
      "c_grim", "Grim"
      "c_incantation", "Incantation"
      "c_talisman", "Talisman"
      "c_aura", "Aura"
      "c_wraith", "Wraith"
      "c_sigil", "Sigil"
      "c_ouija", "Ouija"
      "c_ectoplasm", "Ectoplasm"
      "c_immolate", "Immolate"
      "c_ankh", "Ankh"
      "c_deja_vu", "Deja Vu"
      "c_hex", "Hex"
      "c_trance", "Trance"
      "c_medium", "Medium"
      "c_cryptid", "Cryptid" ]

let private soul = "c_soul", "The Soul"
let private blackHole = "c_black_hole", "Black Hole"

let private vouchers =
    [ "v_overstock_norm", "Overstock"
      "v_clearance_sale", "Clearance Sale"
      "v_hone", "Hone"
      "v_reroll_surplus", "Reroll Surplus"
      "v_crystal_ball", "Crystal Ball"
      "v_telescope", "Telescope"
      "v_grabber", "Grabber"
      "v_wasteful", "Wasteful"
      "v_tarot_merchant", "Tarot Merchant"
      "v_planet_merchant", "Planet Merchant"
      "v_seed_money", "Seed Money"
      "v_blank", "Blank"
      "v_magic_trick", "Magic Trick"
      "v_hieroglyph", "Hieroglyph"
      "v_directors_cut", "Director's Cut"
      "v_paint_brush", "Paint Brush" ]

let private surcharge edition =
    match edition with
    | Some Api.Edition.Foil -> 2
    | Some Api.Edition.Holo -> 3
    | Some Api.Edition.Polychrome
    | Some Api.Edition.Negative -> 5
    | None -> 0

let private build key set label effect edition suit rank enhancement seal base' : Api.Card =
    let buy = max 1 (base' + surcharge edition)

    { Id = 0
      Key = key
      Set = set
      Label = label
      Value =
        { Suit = suit
          Rank = rank
          Effect = effect }
      Modifier =
        { Eternal = false
          Rental = false
          Perishable = None
          Seal = seal
          Edition = edition
          Enhancement = enhancement }
      State =
        { Hidden = false
          Debuff = false
          Highlight = false }
      Cost = { Buy = buy; Sell = max 1 (buy / 2) } }

let private simple key set label cost =
    build key set label "" None None None None None cost

let jokerCard (info: JokerInfo) (edition: Api.Edition option) : Api.Card =
    build info.Key Api.CardSet.Joker info.Label info.Effect edition None None None None info.Cost

let consumableCard (key: string) : Api.Card =
    match
        tarots |> List.tryFind (fst >> (=) key),
        planets |> List.tryFind (fun (k, _, _) -> k = key),
        soul :: blackHole :: spectrals |> List.tryFind (fst >> (=) key)
    with
    | Some(_, label), _, _ -> simple key Api.CardSet.Tarot label 3
    | _, Some(_, label, _), _ -> simple key Api.CardSet.Planet label 3
    | _, _, Some(_, label) -> simple key Api.CardSet.Spectral label 4
    | _ -> failwithf "unknown consumable %s" key

let pool (view: View) : JokerInfo list =
    let holds enhancement =
        view.Deck |> List.exists (fun c -> c.Enhancement = Some enhancement)

    jokers
    |> List.filter (fun j ->
        j.Rarity <> Legendary
        && not (locked.Contains j.Key)
        && not (view.Owned.Contains j.Key)
        && match j.Key with
           | "j_steel_joker" -> holds Domain.Enhancement.Steel
           | "j_stone" -> holds Domain.Enhancement.Stone
           | "j_lucky_cat" -> holds Domain.Enhancement.Lucky
           | "j_gros_michel" -> not view.GrosMichelExtinct
           | "j_cavendish" -> view.GrosMichelExtinct
           | _ -> true)

let private pick (rng: Random) (xs: 'a list) = xs[rng.Next xs.Length]

let private weighted (rng: Random) (xs: ('a * float) list) =
    let target = rng.NextDouble() * (xs |> List.sumBy snd)

    let rec go acc rest =
        match rest with
        | [ (x, _) ] -> x
        | (x, w) :: tail -> if target < acc + w then x else go (acc + w) tail
        | [] -> failwith "empty weights"

    go 0.0 xs

let private rollEdition (rng: Random) thresholds =
    let r = rng.NextDouble()
    thresholds |> List.tryFind (fun (t, _) -> r > t) |> Option.map snd

let private jokerEdition (rng: Random) =
    rollEdition
        rng
        [ 0.997, Api.Edition.Negative
          0.994, Api.Edition.Polychrome
          0.98, Api.Edition.Holo
          0.96, Api.Edition.Foil ]

let private drawJoker (rng: Random) (view: View) =
    let rarity =
        match rng.NextDouble() with
        | r when r > 0.95 -> Rare
        | r when r > 0.70 -> Uncommon
        | _ -> Common

    match pool view |> List.filter (fun j -> j.Rarity = rarity) with
    | [] -> jokers |> List.find (fun j -> j.Key = "j_joker")
    | xs -> pick rng xs

let private jokerOffer rng view =
    let info = drawJoker rng view
    jokerCard info (jokerEdition rng)

let private tarotOffer (rng: Random) (view: View) =
    match tarots |> List.filter (fun (k, _) -> not (view.Owned.Contains k)) with
    | [] -> consumableCard "c_strength"
    | xs -> consumableCard (fst (pick rng xs))

let private planetOffer (rng: Random) (view: View) =
    let unlocked (k, _, hand) =
        not (view.Owned.Contains k)
        && hand |> Option.forall view.Played.Contains

    match planets |> List.filter unlocked with
    | [] -> consumableCard "c_pluto"
    | xs -> consumableCard (let k, _, _ = pick rng xs in k)

let private spectralOffer (rng: Random) (view: View) =
    match spectrals |> List.filter (fun (k, _) -> not (view.Owned.Contains k)) with
    | [] -> consumableCard "c_incantation"
    | xs -> consumableCard (fst (pick rng xs))

let private fill n (view: View) (gen: View -> Api.Card) =
    (([], view.Owned), [ 1..n ])
    ||> List.fold (fun (acc, taken) _ ->
        let card = gen { view with Owned = taken }
        card :: acc, taken.Add card.Key)
    |> fst
    |> List.rev

let shopCards (rng: Random) (view: View) : Api.Card list =
    fill 2 view (fun v ->
        match weighted rng [ 0, 20.0; 1, 4.0; 2, 4.0 ] with
        | 0 -> jokerOffer rng v
        | 1 -> tarotOffer rng v
        | _ -> planetOffer rng v)

let private packTable =
    [ for kind, label, counts, weights in
          [ "arcana", "Arcana", (4, 2, 2), (1.0, 1.0, 0.25)
            "celestial", "Celestial", (4, 2, 2), (1.0, 1.0, 0.25)
            "standard", "Standard", (4, 2, 2), (1.0, 1.0, 0.25)
            "buffoon", "Buffoon", (2, 1, 1), (0.6, 0.6, 0.15)
            "spectral", "Spectral", (2, 1, 1), (0.3, 0.3, 0.07) ] do
          let normals, jumbos, megas = counts
          let wn, wj, wm = weights

          for i in 1..normals -> sprintf "p_%s_normal_%d" kind i, label + " Pack", 4, wn
          for i in 1..jumbos -> sprintf "p_%s_jumbo_%d" kind i, "Jumbo " + label + " Pack", 6, wj
          for i in 1..megas -> sprintf "p_%s_mega_%d" kind i, "Mega " + label + " Pack", 8, wm ]

let private packCard (key, label, cost, _) =
    simple key Api.CardSet.Booster label cost

let packs (rng: Random) (first: bool) : Api.Card list =
    let draw () =
        weighted rng (packTable |> List.map (fun p -> let _, _, _, w = p in p, w))

    let head =
        if first then
            let key = sprintf "p_buffoon_normal_%d" (rng.Next 2 + 1)
            packTable |> List.find (fun (k, _, _, _) -> k = key)
        else
            draw ()

    [ packCard head; packCard (draw ()) ]

let packChoose (packKey: string) : int =
    if packKey.Contains "_mega_" then 2 else 1

let private suitLetters =
    [ Api.Suit.Hearts, "H"; Api.Suit.Diamonds, "D"; Api.Suit.Clubs, "C"; Api.Suit.Spades, "S" ]

let private rankLetters =
    [ Api.Rank.Two, "2"
      Api.Rank.Three, "3"
      Api.Rank.Four, "4"
      Api.Rank.Five, "5"
      Api.Rank.Six, "6"
      Api.Rank.Seven, "7"
      Api.Rank.Eight, "8"
      Api.Rank.Nine, "9"
      Api.Rank.Ten, "T"
      Api.Rank.Jack, "J"
      Api.Rank.Queen, "Q"
      Api.Rank.King, "K"
      Api.Rank.Ace, "A" ]

let private playingCard (rng: Random) : Api.Card =
    let suit, s = pick rng suitLetters
    let rank, r = pick rng rankLetters

    let enhancement =
        if rng.NextDouble() > 0.6 then
            Some(
                pick
                    rng
                    [ Api.Enhancement.Bonus
                      Api.Enhancement.Mult
                      Api.Enhancement.Wild
                      Api.Enhancement.Glass
                      Api.Enhancement.Steel
                      Api.Enhancement.Stone
                      Api.Enhancement.Gold
                      Api.Enhancement.Lucky ]
            )
        else
            None

    let edition =
        rollEdition rng [ 0.988, Api.Edition.Polychrome; 0.96, Api.Edition.Holo; 0.92, Api.Edition.Foil ]

    let seal =
        if rng.NextDouble() > 0.8 then
            Some(pick rng [ Api.Seal.Red; Api.Seal.Blue; Api.Seal.Gold; Api.Seal.Purple ])
        else
            None

    let set =
        if enhancement.IsSome then
            Api.CardSet.Enhanced
        else
            Api.CardSet.Default

    build (s + "_" + r) set "Base Card" "" edition (Some suit) (Some rank) enhancement seal 1

let packContents (rng: Random) (view: View) (packKey: string) : Api.Card list =
    let parts = packKey.Split '_'
    let kind = parts[1]
    let size = if kind = "buffoon" || kind = "spectral" then 2 else 3
    let n = if parts[2] = "normal" then size else size + 2

    let rare (extra: string * string) (v: View) (otherwise: View -> Api.Card) =
        if rng.NextDouble() > 0.997 && not (v.Owned.Contains(fst extra)) then
            consumableCard (fst extra)
        else
            otherwise v

    fill n view (fun v ->
        match kind with
        | "arcana" -> rare soul v (tarotOffer rng)
        | "celestial" -> rare blackHole v (planetOffer rng)
        | "spectral" -> rare soul v (fun v -> rare blackHole v (spectralOffer rng))
        | "standard" -> playingCard rng
        | _ -> jokerOffer rng v)

let voucher (rng: Random) (redeemed: Set<string>) : Api.Card =
    match vouchers |> List.filter (fun (k, _) -> not (redeemed.Contains k)) with
    | [] -> simple "v_blank" Api.CardSet.Voucher "Blank" 10
    | xs ->
        let key, label = pick rng xs
        simple key Api.CardSet.Voucher label 10
