module Balatro.Core.JokersMain

open Balatro.Core.Domain
open Balatro.Core.Jokers

let private value (joker: Joker) = joker.Value |> Option.defaultValue 0.0

let private fixedEffect effect =
    { JokerDef.none with Main = fun _ _ -> [ effect ] }

let private whenContains handType effect =
    { JokerDef.none with
        Main = fun context _ -> if context.Contained.Contains handType then [ effect ] else [] }

let private addValue effect =
    { JokerDef.none with
        Main = fun _ joker -> if value joker > 0.0 then [ effect (value joker) ] else [] }

let private timesValue =
    { JokerDef.none with
        Main = fun _ joker -> if value joker > 1.0 then [ TimesMult(value joker) ] else [] }

let private isWild (card: Card) = card.Enhancement = Some Enhancement.Wild

// A card fills the first still-empty suit of the order that it matches
let private fillFirst (order: Suit list) (covered: Set<Suit>) (matches: Suit -> bool) =
    match order |> List.tryFind (fun suit -> not (covered.Contains suit) && matches suit) with
    | Some suit -> covered.Add suit
    | None -> covered

let private flowerPot context =
    let order = [ Hearts; Diamonds; Spades; Clubs ]

    let fromPlain =
        context.Scoring
        |> List.filter (isWild >> not)
        |> List.fold
            (fun covered card -> fillFirst order covered (fun suit -> isSuit context suit { card with Debuffed = false }))
            Set.empty

    let covered =
        context.Scoring
        |> List.filter isWild
        |> List.fold (fun covered card -> fillFirst order covered (fun suit -> isSuit context suit card)) fromPlain

    covered.Count = 4

let private seeingDouble context =
    let fromPlain =
        context.Scoring
        |> List.filter (isWild >> not)
        |> List.collect (fun card -> [ Hearts; Diamonds; Spades; Clubs ] |> List.filter (fun suit -> isSuit context suit card))
        |> Set.ofList

    let covered =
        context.Scoring
        |> List.filter isWild
        |> List.fold
            (fun covered card -> fillFirst [ Clubs; Diamonds; Spades; Hearts ] covered (fun suit -> isSuit context suit card))
            fromPlain

    covered.Contains Clubs && (covered.Contains Hearts || covered.Contains Diamonds || covered.Contains Spades)

let private noScoreEffect =
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
    |> List.map (fun key -> key, JokerDef.none)

let defs: (string * JokerDef) list =
    [ "j_joker", { JokerDef.none with Main = fun _ _ -> [ AddMult 4.0 ] }
      "j_sly",
      { JokerDef.none with
          Main = fun context _ -> if context.Contained.Contains Pair then [ AddChips 50.0 ] else [] }
      "j_jolly", whenContains Pair (AddMult 8.0)
      "j_zany", whenContains ThreeOfAKind (AddMult 12.0)
      "j_mad", whenContains TwoPair (AddMult 10.0)
      "j_crazy", whenContains Straight (AddMult 12.0)
      "j_droll", whenContains Flush (AddMult 10.0)
      "j_wily", whenContains ThreeOfAKind (AddChips 100.0)
      "j_clever", whenContains TwoPair (AddChips 80.0)
      "j_devious", whenContains Straight (AddChips 100.0)
      "j_crafty", whenContains Flush (AddChips 80.0)
      "j_duo", whenContains Pair (TimesMult 2.0)
      "j_trio", whenContains ThreeOfAKind (TimesMult 3.0)
      "j_family", whenContains FourOfAKind (TimesMult 4.0)
      "j_order", whenContains Straight (TimesMult 3.0)
      "j_tribe", whenContains Flush (TimesMult 2.0)
      "j_half",
      { JokerDef.none with
          Main = fun context _ -> if context.Played.Length <= 3 then [ AddMult 20.0 ] else [] }
      "j_gros_michel", fixedEffect (AddMult 15.0)
      "j_cavendish", fixedEffect (TimesMult 3.0)
      "j_stuntman", fixedEffect (AddChips 250.0)
      "j_blackboard",
      { JokerDef.none with
          // a held card counts as black even when debuffed; Wild only while not debuffed
          Main =
            fun context _ ->
                let isBlack (card: Card) =
                    card.Enhancement <> Some Enhancement.Stone
                    && ((isWild card && not card.Debuffed) || card.Suit = Spades || card.Suit = Clubs)

                if context.Held |> List.forall isBlack then [ TimesMult 3.0 ] else [] }
      "j_flower_pot",
      { JokerDef.none with
          Main = fun context _ -> if flowerPot context then [ TimesMult 3.0 ] else [] }
      "j_seeing_double",
      { JokerDef.none with
          Main = fun context _ -> if seeingDouble context then [ TimesMult 2.0 ] else [] }
      "j_banner",
      { JokerDef.none with
          Main =
            fun context _ ->
                let discards = context.Situation.DiscardsLeft
                if discards > 0 then [ AddChips(30.0 * float discards) ] else [] }
      "j_mystic_summit",
      { JokerDef.none with
          Main = fun context _ -> if context.Situation.DiscardsLeft = 0 then [ AddMult 15.0 ] else [] }
      "j_acrobat",
      { JokerDef.none with
          Main = fun context _ -> if context.Situation.HandsLeft = 0 then [ TimesMult 3.0 ] else [] }
      "j_supernova",
      { JokerDef.none with
          // the game counts this play before the joker runs
          Main =
            fun context _ ->
                let runPlays =
                    context.Situation.HandPlays |> Map.tryFind context.HandType |> Option.map fst |> Option.defaultValue 0

                [ AddMult(float (runPlays + 1)) ] }
      "j_card_sharp",
      { JokerDef.none with
          Main =
            fun context _ ->
                let roundPlays =
                    context.Situation.HandPlays |> Map.tryFind context.HandType |> Option.map snd |> Option.defaultValue 0

                if roundPlays >= 1 then [ TimesMult 3.0 ] else [] }
      "j_stencil", timesValue
      "j_steel_joker", timesValue
      "j_abstract", addValue AddMult
      "j_blue_joker", addValue AddChips
      "j_erosion", addValue AddMult
      "j_fortune_teller", addValue AddMult
      "j_stone", addValue AddChips
      "j_bull", addValue AddChips
      "j_swashbuckler", addValue AddMult
      "j_throwback", timesValue
      "j_bootstraps", addValue AddMult
      "j_drivers_license",
      { JokerDef.none with
          Main = fun _ joker -> if value joker >= 16.0 then [ TimesMult 3.0 ] else [] } ]
    @ noScoreEffect
