module Balatro.Core.JokersCard

open System.Text.RegularExpressions
open Balatro.Core.Domain
open Balatro.Core.Jokers

let private firstFace (context: Context) =
    context.Scoring |> List.tryFindIndex (isFace context)

let private hasRank (ranks: Rank list) (card: Card) =
    card.Enhancement <> Some Enhancement.Stone && List.contains card.Rank ranks

let private suitMult (suit: Suit) =
    { JokerDef.none with
        OnScored = fun context _ _ card -> if isSuit context suit card then [ AddMult 3.0 ] else [] }

let private suitOfName (name: string) =
    match name.TrimEnd 's' with
    | "Spade" -> Some Spades
    | "Heart" -> Some Hearts
    | "Club" -> Some Clubs
    | "Diamond" -> Some Diamonds
    | _ -> None

let private allRanks =
    [ Two; Three; Four; Five; Six; Seven; Eight; Nine; Ten; Jack; Queen; King; Ace ]

let defs: (string * JokerDef) list =
    [ "j_greedy_joker",
      { JokerDef.none with
          OnScored = fun context _ _ card -> if isSuit context Diamonds card then [ AddMult 3.0 ] else [] }
      "j_photograph",
      { JokerDef.none with
          OnScored = fun context _ position _ -> if firstFace context = Some position then [ TimesMult 2.0 ] else [] }
      "j_hanging_chad",
      { JokerDef.none with
          Retriggers = fun _ _ position _ -> if position = 0 then 2 else 0 }
      "j_baron",
      { JokerDef.none with
          OnHeld =
            fun _ _ card ->
                if card.Rank = King && not card.Debuffed && card.Enhancement <> Some Enhancement.Stone then
                    [ TimesMult 1.5 ]
                else
                    [] }
      "j_mime", { JokerDef.none with HeldRetriggers = fun _ _ _ -> 1 }
      "j_lusty_joker", suitMult Hearts
      "j_wrathful_joker", suitMult Spades
      "j_gluttenous_joker", suitMult Clubs
      "j_fibonacci",
      { JokerDef.none with
          OnScored = fun _ _ _ card -> if hasRank [ Ace; Two; Three; Five; Eight ] card then [ AddMult 8.0 ] else [] }
      "j_scary_face",
      { JokerDef.none with
          OnScored = fun context _ _ card -> if isFace context card then [ AddChips 30.0 ] else [] }
      "j_smiley",
      { JokerDef.none with
          OnScored = fun context _ _ card -> if isFace context card then [ AddMult 5.0 ] else [] }
      "j_even_steven",
      { JokerDef.none with
          OnScored = fun _ _ _ card -> if hasRank [ Two; Four; Six; Eight; Ten ] card then [ AddMult 4.0 ] else [] }
      "j_odd_todd",
      { JokerDef.none with
          OnScored =
            fun _ _ _ card -> if hasRank [ Three; Five; Seven; Nine; Ace ] card then [ AddChips 31.0 ] else [] }
      "j_scholar",
      { JokerDef.none with
          OnScored = fun _ _ _ card -> if hasRank [ Ace ] card then [ AddChips 20.0; AddMult 4.0 ] else [] }
      "j_walkie_talkie",
      { JokerDef.none with
          OnScored = fun _ _ _ card -> if hasRank [ Ten; Four ] card then [ AddChips 10.0; AddMult 4.0 ] else [] }
      "j_arrowhead",
      { JokerDef.none with
          OnScored = fun context _ _ card -> if isSuit context Spades card then [ AddChips 50.0 ] else [] }
      "j_onyx_agate",
      { JokerDef.none with
          OnScored = fun context _ _ card -> if isSuit context Clubs card then [ AddMult 7.0 ] else [] }
      "j_triboulet",
      { JokerDef.none with
          OnScored = fun _ _ _ card -> if hasRank [ King; Queen ] card then [ TimesMult 2.0 ] else [] }
      "j_ancient",
      { JokerDef.none with
          OnScored =
            fun context joker _ card ->
                let suit =
                    Regex.Match(joker.Text, @"with (\w+) suit")
                    |> fun m -> if m.Success then suitOfName m.Groups[1].Value else None

                match suit with
                | Some suit when isSuit context suit card -> [ TimesMult 1.5 ]
                | _ -> [] }
      "j_idol",
      { JokerDef.none with
          OnScored =
            fun context joker _ card ->
                let m = Regex.Match(joker.Text, @"played (\w+) of (\w+) gives")

                let target =
                    if m.Success then
                        List.tryFind (fun rank -> Rank.label rank = m.Groups[1].Value) allRanks,
                        suitOfName m.Groups[2].Value
                    else
                        None, None

                match target with
                | Some rank, Some suit when hasRank [ rank ] card && isSuit context suit card -> [ TimesMult 2.0 ]
                | _ -> [] }
      "j_sock_and_buskin",
      { JokerDef.none with
          Retriggers = fun context _ _ card -> if isFace context card then 1 else 0 }
      "j_hack",
      { JokerDef.none with
          Retriggers = fun _ _ _ card -> if hasRank [ Two; Three; Four; Five ] card then 1 else 0 }
      "j_selzer", { JokerDef.none with Retriggers = fun _ _ _ _ -> 1 }
      "j_dusk",
      { JokerDef.none with
          Retriggers = fun context _ _ _ -> if context.Situation.HandsLeft = 0 then 1 else 0 }
      "j_raised_fist",
      { JokerDef.none with
          OnHeld =
            fun context _ card ->
                // Ties go to the last lowest card; a debuffed lowest card scores nothing
                let lowest =
                    context.Held
                    |> List.filter (fun held -> held.Enhancement <> Some Enhancement.Stone)
                    |> List.fold
                        (fun best held ->
                            match best with
                            | Some(lowest: Card) when Rank.id held.Rank > Rank.id lowest.Rank -> best
                            | _ -> Some held)
                        None

                match lowest with
                | Some lowest when obj.ReferenceEquals(lowest, card) && not card.Debuffed ->
                    [ AddMult(2.0 * float (Rank.nominal card.Rank)) ]
                | _ -> [] }
      "j_shoot_the_moon",
      { JokerDef.none with
          OnHeld = fun _ _ card -> if hasRank [ Queen ] card && not card.Debuffed then [ AddMult 13.0 ] else [] }
      "j_8_ball", JokerDef.none
      "j_business", JokerDef.none
      "j_ticket", JokerDef.none
      "j_rough_gem", JokerDef.none
      "j_reserved_parking", JokerDef.none
      "j_bloodstone", JokerDef.none
      "j_hiker", JokerDef.none ]
