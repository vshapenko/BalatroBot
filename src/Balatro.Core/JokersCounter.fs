module Balatro.Core.JokersCounter

open System
open System.Globalization
open System.Text.RegularExpressions
open Balatro.Core.Domain
open Balatro.Core.Jokers

let private value (joker: Joker) = joker.Value |> Option.defaultValue 0.0

let private isEnhanced (card: Card) = card.Enhancement.IsSome && not card.Debuffed

let private chips amount =
    if amount > 0.0 then [ AddChips amount ] else []

let private mult amount =
    if amount > 0.0 then [ AddMult amount ] else []

let private times factor =
    if factor > 1.0 then [ TimesMult factor ] else []

// The counter as it stands, turned into its effect
let private standing (effect: float -> Effect list) =
    { JokerDef.none with
        Main = fun _ joker -> effect (value joker) }

// The current value written at the start of the card text, 0 when the text is not readable
let private leading (prefix: string) (joker: Joker) =
    let m = Regex.Match(joker.Text, "^" + Regex.Escape prefix + @"(\d+(?:\.\d+)?)")

    if m.Success then
        Double.Parse(m.Groups[1].Value, CultureInfo.InvariantCulture)
    else
        0.0

let defs: (string * JokerDef) list =
    [ "j_green_joker",
      { JokerDef.none with
          Main = fun _ joker -> [ AddMult(value joker + 1.0) ] }
      "j_vampire",
      { JokerDef.none with
          BeforeCards =
            fun _ _ cards ->
                cards
                |> List.map (fun card -> if isEnhanced card then { card with Enhancement = None } else card)
          // the context holds the cards as this joker's before step received them
          Main =
            fun context joker ->
                let stripped = context.Seen |> List.filter isEnhanced |> List.length
                let factor = value joker + 0.1 * float stripped
                if factor > 1.0 then [ TimesMult factor ] else [] }
      "j_ride_the_bus",
      { JokerDef.none with
          Main =
            fun context joker ->
                mult (if context.Seen |> List.exists (isFace context) then 0.0 else value joker + 1.0) }
      "j_runner",
      { JokerDef.none with
          Main = fun context joker -> chips (value joker + (if context.Contained.Contains Straight then 15.0 else 0.0)) }
      "j_square",
      { JokerDef.none with
          Main = fun context joker -> chips (value joker + (if context.Played.Length = 4 then 4.0 else 0.0)) }
      "j_trousers",
      { JokerDef.none with
          Main =
            fun context joker ->
                let paired = context.Contained.Contains TwoPair || context.Contained.Contains FullHouse
                mult (value joker + (if paired then 2.0 else 0.0)) }
      "j_obelisk",
      { JokerDef.none with
          Main =
            fun context joker ->
                let plays = context.Situation.HandPlays
                // the game counts the hand being scored before any joker runs
                let thisHand = (plays |> Map.tryFind context.HandType |> Option.map fst |> Option.defaultValue 0) + 1

                let matched =
                    plays |> Map.exists (fun handType (total, _) -> handType <> context.HandType && total >= thisHand)

                times (if matched then value joker + 0.2 else 1.0) }
      "j_ceremonial", standing mult
      "j_constellation", standing times
      "j_red_card", standing mult
      "j_madness", standing times
      "j_hologram", standing times
      "j_flash", standing mult
      "j_castle", standing chips
      "j_campfire", standing times
      "j_glass", standing times
      "j_lucky_cat", standing times
      "j_hit_the_road", standing times
      "j_yorick", standing times
      "j_caino", standing times
      "j_wee", standing chips
      "j_ice_cream",
      { JokerDef.none with
          Main = fun _ joker -> chips (leading "+" joker) }
      "j_popcorn",
      { JokerDef.none with
          Main = fun _ joker -> mult (leading "+" joker) }
      "j_ramen",
      { JokerDef.none with
          Main = fun _ joker -> times (leading "X" joker) }
      // the hand counter moves after scoring, so the hand that pays shows as active in the text
      "j_loyalty_card",
      { JokerDef.none with
          Main = fun _ joker -> if Regex.IsMatch(joker.Text, @"Active!|\b0 remaining") then [ TimesMult 4.0 ] else [] }
      "j_midas_mask",
      { JokerDef.none with
          BeforeCards =
            fun context _ cards ->
                cards
                |> List.map (fun card ->
                    if isFace context card then
                        { card with Enhancement = Some Enhancement.Gold }
                    else
                        card) }
      "j_space", JokerDef.none
      "j_dna", JokerDef.none
      "j_todo_list", JokerDef.none
      "j_sixth_sense", JokerDef.none ]
