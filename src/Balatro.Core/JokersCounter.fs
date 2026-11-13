module Balatro.Core.JokersCounter

open Balatro.Core.Domain
open Balatro.Core.Jokers

let private value (joker: Joker) = joker.Value |> Option.defaultValue 0.0

let private isEnhanced (card: Card) = card.Enhancement.IsSome && not card.Debuffed

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
                let stripped = context.Scoring |> List.filter isEnhanced |> List.length
                let factor = value joker + 0.1 * float stripped
                if factor > 1.0 then [ TimesMult factor ] else [] } ]
