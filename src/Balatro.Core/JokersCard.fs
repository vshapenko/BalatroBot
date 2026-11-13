module Balatro.Core.JokersCard

open Balatro.Core.Domain
open Balatro.Core.Jokers

let private firstFace (context: Context) =
    context.Scoring |> List.tryFindIndex (isFace context)

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
      "j_mime", { JokerDef.none with HeldRetriggers = fun _ _ _ -> 1 } ]
