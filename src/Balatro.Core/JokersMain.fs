module Balatro.Core.JokersMain

open Balatro.Core.Domain
open Balatro.Core.Jokers

let defs: (string * JokerDef) list =
    [ "j_joker", { JokerDef.none with Main = fun _ _ -> [ AddMult 4.0 ] }
      "j_sly",
      { JokerDef.none with
          Main = fun context _ -> if context.Contained.Contains Pair then [ AddChips 50.0 ] else [] } ]
