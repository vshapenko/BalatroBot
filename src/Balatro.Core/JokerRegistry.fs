module Balatro.Core.JokerRegistry

open Balatro.Core.Hands
open Balatro.Core.Jokers

let private table =
    [ JokersMain.defs; JokersCard.defs; JokersCounter.defs ] |> List.concat |> Map.ofList

let find (key: string) : JokerDef =
    Map.tryFind key table |> Option.defaultValue JokerDef.none

let implemented: Set<string> =
    [ "j_four_fingers"
      "j_shortcut"
      "j_smeared"
      "j_splash"
      "j_pareidolia"
      "j_baseball"
      "j_blueprint"
      "j_brainstorm" ]
    @ (table |> Map.keys |> List.ofSeq)
    |> Set.ofList

let uncommon: Set<string> =
    Set.ofList
        [ "j_stencil"
          "j_four_fingers"
          "j_mime"
          "j_ceremonial"
          "j_marble"
          "j_loyalty_card"
          "j_dusk"
          "j_fibonacci"
          "j_steel_joker"
          "j_hack"
          "j_pareidolia"
          "j_space"
          "j_burglar"
          "j_blackboard"
          "j_sixth_sense"
          "j_constellation"
          "j_hiker"
          "j_card_sharp"
          "j_madness"
          "j_seance"
          "j_vampire"
          "j_shortcut"
          "j_hologram"
          "j_cloud_9"
          "j_rocket"
          "j_midas_mask"
          "j_luchador"
          "j_gift"
          "j_turtle_bean"
          "j_erosion"
          "j_to_the_moon"
          "j_stone"
          "j_lucky_cat"
          "j_bull"
          "j_diet_cola"
          "j_trading"
          "j_flash"
          "j_trousers"
          "j_ramen"
          "j_selzer"
          "j_castle"
          "j_mr_bones"
          "j_acrobat"
          "j_sock_and_buskin"
          "j_troubadour"
          "j_certificate"
          "j_smeared"
          "j_throwback"
          "j_rough_gem"
          "j_bloodstone"
          "j_arrowhead"
          "j_onyx_agate"
          "j_glass"
          "j_ring_master"
          "j_flower_pot"
          "j_merry_andy"
          "j_oops"
          "j_idol"
          "j_seeing_double"
          "j_matador"
          "j_satellite"
          "j_cartomancer"
          "j_astronomer"
          "j_bootstraps" ]

let rules (jokers: Joker list) : HandRules =
    let active key =
        jokers |> List.exists (fun joker -> joker.Key = key && not joker.Debuffed)

    { FourFingers = active "j_four_fingers"
      Shortcut = active "j_shortcut"
      Smeared = active "j_smeared"
      Splash = active "j_splash"
      Pareidolia = active "j_pareidolia" }
