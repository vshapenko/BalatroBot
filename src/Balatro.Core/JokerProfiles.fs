module Balatro.Core.JokerProfiles

open Balatro.Core.Domain

type Role =
    | Chips
    | Mult
    | XMult
    | Economy
    | Scaling
    | Retrigger
    | Utility

type Want =
    | Hand of HandType
    | Suit of Suit
    | Ranks of Rank list
    | Faces
    | Discards
    | Money
    | FewCards
    | Enhanced of string
    | DeckSize

type Profile =
    { Roles: Role list
      Wants: Want list
      Earns: int }

let private table: Map<string, Profile> =
    [
        "j_8_ball", [ Utility ], [ Ranks [ Eight ] ], 0
        "j_abstract", [ Mult ], [], 0
        "j_acrobat", [ XMult ], [], 0
        "j_ancient", [ XMult ], [], 0
        "j_arrowhead", [ Chips ], [ Suit Spades ], 0
        "j_astronomer", [ Utility ], [], 0
        "j_banner", [ Chips ], [ Discards ], 0
        "j_baron", [ XMult ], [ Ranks [ King ] ], 0
        "j_baseball", [ XMult ], [], 0
        "j_blackboard", [ XMult ], [ Suit Spades; Suit Clubs ], 0
        "j_bloodstone", [ XMult ], [ Suit Hearts ], 0
        "j_blue_joker", [ Chips ], [ DeckSize ], 0
        "j_blueprint", [ Utility ], [], 0
        "j_bootstraps", [ Mult ], [ Money ], 0
        "j_brainstorm", [ Utility ], [], 0
        "j_bull", [ Chips ], [ Money ], 0
        "j_burglar", [ Utility ], [], 0
        "j_burnt", [ Utility ], [ Discards ], 0
        "j_business", [ Economy ], [ Faces ], 0
        "j_caino", [ Scaling; XMult ], [ Faces ], 0
        "j_campfire", [ Scaling; XMult ], [], 0
        "j_card_sharp", [ XMult ], [], 0
        "j_cartomancer", [ Utility ], [], 0
        "j_castle", [ Scaling; Chips ], [ Discards ], 0
        "j_cavendish", [ XMult ], [], 0
        "j_ceremonial", [ Scaling; Mult ], [], 0
        "j_certificate", [ Utility ], [], 0
        "j_chaos", [ Utility ], [], 0
        "j_chicot", [ Utility ], [], 0
        "j_clever", [ Chips ], [ Hand TwoPair ], 0
        "j_cloud_9", [ Economy ], [ Ranks [ Nine ] ], 0
        "j_constellation", [ Scaling; XMult ], [], 0
        "j_crafty", [ Chips ], [ Hand Flush ], 0
        "j_crazy", [ Mult ], [ Hand Straight ], 0
        "j_credit_card", [ Utility ], [], 0
        "j_delayed_grat", [ Economy ], [ Discards ], 0
        "j_devious", [ Chips ], [ Hand Straight ], 0
        "j_diet_cola", [ Utility ], [], 0
        "j_dna", [ Utility ], [ FewCards ], 0
        "j_drivers_license", [ XMult ], [ Enhanced "Any" ], 0
        "j_droll", [ Mult ], [ Hand Flush ], 0
        "j_drunkard", [ Utility ], [], 0
        "j_duo", [ XMult ], [ Hand Pair ], 0
        "j_dusk", [ Retrigger ], [], 0
        "j_egg", [ Economy ], [], 0
        "j_erosion", [ Mult ], [ DeckSize ], 0
        "j_even_steven", [ Mult ], [ Ranks [ Two; Four; Six; Eight; Ten ] ], 0
        "j_faceless", [ Economy ], [ Faces; Discards ], 0
        "j_family", [ XMult ], [ Hand FourOfAKind ], 0
        "j_fibonacci", [ Mult ], [ Ranks [ Ace; Two; Three; Five; Eight ] ], 0
        "j_flash", [ Scaling; Mult ], [], 0
        "j_flower_pot", [ XMult ], [ Suit Spades; Suit Hearts; Suit Clubs; Suit Diamonds ], 0
        "j_fortune_teller", [ Scaling; Mult ], [], 0
        "j_four_fingers", [ Utility ], [], 0
        "j_gift", [ Economy ], [], 0
        "j_glass", [ Scaling; XMult ], [ Enhanced "Glass" ], 0
        "j_gluttenous_joker", [ Mult ], [ Suit Clubs ], 0
        "j_golden", [ Economy ], [], 4
        "j_greedy_joker", [ Mult ], [ Suit Diamonds ], 0
        "j_green_joker", [ Scaling; Mult ], [ Discards ], 0
        "j_gros_michel", [ Mult ], [], 0
        "j_hack", [ Retrigger ], [ Ranks [ Two; Three; Four; Five ] ], 0
        "j_half", [ Mult ], [ FewCards ], 0
        "j_hallucination", [ Utility ], [], 0
        "j_hanging_chad", [ Retrigger ], [], 0
        "j_hiker", [ Scaling; Chips ], [], 0
        "j_hit_the_road", [ Scaling; XMult ], [ Ranks [ Jack ]; Discards ], 0
        "j_hologram", [ Scaling; XMult ], [], 0
        "j_ice_cream", [ Chips ], [], 0
        "j_idol", [ XMult ], [], 0
        "j_invisible", [ Utility ], [], 0
        "j_joker", [ Mult ], [], 0
        "j_jolly", [ Mult ], [ Hand Pair ], 0
        "j_juggler", [ Utility ], [], 0
        "j_loyalty_card", [ XMult ], [], 0
        "j_luchador", [ Utility ], [], 0
        "j_lucky_cat", [ Scaling; XMult ], [ Enhanced "Lucky" ], 0
        "j_lusty_joker", [ Mult ], [ Suit Hearts ], 0
        "j_mad", [ Mult ], [ Hand TwoPair ], 0
        "j_madness", [ Scaling; XMult ], [], 0
        "j_mail", [ Economy ], [ Discards ], 0
        "j_marble", [ Utility ], [], 0
        "j_matador", [ Economy ], [], 0
        "j_merry_andy", [ Utility ], [], 0
        "j_midas_mask", [ Utility ], [ Faces ], 0
        "j_mime", [ Retrigger ], [], 0
        "j_misprint", [ Mult ], [], 0
        "j_mr_bones", [ Utility ], [], 0
        "j_mystic_summit", [ Mult ], [ Discards ], 0
        "j_obelisk", [ Scaling; XMult ], [], 0
        "j_odd_todd", [ Chips ], [ Ranks [ Ace; Three; Five; Seven; Nine ] ], 0
        "j_onyx_agate", [ Mult ], [ Suit Clubs ], 0
        "j_oops", [ Utility ], [], 0
        "j_order", [ XMult ], [ Hand Straight ], 0
        "j_pareidolia", [ Utility ], [], 0
        "j_perkeo", [ Utility ], [], 0
        "j_photograph", [ XMult ], [ Faces ], 0
        "j_popcorn", [ Mult ], [], 0
        "j_raised_fist", [ Mult ], [], 0
        "j_ramen", [ XMult ], [ Discards ], 0
        "j_red_card", [ Scaling; Mult ], [], 0
        "j_reserved_parking", [ Economy ], [ Faces ], 0
        "j_ride_the_bus", [ Scaling; Mult ], [], 0
        "j_riff_raff", [ Utility ], [], 0
        "j_ring_master", [ Utility ], [], 0
        "j_rocket", [ Economy ], [], 1
        "j_rough_gem", [ Economy ], [ Suit Diamonds ], 0
        "j_runner", [ Scaling; Chips ], [ Hand Straight ], 0
        "j_satellite", [ Economy ], [], 0
        "j_scary_face", [ Chips ], [ Faces ], 0
        "j_scholar", [ Chips; Mult ], [ Ranks [ Ace ] ], 0
        "j_seance", [ Utility ], [ Hand StraightFlush ], 0
        "j_seeing_double", [ XMult ], [ Suit Clubs ], 0
        "j_selzer", [ Retrigger ], [], 0
        "j_shoot_the_moon", [ Mult ], [ Ranks [ Queen ] ], 0
        "j_shortcut", [ Utility ], [], 0
        "j_sixth_sense", [ Utility ], [ Ranks [ Six ] ], 0
        "j_sly", [ Chips ], [ Hand Pair ], 0
        "j_smeared", [ Utility ], [], 0
        "j_smiley", [ Mult ], [ Faces ], 0
        "j_sock_and_buskin", [ Retrigger ], [ Faces ], 0
        "j_space", [ Utility ], [], 0
        "j_splash", [ Utility ], [], 0
        "j_square", [ Scaling; Chips ], [ FewCards ], 0
        "j_steel_joker", [ XMult ], [ Enhanced "Steel" ], 0
        "j_stencil", [ XMult ], [], 0
        "j_stone", [ Chips ], [ Enhanced "Stone" ], 0
        "j_stuntman", [ Chips ], [], 0
        "j_supernova", [ Mult ], [], 0
        "j_superposition", [ Utility ], [ Ranks [ Ace ]; Hand Straight ], 0
        "j_swashbuckler", [ Mult ], [], 0
        "j_throwback", [ Scaling; XMult ], [], 0
        "j_ticket", [ Economy ], [ Enhanced "Gold" ], 0
        "j_to_the_moon", [ Economy ], [ Money ], 0
        "j_todo_list", [ Economy ], [], 0
        "j_trading", [ Economy ], [ Discards ], 0
        "j_tribe", [ XMult ], [ Hand Flush ], 0
        "j_triboulet", [ XMult ], [ Ranks [ King; Queen ] ], 0
        "j_trio", [ XMult ], [ Hand ThreeOfAKind ], 0
        "j_troubadour", [ Utility ], [], 0
        "j_trousers", [ Scaling; Mult ], [ Hand TwoPair ], 0
        "j_turtle_bean", [ Utility ], [], 0
        "j_vagabond", [ Utility ], [], 0
        "j_vampire", [ Scaling; XMult ], [ Enhanced "Any" ], 0
        "j_walkie_talkie", [ Chips; Mult ], [ Ranks [ Ten; Four ] ], 0
        "j_wee", [ Scaling; Chips ], [ Ranks [ Two ] ], 0
        "j_wily", [ Chips ], [ Hand ThreeOfAKind ], 0
        "j_wrathful_joker", [ Mult ], [ Suit Spades ], 0
        "j_yorick", [ Scaling; XMult ], [ Discards ], 0
        "j_zany", [ Mult ], [ Hand ThreeOfAKind ], 0
    ]
    |> List.map (fun (key, roles, wants, earns) -> key, { Roles = roles; Wants = wants; Earns = earns })
    |> Map.ofList

let profile (key: string) : Profile =
    table |> Map.tryFind key |> Option.defaultValue { Roles = [ Utility ]; Wants = []; Earns = 0 }

let profiled: Set<string> = table |> Map.keys |> Set.ofSeq
