module Balatro.Bot.Experts

open Balatro.Core.JokerProfiles
open Balatro.Bot.Decision

let passes (owned: int) (e: Shop.Evaluated) : bool = e.After / e.Before >= Shop.threshold owned e

let judge (owned: int) (l: Shop.Evaluated list) : Shop.Evaluated option = l |> List.tryFind (passes owned)

let private fitsPlan (plan: Plan.Plan) (e: Shop.Evaluated) =
    match e.Offer with
    | Shop.PlanetOffer(_, hand, _, _) -> hand = plan.Hand
    | Shop.JokerOffer(_, joker, _) ->
        (profile joker.Key).Wants
        |> List.exists (function
            | Hand h -> h = plan.Hand
            | Suit s -> Some s = plan.Suit
            | _ -> false)
    | Shop.TarotOffer _ -> false

let views (owned: int) (plan: Plan.Plan) (e: Shop.Evaluated) : ExpertView list =
    [ { Expert = "Survival"
        Points = (e.After / e.Before - 1.0) * 100.0
        Why = sprintf "round score %.0f -> %.0f" e.Before e.After }
      { Expert = "Economy"
        Points = -((Shop.threshold owned e - 1.0) * 100.0)
        Why = sprintf "bar +%d%% base, $%d interest lost" (if owned < 3 then 2 else 5) e.InterestLost }
      { Expert = "Plan"
        Points = 0.0
        Why = if fitsPlan plan e then sprintf "fits %A" plan.Hand else "off plan" } ]
