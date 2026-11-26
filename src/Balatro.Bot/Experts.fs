module Balatro.Bot.Experts

open Balatro.Core.JokerProfiles
open Balatro.Bot.Decision

let planWeight = 3.0
let huntWeight = 5.0

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

let views (owned: int) (plan: Plan.Plan) (wanted: bool) (e: Shop.Evaluated) : ExpertView list =
    [ { Expert = "Survival"
        Points = (e.After / e.Before - 1.0) * 100.0
        Why = sprintf "round score %.0f -> %.0f" e.Before e.After }
      { Expert = "Economy"
        Points = -((Shop.threshold owned e - 1.0) * 100.0)
        Why = sprintf "bar +%d%% base, $%d interest lost" (if owned < 3 then 2 else 5) e.InterestLost }
      { Expert = "Plan"
        Points = if fitsPlan plan e then planWeight else 0.0
        Why = if fitsPlan plan e then sprintf "fits %A" plan.Hand else "off plan" }
      { Expert = "Hunt"
        Points = if wanted then huntWeight else 0.0
        Why = if wanted then "X-Mult joker wanted (takes priority)" else "no hunt" } ]

let total (views: ExpertView list) : float = List.sumBy (fun v -> v.Points) views

let judge
    (owned: int)
    (plan: Plan.Plan)
    (wantedOf: Shop.Evaluated -> bool)
    (l: Shop.Evaluated list)
    : Shop.Evaluated option =
    let passing =
        l
        |> List.map (fun e -> e, total (views owned plan (wantedOf e) e))
        |> List.filter (fun (_, t) -> t >= 0.0)

    let best (c: (Shop.Evaluated * float) list) =
        c
        |> List.fold
            (fun acc (e, t) ->
                match acc with
                | Some(_, bt) when bt >= t -> acc
                | _ -> Some(e, t))
            None
        |> Option.map fst

    match passing |> List.filter (fst >> wantedOf) with
    | [] -> best passing
    | wanted -> best wanted
