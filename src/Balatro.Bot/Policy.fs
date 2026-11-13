module Balatro.Bot.Policy

open Balatro.Core
open Balatro.Core.Domain
open Balatro.Bot.Decision

type Action =
    | Play of int list
    | Discard of int list
    | SelectBlind
    | CashOut
    | NextRound
    | BuyCard of int
    | UsePlanet of int
    | ReorderJokers of int list
    | SkipPack
    | SellJoker of int
    | Reroll
    | BuyPack of int
    | PickPack of int
    | Stop

let private symbol (card: Card) =
    let rank =
        match card.Rank with
        | Ten -> "T"
        | other -> (Rank.label other).Substring(0, 1)

    rank + (string card.Suit).Substring(0, 1)

let private describe (breakdown: Scoring.ScoreBreakdown) (played: Card list) =
    sprintf "play %s [%s]" (HandType.label breakdown.HandType) (played |> List.map symbol |> String.concat " ")

let private decision seq (g: Api.GameState) chosen rule candidates predicted : Decision =
    { Seq = seq
      Ante = g.AnteNum
      Round = g.RoundNum
      Phase = string g.State
      Chosen = chosen
      Rule = rule
      Candidates = candidates
      Predicted = predicted }

let private passive seq (g: Api.GameState) action chosen rule =
    action, decision seq g chosen rule [] None

let private chooseAction seq (g: Api.GameState) =
    let levels = Mapping.levels g

    let hand =
        g.Hand.Cards
        |> List.indexed
        |> List.choose (fun (i, c) -> Mapping.toCard c |> Option.map (fun card -> i, card))

    let blind =
        [ g.Blinds.Value.Small; g.Blinds.Value.Big; g.Blinds.Value.Boss ]
        |> List.find (fun b -> b.Status = Api.BlindStatus.Current)

    let situation = Mapping.situation g

    let input: Search.Input =
        { Hand = hand
          Deck = g.Cards.Cards |> List.choose Mapping.toCard
          HandsLeft = g.Round.HandsLeft
          HandSize = g.Hand.Limit
          DiscardsLeft = g.Round.DiscardsLeft
          Needed = float (blind.Score - g.Round.Chips)
          Score =
            fun handsLeftAfter discardsLeft played held ->
                Scoring.scoreWith
                    { situation with
                        HandsLeft = handsLeftAfter
                        DiscardsLeft = discardsLeft }
                    { Played = played
                      Held = held
                      Levels = levels }
          Seed = seq * 100
          RedrawSamples = 8
          Rollouts = 40 }

    let candidate (c: Search.Candidate, outlook: Search.Outlook) : Candidate =
        let outcome =
            sprintf
                "clears in %.0f%% of simulated rounds, %.1f hands on average, %.0f chips on average"
                (outlook.Clears * 100.0)
                outlook.MeanHands
                outlook.MeanChips

        match c with
        | Search.PlayCards(positions, played, b) ->
            { Action = describe b played
              Value = b.Total
              Detail = sprintf "%s; positions %A, scores %.0f now" outcome positions b.Total }
        | Search.DiscardCards(positions, expected) ->
            let cards = positions |> List.map (fun i -> symbol (List.find (fst >> (=) i) hand |> snd))

            { Action = sprintf "discard [%s]" (String.concat " " cards)
              Value = expected
              Detail = sprintf "%s; positions %A, expected best hand after redraw %.0f" outcome positions expected }

    let evaluated, rule = Search.choose input
    let candidates = evaluated |> List.truncate 5 |> List.map candidate

    match fst evaluated.Head with
    | Search.PlayCards(indices, _, best) ->
        Play indices, decision seq g candidates.Head.Action rule candidates (Some best.Total)
    | Search.DiscardCards(indices, _) -> Discard indices, decision seq g candidates.Head.Action rule candidates None

let private planetHand key =
    match key with
    | "c_mercury" -> Some Pair
    | "c_venus" -> Some ThreeOfAKind
    | "c_earth" -> Some FullHouse
    | "c_mars" -> Some FourOfAKind
    | "c_jupiter" -> Some Flush
    | "c_saturn" -> Some Straight
    | "c_uranus" -> Some TwoPair
    | "c_neptune" -> Some StraightFlush
    | "c_pluto" -> Some HighCard
    | "c_planet_x" -> Some FiveOfAKind
    | "c_ceres" -> Some FlushHouse
    | "c_eris" -> Some FlushFive
    | _ -> None

let private offerName offer =
    match offer with
    | Shop.JokerOffer(_, joker, _) -> joker.Name
    | Shop.PlanetOffer(_, _, name, _) -> name

let shopSeed (g: Api.GameState) = (g.AnteNum * 100 + g.RoundNum) * 100

let private shopBuild (g: Api.GameState) : Shop.Build =
    let situation = Mapping.situation g

    { Jokers = situation.Jokers
      Levels = Mapping.levels g
      Deck = g.Cards.Cards |> List.choose Mapping.toCard
      HandSize = g.Hand.Limit
      Hands = g.Round.HandsLeft
      Situation = situation }

let private interestFloor = 25

let private cheapestPack (g: Api.GameState) (prefix: string) =
    g.Packs.Cards
    |> List.indexed
    |> List.filter (fun (_, c) -> c.Key.StartsWith prefix && g.Money - c.Cost.Buy >= interestFloor)
    |> List.sortBy (fun (i, c) -> c.Cost.Buy, i)
    |> List.tryHead

let private shopAction seq (g: Api.GameState) =
    match g.Consumables.Cards |> List.tryFindIndex (fun c -> c.Set = Api.CardSet.Planet) with
    | Some index ->
        let name = g.Consumables.Cards[index].Label
        UsePlanet index, decision seq g ("use " + name) "use the planet bought in the shop" [] None
    | None ->
        let build = shopBuild g

        let offers =
            g.Shop.Cards
            |> List.indexed
            |> List.choose (fun (i, c) ->
                match c.Set with
                | Api.CardSet.Joker -> Some(Shop.JokerOffer(i, Mapping.joker c, c.Cost.Buy))
                | Api.CardSet.Planet when g.Consumables.Count < g.Consumables.Limit ->
                    planetHand c.Key |> Option.map (fun hand -> Shop.PlanetOffer(i, hand, c.Label, c.Cost.Buy))
                | _ -> None)

        let seed = shopSeed g
        let evaluated = Shop.evaluate seed 20 g.Money build offers
        let percent (e: Shop.Evaluated) = (e.After / e.Before - 1.0) * 100.0

        let candidate (e: Shop.Evaluated) : Candidate =
            let name = offerName e.Offer
            let cost = Shop.cost e.Offer

            { Action = sprintf "buy %s for $%d" name cost
              Value = e.After
              Detail =
                sprintf
                    "%s, $%d, round score %.0f -> %.0f (%+.0f%%), interest lost $%d"
                    name
                    cost
                    e.Before
                    e.After
                    (percent e)
                    e.InterestLost }

        match Shop.choose evaluated with
        | Some e ->
            let others = evaluated |> List.filter (fun o -> Shop.position o.Offer <> Shop.position e.Offer)
            let candidates = e :: others |> List.truncate 5 |> List.map candidate

            let rule =
                sprintf
                    "raises the simulated round score from %.0f to %.0f (+%.0f%%); threshold +%d%%"
                    e.Before
                    e.After
                    (percent e)
                    (5 + 5 * e.InterestLost)

            BuyCard(Shop.position e.Offer), decision seq g candidates.Head.Action rule candidates None
        | None ->
            let upgrades =
                Shop.upgrades seed 20 g.Money build offers
                |> List.filter (fun u -> not g.Jokers.Cards[u.Sell].Modifier.Eternal)


            match Shop.chooseUpgrade upgrades with
            | Some u ->
                let sold = build.Jokers[u.Sell]
                let bought = offerName u.Offer

                let candidate (u: Shop.Upgrade) : Candidate =
                    { Action = sprintf "sell %s, buy %s" build.Jokers[u.Sell].Name (offerName u.Offer)
                      Value = u.After
                      Detail =
                        sprintf
                            "round score %.0f -> %.0f (%+.0f%%), net cost $%d"
                            u.Before
                            u.After
                            ((u.After / u.Before - 1.0) * 100.0)
                            u.Net }

                let rule =
                    sprintf
                        "joker slots are full; selling %s to buy %s raises the simulated round score from %.0f to %.0f (+%.0f%%) for a net cost of $%d; threshold +15%%"
                        sold.Name
                        bought
                        u.Before
                        u.After
                        ((u.After / u.Before - 1.0) * 100.0)
                        u.Net

                let candidates = upgrades |> List.truncate 5 |> List.map candidate
                SellJoker u.Sell, decision seq g (sprintf "sell %s to buy %s" sold.Name bought) rule candidates None
            | None ->
                let order = Shop.bestOrder seed 8 build

                if order <> [ 0 .. build.Jokers.Length - 1 ] then
                    let names = order |> List.map (fun i -> build.Jokers[i].Name) |> String.concat ", "
                    let score order = Shop.roundScore seed 8 { build with Jokers = order |> List.map (fun i -> build.Jokers[i]) }

                    ReorderJokers order,
                    decision
                        seq
                        g
                        (sprintf "reorder jokers to [%s]" names)
                        (sprintf
                            "reordering raises the simulated round score from %.0f to %.0f"
                            (score [ 0 .. build.Jokers.Length - 1 ])
                            (score order))
                        []
                        None
                else
                    let slotFree = build.Jokers.Length < build.Situation.JokerSlots
                    let celestial = cheapestPack g "p_celestial"

                    match (if slotFree then cheapestPack g "p_buffoon" |> Option.orElse celestial else celestial) with
                    | Some(index, pack) ->
                        let kind =
                            if not slotFree then "the joker slots are full"
                            elif pack.Key.StartsWith "p_buffoon" then "a joker slot is free"
                            else "a joker slot is free but no Buffoon pack is affordable"

                        BuyPack index,
                        decision
                            seq
                            g
                            (sprintf "buy %s for $%d" pack.Label pack.Cost.Buy)
                            (sprintf
                                "%s; $%d left after the $%d pack stays at or above the $%d interest cap"
                                kind
                                (g.Money - pack.Cost.Buy)
                                pack.Cost.Buy
                                interestFloor)
                            []
                            None
                    | None when g.Money - g.Round.RerollCost >= interestFloor && g.Round.RerollCost <= 7 ->
                        Reroll,
                        decision
                            seq
                            g
                            (sprintf "reroll the shop for $%d" g.Round.RerollCost)
                            (sprintf
                                "$%d left after the $%d reroll stays at or above the $%d interest cap, and the reroll costs at most $7"
                                (g.Money - g.Round.RerollCost)
                                g.Round.RerollCost
                                interestFloor)
                            []
                            None
                    | None ->
                        let rule =
                            if evaluated.IsEmpty then
                                let slotsFull = offers |> List.exists (function Shop.JokerOffer(_, _, cost) -> cost <= g.Money | _ -> false)
                                if slotsFull then "joker slots are full" else "nothing affordable in the shop"
                            else
                                "nothing in the shop raises the simulated round score enough"

                        passive seq g NextRound "next round" rule

let private packAction seq (g: Api.GameState) =
    let build = shopBuild g

    let cards =
        g.Pack.Cards
        |> List.indexed
        |> List.choose (fun (i, c) ->
            match c.Set with
            | Api.CardSet.Joker -> Some(Shop.JokerOffer(i, Mapping.joker c, 0))
            | Api.CardSet.Planet -> planetHand c.Key |> Option.map (fun hand -> Shop.PlanetOffer(i, hand, c.Label, 0))
            | _ -> None)

    // the mod never answers a pack call when the first card of the pack is a Spectral card
    let unsafeFirst =
        match g.Pack.Cards with
        | first :: _ -> first.Set <> Api.CardSet.Joker && first.Set <> Api.CardSet.Planet
        | [] -> false

    match (if unsafeFirst then None else Shop.pickFromPack (shopSeed g) 20 build cards) with
    | Some e ->
        let name = offerName e.Offer

        PickPack(Shop.position e.Offer),
        decision
            seq
            g
            (sprintf "pick %s" name)
            (sprintf
                "raises the simulated round score from %.0f to %.0f (+%.0f%%)"
                e.Before
                e.After
                ((e.After / e.Before - 1.0) * 100.0))
            []
            None
    | None ->
        let rule =
            if unsafeFirst then
                "the first card of the pack is one this version cannot handle safely"
            elif cards.IsEmpty then
                "the pack holds cards this version does not evaluate"
            else
                "nothing in the pack raises the simulated round score"

        passive seq g SkipPack "skip pack" rule

let decide (seq: int) (g: Api.GameState) : Action * Decision =
    match g.State with
    | Api.State.SelectingHand -> chooseAction seq g
    | Api.State.BlindSelect ->
        let chosen =
            g.Blinds
            |> Option.bind (fun b ->
                [ b.Small; b.Big; b.Boss ] |> List.tryFind (fun x -> x.Status = Api.BlindStatus.Select))
            |> Option.map (fun b ->
                let effect = if b.Effect = "" then "" else ": " + b.Effect
                sprintf "select %s (target %d)%s" b.Name b.Score effect)
            |> Option.defaultValue "select blind"

        passive seq g SelectBlind chosen "skeleton policy only selects the blind in this phase"
    | Api.State.RoundEval -> passive seq g CashOut "cash out" "skeleton policy only cashes out in this phase"
    | Api.State.Shop -> shopAction seq g
    | Api.State.SmodsBoosterOpened -> packAction seq g
    | Api.State.GameOver -> passive seq g Stop "stop" "game is over"
    | other -> passive seq g Stop "stop" (sprintf "unexpected state %A, skeleton policy stops" other)
