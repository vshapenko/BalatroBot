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

let private shopAction seq (g: Api.GameState) =
    match g.Consumables.Cards |> List.tryFindIndex (fun c -> c.Set = Api.CardSet.Planet) with
    | Some index ->
        let name = g.Consumables.Cards[index].Label
        UsePlanet index, decision seq g ("use " + name) "use the planet bought in the shop" [] None
    | None ->
        let situation = Mapping.situation g

        let build: Shop.Build =
            { Jokers = situation.Jokers
              Levels = Mapping.levels g
              Deck = g.Cards.Cards |> List.choose Mapping.toCard
              HandSize = g.Hand.Limit
              Hands = g.Round.HandsLeft
              Situation = situation }

        let offers =
            g.Shop.Cards
            |> List.indexed
            |> List.choose (fun (i, c) ->
                match c.Set with
                | Api.CardSet.Joker -> Some(Shop.JokerOffer(i, Mapping.joker c, c.Cost.Buy))
                | Api.CardSet.Planet when g.Consumables.Count < g.Consumables.Limit ->
                    planetHand c.Key |> Option.map (fun hand -> Shop.PlanetOffer(i, hand, c.Label, c.Cost.Buy))
                | _ -> None)

        let seed = seq * 100
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
            let order = Shop.bestOrder seed 20 build

            if order <> [ 0 .. build.Jokers.Length - 1 ] then
                let names = order |> List.map (fun i -> build.Jokers[i].Name) |> String.concat ", "

                ReorderJokers order,
                decision seq g (sprintf "reorder jokers to [%s]" names) "joker order with the highest simulated round score" [] None
            elif evaluated.IsEmpty then
                passive seq g NextRound "next round" "nothing affordable in the shop"
            else
                passive seq g NextRound "next round" "nothing in the shop raises the simulated round score enough"

let decide (seq: int) (g: Api.GameState) : Action * Decision =
    match g.State with
    | Api.State.SelectingHand -> chooseAction seq g
    | Api.State.BlindSelect ->
        passive seq g SelectBlind "select blind" "skeleton policy only selects the blind in this phase"
    | Api.State.RoundEval -> passive seq g CashOut "cash out" "skeleton policy only cashes out in this phase"
    | Api.State.Shop -> shopAction seq g
    | Api.State.SmodsBoosterOpened -> passive seq g SkipPack "skip pack" "skeleton policy skips every pack"
    | Api.State.GameOver -> passive seq g Stop "stop" "game is over"
    | other -> passive seq g Stop "stop" (sprintf "unexpected state %A, skeleton policy stops" other)
