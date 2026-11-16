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

let private currentBlind (g: Api.GameState) =
    [ g.Blinds.Value.Small; g.Blinds.Value.Big; g.Blinds.Value.Boss ]
    |> List.find (fun b -> b.Status = Api.BlindStatus.Current)

let private chooseAction seq (g: Api.GameState) =
    let levels = Mapping.levels g

    let hand =
        g.Hand.Cards
        |> List.indexed
        |> List.choose (fun (i, c) -> Mapping.toCard c |> Option.map (fun card -> i, card))

    let blind = currentBlind g

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

let private lastAnte (g: Api.GameState) = g.AnteNum >= 8

let private interestFloor relaxed = if relaxed then 0 else 25

let private stickers (card: Api.Card) =
    [ if card.Modifier.Rental then "Rental", 3
      if card.Modifier.Perishable.IsSome then "Perishable", 2 ]

let stickerLoss (card: Api.Card) = stickers card |> List.sumBy snd

let private stickerText (card: Api.Card) =
    stickers card
    |> List.map (fun (name, loss) -> sprintf "%s sticker adds %d%%" name (5 * loss))
    |> String.concat ", "

let private liftVerdantLeaf seq (g: Api.GameState) : (Action * Decision) option =
    let sellable =
        g.Jokers.Cards |> List.indexed |> List.filter (fun (_, c) -> not c.Modifier.Eternal)

    let debuffed = not g.Hand.Cards.IsEmpty && g.Hand.Cards |> List.forall (fun c -> c.State.Debuff)

    if (currentBlind g).Name = "Verdant Leaf" && debuffed && not sellable.IsEmpty then
        let build = shopBuild g
        let clear = { build with Deck = build.Deck |> List.map (fun c -> { c with Debuffed = false }) }

        let scored =
            sellable
            |> List.map (fun (i, c) ->
                i, c.Label, Shop.roundScore (shopSeed g) 8 { clear with Jokers = List.removeAt i clear.Jokers })
            |> List.sortBy (fun (i, _, score) -> -score, i)

        let candidate (_, name, score) : Candidate =
            { Action = sprintf "sell %s" name
              Value = score
              Detail = sprintf "round score %.0f without %s" score name }

        let index, name, score = scored.Head

        Some(
            SellJoker index,
            decision
                seq
                g
                (sprintf "sell %s to lift Verdant Leaf" name)
                (sprintf
                    "Verdant Leaf debuffs all cards until a joker is sold; selling %s keeps the simulated round score at %.0f"
                    name
                    score)
                (scored |> List.truncate 5 |> List.map candidate)
                None
        )
    else
        None

let private cheapestPack (g: Api.GameState) (floor: int) (prefix: string) =
    g.Packs.Cards
    |> List.indexed
    |> List.filter (fun (_, c) -> c.Key.StartsWith prefix && g.Money - c.Cost.Buy >= floor)
    |> List.sortBy (fun (i, c) -> c.Cost.Buy, i)
    |> List.tryHead

let private thresholdText (card: Api.Card) (percent: float) =
    if (stickers card).IsEmpty then
        sprintf "threshold +%.0f%%" percent
    else
        sprintf "threshold +%.0f%% (%s)" percent (stickerText card)

let private shopAction seq (g: Api.GameState) =
    match g.Consumables.Cards |> List.tryFindIndex (fun c -> c.Set = Api.CardSet.Planet) with
    | Some index ->
        let name = g.Consumables.Cards[index].Label
        UsePlanet index, decision seq g ("use " + name) "use the planet bought in the shop" [] None
    | None ->
        let build = shopBuild g

        let owned = build.Jokers.Length
        let early = owned < 3

        let behindScore =
            g.Blinds
            |> Option.filter (fun _ -> not early)
            |> Option.map (fun b -> Shop.roundScore (shopSeed g) 20 (Shop.revalue build), float b.Boss.Score)
            |> Option.filter (fun (score, target) -> score < 1.5 * target)

        let behind = behindScore.IsSome
        let relaxed = lastAnte g || behind

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

        let relaxedText =
            match behindScore with
            | Some(score, target) when not (lastAnte g) ->
                sprintf
                    "the build is behind: simulated round score %.0f is under 1.5x the boss target %.0f; interest no longer matters"
                    score
                    target
            | _ -> "interest no longer matters in the last ante"

        let lastNote = if relaxed then "; " + relaxedText else ""
        let earlyNote = if early && not (lastAnte g) then "; fewer than 3 jokers owned: interest not counted" else ""

        let offered (offer: Shop.Offer) = g.Shop.Cards[Shop.position offer]

        let evaluated =
            Shop.evaluate seed 20 g.Money build offers
            |> List.map (fun e ->
                { e with
                    InterestLost = (if relaxed || early then 0 else e.InterestLost) + stickerLoss (offered e.Offer) })

        let percent (e: Shop.Evaluated) = (e.After / e.Before - 1.0) * 100.0
        let needed (e: Shop.Evaluated) = (Shop.threshold owned e - 1.0) * 100.0

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
                    (e.InterestLost - stickerLoss (offered e.Offer)) }

        match Shop.choose owned evaluated with
        | Some e ->
            let others = evaluated |> List.filter (fun o -> Shop.position o.Offer <> Shop.position e.Offer)
            let candidates = e :: others |> List.truncate 5 |> List.map candidate

            let rule =
                sprintf
                    "raises the simulated round score from %.0f to %.0f (+%.0f%%); %s%s"
                    e.Before
                    e.After
                    (percent e)
                    (thresholdText (offered e.Offer) (needed e))
                    (lastNote + earlyNote)

            BuyCard(Shop.position e.Offer), decision seq g candidates.Head.Action rule candidates None
        | None ->
            let upgrades =
                Shop.upgrades seed 20 g.Money build offers
                |> List.filter (fun u -> not g.Jokers.Cards[u.Sell].Modifier.Eternal)
                |> List.map (fun u ->
                    { u with
                        Threshold =
                            (if relaxed then 1.05 else u.Threshold)
                            + 0.05 * float (stickerLoss (offered u.Offer)) })

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
                        "joker slots are full; selling %s to buy %s raises the simulated round score from %.0f to %.0f (+%.0f%%) for a net cost of $%d; %s%s"
                        sold.Name
                        bought
                        u.Before
                        u.After
                        ((u.After / u.Before - 1.0) * 100.0)
                        u.Net
                        (thresholdText (offered u.Offer) (((max 1.15 u.Threshold) - 1.0) * 100.0))
                        lastNote

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
                    let floor = interestFloor relaxed
                    let celestial = cheapestPack g floor "p_celestial"

                    let floorText =
                        if relaxed then
                            relaxedText
                        else
                            sprintf "stays at or above the $%d interest cap" floor

                    match (if slotFree then cheapestPack g floor "p_buffoon" |> Option.orElse celestial else celestial) with
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
                                "%s; $%d left after the $%d pack; %s"
                                kind
                                (g.Money - pack.Cost.Buy)
                                pack.Cost.Buy
                                floorText)
                            []
                            None
                    | None when g.Money - g.Round.RerollCost >= floor && g.Round.RerollCost <= 7 ->
                        Reroll,
                        decision
                            seq
                            g
                            (sprintf "reroll the shop for $%d" g.Round.RerollCost)
                            (sprintf
                                "$%d left after the $%d reroll; %s; the reroll costs at most $7"
                                (g.Money - g.Round.RerollCost)
                                g.Round.RerollCost
                                floorText)
                            []
                            None
                    | None ->
                        match evaluated with
                        | [] ->
                            let slotsFull = offers |> List.exists (function Shop.JokerOffer(_, _, cost) -> cost <= g.Money | _ -> false)
                            let rule = if slotsFull then "joker slots are full" else "nothing affordable in the shop"
                            passive seq g NextRound "next round" rule
                        | best :: _ ->
                            let declined (e: Shop.Evaluated) : Candidate =
                                let name = offerName e.Offer
                                let cost = Shop.cost e.Offer

                                { Action = sprintf "buy %s for $%d" name cost
                                  Value = e.After
                                  Detail =
                                    sprintf
                                        "%s, $%d, round score %.0f -> %.0f (%+.0f%%), needed +%.0f%%"
                                        name
                                        cost
                                        e.Before
                                        e.After
                                        (percent e)
                                        (needed e) }

                            let rule =
                                sprintf
                                    "best offer %s raises the simulated round score by %.0f%%, below the %.0f%% threshold"
                                    (offerName best.Offer)
                                    (percent best)
                                    (needed best)

                            NextRound, decision seq g "next round" rule (evaluated |> List.truncate 5 |> List.map declined) None

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
    | Api.State.SelectingHand -> liftVerdantLeaf seq g |> Option.defaultWith (fun () -> chooseAction seq g)
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
