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

    let input: Search.Input =
        { Hand = hand
          Deck = g.Cards.Cards |> List.choose Mapping.toCard
          HandsLeft = g.Round.HandsLeft
          HandSize = g.Hand.Limit
          DiscardsLeft = g.Round.DiscardsLeft
          Needed = float (blind.Score - g.Round.Chips)
          Score =
            fun handsLeftAfter played held ->
                Scoring.scoreWith
                    { Mapping.situation g with
                        HandsLeft = handsLeftAfter }
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

let decide (seq: int) (g: Api.GameState) : Action * Decision =
    match g.State with
    | Api.State.SelectingHand -> chooseAction seq g
    | Api.State.BlindSelect ->
        passive seq g SelectBlind "select blind" "skeleton policy only selects the blind in this phase"
    | Api.State.RoundEval -> passive seq g CashOut "cash out" "skeleton policy only cashes out in this phase"
    | Api.State.Shop -> passive seq g NextRound "next round" "skeleton policy leaves the shop without buying"
    | Api.State.SmodsBoosterOpened -> passive seq g SkipPack "skip pack" "skeleton policy skips every pack"
    | Api.State.GameOver -> passive seq g Stop "stop" "game is over"
    | other -> passive seq g Stop "stop" (sprintf "unexpected state %A, skeleton policy stops" other)
