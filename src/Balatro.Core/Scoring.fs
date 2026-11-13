module Balatro.Core.Scoring

open Balatro.Core.Domain
open Balatro.Core.Hands

type ScoreInput =
    { Played: Card list
      Held: Card list
      Levels: Map<HandType, HandLevel> }

type ScoreStep =
    { Source: string
      Chips: float
      Mult: float }

type ScoreBreakdown =
    { HandType: HandType
      ScoringCards: Card list
      Chips: float
      Mult: float
      Total: float
      Steps: ScoreStep list }

type private Effect =
    | AddChips of float
    | AddMult of float
    | TimesMult of float
    | Retrigger

let private cardLabel (card: Card) =
    $"{Rank.label card.Rank} of {card.Suit}"

let private playedEffects (card: Card) =
    let chips =
        match card.Enhancement with
        | Some Enhancement.Stone -> "Stone", 50.0
        | Some Enhancement.Bonus -> cardLabel card, float (Rank.nominal card.Rank) + 30.0
        | _ -> cardLabel card, float (Rank.nominal card.Rank)

    [ yield fst chips, AddChips(snd chips)

      match card.Enhancement with
      | Some Enhancement.Mult -> yield "Mult", AddMult 4.0
      | Some Enhancement.Glass -> yield "Glass", TimesMult 2.0
      | _ -> ()

      match card.Edition with
      | Some Foil -> yield "Foil", AddChips 50.0
      | Some Holo -> yield "Holo", AddMult 10.0
      | Some Polychrome -> yield "Polychrome", TimesMult 1.5
      | _ -> () ]

let private heldEffects (card: Card) =
    match card.Enhancement with
    | Some Enhancement.Steel -> [ "Steel (held)", TimesMult 1.5 ]
    | _ -> []

// A debuffed card yields nothing, and so does its seal
let private withRetriggers (effectsOf: Card -> (string * Effect) list) (card: Card) =
    let effects = if card.Debuffed then [] else effectsOf card

    match card.Seal with
    | Some Seal.Red when not effects.IsEmpty -> effects @ [ "Red Seal retrigger", Retrigger ] @ effects
    | _ -> effects

let private applyEffect (chips, mult, steps) (source, effect) =
    let chips, mult =
        match effect with
        | AddChips amount -> chips + amount, mult
        | AddMult amount -> chips, mult + amount
        | TimesMult factor -> chips, mult * factor
        | Retrigger -> chips, mult

    chips, mult, { Source = source; Chips = chips; Mult = mult } :: steps

let score (input: ScoreInput) : ScoreBreakdown =
    let handType, scoringCards = classify input.Played
    let level = input.Levels[handType]

    let effects =
        (scoringCards |> List.collect (withRetriggers playedEffects))
        @ (input.Held |> List.collect (withRetriggers heldEffects))

    let start =
        level.Chips, level.Mult, [ { Source = HandType.label handType; Chips = level.Chips; Mult = level.Mult } ]

    let chips, mult, steps = List.fold applyEffect start effects

    { HandType = handType
      ScoringCards = scoringCards
      Chips = chips
      Mult = mult
      Total = floor (chips * mult)
      Steps = List.rev steps }
