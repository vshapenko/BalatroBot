module Balatro.Core.Scoring

open Balatro.Core.Domain
open Balatro.Core.Hands
open Balatro.Core.Jokers

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

// None marks a retrigger: it shows in the steps but changes nothing
type private Entry = string * Effect option

// A union's ToString goes through reflection-based formatting, far slower than a match
let private suitLabel suit =
    match suit with
    | Spades -> "Spades"
    | Hearts -> "Hearts"
    | Clubs -> "Clubs"
    | Diamonds -> "Diamonds"

let private cardLabel (card: Card) =
    $"{Rank.label card.Rank} of {suitLabel card.Suit}"

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

let private gains (effects: (string * Effect) list) : Entry list =
    effects |> List.map (fun (source, effect) -> source, Some effect)

// The slot whose logic runs at slot i: itself, or what a Blueprint or Brainstorm chain reaches
let private resolve (jokers: Joker array) (slot: int) : int option =
    let rec go depth i =
        let copy target =
            if target = i || target >= jokers.Length || depth + 1 > jokers.Length + 1 then
                None
            else
                go (depth + 1) target

        match jokers[i] with
        | joker when joker.Debuffed -> None
        | joker when joker.Key = "j_blueprint" -> copy (i + 1)
        | joker when joker.Key = "j_brainstorm" -> copy 0
        | _ -> Some i

    go 0 slot

let private applyEntry (chips, mult, steps) ((source, effect): Entry) =
    let chips, mult =
        match effect with
        | Some(AddChips amount) -> chips + amount, mult
        | Some(AddMult amount) -> chips, mult + amount
        | Some(TimesMult factor) -> chips, mult * factor
        | None -> chips, mult

    chips, mult, { Source = source; Chips = chips; Mult = mult } :: steps

let scoreWith (situation: Situation) (input: ScoreInput) : ScoreBreakdown =
    let rules = JokerRegistry.rules situation.Jokers
    let handType, classified, containedTypes = classifyContained rules input.Played

    let thisRound t =
        match Map.tryFind t situation.HandPlays with
        | Some(_, n) -> n
        | None -> 0

    let notAllowed =
        match situation.Boss with
        | Some Psychic when input.Played.Length < 5 -> Some "Not allowed (The Psychic)"
        | Some Eye when thisRound handType >= 1 -> Some "Not allowed (The Eye)"
        | Some Mouth when thisRound handType = 0 && situation.HandPlays |> Map.exists (fun _ (_, n) -> n >= 1) ->
            Some "Not allowed (The Mouth)"
        | _ -> None

    match notAllowed with
    | Some source ->
        { HandType = handType
          ScoringCards = classified
          Chips = 0.0
          Mult = 0.0
          Total = 0.0
          Steps = [ { Source = source; Chips = 0.0; Mult = 0.0 } ] }
    | None ->
        let level = input.Levels[handType]
        let jokers = Array.ofList situation.Jokers

        let opening: Context =
            { Played = input.Played
              Scoring = (if rules.Splash then input.Played else classified)
              Seen = (if rules.Splash then input.Played else classified)
              Held = input.Held
              HandType = handType
              Contained = containedTypes
              Rules = rules
              Situation = situation }

        // Each joker's before step sees the cards its predecessors left; copies do not run it
        let scoring, seenByJoker =
            jokers
            |> Array.indexed
            |> Array.fold
                (fun (cards, seen) (i, joker) ->
                    if joker.Debuffed then
                        cards, seen
                    else
                        let def = JokerRegistry.find joker.Key
                        def.BeforeCards { opening with Scoring = cards; Seen = cards } joker cards, Map.add i cards seen)
                (opening.Scoring, Map.empty)

        let context = { opening with Scoring = scoring; Seen = scoring }

        let actors =
            jokers
            |> Array.indexed
            |> Array.toList
            |> List.choose (fun (i, slot) ->
                resolve jokers i
                |> Option.map (fun target -> slot.Name, jokers[target], JokerRegistry.find jokers[target].Key))

        let withRetriggers (markers: string list) (pass: Entry list) : Entry list =
            pass @ (markers |> List.collect (fun marker -> (marker, None) :: pass))

        let playedEntries =
            scoring
            |> List.indexed
            |> List.collect (fun (position, card) ->
                if card.Debuffed then
                    []
                else
                    let fromJokers =
                        actors
                        |> List.collect (fun (name, joker, def) ->
                            def.OnScored context joker position card
                            |> List.map (fun effect -> $"{name} ({cardLabel card})", Some effect))

                    let markers =
                        [ if card.Seal = Some Seal.Red then
                              yield "Red Seal retrigger"
                          for name, joker, def in actors do
                              for _ in 1 .. def.Retriggers context joker position card -> name ]

                    withRetriggers markers (gains (playedEffects card) @ fromJokers))

        let heldEntries =
            input.Held
            |> List.collect (fun card ->
                let own = if card.Debuffed then [] else heldEffects card

                let fromJokers =
                    actors
                    |> List.collect (fun (name, joker, def) ->
                        def.OnHeld context joker card
                        |> List.map (fun effect -> $"{name} ({cardLabel card})", Some effect))

                let pass = gains own @ fromJokers

                // Retriggers apply only when the first pass produced an effect
                if pass.IsEmpty then
                    []
                else
                    let markers =
                        [ if card.Seal = Some Seal.Red && not card.Debuffed then
                              yield "Red Seal retrigger"
                          for name, joker, def in actors do
                              for _ in 1 .. def.HeldRetriggers context joker card -> name ]

                    withRetriggers markers pass)

        let jokerEntries =
            jokers
            |> Array.indexed
            |> Array.toList
            |> List.collect (fun (i, slot) ->
                let edition =
                    match slot.Edition with
                    | Some Foil when not slot.Debuffed -> [ AddChips 50.0 ]
                    | Some Holo when not slot.Debuffed -> [ AddMult 10.0 ]
                    | _ -> []

                let own =
                    match resolve jokers i with
                    | Some target ->
                        let def = JokerRegistry.find jokers[target].Key
                        def.Main { context with Seen = seenByJoker[target] } jokers[target]
                    | None -> []

                // Baseball Card reacts to every Uncommon joker other than itself, debuffed or not
                let offered =
                    [ if JokerRegistry.uncommon.Contains slot.Key then
                          for v, offerer in Array.indexed jokers do
                              match resolve jokers v with
                              | Some target when jokers[target].Key = "j_baseball" && target <> i ->
                                  yield offerer.Name, Some(TimesMult 1.5)
                              | _ -> () ]

                let polychrome =
                    if slot.Edition = Some Polychrome && not slot.Debuffed then
                        [ TimesMult 1.5 ]
                    else
                        []

                (edition @ own |> List.map (fun effect -> slot.Name, Some effect))
                @ offered
                @ (polychrome |> List.map (fun effect -> slot.Name, Some effect)))

        let start =
            let baseStep = { Source = HandType.label handType; Chips = level.Chips; Mult = level.Mult }

            match situation.Boss with
            | Some Flint ->
                let mult = max (floor (level.Mult * 0.5 + 0.5)) 1.0
                let chips = max (floor (level.Chips * 0.5 + 0.5)) 0.0
                chips, mult, [ { Source = "The Flint"; Chips = chips; Mult = mult }; baseStep ]
            | _ -> level.Chips, level.Mult, [ baseStep ]

        let chips, mult, steps =
            let chips, mult, steps = List.fold applyEntry start (playedEntries @ heldEntries @ jokerEntries)

            if situation.Balance then
                let balanced = floor ((chips + mult) / 2.0)
                balanced, balanced, { Source = "Plasma Deck"; Chips = balanced; Mult = balanced } :: steps
            else
                chips, mult, steps

        { HandType = handType
          ScoringCards = scoring
          Chips = chips
          Mult = mult
          Total = floor (chips * mult)
          Steps = List.rev steps }

let score (input: ScoreInput) : ScoreBreakdown = scoreWith Situation.empty input
