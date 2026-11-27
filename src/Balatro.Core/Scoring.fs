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

// A union's ToString goes through reflection-based formatting, far slower than a match
let private suitLabel suit =
    match suit with
    | Spades -> "Spades"
    | Hearts -> "Hearts"
    | Clubs -> "Clubs"
    | Diamonds -> "Diamonds"

let private suitSlot suit =
    match suit with
    | Spades -> 0
    | Hearts -> 1
    | Clubs -> 2
    | Diamonds -> 3

let private cardLabels =
    [| for rank in [ Two; Three; Four; Five; Six; Seven; Eight; Nine; Ten; Jack; Queen; King; Ace ] do
           for suit in [ Spades; Hearts; Clubs; Diamonds ] -> $"{Rank.label rank} of {suitLabel suit}" |]

let private labelSlot (card: Card) =
    (Rank.id card.Rank - 2) * 4 + suitSlot card.Suit

let private cardLabel (card: Card) = cardLabels[labelSlot card]

let private handSlot handType =
    match handType with
    | FlushFive -> 0
    | FlushHouse -> 1
    | FiveOfAKind -> 2
    | StraightFlush -> 3
    | FourOfAKind -> 4
    | FullHouse -> 5
    | Flush -> 6
    | Straight -> 7
    | ThreeOfAKind -> 8
    | TwoPair -> 9
    | Pair -> 10
    | HighCard -> 11

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

// The running chips, mult and steps
type private Acc =
    { mutable TotalChips: float
      mutable TotalMult: float
      mutable Trail: ScoreStep list
      mutable Pushed: int }

let private push (acc: Acc) (source: string) (effect: Effect) =
    match effect with
    | AddChips amount -> acc.TotalChips <- acc.TotalChips + amount
    | AddMult amount -> acc.TotalMult <- acc.TotalMult + amount
    | TimesMult factor -> acc.TotalMult <- acc.TotalMult * factor

    acc.Pushed <- acc.Pushed + 1

    acc.Trail <-
        { Source = source
          Chips = acc.TotalChips
          Mult = acc.TotalMult }
        :: acc.Trail

// A retrigger shows in the steps but changes nothing
let private retrigger (acc: Acc) (source: string) =
    acc.Trail <-
        { Source = source
          Chips = acc.TotalChips
          Mult = acc.TotalMult }
        :: acc.Trail

type private Actor =
    { ActorName: string
      ActorJoker: Joker
      Logic: JokerDef
      HasScored: bool
      HasRetriggers: bool
      HasHeld: bool
      HasHeldRetriggers: bool
      Labels: string[] }

let private jokerLabel (actor: Actor) (card: Card) =
    let slot = labelSlot card

    match actor.Labels[slot] with
    | null ->
        let label = $"{actor.ActorName} ({cardLabel card})"
        actor.Labels[slot] <- label
        label
    | label -> label

// A hook still holding the default of JokerDef.none gives nothing and is never called
let private custom (field: 'a) (standard: 'a) = not (obj.ReferenceEquals(field, standard))

let private playedPass (actors: Actor[]) (acc: Acc) (context: Context) (card: Card) (position: int) =
    match card.Enhancement with
    | Some Enhancement.Stone -> push acc "Stone" (AddChips 50.0)
    | Some Enhancement.Bonus -> push acc (cardLabel card) (AddChips(float (Rank.nominal card.Rank) + 30.0))
    | _ -> push acc (cardLabel card) (AddChips(float (Rank.nominal card.Rank)))

    match card.Enhancement with
    | Some Enhancement.Mult -> push acc "Mult" (AddMult 4.0)
    | Some Enhancement.Glass -> push acc "Glass" (TimesMult 2.0)
    | _ -> ()

    match card.Edition with
    | Some Foil -> push acc "Foil" (AddChips 50.0)
    | Some Holo -> push acc "Holo" (AddMult 10.0)
    | Some Polychrome -> push acc "Polychrome" (TimesMult 1.5)
    | _ -> ()

    for actor in actors do
        if actor.HasScored then
            match actor.Logic.OnScored context actor.ActorJoker position card with
            | [] -> ()
            | effects ->
                let source = jokerLabel actor card

                for effect in effects do
                    push acc source effect

let private heldPass (actors: Actor[]) (acc: Acc) (context: Context) (card: Card) =
    if not card.Debuffed then
        match card.Enhancement with
        | Some Enhancement.Steel -> push acc "Steel (held)" (TimesMult 1.5)
        | _ -> ()

    for actor in actors do
        if actor.HasHeld then
            match actor.Logic.OnHeld context actor.ActorJoker card with
            | [] -> ()
            | effects ->
                let source = jokerLabel actor card

                for effect in effects do
                    push acc source effect

// What depends on the situation and the levels alone is worked out here, once
let private prepareOnce
    (situation: Situation)
    (levels: Map<HandType, HandLevel>)
    : Card list -> Card list -> ScoreBreakdown
    =
    let rules = JokerRegistry.rules situation.Jokers
    let jokers = Array.ofList situation.Jokers
    let defs = jokers |> Array.map (fun joker -> JokerRegistry.find joker.Key)
    let targets = Array.init jokers.Length (fun i -> resolve jokers i |> Option.defaultValue (-1))
    let beforeCards = defs |> Array.map (fun def -> custom def.BeforeCards JokerDef.none.BeforeCards)
    let hasMain = targets |> Array.map (fun t -> t >= 0 && custom defs[t].Main JokerDef.none.Main)

    let actors =
        [| for i in 0 .. jokers.Length - 1 do
               let t = targets[i]

               if t >= 0 then
                   let def = defs[t]

                   yield
                       { ActorName = jokers[i].Name
                         ActorJoker = jokers[t]
                         Logic = def
                         HasScored = custom def.OnScored JokerDef.none.OnScored
                         HasRetriggers = custom def.Retriggers JokerDef.none.Retriggers
                         HasHeld = custom def.OnHeld JokerDef.none.OnHeld
                         HasHeldRetriggers = custom def.HeldRetriggers JokerDef.none.HeldRetriggers
                         Labels = Array.zeroCreate 52 } |]

    let edition =
        jokers
        |> Array.map (fun slot ->
            match slot.Edition with
            | Some Foil when not slot.Debuffed -> [| AddChips 50.0 |]
            | Some Holo when not slot.Debuffed -> [| AddMult 10.0 |]
            | _ -> [||])

    // Baseball Card reacts to every Uncommon joker other than itself, debuffed or not
    let trailing =
        jokers
        |> Array.mapi (fun i slot ->
            [| if JokerRegistry.uncommon.Contains slot.Key then
                   for v, offerer in Array.indexed jokers do
                       match resolve jokers v with
                       | Some target when jokers[target].Key = "j_baseball" && target <> i ->
                           yield offerer.Name, TimesMult 1.5
                       | _ -> ()

               if slot.Edition = Some Polychrome && not slot.Debuffed then
                   yield slot.Name, TimesMult 1.5 |])

    // read from the map on first use, as a hand type missing from it must still raise at the read
    let levelSlots: HandLevel[] = Array.zeroCreate 12

    fun (played: Card list) (held: Card list) ->
        let handType, classified, containedTypes = classifyContained rules played

        let thisRound t =
            match Map.tryFind t situation.HandPlays with
            | Some(_, n) -> n
            | None -> 0

        let notAllowed =
            match situation.Boss with
            | Some Psychic when played.Length < 5 -> Some "Not allowed (The Psychic)"
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
            let level =
                let cached = levelSlots[handSlot handType]

                if obj.ReferenceEquals(cached, null) then
                    let level = levels[handType]
                    levelSlots[handSlot handType] <- level
                    level
                else
                    cached

            let opening: Context =
                { Played = played
                  Scoring = (if rules.Splash then played else classified)
                  Seen = (if rules.Splash then played else classified)
                  Held = held
                  HandType = handType
                  Contained = containedTypes
                  Rules = rules
                  Situation = situation }

            // Each joker's before step sees the cards its predecessors left; copies do not run it
            let seen: Card list[] = Array.zeroCreate jokers.Length
            let mutable scoring = opening.Scoring

            for i in 0 .. jokers.Length - 1 do
                if not jokers[i].Debuffed then
                    seen[i] <- scoring

                    if beforeCards[i] then
                        let before = { opening with Scoring = scoring; Seen = scoring }
                        scoring <- defs[i].BeforeCards before jokers[i] scoring

            let context = { opening with Scoring = scoring; Seen = scoring }

            let baseStep =
                { Source = HandType.label handType
                  Chips = level.Chips
                  Mult = level.Mult }

            let acc =
                match situation.Boss with
                | Some Flint ->
                    let mult = max (floor (level.Mult * 0.5 + 0.5)) 1.0
                    let chips = max (floor (level.Chips * 0.5 + 0.5)) 0.0

                    { TotalChips = chips
                      TotalMult = mult
                      Trail = [ { Source = "The Flint"; Chips = chips; Mult = mult }; baseStep ]
                      Pushed = 0 }
                | _ ->
                    { TotalChips = level.Chips
                      TotalMult = level.Mult
                      Trail = [ baseStep ]
                      Pushed = 0 }

            let mutable position = 0

            for card in scoring do
                if not card.Debuffed then
                    playedPass actors acc context card position

                    match card.Seal with
                    | Some Seal.Red ->
                        retrigger acc "Red Seal retrigger"
                        playedPass actors acc context card position
                    | _ -> ()

                    for actor in actors do
                        if actor.HasRetriggers then
                            for _ in 1 .. actor.Logic.Retriggers context actor.ActorJoker position card do
                                retrigger acc actor.ActorName
                                playedPass actors acc context card position

                position <- position + 1

            for card in held do
                let before = acc.Pushed
                heldPass actors acc context card

                // Retriggers apply only when the first pass produced an effect
                if acc.Pushed > before then
                    match card.Seal with
                    | Some Seal.Red when not card.Debuffed ->
                        retrigger acc "Red Seal retrigger"
                        heldPass actors acc context card
                    | _ -> ()

                    for actor in actors do
                        if actor.HasHeldRetriggers then
                            for _ in 1 .. actor.Logic.HeldRetriggers context actor.ActorJoker card do
                                retrigger acc actor.ActorName
                                heldPass actors acc context card

            for i in 0 .. jokers.Length - 1 do
                let name = jokers[i].Name

                for effect in edition[i] do
                    push acc name effect

                if hasMain[i] then
                    let t = targets[i]
                    let seenCards = seen[t]

                    let own =
                        if obj.ReferenceEquals(seenCards, context.Seen) then
                            context
                        else
                            { context with Seen = seenCards }

                    for effect in defs[t].Main own jokers[t] do
                        push acc name effect

                for source, effect in trailing[i] do
                    push acc source effect

            let chips, mult, steps =
                if situation.Balance then
                    let balanced = floor ((acc.TotalChips + acc.TotalMult) / 2.0)
                    balanced, balanced, { Source = "Plasma Deck"; Chips = balanced; Mult = balanced } :: acc.Trail
                else
                    acc.TotalChips, acc.TotalMult, acc.Trail

            { HandType = handType
              ScoringCards = scoring
              Chips = chips
              Mult = mult
              Total = floor (chips * mult)
              Steps = List.rev steps }

let rec private choose k items =
    match k, items with
    | 0, _ -> [ [] ]
    | _, [] -> []
    | _, x :: rest -> (choose (k - 1) rest |> List.map (fun picked -> x :: picked)) @ choose k rest

// The Hook discards 2 random held cards before scoring; the bot cannot know which, so it scores the mean
let private prepare (situation: Situation) (levels: Map<HandType, HandLevel>) : Card list -> Card list -> ScoreBreakdown =
    match situation.Boss with
    | Some Hook ->
        let without = prepareOnce { situation with Boss = None } levels
        let whole = lazy (prepareOnce situation levels)

        fun played held ->
            if held.IsEmpty then
                whole.Value played held
            else
                let results =
                    choose (min 2 held.Length) [ 0 .. held.Length - 1 ]
                    |> List.map (fun discarded ->
                        let kept =
                            held
                            |> List.indexed
                            |> List.filter (fun (i, _) -> not (List.contains i discarded))
                            |> List.map snd

                        without played kept)

                let first = results.Head
                let mean = floor (List.sumBy (fun r -> r.Total) results / float results.Length)
                let source = $"The Hook (mean of {results.Length} discards)"
                let step = { Source = source; Chips = first.Chips; Mult = first.Mult }
                { first with Total = mean; Steps = first.Steps @ [ step ] }
    | _ -> prepareOnce situation levels

type private Prepared =
    { For: Situation
      Under: Map<HandType, HandLevel>
      Scorer: Card list -> Card list -> ScoreBreakdown }

let private lastPrepared = new System.Threading.ThreadLocal<Prepared>()

// Every field of Jokers.Situation must be compared here; the lists and maps are immutable, so one instance is one value
let private sameSituation (a: Situation) (b: Situation) =
    obj.ReferenceEquals(a, b)
    || (obj.ReferenceEquals(a.Jokers, b.Jokers)
        && obj.ReferenceEquals(a.HandPlays, b.HandPlays)
        && a.HandsLeft = b.HandsLeft
        && a.DiscardsLeft = b.DiscardsLeft
        && a.Money = b.Money
        && a.DeckCount = b.DeckCount
        && a.JokerSlots = b.JokerSlots
        && a.Boss = b.Boss
        && a.Balance = b.Balance)

// Calls that keep the situation and levels, with only the cards changing, share one preparation per thread
let scoreWith (situation: Situation) (input: ScoreInput) : ScoreBreakdown =
    let last = lastPrepared.Value

    if not (obj.ReferenceEquals(last, null))
       && obj.ReferenceEquals(last.Under, input.Levels)
       && sameSituation last.For situation then
        last.Scorer input.Played input.Held
    else
        let scorer = prepare situation input.Levels
        lastPrepared.Value <- { For = situation; Under = input.Levels; Scorer = scorer }
        scorer input.Played input.Held

let score (input: ScoreInput) : ScoreBreakdown = scoreWith Situation.empty input
