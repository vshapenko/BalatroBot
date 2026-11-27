module Balatro.Bot.SimJokers

open System.Globalization
open System.Text.RegularExpressions
open Balatro.Core.Domain

type Table =
    { Money: int
      Deck: Card list
      DrawPile: int
      JokerSlots: int
      Jokers: string list
      HandPlays: Map<HandType, int>
      Skips: int }

type Event =
    | BlindSelected of boss: bool
    | HandPlayed of hand: HandType * played: Card list * scoring: Card list * held: Card list
    | Discarded of Card list
    | RoundWon of boss: bool * discardsLeft: int
    | Rerolled
    | Sold of key: string
    | ConsumableUsed of set: Api.CardSet
    | CardsDestroyed of Card list
    | CardsAdded of Card list
    | PackSkipped

type State =
    { Card: Api.Card
      Value: float
      Count: int
      Used: int
      Rank: Rank option
      Suit: Suit option
      Hand: HandType option }

let private currentRe = Regex(@"(?<=\(Currently[^\d)]*)\d+(?:\.\d+)?")
let private leadingRe = Regex(@"(?<=^[+X])\d+(?:\.\d+)?")
let private payoutRe = Regex(@"(?<=Earn \$)\d+")
let private counterRe = Regex(@"Active!|\d+ remaining")
let private remainingRe = Regex(@"\d+(?= remaining)")
let private bracketRe = Regex(@"\[\d+\]")
let private castleRe = Regex(@"(?<=discarded )\w+(?= card)")
let private mailRe = Regex(@"(?<=discarded )\w+(?=,)")
let private ancientRe = Regex(@"(?<=with )\w+(?= suit)")
let private idolRe = Regex(@"(?<=played )\w+ of \w+(?= gives)")
let private todoRe = Regex(@"(?<=is a )[A-Za-z ]+(?=,)")

let private flat (value: float) =
    System.Math.Round(value, 2).ToString("0.##", CultureInfo.InvariantCulture)

let private parse (re: Regex) (text: string) =
    let m = re.Match text
    if m.Success then System.Double.Parse(m.Value, CultureInfo.InvariantCulture) else 0.0

let private replace (re: Regex) (value: string) (text: string) = re.Replace(text, value, 1)

let private suitName (suit: Suit) = (string suit).TrimEnd 's'

let private suits = [ Spades; Hearts; Clubs; Diamonds ]

let private handTypes =
    [ FlushFive
      FlushHouse
      FiveOfAKind
      StraightFlush
      FourOfAKind
      FullHouse
      Flush
      Straight
      ThreeOfAKind
      TwoPair
      Pair
      HighCard ]

let private isFace (table: Table) (card: Card) =
    not card.Debuffed
    && (List.contains "j_pareidolia" table.Jokers
        || (card.Enhancement <> Some Enhancement.Stone
            && (card.Rank = Jack || card.Rank = Queen || card.Rank = King)))

let private isSuit (table: Table) (suit: Suit) (card: Card) =
    let isRed suit = suit = Hearts || suit = Diamonds

    not card.Debuffed
    && card.Enhancement <> Some Enhancement.Stone
    && (card.Enhancement = Some Enhancement.Wild
        || (List.contains "j_smeared" table.Jokers && isRed card.Suit = isRed suit)
        || card.Suit = suit)

let private isRank (rank: Rank) (card: Card) =
    not card.Debuffed && card.Enhancement <> Some Enhancement.Stone && card.Rank = rank

let private countOf predicate (cards: Card list) = cards |> List.filter predicate |> List.length

let private sellOf =
    let sells =
        Map [ for info in SimContent.jokers -> info.Key, (SimContent.jokerCard info None).Cost.Sell ]

    fun key -> sells |> Map.tryFind key |> Option.defaultValue 2

let private pickCard (rng: System.Random) (table: Table) =
    match table.Deck |> List.filter (fun card -> card.Enhancement <> Some Enhancement.Stone) with
    | [] -> None
    | cards -> Some cards[rng.Next cards.Length]

// the target a random joker shows is drawn again whenever a round ends
let private roll (rng: System.Random) (table: Table) (state: State) =
    match state.Card.Key with
    | "j_idol" ->
        let drawn = pickCard rng table
        { state with
            Rank = Some(drawn |> Option.map (fun c -> c.Rank) |> Option.defaultValue Ace)
            Suit = Some(drawn |> Option.map (fun c -> c.Suit) |> Option.defaultValue Spades) }
    | "j_mail" ->
        { state with Rank = Some(pickCard rng table |> Option.map (fun c -> c.Rank) |> Option.defaultValue Ace) }
    | "j_castle" ->
        { state with Suit = Some(pickCard rng table |> Option.map (fun c -> c.Suit) |> Option.defaultValue Spades) }
    | "j_ancient" ->
        let others = suits |> List.filter (fun suit -> Some suit <> state.Suit)
        { state with Suit = Some others[rng.Next others.Length] }
    | "j_todo_list" ->
        let seen hand = table.HandPlays |> Map.tryFind hand |> Option.defaultValue 0 > 0

        let visible =
            handTypes
            |> List.filter (fun hand -> hand <> FlushFive && hand <> FlushHouse && hand <> FiveOfAKind || seen hand)
            |> List.filter (fun hand -> Some hand <> state.Hand)

        { state with Hand = Some visible[rng.Next visible.Length] }
    | _ -> state

let create (rng: System.Random) (table: Table) (card: Api.Card) : State =
    let effect = card.Value.Effect

    let value =
        match card.Key with
        | "j_ice_cream"
        | "j_popcorn" -> parse leadingRe effect
        | "j_rocket" -> parse payoutRe effect
        | _ -> parse currentRe effect

    let count =
        match card.Key with
        | "j_ramen" -> int (System.Math.Round((2.0 - parse leadingRe effect) / 0.01))
        | "j_loyalty_card" -> 5 - int (parse remainingRe effect)
        | _ -> 0

    roll
        rng
        table
        { Card = card
          Value = value
          Count = count
          Used = 0
          Rank = None
          Suit = None
          Hand = None }

let step (rng: System.Random) (table: Table) (event: Event) (state: State) : State option * int =
    let chance odds = rng.Next odds = 0
    let keep (next: State) = Some next, 0
    let add amount = keep { state with Value = state.Value + amount }
    let pay (next: State) dollars = Some next, dollars
    let faces = countOf (isFace table)

    let state =
        match event with
        | BlindSelected _ -> { state with Used = 0 }
        | Discarded _ -> { state with Used = state.Used + 1 }
        | RoundWon _ -> roll rng table state
        | _ -> state

    match state.Card.Key, event with
    | "j_green_joker", HandPlayed _ -> add 1.0
    | "j_green_joker", Discarded _ -> keep { state with Value = max 0.0 (state.Value - 1.0) }
    | "j_ride_the_bus", HandPlayed(_, _, scoring, _) ->
        keep { state with Value = if faces scoring > 0 then 0.0 else state.Value + 1.0 }
    | "j_runner", HandPlayed(hand, _, _, _) when hand = Straight || hand = StraightFlush -> add 15.0
    | "j_square", HandPlayed(_, played, _, _) when played.Length = 4 -> add 4.0
    | "j_trousers", HandPlayed(hand, _, _, _) when hand = TwoPair || hand = FullHouse || hand = FlushHouse -> add 2.0
    | "j_obelisk", HandPlayed(hand, _, _, _) ->
        // HandPlays is the count before this hand
        let thisHand = (table.HandPlays |> Map.tryFind hand |> Option.defaultValue 0) + 1

        let mostPlayed =
            table.HandPlays |> Map.forall (fun other plays -> other = hand || plays < thisHand)

        keep { state with Value = if mostPlayed then 1.0 else state.Value + 0.2 }
    | "j_vampire", HandPlayed(_, _, scoring, _) ->
        add (0.1 * float (countOf (fun c -> c.Enhancement.IsSome && not c.Debuffed) scoring))
    | "j_wee", HandPlayed(_, _, scoring, _) -> add (8.0 * float (countOf (isRank Two) scoring))
    | "j_lucky_cat", HandPlayed(_, _, scoring, _) ->
        let lucky card =
            not card.Debuffed
            && card.Enhancement = Some Enhancement.Lucky
            && (chance 5 || chance 15)

        add (0.25 * float (countOf lucky scoring))
    | "j_loyalty_card", HandPlayed _ -> keep { state with Count = state.Count + 1 }
    | "j_ice_cream", HandPlayed _ -> if state.Value <= 5.0 then None, 0 else add (-5.0)
    | "j_matador", HandPlayed(_, _, scoring, _) ->
        pay state (if scoring |> List.exists (fun c -> c.Debuffed) then 8 else 0)
    | "j_todo_list", HandPlayed(hand, _, _, _) -> pay state (if Some hand = state.Hand then 4 else 0)
    | "j_business", HandPlayed(_, _, scoring, _) ->
        pay state (2 * countOf (fun c -> isFace table c && chance 2) scoring)
    | "j_rough_gem", HandPlayed(_, _, scoring, _) -> pay state (countOf (isSuit table Diamonds) scoring)
    | "j_ticket", HandPlayed(_, _, scoring, _) ->
        pay state (4 * countOf (fun c -> not c.Debuffed && c.Enhancement = Some Enhancement.Gold) scoring)
    | "j_reserved_parking", HandPlayed(_, _, _, held) -> pay state (countOf (fun c -> isFace table c && chance 2) held)

    | "j_ramen", Discarded cards ->
        let eaten = state.Count + cards.Length
        if eaten >= 100 then None, 0 else keep { state with Count = eaten }
    | "j_yorick", Discarded cards ->
        let total = state.Count + cards.Length
        keep { state with Value = state.Value + float (total / 23); Count = total % 23 }
    | "j_castle", Discarded cards ->
        add (3.0 * float (countOf (isSuit table (defaultArg state.Suit Spades)) cards))
    | "j_hit_the_road", Discarded cards -> add (0.5 * float (countOf (isRank Jack) cards))
    | "j_faceless", Discarded cards -> pay state (if faces cards >= 3 then 5 else 0)
    | "j_mail", Discarded cards -> pay state (5 * countOf (isRank (defaultArg state.Rank Ace)) cards)
    | "j_trading", Discarded [ _ ] when state.Used = 1 -> pay state 3

    | "j_golden", RoundWon _ -> pay state 4
    | "j_cloud_9", RoundWon _ -> pay state (countOf (isRank Nine) table.Deck)
    | "j_satellite", RoundWon _ -> pay state (int state.Value)
    | "j_rocket", RoundWon(boss, _) ->
        let next = { state with Value = state.Value + (if boss then 2.0 else 0.0) }
        pay next (int next.Value)
    | "j_delayed_grat", RoundWon(_, left) -> pay state (if state.Used = 0 then 2 * left else 0)
    | "j_to_the_moon", RoundWon _ -> pay state (min (max 0 table.Money / 5) 5)
    | "j_popcorn", RoundWon _ -> if state.Value <= 4.0 then None, 0 else add (-4.0)
    | "j_gros_michel", RoundWon _ when chance 6 -> None, 0
    | "j_cavendish", RoundWon _ when chance 1000 -> None, 0
    | ("j_egg" | "j_gift"), RoundWon _ -> keep { state with Count = state.Count + 1 }
    | "j_campfire", RoundWon(true, _) -> keep { state with Value = 1.0 }
    | "j_hit_the_road", RoundWon _ -> keep { state with Value = 1.0 }

    | "j_madness", BlindSelected false -> add 0.5
    | "j_ceremonial", BlindSelected _ ->
        let right =
            table.Jokers
            |> List.tryFindIndex ((=) state.Card.Key)
            |> Option.bind (fun i -> List.tryItem (i + 1) table.Jokers)

        add (right |> Option.map (fun key -> 2.0 * float (sellOf key)) |> Option.defaultValue 0.0)
    | "j_flash", Rerolled -> add 2.0
    | "j_campfire", Sold _ -> add 0.25
    | "j_red_card", PackSkipped -> add 3.0
    | "j_constellation", ConsumableUsed Api.CardSet.Planet -> add 0.1
    | "j_satellite", ConsumableUsed Api.CardSet.Planet -> add 1.0
    | "j_fortune_teller", ConsumableUsed Api.CardSet.Tarot -> add 1.0
    | "j_glass", CardsDestroyed cards ->
        add (0.75 * float (countOf (fun c -> c.Enhancement = Some Enhancement.Glass) cards))
    | "j_caino", CardsDestroyed cards -> add (float (faces cards))
    | "j_hologram", CardsAdded cards -> add (0.25 * float cards.Length)
    | _ -> keep state

let card (table: Table) (state: State) : Api.Card =
    let source = state.Card
    let text = source.Value.Effect
    let current value = replace currentRe (flat value) text
    let owned key = table.Jokers |> List.filter ((=) key) |> List.length
    let inDeck predicate = float (countOf predicate table.Deck)
    let enhanced enhancement (c: Card) = c.Enhancement = Some enhancement

    let others =
        match List.tryFindIndex ((=) source.Key) table.Jokers with
        | Some i -> List.removeAt i table.Jokers
        | None -> table.Jokers

    let effect =
        match source.Key with
        | "j_blue_joker" -> current (2.0 * float table.DrawPile)
        | "j_bull" -> current (2.0 * float (max 0 table.Money))
        | "j_bootstraps" -> current (2.0 * float (max 0 table.Money / 5))
        | "j_stencil" -> current (float (table.JokerSlots - table.Jokers.Length + owned "j_stencil"))
        | "j_abstract" -> current (3.0 * float table.Jokers.Length)
        | "j_swashbuckler" -> current (float (others |> List.sumBy sellOf))
        | "j_steel_joker" -> current (1.0 + 0.2 * inDeck (enhanced Enhancement.Steel))
        | "j_stone" -> current (25.0 * inDeck (enhanced Enhancement.Stone))
        | "j_erosion" -> current (4.0 * float (max 0 (52 - table.Deck.Length)))
        | "j_drivers_license" -> current (inDeck (fun c -> c.Enhancement.IsSome))
        | "j_throwback" -> current (1.0 + 0.25 * float table.Skips)
        | "j_cloud_9" -> current (inDeck (isRank Nine))
        | "j_loyalty_card" ->
            let left = ((5 - state.Count) % 6 + 6) % 6
            replace counterRe (if left = 0 then "Active!" else $"{left} remaining") text
        | "j_ice_cream"
        | "j_popcorn" -> replace leadingRe (flat state.Value) text
        | "j_ramen" -> replace leadingRe (flat (2.0 - 0.01 * float state.Count)) text
        | "j_rocket" -> replace payoutRe (flat state.Value) text
        | "j_yorick" -> current state.Value |> replace bracketRe $"[{23 - state.Count}]"
        | "j_castle" -> current state.Value |> replace castleRe (suitName (defaultArg state.Suit Spades))
        | "j_ancient" -> replace ancientRe (suitName (defaultArg state.Suit Spades)) text
        | "j_idol" ->
            replace idolRe $"{Rank.label (defaultArg state.Rank Ace)} of {defaultArg state.Suit Spades}" text
        | "j_mail" -> replace mailRe (Rank.label (defaultArg state.Rank Ace)) text
        | "j_todo_list" -> replace todoRe (HandType.label (defaultArg state.Hand HighCard)) text
        | "j_green_joker"
        | "j_ride_the_bus"
        | "j_runner"
        | "j_square"
        | "j_trousers"
        | "j_obelisk"
        | "j_ceremonial"
        | "j_constellation"
        | "j_red_card"
        | "j_madness"
        | "j_hologram"
        | "j_flash"
        | "j_campfire"
        | "j_glass"
        | "j_lucky_cat"
        | "j_hit_the_road"
        | "j_caino"
        | "j_wee"
        | "j_vampire"
        | "j_fortune_teller"
        | "j_satellite" -> current state.Value
        | _ -> text

    let sell =
        match source.Key with
        | "j_egg" -> source.Cost.Sell + 3 * state.Count
        | "j_gift" -> source.Cost.Sell + state.Count
        | _ -> source.Cost.Sell

    { source with
        Value = { source.Value with Effect = effect }
        Cost = { source.Cost with Sell = sell } }
