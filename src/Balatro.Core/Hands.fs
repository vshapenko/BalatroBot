module Balatro.Core.Hands

open Balatro.Core.Domain

type HandRules =
    { FourFingers: bool
      Shortcut: bool
      Smeared: bool
      Splash: bool
      Pareidolia: bool }

module HandRules =
    let none =
        { FourFingers = false
          Shortcut = false
          Smeared = false
          Splash = false
          Pareidolia = false }

let private isStone (card: Card) =
    match card.Enhancement with
    | Some Enhancement.Stone -> true
    | _ -> false

let private isRed suit = suit = Hearts || suit = Diamonds

// A Wild card matches every suit unless debuffed; a Stone card matches none.
let private matchesSuit rules suit (card: Card) =
    let isWild =
        match card.Enhancement with
        | Some Enhancement.Wild -> true
        | _ -> false

    not (isStone card)
    && ((isWild && not card.Debuffed)
        || (rules.Smeared && isRed card.Suit = isRed suit)
        || card.Suit = suit)

let private suitOrder suit =
    match suit with
    | Diamonds -> 1
    | Clubs -> 2
    | Hearts -> 3
    | Spades -> 4

// Rank order, then suit order; the suit order is below 8
let private highCardKey (_, card: Card) = Rank.id card.Rank * 8 + suitOrder card.Suit

let private suits = [ Spades; Hearts; Clubs; Diamonds ]

let private flushCards rules (indexed: (int * Card) list) =
    let need = if rules.FourFingers then 4 else 5

    if indexed.Length > 5 || indexed.Length < need then
        None
    else
        let matching suit =
            indexed |> List.filter (fun (_, card) -> matchesSuit rules suit card)

        suits
        |> List.tryFind (fun suit ->
            (indexed |> List.sumBy (fun (_, card) -> if matchesSuit rules suit card then 1 else 0)) >= need)
        |> Option.map matching

let private straightCards rules (indexed: (int * Card) list) =
    let need = if rules.FourFingers then 4 else 5

    if indexed.Length > 5 || indexed.Length < need then
        None
    else
        let byRank = Array.create 15 []

        for (_, card) as entry in List.rev indexed do
            if not (isStone card) then
                let id = Rank.id card.Rank
                byRank[id] <- entry :: byRank[id]

        // Ace counts at both ends; with Shortcut a single missing rank may be skipped
        let rec walk id length skipped found taken =
            if id > 14 then
                found, taken
            else
                match byRank[if id = 1 then 14 else id] with
                | [] when rules.Shortcut && not skipped && id <> 14 -> walk (id + 1) length true found taken
                | [] when found -> found, taken
                | [] -> walk (id + 1) 0 false false []
                | cards ->
                    let length = length + 1
                    walk (id + 1) length false (found || length >= need) (taken @ cards)

        match walk 1 0 false false [] with
        | true, taken -> Some taken
        | false, _ -> None

// Every hand type the cards contain, best first, each with the cards that score for it
let private evaluate rules (cards: Card list) =
    let indexed = List.indexed cards
    let ranked = indexed |> List.filter (fun (_, card) -> not (isStone card))

    let counts = Array.zeroCreate 15
    let groups = Array.create 15 []

    for (_, card) as entry in List.rev ranked do
        let id = Rank.id card.Rank
        counts[id] <- counts[id] + 1
        groups[id] <- entry :: groups[id]

    let groupsOf size =
        [ for id in 14..-1..2 do
              if counts[id] = size then
                  yield groups[id] ]

    let five = groupsOf 5
    let four = groupsOf 4
    let three = groupsOf 3
    let two = groupsOf 2
    let flush = flushCards rules indexed
    let straight = straightCards rules indexed

    [ if not five.IsEmpty && flush.IsSome then
          yield FlushFive, five.Head
      if not three.IsEmpty && not two.IsEmpty && flush.IsSome then
          yield FlushHouse, three.Head @ two.Head
      if not five.IsEmpty then
          yield FiveOfAKind, five.Head
      match flush, straight with
      | Some f, Some s -> yield StraightFlush, f @ (s |> List.filter (fun x -> not (List.contains x f)))
      | _ -> ()
      if not four.IsEmpty then
          yield FourOfAKind, four.Head
      if not three.IsEmpty && not two.IsEmpty then
          yield FullHouse, three.Head @ two.Head
      match flush with
      | Some f -> yield Flush, f
      | None -> ()
      match straight with
      | Some s -> yield Straight, s
      | None -> ()
      if not three.IsEmpty then
          yield ThreeOfAKind, three.Head
      if two.Length = 2 then
          yield TwoPair, two.Head @ two.Tail.Head
      elif three.Length = 1 && two.Length = 1 then
          yield TwoPair, two.Head @ three.Head
      if not two.IsEmpty then
          yield Pair, two.Head
      if not cards.IsEmpty then
          // Stone cards always rank below every other card
          yield
              HighCard,
              match ranked with
              | [] -> []
              | first :: rest ->
                  [ rest |> List.fold (fun best c -> if highCardKey c > highCardKey best then c else best) first ] ]

let private classifyFound (cards: Card list) (found: (HandType * (int * Card) list) list) =
    let handType, scoring =
        match found with
        | best :: _ -> best
        | [] -> HighCard, []

    let stones = List.indexed cards |> List.filter (fun (_, card) -> isStone card)

    let scoringCards =
        scoring @ stones
        |> List.distinctBy fst
        |> List.sortBy fst
        |> List.map snd

    handType, scoringCards

let private containedIn (found: (HandType * (int * Card) list) list) : Set<HandType> =
    let lower upper below types =
        if Set.contains upper types then Set.add below types else types

    found
    |> List.map fst
    |> Set.ofList
    |> lower FiveOfAKind FourOfAKind
    |> lower FourOfAKind ThreeOfAKind
    |> lower ThreeOfAKind Pair

let classifyWith (rules: HandRules) (cards: Card list) : HandType * Card list =
    classifyFound cards (evaluate rules cards)

let classify (cards: Card list) : HandType * Card list = classifyWith HandRules.none cards

let contained (rules: HandRules) (cards: Card list) : Set<HandType> = containedIn (evaluate rules cards)

// classifyWith and contained from one evaluation
let classifyContained (rules: HandRules) (cards: Card list) : HandType * Card list * Set<HandType> =
    let found = evaluate rules cards
    let handType, scoringCards = classifyFound cards found
    handType, scoringCards, containedIn found
