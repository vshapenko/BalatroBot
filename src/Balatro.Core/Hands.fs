module Balatro.Core.Hands

open Balatro.Core.Domain

let private isStone (card: Card) = card.Enhancement = Some Enhancement.Stone

// A Wild card matches every suit unless debuffed; a Stone card matches none.
let private matchesSuit suit (card: Card) =
    not (isStone card)
    && ((card.Enhancement = Some Enhancement.Wild && not card.Debuffed) || card.Suit = suit)

let private suitOrder suit =
    match suit with
    | Diamonds -> 1
    | Clubs -> 2
    | Hearts -> 3
    | Spades -> 4

let private highCardKey (_, card: Card) = Rank.id card.Rank, suitOrder card.Suit

let private hasStraightRanks (ranks: Rank list) =
    match ranks |> List.map Rank.id |> List.sort with
    | [ 2; 3; 4; 5; 14 ] -> true
    | [ a; b; c; d; e ] -> b = a + 1 && c = b + 1 && d = c + 1 && e = d + 1
    | _ -> false

let classify (cards: Card list) : HandType * Card list =
    let indexed = List.indexed cards
    let ranked = indexed |> List.filter (fun (_, card) -> not (isStone card))

    let groupsOf size =
        ranked
        |> List.groupBy (fun (_, card) -> card.Rank)
        |> List.filter (fun (_, group) -> group.Length = size)
        |> List.sortByDescending (fun (rank, _) -> Rank.id rank)
        |> List.map snd

    let five = groupsOf 5
    let four = groupsOf 4
    let three = groupsOf 3
    let two = groupsOf 2

    let isFlush =
        cards.Length = 5
        && [ Spades; Hearts; Clubs; Diamonds ]
           |> List.exists (fun suit -> cards |> List.forall (matchesSuit suit))

    let isStraight =
        ranked.Length = 5 && hasStraightRanks (ranked |> List.map (fun (_, card) -> card.Rank))

    let handType, scoring =
        if not five.IsEmpty && isFlush then
            FlushFive, five.Head
        elif not three.IsEmpty && not two.IsEmpty && isFlush then
            FlushHouse, three.Head @ two.Head
        elif not five.IsEmpty then
            FiveOfAKind, five.Head
        elif isFlush && isStraight then
            StraightFlush, indexed
        elif not four.IsEmpty then
            FourOfAKind, four.Head
        elif not three.IsEmpty && not two.IsEmpty then
            FullHouse, three.Head @ two.Head
        elif isFlush then
            Flush, indexed
        elif isStraight then
            Straight, indexed
        elif not three.IsEmpty then
            ThreeOfAKind, three.Head
        elif two.Length = 2 then
            TwoPair, two.Head @ two.Tail.Head
        elif not two.IsEmpty then
            Pair, two.Head
        else
            // Stone cards always rank below every other card
            HighCard, ranked |> List.sortByDescending highCardKey |> List.truncate 1

    let stones = indexed |> List.filter (fun (_, card) -> isStone card)

    let scoringCards =
        scoring @ stones
        |> List.distinctBy fst
        |> List.sortBy fst
        |> List.map snd

    handType, scoringCards
