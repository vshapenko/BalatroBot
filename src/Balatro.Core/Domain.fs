module Balatro.Core.Domain

type Suit =
    | Spades
    | Hearts
    | Clubs
    | Diamonds

type Rank =
    | Two
    | Three
    | Four
    | Five
    | Six
    | Seven
    | Eight
    | Nine
    | Ten
    | Jack
    | Queen
    | King
    | Ace

module Rank =
    let id rank =
        match rank with
        | Two -> 2
        | Three -> 3
        | Four -> 4
        | Five -> 5
        | Six -> 6
        | Seven -> 7
        | Eight -> 8
        | Nine -> 9
        | Ten -> 10
        | Jack -> 11
        | Queen -> 12
        | King -> 13
        | Ace -> 14

    let nominal rank =
        match rank with
        | Jack
        | Queen
        | King -> 10
        | Ace -> 11
        | other -> id other

    let label rank =
        match rank with
        | Jack -> "Jack"
        | Queen -> "Queen"
        | King -> "King"
        | Ace -> "Ace"
        | other -> string (id other)

[<RequireQualifiedAccess>]
type Enhancement =
    | Bonus
    | Mult
    | Wild
    | Glass
    | Steel
    | Stone
    | Gold
    | Lucky

type Edition =
    | Foil
    | Holo
    | Polychrome
    | Negative

[<RequireQualifiedAccess>]
type Seal =
    | Red
    | Blue
    | Gold
    | Purple

type Card =
    { Rank: Rank
      Suit: Suit
      Enhancement: Enhancement option
      Edition: Edition option
      Seal: Seal option
      Debuffed: bool }

type HandType =
    | FlushFive
    | FlushHouse
    | FiveOfAKind
    | StraightFlush
    | FourOfAKind
    | FullHouse
    | Flush
    | Straight
    | ThreeOfAKind
    | TwoPair
    | Pair
    | HighCard

module HandType =
    let label handType =
        match handType with
        | FlushFive -> "Flush Five"
        | FlushHouse -> "Flush House"
        | FiveOfAKind -> "Five of a Kind"
        | StraightFlush -> "Straight Flush"
        | FourOfAKind -> "Four of a Kind"
        | FullHouse -> "Full House"
        | Flush -> "Flush"
        | Straight -> "Straight"
        | ThreeOfAKind -> "Three of a Kind"
        | TwoPair -> "Two Pair"
        | Pair -> "Pair"
        | HighCard -> "High Card"

type HandLevel = { Chips: float; Mult: float }

[<RequireQualifiedAccess>]
type Stake =
    | White
    | Red
    | Green
    | Black
    | Blue
    | Purple
    | Orange
    | Gold

module Stake =
    let number stake =
        match stake with
        | Stake.White -> 1
        | Stake.Red -> 2
        | Stake.Green -> 3
        | Stake.Black -> 4
        | Stake.Blue -> 5
        | Stake.Purple -> 6
        | Stake.Orange -> 7
        | Stake.Gold -> 8

let handLevel handType level =
    let sChips, sMult, lChips, lMult =
        match handType with
        | FlushFive -> 160.0, 16.0, 50.0, 3.0
        | FlushHouse -> 140.0, 14.0, 40.0, 4.0
        | FiveOfAKind -> 120.0, 12.0, 35.0, 3.0
        | StraightFlush -> 100.0, 8.0, 40.0, 4.0
        | FourOfAKind -> 60.0, 7.0, 30.0, 3.0
        | FullHouse -> 40.0, 4.0, 25.0, 2.0
        | Flush -> 35.0, 4.0, 15.0, 2.0
        | Straight -> 30.0, 4.0, 30.0, 3.0
        | ThreeOfAKind -> 30.0, 3.0, 20.0, 2.0
        | TwoPair -> 20.0, 2.0, 20.0, 1.0
        | Pair -> 10.0, 2.0, 15.0, 1.0
        | HighCard -> 5.0, 1.0, 10.0, 1.0

    let steps = float (level - 1)

    { Chips = max (sChips + lChips * steps) 0.0
      Mult = max (sMult + lMult * steps) 1.0 }
