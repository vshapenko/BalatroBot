module Balatro.Core.Jokers

open Balatro.Core.Domain
open Balatro.Core.Hands

type Joker =
    { Key: string
      Name: string
      Edition: Edition option
      Debuffed: bool
      Value: float option
      SellValue: int }

type Situation =
    { Jokers: Joker list
      HandsLeft: int
      DiscardsLeft: int
      Money: int
      DeckCount: int
      JokerSlots: int }

module Situation =
    let empty =
        { Jokers = []
          HandsLeft = 0
          DiscardsLeft = 0
          Money = 0
          DeckCount = 0
          JokerSlots = 0 }

type Context =
    { Played: Card list
      Scoring: Card list
      Held: Card list
      HandType: HandType
      Contained: Set<HandType>
      Rules: HandRules
      Situation: Situation }

type Effect =
    | AddChips of float
    | AddMult of float
    | TimesMult of float

type JokerDef =
    { BeforeCards: Context -> Joker -> Card list -> Card list
      OnScored: Context -> Joker -> int -> Card -> Effect list
      Retriggers: Context -> Joker -> int -> Card -> int
      OnHeld: Context -> Joker -> Card -> Effect list
      HeldRetriggers: Context -> Joker -> Card -> int
      Main: Context -> Joker -> Effect list }

module JokerDef =
    let none =
        { BeforeCards = fun _ _ cards -> cards
          OnScored = fun _ _ _ _ -> []
          Retriggers = fun _ _ _ _ -> 0
          OnHeld = fun _ _ _ -> []
          HeldRetriggers = fun _ _ _ -> 0
          Main = fun _ _ -> [] }

let isFace (context: Context) (card: Card) =
    not card.Debuffed
    && (context.Rules.Pareidolia
        || (card.Enhancement <> Some Enhancement.Stone
            && (card.Rank = Jack || card.Rank = Queen || card.Rank = King)))

let isSuit (context: Context) (suit: Suit) (card: Card) =
    let isRed suit = suit = Hearts || suit = Diamonds

    not card.Debuffed
    && card.Enhancement <> Some Enhancement.Stone
    && (card.Enhancement = Some Enhancement.Wild
        || (context.Rules.Smeared && isRed card.Suit = isRed suit)
        || card.Suit = suit)
