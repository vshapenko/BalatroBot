module Balatro.Bot.Mapping

open System.Globalization
open System.Text.RegularExpressions
open Balatro.Core
open Balatro.Core.Domain

let private suit s =
    match s with
    | Api.Suit.Spades -> Spades
    | Api.Suit.Hearts -> Hearts
    | Api.Suit.Clubs -> Clubs
    | Api.Suit.Diamonds -> Diamonds

let private rank r =
    match r with
    | Api.Rank.Two -> Two
    | Api.Rank.Three -> Three
    | Api.Rank.Four -> Four
    | Api.Rank.Five -> Five
    | Api.Rank.Six -> Six
    | Api.Rank.Seven -> Seven
    | Api.Rank.Eight -> Eight
    | Api.Rank.Nine -> Nine
    | Api.Rank.Ten -> Ten
    | Api.Rank.Jack -> Jack
    | Api.Rank.Queen -> Queen
    | Api.Rank.King -> King
    | Api.Rank.Ace -> Ace

let private enhancement e =
    match e with
    | Api.Enhancement.Bonus -> Enhancement.Bonus
    | Api.Enhancement.Mult -> Enhancement.Mult
    | Api.Enhancement.Wild -> Enhancement.Wild
    | Api.Enhancement.Glass -> Enhancement.Glass
    | Api.Enhancement.Steel -> Enhancement.Steel
    | Api.Enhancement.Stone -> Enhancement.Stone
    | Api.Enhancement.Gold -> Enhancement.Gold
    | Api.Enhancement.Lucky -> Enhancement.Lucky

let private edition e =
    match e with
    | Api.Edition.Foil -> Foil
    | Api.Edition.Holo -> Holo
    | Api.Edition.Polychrome -> Polychrome
    | Api.Edition.Negative -> Negative

let private seal s =
    match s with
    | Api.Seal.Red -> Seal.Red
    | Api.Seal.Blue -> Seal.Blue
    | Api.Seal.Gold -> Seal.Gold
    | Api.Seal.Purple -> Seal.Purple

let toCard (card: Api.Card) : Card option =
    match card.Value.Rank, card.Value.Suit with
    | Some r, Some s ->
        Some
            { Rank = rank r
              Suit = suit s
              Enhancement = card.Modifier.Enhancement |> Option.map enhancement
              Edition = card.Modifier.Edition |> Option.map edition
              Seal = card.Modifier.Seal |> Option.map seal
              Debuffed = card.State.Debuff }
    | _ -> None

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

let levels (state: Api.GameState) : Map<HandType, HandLevel> =
    handTypes
    |> List.map (fun t ->
        let info = state.Hands[HandType.label t]
        t, { Chips = float info.Chips; Mult = float info.Mult })
    |> Map.ofList

let currentValue (text: string) : float option =
    let m = Regex.Match(text, @"\(Currently[^\d)]*(\d+(?:\.\d+)?)")

    if m.Success then
        Some(System.Double.Parse(m.Groups[1].Value, CultureInfo.InvariantCulture))
    else
        None

let joker (card: Api.Card) : Jokers.Joker =
    { Key = card.Key
      Name = card.Label
      Edition = card.Modifier.Edition |> Option.map edition
      Debuffed = card.State.Debuff
      Value = currentValue card.Value.Effect
      SellValue = card.Cost.Sell }

let situation (state: Api.GameState) : Jokers.Situation =
    { Jokers = state.Jokers.Cards |> List.map joker
      HandsLeft = max 0 (state.Round.HandsLeft - 1)
      DiscardsLeft = state.Round.DiscardsLeft
      Money = state.Money
      DeckCount = state.Cards.Count
      JokerSlots = state.Jokers.Limit }
