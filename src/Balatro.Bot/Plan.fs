module Balatro.Bot.Plan

open Balatro.Core
open Balatro.Core.Domain
open Balatro.Core.JokerProfiles

type Plan =
    { Hand: HandType
      Suit: Suit option
      Share: float
      Fans: string list }

let private suits = [ Spades; Hearts; Clubs; Diamonds ]

let private wants (joker: Jokers.Joker) = (profile joker.Key).Wants

let make (seed: int) (build: Shop.Build) : Plan =
    let _, rates = Shop.roundStats seed 20 (Shop.revalue build)
    let hand, plays = rates.Plays |> Map.toList |> List.minBy (fun (handType, count) -> -count, handType)
    let total = rates.Plays |> Map.toList |> List.sumBy snd

    let inDeck suit = build.Deck |> List.filter (fun card -> card.Suit = suit) |> List.length
    let wanting suit = build.Jokers |> List.filter (fun joker -> List.contains (Suit suit) (wants joker)) |> List.length

    let wanted =
        suits
        |> List.filter (fun suit -> wanting suit > 0)
        |> List.sortBy (fun suit -> -wanting suit, -inDeck suit, suit)
        |> List.tryHead

    let suit =
        match wanted with
        | None when List.contains hand [ Flush; StraightFlush; FlushHouse; FlushFive ] ->
            suits |> List.sortBy (fun suit -> -inDeck suit, suit) |> List.tryHead
        | _ -> wanted

    let fans =
        build.Jokers
        |> List.filter (fun joker ->
            wants joker |> List.exists (fun want -> want = Hand hand || Some want = Option.map Suit suit))
        |> List.map (fun joker -> joker.Name)

    { Hand = hand
      Suit = suit
      Share = plays / total
      Fans = fans }

let text (plan: Plan) : string =
    let suit = plan.Suit |> Option.map (fun suit -> " in " + string suit) |> Option.defaultValue ""
    let fits = if plan.Fans.IsEmpty then "" else "; fits " + String.concat ", " plan.Fans
    sprintf "plan: %s%s (%.0f%% of simulated plays%s)" (HandType.label plan.Hand) suit (plan.Share * 100.0) fits
