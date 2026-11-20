module Balatro.Bot.Tarot

open Balatro.Core.Domain

type Effect =
    | Enhance of Enhancement * int
    | ToSuit of Suit * int
    | Destroy of int
    | Money

let effect (key: string) : Effect option =
    match key with
    | "c_empress" -> Some(Enhance(Enhancement.Mult, 2))
    | "c_heirophant" -> Some(Enhance(Enhancement.Bonus, 2))
    | "c_justice" -> Some(Enhance(Enhancement.Glass, 1))
    | "c_chariot" -> Some(Enhance(Enhancement.Steel, 1))
    | "c_lovers" -> Some(Enhance(Enhancement.Wild, 1))
    | "c_star" -> Some(ToSuit(Diamonds, 3))
    | "c_moon" -> Some(ToSuit(Clubs, 3))
    | "c_sun" -> Some(ToSuit(Hearts, 3))
    | "c_world" -> Some(ToSuit(Spades, 3))
    | "c_hanged_man" -> Some(Destroy 2)
    | "c_hermit"
    | "c_temperance" -> Some Money
    | _ -> None

let targets (effect: Effect) (plan: Plan.Plan) (cards: Card list) : int list =
    let indexed = List.indexed cards
    let unenhanced = indexed |> List.filter (fun (_, c) -> c.Enhancement.IsNone)

    let lowest n picks =
        picks |> List.sortBy (fun (i, c) -> Rank.id c.Rank, i) |> List.truncate n |> List.map fst

    let highest n picks =
        picks |> List.sortBy (fun (i, c) -> -(Rank.id c.Rank), i) |> List.truncate n |> List.map fst

    match effect, plan.Suit with
    | ToSuit(suit, n), Some wanted when wanted = suit ->
        indexed
        |> List.filter (fun (_, c) -> c.Suit <> suit && c.Enhancement <> Some Enhancement.Wild && c.Enhancement <> Some Enhancement.Stone)
        |> lowest n
    | ToSuit _, _ -> []
    | Enhance(Enhancement.Wild, n), Some wanted -> unenhanced |> List.filter (fun (_, c) -> c.Suit <> wanted) |> highest n
    | Enhance(Enhancement.Wild, _), None -> []
    | Enhance(_, n), suit ->
        let onSuit =
            suit
            |> Option.map (fun wanted -> unenhanced |> List.filter (fun (_, c) -> c.Suit = wanted))
            |> Option.filter (not << List.isEmpty)

        let pairedRanks =
            unenhanced |> List.countBy (fun (_, c) -> c.Rank) |> List.filter (fun (_, count) -> count >= 2) |> List.map fst

        let paired = unenhanced |> List.filter (fun (_, c) -> List.contains c.Rank pairedRanks)

        match onSuit with
        | Some pool -> highest n pool
        | None -> highest n (if paired.IsEmpty then unenhanced else paired)
    | Destroy n, suit ->
        unenhanced
        |> List.filter (fun (_, c) -> suit |> Option.forall (fun wanted -> c.Suit <> wanted))
        |> lowest n
    | Money, _ -> []

let transform (effect: Effect) (chosen: int list) (cards: Card list) : Card list =
    let change f =
        cards |> List.mapi (fun i c -> if List.contains i chosen then f c else c)

    match effect with
    | Enhance(e, _) -> change (fun c -> { c with Enhancement = Some e })
    | ToSuit(suit, _) -> change (fun c -> { c with Suit = suit })
    | Destroy _ -> cards |> List.indexed |> List.filter (fun (i, _) -> not (List.contains i chosen)) |> List.map snd
    | Money -> cards
