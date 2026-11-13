module Balatro.Bot.Api

open System
open System.Net.Http
open System.Text
open System.Text.Json
open System.Text.Json.Nodes
open System.Threading.Tasks

[<RequireQualifiedAccess>]
type State =
    | Menu
    | BlindSelect
    | SelectingHand
    | HandPlayed
    | DrawToHand
    | NewRound
    | RoundEval
    | Shop
    | PlayTarot
    | TarotPack
    | PlanetPack
    | SpectralPack
    | StandardPack
    | BuffoonPack
    | GameOver
    | SmodsBoosterOpened

[<RequireQualifiedAccess>]
type Suit =
    | Hearts
    | Diamonds
    | Clubs
    | Spades

[<RequireQualifiedAccess>]
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

[<RequireQualifiedAccess>]
type Seal =
    | Red
    | Blue
    | Gold
    | Purple

[<RequireQualifiedAccess>]
type Edition =
    | Foil
    | Holo
    | Polychrome
    | Negative

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

[<RequireQualifiedAccess>]
type BlindType =
    | Small
    | Big
    | Boss

[<RequireQualifiedAccess>]
type BlindStatus =
    | Select
    | Current
    | Upcoming
    | Defeated
    | Skipped

[<RequireQualifiedAccess>]
type CardSet =
    | Default
    | Enhanced
    | Joker
    | Tarot
    | Planet
    | Spectral
    | Voucher
    | Booster

[<RequireQualifiedAccess>]
type Deck =
    | Red
    | Blue
    | Yellow
    | Green
    | Black
    | Magic
    | Nebula
    | Ghost
    | Abandoned
    | Checkered
    | Zodiac
    | Painted
    | Anaglyph
    | Plasma
    | Erratic

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

type CardValue =
    { Suit: Suit option
      Rank: Rank option
      Effect: string }

type CardModifier =
    { Eternal: bool
      Rental: bool
      Perishable: int option
      Seal: Seal option
      Edition: Edition option
      Enhancement: Enhancement option }

type CardState =
    { Hidden: bool
      Debuff: bool
      Highlight: bool }

type CardCost = { Sell: int; Buy: int }

type Card =
    { Id: int
      Key: string
      Set: CardSet
      Label: string
      Value: CardValue
      Modifier: CardModifier
      State: CardState
      Cost: CardCost }

type Area =
    { Count: int
      Limit: int
      HighlightedLimit: int
      Cards: Card list }

type Round =
    { HandsPlayed: int
      Chips: int
      RerollCost: int
      HandsLeft: int
      DiscardsLeft: int
      DiscardsUsed: int }

type Blind =
    { Type: BlindType
      Status: BlindStatus
      Name: string
      Effect: string
      Score: int
      TagName: string option
      TagEffect: string option }

type Blinds =
    { Small: Blind
      Big: Blind
      Boss: Blind }

type HandInfo =
    { Order: int
      Level: int
      Chips: int
      Mult: int
      Played: int
      PlayedThisRound: int }

type GameState =
    { State: State
      RoundNum: int
      AnteNum: int
      Money: int
      Won: bool
      Deck: Deck option
      Stake: Stake option
      Seed: string option
      Round: Round
      Hands: Map<string, HandInfo>
      Blinds: Blinds option
      UsedVouchers: Map<string, string>
      Hand: Area
      Jokers: Area
      Consumables: Area
      Cards: Area
      Shop: Area
      Vouchers: Area
      Packs: Area
      Pack: Area }

type RpcError =
    { Code: int
      Name: string
      Message: string }

type ApiError =
    | Rpc of RpcError
    | Transport of string

type private ResultBuilder() =
    member _.Bind(r, f) = Result.bind f r
    member _.Return x = Ok x
    member _.ReturnFrom r = r

let private result = ResultBuilder()

let private stateTable =
    [ "MENU", State.Menu
      "BLIND_SELECT", State.BlindSelect
      "SELECTING_HAND", State.SelectingHand
      "HAND_PLAYED", State.HandPlayed
      "DRAW_TO_HAND", State.DrawToHand
      "NEW_ROUND", State.NewRound
      "ROUND_EVAL", State.RoundEval
      "SHOP", State.Shop
      "PLAY_TAROT", State.PlayTarot
      "TAROT_PACK", State.TarotPack
      "PLANET_PACK", State.PlanetPack
      "SPECTRAL_PACK", State.SpectralPack
      "STANDARD_PACK", State.StandardPack
      "BUFFOON_PACK", State.BuffoonPack
      "GAME_OVER", State.GameOver
      "SMODS_BOOSTER_OPENED", State.SmodsBoosterOpened ]

let private suitTable =
    [ "H", Suit.Hearts
      "D", Suit.Diamonds
      "C", Suit.Clubs
      "S", Suit.Spades ]

let private rankTable =
    [ "2", Rank.Two
      "3", Rank.Three
      "4", Rank.Four
      "5", Rank.Five
      "6", Rank.Six
      "7", Rank.Seven
      "8", Rank.Eight
      "9", Rank.Nine
      "T", Rank.Ten
      "J", Rank.Jack
      "Q", Rank.Queen
      "K", Rank.King
      "A", Rank.Ace ]

let private sealTable =
    [ "RED", Seal.Red
      "BLUE", Seal.Blue
      "GOLD", Seal.Gold
      "PURPLE", Seal.Purple ]

let private editionTable =
    [ "FOIL", Edition.Foil
      "HOLO", Edition.Holo
      "POLYCHROME", Edition.Polychrome
      "NEGATIVE", Edition.Negative ]

let private enhancementTable =
    [ "BONUS", Enhancement.Bonus
      "MULT", Enhancement.Mult
      "WILD", Enhancement.Wild
      "GLASS", Enhancement.Glass
      "STEEL", Enhancement.Steel
      "STONE", Enhancement.Stone
      "GOLD", Enhancement.Gold
      "LUCKY", Enhancement.Lucky ]

let private blindTypeTable =
    [ "SMALL", BlindType.Small
      "BIG", BlindType.Big
      "BOSS", BlindType.Boss ]

let private blindStatusTable =
    [ "SELECT", BlindStatus.Select
      "CURRENT", BlindStatus.Current
      "UPCOMING", BlindStatus.Upcoming
      "DEFEATED", BlindStatus.Defeated
      "SKIPPED", BlindStatus.Skipped ]

let private cardSetTable =
    [ "DEFAULT", CardSet.Default
      "ENHANCED", CardSet.Enhanced
      "JOKER", CardSet.Joker
      "TAROT", CardSet.Tarot
      "PLANET", CardSet.Planet
      "SPECTRAL", CardSet.Spectral
      "VOUCHER", CardSet.Voucher
      "BOOSTER", CardSet.Booster ]

let private deckTable =
    [ "RED", Deck.Red
      "BLUE", Deck.Blue
      "YELLOW", Deck.Yellow
      "GREEN", Deck.Green
      "BLACK", Deck.Black
      "MAGIC", Deck.Magic
      "NEBULA", Deck.Nebula
      "GHOST", Deck.Ghost
      "ABANDONED", Deck.Abandoned
      "CHECKERED", Deck.Checkered
      "ZODIAC", Deck.Zodiac
      "PAINTED", Deck.Painted
      "ANAGLYPH", Deck.Anaglyph
      "PLASMA", Deck.Plasma
      "ERRATIC", Deck.Erratic ]

let private stakeTable =
    [ "WHITE", Stake.White
      "RED", Stake.Red
      "GREEN", Stake.Green
      "BLACK", Stake.Black
      "BLUE", Stake.Blue
      "PURPLE", Stake.Purple
      "ORANGE", Stake.Orange
      "GOLD", Stake.Gold ]

let private wire table value =
    table |> List.find (fun (_, v) -> v = value) |> fst

// Lua serialises empty tables as [], so a non-object reads as an empty object
let private prop (name: string) (e: JsonElement) =
    if e.ValueKind = JsonValueKind.Object then
        match e.TryGetProperty name with
        | true, v when v.ValueKind <> JsonValueKind.Null -> Some v
        | _ -> None
    else
        None

let private emptyObject = JsonDocument.Parse("{}").RootElement

let private sub name e =
    prop name e |> Option.defaultValue emptyObject

let private required name e =
    match prop name e with
    | Some v -> Ok v
    | None -> Error(sprintf "missing field: %s" name)

let private intField name e =
    result {
        let! v = required name e
        return v.GetInt32()
    }

let private strField name e =
    result {
        let! v = required name e
        return v.GetString()
    }

let private intOr name e =
    prop name e |> Option.map (fun v -> v.GetInt32()) |> Option.defaultValue 0

let private boolOr name e =
    prop name e |> Option.map (fun v -> v.GetBoolean()) |> Option.defaultValue false

let private nonEmptyStr name e =
    prop name e
    |> Option.map (fun v -> v.GetString())
    |> Option.filter (fun s -> s <> "")

let private parseWith field table (s: string) =
    match List.tryFind (fun (k, _) -> k = s) table with
    | Some(_, v) -> Ok v
    | None -> Error(sprintf "unknown %s: %s" field s)

let private enumField name table e =
    result {
        let! s = strField name e
        return! parseWith name table s
    }

let private optEnum name table e =
    match prop name e with
    | None -> Ok None
    | Some v -> parseWith name table (v.GetString()) |> Result.map Some

let private traverse f xs =
    List.foldBack
        (fun x acc ->
            result {
                let! y = f x
                let! ys = acc
                return y :: ys
            })
        xs
        (Ok [])

let private decodeCard (e: JsonElement) =
    result {
        let! id = intField "id" e
        let! key = strField "key" e
        let! label = strField "label" e
        let! set = enumField "set" cardSetTable e
        let value = sub "value" e
        let! suit = optEnum "suit" suitTable value
        let! rank = optEnum "rank" rankTable value
        let! effect = strField "effect" value
        let modifier = sub "modifier" e

        let! seal = optEnum "seal" sealTable modifier
        let! edition = optEnum "edition" editionTable modifier

        // Outside playing cards this field is an effect-type label, not an enhancement
        let! enhancement =
            match set with
            | CardSet.Default
            | CardSet.Enhanced -> optEnum "enhancement" enhancementTable modifier
            | _ -> Ok None

        let state = sub "state" e
        let cost = sub "cost" e
        let! sell = intField "sell" cost
        let! buy = intField "buy" cost

        return
            { Id = id
              Key = key
              Set = set
              Label = label
              Value =
                { Suit = suit
                  Rank = rank
                  Effect = effect }
              Modifier =
                { Eternal = boolOr "eternal" modifier
                  Rental = boolOr "rental" modifier
                  Perishable = prop "perishable" modifier |> Option.map (fun v -> v.GetInt32())
                  Seal = seal
                  Edition = edition
                  Enhancement = enhancement }
              State =
                { Hidden = boolOr "hidden" state
                  Debuff = boolOr "debuff" state
                  Highlight = boolOr "highlight" state }
              Cost = { Sell = sell; Buy = buy } }
    }

let private emptyArea =
    { Count = 0
      Limit = 0
      HighlightedLimit = 0
      Cards = [] }

let private decodeArea name e =
    match prop name e with
    | None -> Ok emptyArea
    | Some a ->
        result {
            let! cards =
                match prop "cards" a with
                | Some cs -> cs.EnumerateArray() |> List.ofSeq |> traverse decodeCard
                | None -> Ok []

            return
                { Count = intOr "count" a
                  Limit = intOr "limit" a
                  HighlightedLimit = intOr "highlighted_limit" a
                  Cards = cards }
        }

let private decodeBlind (e: JsonElement) =
    result {
        let! t = enumField "type" blindTypeTable e
        let! status = enumField "status" blindStatusTable e
        let! name = strField "name" e
        let! effect = strField "effect" e
        let! score = intField "score" e

        return
            { Type = t
              Status = status
              Name = name
              Effect = effect
              Score = score
              TagName = nonEmptyStr "tag_name" e
              TagEffect = nonEmptyStr "tag_effect" e }
    }

let private decodeBlinds (e: JsonElement) =
    match prop "blinds" e with
    | None -> Ok None
    | Some b ->
        result {
            let! small = required "small" b |> Result.bind decodeBlind
            let! big = required "big" b |> Result.bind decodeBlind
            let! boss = required "boss" b |> Result.bind decodeBlind
            return Some { Small = small; Big = big; Boss = boss }
        }

let private decodeHand (e: JsonElement) =
    result {
        let! order = intField "order" e
        let! level = intField "level" e
        let! chips = intField "chips" e
        let! mult = intField "mult" e
        let! played = intField "played" e
        let! playedThisRound = intField "played_this_round" e

        return
            { Order = order
              Level = level
              Chips = chips
              Mult = mult
              Played = played
              PlayedThisRound = playedThisRound }
    }

let private members (e: JsonElement) =
    if e.ValueKind = JsonValueKind.Object then
        e.EnumerateObject() |> Seq.map (fun p -> p.Name, p.Value) |> List.ofSeq
    else
        []

let decodeGameState (e: JsonElement) : Result<GameState, string> =
    result {
        let! state = enumField "state" stateTable e
        let! roundNum = intField "round_num" e
        let! anteNum = intField "ante_num" e
        let! money = intField "money" e
        let! deck = optEnum "deck" deckTable e
        let! stake = optEnum "stake" stakeTable e

        let! hands =
            sub "hands" e
            |> members
            |> traverse (fun (k, v) -> decodeHand v |> Result.map (fun h -> k, h))

        let! blinds = decodeBlinds e
        let! hand = decodeArea "hand" e
        let! jokers = decodeArea "jokers" e
        let! consumables = decodeArea "consumables" e
        let! cards = decodeArea "cards" e
        let! shop = decodeArea "shop" e
        let! vouchers = decodeArea "vouchers" e
        let! packs = decodeArea "packs" e
        let! pack = decodeArea "pack" e
        let round = sub "round" e

        return
            { State = state
              RoundNum = roundNum
              AnteNum = anteNum
              Money = money
              Won = boolOr "won" e
              Deck = deck
              Stake = stake
              Seed = prop "seed" e |> Option.map (fun v -> v.GetString())
              Round =
                { HandsPlayed = intOr "hands_played" round
                  Chips = intOr "chips" round
                  RerollCost = intOr "reroll_cost" round
                  HandsLeft = intOr "hands_left" round
                  DiscardsLeft = intOr "discards_left" round
                  DiscardsUsed = intOr "discards_used" round }
              Hands = Map.ofList hands
              Blinds = blinds
              UsedVouchers =
                sub "used_vouchers" e
                |> members
                |> List.map (fun (k, v) -> k, v.GetString())
                |> Map.ofList
              Hand = hand
              Jokers = jokers
              Consumables = consumables
              Cards = cards
              Shop = shop
              Vouchers = vouchers
              Packs = packs
              Pack = pack }
    }

let encodeRequest (name: string) (parameters: (string * JsonNode) list) : string =
    let p = JsonObject()
    parameters |> List.iter (fun (k, v) -> p.Add(k, v))
    let o = JsonObject()
    o.Add("jsonrpc", JsonValue.Create "2.0" :> JsonNode)
    o.Add("method", JsonValue.Create name :> JsonNode)
    o.Add("params", p)
    o.Add("id", JsonValue.Create 1 :> JsonNode)
    o.ToJsonString()

let decodeResponse (body: string) : Result<JsonElement, ApiError> =
    try
        use doc = JsonDocument.Parse body
        let root = doc.RootElement

        match prop "error" root with
        | Some err ->
            Error(
                Rpc
                    { Code = intOr "code" err
                      Name = sub "data" err |> nonEmptyStr "name" |> Option.defaultValue ""
                      Message = nonEmptyStr "message" err |> Option.defaultValue "" }
            )
        | None ->
            match prop "result" root with
            | Some r -> Ok(r.Clone())
            | None -> Error(Transport "response has neither result nor error")
    with :? JsonException as ex ->
        Error(Transport ex.Message)

type Client = { Http: HttpClient }

let create (baseUrl: string) : Client =
    { Http = new HttpClient(BaseAddress = Uri baseUrl, Timeout = TimeSpan.FromSeconds 60.0) }

type BuyTarget =
    | BuyCard of int
    | BuyVoucher of int
    | BuyPack of int

type SellTarget =
    | SellJoker of int
    | SellConsumable of int

type PackAction =
    | PickCard of card: int * targets: int list
    | SkipPack

type Rearrangement =
    | RearrangeHand of int list
    | RearrangeJokers of int list
    | RearrangeConsumables of int list

let private ints (xs: int list) =
    JsonArray(xs |> List.map (fun i -> JsonValue.Create i :> JsonNode) |> Array.ofList) :> JsonNode

let private int' (i: int) = JsonValue.Create i :> JsonNode

let private call client name parameters =
    try
        use content =
            new StringContent(encodeRequest name parameters, Encoding.UTF8, "application/json")

        use response = client.Http.PostAsync("/", content).GetAwaiter().GetResult()
        response.Content.ReadAsStringAsync().GetAwaiter().GetResult() |> decodeResponse
    with
    | :? HttpRequestException as ex -> Error(Transport ex.Message)
    | :? TaskCanceledException as ex -> Error(Transport ex.Message)

let decodeChecked (e: JsonElement) : Result<GameState, ApiError> =
    try
        decodeGameState e |> Result.mapError (fun m -> Transport("decode: " + m))
    with
    | :? InvalidOperationException as ex -> Error(Transport("decode: " + ex.Message))
    | :? FormatException as ex -> Error(Transport("decode: " + ex.Message))

let private game client name parameters =
    call client name parameters |> Result.bind decodeChecked

let health client : Result<unit, ApiError> =
    call client "health" [] |> Result.map ignore

let gamestate client = game client "gamestate" []

let start client (deck: Deck) (stake: Stake) (seed: string option) =
    let seedParam =
        seed |> Option.map (fun s -> "seed", JsonValue.Create s :> JsonNode) |> Option.toList

    game
        client
        "start"
        ([ "deck", JsonValue.Create(wire deckTable deck) :> JsonNode
           "stake", JsonValue.Create(wire stakeTable stake) :> JsonNode ]
         @ seedParam)

let menu client = game client "menu" []

let select client = game client "select" []

let skip client = game client "skip" []

let play client (cards: int list) = game client "play" [ "cards", ints cards ]

let discard client (cards: int list) =
    game client "discard" [ "cards", ints cards ]

let buy client target =
    let p =
        match target with
        | BuyCard i -> "card", int' i
        | BuyVoucher i -> "voucher", int' i
        | BuyPack i -> "pack", int' i

    game client "buy" [ p ]

let sell client target =
    let p =
        match target with
        | SellJoker i -> "joker", int' i
        | SellConsumable i -> "consumable", int' i

    game client "sell" [ p ]

let reroll client = game client "reroll" []

let cashOut client = game client "cash_out" []

let nextRound client = game client "next_round" []

// Not listed in rpc.discover: card is the 0-based index in the opened pack
let pack client action =
    let ps =
        match action with
        | PickCard(card, targets) ->
            [ "card", int' card ] @ (if targets.IsEmpty then [] else [ "targets", ints targets ])
        | SkipPack -> [ "skip", JsonValue.Create true :> JsonNode ]

    game client "pack" ps

let useConsumable client (index: int) (targets: int list) =
    game
        client
        "use"
        ([ "consumable", int' index ]
         @ (if targets.IsEmpty then [] else [ "cards", ints targets ]))

let rearrange client order =
    let p =
        match order with
        | RearrangeHand xs -> "hand", ints xs
        | RearrangeJokers xs -> "jokers", ints xs
        | RearrangeConsumables xs -> "consumables", ints xs

    game client "rearrange" [ p ]

let addParameters (key: string) = [ "key", JsonValue.Create key :> JsonNode ]

let add client (key: string) = game client "add" (addParameters key)

let setParameters (fields: (string * int) list) =
    fields |> List.map (fun (k, v) -> k, int' v)

let set client (fields: (string * int) list) = game client "set" (setParameters fields)
