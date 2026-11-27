module Balatro.Bot.Sim

open System
open System.Text
open Balatro.Core
open Balatro.Core.Domain

[<assembly: System.Runtime.CompilerServices.InternalsVisibleTo("Balatro.Tests")>]
do ()

type internal Pc = { Id: int; Card: Card }

type private BossInfo =
    { Name: string
      MinAnte: int
      Showdown: bool
      Multiplier: float
      Effect: string }

type internal Run =
    { Rng: Random
      State: Api.State
      Ante: int
      RoundNum: int
      Money: int
      Won: bool
      Seed: string
      Levels: Map<HandType, int>
      Played: Map<HandType, int>
      PlayedRound: Map<HandType, int>
      MostPlayed: HandType
      Deck: Pc list
      Pile: Pc list
      Hand: Pc list
      NextId: int
      Jokers: SimJokers.State list
      Consumables: Api.Card list
      Boss: string
      BossUsed: Map<string, int>
      Next: int
      Cur: int
      HandsLeft: int
      DiscardsLeft: int
      HandsUsed: int
      DiscardsUsed: int
      HandSize: int
      Chips: int
      Reward: int
      Left: int
      Pending: int
      Rerolls: int
      Shop: Api.Card list
      Packs: Api.Card list
      Voucher: Api.Card
      FirstShop: bool
      Pack: Api.Card list
      PackKey: string
      PackLeft: int
      PlayedIds: Set<int>
      LeafLifted: bool
      GrosMichelExtinct: bool }

let private rpc message =
    Api.Rpc
        { Code = -32001
          Name = "BAD_REQUEST"
          Message = message }

type private Check() =
    member _.Bind(r, f) = Result.bind f r
    member _.Return x = Ok x
    member _.ReturnFrom r = r
    member _.Zero() = Ok()

let private check = Check()

let private require (condition: bool) (message: string) =
    if condition then Ok() else Error message

let private baseHands = 4
let private baseDiscards = 4
let private baseHandSize = 8
let private baseJokerSlots = 5
let private consumableSlots = 2
let private baseReroll = 5

// FNV-1a over the UTF-8 bytes, so a seed gives the same run in every process
let private hashSeed (seed: string) : int =
    let mutable h = 2166136261u

    for b in Encoding.UTF8.GetBytes seed do
        h <- (h ^^^ uint32 b) * 16777619u

    int (h &&& 0x7FFFFFFFu)

let private shuffle (rng: Random) (xs: 'a list) : 'a list =
    let a = Array.ofList xs

    for i in a.Length - 1 .. -1 .. 1 do
        let j = rng.Next(i + 1)
        let t = a[i]
        a[i] <- a[j]
        a[j] <- t

    List.ofArray a

let private boss name minAnte effect =
    { Name = name
      MinAnte = minAnte
      Showdown = false
      Multiplier = 2.0
      Effect = effect }

let private bosses =
    [ boss "The Hook" 1 "Discards 2 random cards per hand played"
      boss "The Club" 1 "All Club cards are debuffed"
      boss "The Manacle" 1 "-1 hand size"
      boss "The Psychic" 1 "Must play 5 cards"
      boss "The Goad" 1 "All Spade cards are debuffed"
      boss "The Window" 1 "All Diamond cards are debuffed"
      boss "The Pillar" 1 "Cards played previously this ante are debuffed"
      boss "The Head" 1 "All Heart cards are debuffed"
      boss "The Mouth" 2 "Play only 1 hand type this round"
      boss "The Fish" 2 "Cards drawn face down after each hand played"
      { boss "The Wall" 2 "Extra large blind" with
          Multiplier = 4.0 }
      boss "The House" 2 "First hand is drawn face down"
      boss "The Mark" 2 "All face cards are drawn face down"
      boss "The Wheel" 2 "1 in 7 cards get drawn face down"
      boss "The Arm" 2 "Decrease level of played poker hand"
      boss "The Water" 2 "Start with 0 discards"
      { boss "The Needle" 2 "Play only 1 hand" with
          Multiplier = 1.0 }
      boss "The Flint" 2 "Base Chips and Mult are halved"
      boss "The Tooth" 3 "Lose $1 per card played"
      boss "The Eye" 3 "No repeat hand types this round"
      boss "The Plant" 4 "All face cards are debuffed"
      boss "The Serpent" 5 "After Play or Discard, always draw 3 cards"
      boss "The Ox" 6 "Playing your most played hand sets money to $0"
      { boss "Cerulean Bell" 8 "Forces 1 card to always be selected" with
          Showdown = true }
      { boss "Verdant Leaf" 8 "All cards are debuffed until 1 Joker is sold" with
          Showdown = true }
      { boss "Violet Vessel" 8 "Extra large blind" with
          Showdown = true
          Multiplier = 6.0 }
      { boss "Amber Acorn" 8 "Flips and shuffles all Jokers" with
          Showdown = true }
      { boss "Crimson Heart" 8 "One random Joker is disabled every hand" with
          Showdown = true } ]

let private bossInfo name =
    bosses |> List.find (fun b -> b.Name = name)

let internal chooseBoss (rng: Random) (ante: int) (used: Map<string, int>) : string =
    let pool =
        if ante >= 2 && ante % 8 = 0 then
            bosses |> List.filter (fun b -> b.Showdown)
        else
            bosses |> List.filter (fun b -> not b.Showdown && b.MinAnte <= ante)

    let count (b: BossInfo) =
        used |> Map.tryFind b.Name |> Option.defaultValue 0

    let least = pool |> List.map count |> List.min
    let choices = pool |> List.filter (fun b -> count b = least)
    choices[rng.Next choices.Length].Name

let private handOrders =
    [ FlushFive, 1
      FlushHouse, 2
      FiveOfAKind, 3
      StraightFlush, 4
      FourOfAKind, 5
      FullHouse, 6
      Flush, 7
      Straight, 8
      ThreeOfAKind, 9
      TwoPair, 10
      Pair, 11
      HighCard, 12 ]

let private handTypes = handOrders |> List.map fst

let private allRanks =
    [ Two; Three; Four; Five; Six; Seven; Eight; Nine; Ten; Jack; Queen; King; Ace ]

let private zeroHands = handTypes |> List.map (fun t -> t, 0) |> Map.ofList

let private apiSuit suit =
    match suit with
    | Spades -> Api.Suit.Spades
    | Hearts -> Api.Suit.Hearts
    | Clubs -> Api.Suit.Clubs
    | Diamonds -> Api.Suit.Diamonds

let private apiRank rank =
    match rank with
    | Two -> Api.Rank.Two
    | Three -> Api.Rank.Three
    | Four -> Api.Rank.Four
    | Five -> Api.Rank.Five
    | Six -> Api.Rank.Six
    | Seven -> Api.Rank.Seven
    | Eight -> Api.Rank.Eight
    | Nine -> Api.Rank.Nine
    | Ten -> Api.Rank.Ten
    | Jack -> Api.Rank.Jack
    | Queen -> Api.Rank.Queen
    | King -> Api.Rank.King
    | Ace -> Api.Rank.Ace

let private apiEnhancement e =
    match e with
    | Enhancement.Bonus -> Api.Enhancement.Bonus
    | Enhancement.Mult -> Api.Enhancement.Mult
    | Enhancement.Wild -> Api.Enhancement.Wild
    | Enhancement.Glass -> Api.Enhancement.Glass
    | Enhancement.Steel -> Api.Enhancement.Steel
    | Enhancement.Stone -> Api.Enhancement.Stone
    | Enhancement.Gold -> Api.Enhancement.Gold
    | Enhancement.Lucky -> Api.Enhancement.Lucky

let private apiEdition e =
    match e with
    | Foil -> Api.Edition.Foil
    | Holo -> Api.Edition.Holo
    | Polychrome -> Api.Edition.Polychrome
    | Negative -> Api.Edition.Negative

let private apiSeal s =
    match s with
    | Seal.Red -> Api.Seal.Red
    | Seal.Blue -> Api.Seal.Blue
    | Seal.Gold -> Api.Seal.Gold
    | Seal.Purple -> Api.Seal.Purple

let private suitLetter suit =
    match suit with
    | Spades -> "S"
    | Hearts -> "H"
    | Clubs -> "C"
    | Diamonds -> "D"

let private rankLetter rank =
    match rank with
    | Ten -> "T"
    | Jack -> "J"
    | Queen -> "Q"
    | King -> "K"
    | Ace -> "A"
    | other -> string (Rank.id other)

let private isStone (c: Card) = c.Enhancement = Some Enhancement.Stone

let private hasJoker key (run: Run) =
    run.Jokers |> List.exists (fun j -> j.Card.Key = key)

let private jokerLimit (run: Run) =
    baseJokerSlots
    + (run.Jokers
       |> List.filter (fun j -> j.Card.Modifier.Edition = Some Api.Edition.Negative)
       |> List.length)

let private selecting (run: Run) = run.State = Api.State.SelectingHand

let private bossOf (run: Run) = if run.Cur = 2 then Some run.Boss else None

// Chicot disables the boss; only a blind being played has an active effect
let private activeBoss (run: Run) : string option =
    if selecting run && not (hasJoker "j_chicot" run) then
        bossOf run
    else
        None

let private debuffed (run: Run) (pc: Pc) =
    let c = pc.Card

    let suit s =
        not (isStone c) && (c.Enhancement = Some Enhancement.Wild || c.Suit = s)

    match activeBoss run with
    | Some "The Club" -> suit Clubs
    | Some "The Goad" -> suit Spades
    | Some "The Window" -> suit Diamonds
    | Some "The Head" -> suit Hearts
    | Some "The Plant" ->
        hasJoker "j_pareidolia" run
        || (not (isStone c) && (c.Rank = Jack || c.Rank = Queen || c.Rank = King))
    | Some "The Pillar" -> run.PlayedIds.Contains pc.Id
    | Some "Verdant Leaf" -> not run.LeafLifted
    | _ -> false

let private core (run: Run) (pc: Pc) : Card =
    { pc.Card with Debuffed = debuffed run pc }

let private targetOf (run: Run) (index: int) : int =
    match index with
    | 0 -> int (Blinds.blindRequirement Stake.White run.Ante Blinds.Small)
    | 1 -> int (Blinds.blindRequirement Stake.White run.Ante Blinds.Big)
    | _ -> int (Blinds.blindAmount Stake.White run.Ante * (bossInfo run.Boss).Multiplier)

let private rewardOf (run: Run) (index: int) =
    match index with
    | 0 -> 3
    | 1 -> 4
    | _ -> if run.Ante = 8 then 8 else 5

let internal payout (reward: int) (handsLeft: int) (jokerDollars: int) (money: int) : int =
    reward + handsLeft + jokerDollars + min 5 (max 0 money / 5)

let private showCard (debuff: bool) (pc: Pc) : Api.Card =
    let c = pc.Card
    let stone = isStone c

    { Id = pc.Id
      Key = suitLetter c.Suit + "_" + rankLetter c.Rank
      Set = (if c.Enhancement.IsSome then Api.CardSet.Enhanced else Api.CardSet.Default)
      Label = "Base Card"
      Value =
        { Suit = (if stone then None else Some(apiSuit c.Suit))
          Rank = (if stone then None else Some(apiRank c.Rank))
          Effect = "" }
      Modifier =
        { Eternal = false
          Rental = false
          Perishable = None
          Seal = c.Seal |> Option.map apiSeal
          Edition = c.Edition |> Option.map apiEdition
          Enhancement = c.Enhancement |> Option.map apiEnhancement }
      State =
        { Hidden = false
          Debuff = debuff
          Highlight = false }
      Cost = { Sell = 0; Buy = 1 } }

let private tableOf (run: Run) : SimJokers.Table =
    { Money = run.Money
      Deck = run.Deck |> List.map (fun p -> p.Card)
      DrawPile = (if selecting run then run.Pile.Length else run.Deck.Length)
      JokerSlots = jokerLimit run
      Jokers = run.Jokers |> List.map (fun j -> j.Card.Key)
      HandPlays = run.Played
      Skips = 0 }

let private area limit (cards: Api.Card list) : Api.Area =
    { Count = cards.Length
      Limit = limit
      HighlightedLimit = 0
      Cards = cards }

let private handsView (run: Run) : Map<string, Api.HandInfo> =
    Map.ofList
        [ for t, order in handOrders ->
              let level = handLevel t run.Levels[t]

              HandType.label t,
              ({ Order = order
                 Level = run.Levels[t]
                 Chips = int level.Chips
                 Mult = int level.Mult
                 Played = run.Played[t]
                 PlayedThisRound = run.PlayedRound[t] }: Api.HandInfo) ]

let private view (run: Run) : Api.GameState =
    let inHand = selecting run
    let inRound = inHand || run.State = Api.State.GameOver
    let table = tableOf run

    let status i : Api.BlindStatus =
        match run.State with
        | Api.State.BlindSelect ->
            if i < run.Next then Api.BlindStatus.Defeated
            elif i = run.Next then Api.BlindStatus.Select
            else Api.BlindStatus.Upcoming
        | Api.State.SelectingHand
        | Api.State.GameOver ->
            if i < run.Cur then Api.BlindStatus.Defeated
            elif i = run.Cur then Api.BlindStatus.Current
            else Api.BlindStatus.Upcoming
        | _ -> if i < run.Next then Api.BlindStatus.Defeated else Api.BlindStatus.Upcoming

    let blind i kind name effect : Api.Blind =
        { Type = kind
          Status = status i
          Name = name
          Effect = effect
          Score = targetOf run i
          TagName = None
          TagEffect = None }

    let jokers = run.Jokers |> List.map (SimJokers.card table)
    let packOpen = run.State = Api.State.SmodsBoosterOpened
    let shopOpen = run.State = Api.State.Shop || packOpen

    { State = run.State
      RoundNum = run.RoundNum
      AnteNum = run.Ante
      Money = run.Money
      Won = run.Won
      Deck = Some Api.Deck.Red
      Stake = Some Api.Stake.White
      Seed = Some run.Seed
      Round =
        { HandsPlayed = run.HandsUsed
          Chips = (if inRound || run.State = Api.State.RoundEval then run.Chips else 0)
          RerollCost = baseReroll + run.Rerolls
          HandsLeft = (if inRound then run.HandsLeft else baseHands)
          DiscardsLeft = (if inRound then run.DiscardsLeft else baseDiscards)
          DiscardsUsed = run.DiscardsUsed }
      Hands = handsView run
      Blinds =
        Some
            { Small = blind 0 Api.BlindType.Small "Small Blind" ""
              Big = blind 1 Api.BlindType.Big "Big Blind" ""
              Boss = blind 2 Api.BlindType.Boss run.Boss (bossInfo run.Boss).Effect }
      UsedVouchers = Map.empty
      Hand =
        { Count = (if inHand then run.Hand.Length else 0)
          Limit = run.HandSize
          HighlightedLimit = 5
          Cards = (if inHand then run.Hand |> List.map (fun pc -> showCard (debuffed run pc) pc) else []) }
      Jokers = area (jokerLimit run) jokers
      Consumables = area consumableSlots run.Consumables
      Cards =
        (let source = if inHand then run.Pile else run.Deck
         let cards = source |> List.map (fun pc -> showCard (inHand && debuffed run pc) pc)
         area cards.Length cards)
      Shop = area 2 (if shopOpen then run.Shop else [])
      Vouchers = area 1 (if shopOpen then [ run.Voucher ] else [])
      Packs = area 2 (if shopOpen then run.Packs else [])
      Pack = area run.Pack.Length (if packOpen then run.Pack else []) }

let private menuState: Api.GameState =
    let hands =
        Map.ofList
            [ for t, order in handOrders ->
                  let level = handLevel t 1

                  HandType.label t,
                  ({ Order = order
                     Level = 1
                     Chips = int level.Chips
                     Mult = int level.Mult
                     Played = 0
                     PlayedThisRound = 0 }: Api.HandInfo) ]

    { State = Api.State.Menu
      RoundNum = 0
      AnteNum = 1
      Money = 0
      Won = false
      Deck = None
      Stake = None
      Seed = None
      Round =
        { HandsPlayed = 0
          Chips = 0
          RerollCost = 0
          HandsLeft = 0
          DiscardsLeft = 0
          DiscardsUsed = 0 }
      Hands = hands
      Blinds = None
      UsedVouchers = Map.empty
      Hand = area 0 []
      Jokers = area 0 []
      Consumables = area 0 []
      Cards = area 0 []
      Shop = area 0 []
      Vouchers = area 0 []
      Packs = area 0 []
      Pack = area 0 [] }

let private tag (run: Run) (cards: Api.Card list) : Run * Api.Card list =
    let tagged = cards |> List.mapi (fun i c -> { c with Id = run.NextId + i })
    { run with NextId = run.NextId + cards.Length }, tagged

let private contentView (run: Run) (extra: string list) : SimContent.View =
    { Owned =
        Set.ofList (
            extra
            @ (run.Jokers |> List.map (fun j -> j.Card.Key))
            @ (run.Consumables |> List.map (fun c -> c.Key))
        )
      Deck = run.Deck |> List.map (fun p -> p.Card)
      Played = run.Played |> Map.filter (fun _ n -> n > 0) |> Map.keys |> Set.ofSeq
      GrosMichelExtinct = run.GrosMichelExtinct }

let private restock (run: Run) : Run =
    let run, cards = tag run (SimContent.shopCards run.Rng (contentView run []))
    { run with Shop = cards }

let private stockShop (run: Run) : Run =
    let run = restock run
    let run, packs = tag run (SimContent.packs run.Rng run.FirstShop)

    { run with
        Packs = packs
        FirstShop = false
        Rerolls = 0 }

let private broadcastOn
    (rng: Random)
    (table: SimJokers.Table)
    (jokers: SimJokers.State list)
    (event: SimJokers.Event)
    : SimJokers.State list * int =
    let results = jokers |> List.map (SimJokers.step rng table event)
    results |> List.choose fst, results |> List.sumBy snd

let private broadcast (event: SimJokers.Event) (run: Run) : Run * int =
    let survivors, dollars = broadcastOn run.Rng (tableOf run) run.Jokers event
    let lost key = hasJoker key run && not (survivors |> List.exists (fun j -> j.Card.Key = key))

    { run with
        Jokers = survivors
        GrosMichelExtinct = run.GrosMichelExtinct || lost "j_gros_michel" },
    dollars

let private fire (event: SimJokers.Event) (run: Run) : Run =
    let next, dollars = broadcast event run
    { next with Money = next.Money + dollars }

let private removeCards (pcs: Pc list) (run: Run) : Run =
    let gone = pcs |> List.map (fun p -> p.Id) |> Set.ofList
    let keep (p: Pc) = not (gone.Contains p.Id)

    { run with
        Deck = run.Deck |> List.filter keep
        Hand = run.Hand |> List.filter keep
        Pile = run.Pile |> List.filter keep }

let private destroy (pcs: Pc list) (run: Run) : Run =
    removeCards pcs run |> fire (SimJokers.CardsDestroyed(pcs |> List.map (core run)))

let private draw (n: int) (run: Run) : Run =
    let taken, rest = List.splitAt (min n run.Pile.Length) run.Pile

    { run with
        Hand = run.Hand @ taken
        Pile = rest }

let private refill (run: Run) : Run =
    let serpent =
        activeBoss run = Some "The Serpent" && (run.HandsUsed > 0 || run.DiscardsUsed > 0)

    draw (if serpent then 3 else max 0 (run.HandSize - run.Hand.Length)) run

let private endIfEmpty (run: Run) : Run =
    if run.Hand.IsEmpty then
        { run with State = Api.State.GameOver }
    else
        run

let private pickRandom (rng: Random) (xs: 'a list) = xs[rng.Next xs.Length]

let private dropRandom (n: int) (rng: Random) (xs: 'a list) : 'a list =
    (xs, [ 1..n ])
    ||> List.fold (fun rest _ ->
        if rest.IsEmpty then
            rest
        else
            List.removeAt (rng.Next rest.Length) rest)

let private validPick (verb: string) (count: int) (picked: int list) =
    if picked.IsEmpty then
        Error(sprintf "You must %s at least 1 card" verb)
    elif picked.Length > 5 then
        Error(sprintf "You can only %s 5 cards" verb)
    elif List.distinct picked <> picked then
        Error "Duplicate card index"
    elif picked |> List.exists (fun i -> i < 0 || i >= count) then
        Error "Card index out of range"
    else
        Ok()

let private winRound (run: Run) : Run =
    let bossBlind = run.Cur = 2
    let reward = rewardOf run run.Cur

    let gold =
        run.Hand
        |> List.filter (fun pc -> pc.Card.Enhancement = Some Enhancement.Gold && not (debuffed run pc))
        |> List.length

    let run = { run with Money = run.Money + 3 * gold }
    let run, dollars = broadcast (SimJokers.RoundWon(bossBlind, run.DiscardsLeft)) run

    let gifts =
        run.Jokers
        |> List.indexed
        |> List.filter (fun (_, j) -> j.Card.Key = "j_gift")
        |> List.map fst

    let bump n (c: Api.Card) =
        { c with Cost = { c.Cost with Sell = c.Cost.Sell + n } }

    let run =
        { run with
            Jokers =
                run.Jokers
                |> List.mapi (fun i j ->
                    { j with Card = bump (gifts |> List.filter ((<>) i) |> List.length) j.Card })
            Consumables = run.Consumables |> List.map (bump gifts.Length) }

    let finished =
        { run with
            State = Api.State.RoundEval
            Hand = []
            Reward = reward
            Left = run.HandsLeft
            Pending = dollars }

    if bossBlind then
        let mostPlayed =
            handOrders
            |> List.sortBy (fun (t, order) -> -run.Played[t], order)
            |> List.head
            |> fst

        let ante = run.Ante + 1
        let used = run.BossUsed
        let next = chooseBoss run.Rng ante used
        let voucher = SimContent.voucher run.Rng Set.empty
        let finished, tagged = tag finished [ voucher ]

        { finished with
            Ante = ante
            Won = finished.Won || ante > 8
            MostPlayed = mostPlayed
            PlayedIds = Set.empty
            Boss = next
            BossUsed = Map.change next (fun n -> Some(defaultArg n 0 + 1)) used
            Voucher = tagged.Head
            Next = 0 }
    else
        { finished with Next = run.Cur + 1 }

let private playHand (idx: int list) (run: Run) : Run =
    let g = view run
    let activeName = activeBoss run
    let hook = activeName = Some "The Hook"
    let playedPcs = idx |> List.map (fun i -> run.Hand[i])

    let heldAll =
        run.Hand |> List.indexed |> List.filter (fun (i, _) -> not (List.contains i idx)) |> List.map snd

    let heldPcs = if hook then dropRandom 2 run.Rng heldAll else heldAll
    let played = playedPcs |> List.map (core run)
    let held = heldPcs |> List.map (core run)
    let situation = Mapping.situation g
    let situation = if hook then { situation with Boss = None } else situation

    let breakdown =
        Scoring.scoreWith situation { Played = played; Held = held; Levels = Mapping.levels g }

    let blocked =
        breakdown.Steps |> List.exists (fun s -> s.Source.StartsWith "Not allowed")

    let hand = breakdown.HandType
    let score = int (min breakdown.Total 2.0e9)

    let toll = if activeName = Some "The Tooth" then played.Length else 0
    let run1 = { run with Money = run.Money - toll }

    let run1 =
        if activeName = Some "The Ox" && hand = run.MostPlayed then
            { run1 with Money = 0 }
        else
            run1

    let scoringPcs =
        (([], breakdown.ScoringCards), List.zip playedPcs played)
        ||> List.fold (fun (acc, remaining) (pc, c) ->
            match List.tryFindIndex ((=) c) remaining with
            | Some k -> acc @ [ pc ], List.removeAt k remaining
            | None -> acc, remaining)
        |> fst
        |> List.filter (fun pc -> not (debuffed run pc))

    let strip =
        if not blocked && hasJoker "j_vampire" run then
            scoringPcs |> List.filter (fun pc -> pc.Card.Enhancement.IsSome)
        else
            []

    let rolling =
        scoringPcs |> List.filter (fun pc -> not (strip |> List.exists (fun s -> s.Id = pc.Id)))

    let sealed' =
        if blocked then 0 else scoringPcs |> List.filter (fun pc -> pc.Card.Seal = Some Seal.Gold) |> List.length

    let lucky =
        if blocked then
            0
        else
            rolling
            |> List.filter (fun pc -> pc.Card.Enhancement = Some Enhancement.Lucky)
            |> List.filter (fun _ -> run.Rng.Next 15 = 0)
            |> List.length

    let broken =
        if blocked then
            []
        else
            rolling
            |> List.filter (fun pc -> pc.Card.Enhancement = Some Enhancement.Glass)
            |> List.filter (fun _ -> run.Rng.Next 4 = 0)

    let run2 = { run1 with Money = run1.Money + 3 * sealed' + 20 * lucky }

    let run3 =
        if blocked then
            run2
        else
            fire (SimJokers.HandPlayed(hand, played, breakdown.ScoringCards, held)) run2

    let stripped =
        { run3 with
            Deck =
                run3.Deck
                |> List.map (fun pc ->
                    if strip |> List.exists (fun s -> s.Id = pc.Id) then
                        { pc with Card = { pc.Card with Enhancement = None } }
                    else
                        pc) }

    let lowered =
        if activeName = Some "The Arm" && stripped.Levels[hand] > 1 then
            Map.add hand (stripped.Levels[hand] - 1) stripped.Levels
        else
            stripped.Levels

    let bump m = Map.add hand (Map.find hand m + 1) m

    let after =
        { stripped with
            Levels = lowered
            Played = bump stripped.Played
            PlayedRound = bump stripped.PlayedRound
            Chips = int (min (int64 stripped.Chips + int64 score) 2000000000L)
            HandsLeft = stripped.HandsLeft - 1
            HandsUsed = stripped.HandsUsed + 1
            Hand = heldPcs
            PlayedIds = (stripped.PlayedIds, playedPcs) ||> List.fold (fun s pc -> s.Add pc.Id) }

    let after = if broken.IsEmpty then after else destroy broken after

    if after.Chips >= targetOf after after.Cur then winRound after
    elif after.HandsLeft = 0 then { after with State = Api.State.GameOver }
    else after |> refill |> endIfEmpty

let private discardHand (idx: int list) (run: Run) : Run =
    let pcs = idx |> List.map (fun i -> run.Hand[i])
    let cards = pcs |> List.map (core run)
    let first = run.DiscardsUsed = 0

    let run1 =
        { run with
            Hand = run.Hand |> List.indexed |> List.filter (fun (i, _) -> not (List.contains i idx)) |> List.map snd
            DiscardsLeft = run.DiscardsLeft - 1
            DiscardsUsed = run.DiscardsUsed + 1 }

    let run2 = fire (SimJokers.Discarded cards) run1

    let run3 =
        if first && pcs.Length = 1 && hasJoker "j_trading" run then
            destroy pcs run2
        else
            run2

    run3 |> refill |> endIfEmpty

let private select (run: Run) : Result<Run, string> =
    check {
        do! require (run.State = Api.State.BlindSelect) "No blind to select"
        let bossBlind = run.Next = 2
        let active = bossBlind && not (hasJoker "j_chicot" run)
        let isBoss name = active && run.Boss = name

        let started =
            { run with
                State = Api.State.SelectingHand
                Cur = run.Next
                RoundNum = run.RoundNum + 1
                HandsLeft = (if isBoss "The Needle" then 1 else baseHands)
                DiscardsLeft = (if isBoss "The Water" then 0 else baseDiscards)
                HandSize = (if isBoss "The Manacle" then baseHandSize - 1 else baseHandSize)
                HandsUsed = 0
                DiscardsUsed = 0
                Chips = 0
                PlayedRound = zeroHands
                Rerolls = 0
                LeafLifted = false
                Hand = [] }

        let started = { started with Pile = shuffle started.Rng started.Deck }
        let run1 = fire (SimJokers.BlindSelected bossBlind) started

        let n = run1.Jokers.Length

        let daggers =
            run1.Jokers
            |> List.mapi (fun i j -> i, j)
            |> List.filter (fun (i, j) -> j.Card.Key = "j_ceremonial" && i + 1 < n)
            |> List.map (fun (i, _) -> i + 1)

        let madness =
            run1.Jokers
            |> List.mapi (fun i j -> i, j)
            |> List.filter (fun (_, j) -> j.Card.Key = "j_madness" && not bossBlind)
            |> List.map fst

        let victims =
            (daggers, madness)
            ||> List.fold (fun gone i ->
                match [ 0 .. n - 1 ] |> List.filter (fun k -> k <> i && not (List.contains k gone)) with
                | [] -> gone
                | choices -> pickRandom run1.Rng choices :: gone)

        let run2 =
            { run1 with
                Jokers = run1.Jokers |> List.indexed |> List.filter (fun (i, _) -> not (List.contains i victims)) |> List.map snd }

        return run2 |> refill
    }

let private play (picked: int list) (run: Run) : Result<Run, string> =
    check {
        do! require (selecting run) "Not selecting a hand"
        do! validPick "play" run.Hand.Length picked
        do! require (run.HandsLeft > 0) "No hands left"
        return playHand (List.sort picked) run
    }

let private discard (picked: int list) (run: Run) : Result<Run, string> =
    check {
        do! require (selecting run) "Not selecting a hand"
        do! validPick "discard" run.Hand.Length picked
        do! require (run.DiscardsLeft > 0) "No discards left"
        return discardHand (List.sort picked) run
    }

let private cashOut (run: Run) : Result<Run, string> =
    check {
        do! require (run.State = Api.State.RoundEval) "Not at the round evaluation"
        let paid = run.Money + payout run.Reward run.Left run.Pending run.Money

        return
            stockShop
                { run with
                    State = Api.State.Shop
                    Money = paid
                    Chips = 0
                    HandsUsed = 0
                    DiscardsUsed = 0
                    HandSize = baseHandSize
                    Left = 0
                    Reward = 0
                    Pending = 0 }
    }

let private nextRound (run: Run) : Result<Run, string> =
    check {
        do! require (run.State = Api.State.Shop) "Not in the shop"
        return { run with State = Api.State.BlindSelect }
    }

let private takeSlot (run: Run) (card: Api.Card) =
    match card.Set with
    | Api.CardSet.Joker ->
        require
            (run.Jokers.Length < jokerLimit run || card.Modifier.Edition = Some Api.Edition.Negative)
            "No free joker slot"
    | Api.CardSet.Planet
    | Api.CardSet.Tarot
    | Api.CardSet.Spectral -> require (run.Consumables.Length < consumableSlots) "No free consumable slot"
    | _ -> Error "That card cannot be bought"

let private addCard (card: Api.Card) (run: Run) : Run =
    match card.Set with
    | Api.CardSet.Joker ->
        let state = SimJokers.create run.Rng (tableOf run) card
        { run with Jokers = run.Jokers @ [ state ] }
    | _ -> { run with Consumables = run.Consumables @ [ card ] }

let private buyCard (i: int) (run: Run) : Result<Run, string> =
    check {
        do! require (run.State = Api.State.Shop) "Not in the shop"
        do! require (i >= 0 && i < run.Shop.Length) "No such shop card"
        let card = run.Shop[i]
        do! require (run.Money >= card.Cost.Buy) "Not enough money"
        do! takeSlot run card

        return
            { run with
                Money = run.Money - card.Cost.Buy
                Shop = List.removeAt i run.Shop }
            |> addCard card
    }

let private buyPack (i: int) (run: Run) : Result<Run, string> =
    check {
        do! require (run.State = Api.State.Shop) "Not in the shop"
        do! require (i >= 0 && i < run.Packs.Length) "No such pack"
        let pack = run.Packs[i]
        do! require (run.Money >= pack.Cost.Buy) "Not enough money"

        let contents =
            SimContent.packContents run.Rng (contentView run (run.Shop |> List.map (fun c -> c.Key))) pack.Key

        let run, tagged = tag run contents

        return
            { run with
                State = Api.State.SmodsBoosterOpened
                Money = run.Money - pack.Cost.Buy
                Packs = List.removeAt i run.Packs
                Pack = tagged
                PackKey = pack.Key
                PackLeft = SimContent.packChoose pack.Key }
    }

let private closePack (run: Run) : Run =
    { run with
        State = Api.State.Shop
        Pack = []
        PackKey = ""
        PackLeft = 0 }

let private levelUp (hand: HandType) (run: Run) : Run =
    { run with Levels = Map.add hand (run.Levels[hand] + 1) run.Levels }

let private planetHand key =
    match key with
    | "c_pluto" -> Some HighCard
    | "c_mercury" -> Some Pair
    | "c_uranus" -> Some TwoPair
    | "c_venus" -> Some ThreeOfAKind
    | "c_saturn" -> Some Straight
    | "c_jupiter" -> Some Flush
    | "c_earth" -> Some FullHouse
    | "c_mars" -> Some FourOfAKind
    | "c_neptune" -> Some StraightFlush
    | "c_planet_x" -> Some FiveOfAKind
    | "c_ceres" -> Some FlushHouse
    | "c_eris" -> Some FlushFive
    | _ -> None

let private usePlanet (card: Api.Card) (run: Run) : Run =
    let leveled =
        match planetHand card.Key with
        | Some hand -> levelUp hand run
        | None -> run

    fire (SimJokers.ConsumableUsed Api.CardSet.Planet) leveled

let private pickCard (i: int) (run: Run) : Result<Run, string> =
    check {
        do! require (run.State = Api.State.SmodsBoosterOpened) "No pack is open"
        do! require (i >= 0 && i < run.Pack.Length) "No such pack card"
        let card = run.Pack[i]

        do!
            match card.Set with
            | Api.CardSet.Planet
            | Api.CardSet.Default
            | Api.CardSet.Enhanced -> Ok()
            | _ -> takeSlot run card

        let taken = { run with Pack = List.removeAt i run.Pack; PackLeft = run.PackLeft - 1 }

        let added =
            match card.Set with
            | Api.CardSet.Planet -> usePlanet card taken
            | Api.CardSet.Default
            | Api.CardSet.Enhanced ->
                let pc = { Id = card.Id; Card = (Mapping.toCard card).Value }

                { taken with Deck = taken.Deck @ [ pc ] }
                |> fire (SimJokers.CardsAdded [ pc.Card ])
            | _ -> addCard card taken

        return if added.PackLeft = 0 || added.Pack.IsEmpty then closePack added else added
    }

let private skipPack (run: Run) : Result<Run, string> =
    check {
        do! require (run.State = Api.State.SmodsBoosterOpened) "No pack is open"
        return closePack run |> fire SimJokers.PackSkipped
    }

let private reroll (run: Run) : Result<Run, string> =
    check {
        let cost = baseReroll + run.Rerolls
        do! require (run.State = Api.State.Shop) "Not in the shop"
        do! require (run.Money >= cost) "Not enough money"

        let paid =
            { run with
                Money = run.Money - cost
                Rerolls = run.Rerolls + 1 }

        return paid |> fire SimJokers.Rerolled |> restock
    }

let private sellJoker (i: int) (run: Run) : Result<Run, string> =
    check {
        do! require (selecting run || run.State = Api.State.Shop || run.State = Api.State.BlindSelect) "Cannot sell now"
        do! require (i >= 0 && i < run.Jokers.Length) "No such joker"
        let sold = run.Jokers[i]
        let table = tableOf run
        let rest = List.removeAt i run.Jokers
        let survivors, dollars = broadcastOn run.Rng table rest (SimJokers.Sold sold.Card.Key)
        let price = (SimJokers.card table sold).Cost.Sell

        return
            { run with
                Jokers = survivors
                Money = run.Money + price + dollars
                LeafLifted = run.LeafLifted || (selecting run && bossOf run = Some "Verdant Leaf") }
    }

let private sellConsumable (i: int) (run: Run) : Result<Run, string> =
    check {
        do! require (selecting run || run.State = Api.State.Shop || run.State = Api.State.BlindSelect) "Cannot sell now"
        do! require (i >= 0 && i < run.Consumables.Length) "No such consumable"
        let sold = run.Consumables[i]

        return
            { run with
                Consumables = List.removeAt i run.Consumables
                Money = run.Money + sold.Cost.Sell }
            |> fire (SimJokers.Sold sold.Key)
    }

let private apply (effect: Tarot.Effect) (targets: int list) (run: Run) : Result<Run, string> =
    let count =
        match effect with
        | Tarot.Enhance(_, n)
        | Tarot.ToSuit(_, n)
        | Tarot.Destroy n -> n
        | Tarot.Money -> 0

    check {
        do! require (selecting run) "Cards must be selected in a hand"
        do! require (not targets.IsEmpty && targets.Length <= count) "Wrong number of target cards"
        do! validPick "use" run.Hand.Length targets
        let chosen = targets |> List.map (fun i -> run.Hand[i])
        let ids = chosen |> List.map (fun p -> p.Id) |> Set.ofList

        let change f =
            let edit (pc: Pc) =
                if ids.Contains pc.Id then { pc with Card = f pc.Card } else pc

            { run with
                Deck = run.Deck |> List.map edit
                Hand = run.Hand |> List.map edit }

        return
            match effect with
            | Tarot.Enhance(e, _) -> change (fun c -> { c with Enhancement = Some e })
            | Tarot.ToSuit(suit, _) -> change (fun c -> { c with Suit = suit })
            | Tarot.Destroy _ -> destroy chosen run
            | Tarot.Money -> run
    }

let private useConsumable (i: int) (targets: int list) (run: Run) : Result<Run, string> =
    check {
        do! require (selecting run || run.State = Api.State.Shop) "Cannot use a consumable now"
        do! require (i >= 0 && i < run.Consumables.Length) "No such consumable"
        let card = run.Consumables[i]
        let without = { run with Consumables = List.removeAt i run.Consumables }

        match card.Set, Tarot.effect card.Key with
        | Api.CardSet.Planet, _ -> return usePlanet card without
        | Api.CardSet.Tarot, Some Tarot.Money ->
            let gain =
                if card.Key = "c_hermit" then
                    min (max 0 run.Money) 20
                else
                    min 50 (run.Jokers |> List.sumBy (fun j -> (SimJokers.card (tableOf run) j).Cost.Sell))

            return
                { without with Money = without.Money + gain }
                |> fire (SimJokers.ConsumableUsed Api.CardSet.Tarot)
        | Api.CardSet.Tarot, Some effect ->
            let! applied = apply effect targets without
            return applied |> fire (SimJokers.ConsumableUsed Api.CardSet.Tarot)
        | set, _ -> return fire (SimJokers.ConsumableUsed set) without
    }

let private permute (order: int list) (items: 'a list) : Result<'a list, string> =
    if List.sort order = [ 0 .. items.Length - 1 ] then
        Ok(order |> List.map (fun i -> items[i]))
    else
        Error "Not a permutation of the cards"

let private rearrange (order: Api.Rearrangement) (run: Run) : Result<Run, string> =
    check {
        match order with
        | Api.RearrangeHand xs ->
            do! require (selecting run) "Not selecting a hand"
            let! hand = permute xs run.Hand
            return { run with Hand = hand }
        | Api.RearrangeJokers xs ->
            let! jokers = permute xs run.Jokers
            return { run with Jokers = jokers }
        | Api.RearrangeConsumables xs ->
            let! consumables = permute xs run.Consumables
            return { run with Consumables = consumables }
    }

let internal newRun (seed: string) : Run =
    let rng = Random(hashSeed seed)

    let deck =
        [ for suit in [ Hearts; Diamonds; Clubs; Spades ] do
              for rank in allRanks ->
                  { Rank = rank
                    Suit = suit
                    Enhancement = None
                    Edition = None
                    Seal = None
                    Debuffed = false } ]
        |> List.mapi (fun i card -> { Id = i + 1; Card = card })

    let first = chooseBoss rng 1 Map.empty
    let voucher = SimContent.voucher rng Set.empty

    { Rng = rng
      State = Api.State.BlindSelect
      Ante = 1
      RoundNum = 0
      Money = 4
      Won = false
      Seed = seed
      Levels = zeroHands |> Map.map (fun _ _ -> 1)
      Played = zeroHands
      PlayedRound = zeroHands
      MostPlayed = HighCard
      Deck = deck
      Pile = []
      Hand = []
      NextId = 54
      Jokers = []
      Consumables = []
      Boss = first
      BossUsed = Map [ first, 1 ]
      Next = 0
      Cur = 0
      HandsLeft = baseHands
      DiscardsLeft = baseDiscards
      HandsUsed = 0
      DiscardsUsed = 0
      HandSize = baseHandSize
      Chips = 0
      Reward = 0
      Left = 0
      Pending = 0
      Rerolls = 0
      Shop = []
      Packs = []
      Voucher = { voucher with Id = 53 }
      FirstShop = true
      Pack = []
      PackKey = ""
      PackLeft = 0
      PlayedIds = Set.empty
      LeafLifted = false
      GrosMichelExtinct = false }

let internal gameFrom (initial: Run option) : Game.Game =
    let cell = ref initial

    let act (f: Run -> Result<Run, string>) =
        match cell.Value with
        | None -> Error(rpc "No run in progress")
        | Some run ->
            match f run with
            | Ok next ->
                cell.Value <- Some next
                Ok(view next)
            | Error message -> Error(rpc message)

    { Menu =
        fun () ->
            cell.Value <- None
            Ok menuState
      Start =
        fun deck stake seed ->
            match cell.Value, deck, stake with
            | None, Api.Deck.Red, Api.Stake.White ->
                let run = newRun (defaultArg seed "")
                cell.Value <- Some run
                Ok(view run)
            | None, _, _ -> Error(rpc "Only the Red Deck on White Stake is simulated")
            | _ -> Error(rpc "A run is already in progress")
      State =
        fun () ->
            match cell.Value with
            | None -> Ok menuState
            | Some run -> Ok(view run)
      Select = fun () -> act select
      Play = fun picked -> act (play picked)
      Discard = fun picked -> act (discard picked)
      Buy =
        fun target ->
            match target with
            | Api.BuyCard i -> act (buyCard i)
            | Api.BuyPack i -> act (buyPack i)
            | Api.BuyVoucher _ -> Error(rpc "Vouchers are not simulated")
      Sell =
        fun target ->
            match target with
            | Api.SellJoker i -> act (sellJoker i)
            | Api.SellConsumable i -> act (sellConsumable i)
      Reroll = fun () -> act reroll
      CashOut = fun () -> act cashOut
      NextRound = fun () -> act nextRound
      Pack =
        fun action ->
            match action with
            | Api.PickCard(i, _) -> act (pickCard i)
            | Api.SkipPack -> act skipPack
      Use = fun i targets -> act (useConsumable i targets)
      Rearrange = fun order -> act (rearrange order) }

let create () : Game.Game = gameFrom None
