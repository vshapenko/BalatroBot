module Balatro.Bot.PlayNet

open System
open System.Collections.Generic
open System.Globalization
open System.IO
open System.Runtime.InteropServices
open System.Text.Json
open System.Text.RegularExpressions
open System.Threading
open Balatro.Core.Domain
open Balatro.Core.Jokers

// A sparse matrix grouped by row or by column: group g holds Index/Vals from Start[g] to Start[g + 1] - 1
type private Sparse =
    { Start: int[]
      Index: int[]
      Vals: float[]
      Active: int[] }

type private Head =
    { Wq: Sparse
      Wk: Sparse
      Wv: Sparse
      Wo: Sparse }

type private Limits =
    { Mult: float
      Chips: float
      Weight: int
      MultCap: float
      ChipsCap: float
      ProductCap: float
      Doubles: int
      Halves: int
      Passes: int
      HeldHalves: int
      HeldPasses: int
      Jokers: int
      Factor: float
      Value: float
      Left: int
      Plays: int }

type Net =
    private
        { D: int
          Loops: int
          Limits: Limits
          Keys: Map<string, string>
          Main: Set<string>
          Factors: Set<string>
          Uncommon: Set<string>
          AddsMult: Set<string>
          AddsChips: Set<string>
          Dims: Map<string, int>
          Heads: Head[]
          W1: Sparse
          B1: float[]
          Fresh: int[]
          Readers: int[][]
          W2: Sparse
          W1Rows: Sparse
          W2Rows: Sparse
          StartOut: float[]
          IsFresh: bool[]
          Inputs: int[][]
          At: int[][]
          Affects: int[][]
          Sink: bool[] }

let private cardSlots = 8
let private positions = 5

let private handTypes =
    [ FlushFive, "FlushFive"
      FlushHouse, "FlushHouse"
      FiveOfAKind, "FiveOfAKind"
      StraightFlush, "StraightFlush"
      FourOfAKind, "FourOfAKind"
      FullHouse, "FullHouse"
      Flush, "Flush"
      Straight, "Straight"
      ThreeOfAKind, "ThreeOfAKind"
      TwoPair, "TwoPair"
      Pair, "Pair"
      HighCard, "HighCard" ]

let private suitName suit =
    match suit with
    | Spades -> "S"
    | Hearts -> "H"
    | Clubs -> "C"
    | Diamonds -> "D"

let private sparse (groups: int) (byColumn: bool) (triplets: JsonElement) =
    let entries =
        [| for t in triplets.EnumerateArray() ->
               let row, col = t[0].GetInt32(), t[1].GetInt32()
               (if byColumn then col, row else row, col), t[2].GetDouble() |]
        |> Array.sortBy fst

    let start = Array.zeroCreate (groups + 1)

    for (group, _), _ in entries do
        start[group + 1] <- start[group + 1] + 1

    for g in 0 .. groups - 1 do
        start[g + 1] <- start[g + 1] + start[g]

    { Start = start
      Index = entries |> Array.map (fun ((_, other), _) -> other)
      Vals = entries |> Array.map snd
      Active = [| for g in 0 .. groups - 1 do if start[g + 1] > start[g] then g |] }

let private rowsOf (triplets: JsonElement) =
    1 + (triplets.EnumerateArray() |> Seq.map (fun t -> t[0].GetInt32()) |> Seq.max)

let private dense (size: int) (pairs: JsonElement) =
    let v = Array.zeroCreate size

    for p in pairs.EnumerateArray() do
        v[p[0].GetInt32()] <- p[1].GetDouble()

    v

let load (path: string) : Net =
    use doc = JsonDocument.Parse(File.ReadAllText path)
    let root = doc.RootElement
    let d = root.GetProperty("d").GetInt32()
    let hidden = root.GetProperty("hidden").GetInt32()
    let limits = root.GetProperty "limits"

    let head (h: JsonElement) =
        let wq = h.GetProperty "Wq"
        let wv = h.GetProperty "Wv"

        { Wq = sparse (rowsOf wq) false wq
          Wk = sparse (rowsOf wq) false (h.GetProperty "Wk")
          Wv = sparse (rowsOf wv) false wv
          Wo = sparse d false (h.GetProperty "Wo") }

    let heads = [| for h in root.GetProperty("heads").EnumerateArray() -> head h |]
    let b1 = dense hidden (root.GetProperty "b1")
    let w1 = sparse d true (root.GetProperty "W1")
    let w2 = sparse hidden true (root.GetProperty "W2")
    let w1Rows = sparse hidden false (root.GetProperty "W1")
    let w2Rows = sparse d false (root.GetProperty "W2")
    let dims = [ for p in root.GetProperty("dims").EnumerateObject() -> p.Name, p.Value.GetInt32() ] |> Map.ofList
    let fresh = heads |> Array.collect (fun h -> h.Wo.Active) |> Array.distinct
    let unitsOf (r: int) = w1.Index[w1.Start[r] .. w1.Start[r + 1] - 1]
    let outputsOf (h: int) = w2.Index[w2.Start[h] .. w2.Start[h + 1] - 1]
    let inputsOf (h: int) = w1Rows.Index[w1Rows.Start[h] .. w1Rows.Start[h + 1] - 1]
    let startOut = Array.zeroCreate d

    // with x = 0, only the units with a positive bias are active
    for h in 0 .. hidden - 1 do
        if b1[h] > 0.0 then
            for k in w2.Start[h] .. w2.Start[h + 1] - 1 do
                startOut[w2.Index[k]] <- startOut[w2.Index[k]] + w2.Vals[k] * b1[h]

    // the dims the selection head writes on a candidate, whose query there is 0, and every dim computed from them
    let sink = Array.zeroCreate d
    let rec spread (r: int) =
        if not sink[r] then
            sink[r] <- true

            for h in unitsOf r do
                for o in outputsOf h do
                    spread o

    for h in heads do
        if Array.contains dims["isSel"] h.Wq.Index then
            Array.iter spread h.Wo.Active

    { D = d
      Loops = root.GetProperty("loops").GetInt32()
      Limits =
        { Mult = limits.GetProperty("mult").GetDouble()
          Chips = limits.GetProperty("chips").GetDouble()
          Weight = limits.GetProperty("weight").GetInt32()
          MultCap = limits.GetProperty("multCap").GetDouble()
          ChipsCap = limits.GetProperty("chipsCap").GetDouble()
          ProductCap = limits.GetProperty("productCap").GetDouble()
          Doubles = limits.GetProperty("doubles").GetInt32()
          Halves = limits.GetProperty("halves").GetInt32()
          Passes = limits.GetProperty("passes").GetInt32()
          HeldHalves = limits.GetProperty("heldHalves").GetInt32()
          HeldPasses = limits.GetProperty("heldPasses").GetInt32()
          Jokers = limits.GetProperty("jokers").GetInt32()
          Factor = limits.GetProperty("factor").GetDouble()
          Value = limits.GetProperty("value").GetDouble()
          Left = limits.GetProperty("left").GetInt32()
          Plays = limits.GetProperty("plays").GetInt32() }
      Keys = [ for p in root.GetProperty("keys").EnumerateObject() -> p.Name, p.Value.GetString() ] |> Map.ofList
      Main = [ for k in root.GetProperty("main").EnumerateArray() -> k.GetString() ] |> Set.ofList
      Factors = [ for k in root.GetProperty("factors").EnumerateArray() -> k.GetString() ] |> Set.ofList
      Uncommon = [ for k in root.GetProperty("uncommon").EnumerateArray() -> k.GetString() ] |> Set.ofList
      AddsMult = [ for k in root.GetProperty("addsMult").EnumerateArray() -> k.GetString() ] |> Set.ofList
      AddsChips = [ for k in root.GetProperty("addsChips").EnumerateArray() -> k.GetString() ] |> Set.ofList
      Dims = dims
      Heads = heads
      W1 = w1
      B1 = b1
      Fresh = fresh
      Readers =
        Array.init d (fun r ->
            [| for i in 0 .. heads.Length - 1 do
                   let h = heads[i]
                   if Array.contains r h.Wq.Index || Array.contains r h.Wk.Index || Array.contains r h.Wv.Index then
                       i |])
      W2 = w2
      W1Rows = w1Rows
      W2Rows = w2Rows
      StartOut = startOut
      IsFresh = Array.init d (fun r -> Array.contains r fresh)
      Inputs = Array.init d (fun r -> w2Rows.Index[w2Rows.Start[r] .. w2Rows.Start[r + 1] - 1] |> Array.collect inputsOf |> Array.distinct)
      At =
        Array.init d (fun r ->
            let inputs = w2Rows.Index[w2Rows.Start[r] .. w2Rows.Start[r + 1] - 1] |> Array.collect inputsOf |> Array.distinct

            w2Rows.Index[w2Rows.Start[r] .. w2Rows.Start[r + 1] - 1]
            |> Array.collect inputsOf
            |> Array.map (fun i -> Array.IndexOf(inputs, i)))
      Affects = Array.init d (fun r -> unitsOf r |> Array.collect outputsOf |> Array.distinct)
      Sink = sink }

let rec private combos k start n =
    if k = 0 then
        [ [] ]
    else
        [ for i in start .. n - k do
              for rest in combos (k - 1) (i + 1) n -> i :: rest ]

// Every set of one to five positions, by size, then in order: the order of Search.plays
let private subsetsOf n =
    [| for k in 1..5 do
           for c in combos k 0 n -> c |]

let private isWhole (v: float) = v = Math.Floor v

let private isStone (c: Card) = c.Enhancement = Some Enhancement.Stone

let private suitOfName (name: string) =
    match name.TrimEnd 's' with
    | "Spade" -> Some Spades
    | "Heart" -> Some Hearts
    | "Club" -> Some Clubs
    | "Diamond" -> Some Diamonds
    | _ -> None

let private allRanks =
    [ Two; Three; Four; Five; Six; Seven; Eight; Nine; Ten; Jack; Queen; King; Ace ]

// The suit an Ancient Joker names, and the rank and suit an Idol names, read from the joker text; None if it does not parse
let private ancientSuit (joker: Joker) =
    let m = Regex.Match(joker.Text, @"with (\w+) suit")
    if m.Success then suitOfName m.Groups[1].Value else None

let private idolTarget (joker: Joker) =
    let m = Regex.Match(joker.Text, @"played (\w+) of (\w+) gives")

    if m.Success then
        match List.tryFind (fun rank -> Rank.label rank = m.Groups[1].Value) allRanks, suitOfName m.Groups[2].Value with
        | Some rank, Some suit -> Some(rank, suit)
        | _ -> None
    else
        None

// The number a joker text starts with after a prefix, 0 when it is not readable (JokersCounter.leading)
let private leading (prefix: string) (joker: Joker) =
    let m = Regex.Match(joker.Text, "^" + Regex.Escape prefix + @"(\d+(?:\.\d+)?)")

    if m.Success then
        Double.Parse(m.Groups[1].Value, CultureInfo.InvariantCulture)
    else
        0.0

// The number a joker adds: its value, or for Ice Cream and Popcorn the number in their text
let private valueOf (joker: Joker) =
    match joker.Key with
    | "j_ice_cream"
    | "j_popcorn" -> leading "+" joker
    | _ -> Option.defaultValue 0.0 joker.Value

// The factor a joker multiplies mult by, from its value or text as the reference derives it (1 for no effect);
// None for a joker that does not multiply by an input
let private factorOf (net: Net) (joker: Joker) : float option =
    let times factor = if factor > 1.0 then factor else 1.0

    match joker.Key with
    | "j_obelisk" -> Some(times (Option.defaultValue 0.0 joker.Value + 0.2))
    | "j_ramen" ->
        // the game subtracts 0.01 once per discarded card from 2, so the text value is rebuilt the same way
        let discarded = int (Math.Round((2.0 - leading "X" joker) / 0.01))
        Some(times (Seq.fold (fun factor _ -> factor - 0.01) 2.0 (seq { 1..discarded })))
    | key when net.Factors.Contains key -> Some(times (Option.defaultValue 0.0 joker.Value))
    | _ -> None

let private loyaltyActive (joker: Joker) =
    Regex.IsMatch(joker.Text, @"Active!|\b0 remaining")

// The joker whose logic runs at each slot (None for a slot that gives nothing), as resolve in Scoring.fs
let private actorsOf (jokers: Joker list) : (Joker * Joker option) list =
    let slots = Array.ofList jokers

    let rec go depth i =
        let copy target =
            if target = i || target >= slots.Length || depth + 1 > slots.Length + 1 then
                None
            else
                go (depth + 1) target

        match slots[i].Key with
        | "j_blueprint" -> copy (i + 1)
        | "j_brainstorm" -> copy 0
        | _ -> Some i

    [ for i in 0 .. slots.Length - 1 -> slots[i], go 0 i |> Option.map (fun target -> slots[target]) ]

// What one card can meet: passes, doublings and factors 1.5 per pass, and the same for a held pass
type private Reach =
    { Passes: int
      Doubles: int
      Halves: int
      Own: int
      HeldPasses: int
      HeldHalves: int
      HeldAdd: int }

// Whether the cards and jokers alone keep every value of the net inside the exact range of its arithmetic. The mult is
// bounded by the passes, doublings and factors 1.5 the five best cards can meet, all adds taken before all factors, and
// the jokers' main-phase adds and factors; Hanging Chad and Photograph act on one card, so they are counted once.
// Every count is over the jokers whose logic runs (a Blueprint or Brainstorm counts as its target)
let private withinBounds (net: Net) (situation: Situation) (hand: Card list) (levels: Map<HandType, HandLevel>) =
    let limits = net.Limits
    let slots = actorsOf situation.Jokers
    let jokers = slots |> List.choose snd
    let count key = jokers |> List.filter (fun j -> j.Key = key) |> List.length
    let chad, photograph = count "j_hanging_chad", count "j_photograph"
    let ancients = jokers |> List.choose (fun j -> if j.Key = "j_ancient" then ancientSuit j else None)
    let idols = jokers |> List.choose (fun j -> if j.Key = "j_idol" then idolTarget j else None)
    let adders = jokers |> List.filter (fun j -> net.AddsMult.Contains j.Key) |> List.length
    let chippers = jokers |> List.filter (fun j -> net.AddsChips.Contains j.Key) |> List.length
    let polychromes = situation.Jokers |> List.filter (fun j -> j.Edition = Some Polychrome) |> List.length
    let baseballs = count "j_baseball"
    // the rules read the raw keys, as JokerRegistry.rules does
    let held key = situation.Jokers |> List.exists (fun j -> j.Key = key)
    let pareidolia, smeared, midas = held "j_pareidolia", held "j_smeared", held "j_midas_mask"
    let uncommon = situation.Jokers |> List.filter (fun j -> net.Uncommon.Contains j.Key) |> List.length

    // fraction bits of a factor: 4 times it is a whole number
    let factorBits (factor: float) =
        match int (4.0 * factor) % 4 with
        | 0 -> 0
        | 2 -> 1
        | _ -> 2

    let factorWeight =
        jokers |> List.sumBy (fun j -> factorOf net j |> Option.map factorBits |> Option.defaultValue 0)

    let flag b = if b then 1 else 0
    let top5 (values: int list) = values |> List.sortDescending |> List.truncate 5 |> List.sum

    let isRed suit = suit = Hearts || suit = Diamonds

    // Passes and doublings without the two first-card jokers, which the bound adds once. A played Stone has no rank
    // or suit, unless Pareidolia makes it a face card and a Midas Mask turns it Gold; Pareidolia makes every card a face
    // card, and Smeared lets a card match the other suit of its colour
    let reach (c: Card) =
        let live = not (isStone c)
        let liveScored = live || (pareidolia && midas)
        let face = pareidolia || (live && (c.Rank = Jack || c.Rank = Queen || c.Rank = King))

        let suited suit =
            liveScored
            && (c.Enhancement = Some Enhancement.Wild
                || c.Suit = suit
                || (smeared && isRed c.Suit = isRed suit))
        let red = flag (c.Seal = Some Seal.Red)

        { Passes =
            1 + red + count "j_sock_and_buskin" * flag face
            + count "j_hack" * flag (liveScored && List.contains c.Rank [ Two; Three; Four; Five ])
            + count "j_selzer"
            + (if situation.HandsLeft = 0 then count "j_dusk" else 0)
          Doubles =
            flag (c.Enhancement = Some Enhancement.Glass)
            + count "j_triboulet" * flag (liveScored && (c.Rank = King || c.Rank = Queen))
            + (idols |> List.filter (fun (rank, suit) -> liveScored && c.Rank = rank && suited suit) |> List.length)
          Halves = flag (c.Edition = Some Polychrome) + (ancients |> List.filter suited |> List.length)
          Own = 4 * flag (c.Enhancement = Some Enhancement.Mult) + 10 * flag (c.Edition = Some Holo)
          HeldPasses = 1 + red + count "j_mime"
          HeldHalves = flag (c.Enhancement = Some Enhancement.Steel) + count "j_baron" * flag (live && c.Rank = King)
          HeldAdd = 13 * count "j_shoot_the_moon" * flag (live && c.Rank = Queen) + 22 * count "j_raised_fist" * flag live },
        flag face

    let reaches = hand |> List.map reach
    let cards = reaches |> List.map fst
    let faces = reaches |> List.map snd
    let largest (f: Reach -> int) = cards |> List.map f |> List.max

    let eachCard =
        cards
        |> List.forall (fun r ->
            r.Passes + 2 * chad <= limits.Passes
            && r.Halves <= limits.Halves
            && r.HeldHalves <= limits.HeldHalves
            && r.HeldPasses <= limits.HeldPasses)
        && List.forall2 (fun (r: Reach) face -> r.Doubles + photograph * face <= limits.Doubles) cards faces

    let doubling =
        top5 (cards |> List.map (fun r -> r.Passes * r.Doubles))
        + 2 * chad * largest (fun r -> r.Doubles)
        + photograph * largest (fun r -> r.Passes)
        + 2 * chad * photograph

    let passes = top5 (cards |> List.map (fun r -> r.Passes)) + 2 * chad
    let heldWeight = cards |> List.sumBy (fun r -> r.HeldPasses * r.HeldHalves)
    let cardWeight = top5 (cards |> List.map (fun r -> r.Passes * r.Halves)) + 2 * chad * largest (fun r -> r.Halves)
    let cardFactors = cardWeight + heldWeight
    let weight = cardFactors + polychromes + factorWeight + uncommon * baseballs
    let perPass (r: Reach) = r.Own + 8 * adders

    let adds =
        top5 (cards |> List.map (fun r -> r.Passes * perPass r))
        + 2 * chad * largest perPass
        + (cards |> List.sumBy (fun r -> r.HeldPasses * r.HeldAdd))
    // a main-phase add is at most the value plus 2 (Trousers), and a Holo adds 10 before it
    let slot = limits.Value + 12.0
    let top (value: HandLevel -> float) = levels |> Map.toSeq |> Seq.map (snd >> value) |> Seq.max

    let cardMult = (top (fun l -> l.Mult) + float adds) * 2.0 ** float doubling * 1.5 ** float cardFactors

    // a slot adds its Holo and its own add, multiplies by its own factor and by 1.5 per Baseball Card if it is Uncommon,
    // then by 1.5 for its Polychrome
    let mult =
        slots
        |> List.fold
            (fun m (slotJoker, actor) ->
                let main = actor |> Option.exists (fun a -> net.Main.Contains a.Key)

                let own =
                    match actor |> Option.bind (factorOf net) with
                    | Some factor -> factor
                    | None -> if main then 4.0 else 1.0

                let baseball = if net.Uncommon.Contains slotJoker.Key then 1.5 ** float baseballs else 1.0
                let polychrome = if slotJoker.Edition = Some Polychrome then 1.5 else 1.0
                (m + (if main then slot else 10.0)) * own * baseball * polychrome)
            cardMult

    // an own add of a counter is at most the value plus 15 (Runner)
    let chips =
        top (fun l -> l.Chips)
        + float passes * float (100 + 50 * chippers)
        + (slots
           |> List.sumBy (fun (_, actor) ->
               50.0 + (if actor |> Option.exists (fun a -> net.Main.Contains a.Key) then limits.Value + 15.0 else 0.0)))

    eachCard
    && weight <= limits.Weight
    && mult <= limits.MultCap
    && chips <= limits.ChipsCap
    && mult * chips <= limits.ProductCap

let private inScope (net: Net) (situation: Situation) (hand: Card list) (levels: Map<HandType, HandLevel>) =
    let limits = net.Limits
    let counted (n: int) (limit: int) = n >= 0 && n <= limit

    not hand.IsEmpty
    && hand.Length <= cardSlots
    && hand |> List.forall (fun c -> not c.Debuffed)
    && situation.Jokers.Length <= limits.Jokers
    && situation.Jokers
       |> List.forall (fun j ->
           let value = valueOf j

           not j.Debuffed
           && net.Keys.ContainsKey j.Key
           && (match factorOf net j with
               | Some factor -> factor <= limits.Factor && isWhole (4.0 * factor)
               | None -> isWhole value && value >= 0.0 && value <= limits.Value))
    && situation.Boss.IsNone
    && not situation.Balance
    && counted situation.HandsLeft limits.Left
    && counted situation.DiscardsLeft limits.Left
    && situation.HandPlays |> Map.forall (fun _ (run, round) -> counted run limits.Plays && counted round limits.Plays)
    && handTypes
       |> List.forall (fun (t, _) ->
           match Map.tryFind t levels with
           | Some level ->
               isWhole level.Mult
               && level.Mult >= 1.0
               && level.Mult <= limits.Mult
               && isWhole level.Chips
               && level.Chips >= 0.0
               && level.Chips <= limits.Chips
           | None -> false)
    && withinBounds net situation hand levels

// Tokens: the eight card slots (empty ones blank), 5 nulls, one per subset of the cards, the selector, each as the
// entries it sets on a token of zeros, in the order it sets them; the candidates share all but their first entries
let private encode
    (net: Net)
    (situation: Situation)
    (hand: Card list)
    (levels: Map<HandType, HandLevel>)
    : (int * float)[][] =
    let dim (name: string) = net.Dims[name]
    let one = dim "one", 1.0

    let slotTokens =
        List.init cardSlots (fun i ->
            [| one
               dim $"slot_{i}", 1.0

               match List.tryItem i hand with
               | Some card ->
                   dim "isCard", 1.0
                   dim $"rank_{Rank.id card.Rank}", 1.0
                   dim $"suit_{suitName card.Suit}", 1.0
                   dim "nominal", float (Rank.nominal card.Rank)

                   for e in Option.toList card.Enhancement do
                       dim $"enh_{e}", 1.0

                   for e in Option.toList card.Edition do
                       dim $"ed_{e}", 1.0

                   for e in Option.toList card.Seal do
                       dim $"seal_{e}", 1.0
               | None -> () |])

    let nulls =
        List.init 5 (fun k -> [| one; dim "isNull", 1.0; dim $"slot_{cardSlots + k}", 1.0 |])

    let shared =
        [| for t, name in handTypes do
               dim $"C_{name}", levels[t].Chips
               dim $"M_{name}", levels[t].Mult

               let run, round = Map.tryFind t situation.HandPlays |> Option.defaultValue (0, 0)
               dim $"run_{name}", float run
               dim $"rnd_{name}", float round

           dim "hl", float situation.HandsLeft
           dim "dl", float situation.DiscardsLeft

           for i, joker in List.indexed situation.Jokers do
               let slot = i + 1
               dim $"j{slot}_k_{net.Keys[joker.Key]}", 1.0

               for e in Option.toList joker.Edition do
                   dim $"j{slot}_e_{e}", 1.0

               // a joker that multiplies by an input factor never reads its value (Ramen's comes from its text)
               if (factorOf net joker).IsNone then
                   dim $"j{slot}_v", valueOf joker

               for factor in Option.toList (factorOf net joker) do
                   dim (if joker.Key = "j_obelisk" then $"j{slot}_fo" else $"j{slot}_fx"), factor - 1.0

               if joker.Key = "j_loyalty_card" && loyaltyActive joker then
                   dim $"j{slot}_loy", 1.0

               match joker.Key with
               | "j_ancient" ->
                   for suit in Option.toList (ancientSuit joker) do
                       dim $"j{slot}_ts_{suitName suit}", 1.0
               | "j_idol" ->
                   for rank, suit in Option.toList (idolTarget joker) do
                       dim $"j{slot}_ts_{suitName suit}", 1.0
                       dim $"j{slot}_tid", float (Rank.id rank)
               | _ -> () |]

    let isCand, idx = dim "isCand", dim "idx"
    let picks = Array2D.create positions (cardSlots + positions) -1

    let pick k slot =
        if picks[k, slot] < 0 then
            picks[k, slot] <- dim $"m{k + 1}_{slot}"

        picks[k, slot]

    let candidates =
        subsetsOf hand.Length
        |> Array.mapi (fun position subset ->
            Array.append
                [| one
                   isCand, 1.0
                   idx, float position

                   for k in 0 .. positions - 1 do
                       let slot = if k < subset.Length then subset[k] else cardSlots + k - subset.Length
                       pick k slot, 1.0 |]
                shared)
        |> List.ofArray

    slotTokens @ nulls @ candidates @ [ [| one; dim "isSel", 1.0 |] ] |> Array.ofList

// out[offset + r] = row r of m times x
let private mul (m: Sparse) (x: float[]) (out: float[]) (offset: int) =
    for r in m.Active do
        let mutable sum = 0.0

        for k in m.Start[r] .. m.Start[r + 1] - 1 do
            sum <- sum + m.Vals[k] * x[m.Index[k]]

        out[offset + r] <- sum

let private rows (m: Sparse) = m.Start.Length - 1

// A partition of the candidate tokens: candidate t is in class Cls[t], the classes numbered in the order they first
// occur, Rep[c] the first candidate in class c
[<AllowNullLiteral>]
type private Part(cls: byte[], count: int, rep: int[], id: int) =
    member _.Cls = cls
    member _.Count = count
    member _.Rep = rep
    member _.Id = id
    // a partition this one was merged from, so finer than it
    member val Finer: Part = null with get, set

// One dim on every candidate: candidate t holds Values[Part.Cls[t]], and no two classes hold the same value
type private Column = { Part: Part; Values: float[] }

let private value (c: Column) (t: int) = c.Values[int c.Part.Cls[t]]

let private same (a: Column) (b: Column) =
    obj.ReferenceEquals(a.Part, b.Part)
    && MemoryExtensions.SequenceEqual(
        MemoryMarshal.Cast<float, int64>(ReadOnlySpan<float>(a.Values)),
        MemoryMarshal.Cast<float, int64>(ReadOnlySpan<float>(b.Values))
    )

// The partitions of one call, each kept once, so that equal partitions are the same object
type private Parts(count: int, pairs: int[], buffers: float[][]) =
    let known = Dictionary<int, ResizeArray<Part>>()
    let meets = Dictionary<struct (int * int), Part>()
    let merges = Dictionary<struct (int * int64), Part>()
    let classOf = Dictionary<int64, int>()
    let distinct = ResizeArray<Part>()
    let keys = Array.zeroCreate count
    let firsts = Array.zeroCreate count
    let mutable parts = 0

    // the partition with these classes, firsts[c] the first candidate of class c
    let intern (cls: byte[]) (classes: int) =
        let mutable hash = classes

        for c in cls do
            hash <- hash * 31 + int c

        let bucket =
            match known.TryGetValue hash with
            | true, bucket -> bucket
            | _ ->
                let bucket = ResizeArray()
                known[hash] <- bucket
                bucket

        match bucket.Find(fun p -> MemoryExtensions.SequenceEqual(ReadOnlySpan<byte>(p.Cls), ReadOnlySpan<byte>(cls))) with
        | null ->
            let p = Part(cls, classes, Array.sub firsts 0 classes, parts)
            parts <- parts + 1
            bucket.Add p
            p
        | p -> p

    let one = intern (Array.zeroCreate count) 1

    let each =
        for t in 0 .. count - 1 do
            firsts[t] <- t

        intern (Array.init count byte) count

    member _.One = one

    member _.Each = each

    // A reused array of at least size elements, one per slot
    member _.Buffer(slot: int, size: int) =
        if buffers[slot].Length < size then
            buffers[slot] <- Array.zeroCreate (max size (2 * buffers[slot].Length))

        buffers[slot]

    // The coarsest partition finer than both
    member _.Meet(p: Part, q: Part) =
        if p.Count = 1 || obj.ReferenceEquals(q, p.Finer) then
            q
        elif q.Count = 1 || obj.ReferenceEquals(p, q) || obj.ReferenceEquals(p, q.Finer) then
            p
        else
            let key = if p.Id < q.Id then struct (p.Id, q.Id) else struct (q.Id, p.Id)

            match meets.TryGetValue key with
            | true, m -> m
            | _ ->
                let cls = Array.zeroCreate count
                let mutable classes = 0

                for t in 0 .. count - 1 do
                    let k = int p.Cls[t] * q.Count + int q.Cls[t]
                    keys[t] <- k

                    if pairs[k] < 0 then
                        pairs[k] <- classes
                        firsts[classes] <- t
                        classes <- classes + 1

                    cls[t] <- byte pairs[k]

                for t in 0 .. count - 1 do
                    pairs[keys[t]] <- -1

                let m = intern cls classes
                meets[key] <- m
                m

    // The meet of the partitions of the given columns, taken in the order of their ids so that the same set of
    // partitions meets through the same steps
    member this.MeetAll(dims: int[], cols: Column[]) =
        distinct.Clear()

        for i in dims do
            let q = cols[i].Part

            if q.Count > 1 && not (distinct.Contains q) then
                distinct.Add q

        if distinct.Count = 0 then
            one
        else
            distinct.Sort(fun a b -> compare a.Id b.Id)
            let mutable m = distinct[0]

            for a in 1 .. distinct.Count - 1 do
                m <- this.Meet(m, distinct[a])

            m

    // The column holding values[c] on class c of part, with the classes of equal value merged
    member _.Column(part: Part, values: float[]) =
        if part.Count = 1 then
            { Part = one; Values = values }
        else
            let classes = part.Count
            let merged = Array.zeroCreate classes
            let map = Array.zeroCreate classes
            let mutable m = 0
            let mutable pattern = 0L
            classOf.Clear()

            for c in 0 .. classes - 1 do
                let bits = BitConverter.DoubleToInt64Bits values[c]
                let mutable j = 0

                if classes <= 16 then
                    while j < m && BitConverter.DoubleToInt64Bits merged[j] <> bits do
                        j <- j + 1
                else
                    match classOf.TryGetValue bits with
                    | true, found -> j <- found
                    | _ ->
                        j <- m
                        classOf[bits] <- m

                if j = m then
                    merged[m] <- values[c]
                    firsts[m] <- part.Rep[c]
                    m <- m + 1

                map[c] <- j
                pattern <- pattern * 1000003L + int64 j

            if m = classes then
                { Part = part; Values = values }
            elif m = 1 then
                { Part = one; Values = [| values[0] |] }
            else
                let key = struct (part.Id, pattern)

                let merge =
                    let mutable found = null

                    match merges.TryGetValue key with
                    | true, p ->
                        let mutable c = 0

                        while c < classes && map[c] = int p.Cls[part.Rep[c]] do
                            c <- c + 1

                        if c = classes then
                            found <- p
                    | _ -> ()

                    if isNull found then
                        found <- intern (Array.init count (fun t -> byte map[int part.Cls[t]])) m

                        if isNull found.Finer then
                            found.Finer <- part

                        merges[key] <- found

                    found

                { Part = merge; Values = Array.sub merged 0 m }

// x + a on every candidate where a is not 0, as a token adds each change
let private plus (parts: Parts) (x: Column) (a: Column) =
    if a.Part.Count = 1 && a.Values[0] = 0.0 then
        x
    else
        let p = parts.Meet(x.Part, a.Part)

        parts.Column(
            p,
            Array.init p.Count (fun c ->
                let t = p.Rep[c]
                let change = value a t
                if change <> 0.0 then value x t + change else value x t)
        )

// out[offset + r] = row r of m times the x of candidate t
let private mulColumns (m: Sparse) (cols: Column[]) (t: int) (out: float[]) (offset: int) =
    for r in m.Active do
        let mutable sum = 0.0

        for k in m.Start[r] .. m.Start[r + 1] - 1 do
            sum <- sum + m.Vals[k] * value cols[m.Index[k]] t

        out[offset + r] <- sum

// What the MLP adds to dim r, W2 ReLU(W1 z + b1), computed once per class of candidates that agree on every input of
// the units that write r. Every value on a candidate is exact (see the precision bounds in nn/README.md), so this is
// the sum the token passes reach change by change
let private mlp (net: Net) (parts: Parts) (z: Column[]) (r: int) =
    let w1, w2 = net.W1Rows, net.W2Rows
    let inputs = net.Inputs[r]
    let part = parts.MeetAll(inputs, z)
    let classes = part.Count

    if classes = 1 then
        let mutable out = 0.0

        for k in w2.Start[r] .. w2.Start[r + 1] - 1 do
            let h = w2.Index[k]
            let mutable pre = net.B1[h]

            for k1 in w1.Start[h] .. w1.Start[h + 1] - 1 do
                pre <- pre + w1.Vals[k1] * z[w1.Index[k1]].Values[0]

            if pre > 0.0 then
                out <- out + w2.Vals[k] * pre

        { Part = part; Values = [| out |] }
    else
        let fetched = parts.Buffer(0, inputs.Length * classes)
        let pre = parts.Buffer(1, classes)
        let out = Array.zeroCreate classes
        let rep = part.Rep

        for a in 0 .. inputs.Length - 1 do
            let col = z[inputs[a]]
            let offset = a * classes

            if col.Part.Count = 1 then
                Array.fill fetched offset classes col.Values[0]
            else
                let cls, values = col.Part.Cls, col.Values

                for c in 0 .. classes - 1 do
                    fetched[offset + c] <- values[int cls[rep[c]]]

        let at = net.At[r]
        let mutable p = 0

        for k in w2.Start[r] .. w2.Start[r + 1] - 1 do
            let h = w2.Index[k]
            Array.fill pre 0 classes net.B1[h]

            for k1 in w1.Start[h] .. w1.Start[h + 1] - 1 do
                let w = w1.Vals[k1]
                let offset = at[p] * classes
                p <- p + 1

                for c in 0 .. classes - 1 do
                    pre[c] <- pre[c] + w * fetched[offset + c]

            let w = w2.Vals[k]

            for c in 0 .. classes - 1 do
                if pre[c] > 0.0 then
                    out[c] <- out[c] + w * pre[c]

        parts.Column(part, out)

// The candidates: X after the last loop, Z = X + attention (what the MLP reads), M what the MLP adds, A the attention
// output, and the dims each of them changed in
type private Candidates =
    { X: Column[]
      Z: Column[]
      M: Column[]
      A: Column[]
      Moved: ResizeArray<int>
      Attended: ResizeArray<int>
      Read: ResizeArray<int>
      Added: ResizeArray<int>
      mutable Seed: int[]
      Seen: int[]
      mutable Id: int }

// Softmax attention over all tokens. A token whose key is all zero scores 0 with every query, so those tokens share
// one weight and their values enter through one sum. The card slots, nulls and selector read their x and get their
// output in delta; the candidates read their columns and get theirs once per class of equal query, in A or, for the
// sink, in sinkDelta
let private attend
    (net: Net)
    (parts: Parts)
    (head: Head)
    (xs: float[][])
    (first: int)
    (cands: Candidates)
    (delta: float[][])
    (sinkDelta: float[])
    =
    let n = xs.Length
    let isRow i = i < first || i = n - 1
    let dq = rows head.Wq
    let dv = rows head.Wv
    let q = parts.Buffer(2, n * dq)
    let k = parts.Buffer(3, n * dq)
    let v = parts.Buffer(4, n * dv)
    Array.Clear(q, 0, n * dq)
    Array.Clear(k, 0, n * dq)
    Array.Clear(v, 0, n * dv)

    for i in 0 .. n - 1 do
        if isRow i then
            mul head.Wq xs[i] q (i * dq)
            mul head.Wk xs[i] k (i * dq)
            mul head.Wv xs[i] v (i * dv)
        else
            mulColumns head.Wk cands.X (i - first) k (i * dq)
            mulColumns head.Wv cands.X (i - first) v (i * dv)

    let isKeyed = Array.zeroCreate n

    for j in 0 .. n - 1 do
        let mutable c = 0

        while c < dq && k[j * dq + c] = 0.0 do
            c <- c + 1

        isKeyed[j] <- c < dq

    let keyed = [| for j in 0 .. n - 1 do if isKeyed[j] then j |]

    let unkeyedSum = Array.zeroCreate dv

    for j in 0 .. n - 1 do
        if not isKeyed[j] then
            for c in 0 .. dv - 1 do
                unkeyedSum[c] <- unkeyedSum[c] + v[j * dv + c]

    let unkeyed = float (n - keyed.Length)
    let weights = Array.zeroCreate keyed.Length
    let mixed = Array.zeroCreate dv
    let projected = parts.Buffer(5, net.D)

    // a query of zero scores every token 0, so every token weighs 1 / n
    let meanV = Array.zeroCreate dv

    for j in 0 .. n - 1 do
        for c in 0 .. dv - 1 do
            meanV[c] <- meanV[c] + v[j * dv + c] / float n

    let mix (qs: float[]) (offset: int) =
        let mutable zeroQuery = true

        for c in 0 .. dq - 1 do
            if qs[offset + c] <> 0.0 then
                zeroQuery <- false

        if zeroQuery then
            Array.blit meanV 0 mixed 0 dv
        else
            let mutable top = 0.0

            for a in 0 .. keyed.Length - 1 do
                let mutable s = 0.0

                for c in 0 .. dq - 1 do
                    s <- s + qs[offset + c] * k[keyed[a] * dq + c]

                weights[a] <- s
                top <- Math.Max(top, s)

            let unkeyedWeight = Math.Exp(-top)
            let mutable total = unkeyed * unkeyedWeight

            for a in 0 .. keyed.Length - 1 do
                weights[a] <- Math.Exp(weights[a] - top)
                total <- total + weights[a]

            for c in 0 .. dv - 1 do
                mixed[c] <- unkeyedWeight / total * unkeyedSum[c]

            for a in 0 .. keyed.Length - 1 do
                let w = weights[a] / total

                for c in 0 .. dv - 1 do
                    mixed[c] <- mixed[c] + w * v[keyed[a] * dv + c]

        mul head.Wo mixed projected 0

    for i in 0 .. n - 1 do
        if isRow i then
            mix q (i * dq)

            for r in head.Wo.Active do
                delta[i][r] <- projected[r]

    let part = parts.MeetAll(head.Wq.Index, cands.X)
    let outs = head.Wo.Active |> Array.map (fun _ -> Array.zeroCreate part.Count)
    let query = Array.zeroCreate dq

    for c in 0 .. part.Count - 1 do
        mulColumns head.Wq cands.X part.Rep[c] query 0
        mix query 0

        for a in 0 .. outs.Length - 1 do
            outs[a][c] <- projected[head.Wo.Active[a]]

    for a in 0 .. outs.Length - 1 do
        let r = head.Wo.Active[a]

        if net.Sink[r] then
            sinkDelta[r] <- outs[a][0]
        else
            let col = parts.Column(part, outs[a])

            if not (same col cands.A[r]) then
                cands.A[r] <- col
                cands.Attended.Add r

// One token and what is known of its MLP: Pre = W1 x + b1 for the x last folded in, Out = W2 ReLU(Pre) (what the MLP
// adds to x every loop), and Pend, the change of x not yet folded into Pre. Only changes are propagated, so a dim that
// has settled costs nothing. The selector, whose attention averages with weights 1/n until the key settles, can keep
// rounding residue in its dims (below 1e-8 observed) that the readout rounds away
type private Token =
    { X: float[]
      Pre: float[]
      Out: float[]
      OutDims: int[]
      OutSeen: bool[]
      mutable OutCount: int
      Pend: float[]
      PendDims: int[]
      PendSeen: bool[]
      mutable PendCount: int }

type private Scratch =
    { Dirty: bool[]
      Old: float[]
      Stamp: int[]
      Touched: int[]
      mutable Id: int }

let private noteOut (t: Token) (r: int) =
    if not (t.OutSeen[r]) then
        t.OutSeen[r] <- true
        t.OutDims[t.OutCount] <- r
        t.OutCount <- t.OutCount + 1

let private push (t: Token) (r: int) (change: float) =
    if not (t.PendSeen[r]) then
        t.PendSeen[r] <- true
        t.PendDims[t.PendCount] <- r
        t.PendCount <- t.PendCount + 1

    t.Pend[r] <- t.Pend[r] + change

// Fold the pending changes of x into Pre, and the changes of ReLU(Pre) into Out
let private propagate (net: Net) (w: Scratch) (t: Token) =
    w.Id <- w.Id + 1
    let id = w.Id
    let mutable count = 0
    let pre, pend, pendSeen, pendDims = t.Pre, t.Pend, t.PendSeen, t.PendDims
    let start1, index1, vals1 = net.W1.Start, net.W1.Index, net.W1.Vals
    let stamp, old, touched = w.Stamp, w.Old, w.Touched

    for a in 0 .. t.PendCount - 1 do
        let r = pendDims[a]
        let change = pend[r]
        pend[r] <- 0.0
        pendSeen[r] <- false

        if change <> 0.0 then
            for k in start1[r] .. start1[r + 1] - 1 do
                let h = index1[k]

                if stamp[h] <> id then
                    stamp[h] <- id
                    old[h] <- (if pre[h] > 0.0 then pre[h] else 0.0)
                    touched[count] <- h
                    count <- count + 1

                pre[h] <- pre[h] + vals1[k] * change

    t.PendCount <- 0
    let start2, index2, vals2 = net.W2.Start, net.W2.Index, net.W2.Vals
    let out = t.Out

    for a in 0 .. count - 1 do
        let h = touched[a]
        let change = (if pre[h] > 0.0 then pre[h] else 0.0) - old[h]

        if change <> 0.0 then
            for k in start2[h] .. start2[h + 1] - 1 do
                let r = index2[k]
                out[r] <- out[r] + vals2[k] * change
                noteOut t r

// One loop on one token: x += attention; x += W2 ReLU(W1 x + b1)
let private advance (net: Net) (w: Scratch) (t: Token) (delta: float[]) =
    for r in net.Fresh do
        let change = delta[r]

        if change <> 0.0 then
            t.X[r] <- t.X[r] + change
            push t r change

            for h in net.Readers[r] do
                w.Dirty[h] <- true

    propagate net w t
    let mutable kept = 0

    for a in 0 .. t.OutCount - 1 do
        let r = t.OutDims[a]
        let out = t.Out[r]

        if out <> 0.0 then
            t.X[r] <- t.X[r] + out
            push t r out

            for h in net.Readers[r] do
                w.Dirty[h] <- true

            t.OutDims[kept] <- r
            kept <- kept + 1
        else
            t.OutSeen[r] <- false

    t.OutCount <- kept

// One loop on the candidates, as advance does it on each: z = x + attention, m = W2 ReLU(W1 z + b1), x = z + m, each
// recomputed only where an operand changed
let private step (net: Net) (parts: Parts) (c: Candidates) (dirty: bool[]) =
    let first (r: int) =
        if c.Seen[r] = c.Id then
            false
        else
            c.Seen[r] <- c.Id
            true

    let read (r: int) =
        if first r then
            let z = if net.IsFresh[r] then plus parts c.X[r] c.A[r] else c.X[r]

            if not (same z c.Z[r]) then
                c.Z[r] <- z
                c.Read.Add r

    let add (r: int) =
        if first r then
            let m = mlp net parts c.Z r

            if not (same m c.M[r]) then
                c.M[r] <- m
                c.Added.Add r

    let move (r: int) =
        if first r then
            let x = plus parts c.Z[r] c.M[r]

            if not (same x c.X[r]) then
                c.X[r] <- x
                c.Moved.Add r

                for h in net.Readers[r] do
                    dirty[h] <- true

    c.Id <- c.Id + 1
    c.Read.Clear()
    c.Moved.ForEach read
    c.Attended.ForEach read
    c.Attended.Clear()
    c.Id <- c.Id + 1
    c.Added.Clear()
    Array.iter add c.Seed
    c.Seed <- [||]

    for z in c.Read do
        Array.iter add net.Affects[z]

    c.Id <- c.Id + 1
    c.Moved.Clear()
    c.Read.ForEach move
    c.Added.ForEach move


// What one thread reuses from call to call, sized by the net alone: the state of the card slot, null and selector
// tokens and of the sink token, their attention outputs, and the candidates' dims
type private Workspace =
    { Net: Net
      Tokens: Token[]
      Delta: float[][]
      Blank: float[]
      Scratch: Scratch
      Cands: Candidates
      Pairs: int[]
      Buffers: float[][] }

let private workspaces = new ThreadLocal<Workspace>()

let private workspace (net: Net) =
    let w = workspaces.Value

    if not (isNull (box w)) && obj.ReferenceEquals(w.Net, net) then
        w
    else
        let hidden = net.B1.Length
        let rowTokens = cardSlots + positions + 1
        let candidates = (subsetsOf cardSlots).Length

        let blank () =
            { X = Array.zeroCreate net.D
              Pre = Array.zeroCreate hidden
              Out = Array.zeroCreate net.D
              OutDims = Array.zeroCreate net.D
              OutSeen = Array.zeroCreate net.D
              OutCount = 0
              Pend = Array.zeroCreate net.D
              PendDims = Array.zeroCreate net.D
              PendSeen = Array.zeroCreate net.D
              PendCount = 0 }

        let w =
            { Net = net
              Tokens = Array.init (rowTokens + 1) (fun _ -> blank ())
              Delta = Array.init (rowTokens + 1) (fun _ -> Array.zeroCreate net.D)
              Blank = Array.zeroCreate net.D
              Scratch =
                { Dirty = Array.zeroCreate net.Heads.Length
                  Old = Array.zeroCreate hidden
                  Stamp = Array.zeroCreate hidden
                  Touched = Array.zeroCreate hidden
                  Id = 0 }
              Cands =
                { X = Array.zeroCreate net.D
                  Z = Array.zeroCreate net.D
                  M = Array.zeroCreate net.D
                  A = Array.zeroCreate net.D
                  Moved = ResizeArray()
                  Attended = ResizeArray()
                  Read = ResizeArray()
                  Added = ResizeArray()
                  Seed = [||]
                  Seen = Array.zeroCreate net.D
                  Id = 0 }
              Pairs = Array.create (candidates * candidates) -1
              Buffers = Array.init 6 (fun _ -> Array.zeroCreate 64) }

        workspaces.Value <- w
        w

// Start a token from its entries, x = 0 elsewhere, Pre = b1 and Out = start
let private reset (net: Net) (t: Token) (entries: (int * float)[]) (start: float[]) =
    Array.Clear t.X

    for r, v in entries do
        t.X[r] <- v

    Array.blit net.B1 0 t.Pre 0 net.B1.Length
    Array.blit start 0 t.Out 0 net.D

    for a in 0 .. t.OutCount - 1 do
        t.OutSeen[t.OutDims[a]] <- false

    t.OutCount <- 0

    for a in 0 .. t.PendCount - 1 do
        t.Pend[t.PendDims[a]] <- 0.0
        t.PendSeen[t.PendDims[a]] <- false

    t.PendCount <- 0

    for r in 0 .. net.D - 1 do
        if start[r] <> 0.0 then
            noteOut t r

        if t.X[r] <> 0.0 then
            push t r t.X[r]

let choose
    (net: Net)
    (situation: Situation)
    (hand: Card list)
    (levels: Map<HandType, HandLevel>)
    : (int list * float * HandType) option =
    if not (inScope net situation hand levels) then
        None
    else
        let entries = encode net situation hand levels
        let w = workspace net
        let n = entries.Length
        let first = cardSlots + positions
        let count = n - 1 - first
        let isRow i = i < first || i = n - 1
        // the workspace token of a row: the card slots and nulls in order, then the selector, then the sink
        let rowOf i = if i < first then i else first
        let sink = w.Tokens[first + 1]
        let sinkDelta = w.Delta[first + 1]

        for i in 0 .. n - 1 do
            if isRow i then
                reset net w.Tokens[rowOf i] entries[i] net.StartOut

        // the sink is computed on one token for all candidates: no other dim reads it, it reads nothing else, and its
        // input, the attention of a zero query, is the same on every candidate
        reset net sink [||] w.Blank

        let xs = Array.init n (fun i -> if isRow i then w.Tokens[rowOf i].X else null)
        let delta = Array.init n (fun i -> if isRow i then w.Delta[rowOf i] else null)

        let tokens =
            [| for i in 0 .. n - 1 do
                   if isRow i then
                       w.Tokens[rowOf i], delta[i]
               sink, sinkDelta |]

        let scratch = w.Scratch
        Array.fill scratch.Dirty 0 scratch.Dirty.Length true
        Array.Clear scratch.Stamp
        scratch.Id <- 0

        let parts = Parts(count, w.Pairs, w.Buffers)
        let zero = { Part = parts.One; Values = [| 0.0 |] }
        let cands = w.Cands

        for cols in [ cands.X; cands.Z; cands.M; cands.A ] do
            Array.fill cols 0 net.D zero

        cands.Moved.Clear()
        cands.Attended.Clear()
        cands.Seed <- [| for r in 0 .. net.D - 1 do if net.StartOut[r] <> 0.0 && not net.Sink[r] then r |]
        Array.Clear cands.Seen
        cands.Id <- 0
        let inputs = Dictionary<int, float[]>()

        for t in 0 .. count - 1 do
            for r, v in entries[first + t] do
                match inputs.TryGetValue r with
                | true, values -> values[t] <- v
                | _ ->
                    let values = Array.zeroCreate count
                    values[t] <- v
                    inputs[r] <- values

        for KeyValue(r, values) in inputs do
            let x =
                if Array.forall (fun v -> BitConverter.DoubleToInt64Bits v = BitConverter.DoubleToInt64Bits values[0]) values then
                    { Part = parts.One; Values = [| values[0] |] }
                else
                    parts.Column(parts.Each, values)

            if not (same x zero) then
                cands.X[r] <- x
                cands.Moved.Add r

        for _ in 1 .. net.Loops do
            for h in 0 .. net.Heads.Length - 1 do
                if scratch.Dirty[h] then
                    scratch.Dirty[h] <- false
                    attend net parts net.Heads[h] xs first cands delta sinkDelta

            for t, d in tokens do
                advance net scratch t d

            step net parts cands scratch.Dirty

        let selector = xs[n - 1]
        let dim (name: string) = selector[net.Dims[name]]

        let played = [ for i in 0 .. cardSlots - 1 do if dim $"sel_slot_{i}" > 0.5 then i ]
        let handType = handTypes |> List.find (fun (_, name) -> dim $"sel_t_{name}" > 0.5) |> fst
        Some(played, Math.Round(dim "sel_total"), handType)
