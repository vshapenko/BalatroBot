module Balatro.Bot.PlayNet

open System
open System.IO
open System.Text.Json
open System.Text.RegularExpressions
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
          AddsMult: Set<string>
          AddsChips: Set<string>
          Dims: Map<string, int>
          Heads: Head[]
          W1: Sparse
          B1: float[]
          Lit: int[]
          Fresh: int[]
          Readers: int[][]
          W2: Sparse }

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
          Value = limits.GetProperty("value").GetDouble()
          Left = limits.GetProperty("left").GetInt32()
          Plays = limits.GetProperty("plays").GetInt32() }
      Keys = [ for p in root.GetProperty("keys").EnumerateObject() -> p.Name, p.Value.GetString() ] |> Map.ofList
      Main = [ for k in root.GetProperty("main").EnumerateArray() -> k.GetString() ] |> Set.ofList
      AddsMult = [ for k in root.GetProperty("addsMult").EnumerateArray() -> k.GetString() ] |> Set.ofList
      AddsChips = [ for k in root.GetProperty("addsChips").EnumerateArray() -> k.GetString() ] |> Set.ofList
      Dims = [ for p in root.GetProperty("dims").EnumerateObject() -> p.Name, p.Value.GetInt32() ] |> Map.ofList
      Heads = heads
      W1 = sparse d true (root.GetProperty "W1")
      B1 = b1
      Lit = [| for h in 0 .. hidden - 1 do if b1[h] > 0.0 then h |]
      Fresh = heads |> Array.collect (fun h -> h.Wo.Active) |> Array.distinct
      Readers =
        Array.init d (fun r ->
            [| for i in 0 .. heads.Length - 1 do
                   let h = heads[i]
                   if Array.contains r h.Wq.Index || Array.contains r h.Wk.Index || Array.contains r h.Wv.Index then
                       i |])
      W2 = sparse hidden true (root.GetProperty "W2") }

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
// the jokers' main-phase adds and factors; Hanging Chad and Photograph act on one card, so they are counted once
let private withinBounds (net: Net) (situation: Situation) (hand: Card list) (levels: Map<HandType, HandLevel>) =
    let limits = net.Limits
    let jokers = situation.Jokers
    let count key = jokers |> List.filter (fun j -> j.Key = key) |> List.length
    let chad, photograph = count "j_hanging_chad", count "j_photograph"
    let ancients = jokers |> List.choose (fun j -> if j.Key = "j_ancient" then ancientSuit j else None)
    let idols = jokers |> List.choose (fun j -> if j.Key = "j_idol" then idolTarget j else None)
    let adders = jokers |> List.filter (fun j -> net.AddsMult.Contains j.Key) |> List.length
    let chippers = jokers |> List.filter (fun j -> net.AddsChips.Contains j.Key) |> List.length
    let polychromes = jokers |> List.filter (fun j -> j.Edition = Some Polychrome) |> List.length
    let flag b = if b then 1 else 0
    let top5 (values: int list) = values |> List.sortDescending |> List.truncate 5 |> List.sum

    // Passes and doublings without the two first-card jokers, which the bound adds once
    let reach (c: Card) =
        let live = not (isStone c)
        let face = live && (c.Rank = Jack || c.Rank = Queen || c.Rank = King)
        let suited suit = live && (c.Enhancement = Some Enhancement.Wild || c.Suit = suit)
        let red = flag (c.Seal = Some Seal.Red)

        { Passes =
            1 + red + count "j_sock_and_buskin" * flag face
            + count "j_hack" * flag (live && List.contains c.Rank [ Two; Three; Four; Five ])
            + count "j_selzer"
            + (if situation.HandsLeft = 0 then count "j_dusk" else 0)
          Doubles =
            flag (c.Enhancement = Some Enhancement.Glass)
            + count "j_triboulet" * flag (live && (c.Rank = King || c.Rank = Queen))
            + (idols |> List.filter (fun (rank, suit) -> live && c.Rank = rank && suited suit) |> List.length)
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
    let weight = cardWeight + heldWeight + polychromes
    let perPass (r: Reach) = r.Own + 8 * adders

    let adds =
        top5 (cards |> List.map (fun r -> r.Passes * perPass r))
        + 2 * chad * largest perPass
        + (cards |> List.sumBy (fun r -> r.HeldPasses * r.HeldAdd))
    let slot = limits.Value + 10.0
    let top (value: HandLevel -> float) = levels |> Map.toSeq |> Seq.map (snd >> value) |> Seq.max

    let cardMult = (top (fun l -> l.Mult) + float adds) * 2.0 ** float doubling * 1.5 ** float (weight - polychromes)

    let mult =
        jokers
        |> List.fold
            (fun m j ->
                let main = net.Main.Contains j.Key
                let factor = if j.Edition = Some Polychrome then 1.5 else 1.0
                (m + (if main then slot else 10.0)) * (if main then 4.0 else 1.0) * factor)
            cardMult

    let chips =
        top (fun l -> l.Chips)
        + float passes * float (100 + 50 * chippers)
        + (jokers |> List.sumBy (fun j -> 50.0 + (if net.Main.Contains j.Key then limits.Value else 0.0)))

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
           let value = Option.defaultValue 0.0 j.Value
           not j.Debuffed && net.Keys.ContainsKey j.Key && isWhole value && value >= 0.0 && value <= limits.Value)
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

// Tokens: the eight card slots (empty ones blank), 5 nulls, one per subset of the cards, the selector
let private encode (net: Net) (situation: Situation) (hand: Card list) (levels: Map<HandType, HandLevel>) : float[][] =
    let dim (name: string) = net.Dims[name]

    let token () =
        let x = Array.zeroCreate net.D
        x[dim "one"] <- 1.0
        x

    let slotTokens =
        List.init cardSlots (fun i ->
            let x = token ()
            x[dim $"slot_{i}"] <- 1.0

            match List.tryItem i hand with
            | Some card ->
                x[dim "isCard"] <- 1.0
                x[dim $"rank_{Rank.id card.Rank}"] <- 1.0
                x[dim $"suit_{suitName card.Suit}"] <- 1.0
                x[dim "nominal"] <- float (Rank.nominal card.Rank)
                card.Enhancement |> Option.iter (fun e -> x[dim $"enh_{e}"] <- 1.0)
                card.Edition |> Option.iter (fun e -> x[dim $"ed_{e}"] <- 1.0)
                card.Seal |> Option.iter (fun e -> x[dim $"seal_{e}"] <- 1.0)
            | None -> ()

            x)

    let nulls =
        List.init 5 (fun k ->
            let x = token ()
            x[dim "isNull"] <- 1.0
            x[dim $"slot_{cardSlots + k}"] <- 1.0
            x)

    let candidates =
        subsetsOf hand.Length
        |> Array.mapi (fun position subset ->
            let x = token ()
            x[dim "isCand"] <- 1.0
            x[dim "idx"] <- float position

            for k in 0 .. positions - 1 do
                let slot = if k < subset.Length then subset[k] else cardSlots + k - subset.Length
                x[dim $"m{k + 1}_{slot}"] <- 1.0

            for t, name in handTypes do
                x[dim $"C_{name}"] <- levels[t].Chips
                x[dim $"M_{name}"] <- levels[t].Mult

                let run, round = Map.tryFind t situation.HandPlays |> Option.defaultValue (0, 0)
                x[dim $"run_{name}"] <- float run
                x[dim $"rnd_{name}"] <- float round

            x[dim "hl"] <- float situation.HandsLeft
            x[dim "dl"] <- float situation.DiscardsLeft

            situation.Jokers
            |> List.iteri (fun i joker ->
                let slot = i + 1
                x[dim $"j{slot}_k_{net.Keys[joker.Key]}"] <- 1.0
                joker.Edition |> Option.iter (fun e -> x[dim $"j{slot}_e_{e}"] <- 1.0)
                x[dim $"j{slot}_v"] <- Option.defaultValue 0.0 joker.Value

                match joker.Key with
                | "j_ancient" ->
                    ancientSuit joker |> Option.iter (fun suit -> x[dim $"j{slot}_ts_{suitName suit}"] <- 1.0)
                | "j_idol" ->
                    idolTarget joker
                    |> Option.iter (fun (rank, suit) ->
                        x[dim $"j{slot}_ts_{suitName suit}"] <- 1.0
                        x[dim $"j{slot}_tid"] <- float (Rank.id rank))
                | _ -> ())

            x)
        |> List.ofArray

    let selector =
        let x = token ()
        x[dim "isSel"] <- 1.0
        x

    slotTokens @ nulls @ candidates @ [ selector ] |> Array.ofList

// out[offset + r] = row r of m times x
let private mul (m: Sparse) (x: float[]) (out: float[]) (offset: int) =
    for r in m.Active do
        let mutable sum = 0.0

        for k in m.Start[r] .. m.Start[r + 1] - 1 do
            sum <- sum + m.Vals[k] * x[m.Index[k]]

        out[offset + r] <- sum

let private rows (m: Sparse) = m.Start.Length - 1

// Softmax attention over all tokens, added to delta. A token whose key is all zero scores 0 with every
// query, so those tokens share one weight and their values enter through one sum
let private attend (head: Head) (xs: float[][]) (delta: float[][]) =
    let n = xs.Length
    let dq = rows head.Wq
    let dv = rows head.Wv
    let q = Array.zeroCreate (n * dq)
    let k = Array.zeroCreate (n * dq)
    let v = Array.zeroCreate (n * dv)

    for i in 0 .. n - 1 do
        mul head.Wq xs[i] q (i * dq)
        mul head.Wk xs[i] k (i * dq)
        mul head.Wv xs[i] v (i * dv)

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
    let projected = Array.zeroCreate delta[0].Length

    // a query of zero scores every token 0, so every token weighs 1 / n
    let meanV = Array.zeroCreate dv

    for j in 0 .. n - 1 do
        for c in 0 .. dv - 1 do
            meanV[c] <- meanV[c] + v[j * dv + c] / float n

    for i in 0 .. n - 1 do
        let mutable zeroQuery = true

        for c in 0 .. dq - 1 do
            if q[i * dq + c] <> 0.0 then
                zeroQuery <- false

        if zeroQuery then
            Array.blit meanV 0 mixed 0 dv
        else
            let mutable top = 0.0

            for a in 0 .. keyed.Length - 1 do
                let mutable s = 0.0

                for c in 0 .. dq - 1 do
                    s <- s + q[i * dq + c] * k[keyed[a] * dq + c]

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

        for r in head.Wo.Active do
            delta[i][r] <- projected[r]

// One token and what is known of its MLP: Pre = W1 x + b1 for the x last folded in, Out = W2 ReLU(Pre) (what the MLP
// adds to x every loop), and Pend, the change of x not yet folded into Pre. Only changes are propagated, so a dim that
// has settled costs nothing. On the candidate tokens the result is bit-identical to recomputing W2 ReLU(W1 x + b1) from
// scratch every loop; on the selector, whose attention averages with weights 1/n until the key settles, the dims can keep
// rounding residue (below 1e-8 observed) that the readout rounds away
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

// x += attention; x += W2 ReLU(W1 x + b1)
let private block (net: Net) (xs: float[][]) (tokens: Token[]) (delta: float[][]) (w: Scratch) =
    // a head's output changes only when something it reads has changed; each head owns its fresh dims in delta
    for h in 0 .. net.Heads.Length - 1 do
        if w.Dirty[h] then
            w.Dirty[h] <- false
            attend net.Heads[h] xs delta

    for i in 0 .. tokens.Length - 1 do
        let t = tokens[i]

        for r in net.Fresh do
            let change = delta[i][r]

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

let choose
    (net: Net)
    (situation: Situation)
    (hand: Card list)
    (levels: Map<HandType, HandLevel>)
    : (int list * float * HandType) option =
    if not (inScope net situation hand levels) then
        None
    else
        let xs = encode net situation hand levels
        let delta = Array.init xs.Length (fun _ -> Array.zeroCreate net.D)
        let hidden = net.B1.Length

        let scratch =
            { Dirty = Array.create net.Heads.Length true
              Old = Array.zeroCreate hidden
              Stamp = Array.zeroCreate hidden
              Touched = Array.zeroCreate hidden
              Id = 0 }

        // with x = 0, only the units with a positive bias are active
        let startOut = Array.zeroCreate net.D

        for h in net.Lit do
            for k in net.W2.Start[h] .. net.W2.Start[h + 1] - 1 do
                startOut[net.W2.Index[k]] <- startOut[net.W2.Index[k]] + net.W2.Vals[k] * net.B1[h]

        let tokens =
            xs
            |> Array.map (fun x ->
                let t =
                    { X = x
                      Pre = Array.copy net.B1
                      Out = Array.copy startOut
                      OutDims = Array.zeroCreate net.D
                      OutSeen = Array.zeroCreate net.D
                      OutCount = 0
                      Pend = Array.zeroCreate net.D
                      PendDims = Array.zeroCreate net.D
                      PendSeen = Array.zeroCreate net.D
                      PendCount = 0 }

                for r in 0 .. net.D - 1 do
                    if startOut[r] <> 0.0 then
                        noteOut t r

                    if x[r] <> 0.0 then
                        push t r x[r]

                t)

        for _ in 1 .. net.Loops do
            block net xs tokens delta scratch

        let selector = xs[xs.Length - 1]
        let dim (name: string) = selector[net.Dims[name]]

        let played = [ for i in 0 .. cardSlots - 1 do if dim $"sel_slot_{i}" > 0.5 then i ]
        let handType = handTypes |> List.find (fun (_, name) -> dim $"sel_t_{name}" > 0.5) |> fst
        Some(played, Math.Round(dim "sel_total"), handType)
