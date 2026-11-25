module Balatro.Bot.PlayNet

open System
open System.IO
open System.Text.Json
open Balatro.Core.Domain
open Balatro.Core.Jokers

// A sparse matrix grouped by row or by column: group g holds Index/Vals from Start[g] to Start[g + 1] - 1
type private Sparse =
    { Start: int[]
      Index: int[]
      Vals: float[] }

type private Head =
    { Wq: Sparse
      Wk: Sparse
      Wv: Sparse
      Wo: Sparse }

type private Limits =
    { Mult: float
      Chips: float
      Weight: int
      Glass: int
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
          Dims: Map<string, int>
          Heads: Head[]
          W1: Sparse
          B1: float[]
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
      Vals = entries |> Array.map snd }

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

    { D = d
      Loops = root.GetProperty("loops").GetInt32()
      Limits =
        { Mult = limits.GetProperty("mult").GetDouble()
          Chips = limits.GetProperty("chips").GetDouble()
          Weight = limits.GetProperty("weight").GetInt32()
          Glass = limits.GetProperty("glass").GetInt32()
          Jokers = limits.GetProperty("jokers").GetInt32()
          Value = limits.GetProperty("value").GetDouble()
          Left = limits.GetProperty("left").GetInt32()
          Plays = limits.GetProperty("plays").GetInt32() }
      Keys = [ for p in root.GetProperty("keys").EnumerateObject() -> p.Name, p.Value.GetString() ] |> Map.ofList
      Dims = [ for p in root.GetProperty("dims").EnumerateObject() -> p.Name, p.Value.GetInt32() ] |> Map.ofList
      Heads = [| for h in root.GetProperty("heads").EnumerateArray() -> head h |]
      W1 = sparse d true (root.GetProperty "W1")
      B1 = dense hidden (root.GetProperty "b1")
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

// Passes of the Polychrome and Steel effects, and of the Glass effect, that a hand can make; a Red Seal doubles a pass.
// A Polychrome joker is one Polychrome pass
let private passes (hand: Card list) (jokers: Joker list) =
    let count (c: Card) (flags: bool list) =
        (if c.Seal = Some Seal.Red then 2 else 1) * (flags |> List.filter id |> List.length)

    let weight =
        hand |> List.sumBy (fun c -> count c [ c.Edition = Some Polychrome; c.Enhancement = Some Enhancement.Steel ])

    weight + (jokers |> List.filter (fun j -> j.Edition = Some Polychrome) |> List.length),
    hand |> List.sumBy (fun c -> count c [ c.Enhancement = Some Enhancement.Glass ])

let private inScope (net: Net) (situation: Situation) (hand: Card list) (levels: Map<HandType, HandLevel>) =
    let weight, glass = passes hand situation.Jokers
    let limits = net.Limits
    let counted (n: int) (limit: int) = n >= 0 && n <= limit

    not hand.IsEmpty
    && hand.Length <= cardSlots
    && hand |> List.forall (fun c -> not c.Debuffed)
    && weight <= limits.Weight
    && glass <= limits.Glass
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
                x[dim $"j{slot}_v"] <- Option.defaultValue 0.0 joker.Value)

            x)
        |> List.ofArray

    let selector =
        let x = token ()
        x[dim "isSel"] <- 1.0
        x

    slotTokens @ nulls @ candidates @ [ selector ] |> Array.ofList

// out[offset + r] = row r of m times x
let private mul (m: Sparse) (x: float[]) (out: float[]) (offset: int) =
    for r in 0 .. m.Start.Length - 2 do
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

    let keyed = [| for j in 0 .. n - 1 do if Seq.exists (fun c -> k[j * dq + c] <> 0.0) (seq { 0 .. dq - 1 }) then j |]
    let isKeyed = Array.zeroCreate n

    for j in keyed do
        isKeyed[j] <- true

    let unkeyedSum = Array.zeroCreate dv

    for j in 0 .. n - 1 do
        if not isKeyed[j] then
            for c in 0 .. dv - 1 do
                unkeyedSum[c] <- unkeyedSum[c] + v[j * dv + c]

    let unkeyed = float (n - keyed.Length)
    let weights = Array.zeroCreate keyed.Length
    let mixed = Array.zeroCreate dv
    let projected = Array.zeroCreate delta[0].Length

    for i in 0 .. n - 1 do
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

        for r in 0 .. projected.Length - 1 do
            delta[i][r] <- delta[i][r] + projected[r]

// x += attention; x += W2 ReLU(W1 x + b1). Both products skip the zero entries of their input
let private block (net: Net) (xs: float[][]) (delta: float[][]) (hidden: float[]) (written: float[]) =
    for d in delta do
        Array.fill d 0 d.Length 0.0

    for head in net.Heads do
        attend head xs delta

    for i in 0 .. xs.Length - 1 do
        let x = xs[i]

        for r in 0 .. net.D - 1 do
            x[r] <- x[r] + delta[i][r]

        Array.blit net.B1 0 hidden 0 hidden.Length

        for c in 0 .. net.D - 1 do
            if x[c] <> 0.0 then
                for k in net.W1.Start[c] .. net.W1.Start[c + 1] - 1 do
                    hidden[net.W1.Index[k]] <- hidden[net.W1.Index[k]] + net.W1.Vals[k] * x[c]

        Array.fill written 0 written.Length 0.0

        for h in 0 .. hidden.Length - 1 do
            if hidden[h] > 0.0 then
                for k in net.W2.Start[h] .. net.W2.Start[h + 1] - 1 do
                    written[net.W2.Index[k]] <- written[net.W2.Index[k]] + net.W2.Vals[k] * hidden[h]

        for r in 0 .. net.D - 1 do
            x[r] <- x[r] + written[r]

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
        let hidden = Array.zeroCreate net.B1.Length
        let written = Array.zeroCreate net.D

        for _ in 1 .. net.Loops do
            block net xs delta hidden written

        let selector = xs[xs.Length - 1]
        let dim (name: string) = selector[net.Dims[name]]

        let played = [ for i in 0 .. cardSlots - 1 do if dim $"sel_slot_{i}" > 0.5 then i ]
        let handType = handTypes |> List.find (fun (_, name) -> dim $"sel_t_{name}" > 0.5) |> fst
        Some(played, Math.Round(dim "sel_total"), handType)
