module Balatro.Bot.PlayNet

open System
open System.IO
open System.Text.Json
open Balatro.Core.Domain

// A sparse matrix by rows: row r holds Cols/Vals from Start[r] to Start[r + 1] - 1
type private Sparse =
    { Start: int[]
      Cols: int[]
      Vals: float[] }

type private Head =
    { Wq: Sparse
      Wk: Sparse
      Wv: Sparse
      Wo: Sparse }

type Net =
    private
        { D: int
          Loops: int
          JStart: float
          Dims: Map<string, int>
          Heads: Head[]
          W1: Sparse
          B1: float[]
          W2: Sparse
          B2: float[] }

let private maxMult = 200.0
let private maxChips = 5000.0
let private slots = 13
let private cardSlots = 8

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

let private sparse (rows: int) (triplets: JsonElement) =
    let entries =
        [| for t in triplets.EnumerateArray() -> t[0].GetInt32(), t[1].GetInt32(), t[2].GetDouble() |]
        |> Array.sortBy (fun (r, c, _) -> r, c)

    let start = Array.zeroCreate (rows + 1)

    for r, _, _ in entries do
        start[r + 1] <- start[r + 1] + 1

    for r in 0 .. rows - 1 do
        start[r + 1] <- start[r + 1] + start[r]

    { Start = start
      Cols = entries |> Array.map (fun (_, c, _) -> c)
      Vals = entries |> Array.map (fun (_, _, v) -> v) }

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

    let head (h: JsonElement) =
        let wq = h.GetProperty "Wq"
        let wv = h.GetProperty "Wv"

        { Wq = sparse (rowsOf wq) wq
          Wk = sparse (rowsOf wq) (h.GetProperty "Wk")
          Wv = sparse (rowsOf wv) wv
          Wo = sparse d (h.GetProperty "Wo") }

    { D = d
      Loops = root.GetProperty("loops").GetInt32()
      JStart = root.GetProperty("jStart").GetDouble()
      Dims = [ for p in root.GetProperty("dims").EnumerateObject() -> p.Name, p.Value.GetInt32() ] |> Map.ofList
      Heads = [| for h in root.GetProperty("heads").EnumerateArray() -> head h |]
      W1 = sparse hidden (root.GetProperty "W1")
      B1 = dense hidden (root.GetProperty "b1")
      W2 = sparse d (root.GetProperty "W2")
      B2 = dense d (root.GetProperty "b2") }

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

let private inScope (hand: Card list) (levels: Map<HandType, HandLevel>) =
    not hand.IsEmpty
    && hand.Length <= cardSlots
    && hand
       |> List.forall (fun c -> c.Enhancement.IsNone && c.Edition.IsNone && c.Seal.IsNone && not c.Debuffed)
    && handTypes
       |> List.forall (fun (t, _) ->
           let level = levels[t]

           isWhole level.Mult
           && level.Mult >= 1.0
           && level.Mult <= maxMult
           && isWhole level.Chips
           && level.Chips >= 0.0
           && level.Chips <= maxChips)

// Tokens: the cards, 5 nulls, one per subset of the cards, the selector
let private encode (net: Net) (hand: Card list) (levels: Map<HandType, HandLevel>) : float[][] =
    let dim (name: string) = net.Dims[name]

    let token () =
        let x = Array.zeroCreate net.D
        x[dim "one"] <- 1.0
        x[dim "j"] <- net.JStart
        x

    let cards =
        hand
        |> List.mapi (fun i card ->
            let x = token ()
            x[dim "isCard"] <- 1.0
            x[dim $"slot_{i}"] <- 1.0
            x[dim $"rank_{Rank.id card.Rank}"] <- 1.0
            x[dim $"suit_{suitName card.Suit}"] <- 1.0
            x[dim "nominal"] <- float (Rank.nominal card.Rank)
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

            for i in subset do
                x[dim $"member_{i}"] <- 1.0

            for k in 0 .. 5 - subset.Length - 1 do
                x[dim $"member_{cardSlots + k}"] <- 1.0

            for t, name in handTypes do
                x[dim $"C_{name}"] <- levels[t].Chips
                x[dim $"M_{name}"] <- levels[t].Mult

            x)
        |> List.ofArray

    let selector =
        let x = token ()
        x[dim "isSel"] <- 1.0
        x

    cards @ nulls @ candidates @ [ selector ] |> Array.ofList

// out[offset + r] = row r of m times x
let private mul (m: Sparse) (x: float[]) (out: float[]) (offset: int) =
    for r in 0 .. m.Start.Length - 2 do
        let mutable sum = 0.0

        for k in m.Start[r] .. m.Start[r + 1] - 1 do
            sum <- sum + m.Vals[k] * x[m.Cols[k]]

        out[offset + r] <- sum

let private rows (m: Sparse) = m.Start.Length - 1

// Dense softmax attention with max subtraction over all tokens; the result is added to delta
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

    let weights = Array.zeroCreate n
    let mixed = Array.zeroCreate dv
    let projected = Array.zeroCreate delta[0].Length

    for i in 0 .. n - 1 do
        let mutable top = Double.NegativeInfinity

        for j in 0 .. n - 1 do
            let mutable s = 0.0

            for c in 0 .. dq - 1 do
                s <- s + q[i * dq + c] * k[j * dq + c]

            weights[j] <- s
            top <- Math.Max(top, s)

        let mutable total = 0.0

        for j in 0 .. n - 1 do
            weights[j] <- Math.Exp(weights[j] - top)
            total <- total + weights[j]

        Array.fill mixed 0 dv 0.0

        for j in 0 .. n - 1 do
            let w = weights[j] / total

            for c in 0 .. dv - 1 do
                mixed[c] <- mixed[c] + w * v[j * dv + c]

        mul head.Wo mixed projected 0

        for r in 0 .. projected.Length - 1 do
            delta[i][r] <- delta[i][r] + projected[r]

let private block (net: Net) (xs: float[][]) (delta: float[][]) (hidden: float[]) (written: float[]) =
    for d in delta do
        Array.fill d 0 d.Length 0.0

    for head in net.Heads do
        attend head xs delta

    for i in 0 .. xs.Length - 1 do
        for r in 0 .. net.D - 1 do
            xs[i][r] <- xs[i][r] + delta[i][r]

        mul net.W1 xs[i] hidden 0

        for h in 0 .. hidden.Length - 1 do
            hidden[h] <- Math.Max(0.0, hidden[h] + net.B1[h])

        mul net.W2 hidden written 0

        for r in 0 .. net.D - 1 do
            xs[i][r] <- xs[i][r] + written[r] + net.B2[r]

let choose (net: Net) (hand: Card list) (levels: Map<HandType, HandLevel>) : (int list * float * HandType) option =
    if not (inScope hand levels) then
        None
    else
        let xs = encode net hand levels
        let delta = Array.init xs.Length (fun _ -> Array.zeroCreate net.D)
        let hidden = Array.zeroCreate (rows net.W1)
        let written = Array.zeroCreate net.D

        for _ in 1 .. net.Loops do
            block net xs delta hidden written

        let selector = xs[xs.Length - 1]
        let dim (name: string) = selector[net.Dims[name]]

        let positions = [ for i in 0 .. cardSlots - 1 do if dim $"sel_slot_{i}" > 0.5 then i ]
        let handType = handTypes |> List.find (fun (_, name) -> dim $"sel_t_{name}" > 0.5) |> fst
        Some(positions, Math.Round(dim "sel_acc"), handType)
