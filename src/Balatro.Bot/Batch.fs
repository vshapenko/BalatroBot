module Balatro.Bot.Batch

open System
open System.IO

type Row =
    { Seed: string
      Outcome: string
      Ante: int
      Round: int
      Plays: int
      Discards: int
      Purchases: int
      Exact: int
      Mismatches: int }

let summary (stake: string) (rows: Row list) : string =
    match rows with
    | [] -> sprintf "batch %s: 0 runs" stake
    | _ ->
        let n = rows.Length
        let won = rows |> List.filter (fun r -> r.Outcome = "won") |> List.length
        let pct = Math.Round(100.0 * float won / float n, MidpointRounding.AwayFromZero) |> int
        let played = rows |> List.filter (fun r -> not (r.Outcome.StartsWith "error"))
        let mean = if played.IsEmpty then 0.0 else played |> List.averageBy (fun r -> float r.Ante)

        let antes =
            played
            |> List.countBy (fun r -> r.Ante)
            |> List.sortBy fst
            |> List.map (fun (a, c) -> sprintf "%dx%d" a c)
            |> String.concat " "

        let count (prefix: string) =
            rows |> List.filter (fun r -> r.Outcome.StartsWith prefix) |> List.length

        sprintf
            "batch %s: %d runs, %d won (%d%%), mean ante %.1f, antes %s, exact predictions %d/%d, aborted %d, errors %d"
            stake
            n
            won
            pct
            mean
            antes
            (rows |> List.sumBy (fun r -> r.Exact))
            (rows |> List.sumBy (fun r -> r.Plays))
            (count "aborted")
            (count "error")

let errorText (e: Api.ApiError) =
    match e with
    | Api.Transport m -> "error: " + Runner.oneLine m
    | Api.Rpc r -> sprintf "error: %s: %s" r.Name (Runner.oneLine r.Message)

let private csvLine (r: Row) =
    let outcome = "\"" + r.Outcome.Replace("\"", "\"\"") + "\""

    sprintf "%s,%s,%d,%d,%d,%d,%d,%d,%d" r.Seed outcome r.Ante r.Round r.Plays r.Discards r.Purchases r.Exact r.Mismatches

let private header = "seed,outcome,ante,round,plays,discards,purchases,exact,mismatches"

let private stakeName (stake: Api.Stake) = (sprintf "%A" stake).ToUpperInvariant()

let private playSeed net game deck stake (seed: string) (outDir: string) : Row =
    let result = Runner.runOne net game deck stake seed (Path.Combine(outDir, sprintf "%s-%s" (stakeName stake) seed))

    let row =
        match result with
        | Ok r ->
            { Seed = seed
              Outcome = Runner.outcomeText r.Outcome
              Ante = r.Ante
              Round = r.Round
              Plays = r.Plays
              Discards = r.Discards
              Purchases = r.Purchases
              Exact = r.ExactPredictions
              Mismatches = r.Mismatches }
        | Error e ->
            { Seed = seed
              Outcome = errorText e
              Ante = 0
              Round = 0
              Plays = 0
              Discards = 0
              Purchases = 0
              Exact = 0
              Mismatches = 0 }

    printfn "%s: %s, ante %d, round %d, plays %d, exact %d/%d" seed row.Outcome row.Ante row.Round row.Plays row.Exact row.Plays
    row

let private csvPath (stake: Api.Stake) (outDir: string) =
    Directory.CreateDirectory outDir |> ignore
    Path.Combine(outDir, sprintf "batch-%s.csv" (stakeName stake))

let run (net: PlayNet.Net option) (game: Game.Game) (deck: Api.Deck) (stake: Api.Stake) (seeds: string list) (outDir: string) : Row list =
    let csv = csvPath stake outDir
    File.Delete csv
    File.AppendAllLines(csv, [ header ])

    seeds
    |> List.map (fun seed ->
        let row = playSeed net game deck stake seed outDir
        File.AppendAllLines(csv, [ csvLine row ])
        row)

let runParallel
    (degree: int)
    (net: PlayNet.Net option)
    (makeGame: unit -> Game.Game)
    (deck: Api.Deck)
    (stake: Api.Stake)
    (seeds: string list)
    (outDir: string)
    : Row list =
    let csv = csvPath stake outDir
    let seeds = Array.ofList seeds
    let rows = Array.zeroCreate<Row> seeds.Length

    Threading.Tasks.Parallel.For(
        0,
        seeds.Length,
        Threading.Tasks.ParallelOptions(MaxDegreeOfParallelism = degree),
        fun i -> rows[i] <- playSeed net (makeGame ()) deck stake seeds[i] outDir
    )
    |> ignore

    File.WriteAllLines(csv, header :: [ for r in rows -> csvLine r ])
    List.ofArray rows
