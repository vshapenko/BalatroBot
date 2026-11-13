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
        let mean = rows |> List.averageBy (fun r -> float r.Ante)

        let antes =
            rows
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

let private csvLine (r: Row) =
    let outcome = "\"" + r.Outcome.Replace("\"", "\"\"") + "\""

    sprintf "%s,%s,%d,%d,%d,%d,%d,%d,%d" r.Seed outcome r.Ante r.Round r.Plays r.Discards r.Purchases r.Exact r.Mismatches

let run (client: Api.Client) (deck: Api.Deck) (stake: Api.Stake) (seeds: string list) (outDir: string) : Row list =
    let stakeName = (sprintf "%A" stake).ToUpperInvariant()
    Directory.CreateDirectory outDir |> ignore
    let csv = Path.Combine(outDir, sprintf "batch-%s.csv" stakeName)
    File.Delete csv
    File.AppendAllLines(csv, [ "seed,outcome,ante,round,plays,discards,purchases,exact,mismatches" ])

    seeds
    |> List.map (fun seed ->
        let row =
            match Runner.runOne client deck stake seed (Path.Combine(outDir, sprintf "%s-%s" stakeName seed)) with
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
                  Outcome = sprintf "error: %A" e
                  Ante = 0
                  Round = 0
                  Plays = 0
                  Discards = 0
                  Purchases = 0
                  Exact = 0
                  Mismatches = 0 }

        printfn "%s: %s, ante %d, round %d, plays %d, exact %d/%d" seed row.Outcome row.Ante row.Round row.Plays row.Exact row.Plays
        File.AppendAllLines(csv, [ csvLine row ])
        row)
