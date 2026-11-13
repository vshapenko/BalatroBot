module Balatro.Bot.Program

open System
open Microsoft.FSharp.Reflection

let private usage = "usage: run --deck <DECK> --stake <STAKE> --seed <SEED> [--url <URL>]"

let private parseUnion<'a> (s: string) =
    FSharpType.GetUnionCases typeof<'a>
    |> Array.tryFind (fun c -> c.Name.ToUpperInvariant() = s)
    |> Option.map (fun c -> FSharpValue.MakeUnion(c, [||]) :?> 'a)

let private options (args: string list) =
    let rec go acc rest =
        match rest with
        | [] -> Some(Map.ofList acc)
        | (k: string) :: v :: tail when k.StartsWith "--" -> go ((k, v) :: acc) tail
        | _ -> None

    go [] args

let private oracle (rest: string list) =
    match options rest with
    | Some o when o.Keys |> Seq.forall (fun k -> k = "--keys" || k = "--url") ->
        let keys =
            match Map.tryFind "--keys" o with
            | Some k -> k.Split ',' |> List.ofArray
            | None -> Balatro.Core.JokerRegistry.implemented |> Set.toList

        let url = Map.tryFind "--url" o |> Option.defaultValue "http://127.0.0.1:12346"

        match Oracle.run (Api.create url) keys "runs/oracle" with
        | Ok(exact, total) ->
            printfn "oracle: %d/%d exact, %d keys" exact total keys.Length
            0
        | Error e ->
            eprintfn "API error: %A" e
            1
    | _ ->
        eprintfn "usage: oracle [--keys k1,k2,...] [--url <URL>]"
        2

let private batch (rest: string list) =
    let parsed =
        options rest
        |> Option.bind (fun o ->
            match Map.tryFind "--deck" o, Map.tryFind "--stake" o, Map.tryFind "--count" o with
            | Some d, Some s, Some c ->
                match parseUnion<Api.Deck> d, parseUnion<Api.Stake> s, Int32.TryParse c with
                | Some deck, Some stake, (true, count) -> Some(deck, stake, s, count, o)
                | _ -> None
            | _ -> None)

    match parsed with
    | Some(deck, stake, stakeName, count, o) ->
        let prefix = Map.tryFind "--prefix" o |> Option.defaultValue "B"
        let url = Map.tryFind "--url" o |> Option.defaultValue "http://127.0.0.1:12346"
        let seeds = [ for i in 1..count -> sprintf "%s%04d" prefix i ]
        Batch.run (Api.create url) deck stake seeds "runs" |> Batch.summary stakeName |> printfn "%s"
        0
    | None ->
        eprintfn "usage: batch --deck <DECK> --stake <STAKE> --count <N> [--prefix <P>] [--url <URL>]"
        2

[<EntryPoint>]
let main argv =
    match List.ofArray argv with
    | "oracle" :: rest -> oracle rest
    | "batch" :: rest -> batch rest
    | _ ->

    let parsed =
        match List.ofArray argv with
        | "run" :: rest ->
            options rest
            |> Option.bind (fun o ->
                match Map.tryFind "--deck" o, Map.tryFind "--stake" o, Map.tryFind "--seed" o with
                | Some d, Some s, Some seed ->
                    match parseUnion<Api.Deck> d, parseUnion<Api.Stake> s with
                    | Some deck, Some stake ->
                        Some(deck, stake, s, seed, Map.tryFind "--url" o |> Option.defaultValue "http://127.0.0.1:12346")
                    | _ -> None
                | _ -> None)
        | _ -> None

    match parsed with
    | None ->
        eprintfn "%s" usage
        2
    | Some(deck, stake, stakeName, seed, url) ->
        match Runner.runOne (Api.create url) deck stake seed (sprintf "runs/%s-%s" stakeName seed) with
        | Ok r ->
            printfn
                "%s, ante %d, round %d, plays %d, exact predictions %d/%d"
                (Runner.outcomeText r.Outcome)
                r.Ante
                r.Round
                r.Plays
                r.ExactPredictions
                r.Plays

            0
        | Error e ->
            eprintfn "API error: %A" e
            1
