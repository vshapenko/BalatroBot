module Balatro.Bot.Program

open System
open Microsoft.FSharp.Reflection

let private usage =
    String.Join(
        "\n",
        [ "usage:"
          "  run --deck <DECK> --stake <STAKE> --seed <SEED> [--net <PATH>] [--shop-net <PATH>] [--url <URL>]"
          "  batch --deck <DECK> --stake <STAKE> --count <N> [--prefix <P>] [--net <PATH>] [--shop-net <PATH>] [--url <URL>]"
          "  sim --count <N> [--from <K>] [--parallel <P>] [--fast] [--net <PATH>] [--shop-net <PATH>]"
          "  oracle [--deck <DECK>] [--stake <STAKE>] [--keys k1,k2,...] [--url <URL>]" ]
    )

let private parseUnion<'a> (s: string) =
    FSharpType.GetUnionCases typeof<'a>
    |> Array.tryFind (fun c -> c.Name.ToUpperInvariant() = s)
    |> Option.map (fun c -> FSharpValue.MakeUnion(c, [||]) :?> 'a)

let private shopLine () =
    let picks, declines, differs = Shop.networkCounts ()
    sprintf "shop network: %d picks, %d declined, %d differ" picks declines differs

let private options (args: string list) =
    let rec go acc rest =
        match rest with
        | [] -> Some(Map.ofList acc)
        | (k: string) :: v :: tail when k.StartsWith "--" -> go ((k, v) :: acc) tail
        | _ -> None

    go [] args

let private oracle (rest: string list) =
    let parsed =
        options rest
        |> Option.filter (fun o -> o.Keys |> Seq.forall (fun k -> k = "--deck" || k = "--stake" || k = "--keys" || k = "--url"))
        |> Option.bind (fun o ->
            match
                parseUnion<Api.Deck> (Map.tryFind "--deck" o |> Option.defaultValue "RED"),
                parseUnion<Api.Stake> (Map.tryFind "--stake" o |> Option.defaultValue "WHITE")
            with
            | Some deck, Some stake -> Some(deck, stake, o)
            | _ -> None)

    match parsed with
    | Some(deck, stake, o) ->
        let keys =
            match Map.tryFind "--keys" o with
            | Some k -> k.Split ',' |> List.ofArray
            | None -> Balatro.Core.JokerRegistry.implemented |> Set.toList

        let url = Map.tryFind "--url" o |> Option.defaultValue "http://127.0.0.1:12346"

        match Oracle.run (Api.create url) deck stake keys "runs/oracle" with
        | Ok(exact, total) ->
            printfn "oracle: %d/%d exact, %d keys" exact total keys.Length
            0
        | Error e ->
            eprintfn "API error: %A" e
            1
    | None ->
        eprintfn "usage: oracle [--deck <DECK>] [--stake <STAKE>] [--keys k1,k2,...] [--url <URL>]"
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
        let net = Map.tryFind "--net" o |> Option.map PlayNet.load
        let shopNet = Map.tryFind "--shop-net" o |> Option.map PlayNet.load
        Shop.useNetwork shopNet
        Batch.run net (Game.live (Api.create url)) deck stake seeds "runs" |> Batch.summary stakeName |> printfn "%s"
        if shopNet.IsSome then printfn "%s" (shopLine ())
        0
    | None ->
        eprintfn "usage: batch --deck <DECK> --stake <STAKE> --count <N> [--prefix <P>] [--net <PATH>] [--shop-net <PATH>] [--url <URL>]"
        2

let private sim (rest: string list) =
    let flags, rest = rest |> List.partition (fun a -> a = "--fast")
    let fast = not flags.IsEmpty

    let parsed =
        options rest
        |> Option.filter (fun o -> o.Keys |> Seq.forall (fun k -> List.contains k [ "--count"; "--from"; "--parallel"; "--net"; "--shop-net" ]))
        |> Option.bind (fun o ->
            let number key fallback =
                match Map.tryFind key o with
                | None -> fallback
                | Some v ->
                    match Int32.TryParse v with
                    | true, n when n >= 1 -> Some n
                    | _ -> None

            match number "--count" None, number "--from" (Some 1), number "--parallel" (Some(max 1 (Environment.ProcessorCount / 2))) with
            | Some count, Some from, Some degree -> Some(count, from, degree, o)
            | _ -> None)

    match parsed with
    | Some(count, from, degree, o) ->
        let seeds = [ for i in from .. from + count - 1 -> sprintf "SIM%04d" i ]
        let net = Map.tryFind "--net" o |> Option.map PlayNet.load
        let shopNet = Map.tryFind "--shop-net" o |> Option.map PlayNet.load
        Shop.useNetwork shopNet
        if fast then Policy.shopSamples <- (4, 8)
        let watch = Diagnostics.Stopwatch.StartNew()

        let rows =
            Batch.runParallel degree net Sim.create Api.Deck.Red Api.Stake.White seeds (if fast then "runs/sim-fast" else "runs/sim")

        Batch.summary "WHITE" rows |> printfn "%s"
        let seconds = watch.Elapsed.TotalSeconds
        printfn "wall time %.0f s, %.1f runs per hour" seconds (float rows.Length * 3600.0 / seconds)
        if shopNet.IsSome then printfn "%s" (shopLine ())
        0
    | None ->
        eprintfn "usage: sim --count <N> [--from <K>] [--parallel <P>] [--fast] [--net <PATH>] [--shop-net <PATH>]"
        2

[<EntryPoint>]
let main argv =
    match List.ofArray argv with
    | "oracle" :: rest -> oracle rest
    | "batch" :: rest -> batch rest
    | "sim" :: rest -> sim rest
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
                        Some(
                            deck,
                            stake,
                            s,
                            seed,
                            Map.tryFind "--url" o |> Option.defaultValue "http://127.0.0.1:12346",
                            Map.tryFind "--net" o |> Option.map PlayNet.load,
                            Map.tryFind "--shop-net" o |> Option.map PlayNet.load
                        )
                    | _ -> None
                | _ -> None)
        | _ -> None

    match parsed with
    | None ->
        eprintfn "%s" usage
        2
    | Some(deck, stake, stakeName, seed, url, net, shopNet) ->
        Shop.useNetwork shopNet

        match Runner.runOne net (Game.live (Api.create url)) deck stake seed (sprintf "runs/%s-%s" stakeName seed) with
        | Ok r ->
            printfn
                "%s, ante %d, round %d, plays %d, exact predictions %d/%d"
                (Runner.outcomeText r.Outcome)
                r.Ante
                r.Round
                r.Plays
                r.ExactPredictions
                r.Plays

            if shopNet.IsSome then printfn "%s" (shopLine ())
            0
        | Error e ->
            eprintfn "API error: %A" e
            1
