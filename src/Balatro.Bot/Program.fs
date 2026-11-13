module Balatro.Bot.Program

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

[<EntryPoint>]
let main argv =
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
                (if r.Won then "won" else "lost")
                r.Ante
                r.Round
                r.Plays
                r.ExactPredictions
                r.Plays

            0
        | Error e ->
            eprintfn "API error: %A" e
            1
