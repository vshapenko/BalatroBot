module Balatro.Tests.BatchTests

open System
open System.IO
open System.Text.Json
open Xunit
open Balatro.Bot

let private load name =
    let text = File.ReadAllText(AppContext.BaseDirectory + "fixtures/gamestate." + name + ".json")

    match Api.decodeGameState (JsonDocument.Parse(text).RootElement) with
    | Ok g -> g
    | Error e -> failwithf "%s: %s" name e

let private row seed outcome ante plays exact : Batch.Row =
    { Seed = seed
      Outcome = outcome
      Ante = ante
      Round = 1
      Plays = plays
      Discards = 0
      Purchases = 0
      Exact = exact
      Mismatches = plays - exact }

[<Fact>]
let ``summary of three runs`` () =
    let rows =
        [ row "A" "won" 9 22 20
          row "B" "lost" 4 10 10
          row "C" "aborted: unexpected state" 2 4 3 ]

    Assert.Equal(
        "batch WHITE: 3 runs, 1 won (33%), mean ante 5.0, antes 2x1 4x1 9x1, exact predictions 33/36, aborted 1, errors 0",
        Batch.summary "WHITE" rows
    )

[<Fact>]
let ``summary of no runs`` () =
    Assert.Equal("batch WHITE: 0 runs", Batch.summary "WHITE" [])

[<Fact>]
let ``a won flag carried over from an earlier run does not end the run`` () =
    let g = { load "SELECTING_HAND" with Won = true }
    Assert.False(Runner.isWon false g)

[<Fact>]
let ``beating the ante 8 boss ends the run as won`` () =
    let g = { load "ROUND_EVAL" with Won = true; AnteNum = 9 }
    Assert.True(Runner.isWon false g)

[<Fact>]
let ``losing the ante 8 boss is not a win`` () =
    let g = { load "GAME_OVER" with Won = true; AnteNum = 8 }
    Assert.False(Runner.isWon false g)
    Assert.False(Runner.isWon true { g with AnteNum = 9 })

[<Fact>]
let ``error rows stay out of the mean ante and the histogram`` () =
    let rows =
        [ row "A" "won" 9 22 20
          row "B" "lost" 4 10 10
          row "C" "aborted: unexpected state" 2 4 3
          row "D" "error: timeout" 0 0 0 ]

    Assert.Equal(
        "batch WHITE: 4 runs, 1 won (25%), mean ante 5.0, antes 2x1 4x1 9x1, exact predictions 33/36, aborted 1, errors 1",
        Batch.summary "WHITE" rows
    )

[<Fact>]
let ``win rule needs a high ante and the won flag`` () =
    let g = load "ROUND_EVAL"
    Assert.False(Runner.isWon true { g with AnteNum = 1; Won = true })
    Assert.True(Runner.isWon true { g with AnteNum = 9; Won = false })
    Assert.False(Runner.isWon false { g with AnteNum = 9; Won = false })

[<Fact>]
let ``an error outcome is one line`` () =
    Assert.DoesNotContain('\n', Batch.errorText (Api.Transport "first\r\nsecond\nthird"))
    Assert.Equal("error: Bad: a b", Batch.errorText (Api.Rpc { Code = 1; Name = "Bad"; Message = "a\nb" }))

[<Fact>]
let ``context line names the blind and the owned jokers`` () =
    let g = load "SELECTING_HAND.with_joker"
    let json, text = Runner.context 7 g
    use doc = JsonDocument.Parse json
    Assert.Equal("context", doc.RootElement.GetProperty("kind").GetString())
    Assert.Equal(1, doc.RootElement.GetProperty("jokers").GetArrayLength())
    Assert.StartsWith("#7 context: blind ", text)
    Assert.DoesNotContain('\n', text)

[<Fact>]
let ``parallel runs return rows and write the csv in seed order`` () =
    let antes = dict [ "P1", (3, 400); "P2", (5, 50); "P3", (2, 250); "P4", (7, 0); "P5", (4, 150) ]
    let unexpected _ = failwith "unexpected call"
    let over = load "GAME_OVER"

    let makeGame () : Game.Game =
        { Menu = fun () -> Ok over
          Start =
            fun _ _ seed ->
                let ante, delay = antes[Option.get seed]
                Threading.Thread.Sleep(delay: int)
                Ok { over with AnteNum = ante }
          State = unexpected
          Select = unexpected
          Play = unexpected
          Discard = unexpected
          Buy = unexpected
          Sell = unexpected
          Reroll = unexpected
          CashOut = unexpected
          NextRound = unexpected
          Pack = unexpected
          Use = fun _ _ -> failwith "unexpected call"
          Rearrange = unexpected }

    let seeds = [ "P1"; "P2"; "P3"; "P4"; "P5" ]
    let dir = Path.Combine(Path.GetTempPath(), "balatro-batch-" + Guid.NewGuid().ToString("N"))
    let rows = Batch.runParallel 3 None makeGame Api.Deck.Red Api.Stake.White seeds dir
    Assert.Equal<string list>(seeds, rows |> List.map (fun r -> r.Seed))
    Assert.Equal<int list>([ 3; 5; 2; 7; 4 ], rows |> List.map (fun r -> r.Ante))
    let lines = File.ReadAllLines(Path.Combine(dir, "batch-WHITE.csv"))
    Assert.Equal<string list>(seeds, lines |> Array.skip 1 |> Array.map (fun l -> l.Split(',')[0]) |> List.ofArray)
