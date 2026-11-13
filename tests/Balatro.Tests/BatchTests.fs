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
