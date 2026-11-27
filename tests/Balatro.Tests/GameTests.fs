module Balatro.Tests.GameTests

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

let private unexpected name _ : Result<Api.GameState, Api.ApiError> = failwithf "unexpected call: %s" name

let private fake menu start play discard : Game.Game =
    { Menu = menu
      Start = start
      State = unexpected "State"
      Select = unexpected "Select"
      Play = play
      Discard = discard
      Buy = unexpected "Buy"
      Sell = unexpected "Sell"
      Reroll = unexpected "Reroll"
      CashOut = unexpected "CashOut"
      NextRound = unexpected "NextRound"
      Pack = unexpected "Pack"
      Use = fun _ _ -> failwith "unexpected call: Use"
      Rearrange = unexpected "Rearrange" }

let private tempDir () =
    Path.Combine(Path.GetTempPath(), "balatro-game-" + Guid.NewGuid().ToString("N"))

[<Fact>]
let ``a game over at the start ends the run as lost`` () =
    let game =
        fake (fun () -> Ok(load "BLIND_SELECT")) (fun _ _ _ -> Ok(load "GAME_OVER")) (unexpected "Play") (unexpected "Discard")

    let dir = tempDir ()

    match Runner.runOne None game Api.Deck.Red Api.Stake.White "T1" dir with
    | Ok r ->
        Assert.Equal(Runner.Lost, r.Outcome)
        Assert.Equal(0, r.Plays)
        Assert.EndsWith("outcome: lost", (File.ReadAllText(Path.Combine(dir, "trace.txt"))).TrimEnd())
    | Error e -> failwithf "%A" e

[<Fact>]
let ``the first action in a hand goes through the game`` () =
    let calls = ResizeArray<string>()

    let over name _ =
        calls.Add name
        Ok(load "GAME_OVER")

    let game =
        fake (fun () -> Ok(load "BLIND_SELECT")) (fun _ _ _ -> Ok(load "SELECTING_HAND")) (over "Play") (over "Discard")

    match Runner.runOne None game Api.Deck.Red Api.Stake.White "T1" (tempDir ()) with
    | Ok r ->
        Assert.Equal(Runner.Lost, r.Outcome)
        Assert.Equal(1, calls.Count)
    | Error e -> failwithf "%A" e
