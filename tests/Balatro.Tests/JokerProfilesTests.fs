module Balatro.Tests.JokerProfilesTests

open System
open System.IO
open System.Text.Json
open Xunit
open Balatro.Core.Domain
open Balatro.Core.JokerProfiles

let private fixtureKeys =
    JsonDocument
        .Parse(File.ReadAllText(AppContext.BaseDirectory + "fixtures/jokers.json"))
        .RootElement.EnumerateArray()
    |> Seq.map (fun element -> element.GetProperty("key").GetString())
    |> Set.ofSeq

[<Fact>]
let ``profiled equals the fixture keys`` () = Assert.Equal<Set<string>>(fixtureKeys, profiled)

[<Fact>]
let ``every profile has a role`` () =
    for key in fixtureKeys do
        Assert.NotEmpty((profile key).Roles)

[<Fact>]
let ``Gluttonous Joker wants Clubs`` () =
    Assert.Contains(Suit Clubs, (profile "j_gluttenous_joker").Wants)

[<Fact>]
let ``Jolly Joker wants a Pair`` () =
    Assert.Contains(Hand Pair, (profile "j_jolly").Wants)

[<Fact>]
let ``Jolly Joker adds mult`` () =
    Assert.Contains(Mult, (profile "j_jolly").Roles)

[<Fact>]
let ``Flower Pot multiplies`` () =
    Assert.Contains(XMult, (profile "j_flower_pot").Roles)

[<Fact>]
let ``Golden Joker is economy`` () =
    Assert.Contains(Economy, (profile "j_golden").Roles)

[<Fact>]
let ``Golden Joker earns 4`` () = Assert.Equal(4, (profile "j_golden").Earns)

[<Fact>]
let ``Banner wants discards`` () =
    Assert.Contains(Discards, (profile "j_banner").Wants)

[<Fact>]
let ``Mr. Bones is utility only`` () =
    Assert.Equal<Role list>([ Utility ], (profile "j_mr_bones").Roles)

[<Fact>]
let ``Fibonacci wants its five ranks`` () =
    let ranks =
        (profile "j_fibonacci").Wants
        |> List.collect (function
            | Ranks ranks -> ranks
            | _ -> [])

    Assert.Equal<Set<Rank>>(Set.ofList [ Ace; Two; Three; Five; Eight ], Set.ofList ranks)

[<Fact>]
let ``Scary Face wants faces`` () =
    Assert.Contains(Faces, (profile "j_scary_face").Wants)

[<Fact>]
let ``Steel Joker wants Steel cards`` () =
    Assert.Contains(Enhanced "Steel", (profile "j_steel_joker").Wants)

[<Fact>]
let ``Hack retriggers`` () =
    Assert.Contains(Retrigger, (profile "j_hack").Roles)

[<Fact>]
let ``unknown key gives the default`` () =
    Assert.Equal({ Roles = [ Utility ]; Wants = []; Earns = 0 }, profile "j_unknown")
