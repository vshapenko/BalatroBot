module Balatro.Tests.BlindsTests

open Xunit
open Balatro.Core.Domain
open Balatro.Core.Blinds

let private amounts stake =
    [ 1..8 ] |> List.map (blindAmount stake)

[<Fact>]
let ``White stake ante 1 requirements`` () =
    Assert.Equal<float list>(
        [ 300.0; 450.0; 600.0 ],
        [ Small; Big; Boss ] |> List.map (blindRequirement Stake.White 1)
    )

[<Fact>]
let ``Base amounts at scaling level 1`` () =
    Assert.Equal<float list>(
        [ 300.0; 800.0; 2000.0; 5000.0; 11000.0; 20000.0; 35000.0; 50000.0 ],
        amounts Stake.White
    )

[<Fact>]
let ``Base amounts at scaling level 2`` () =
    Assert.Equal<float list>(
        [ 300.0; 900.0; 2600.0; 8000.0; 20000.0; 36000.0; 60000.0; 100000.0 ],
        amounts Stake.Green
    )

[<Fact>]
let ``Base amounts at scaling level 3`` () =
    Assert.Equal<float list>(
        [ 300.0; 1000.0; 3200.0; 9000.0; 25000.0; 60000.0; 110000.0; 200000.0 ],
        amounts Stake.Purple
    )

[<Fact>]
let ``Scaling level follows the stake`` () =
    Assert.Equal<float list>(
        [ 800.0; 900.0; 900.0; 900.0; 1000.0; 1000.0; 1000.0 ],
        [ Stake.Red; Stake.Green; Stake.Black; Stake.Blue; Stake.Purple; Stake.Orange; Stake.Gold ]
        |> List.map (fun stake -> blindAmount stake 2)
    )

[<Fact>]
let ``Ante above 8 at each scaling level`` () =
    Assert.Equal<float list>(
        [ 560000.0; 1100000.0; 2200000.0 ],
        [ Stake.White; Stake.Green; Stake.Purple ] |> List.map (fun stake -> blindAmount stake 10)
    )

[<Fact>]
let ``Ante 0 is 100`` () =
    Assert.Equal(100.0, blindAmount Stake.White 0)
