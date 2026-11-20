module Balatro.Tests.TarotTests

open Xunit
open Balatro.Core.Domain
open Balatro.Bot
open Balatro.Tests.HandsTests

let private planIn suit : Plan.Plan =
    { Hand = Flush
      Suit = suit
      Share = 0.5
      Level = 1
      Fans = [] }

// 0 K spades, 1 3 hearts, 2 9 diamonds, 3 5 clubs, 4 2 hearts, 5 5 spades
let private hand =
    [ card King Spades
      card Three Hearts
      card Nine Diamonds
      card Five Clubs
      card Two Hearts
      card Five Spades ]

let private star = Tarot.ToSuit(Diamonds, 3)
let private empress = Tarot.Enhance(Enhancement.Mult, 2)

[<Fact>]
let ``effect maps the modelled tarots and nothing else`` () =
    Assert.Equal(Some star, Tarot.effect "c_star")
    Assert.Equal(Some empress, Tarot.effect "c_empress")
    Assert.Equal(Some(Tarot.Destroy 2), Tarot.effect "c_hanged_man")
    Assert.Equal(Some(Tarot.Enhance(Enhancement.Bonus, 2)), Tarot.effect "c_heirophant")
    Assert.Equal(Some Tarot.Money, Tarot.effect "c_hermit")
    Assert.Equal(None, Tarot.effect "c_fool")

[<Fact>]
let ``a suit tarot takes the lowest off-suit cards of its plan suit and nothing for another`` () =
    Assert.Equal<int list>([ 4; 1; 3 ], Tarot.targets star (planIn (Some Diamonds)) hand)
    Assert.Empty(Tarot.targets star (planIn (Some Hearts)) hand)
    Assert.Empty(Tarot.targets star (planIn None) hand)

[<Fact>]
let ``a suit tarot leaves wild and stone cards alone`` () =
    let wild = hand |> List.mapi (fun i c -> if i = 4 then withEnhancement Enhancement.Wild c else c)
    Assert.Equal<int list>([ 1; 3; 5 ], Tarot.targets star (planIn (Some Diamonds)) wild)

[<Fact>]
let ``an enhancer prefers the cards of the plan suit`` () =
    Assert.Equal<int list>([ 0; 5 ], Tarot.targets empress (planIn (Some Spades)) hand)
    Assert.Equal<int list>([ 2 ], Tarot.targets empress (planIn (Some Diamonds)) hand)

[<Fact>]
let ``an enhancer without a suit prefers a paired rank and else the highest cards`` () =
    Assert.Equal<int list>([ 3; 5 ], Tarot.targets empress (planIn None) hand)
    Assert.Equal<int list>([ 0; 2 ], Tarot.targets empress (planIn None) hand[0..2])

[<Fact>]
let ``an enhancer skips enhanced cards`` () =
    let marked = hand |> List.mapi (fun i c -> if i = 0 then withEnhancement Enhancement.Bonus c else c)
    Assert.Equal<int list>([ 5 ], Tarot.targets empress (planIn (Some Spades)) marked)

[<Fact>]
let ``the wild tarot needs a plan suit and takes the highest off-suit cards`` () =
    let lovers = Tarot.Enhance(Enhancement.Wild, 1)
    Assert.Equal<int list>([ 0 ], Tarot.targets lovers (planIn (Some Hearts)) hand)
    Assert.Empty(Tarot.targets lovers (planIn None) hand)

[<Fact>]
let ``the hanged man destroys the lowest cards off the plan suit`` () =
    let hanged = Tarot.Destroy 2
    Assert.Equal<int list>([ 3; 5 ], Tarot.targets hanged (planIn (Some Hearts)) hand)
    Assert.Equal<int list>([ 4; 1 ], Tarot.targets hanged (planIn None) hand)

[<Fact>]
let ``a money tarot has no targets`` () =
    Assert.Empty(Tarot.targets Tarot.Money (planIn (Some Hearts)) hand)

[<Fact>]
let ``transform applies each effect to the chosen cards only`` () =
    let suited = Tarot.transform star [ 4; 1 ] hand
    Assert.Equal<Suit list>([ Spades; Diamonds; Diamonds; Clubs; Diamonds; Spades ], suited |> List.map (fun c -> c.Suit))

    let enhanced = Tarot.transform empress [ 0; 5 ] hand
    Assert.Equal<Enhancement option list>(
        [ Some Enhancement.Mult; None; None; None; None; Some Enhancement.Mult ],
        enhanced |> List.map (fun c -> c.Enhancement)
    )

    Assert.Equal<Card list>([ hand[0]; hand[2]; hand[4]; hand[5] ], Tarot.transform (Tarot.Destroy 2) [ 1; 3 ] hand)
    Assert.Equal<Card list>(hand, Tarot.transform Tarot.Money [] hand)
