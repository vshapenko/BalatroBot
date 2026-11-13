module Balatro.Tests.HandsTests

open Xunit
open Balatro.Core.Domain
open Balatro.Core.Hands

let card rank suit =
    { Rank = rank
      Suit = suit
      Enhancement = None
      Edition = None
      Seal = None
      Debuffed = false }

let withEnhancement enhancement (c: Card) =
    { c with Enhancement = Some enhancement }

[<Fact>]
let ``Flush Five`` () =
    let cards = List.replicate 5 (card Ace Spades)
    Assert.Equal((FlushFive, cards), classify cards)

[<Fact>]
let ``Flush House`` () =
    let cards =
        [ card Seven Diamonds
          card Seven Diamonds
          card Seven Diamonds
          card Four Diamonds
          card Four Diamonds ]

    Assert.Equal((FlushHouse, cards), classify cards)

[<Fact>]
let ``Five of a Kind`` () =
    let cards =
        [ card Ace Spades
          card Ace Hearts
          card Ace Hearts
          card Ace Clubs
          card Ace Diamonds ]

    Assert.Equal((FiveOfAKind, cards), classify cards)

[<Fact>]
let ``Straight Flush`` () =
    let cards =
        [ card Queen Spades
          card Jack Spades
          card Ten Spades
          card Nine Spades
          card Eight Spades ]

    Assert.Equal((StraightFlush, cards), classify cards)

[<Fact>]
let ``Royal flush is a Straight Flush`` () =
    let cards =
        [ card Ace Hearts
          card King Hearts
          card Queen Hearts
          card Jack Hearts
          card Ten Hearts ]

    Assert.Equal((StraightFlush, cards), classify cards)

[<Fact>]
let ``Four of a Kind scores only the four`` () =
    let four =
        [ card Jack Spades
          card Jack Hearts
          card Jack Clubs
          card Jack Diamonds ]

    Assert.Equal((FourOfAKind, four), classify (four @ [ card Three Clubs ]))

[<Fact>]
let ``Full House`` () =
    let cards =
        [ card King Hearts
          card King Clubs
          card King Diamonds
          card Two Spades
          card Two Diamonds ]

    Assert.Equal((FullHouse, cards), classify cards)

[<Fact>]
let ``Flush of five hearts`` () =
    let cards =
        [ card Ace Hearts
          card King Hearts
          card Ten Hearts
          card Five Hearts
          card Four Hearts ]

    Assert.Equal((Flush, cards), classify cards)

[<Fact>]
let ``Straight of five ranks`` () =
    let cards =
        [ card Jack Diamonds
          card Ten Clubs
          card Nine Clubs
          card Eight Spades
          card Seven Hearts ]

    Assert.Equal((Straight, cards), classify cards)

[<Fact>]
let ``Three of a Kind scores only the three`` () =
    let three =
        [ card Ten Spades
          card Ten Clubs
          card Ten Diamonds ]

    Assert.Equal((ThreeOfAKind, three), classify (three @ [ card Six Hearts; card Five Diamonds ]))

[<Fact>]
let ``Two Pair scores only the two pairs in played order`` () =
    let aces = card Ace Hearts
    let fours = card Four Hearts
    let cards = [ fours; aces; card Queen Clubs; fours; aces ]
    Assert.Equal((TwoPair, [ fours; aces; fours; aces ]), classify cards)

[<Fact>]
let ``Pair of nines`` () =
    let pair = [ card Nine Spades; card Nine Diamonds ]

    Assert.Equal(
        (Pair, pair),
        classify [ card King Spades; card Nine Spades; card Nine Diamonds; card Six Hearts; card Three Diamonds ]
    )

[<Fact>]
let ``High Card scores only the highest card`` () =
    let cards =
        [ card Queen Diamonds
          card Ace Spades
          card Nine Diamonds
          card Four Clubs
          card Three Diamonds ]

    Assert.Equal((HighCard, [ card Ace Spades ]), classify cards)

[<Fact>]
let ``Five cards with one pair score exactly the paired cards`` () =
    let cards =
        [ card Two Clubs
          card Eight Hearts
          card Five Spades
          card Eight Diamonds
          card King Clubs ]

    let handType, scoring = classify cards
    Assert.Equal(Pair, handType)
    Assert.Equal<Card list>([ card Eight Hearts; card Eight Diamonds ], scoring)

[<Fact>]
let ``Stone card in a Pair play scores`` () =
    let stone = card King Clubs |> withEnhancement Enhancement.Stone
    let cards = [ card Nine Spades; stone; card Nine Hearts; card Four Clubs ]
    let handType, scoring = classify cards
    Assert.Equal(Pair, handType)
    Assert.Equal<Card list>([ card Nine Spades; stone; card Nine Hearts ], scoring)

[<Fact>]
let ``Wild card completes a Flush`` () =
    let wild = card Two Spades |> withEnhancement Enhancement.Wild

    let cards =
        [ card Ace Hearts
          card King Hearts
          wild
          card Nine Hearts
          card Four Hearts ]

    Assert.Equal((Flush, cards), classify cards)

[<Fact>]
let ``Four suited cards are not a Flush`` () =
    let cards =
        [ card Ace Hearts
          card King Hearts
          card Nine Hearts
          card Four Hearts ]

    Assert.Equal((HighCard, [ card Ace Hearts ]), classify cards)

[<Fact>]
let ``Ace low Straight`` () =
    let cards =
        [ card Ace Spades
          card Two Hearts
          card Three Clubs
          card Four Diamonds
          card Five Spades ]

    Assert.Equal((Straight, cards), classify cards)

[<Fact>]
let ``Straight does not wrap around the Ace`` () =
    let cards =
        [ card Queen Spades
          card King Hearts
          card Ace Clubs
          card Two Diamonds
          card Three Spades ]

    Assert.Equal((HighCard, [ card Ace Clubs ]), classify cards)
