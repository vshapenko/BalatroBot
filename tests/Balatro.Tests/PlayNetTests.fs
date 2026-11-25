module Balatro.Tests.PlayNetTests

open System
open System.IO
open System.Text.Json
open Xunit
open Balatro.Core
open Balatro.Core.Domain
open Balatro.Core.Jokers
open Balatro.Bot
open Balatro.Tests.HandsTests

let private net = lazy (PlayNet.load (AppContext.BaseDirectory + "nn/playnet.json"))

// Every in-scope joker key with the one-hot name the net gives it; "inert" marks a key with no score effect
let private jokerKeys =
    lazy
        (use doc = JsonDocument.Parse(File.ReadAllText(AppContext.BaseDirectory + "nn/playnet.json"))
         [ for p in doc.RootElement.GetProperty("keys").EnumerateObject() -> p.Name, p.Value.GetString() ])

// The joker keys the net multiplies by an input factor, and the Uncommon keys
let private netSets =
    lazy
        (use doc = JsonDocument.Parse(File.ReadAllText(AppContext.BaseDirectory + "nn/playnet.json"))
         let keys (name: string) = [ for k in doc.RootElement.GetProperty(name).EnumerateArray() -> k.GetString() ] |> Set.ofList
         keys "factors", keys "uncommon")

let private handTypes =
    [ FlushFive
      FlushHouse
      FiveOfAKind
      StraightFlush
      FourOfAKind
      FullHouse
      Flush
      Straight
      ThreeOfAKind
      TwoPair
      Pair
      HighCard ]

let private deck =
    [| for suit in [ Spades; Hearts; Clubs; Diamonds ] do
           for rank in [ Two; Three; Four; Five; Six; Seven; Eight; Nine; Ten; Jack; Queen; King; Ace ] ->
               card rank suit |]

let private levelsAt (levelOf: HandType -> int) =
    handTypes |> List.map (fun t -> t, handLevel t (levelOf t)) |> Map.ofList

let private levelOne = levelsAt (fun _ -> 1)

let private score levels played held =
    Scoring.score { Played = played; Held = held; Levels = levels }

let private joker key edition value : Joker =
    { Key = key
      Name = key
      Edition = edition
      Debuffed = false
      Value = Some value
      SellValue = 0
      Text = "" }

// The keys of JokersCard.fs: per-card, retrigger and held jokers, and the ones with no effect
let private cardKeys =
    [ "j_greedy_joker"; "j_photograph"; "j_hanging_chad"; "j_baron"; "j_mime"; "j_lusty_joker"; "j_wrathful_joker"
      "j_gluttenous_joker"; "j_fibonacci"; "j_scary_face"; "j_smiley"; "j_even_steven"; "j_odd_todd"; "j_scholar"
      "j_walkie_talkie"; "j_arrowhead"; "j_onyx_agate"; "j_triboulet"; "j_ancient"; "j_idol"; "j_sock_and_buskin"
      "j_hack"; "j_selzer"; "j_dusk"; "j_raised_fist"; "j_shoot_the_moon"; "j_8_ball"; "j_business"; "j_ticket"
      "j_rough_gem"; "j_reserved_parking"; "j_bloodstone"; "j_hiker" ]

let private ancientText suit = $"Played cards with {suit} suit give X1.5 Mult when scored"
let private idolText (rank: Rank) suit = $"Each played {Rank.label rank} of {suit} gives X2 Mult when scored"

// A joker with the text its key reads: the Ancient Joker names Spades, the Idol the King of Spades
let private jokerOf key value =
    match key with
    | "j_ancient" -> { joker key None value with Text = ancientText "Spades" }
    | "j_idol" -> { joker key None value with Text = idolText King "Spades" }
    | "j_ice_cream" -> { joker key None value with Text = $"+{int value} Chips" }
    | "j_popcorn" -> { joker key None value with Text = $"+{int value} Mult" }
    | "j_ramen" -> { joker key None value with Text = $"X{value}" }
    | "j_loyalty_card" -> { joker key None value with Text = (if value > 0.0 then "Active!" else "3 remaining") }
    | _ -> joker key None value

// The keys stage 4c added: counters, factors, Midas Mask and the copies
let private stage4cKeys =
    [ "j_green_joker"; "j_ride_the_bus"; "j_runner"; "j_square"; "j_trousers"; "j_ceremonial"; "j_red_card"; "j_flash"
      "j_castle"; "j_wee"; "j_ice_cream"; "j_popcorn"; "j_space"; "j_dna"; "j_todo_list"; "j_sixth_sense"; "j_stencil"
      "j_steel_joker"; "j_throwback"; "j_constellation"; "j_madness"; "j_hologram"; "j_campfire"; "j_glass"; "j_lucky_cat"
      "j_hit_the_road"; "j_yorick"; "j_caino"; "j_obelisk"; "j_ramen"; "j_loyalty_card"; "j_midas_mask"; "j_blueprint"
      "j_brainstorm"; "j_baseball" ]

// A joker of a key for the random tests: a whole value, a factor that is mostly dyadic for the keys that multiply, and
// the text its key reads
let private randomJoker (random: Random) (hand: Card list) key edition : Joker =
    let pick (options: 'a list) = options[random.Next options.Length]
    let whole = joker key edition (float (random.Next(0, 41)))
    let target = if random.Next 2 = 0 then pick hand else pick (List.ofArray deck)
    let suit = if random.Next 2 = 0 then pick [ "Spades"; "Hearts"; "Clubs"; "Diamonds" ] else string target.Suit

    match key with
    | "j_ancient" -> { whole with Text = ancientText suit }
    | "j_idol" -> { whole with Text = idolText target.Rank suit }
    | "j_ice_cream" -> { whole with Text = $"+{random.Next(0, 101)} Chips" }
    | "j_popcorn" -> { whole with Text = $"+{random.Next(0, 21)} Mult" }
    | "j_ramen" -> { whole with Text = pick [ "X2"; "X2"; "X1.99"; "X1.5" ] }
    | "j_loyalty_card" -> { whole with Text = pick [ "Active!"; "3 remaining"; "0 remaining" ] }
    | "j_obelisk" -> { whole with Value = Some(pick [ 0.5; 1.0; 1.8; 2.8; 1.3 ]) }
    | _ when (fst netSets.Value).Contains key -> { whole with Value = Some(pick [ 1.0; 1.25; 1.5; 2.0; 2.5; 3.1; 4.0 ]) }
    | _ -> whole

let private situationOf hands discards (plays: (HandType * (int * int)) list) jokers : Situation =
    { Situation.empty with
        Jokers = jokers
        HandsLeft = hands
        DiscardsLeft = discards
        HandPlays = Map.ofList plays }

// The head of Search.plays; its Score sees the situation as the bot's Score does
let private bestWith (situation: Situation) (hand: Card list) levels =
    let input: Search.Input =
        { Hand = List.indexed hand
          Deck = []
          HandsLeft = situation.HandsLeft + 1
          HandSize = 8
          DiscardsLeft = situation.DiscardsLeft
          Needed = 0.0
          Score =
            fun handsLeft discardsLeft played held ->
                Scoring.scoreWith
                    { situation with HandsLeft = handsLeft; DiscardsLeft = discardsLeft }
                    { Played = played; Held = held; Levels = levels }
          Seed = 1
          RedrawSamples = 1
          Rollouts = 1 }

    let indices, _, breakdown = Search.plays input |> List.head
    indices, breakdown.Total, breakdown.HandType

let private best hand levels = bestWith Situation.empty hand levels

let private chosenWith situation hand levels =
    PlayNet.choose net.Value situation hand levels |> Option.get

let private chosen hand levels = chosenWith Situation.empty hand levels

let private disagreements (cases: (Card list * Map<HandType, HandLevel>)[]) =
    cases
    |> Array.Parallel.map (fun (hand, levels) ->
        hand, levels, PlayNet.choose net.Value Situation.empty hand levels, best hand levels)
    |> Array.filter (fun (_, _, got, expected) -> got <> Some expected)

let private enhancements =
    None
    :: [ for e in [ Enhancement.Bonus; Enhancement.Mult; Enhancement.Wild; Enhancement.Glass
                    Enhancement.Steel; Enhancement.Stone; Enhancement.Gold; Enhancement.Lucky ] -> Some e ]

let private editions = None :: [ for e in [ Foil; Holo; Polychrome; Negative ] -> Some e ]

let private seals =
    None :: [ for e in [ Seal.Red; Seal.Blue; Seal.Gold; Seal.Purple ] -> Some e ]

// The limits and joker classes the net was built with
let private netLimits =
    lazy
        (use doc = JsonDocument.Parse(File.ReadAllText(AppContext.BaseDirectory + "nn/playnet.json"))
         let root = doc.RootElement
         let keys (name: string) = [ for k in root.GetProperty(name).EnumerateArray() -> k.GetString() ] |> Set.ofList
         let limits = [ for p in root.GetProperty("limits").EnumerateObject() -> p.Name, p.Value.GetDouble() ]
         Map.ofList limits, keys "main", keys "addsMult", keys "addsChips")

// The joker whose logic runs at each slot: follow Blueprint and Brainstorm to a joker that is not a copy; a loop, the
// last slot or a missing target gives nothing
let private actorsOf (jokers: Joker list) =
    let slots = Array.ofList jokers

    let rec follow visited i =
        if List.contains i visited then
            None
        else
            match slots[i].Key with
            | "j_blueprint" -> if i + 1 < slots.Length then follow (i :: visited) (i + 1) else None
            | "j_brainstorm" -> follow (i :: visited) 0
            | _ -> Some slots[i]

    [ for i in 0 .. slots.Length - 1 -> slots[i], follow [] i ]

// The factor a joker multiplies mult by (1 for none); None when its key does not multiply by an input
let private factorFor (j: Joker) : float option =
    let times factor = if factor > 1.0 then factor else 1.0
    let value = Option.defaultValue 0.0 j.Value

    match j.Key with
    | "j_obelisk" -> Some(times (value + 0.2))
    | "j_ramen" ->
        let m = Text.RegularExpressions.Regex.Match(j.Text, @"^X(\d+(?:\.\d+)?)")
        let shown = if m.Success then Double.Parse(m.Groups[1].Value, Globalization.CultureInfo.InvariantCulture) else 0.0
        let discarded = int (Math.Round((2.0 - shown) / 0.01))
        Some(times (List.fold (fun factor _ -> factor - 0.01) 2.0 [ 1..discarded ]))
    | key when (fst netSets.Value).Contains key -> Some(times value)
    | _ -> None

let private suitNamed (name: string) =
    [ Spades; Hearts; Clubs; Diamonds ] |> List.tryFind (fun s -> (string s).TrimEnd 's' = name.TrimEnd 's')

let private ranks = [ Two; Three; Four; Five; Six; Seven; Eight; Nine; Ten; Jack; Queen; King; Ace ]

let private ancientSuit (j: Joker) =
    let m = Text.RegularExpressions.Regex.Match(j.Text, @"with (\w+) suit")
    if m.Success then suitNamed m.Groups[1].Value else None

let private idolTarget (j: Joker) =
    let m = Text.RegularExpressions.Regex.Match(j.Text, @"played (\w+) of (\w+) gives")

    if m.Success then
        match ranks |> List.tryFind (fun r -> Rank.label r = m.Groups[1].Value), suitNamed m.Groups[2].Value with
        | Some r, Some s -> Some(r, s)
        | _ -> None
    else
        None

// Whether the net's precision limits are broken, written again from 48-design: every count is an upper bound over every
// play of the hand. Five cards at most are played; Hanging Chad and Photograph act on one card; the mult is the level
// mult plus every add, times every doubling and factor 1.5 of the cards, then the main-phase jokers slot by slot. Every
// joker count is over the jokers whose logic runs; a factor must be at most FMAX with 4 times it a whole number
let private breaksLimits (hand: Card list) (situation: Situation) (levels: Map<HandType, HandLevel>) =
    let limits, mains, multAdders, chipAdders = netLimits.Value
    let limit (name: string) = limits[name]
    let slots = actorsOf situation.Jokers
    let jokers = slots |> List.choose snd
    let number key = jokers |> List.filter (fun j -> j.Key = key) |> List.length
    let bit b = if b then 1 else 0
    let sum5 (xs: int list) = (List.sortDescending xs |> List.truncate 5) |> List.sum
    let chad, photo = number "j_hanging_chad", number "j_photograph"
    let nAdders = jokers |> List.filter (fun j -> multAdders.Contains j.Key) |> List.length
    let nChippers = jokers |> List.filter (fun j -> chipAdders.Contains j.Key) |> List.length
    let nPoly = situation.Jokers |> List.filter (fun j -> j.Edition = Some Polychrome) |> List.length
    let uncommon = situation.Jokers |> List.filter (fun j -> (snd netSets.Value).Contains j.Key) |> List.length
    let heldKey key = situation.Jokers |> List.exists (fun j -> j.Key = key)
    let pareidolia, smeared, midas = heldKey "j_pareidolia", heldKey "j_smeared", heldKey "j_midas_mask"
    let colour suit = suit = Hearts || suit = Diamonds
    let factors = situation.Jokers |> List.choose factorFor
    let factorsRun = jokers |> List.choose factorFor

    let factorBroken =
        factors |> List.exists (fun f -> f > limit "factor" || Math.Floor(4.0 * f) <> 4.0 * f)

    let factorBits (f: float) =
        match int (4.0 * f) % 4 with
        | 0 -> 0
        | 2 -> 1
        | _ -> 2

    let perCard (c: Card) =
        let stone = c.Enhancement = Some Enhancement.Stone
        // a played Stone has a rank and suit only when Pareidolia makes it a face card and a Midas Mask turns it Gold
        let ranked = not stone || (pareidolia && midas)

        let matches suit =
            ranked && (c.Enhancement = Some Enhancement.Wild || c.Suit = suit || (smeared && colour c.Suit = colour suit))

        let face = pareidolia || (not stone && List.contains c.Rank [ Jack; Queen; King ])
        let red = bit (c.Seal = Some Seal.Red)

        let passes =
            1 + red + number "j_sock_and_buskin" * bit face
            + number "j_hack" * bit (ranked && List.contains c.Rank [ Two; Three; Four; Five ])
            + number "j_selzer" + (if situation.HandsLeft = 0 then number "j_dusk" else 0)

        let doubles =
            bit (c.Enhancement = Some Enhancement.Glass)
            + number "j_triboulet" * bit (ranked && List.contains c.Rank [ King; Queen ])
            + (jokers |> List.sumBy (fun j -> match idolTarget j with
                                              | Some(r, s) when j.Key = "j_idol" && ranked && c.Rank = r && matches s -> 1
                                              | _ -> 0))

        let halves =
            bit (c.Edition = Some Polychrome)
            + (jokers |> List.sumBy (fun j -> match ancientSuit j with
                                              | Some s when j.Key = "j_ancient" && matches s -> 1
                                              | _ -> 0))

        let heldPasses = 1 + red + number "j_mime"
        let heldHalves = bit (c.Enhancement = Some Enhancement.Steel) + number "j_baron" * bit (not stone && c.Rank = King)
        let heldAdd = 13 * number "j_shoot_the_moon" * bit (not stone && c.Rank = Queen) + 22 * number "j_raised_fist" * bit (not stone)
        let own = 4 * bit (c.Enhancement = Some Enhancement.Mult) + 10 * bit (c.Edition = Some Holo)
        passes, doubles, halves, heldPasses, heldHalves, heldAdd, bit face, own

    let rows = hand |> List.map perCard
    let col pick = rows |> List.map pick
    let passes = col (fun (p, _, _, _, _, _, _, _) -> p)
    let doubles = col (fun (_, d, _, _, _, _, _, _) -> d)
    let halves = col (fun (_, _, h, _, _, _, _, _) -> h)
    let heldPasses = col (fun (_, _, _, p, _, _, _, _) -> p)
    let heldHalves = col (fun (_, _, _, _, h, _, _, _) -> h)
    let heldAdd = col (fun (_, _, _, _, _, a, _, _) -> a)
    let faces = col (fun (_, _, _, _, _, _, f, _) -> f)
    let owns = col (fun (_, _, _, _, _, _, _, o) -> o)
    let product = List.map2 (*)

    let perCardBroken =
        (passes |> List.exists (fun p -> float (p + 2 * chad) > limit "passes"))
        || List.exists2 (fun d f -> float (d + photo * f) > limit "doubles") doubles faces
        || (halves |> List.exists (fun h -> float h > limit "halves"))
        || (heldHalves |> List.exists (fun h -> float h > limit "heldHalves"))
        || (heldPasses |> List.exists (fun p -> float p > limit "heldPasses"))

    let doublings =
        sum5 (product passes doubles) + 2 * chad * List.max doubles + photo * List.max passes + 2 * chad * photo

    let totalPasses = sum5 passes + 2 * chad
    let cardFactors = sum5 (product passes halves) + 2 * chad * List.max halves + List.sum (product heldPasses heldHalves)
    let baseballs = number "j_baseball"
    let weight = cardFactors + nPoly + (factorsRun |> List.sumBy factorBits) + uncommon * baseballs
    let perPass = owns |> List.map (fun o -> o + 8 * nAdders)
    let adds = sum5 (product passes perPass) + 2 * chad * List.max perPass + List.sum (product heldPasses heldAdd)
    let topLevel f = levels |> Map.toList |> List.map (snd >> f) |> List.max
    let afterCards = (topLevel (fun l -> l.Mult) + float adds) * 2.0 ** float doublings * 1.5 ** float cardFactors

    let mutable mult = afterCards

    for slot, actor in slots do
        let main = actor |> Option.exists (fun a -> mains.Contains a.Key)
        let own = actor |> Option.bind factorFor |> Option.defaultValue (if main then 4.0 else 1.0)
        let baseball = if (snd netSets.Value).Contains slot.Key then 1.5 ** float baseballs else 1.0
        let poly = if slot.Edition = Some Polychrome then 1.5 else 1.0
        mult <- (mult + (if main then limit "value" + 12.0 else 10.0)) * own * baseball * poly

    let chips =
        topLevel (fun l -> l.Chips)
        + float totalPasses * float (100 + 50 * nChippers)
        + (slots |> List.sumBy (fun (_, actor) ->
            50.0 + (if actor |> Option.exists (fun a -> mains.Contains a.Key) then limit "value" + 15.0 else 0.0)))

    perCardBroken
    || factorBroken
    || float weight > limit "weight"
    || mult > limit "multCap"
    || chips > limit "chipsCap"
    || mult * chips > limit "productCap"

let private modified enhancement edition seal (c: Card) =
    { c with Enhancement = enhancement; Edition = edition; Seal = seal }

let private withEdition edition (c: Card) = { c with Edition = Some edition }

let private withSeal seal (c: Card) = { c with Seal = Some seal }

[<Fact>]
let ``agrees with the search on 300 random plain hands`` () =
    let random = Random(2026)

    let cases =
        Array.init 300 (fun _ ->
            let size = random.Next(1, 9)
            let shuffled = Array.copy deck
            random.Shuffle shuffled
            let levels = levelsAt (fun _ -> random.Next(1, 31))
            List.ofArray shuffled[.. size - 1], levels)

    let failures = disagreements cases

    Assert.True(failures.Length = 0, $"{failures.Length} of {cases.Length} hands disagree; first: %A{Array.truncate 5 failures}")

type ModifierAgreement(output: Xunit.Abstractions.ITestOutputHelper) =
    [<Fact>]
    member _.``agrees with the search on 500 random modified hands`` () =
        let random = Random(2027)
        let pick (options: 'a list) = options[random.Next options.Length]

        let cases =
            Array.init 500 (fun _ ->
                let size = random.Next(1, 9)
                let shuffled = Array.copy deck
                random.Shuffle shuffled
                let hand = [ for c in shuffled[.. size - 1] -> modified (pick enhancements) (pick editions) (pick seals) c ]
                hand, levelsAt (fun _ -> random.Next(1, 31)))

        let results =
            cases
            |> Array.Parallel.map (fun (hand, levels) ->
                hand, levels, PlayNet.choose net.Value Situation.empty hand levels, best hand levels)

        let rejected = results |> Array.filter (fun (_, _, got, _) -> got.IsNone)
        let checkedHands = results |> Array.filter (fun (_, _, got, _) -> got.IsSome)
        let loose = rejected |> Array.filter (fun (hand, levels, _, _) -> not (breaksLimits hand Situation.empty levels))
        let failures = checkedHands |> Array.filter (fun (_, _, got, expected) -> got <> Some expected)

        output.WriteLine $"checked {checkedHands.Length}, rejected {rejected.Length}"

        Assert.True(loose.Length = 0, $"{loose.Length} hands rejected inside the limits; first: %A{Array.truncate 5 loose}")
        Assert.True(checkedHands.Length >= 450, $"only {checkedHands.Length} hands checked")
        Assert.True(failures.Length = 0, $"{failures.Length} of {checkedHands.Length} hands disagree; first: %A{Array.truncate 5 failures}")

type JokerAgreement(output: Xunit.Abstractions.ITestOutputHelper) =
    [<Fact>]
    member _.``agrees with the search on 500 random hands with main-phase jokers`` () =
        let random = Random(2028)
        let pick (options: 'a list) = options[random.Next options.Length]
        let keys = jokerKeys.Value |> List.map fst |> List.filter (fun k -> not (List.contains k cardKeys))

        let cases =
            Array.init 500 (fun _ ->
                let size = random.Next(1, 9)
                let shuffled = Array.copy deck
                random.Shuffle shuffled
                let hand = [ for c in shuffled[.. size - 1] -> modified (pick enhancements) (pick editions) (pick seals) c ]

                let jokers =
                    [ for _ in 1 .. random.Next(0, 6) -> randomJoker random hand (pick keys) (pick editions) ]

                let plays =
                    [ for t in handTypes ->
                          let run = random.Next(0, 6)
                          t, (run, random.Next(0, run + 1)) ]

                let situation = situationOf (random.Next(0, 5)) (random.Next(0, 5)) plays jokers
                hand, situation, levelsAt (fun _ -> random.Next(1, 31)))

        let results =
            cases
            |> Array.Parallel.map (fun (hand, situation, levels) ->
                hand, situation, levels, PlayNet.choose net.Value situation hand levels, bestWith situation hand levels)

        let rejected = results |> Array.filter (fun (_, _, _, got, _) -> got.IsNone)
        let checkedCases = results |> Array.filter (fun (_, _, _, got, _) -> got.IsSome)

        let loose =
            rejected |> Array.filter (fun (hand, situation, levels, _, _) -> not (breaksLimits hand situation levels))

        let failures = checkedCases |> Array.filter (fun (_, _, _, got, expected) -> got <> Some expected)

        output.WriteLine $"checked {checkedCases.Length}, rejected {rejected.Length}"

        Assert.True(loose.Length = 0, $"{loose.Length} cases rejected inside the limits; first: %A{Array.truncate 5 loose}")
        Assert.True(checkedCases.Length >= 450, $"only {checkedCases.Length} cases checked")
        Assert.True(failures.Length = 0, $"{failures.Length} of {checkedCases.Length} cases disagree; first: %A{Array.truncate 5 failures}")

type PerCardAgreement(output: Xunit.Abstractions.ITestOutputHelper) =
    [<Fact>]
    member _.``agrees with the search on 500 random hands with per-card jokers`` () =
        let random = Random(2030)
        let pick (options: 'a list) = options[random.Next options.Length]
        let main = jokerKeys.Value |> List.map fst |> List.filter (fun k -> not (List.contains k cardKeys))
        // JokersCard keys weigh 3, the main-phase keys 1
        let keys = List.collect (fun k -> [ k; k; k ]) cardKeys @ main

        let cases =
            Array.init 500 (fun _ ->
                let size = random.Next(1, 9)
                let shuffled = Array.copy deck
                random.Shuffle shuffled
                let hand = [ for c in shuffled[.. size - 1] -> modified (pick enhancements) (pick editions) (pick seals) c ]

                // the text of an Ancient Joker or an Idol names a random target, half the time a card of the hand
                let jokers =
                    [ for _ in 1 .. random.Next(0, 6) -> randomJoker random hand (pick keys) (pick editions) ]

                let plays =
                    [ for t in handTypes ->
                          let run = random.Next(0, 6)
                          t, (run, random.Next(0, run + 1)) ]

                let situation = situationOf (random.Next(0, 5)) (random.Next(0, 5)) plays jokers
                hand, situation, levelsAt (fun _ -> random.Next(1, 31)))

        let results =
            cases
            |> Array.Parallel.map (fun (hand, situation, levels) ->
                hand, situation, levels, PlayNet.choose net.Value situation hand levels, bestWith situation hand levels)

        let rejected = results |> Array.filter (fun (_, _, _, got, _) -> got.IsNone)
        let checkedCases = results |> Array.filter (fun (_, _, _, got, _) -> got.IsSome)

        let loose =
            rejected |> Array.filter (fun (hand, situation, levels, _, _) -> not (breaksLimits hand situation levels))

        let failures = checkedCases |> Array.filter (fun (_, _, _, got, expected) -> got <> Some expected)

        // the true mult, chips and product of every accepted case stay inside the caps the net states
        let limits, _, _, _ = netLimits.Value

        let beyond =
            checkedCases
            |> Array.filter (fun (hand, situation, levels, got, _) ->
                let positions, _, _ = got.Value
                let played = positions |> List.map (fun i -> hand[i])
                let held = hand |> List.indexed |> List.filter (fun (i, _) -> not (List.contains i positions)) |> List.map snd
                let b = Scoring.scoreWith situation { Played = played; Held = held; Levels = levels }
                b.Mult > limits["multCap"] || b.Chips > limits["chipsCap"] || b.Mult * b.Chips > limits["productCap"])

        output.WriteLine $"checked {checkedCases.Length}, rejected {rejected.Length}"

        Assert.True(beyond.Length = 0, $"{beyond.Length} accepted cases reach beyond the caps")
        Assert.True(loose.Length = 0, $"{loose.Length} cases rejected inside the limits; first: %A{Array.truncate 5 loose}")
        Assert.True(checkedCases.Length >= 330, $"only {checkedCases.Length} cases checked")
        Assert.True(failures.Length = 0, $"{failures.Length} of {checkedCases.Length} cases disagree; first: %A{Array.truncate 5 failures}")

// The net's choice must equal the search's head and Scoring.scoreWith of that play
let private agreeWith (situation: Situation) (hand: Card list) levels =
    let positions, total, got = chosenWith situation hand levels
    let played = positions |> List.map (fun i -> hand[i])
    let held = hand |> List.indexed |> List.filter (fun (i, _) -> not (List.contains i positions)) |> List.map snd

    let expected =
        Scoring.scoreWith situation { Played = played; Held = held; Levels = levels }

    Assert.Equal(expected.Total, total)
    Assert.Equal(expected.HandType, got)
    Assert.Equal(bestWith situation hand levels, (positions, total, got))
    positions, total, got

let private agree hand levels = agreeWith Situation.empty hand levels

[<Fact>]
let ``folds mult in played order`` () =
    let multFirst = [ withEnhancement Enhancement.Mult (card King Spades); withEnhancement Enhancement.Glass (card King Hearts) ]
    let firstPositions, firstTotal, _ = agree multFirst levelOne
    let _, secondTotal, _ = agree (List.rev multFirst) levelOne

    Assert.Equal<int list>([ 0; 1 ], firstPositions)
    Assert.Equal(360.0, firstTotal)
    Assert.Equal(240.0, secondTotal)

[<Fact>]
let ``repeats a Polychrome card with a Red Seal`` () =
    let polychrome = withEdition Polychrome (card Ace Spades)
    let hand seal = [ (if seal then withSeal Seal.Red polychrome else polychrome); card Ace Hearts ]
    let _, plain, _ = agree (hand false) levelOne
    let _, repeated, _ = agree (hand true) levelOne

    Assert.True(repeated > plain)

[<Fact>]
let ``multiplies by held Steel once, or twice with a Red Seal`` () =
    let hand steel = [ card Ace Spades; steel (card King Hearts) ]
    let steel = withEnhancement Enhancement.Steel
    let positions, once, _ = agree (hand steel) levelOne
    let _, twice, _ = agree (hand (steel >> withSeal Seal.Red)) levelOne

    Assert.Equal<int list>([ 0 ], positions)
    Assert.Equal(24.0, once)
    Assert.Equal(36.0, twice)

[<Fact>]
let ``scores a Stone with no rank or suit`` () =
    let stone = withEnhancement Enhancement.Stone
    let alone, _, _ = agree [ stone (card Ace Spades) ] levelOne
    let _, _, withKicker = agree [ card King Spades; card King Hearts; card King Clubs; card King Diamonds; stone (card Three Spades) ] levelOne
    let _, _, highCard = agree [ stone (card Two Spades); card Seven Clubs ] levelOne

    Assert.Equal<int list>([ 0 ], alone)
    Assert.Equal(FourOfAKind, withKicker)
    Assert.Equal(HighCard, highCard)

[<Fact>]
let ``counts a Wild card in a Flush`` () =
    let hand = [ card Two Hearts; card Five Hearts; card Nine Hearts; card King Hearts; withEnhancement Enhancement.Wild (card Seven Spades) ]
    let positions, _, handType = agree hand levelOne

    Assert.Equal(Flush, handType)
    Assert.Equal<int list>([ 0; 1; 2; 3; 4 ], positions)

[<Fact>]
let ``agrees at the top levels and the precision limits`` () =
    let top = handTypes |> List.map (fun t -> t, { Chips = 5000.0; Mult = 200.0 }) |> Map.ofList
    let glassPolychromeRed = card King Spades |> modified (Some Enhancement.Glass) (Some Polychrome) (Some Seal.Red)
    let rest = [ card Queen Spades; card Queen Hearts; card Seven Clubs; card Four Diamonds ]

    let glassHand =
        [ glassPolychromeRed
          withSeal Seal.Red (withEnhancement Enhancement.Glass (withEdition Polychrome (card King Hearts)))
          withSeal Seal.Red (withEnhancement Enhancement.Glass (withEdition Polychrome (card King Clubs)))
          withSeal Seal.Red (withEdition Polychrome (card Two Spades)) ]
        @ rest

    let steelHand =
        [ for c in [ card King Spades; card King Hearts; card King Clubs ] -> withSeal Seal.Red (withEnhancement Enhancement.Glass c) ]
        @ [ for c in [ card Queen Spades; card Queen Hearts; card Seven Clubs; card Four Diamonds ] ->
                withSeal Seal.Red (withEnhancement Enhancement.Steel c) ]
        @ [ card Two Spades ]

    for hand in [ glassHand; steelHand ] do
        Assert.False(breaksLimits hand Situation.empty top)
        agree hand top |> ignore

[<Fact>]
let ``reaches every hand type`` () =
    let cases =
        [ FlushFive, List.replicate 5 (card Ace Spades)
          FlushHouse, List.replicate 3 (card Seven Diamonds) @ List.replicate 2 (card Four Diamonds)
          FiveOfAKind, [ card Ace Spades; card Ace Hearts; card Ace Hearts; card Ace Clubs; card Ace Diamonds ]
          StraightFlush, [ card Queen Spades; card Jack Spades; card Ten Spades; card Nine Spades; card Eight Spades ]
          FourOfAKind, [ card King Spades; card King Hearts; card King Clubs; card King Diamonds; card Three Spades ]
          FullHouse, [ card Nine Spades; card Nine Hearts; card Nine Clubs; card Four Diamonds; card Four Spades ]
          Flush, [ card Two Hearts; card Five Hearts; card Nine Hearts; card King Hearts; card Seven Hearts ]
          Straight, [ card Ace Spades; card Two Hearts; card Three Clubs; card Four Diamonds; card Five Spades ]
          ThreeOfAKind, [ card Eight Spades; card Eight Hearts; card Eight Clubs; card King Diamonds; card Two Spades ]
          TwoPair, [ card Nine Spades; card Nine Hearts; card Four Clubs; card Four Diamonds ]
          Pair, [ card Five Clubs; card Five Diamonds ]
          HighCard, [ card Seven Clubs ] ]

    for handType, hand in cases do
        let positions, total, got = chosen hand levelOne
        let played = positions |> List.map (fun i -> hand[i])
        let held = hand |> List.indexed |> List.filter (fun (i, _) -> not (List.contains i positions)) |> List.map snd
        let expected = score levelOne played held

        Assert.Equal(handType, got)
        Assert.Equal(expected.HandType, got)
        Assert.Equal(expected.Total, total)
        Assert.Equal(best hand levelOne, (positions, total, got))

[<Fact>]
let ``agrees with the search on 300 straight windows`` () =
    let random = Random(2029)
    let suits = [| Spades; Hearts; Clubs; Diamonds |]
    let ranks = [| Ace; Two; Three; Four; Five; Six; Seven; Eight; Nine; Ten; Jack; Queen; King; Ace |]

    let cases =
        Array.init 300 (fun _ ->
            let start = random.Next 10
            let sameSuit = random.Next 3 = 0
            let suit = suits[random.Next 4]
            let run = [ for r in ranks[start .. start + 4] -> card r (if sameSuit then suit else suits[random.Next 4]) ]
            let others = deck |> Array.filter (fun c -> not (List.contains c run))
            random.Shuffle others
            let hand = Array.ofList (run @ List.ofArray others[.. random.Next 4 - 1])
            random.Shuffle hand
            start, List.ofArray hand, levelsAt (fun _ -> random.Next(1, 31)))

    let results =
        cases
        |> Array.Parallel.map (fun (start, hand, levels) ->
            start, hand, levels, PlayNet.choose net.Value Situation.empty hand levels, best hand levels)

    let failures = results |> Array.filter (fun (_, _, _, got, expected) -> got <> Some expected)
    let typeOf (_, _, _, _, (_, _, handType)) = handType

    Assert.True(failures.Length = 0, $"{failures.Length} of {results.Length} hands disagree; first: %A{Array.truncate 5 failures}")
    Assert.True(results |> Array.exists (fun r -> typeOf r = Straight), "no Straight reached")
    Assert.True(results |> Array.exists (fun r -> typeOf r = StraightFlush), "no Straight Flush reached")

    Assert.True(
        results |> Array.exists (fun ((start, _, _, _, _) as r) -> start = 0 && (typeOf r = Straight || typeOf r = StraightFlush)),
        "no Ace-low straight reached"
    )

[<Fact>]
let ``breaks ties like the search`` () =
    let hand =
        [ card Queen Hearts
          card King Spades
          card Two Clubs
          card Four Diamonds
          card Six Spades
          card Nine Hearts
          card Three Clubs
          card Eight Diamonds ]

    let positions, _, _ = chosen hand levelOne
    let searched, _, _ = best hand levelOne

    Assert.Equal<int list>([ 0 ], positions)
    Assert.Equal<int list>(searched, positions)

[<Fact>]
let ``declines hands outside its scope`` () =
    let plain = [ card Ace Spades; card King Hearts ]
    let pairLevel = levelOne[Pair]
    let steelRed = card Ace Spades |> modified (Some Enhancement.Steel) None (Some Seal.Red)
    let declined hand levels = PlayNet.choose net.Value Situation.empty hand levels

    Assert.Equal(None, declined [ { card Ace Spades with Debuffed = true } ] levelOne)
    Assert.Equal(None, declined (List.init 9 (fun i -> deck[i])) levelOne)
    Assert.Equal(None, declined [] levelOne)
    Assert.Equal(None, declined plain (Map.add Pair { pairLevel with Mult = 201.0 } levelOne))
    Assert.Equal(None, declined plain (Map.remove Pair levelOne))
    Assert.Equal(None, declined (List.replicate 6 steelRed) levelOne)

let private single = [ card King Spades ]

// A hand, its levels and how to build the situation around a joker list
type private Scene =
    { Cards: Card list
      Rates: Map<HandType, HandLevel>
      Make: Joker list -> Situation }

let private scene cards =
    { Cards = cards
      Rates = levelOne
      Make = situationOf 2 2 [] }

let private withLeft hands discards scene =
    { scene with Make = situationOf hands discards [] }

let private withPlays plays scene =
    { scene with Make = situationOf 2 2 plays }

let private favouring handType scene =
    { scene with Rates = levelsAt (fun t -> if t = handType then 30 else 1) }

// The jokers around the one under test: Baseball Card needs an Uncommon joker (Mime, with nothing held) in one scene and
// none in the other, Blueprint a Cavendish to its right where it fires and to its left where it does not, Brainstorm
// the opposite
let private lineup key quiet (j: Joker) =
    let cavendish = joker "j_cavendish" None 0.0

    match key, quiet with
    | "j_baseball", false -> [ j; joker "j_mime" None 0.0 ]
    | "j_baseball", true -> [ j; cavendish ]
    | "j_blueprint", false -> [ j; cavendish ]
    | "j_blueprint", true -> [ cavendish; j ]
    | "j_brainstorm", false -> [ cavendish; j ]
    | "j_brainstorm", true -> [ j ]
    | "j_pareidolia", false -> [ j; joker "j_smiley" None 0.0 ]
    | "j_pareidolia", true -> [ j; cavendish ]
    | _ -> [ j ]

// What the net chooses for one joker in a scene, and what the same play scores with the joker taken out
let private effectWith (scene: Scene) key value quiet =
    let jokers = lineup key quiet (jokerOf key value)
    let situation = scene.Make jokers
    let positions, total, _ = agreeWith situation scene.Cards scene.Rates
    let played = positions |> List.map (fun i -> scene.Cards[i])
    let held = scene.Cards |> List.indexed |> List.filter (fun (i, _) -> not (List.contains i positions)) |> List.map snd
    let others = jokers |> List.filter (fun j -> j.Key <> key)
    let without = Scoring.scoreWith { situation with Jokers = others } { Played = played; Held = held; Levels = scene.Rates }
    total, without.Total

let private effectOf (scene: Scene) key value = effectWith scene key value false

let private pairs = scene [ card King Spades; card King Hearts; card Four Diamonds ]
let private noPair = scene [ card King Spades; card Seven Hearts ]
let private threes = scene [ card Eight Spades; card Eight Hearts; card Eight Clubs ]
let private noThree = scene [ card Eight Spades; card Eight Hearts ]
let private twoPairs = scene [ card Nine Spades; card Nine Hearts; card Four Clubs; card Four Diamonds ]
let private noTwoPair = scene [ card Nine Spades; card Nine Hearts; card Four Clubs ]
let private straights = scene [ card Five Spades; card Six Hearts; card Seven Clubs; card Eight Diamonds; card Nine Spades ]
let private noStraight = scene [ card Five Spades; card Six Hearts; card Seven Clubs; card Eight Diamonds ]
let private flushes = scene [ card Two Hearts; card Five Hearts; card Nine Hearts; card King Hearts; card Seven Hearts ]
let private noFlush = scene [ card Two Hearts; card Five Hearts; card Nine Hearts; card King Hearts; card Seven Spades ]
let private fours = scene [ card King Spades; card King Hearts; card King Clubs; card King Diamonds ]
let private noFour = scene [ card King Spades; card King Hearts; card King Clubs ]

// Five spades to play and three red cards to hold: some held card is never black
let private redHeld =
    scene [ card Two Spades; card Five Spades; card Nine Spades; card King Spades; card Seven Spades; card Three Hearts; card Four Diamonds; card Six Hearts ]
    |> favouring Flush

// Four of a flush and one spade: a Flush only with Four Fingers
let private fourFlush = favouring Flush noFlush
// 5-6-8-9-10, a gap at the 7: a Straight only with Shortcut
let private gapStraight =
    scene [ card Five Spades; card Six Hearts; card Eight Clubs; card Nine Diamonds; card Ten Spades ] |> favouring Straight
// Hearts and Diamonds, the colours of one suit under Smeared
let private redFlush =
    scene [ card Two Hearts; card Five Diamonds; card Nine Hearts; card King Diamonds; card Seven Hearts ] |> favouring Flush

let private scenes = scene single
let private oneCard rank suit = scene [ card rank suit ]

// Two aces to play, the rest held: the best play leaves the held cards where they are
let private aces held = scene ([ card Ace Spades; card Ace Hearts ] @ held) |> favouring Pair

let private heldKing = aces [ card King Clubs ]
let private heldQueen = aces [ card Queen Clubs ]
let private heldThree = aces [ card Three Clubs ]
let private heldSteel = aces [ withEnhancement Enhancement.Steel (card King Clubs) ]
let private nothingHeld = aces []

// A scene where the joker's effect fires and, for a conditional joker, one where it does not; each with a value
let private jokerCases: (string * (float * Scene) * (float * Scene) option)[] =
    [| "j_joker", (0.0, scenes), None
       "j_gros_michel", (0.0, scenes), None
       "j_cavendish", (0.0, scenes), None
       "j_stuntman", (0.0, scenes), None
       "j_sly", (0.0, pairs), Some(0.0, noPair)
       "j_jolly", (0.0, pairs), Some(0.0, noPair)
       "j_duo", (0.0, pairs), Some(0.0, noPair)
       "j_wily", (0.0, threes), Some(0.0, noThree)
       "j_zany", (0.0, threes), Some(0.0, noThree)
       "j_trio", (0.0, threes), Some(0.0, noThree)
       "j_mad", (0.0, twoPairs), Some(0.0, noTwoPair)
       "j_clever", (0.0, twoPairs), Some(0.0, noTwoPair)
       "j_crazy", (0.0, straights), Some(0.0, noStraight)
       "j_devious", (0.0, straights), Some(0.0, noStraight)
       "j_order", (0.0, straights), Some(0.0, noStraight)
       "j_droll", (0.0, flushes), Some(0.0, noFlush)
       "j_crafty", (0.0, flushes), Some(0.0, noFlush)
       "j_tribe", (0.0, flushes), Some(0.0, noFlush)
       "j_family", (0.0, fours), Some(0.0, noFour)
       "j_half", (0.0, scenes), Some(0.0, favouring Flush flushes)
       "j_blackboard", (0.0, scene [ card Ace Spades; card King Clubs ]), Some(0.0, redHeld)
       "j_flower_pot", (0.0, straights), Some(0.0, scene [ card Five Spades; card Six Hearts; card Seven Spades; card Eight Diamonds; card Nine Spades ])
       "j_seeing_double", (0.0, straights), Some(0.0, scene [ card Five Spades; card Six Hearts; card Seven Diamonds; card Eight Diamonds; card Nine Spades ])
       "j_banner", (0.0, withLeft 2 3 scenes), Some(0.0, withLeft 2 0 scenes)
       "j_mystic_summit", (0.0, withLeft 2 0 scenes), Some(0.0, withLeft 2 2 scenes)
       "j_acrobat", (0.0, withLeft 0 2 scenes), Some(0.0, withLeft 2 2 scenes)
       "j_supernova", (0.0, withPlays [ HighCard, (5, 1) ] scenes), None
       "j_card_sharp", (0.0, withPlays [ HighCard, (1, 1) ] scenes), Some(0.0, scenes)
       "j_abstract", (7.0, scenes), Some(0.0, scenes)
       "j_erosion", (7.0, scenes), Some(0.0, scenes)
       "j_fortune_teller", (7.0, scenes), Some(0.0, scenes)
       "j_swashbuckler", (7.0, scenes), Some(0.0, scenes)
       "j_bootstraps", (7.0, scenes), Some(0.0, scenes)
       "j_blue_joker", (7.0, scenes), Some(0.0, scenes)
       "j_stone", (7.0, scenes), Some(0.0, scenes)
       "j_bull", (7.0, scenes), Some(0.0, scenes)
       "j_drivers_license", (16.0, scenes), Some(15.0, scenes)
       "j_greedy_joker", (0.0, oneCard Six Diamonds), Some(0.0, oneCard Six Spades)
       "j_lusty_joker", (0.0, oneCard Six Hearts), Some(0.0, oneCard Six Spades)
       "j_wrathful_joker", (0.0, oneCard Six Spades), Some(0.0, oneCard Six Hearts)
       "j_gluttenous_joker", (0.0, oneCard Six Clubs), Some(0.0, oneCard Six Hearts)
       "j_onyx_agate", (0.0, oneCard Six Clubs), Some(0.0, oneCard Six Hearts)
       "j_arrowhead", (0.0, oneCard Six Spades), Some(0.0, oneCard Six Hearts)
       "j_fibonacci", (0.0, oneCard Ace Spades), Some(0.0, oneCard Six Spades)
       "j_scary_face", (0.0, oneCard King Spades), Some(0.0, oneCard Seven Spades)
       "j_smiley", (0.0, oneCard King Spades), Some(0.0, oneCard Seven Spades)
       "j_even_steven", (0.0, oneCard Four Spades), Some(0.0, oneCard Five Spades)
       "j_odd_todd", (0.0, oneCard Five Spades), Some(0.0, oneCard Four Spades)
       "j_scholar", (0.0, oneCard Ace Spades), Some(0.0, oneCard Seven Spades)
       "j_walkie_talkie", (0.0, oneCard Ten Spades), Some(0.0, oneCard Seven Spades)
       "j_triboulet", (0.0, oneCard King Spades), Some(0.0, oneCard Seven Spades)
       "j_photograph", (0.0, oneCard King Spades), Some(0.0, oneCard Seven Spades)
       "j_ancient", (0.0, oneCard King Spades), Some(0.0, oneCard King Hearts)
       "j_idol", (0.0, oneCard King Spades), Some(0.0, oneCard Queen Spades)
       "j_hanging_chad", (0.0, oneCard King Spades), None
       "j_sock_and_buskin", (0.0, oneCard King Spades), Some(0.0, oneCard Seven Spades)
       "j_hack", (0.0, oneCard Four Spades), Some(0.0, oneCard King Spades)
       "j_selzer", (0.0, oneCard King Spades), None
       "j_dusk", (0.0, withLeft 0 2 (oneCard King Spades)), Some(0.0, withLeft 2 2 (oneCard King Spades))
       "j_baron", (0.0, heldKing), Some(0.0, heldQueen)
       "j_shoot_the_moon", (0.0, heldQueen), Some(0.0, heldKing)
       "j_raised_fist", (0.0, heldThree), Some(0.0, nothingHeld)
       "j_mime", (0.0, heldSteel), Some(0.0, heldKing)
       "j_green_joker", (7.0, scenes), None
       "j_ride_the_bus", (3.0, oneCard Six Spades), Some(3.0, oneCard King Spades)
       "j_runner", (0.0, straights), Some(0.0, noStraight)
       "j_square", (0.0, fours), Some(0.0, noFour)
       "j_trousers", (0.0, twoPairs), Some(0.0, noTwoPair)
       "j_ceremonial", (7.0, scenes), Some(0.0, scenes)
       "j_red_card", (7.0, scenes), Some(0.0, scenes)
       "j_flash", (7.0, scenes), Some(0.0, scenes)
       "j_castle", (7.0, scenes), Some(0.0, scenes)
       "j_wee", (7.0, scenes), Some(0.0, scenes)
       "j_ice_cream", (7.0, scenes), Some(0.0, scenes)
       "j_popcorn", (7.0, scenes), Some(0.0, scenes)
       "j_stencil", (2.0, scenes), Some(1.0, scenes)
       "j_steel_joker", (2.0, scenes), Some(1.0, scenes)
       "j_throwback", (2.0, scenes), Some(1.0, scenes)
       "j_constellation", (2.0, scenes), Some(1.0, scenes)
       "j_madness", (2.0, scenes), Some(1.0, scenes)
       "j_hologram", (2.5, scenes), Some(1.0, scenes)
       "j_campfire", (1.5, scenes), Some(1.0, scenes)
       "j_glass", (1.75, scenes), Some(1.0, scenes)
       "j_lucky_cat", (1.25, scenes), Some(1.0, scenes)
       "j_hit_the_road", (2.0, scenes), Some(1.0, scenes)
       "j_yorick", (2.0, scenes), Some(1.0, scenes)
       "j_caino", (2.0, scenes), Some(1.0, scenes)
       "j_ramen", (2.0, scenes), Some(0.0, scenes)
       "j_obelisk", (1.8, withPlays [ Pair, (5, 1) ] scenes), Some(1.8, scenes)
       "j_loyalty_card", (1.0, scenes), Some(0.0, scenes)
       "j_midas_mask", (0.0, scene [ withEnhancement Enhancement.Mult (card King Spades) ]), Some(0.0, oneCard Seven Spades)
       "j_four_fingers", (0.0, fourFlush), Some(0.0, scenes)
       "j_shortcut", (0.0, gapStraight), Some(0.0, scenes)
       "j_smeared", (0.0, redFlush), Some(0.0, scenes)
       "j_splash", (0.0, pairs), Some(0.0, scenes)
       "j_pareidolia", (0.0, oneCard Six Spades), Some(0.0, oneCard Six Spades)
       "j_baseball", (0.0, scenes), Some(0.0, scenes)
       "j_blueprint", (0.0, scenes), Some(0.0, scenes)
       "j_brainstorm", (0.0, scenes), Some(0.0, scenes) |]

[<Fact>]
let ``scores every joker where its effect fires and where it does not`` () =
    let effectKeys = jokerKeys.Value |> List.filter (fun (_, name) -> not (name.StartsWith "inert")) |> List.map fst
    let covered = jokerCases |> Array.map (fun (key, _, _) -> key) |> List.ofArray

    Assert.Equal<string list>(List.sort effectKeys, List.sort covered)

    for key, (value, fires), other in jokerCases do
        let total, without = effectOf fires key value
        Assert.True(total <> without, $"{key} did not fire where it should")

        match other with
        | Some(value, quiet) ->
            let total, without = effectWith quiet key value true
            Assert.True((total = without), $"{key} fired where it should not")
        | None -> ()

[<Fact>]
let ``lets an inert joker occupy a slot`` () =
    let total, without = effectOf pairs "j_misprint" 0.0

    Assert.Equal(without, total)

[<Fact>]
let ``orders Foil and Holo before and after a joker effect`` () =
    let holo = joker "j_misprint" (Some Holo) 0.0
    let foil = joker "j_misprint" (Some Foil) 0.0
    let triple = joker "j_cavendish" None 0.0
    let hand = single

    let totalOf jokers =
        let _, total, _ = agreeWith (situationOf 2 2 [] jokers) hand levelOne
        total

    // chips 15, mult 1: (1 + 10) * 3 against 1 * 3 + 10
    Assert.Equal(495.0, totalOf [ holo; triple ])
    Assert.Equal(195.0, totalOf [ triple; holo ])
    // Foil adds chips, so the order does not matter
    Assert.Equal(totalOf [ foil; triple ], totalOf [ triple; foil ])
    Assert.Equal(195.0, totalOf [ foil; triple ])

[<Fact>]
let ``applies a Polychrome joker after its own effect`` () =
    let polychrome key = joker key (Some Polychrome) 0.0
    let plain key = joker key None 0.0

    let totalOf jokers =
        let _, total, _ = agreeWith (situationOf 2 2 [] jokers) single levelOne
        total

    // chips 15: (1 + 4) * 1.5 = 7.5 floors to 112; had the 1.5 come first, 1.5 + 4 = 5.5 would give 82
    Assert.Equal(112.0, totalOf [ polychrome "j_joker" ])
    // the 1.5 closes its own slot: (1 + 4) * 1.5 + 4 = 11.5, but (1 + 4 + 4) * 1.5 = 13.5
    Assert.Equal(172.0, totalOf [ polychrome "j_joker"; plain "j_joker" ])
    Assert.Equal(202.0, totalOf [ plain "j_joker"; polychrome "j_joker" ])

[<Fact>]
let ``repeats a card pass for a Red Seal and Sock and Buskin, jokers in slot order`` () =
    let hand = [ withSeal Seal.Red (card King Spades) ]
    let sock, smiley, triboulet = jokerOf "j_sock_and_buskin" 0.0, jokerOf "j_smiley" 0.0, jokerOf "j_triboulet" 0.0

    let totalOf jokers =
        let _, total, _ = agreeWith (situationOf 2 2 [] jokers) hand levelOne
        total

    // chips 5 + 3 * 10; three passes of (mult + 5) * 2 from 1 give 78, of mult * 2 + 5 give 43
    Assert.Equal(2730.0, totalOf [ sock; smiley; triboulet ])
    Assert.Equal(1505.0, totalOf [ sock; triboulet; smiley ])

[<Fact>]
let ``gives Raised Fist to the last of the lowest held cards`` () =
    let flush = [ card King Spades; card Queen Spades; card Jack Spades; card Nine Spades; card Seven Spades ]
    let hand = flush @ [ card Three Diamonds; withEnhancement Enhancement.Steel (card Three Clubs) ]
    let positions, total, _ = agreeWith (situationOf 2 2 [] [ jokerOf "j_raised_fist" 0.0 ]) hand levelOne

    Assert.Equal<int list>([ 0; 1; 2; 3; 4 ], positions)
    // chips 35 + 46; the last Three is Steel: 4 * 1.5 + 6 = 12, where the first Three would give 4 + 6, then * 1.5 = 15
    Assert.Equal(972.0, total)

[<Fact>]
let ``orders Baron and Shoot the Moon with held Steel by hand position`` () =
    let queen, steelKing = card Queen Spades, withEnhancement Enhancement.Steel (card King Clubs)
    let jokers = situationOf 2 2 [] [ jokerOf "j_baron" 0.0; jokerOf "j_shoot_the_moon" 0.0 ]

    let totalOf held =
        let _, total, _ = agreeWith jokers ([ card Ace Spades; card Ace Hearts ] @ held) levelOne
        total

    // chips 32; the Queen adds 13, the Steel King multiplies by 1.5 for Steel and again for Baron
    Assert.Equal(1080.0, totalOf [ queen; steelKing ])
    Assert.Equal(560.0, totalOf [ steelKing; queen ])

[<Fact>]
let ``gives nothing for a held card with an empty pass under Mime`` () =
    let total, without = effectOf heldKing "j_mime" 0.0

    Assert.Equal(without, total)

[<Fact>]
let ``agrees on the four widest per-card hands in scope`` () =
    let top = handTypes |> List.map (fun t -> t, { Chips = 5000.0; Mult = 200.0 }) |> Map.ofList
    let one key = jokerOf key 0.0
    let red = withSeal Seal.Red
    let steel = withEnhancement Enhancement.Steel

    let check situation hand levels =
        Assert.False(breaksLimits hand situation levels)
        agreeWith situation hand levels |> ignore

    // 1. five Kings and Queens played with Triboulet and Sock and Buskin, among the heaviest other jokers
    let faces = [ card King Spades; card King Hearts; card Queen Spades; card Queen Hearts; card King Diamonds ]
    let facesAndMore = faces @ [ card King Clubs; card Queen Clubs; card Queen Diamonds ]
    let twoFaceJokers = [ one "j_triboulet"; one "j_sock_and_buskin" ]
    check (situationOf 2 2 [] (twoFaceJokers @ [ one "j_smiley"; one "j_scary_face"; one "j_photograph" ])) facesAndMore top
    check (situationOf 0 2 [] (twoFaceJokers @ [ one "j_hanging_chad"; one "j_selzer"; one "j_dusk" ])) faces levelOne

    // 2. Baron and Mime, two held Red Seal Kings and a held Steel King (the widest Steel card)
    let heldHand = [ card Ace Spades; card Ace Hearts; red (card King Spades); red (card King Hearts); steel (card King Clubs) ]
    check (situationOf 2 2 [] [ one "j_baron"; one "j_mime" ]) heldHand top

    // 3. Hanging Chad, Seltzer and Photograph, a Red Seal face card first
    let chadHand = [ red (card King Spades); card Queen Hearts; card Five Clubs; card Jack Diamonds ]
    check (situationOf 2 2 [] [ one "j_hanging_chad"; one "j_selzer"; one "j_photograph" ]) chadHand top

    let sevenPasses = [ red (card King Spades); card Queen Hearts; card Jack Clubs; card Jack Diamonds; card King Clubs ]
    let sevenJokers = [ one "j_hanging_chad"; one "j_selzer"; one "j_photograph"; one "j_sock_and_buskin"; one "j_dusk" ]
    check (situationOf 0 2 [] sevenJokers) sevenPasses levelOne

[<Fact>]
let ``counts the Mult and the Holo adds of one card in the bound`` () =
    let dense (c: Card) = withSeal Seal.Red (withEnhancement Enhancement.Glass c)

    let hand =
        [ card Two Hearts |> modified (Some Enhancement.Mult) (Some Holo) (Some Seal.Red)
          dense (card King Hearts)
          dense (card Queen Hearts)
          dense (card King Hearts)
          dense (card Queen Hearts) ]

    let situation = situationOf 2 2 [] [ jokerOf "j_triboulet" 0.0; jokerOf "j_triboulet" 0.0 ]
    let levels mult = handTypes |> List.map (fun t -> t, { Chips = 100.0; Mult = mult }) |> Map.ofList

    // the card adds 14 per pass, so (40 + 28) * 2^24 passes 2^30; with level mult 30 it does not
    Assert.Equal(None, PlayNet.choose net.Value situation hand (levels 40.0))
    Assert.True(breaksLimits hand situation (levels 40.0))
    Assert.False(breaksLimits hand situation (levels 30.0))
    agreeWith situation hand (levels 30.0) |> ignore

[<Fact>]
let ``agrees on the widest joker hand in scope`` () =
    let top = handTypes |> List.map (fun t -> t, { Chips = 5000.0; Mult = 200.0 }) |> Map.ofList
    let glass = withEnhancement Enhancement.Glass
    let steel = withEnhancement Enhancement.Steel

    let hand =
        [ glass (card King Spades)
          glass (card King Hearts)
          glass (card King Clubs)
          withEdition Polychrome (card King Diamonds)
          withEdition Polychrome (card Queen Spades)
          steel (card Queen Hearts)
          steel (card Queen Clubs)
          card Three Diamonds ]

    let jokers =
        [ joker "j_blue_joker" None 500.0
          joker "j_abstract" (Some Polychrome) 500.0
          joker "j_family" None 500.0
          joker "j_trio" (Some Polychrome) 500.0
          joker "j_drivers_license" None 500.0 ]

    let situation = situationOf 0 0 [] jokers
    let positions, total, handType = agreeWith situation hand top

    Assert.False(breaksLimits hand situation top)
    Assert.Equal(FourOfAKind, handType)
    Assert.Equal<int list>([ 0; 1; 2; 3 ], positions)
    Assert.True(total > 1e9, $"total {total} is not large")

[<Fact>]
let ``declines jokers and situations outside its scope`` () =
    let declined jokers = PlayNet.choose net.Value (situationOf 2 2 [] jokers) single levelOne
    let one key = joker key None 0.0
    let boss = { situationOf 2 2 [] [] with Boss = Some Psychic }

    Assert.Equal(None, declined (List.replicate 6 (one "j_joker")))
    Assert.Equal(None, declined [ { one "j_joker" with Debuffed = true } ])
    Assert.Equal(None, declined [ joker "j_joker" None 501.0 ])
    Assert.Equal(None, PlayNet.choose net.Value boss single levelOne)
    Assert.Equal(None, PlayNet.choose net.Value { situationOf 2 2 [] [] with Balance = true } single levelOne)
    Assert.True((declined [ joker "j_joker" None 500.0 ]).IsSome)
    // eleven passes of the first card: five Hanging Chads
    Assert.Equal(None, declined (List.replicate 5 (one "j_hanging_chad")))
    // the product of mult and chips beyond the exact range: Triboulet on every pass of five Kings
    let kings = [ card King Spades; card King Hearts; card King Clubs; card King Diamonds; withEnhancement Enhancement.Glass (card King Spades) ]
    let doubling = [ one "j_triboulet"; one "j_triboulet"; one "j_sock_and_buskin"; one "j_selzer" ]
    Assert.Equal(None, PlayNet.choose net.Value (situationOf 2 2 [] doubling) kings (levelsAt (fun _ -> 200)))

[<Fact>]
let ``fills suits with Wild cards for Flower Pot and Seeing Double`` () =
    let wild = withEnhancement Enhancement.Wild
    let fires hand joker = let total, without = effectOf (favouring FourOfAKind (scene hand)) joker 0.0 in total <> without
    let firesPair hand joker = let total, without = effectOf (favouring Pair (scene hand)) joker 0.0 in total <> without

    // a plain Hearts card, then Wilds fill Diamonds, Spades, Clubs in turn
    Assert.True(fires [ card King Hearts; wild (card King Spades); wild (card King Clubs); wild (card King Diamonds) ] "j_flower_pot")
    // a repeated plain suit leaves one suit open
    Assert.False(fires [ card King Hearts; card King Hearts; wild (card King Spades); wild (card King Clubs) ] "j_flower_pot")
    // a lone Wild fills Clubs only: no second suit
    Assert.False(firesPair [ wild (card King Hearts) ] "j_seeing_double")
    // two Wilds fill Clubs, then Diamonds
    Assert.True(firesPair [ wild (card King Hearts); wild (card King Spades) ] "j_seeing_double")
    // plain Clubs and a Wild, which fills Diamonds
    Assert.True(firesPair [ card King Clubs; wild (card King Spades) ] "j_seeing_double")

[<Fact>]
let ``counts a held Wild as black and a held Stone as not for Blackboard`` () =
    let flush = [ card Two Spades; card Five Spades; card Nine Spades; card King Spades; card Seven Spades ]
    let heldWith card = effectOf (favouring Flush (scene (flush @ [ card ]))) "j_blackboard" 0.0

    let stone, stoneWithout = heldWith (withEnhancement Enhancement.Stone (card Three Hearts))
    let wild, wildWithout = heldWith (withEnhancement Enhancement.Wild (card Three Hearts))

    Assert.Equal(stoneWithout, stone)
    Assert.True(wild <> wildWithout)

type CombinedAgreement(output: Xunit.Abstractions.ITestOutputHelper) =
    [<Fact>]
    member _.``agrees with the search on 1000 random hands over every in-scope key`` () =
        let random = Random(2031)
        let pick (options: 'a list) = options[random.Next options.Length]
        // the keys of stage 4c weigh 3, the others 1
        let keys = jokerKeys.Value |> List.map fst |> List.collect (fun k -> if List.contains k stage4cKeys then [ k; k; k ] else [ k ])

        let cases =
            Array.init 1000 (fun _ ->
                let size = random.Next(1, 9)
                let shuffled = Array.copy deck
                random.Shuffle shuffled
                let hand = [ for c in shuffled[.. size - 1] -> modified (pick enhancements) (pick editions) (pick seals) c ]

                let jokers =
                    [ for _ in 1 .. random.Next(0, 6) -> randomJoker random hand (pick keys) (pick editions) ]

                let plays =
                    [ for t in handTypes ->
                          let run = random.Next(0, 6)
                          t, (run, random.Next(0, run + 1)) ]

                let situation = situationOf (random.Next(0, 5)) (random.Next(0, 5)) plays jokers
                hand, situation, levelsAt (fun _ -> random.Next(1, 31)))

        let results =
            cases
            |> Array.Parallel.map (fun (hand, situation, levels) ->
                hand, situation, levels, PlayNet.choose net.Value situation hand levels, bestWith situation hand levels)

        let rejected = results |> Array.filter (fun (_, _, _, got, _) -> got.IsNone)
        let checkedCases = results |> Array.filter (fun (_, _, _, got, _) -> got.IsSome)

        // a rejection must break a limit or a factor rule, which breaksLimits computes
        let loose =
            rejected |> Array.filter (fun (hand, situation, levels, _, _) -> not (breaksLimits hand situation levels))

        let failures = checkedCases |> Array.filter (fun (_, _, _, got, expected) -> got <> Some expected)
        let limits, _, _, _ = netLimits.Value

        let beyond =
            checkedCases
            |> Array.filter (fun (hand, situation, levels, got, _) ->
                let positions, _, _ = got.Value
                let played = positions |> List.map (fun i -> hand[i])
                let held = hand |> List.indexed |> List.filter (fun (i, _) -> not (List.contains i positions)) |> List.map snd
                let b = Scoring.scoreWith situation { Played = played; Held = held; Levels = levels }
                b.Mult > limits["multCap"] || b.Chips > limits["chipsCap"] || b.Mult * b.Chips > limits["productCap"])

        output.WriteLine $"checked {checkedCases.Length}, rejected {rejected.Length}"

        Assert.True(beyond.Length = 0, $"{beyond.Length} accepted cases reach beyond the caps")
        Assert.True(loose.Length = 0, $"{loose.Length} cases rejected inside the limits; first: %A{Array.truncate 5 loose}")
        Assert.True(checkedCases.Length >= 700, $"only {checkedCases.Length} cases checked")
        Assert.True(failures.Length = 0, $"{failures.Length} of {checkedCases.Length} cases disagree; first: %A{Array.truncate 5 failures}")

[<Fact>]
let ``covers every implemented key but Vampire, and the registry's Uncommon set`` () =
    let outOfScope = set [ "j_vampire" ]
    let keys = jokerKeys.Value |> List.map fst |> Set.ofList

    Assert.Equal<Set<string>>(Set.difference JokerRegistry.implemented outOfScope, keys)
    Assert.Equal<Set<string>>(Set.intersect JokerRegistry.uncommon keys, snd netSets.Value)

[<Fact>]
let ``resolves Blueprint and Brainstorm as the reference does`` () =
    let one key = jokerOf key 0.0
    let cavendish, triboulet, blueprint, brainstorm = one "j_cavendish", one "j_triboulet", one "j_blueprint", one "j_brainstorm"
    let king = [ card King Spades ]

    let totalOf jokers hand =
        let _, total, _ = agreeWith (situationOf 2 2 [] jokers) hand levelOne
        total

    // at the right end Blueprint copies nothing
    Assert.Equal(totalOf [ cavendish ] king, totalOf [ cavendish; blueprint ] king)
    // a chain of two Blueprints reaches the Triboulet, three of them double three times
    Assert.Equal(totalOf [ triboulet; triboulet; triboulet ] king, totalOf [ blueprint; blueprint; triboulet ] king)
    // Brainstorm copies Blueprint, which copies Brainstorm: the loop is cut and gives neither anything
    Assert.Equal(totalOf [ cavendish ] king, totalOf [ blueprint; brainstorm; cavendish ] king)
    // Brainstorm in the first slot copies itself
    Assert.Equal(totalOf [ cavendish ] king, totalOf [ brainstorm; cavendish ] king)
    // Brainstorm copies the first slot, past the jokers in between
    Assert.Equal(totalOf [ triboulet; cavendish; triboulet ] king, totalOf [ triboulet; cavendish; brainstorm ] king)

[<Fact>]
let ``copies the key and value of a joker but keeps its own edition`` () =
    let one key = jokerOf key 0.0
    let plain, polychrome = joker "j_joker" None 0.0, joker "j_joker" (Some Polychrome) 0.0
    let blueprint, baseball, mime = one "j_blueprint", one "j_baseball", one "j_mime"

    let totalOf jokers hand =
        let _, total, _ = agreeWith (situationOf 2 2 [] jokers) hand levelOne
        total

    let king = [ card King Spades ]
    // the Blueprint acts as a plain Joker: the Polychrome stays on the joker it sits on
    Assert.Equal(totalOf [ plain; polychrome ] king, totalOf [ blueprint; polychrome ] king)
    // the value is the target's: Abstract Joker adds its own value twice
    Assert.Equal(totalOf [ jokerOf "j_abstract" 7.0; jokerOf "j_abstract" 7.0 ] king, totalOf [ blueprint; jokerOf "j_abstract" 7.0 ] king)
    // Baseball Card copied by a Blueprint multiplies the Uncommon Mime twice
    let once = totalOf [ baseball; mime ] king
    let twice = totalOf [ blueprint; baseball; mime ] king
    Assert.Equal(totalOf [ baseball; baseball; mime ] king, twice)
    Assert.True(twice > once)
    // five slots that all act as Baseball Card meet no Uncommon joker: nothing is multiplied
    Assert.Equal(totalOf [] king, totalOf [ blueprint; baseball; blueprint; baseball; baseball ] king)
    // a Baseball Card does not multiply itself or a common joker
    Assert.Equal(totalOf [ one "j_cavendish" ] king, totalOf [ baseball; one "j_cavendish" ] king)

[<Fact>]
let ``turns Glass face cards Gold under Midas Mask before Ride the Bus and Smiley Face`` () =
    let one key = jokerOf key 0.0
    let midas, smiley, bus = one "j_midas_mask", one "j_smiley", jokerOf "j_ride_the_bus" 3.0
    let glassKing = withEnhancement Enhancement.Glass (card King Spades)

    let totalOf jokers hand levels =
        let _, total, _ = agreeWith (situationOf 2 2 [] jokers) hand levels
        total

    // the Glass x2 is gone: the Gold King scores as a plain King with Smiley Face's 5 on it
    Assert.Equal(totalOf [ smiley ] [ card King Spades ] levelOne, totalOf [ midas; smiley ] [ glassKing ] levelOne)
    Assert.True(totalOf [ smiley ] [ glassKing ] levelOne > totalOf [ midas; smiley ] [ glassKing ] levelOne)
    // Ride the Bus sees a face card on either side of the Midas Mask, and adds nothing
    Assert.Equal(totalOf [ bus ] [ card King Spades ] levelOne, totalOf [ midas; bus ] [ glassKing ] levelOne)
    Assert.Equal(totalOf [ midas; bus ] [ glassKing ] levelOne, totalOf [ bus; midas ] [ glassKing ] levelOne)
    Assert.True(totalOf [ bus ] [ card Six Spades ] levelOne > totalOf [] [ card Six Spades ] levelOne)
    // a Bonus card loses its 30 chips, and a Wild card its suits for Flower Pot
    let bonus = withEnhancement Enhancement.Bonus (card King Spades)
    Assert.Equal(totalOf [ midas ] [ card King Spades ] levelOne, totalOf [ midas ] [ bonus ] levelOne)
    let wild = withEnhancement Enhancement.Wild
    let kings = [ card King Hearts; wild (card King Hearts); wild (card King Clubs); wild (card King Diamonds) ]
    let fourKings = (favouring FourOfAKind (scene kings)).Rates
    let pot = one "j_flower_pot"
    Assert.True(totalOf [ pot ] kings fourKings > totalOf [] kings fourKings)
    Assert.Equal(totalOf [ midas ] kings fourKings, totalOf [ midas; pot ] kings fourKings)

[<Fact>]
let ``agrees on the widest counter, factor and copy hands in scope`` () =
    let top = handTypes |> List.map (fun t -> t, { Chips = 5000.0; Mult = 200.0 }) |> Map.ofList
    let one key = jokerOf key 0.0
    let glass = withEnhancement Enhancement.Glass

    let check situation hand levels =
        Assert.False(breaksLimits hand situation levels)
        agreeWith situation hand levels |> ignore

    let faces = [ card King Spades; card King Hearts; card Queen Spades; card Queen Hearts; card King Diamonds ]

    // 1. Blueprint copying Triboulet, Brainstorm copying Blueprint: three doublings on every pass of five Kings and Queens
    check (situationOf 2 2 [] [ one "j_blueprint"; one "j_triboulet"; one "j_brainstorm" ]) faces top

    // 2. Hologram x2.5, Campfire x1.5 and Baseball Card with two Uncommon jokers (Hologram and Mime), levels at the limits
    let hologram, campfire = jokerOf "j_hologram" 2.5, jokerOf "j_campfire" 1.5
    check (situationOf 2 2 [] [ hologram; campfire; one "j_baseball"; one "j_mime"; one "j_joker" ]) [ card Ace Spades; card Ace Hearts; card King Spades ] top

    // 3. Midas Mask before Smiley Face and Ride the Bus with Glass face cards played
    let glassFaces = faces |> List.map glass
    check (situationOf 2 2 [] [ one "j_midas_mask"; one "j_smiley"; jokerOf "j_ride_the_bus" 40.0 ]) glassFaces top

[<Fact>]
let ``declines factors and jokers it cannot score exactly`` () =
    let declined jokers = PlayNet.choose net.Value (situationOf 2 2 [] jokers) single levelOne

    // a factor of 1.1 is not a multiple of 1/4, and 9 is above FMAX
    Assert.Equal(None, declined [ jokerOf "j_constellation" 1.1 ])
    Assert.Equal(None, declined [ jokerOf "j_hologram" 9.0 ])
    Assert.Equal(None, declined [ jokerOf "j_obelisk" 1.0 ])
    Assert.True((declined [ jokerOf "j_hologram" 8.0 ]).IsSome)
    Assert.True((declined [ jokerOf "j_constellation" 1.25 ]).IsSome)
    Assert.True((declined [ jokerOf "j_constellation" 1.0 ]).IsSome)
    Assert.Equal(None, declined [ jokerOf "j_vampire" 1.0 ])
    Assert.True((declined [ jokerOf "j_four_fingers" 0.0 ]).IsSome)
    Assert.Equal(None, declined [ jokerOf "j_blueprint" 0.0; jokerOf "j_vampire" 0.0 ])

[<Fact>]
let ``ignores the value of a joker that multiplies by an input factor`` () =
    let hand = [ card Ace Spades; card Seven Hearts; card Seven Clubs; card Three Diamonds; card Nine Spades ]
    let levels = handTypes |> List.map (fun t -> t, { Chips = 30.0; Mult = 3.0 }) |> Map.ofList
    let green = jokerOf "j_green_joker" 5.0

    // Ramen reads its text, so a huge or non-finite value must not leak into the net
    for value in [ 1000.0; Double.NaN; Double.PositiveInfinity ] do
        let ramen = { jokerOf "j_ramen" 2.0 with Value = Some value }
        agreeWith (situationOf 2 2 [] [ ramen; green ]) hand levels |> ignore

[<Fact>]
let ``retriggers only the first scoring card for Hanging Chad`` () =
    let hanging = situationOf 2 2 [] [ jokerOf "j_hanging_chad" 0.0 ]
    let mult = withEnhancement Enhancement.Mult (card King Spades)
    let pairLevel = levelsAt (fun t -> if t = Pair then 30 else 1)

    let totalOf hand =
        let _, total, _ = agreeWith hanging hand pairLevel
        total

    // Pair at level 30: chips 445 + 10 + 3 * 10, mult 31 + 3 * 4 when the Mult card is first, 31 + 4 when it is second;
    // had both cards been retriggered the two orders would score alike
    Assert.Equal(20855.0, totalOf [ mult; card King Hearts ])
    Assert.Equal(16975.0, totalOf [ card King Hearts; mult ])

type RuleAgreement(output: Xunit.Abstractions.ITestOutputHelper) =
    [<Fact>]
    member _.``agrees with the search on 1000 random hands holding rule jokers`` () =
        let random = Random(2032)
        let pick (options: 'a list) = options[random.Next options.Length]
        let ruleKeys = [ "j_four_fingers"; "j_shortcut"; "j_smeared"; "j_splash"; "j_pareidolia" ]
        let others = jokerKeys.Value |> List.map fst |> List.filter (fun k -> not (List.contains k ruleKeys))
        let ranks14 = [| Ace; Two; Three; Four; Five; Six; Seven; Eight; Nine; Ten; Jack; Queen; King; Ace |]
        let suits = [| Spades; Hearts; Clubs; Diamonds |]

        // ranks from a window of seven, Ace low or high, and mostly one suit: straights with a gap and 4-card flushes are common
        let handOf size =
            let low = random.Next(0, 8)
            let main = suits[random.Next 4]
            let chosen = Collections.Generic.HashSet<Rank * Suit>()

            [ while chosen.Count < size do
                  let rank = ranks14[low + random.Next 7]
                  let suit = if random.Next 3 > 0 then main else suits[random.Next 4]

                  if chosen.Add((rank, suit)) then
                      yield modified (pick enhancements) (pick editions) (pick seals) (card rank suit) ]

        let cases =
            Array.init 1000 (fun _ ->
                let hand = handOf (random.Next(1, 9))

                let held = [ for key in ruleKeys do if random.Next 3 = 0 then jokerOf key 0.0 ]
                let more = [ for _ in 1 .. random.Next(0, 4) -> randomJoker random hand (pick others) (pick editions) ]
                let jokers = (held @ more) |> List.sortBy (fun _ -> random.Next()) |> List.truncate 5

                let plays =
                    [ for t in handTypes ->
                          let run = random.Next(0, 6)
                          t, (run, random.Next(0, run + 1)) ]

                let situation = situationOf (random.Next(0, 5)) (random.Next(0, 5)) plays jokers
                hand, situation, levelsAt (fun _ -> random.Next(1, 21)))

        let results =
            cases
            |> Array.Parallel.map (fun (hand, situation, levels) ->
                hand, situation, levels, PlayNet.choose net.Value situation hand levels, bestWith situation hand levels)

        let rejected = results |> Array.filter (fun (_, _, _, got, _) -> got.IsNone)
        let checkedCases = results |> Array.filter (fun (_, _, _, got, _) -> got.IsSome)

        // a rejection must break a limit or a factor rule, which breaksLimits computes
        let loose =
            rejected |> Array.filter (fun (hand, situation, levels, _, _) -> not (breaksLimits hand situation levels))

        let failures = checkedCases |> Array.filter (fun (_, _, _, got, expected) -> got <> Some expected)
        let limits, _, _, _ = netLimits.Value

        let beyond =
            checkedCases
            |> Array.filter (fun (hand, situation, levels, got, _) ->
                let positions, _, _ = got.Value
                let played = positions |> List.map (fun i -> hand[i])
                let held = hand |> List.indexed |> List.filter (fun (i, _) -> not (List.contains i positions)) |> List.map snd
                let b = Scoring.scoreWith situation { Played = played; Held = held; Levels = levels }
                b.Mult > limits["multCap"] || b.Chips > limits["chipsCap"] || b.Mult * b.Chips > limits["productCap"])

        output.WriteLine $"checked {checkedCases.Length}, rejected {rejected.Length}"

        Assert.True(beyond.Length = 0, $"{beyond.Length} accepted cases reach beyond the caps")
        Assert.True(loose.Length = 0, $"{loose.Length} cases rejected inside the limits; first: %A{Array.truncate 5 loose}")
        Assert.True(checkedCases.Length >= 700, $"only {checkedCases.Length} cases checked")
        Assert.True(failures.Length = 0, $"{failures.Length} of {checkedCases.Length} cases disagree; first: %A{Array.truncate 5 failures}")

let private rulesOf (ff: bool) (sh: bool) =
    [ if ff then jokerOf "j_four_fingers" 0.0
      if sh then jokerOf "j_shortcut" 0.0 ]

let private totalWith jokers cards levels =
    let _, total, _ = agreeWith (situationOf 2 2 [] jokers) cards levels
    total

[<Fact>]
let ``reads straights under Four Fingers and Shortcut as Hands does`` () =
    let suits = [| Spades; Hearts; Clubs; Diamonds; Spades |]
    // suits alternate so that no case holds a Flush
    let hand (ranks: Rank list) = ranks |> List.mapi (fun i r -> card r suits[i])
    let straightLevels = levelsAt (fun t -> if t = Straight then 30 else 1)

    // the ranks, and whether Four Fingers and Shortcut make them a Straight
    let cases: (Rank list * (bool -> bool -> bool))[] =
        [| [ Five; Six; Seven; Eight; Nine ], (fun _ _ -> true)
           // one gap at the 7
           [ Five; Six; Eight; Nine; Ten ], (fun _ sh -> sh)
           // four in a row and a King
           [ Five; Six; Seven; Eight; King ], (fun ff _ -> ff)
           // four with a gap and a King
           [ Five; Six; Eight; Nine; King ], (fun ff sh -> ff && sh)
           // Ace low
           [ Ace; Two; Three; Four; Nine ], (fun ff _ -> ff)
           // Ace low with a gap at the 4
           [ Ace; Two; Three; Five; Nine ], (fun ff sh -> ff && sh)
           // Ace low with a gap at the 2, next to the Ace
           [ Ace; Three; Four; Five; Nine ], (fun ff sh -> ff && sh)
           // Ace high
           [ Jack; Queen; King; Ace; Two ], (fun ff _ -> ff)
           // Ace high with a gap at the King, at the Ace end
           [ Ten; Jack; Queen; Ace; Two ], (fun ff sh -> ff && sh)
           // a step of 3 from the Jack to the Ace
           [ Eight; Nine; Ten; Jack; Ace ], (fun ff _ -> ff)
           // a gap of 1 at every step
           [ Five; Seven; Nine; Jack; King ], (fun _ sh -> sh)
           [ Five; Six; Eight; Ten; Jack ], (fun _ sh -> sh)
           // two gaps in a row: 7 and 8
           [ Five; Six; Nine; Ten; Jack ], (fun _ _ -> false)
           // the Ace does not wrap round to the 2
           [ Queen; King; Ace; Two; Three ], (fun _ _ -> false) |]

    for ranks, expected in cases do
        for ff, sh in [ false, false; true, false; false, true; true, true ] do
            let _, _, handType = agreeWith (situationOf 2 2 [] (rulesOf ff sh)) (hand ranks) straightLevels
            Assert.True((handType = Straight) = expected ff sh, $"%A{ranks} with Four Fingers {ff} and Shortcut {sh} read as {handType}")

    // the scoring cards of a 4-card straight with a gap leave the King out, and Splash brings it in
    let gapped = hand [ Five; Six; Eight; Nine; King ]
    let positions, total, _ = agreeWith (situationOf 2 2 [] (rulesOf true true)) gapped straightLevels

    let splashPositions, splashTotal, _ =
        agreeWith (situationOf 2 2 [] (rulesOf true true @ [ jokerOf "j_splash" 0.0 ])) gapped straightLevels

    Assert.Equal<int list>([ 0; 1; 2; 3 ], positions)
    Assert.Equal<int list>([ 0; 1; 2; 3; 4 ], splashPositions)
    Assert.True(splashTotal > total)

[<Fact>]
let ``scores the cards of a flush that Four Fingers finds, not the odd one out`` () =
    let hearts = [ card Two Hearts; card Five Hearts; card Nine Hearts; card King Hearts ]
    let hand = hearts @ [ card Seven Spades ]
    let flushLevels = levelsAt (fun t -> if t = Flush then 30 else 1)
    let fourFingers = jokerOf "j_four_fingers" 0.0
    let splash = jokerOf "j_splash" 0.0

    // the Spade is left out of the play: it would add nothing
    let positions, total, handType = agreeWith (situationOf 2 2 [] [ fourFingers ]) hand flushLevels
    Assert.Equal<int list>([ 0; 1; 2; 3 ], positions)
    Assert.Equal(Flush, handType)
    Assert.Equal(totalWith [ fourFingers ] hearts flushLevels, total)
    // without Four Fingers four hearts are no Flush
    let _, _, plain = agreeWith (situationOf 2 2 [] []) hand flushLevels
    Assert.NotEqual(Flush, plain)
    // with Splash the Spade scores too
    let splashPositions, splashTotal, _ = agreeWith (situationOf 2 2 [] [ fourFingers; splash ]) hand flushLevels
    Assert.Equal<int list>([ 0; 1; 2; 3; 4 ], splashPositions)
    Assert.True(splashTotal > total)
    // a Wild card counts as a fourth heart, and a Stone never does
    let wild = withEnhancement Enhancement.Wild (card Seven Spades)
    let stone = withEnhancement Enhancement.Stone (card Seven Spades)
    let _, _, withWild = agreeWith (situationOf 2 2 [] [ fourFingers ]) (List.take 3 hearts @ [ wild ]) flushLevels
    let _, _, withStone = agreeWith (situationOf 2 2 [] [ fourFingers ]) (List.take 3 hearts @ [ stone ]) flushLevels
    Assert.Equal(Flush, withWild)
    Assert.NotEqual(Flush, withStone)

[<Fact>]
let ``reads Smeared suits in flushes, suit jokers and Flower Pot`` () =
    let smeared, lusty, pot = jokerOf "j_smeared" 0.0, jokerOf "j_lusty_joker" 0.0, jokerOf "j_flower_pot" 0.0
    let wild = withEnhancement Enhancement.Wild
    let flushLevels = levelsAt (fun t -> if t = Flush then 30 else 1)

    // Hearts and Diamonds make a Flush, and Lusty Joker counts both
    let reds = [ card Two Hearts; card Five Diamonds; card Nine Hearts; card King Diamonds; card Seven Hearts ]
    let _, _, redType = agreeWith (situationOf 2 2 [] [ smeared ]) reds flushLevels
    let _, _, plainType = agreeWith (situationOf 2 2 [] []) reds flushLevels
    Assert.Equal(Flush, redType)
    Assert.NotEqual(Flush, plainType)
    Assert.True(totalWith [ smeared; lusty ] reds flushLevels > totalWith [ smeared ] reds flushLevels)

    // two colours cover only two suits: Flower Pot stays quiet with Hearts and Diamonds only
    Assert.Equal(totalWith [ smeared; lusty ] reds flushLevels, totalWith [ smeared; lusty; pot ] reds flushLevels)
    // Splash scores all four cards: two red and two black cards fill Hearts, Diamonds, Spades and Clubs only with Smeared
    let splash = jokerOf "j_splash" 0.0
    let fourColours = [ card Two Hearts; card Five Hearts; card Nine Spades; card King Spades ]
    Assert.True(totalWith [ smeared; splash; pot ] fourColours levelOne > totalWith [ smeared; splash ] fourColours levelOne)
    Assert.Equal(totalWith [ splash; pot ] fourColours levelOne, totalWith [ splash ] fourColours levelOne)
    // one red card, one black card and two Wild cards fill the four suits
    let withWilds = [ card Two Hearts; card Five Spades; wild (card Nine Spades); wild (card King Clubs) ]
    Assert.True(totalWith [ smeared; splash; pot ] withWilds levelOne > totalWith [ smeared; splash ] withWilds levelOne)
    // one card per colour fills two suits only
    let twoColours = [ card Two Hearts; card Five Spades ]
    Assert.Equal(totalWith [ smeared; splash; pot ] twoColours levelOne, totalWith [ smeared; splash ] twoColours levelOne)
    // Seeing Double needs Clubs: a Spade covers it only with Smeared
    let double = jokerOf "j_seeing_double" 0.0
    let kings = [ card King Spades; card King Hearts ]
    Assert.True(totalWith [ smeared; double ] kings levelOne > totalWith [ smeared ] kings levelOne)
    Assert.Equal(totalWith [ double ] kings levelOne, totalWith [] kings levelOne)

[<Fact>]
let ``lets a Blueprint or Brainstorm beside a rule joker add nothing to the rules`` () =
    let one key = jokerOf key 0.0
    let blueprint, brainstorm, partner = one "j_blueprint", one "j_brainstorm", one "j_cavendish"
    let flushLevels = levelsAt (fun t -> if t = Flush then 30 else 1)

    for key, hand, levels, helper in
        [ "j_splash", [ card King Spades; card King Hearts; card Four Diamonds ], levelOne, partner
          "j_smeared", [ card Two Hearts; card Five Diamonds; card Nine Hearts; card King Diamonds; card Seven Hearts ], flushLevels, partner
          "j_pareidolia", [ card Six Spades ], levelOne, one "j_smiley" ] do
        let rule = one key
        let held = totalWith [ rule; helper ] hand levels
        // the rule changes the total
        Assert.NotEqual(totalWith [ helper; helper ] hand levels, totalWith [ rule; helper; helper ] hand levels)
        // a copy of the rule joker adds no effect and no second rule, in front of it or after it
        Assert.Equal(held, totalWith [ blueprint; rule; helper ] hand levels)
        Assert.Equal(held, totalWith [ rule; helper; brainstorm ] hand levels)
        // a Blueprint that copies a joker which is not a rule joker makes no rule: it reads as the joker twice
        Assert.Equal(totalWith [ helper; helper ] hand levels, totalWith [ blueprint; helper ] hand levels)

[<Fact>]
let ``agrees on the must-cover rule hands in scope at the top levels`` () =
    let top = handTypes |> List.map (fun t -> t, { Chips = 5000.0; Mult = 200.0 }) |> Map.ofList
    let one key = jokerOf key 0.0
    let wild = withEnhancement Enhancement.Wild

    let check jokers hand =
        let situation = situationOf 2 2 [] jokers
        Assert.False(breaksLimits hand situation top)
        agreeWith situation hand top |> ignore

    // 1. Four Fingers and Shortcut: a 4-card straight with a gap, and a 4-card flush in a 5-card play with an off-suit card
    check [ one "j_four_fingers"; one "j_shortcut" ] [ card Five Spades; card Six Hearts; card Eight Clubs; card Nine Diamonds; card King Spades ]
    check [ one "j_four_fingers"; one "j_shortcut"; one "j_splash" ] [ card Five Spades; card Six Hearts; card Eight Clubs; card Nine Diamonds; card King Spades ]
    check [ one "j_four_fingers" ] [ card Two Hearts; card Five Hearts; card Nine Hearts; card King Hearts; card Seven Spades ]
    // 2. Smeared, Lusty Joker and Flower Pot with Hearts and Diamonds, and with Wild cards
    check [ one "j_smeared"; one "j_lusty_joker"; one "j_flower_pot" ] [ card Two Hearts; card Five Diamonds; card Nine Hearts; card King Diamonds; card Seven Hearts ]
    check [ one "j_smeared"; one "j_lusty_joker"; one "j_flower_pot" ] [ card Two Hearts; card Five Diamonds; wild (card Nine Spades); wild (card King Clubs); card Seven Hearts ]
    // 3. Splash, Triboulet and Sock and Buskin: a Pair of Kings and cards that would not score without Splash
    check [ one "j_splash"; one "j_triboulet"; one "j_sock_and_buskin" ] [ card King Spades; card King Hearts; card Queen Diamonds; card Jack Clubs; card Ten Spades ]
    // 4. Pareidolia, Photograph, Midas Mask and Sock and Buskin with number cards
    check [ one "j_pareidolia"; one "j_photograph"; one "j_midas_mask"; one "j_sock_and_buskin" ] [ card Three Spades; card Five Hearts; card Seven Clubs; card Nine Diamonds; card Two Spades ]
    // the widest Splash hand in scope: five Glass Kings and Queens, one doubling from Triboulet and one pass from Sock and Buskin on each
    let glassFaces = [ card King Spades; card King Hearts; card Queen Spades; card Queen Hearts; card King Diamonds ] |> List.map (withEnhancement Enhancement.Glass)
    check [ one "j_splash"; one "j_triboulet"; one "j_sock_and_buskin" ] glassFaces

[<Fact>]
let ``turns a Stone Gold under Pareidolia and a Midas Mask`` () =
    let one key = jokerOf key 0.0
    let stone = withEnhancement Enhancement.Stone
    let hand = [ stone (card King Spades) ]
    let totalOf jokers cards = totalWith jokers cards levelOne

    // a Stone scores 50 chips and has no rank: Triboulet ignores it
    let plain = totalOf [ one "j_pareidolia"; one "j_triboulet" ] hand
    // Pareidolia makes it a face card, so a Midas Mask turns it Gold: it scores as a King of Spades, which Triboulet doubles
    let gold = totalOf [ one "j_pareidolia"; one "j_midas_mask"; one "j_triboulet" ] hand
    Assert.NotEqual(plain, gold)
    // without Pareidolia a Stone is no face card, and the Midas Mask leaves it be
    Assert.Equal(plain, totalOf [ one "j_midas_mask"; one "j_triboulet" ] hand)
    // the Gold card has its suit and rank for the jokers that read them
    let diamonds = [ stone (card Two Diamonds) ]
    let jokers = [ one "j_pareidolia"; one "j_midas_mask"; one "j_greedy_joker"; one "j_even_steven"; one "j_hack" ]
    Assert.True(totalOf jokers diamonds > totalOf [ one "j_pareidolia"; one "j_greedy_joker"; one "j_even_steven"; one "j_hack" ] diamonds)

[<Fact>]
let ``declines hands where a rule joker pushes a limit over`` () =
    let one key = jokerOf key 0.0
    let flat chips mult = handTypes |> List.map (fun t -> t, { Chips = chips; Mult = mult }) |> Map.ofList
    let declined jokers hand levels = PlayNet.choose net.Value (situationOf 2 2 [] jokers) hand levels

    // Vampire is out of scope with Splash too
    Assert.Equal(None, declined [ one "j_vampire"; one "j_splash" ] [ card King Spades ] levelOne)

    // each case: the jokers with the rule, the same without it, a hand and levels; the rule alone breaks a limit
    let pushes name withRule withoutRule hand levels =
        Assert.Equal(None, declined withRule hand levels)
        Assert.True(breaksLimits hand (situationOf 2 2 [] withRule) levels, $"{name}: the test's bound is not broken")
        Assert.False(breaksLimits hand (situationOf 2 2 [] withoutRule) levels, $"{name}: the bound is broken without the rule")
        agreeWith (situationOf 2 2 [] withoutRule) hand levels |> ignore

    // Pareidolia makes every card a face card: three Sock and Buskin retrigger five Glass cards three more times each
    let glassNumbers =
        [ card Two Spades; card Three Hearts; card Four Clubs; card Five Diamonds; card Six Spades ]
        |> List.map (withEnhancement Enhancement.Glass >> withSeal Seal.Red)
    let socks = [ one "j_sock_and_buskin"; one "j_sock_and_buskin"; one "j_sock_and_buskin" ]
    pushes "Pareidolia" (one "j_pareidolia" :: socks) (one "j_cavendish" :: socks) glassNumbers (flat 100.0 40.0)

    // Smeared lets Clubs match the Spades two Ancient Jokers name: five cards with a Red Seal take 2 * 2 factors 1.5 each
    let ancient = { one "j_ancient" with Text = ancientText "Spades" }
    let blacks = [ card King Spades; card Seven Spades; card Five Clubs; card Nine Clubs; card Three Clubs ] |> List.map (withSeal Seal.Red)
    pushes "Smeared" [ one "j_smeared"; ancient; ancient ] [ one "j_cavendish"; ancient; ancient ] blacks (flat 10.0 2.0)

    // a Stone is a King to Triboulet only when Pareidolia makes it a face card and a Midas Mask turns it Gold
    let stones = List.replicate 5 (withSeal Seal.Red (withEnhancement Enhancement.Stone (card King Spades)))
    let triboulets = List.replicate 3 (one "j_triboulet")
    pushes "Pareidolia and Midas Mask" ([ one "j_pareidolia"; one "j_midas_mask" ] @ triboulets) ([ one "j_pareidolia"; one "j_cavendish" ] @ triboulets) stones (flat 10.0 10.0)
    Assert.Equal(None, declined ([ one "j_midas_mask"; one "j_pareidolia" ] @ triboulets) stones (flat 10.0 10.0))
    Assert.True((declined ([ one "j_midas_mask"; one "j_cavendish" ] @ triboulets) stones (flat 10.0 10.0)).IsSome)

    // Splash scores all five cards, but the bound already counts the five best cards of the hand: with or without it the
    // heavy hand is declined, and the same hand without the Red Seals is in scope with Splash
    let glassKings = [ card King Spades; card King Hearts; card Queen Spades; card Queen Hearts; card King Diamonds ] |> List.map (withEnhancement Enhancement.Glass)
    let sealed' = glassKings |> List.map (withSeal Seal.Red)
    let splashJokers = [ one "j_splash"; one "j_triboulet"; one "j_sock_and_buskin" ]
    let top = flat 5000.0 200.0
    Assert.Equal(None, declined splashJokers sealed' top)
    Assert.Equal(None, declined (List.tail splashJokers) sealed' top)
    Assert.True(breaksLimits sealed' (situationOf 2 2 [] splashJokers) top)
    Assert.False(breaksLimits glassKings (situationOf 2 2 [] splashJokers) top)
    agreeWith (situationOf 2 2 [] splashJokers) glassKings top |> ignore

[<Fact>]
let ``readme lists every dim once`` () =
    use doc = JsonDocument.Parse(File.ReadAllText(AppContext.BaseDirectory + "nn/playnet.json"))
    let d = doc.RootElement.GetProperty("d").GetInt32()

    let indices =
        File.ReadAllLines(Path.Combine(__SOURCE_DIRECTORY__, "../../nn/README.md"))
        |> Array.choose (fun line ->
            let cells = line.Split('|')

            if cells.Length > 2 then
                match Int32.TryParse(cells[1].Trim()) with
                | true, index -> Some index
                | _ -> None
            else
                None)

    Assert.Equal<int[]>([| 0 .. d - 1 |], Array.sort indices)
