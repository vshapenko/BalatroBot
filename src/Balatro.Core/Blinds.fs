module Balatro.Core.Blinds

open Balatro.Core.Domain

type BlindKind =
    | Small
    | Big
    | Boss

let private baseAmounts stake =
    let number = Stake.number stake

    if number >= 6 then
        [| 300.0; 1000.0; 3200.0; 9000.0; 25000.0; 60000.0; 110000.0; 200000.0 |]
    elif number >= 3 then
        [| 300.0; 900.0; 2600.0; 8000.0; 20000.0; 36000.0; 60000.0; 100000.0 |]
    else
        [| 300.0; 800.0; 2000.0; 5000.0; 11000.0; 20000.0; 35000.0; 50000.0 |]

let blindAmount (stake: Stake) (ante: int) : float =
    let amounts = baseAmounts stake

    if ante < 1 then
        100.0
    elif ante <= 8 then
        amounts[ante - 1]
    else
        let c = float (ante - 8)
        let d = 1.0 + 0.2 * c
        let amount = floor (amounts[7] * (1.6 + (0.75 * c) ** d) ** c)
        amount - amount % (10.0 ** floor (log10 amount - 1.0))

let blindRequirement (stake: Stake) (ante: int) (kind: BlindKind) : float =
    let multiplier =
        match kind with
        | Small -> 1.0
        | Big -> 1.5
        | Boss -> 2.0

    blindAmount stake ante * multiplier
