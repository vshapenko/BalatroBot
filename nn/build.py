import json
import math
from pathlib import Path

Expr = dict[str, float]

BETA = 800.0
MMAX = 200
CMAX = 5000
WMAX = 8
GMAX = 6
POSITIONS = 5
SLOTS = 13
CARD_SLOTS = 8
SENTINEL = 20
CHIP_RADIX_BITS = 4
FLOOR_RADIX_BITS = 8

TYPES = [
    "FlushFive",
    "FlushHouse",
    "FiveOfAKind",
    "StraightFlush",
    "FourOfAKind",
    "FullHouse",
    "Flush",
    "Straight",
    "ThreeOfAKind",
    "TwoPair",
    "Pair",
    "HighCard",
]
RANKS = list(range(2, 15))
SUITS = ["S", "H", "C", "D"]
ENHANCEMENTS = ["Bonus", "Mult", "Wild", "Glass", "Steel", "Stone", "Gold", "Lucky"]
EDITIONS = ["Foil", "Holo", "Polychrome", "Negative"]
SEALS = ["Red", "Blue", "Gold", "Purple"]
WINDOW = 5
PAIRS = [(k, m) for k in range(1, POSITIONS + 1) for m in range(k + 1, POSITIONS + 1)]
ORDERED = [
    (k, m) for k in range(1, POSITIONS + 1) for m in range(1, POSITIONS + 1) if k != m
]

# Worst cases under the limits: every add comes first, then every factor
ADDS = POSITIONS * 2 * (4 + 10)
MULT_CAP = math.ceil((MMAX + ADDS) * 2**GMAX * 1.5**WMAX)
CARD_CHIPS_CAP = 100
CHIPS_CAP = CMAX + POSITIONS * 2 * CARD_CHIPS_CAP
PRODUCT_CAP = CHIPS_CAP * MULT_CAP
FRAC = WMAX
RESOLUTION = float(2**FRAC)
FLOOR_DIGITS = math.ceil(PRODUCT_CAP.bit_length() / FLOOR_RADIX_BITS)
KEY_BIG = float(256 ** (FLOOR_DIGITS + 1))
GATE = float(MULT_CAP + 1)

STAGES = [
    ("in", "input features, set by the encoder"),
    ("card", "per-card features: effective rank, suit matches, chip value parts"),
    (
        "fetch",
        "attention heads P1-P5 fetch the features of the candidate's k-th card, T sums Steel",
    ),
    ("pair", "rank equalities, rank gaps, suit counts, held Steel count"),
    (
        "shape",
        "rank structure of the played cards, flush, straight, scoring-card hints",
    ),
    ("type", "one-hot hand type"),
    ("score", "which played cards score, level chips and mult"),
    (
        "chips",
        "chips of the scoring cards, total chips, chips split into base-16 digits",
    ),
    ("fold", "mult folded card by card in played order, a pass per Red Seal repeat"),
    ("steel", "held Steel: mult times 1.5 once per held Steel pass"),
    ("product", "chips times mult, one gated copy of mult per chips digit step"),
    ("floor", "floor of the product by base-256 digit extraction, then the total"),
    (
        "select",
        "key = 256 * total - idx; attention B reads the winner; selector copies it",
    ),
]


class Builder:
    def __init__(self) -> None:
        self.names = []
        self.index = {}
        self.rows = []
        self.ready = {}
        self.units = {}
        self.unit_ready = []
        self.W1 = {}
        self.b1 = {}
        self.W2 = {}

    def dim(
        self, name: str, stage: str, meaning: str, ready: int | None = None
    ) -> None:
        self.index[name] = len(self.names)
        self.names.append(name)
        self.rows.append([name, stage, meaning])
        if ready is not None:
            self.ready[name] = ready

    def unit(self, expr: Expr, bias: float = 0.0) -> int:
        key = (tuple(sorted(expr.items())), bias)
        if key not in self.units:
            h = len(self.units)
            self.units[key] = h
            self.unit_ready.append(max(self.ready[name] for name in expr))
            for name, coef in expr.items():
                self.W1[h, self.index[name]] = coef
            if bias:
                self.b1[h] = bias
        return self.units[key]

    def add(self, name: str, h: int, coef: float) -> None:
        cell = (self.index[name], h)
        self.W2[cell] = self.W2.get(cell, 0.0) + coef

    # ReLU(own) - ReLU(-own) is the exact copy; a dim that is never negative needs only the first
    def reset(self, name: str, signed: bool = False) -> None:
        self.add(name, self.unit({name: 1.0}), -1.0)
        if signed:
            self.add(name, self.unit({name: -1.0}), 1.0)

    def define(
        self,
        name: str,
        stage: str,
        meaning: str,
        terms: list[tuple[int, float]],
        signed: bool = False,
    ) -> None:
        self.dim(name, stage, meaning)
        self.ready[name] = 1 + max(self.unit_ready[h] for h, _ in terms)
        self.reset(name, signed)
        for h, coef in terms:
            self.add(name, h, coef)

    def fresh(self, name: str, stage: str, meaning: str, ready: int) -> None:
        self.dim(name, stage, meaning, ready)
        self.reset(name)


def scaled(expr: Expr, factor: float) -> Expr:
    return {name: factor * coef for name, coef in expr.items()}


def merged(*exprs: Expr) -> Expr:
    total = {}
    for expr in exprs:
        for name, coef in expr.items():
            total[name] = total.get(name, 0.0) + coef
    return total


def total_of(names: list[str], factor: float = 1.0) -> Expr:
    return {name: factor for name in names}


# [v >= a] for an integer multiple of 1 / scale: two ReLUs of slope scale
def at_least(
    b: Builder, expr: Expr, a: float, scale: float = 1.0
) -> list[tuple[int, float]]:
    steep = scaled(expr, scale)
    return [(b.unit(steep, 1.0 - scale * a), 1.0), (b.unit(steep, -scale * a), -1.0)]


# [v = a] for an integer v
def equals(b: Builder, expr: Expr, a: float) -> list[tuple[int, float]]:
    return [
        (b.unit(expr, 1.0 - a), 1.0),
        (b.unit(expr, -a), -2.0),
        (b.unit(expr, -a - 1.0), 1.0),
    ]


def weighted(terms: list[tuple[int, float]], factor: float) -> list[tuple[int, float]]:
    return [(h, factor * coef) for h, coef in terms]


def declare_inputs(b: Builder) -> None:
    b.dim("one", "in", "constant 1", 0)
    b.dim("isCard", "in", "1 on slot tokens that hold a card", 0)
    b.dim("isNull", "in", "1 on null padding tokens", 0)
    b.dim("isCand", "in", "1 on candidate tokens", 0)
    b.dim("isSel", "in", "1 on the selector token", 0)
    for s in range(SLOTS):
        b.dim(
            f"slot_{s}",
            "in",
            f"1 on the token of slot {s} (slots 8-12 are null slots)",
            0,
        )
    for r in RANKS:
        b.dim(f"rank_{r}", "in", f"1 on a card of rank id {r}", 0)
    for s in SUITS:
        b.dim(f"suit_{s}", "in", f"1 on a card of suit {s}", 0)
    b.dim("nominal", "in", "chip value of the card rank", 0)
    for e in ENHANCEMENTS:
        b.dim(f"enh_{e}", "in", f"1 on a card with the {e} enhancement", 0)
    for e in EDITIONS:
        b.dim(f"ed_{e}", "in", f"1 on a card with the {e} edition", 0)
    for e in SEALS:
        b.dim(f"seal_{e}", "in", f"1 on a card with the {e} seal", 0)
    for k in range(1, POSITIONS + 1):
        for s in range(SLOTS):
            b.dim(
                f"m{k}_{s}",
                "in",
                f"1 on a candidate whose card number {k} (by position) is slot {s}",
                0,
            )
    b.dim(
        "idx",
        "in",
        "candidate position in the subset enumeration (size, then lexicographic)",
        0,
    )
    for t in TYPES:
        b.dim(f"C_{t}", "in", f"level chips of {t}, on candidates", 0)
    for t in TYPES:
        b.dim(f"M_{t}", "in", f"level mult of {t}, on candidates", 0)


def stage_card(b: Builder) -> None:
    rank_id = {f"rank_{r}": float(r) for r in RANKS}
    b.define(
        "rr",
        "card",
        "rank id of the card, 0 for a Stone",
        [(b.unit({**rank_id, "enh_Stone": -14.0}), 1.0)],
    )
    b.define(
        "rkA",
        "card",
        "1 on a non-Stone Ace",
        [(b.unit({"rank_14": 1.0, "enh_Stone": -1.0}), 1.0)],
    )
    b.define(
        "nomNS",
        "card",
        "nominal of the card, 0 for a Stone",
        [(b.unit({"nominal": 1.0, "enh_Stone": -11.0}), 1.0)],
    )
    b.define(
        "steelRed",
        "card",
        "1 on a Steel card with a Red Seal",
        [(b.unit({"enh_Steel": 1.0, "seal_Red": 1.0}, -1.0), 1.0)],
    )
    for s in SUITS:
        own = b.unit({f"suit_{s}": 1.0, "enh_Stone": -1.0})
        wild = b.unit({"enh_Wild": 1.0, f"suit_{s}": -1.0})
        b.define(
            f"sm_{s}",
            "card",
            f"1 when the card matches suit {s}: its suit or Wild, never Stone",
            [(own, 1.0), (wild, 1.0)],
        )


def fetched(k: int) -> dict[str, tuple[Expr, str]]:
    sentinel = {"enh_Stone": float(SENTINEL * k), "isNull": float(SENTINEL * k)}
    values = {
        "rs": (
            {"rr": 1.0, **sentinel},
            "rank id; a distinct large value for Stone and null",
        ),
        "rl": (
            {"rr": 1.0, "rkA": -13.0, **sentinel},
            "rank id with the Ace low; a distinct large value for Stone and null",
        ),
        "rr": ({"rr": 1.0}, "rank id, 0 for Stone and null"),
        "cv": (
            {"nomNS": 1.0, "enh_Stone": 50.0, "enh_Bonus": 30.0, "ed_Foil": 50.0},
            "chips of one scoring pass",
        ),
        "a": ({"enh_Mult": 1.0}, "1 for the Mult enhancement"),
        "gl": ({"enh_Glass": 1.0}, "1 for the Glass enhancement"),
        "ho": ({"ed_Holo": 1.0}, "1 for the Holo edition"),
        "po": ({"ed_Polychrome": 1.0}, "1 for the Polychrome edition"),
        "red": ({"seal_Red": 1.0}, "1 for the Red Seal"),
        "st": ({"enh_Stone": 1.0}, "1 for the Stone enhancement"),
        "ac": ({"rkA": 1.0}, "1 for a non-Stone Ace"),
        "ns": ({"isCard": 1.0, "enh_Stone": -1.0}, "1 for a card that is not a Stone"),
        "sw": (
            {"enh_Steel": 1.0, "steelRed": 1.0},
            "held Steel passes: 1, or 2 with a Red Seal",
        ),
    }
    for s in SUITS:
        values[f"sm_{s}"] = ({f"sm_{s}": 1.0}, f"1 when the card matches suit {s}")
    return values


def head(
    b: Builder, name: str, wq: dict, wk: dict, values: list[tuple[str, Expr]]
) -> dict:
    ix = b.index
    return {
        "name": name,
        "Wq": {(row, ix[dim]): coef for (row, dim), coef in wq.items()},
        "Wk": {(row, ix[dim]): coef for (row, dim), coef in wk.items()},
        "Wv": {
            (v, ix[dim]): coef
            for v, (_, expr) in enumerate(values)
            for dim, coef in expr.items()
        },
        "Wo": {(ix[fresh], v): 1.0 for v, (fresh, _) in enumerate(values)},
    }


def position_heads(b: Builder) -> list[dict]:
    heads = []
    for k in range(1, POSITIONS + 1):
        values = []
        for v, (expr, meaning) in fetched(k).items():
            ready = max(b.ready[dim] for dim in expr)
            b.fresh(
                f"P{k}_{v}",
                "fetch",
                f"fresh: {meaning}, of the candidate's card number {k}",
                ready,
            )
            values.append((f"P{k}_{v}", expr))
        wq = {(s, f"m{k}_{s}"): BETA for s in range(SLOTS)}
        wk = {(s, f"slot_{s}"): 1.0 for s in range(SLOTS)}
        heads.append(head(b, f"position{k}", wq, wk, values))
    return heads


def steel_head(b: Builder) -> dict:
    expr = {"enh_Steel": 1.0, "steelRed": 1.0}
    b.fresh(
        "T_sw",
        "fetch",
        "fresh: held Steel passes of all cards, divided by 8",
        max(b.ready[d] for d in expr),
    )
    wk = {(0, f"slot_{s}"): 1.0 for s in range(CARD_SLOTS)}
    return head(b, "steel", {(0, "isCand"): BETA}, wk, [("T_sw", expr)])


def stage_pairs(b: Builder) -> None:
    for k, m in PAIRS:
        gap = {f"P{k}_rs": 1.0, f"P{m}_rs": -1.0}
        b.define(
            f"e_{k}_{m}",
            "pair",
            f"1 when cards {k} and {m} have the same rank",
            equals(b, gap, 0.0),
        )
        for tag, key in (("H", "rs"), ("L", "rl")):
            gap = {f"P{k}_{key}": 1.0, f"P{m}_{key}": -1.0}
            far = at_least(b, gap, 5.0) + at_least(b, scaled(gap, -1.0), 5.0)
            b.define(
                f"far{tag}_{k}_{m}",
                "pair",
                f"1 when cards {k} and {m} are 5 or more ranks apart ({tag})",
                far,
            )
    for k, m in ORDERED:
        gap = {f"P{m}_rr": 1.0, f"P{k}_rr": -1.0}
        b.define(
            f"gt_{k}_{m}",
            "pair",
            f"1 when card {m} outranks card {k}",
            at_least(b, gap, 1.0),
        )
    for s in SUITS:
        count = total_of([f"P{k}_sm_{s}" for k in range(1, POSITIONS + 1)])
        b.define(
            f"fl_{s}",
            "pair",
            f"1 when all five cards match suit {s}",
            [(b.unit(count, -4.0), 1.0)],
        )
    held = merged(
        {"T_sw": 8.0}, total_of([f"P{k}_sw" for k in range(1, POSITIONS + 1)], -1.0)
    )
    for i in range(WMAX + 1):
        b.define(
            f"hs_{i}",
            "pair",
            f"1 when exactly {i} held Steel passes happen",
            equals(b, held, float(i)),
        )


def stage_shape(b: Builder) -> None:
    same = total_of([f"e_{k}_{m}" for k, m in PAIRS])
    for name, count in (
        ("five", 10),
        ("four", 6),
        ("fh", 4),
        ("three", 3),
        ("tp", 2),
        ("pair", 1),
    ):
        b.define(
            name,
            "shape",
            f"1 when the rank equalities total {count}",
            equals(b, same, float(count)),
        )
    flushes = total_of([f"fl_{s}" for s in SUITS])
    b.define(
        "flush",
        "shape",
        "1 when all five cards share a suit",
        at_least(b, flushes, 1.0),
    )
    straight = []
    for tag in ("H", "L"):
        apart = total_of([f"far{tag}_{k}_{m}" for k, m in PAIRS])
        aces = total_of([f"P{k}_ac" for k in range(1, POSITIONS + 1)])
        # an Ace-low run needs an Ace; 10 * same rules out two Aces
        start = {"H": ({}, 1.0), "L": (aces, 0.0)}[tag]
        expr = merged(start[0], scaled(same, -10.0), scaled(apart, -1.0))
        straight.append((b.unit(expr, start[1]), 1.0))
    b.define(
        "straight",
        "shape",
        "1 when five different ranks run in a row, Ace high or low",
        straight,
    )
    for k in range(1, POSITIONS + 1):
        mates = total_of(
            [f"e_{min(k, m)}_{max(k, m)}" for m in range(1, POSITIONS + 1) if m != k]
        )
        b.define(
            f"pl_{k}",
            "shape",
            f"1 when card {k} shares its rank with another card",
            at_least(b, mates, 1.0),
        )
        beaten = total_of(
            [f"gt_{k}_{m}" for m in range(1, POSITIONS + 1) if m != k], -1.0
        )
        ranked = {f"P{k}_ns": 1.0, **beaten}
        b.define(
            f"top_{k}",
            "shape",
            f"1 when card {k} has a rank and no card outranks it",
            [(b.unit(ranked), 1.0)],
        )


def stage_type(b: Builder) -> None:
    def unit(expr: Expr, bias: float = 0.0) -> list[tuple[int, float]]:
        return [(b.unit(expr, bias), 1.0)]

    rank = ["five", "four", "fh", "three", "tp", "pair"]
    one_hot = {
        "FlushFive": unit({"five": 1.0, "flush": 1.0}, -1.0),
        "FlushHouse": unit({"fh": 1.0, "flush": 1.0}, -1.0),
        "FiveOfAKind": unit({"five": 1.0, "flush": -1.0}),
        "StraightFlush": unit({"straight": 1.0, "flush": 1.0}, -1.0),
        "FourOfAKind": unit({"four": 1.0}),
        "FullHouse": unit({"fh": 1.0, "flush": -1.0}),
        "Flush": unit(
            {"flush": 1.0, "five": -1.0, "four": -1.0, "fh": -1.0, "straight": -1.0}
        ),
        "Straight": unit({"straight": 1.0, "flush": -1.0}),
        "ThreeOfAKind": unit({"three": 1.0, "flush": -1.0}),
        "TwoPair": unit({"tp": 1.0, "flush": -1.0}),
        "Pair": unit({"pair": 1.0, "flush": -1.0}),
        "HighCard": unit(total_of(rank + ["flush", "straight"], -1.0), 1.0),
    }
    for t in TYPES:
        b.define(f"t_{t}", "type", f"1 when the hand type is {t}", one_hot[t])


def stage_score(b: Builder) -> None:
    whole = [
        "FlushFive",
        "FlushHouse",
        "FiveOfAKind",
        "StraightFlush",
        "FullHouse",
        "Flush",
        "Straight",
    ]
    for k in range(1, POSITIONS + 1):
        joined = merged(
            {f"P{k}_st": 1.0, f"pl_{k}": 1.0}, total_of([f"t_{t}" for t in whole])
        )
        terms = [
            (b.unit(joined), 1.0),
            (b.unit(joined, -1.0), -1.0),
            (b.unit({f"top_{k}": 1.0, "t_HighCard": 1.0}, -1.0), 1.0),
        ]
        b.define(
            f"sc_{k}",
            "score",
            f"1 when card {k} scores: a Stone, a rank mate, in a five-card type, or the High Card",
            terms,
        )
    for k in range(1, POSITIONS + 1):
        b.define(
            f"sr_{k}",
            "score",
            f"1 when card {k} scores and has a Red Seal",
            [(b.unit({f"sc_{k}": 1.0, f"P{k}_red": 1.0}, -1.0), 1.0)],
        )
    base = [
        (b.unit({f"C_{t}": 1.0, f"t_{t}": float(CMAX + 1)}, -float(CMAX + 1)), 1.0)
        for t in TYPES
    ]
    b.define("chipsBase", "score", "level chips of the hand type", base)
    mult = [(b.unit({f"M_{t}": 1.0, f"t_{t}": GATE}, -GATE), 1.0) for t in TYPES]
    b.define("mult0", "score", "level mult of the hand type", mult)


def stage_chips(b: Builder) -> None:
    cap = float(CARD_CHIPS_CAP + 28)
    terms = []
    for k in range(1, POSITIONS + 1):
        cv = f"P{k}_cv"
        terms.append((b.unit({cv: 1.0, f"sc_{k}": cap}, -cap), 1.0))
        terms.append((b.unit({cv: 1.0, f"sr_{k}": cap}, -cap), 1.0))
    b.define(
        "chipsCards",
        "chips",
        "chips of the scoring cards, once, and again with a Red Seal",
        terms,
    )
    b.define(
        "chips",
        "chips",
        "level chips plus the chips of the scoring cards",
        [(b.unit({"chipsBase": 1.0}), 1.0), (b.unit({"chipsCards": 1.0}), 1.0)],
    )


def stage_digits(b: Builder) -> None:
    radix = 2**CHIP_RADIX_BITS
    digits = math.ceil(CHIPS_CAP.bit_length() / CHIP_RADIX_BITS)
    rest = "chips"
    for j in reversed(range(digits)):
        base = radix**j
        steps = []
        for t in range(1, min(radix - 1, CHIPS_CAP // base) + 1):
            up = at_least(b, {rest: 1.0}, float(t * base))
            b.define(
                f"th_{j}_{t}",
                "chips",
                f"1 when the chips left reach {t} times {base}",
                up,
            )
            steps += weighted(up, -float(base))
        if j > 0:
            rest_next = f"cr_{j - 1}"
            b.define(
                rest_next,
                "chips",
                f"chips left after the base-{radix} digits above {j - 1}",
                [(b.unit({rest: 1.0}), 1.0)] + steps,
                True,
            )
            rest = rest_next


def fold_pass(
    b: Builder, name: str, previous: str, gate: str, k: int, meaning: str
) -> None:
    a, gl, ho, po = (f"P{k}_{x}" for x in ("a", "gl", "ho", "po"))
    terms = [
        (b.unit({previous: 1.0}), 1.0),
        (b.unit({a: 1.0, gate: 1.0}, -1.0), 4.0),
        (b.unit({previous: 1.0, gl: GATE, gate: GATE}, -2 * GATE), 1.0),
        (b.unit({previous: 0.5, po: GATE, gate: GATE}, -2 * GATE), 1.0),
        (b.unit({a: 1.0, po: 1.0, gate: 1.0}, -2.0), 2.0),
        (b.unit({previous: 0.5, gl: GATE, po: GATE, gate: GATE}, -3 * GATE), 1.0),
        (b.unit({ho: 1.0, gate: 1.0}, -1.0), 10.0),
    ]
    b.define(name, "fold", meaning, terms)


def stage_fold(b: Builder) -> str:
    previous = "mult0"
    for k in range(1, POSITIONS + 1):
        for repeat, gate in ((1, f"sc_{k}"), (2, f"sr_{k}")):
            name = f"mult_{k}_{repeat}"
            meaning = f"mult after card {k}, pass {repeat}: +4 Mult, x2 Glass, +10 Holo, x1.5 Polychrome"
            fold_pass(b, name, previous, gate, k, meaning)
            previous = name
    return previous


def stage_steel(b: Builder, folded: str) -> None:
    terms = []
    for i in range(WMAX + 1):
        gate = 1.5**i * MULT_CAP + 1.0
        terms.append((b.unit({folded: 1.5**i, f"hs_{i}": gate}, -gate), 1.0))
    b.define("mult", "steel", "mult after the played cards and the held Steel", terms)


def stage_product(b: Builder) -> None:
    radix = 2**CHIP_RADIX_BITS
    terms = []
    for name in b.names:
        if name.startswith("th_"):
            _, j, _ = name.split("_")
            base = float(radix ** int(j))
            gate = base * MULT_CAP + 1.0
            terms.append((b.unit({"mult": base, name: gate}, -gate), 1.0))
    b.define(
        "prod",
        "product",
        "chips times mult: one copy of mult per chips digit step",
        terms,
    )


def stage_floor(b: Builder) -> None:
    radix = 2**FLOOR_RADIX_BITS
    rest = "prod"
    digits = []
    for j in reversed(range(FLOOR_DIGITS)):
        base = radix**j
        steps = []
        for t in range(1, min(radix - 1, PRODUCT_CAP // base) + 1):
            steps += at_least(b, {rest: 1.0}, float(t * base), RESOLUTION)
        b.define(
            f"fd_{j}",
            "floor",
            f"base-{radix} digit {j} of the integer part of the product",
            steps,
        )
        digits.append((f"fd_{j}", float(base)))
        if j > 0:
            rest_next = f"fr_{j - 1}"
            left = [
                (b.unit({rest: 1.0}), 1.0),
                (b.unit({f"fd_{j}": 1.0}), -float(base)),
            ]
            b.define(
                rest_next,
                "floor",
                f"product left after the digits above {j - 1}",
                left,
                True,
            )
            rest = rest_next
    b.define(
        "total",
        "floor",
        "floor of chips times mult",
        [(b.unit({name: 1.0}), base) for name, base in digits],
    )


def stage_key(b: Builder) -> None:
    b.define(
        "K",
        "select",
        "selection key: 256 * total - idx on candidates, 0 elsewhere",
        [(b.unit({"total": 256.0, "idx": -1.0, "isCand": KEY_BIG}, -KEY_BIG), 1.0)],
    )


def selection_head(b: Builder) -> dict:
    values = [
        (f"Bf_slot_{s}", total_of([f"m{k}_{s}" for k in range(1, POSITIONS + 1)]))
        for s in range(CARD_SLOTS)
    ]
    values += [("Bf_total", {"total": 1.0})]
    values += [(f"Bf_t_{t}", {f"t_{t}": 1.0}) for t in TYPES]
    meanings = {
        **{
            f"Bf_slot_{s}": f"fresh: selector's read of card slot {s} of the winner"
            for s in range(CARD_SLOTS)
        },
        "Bf_total": "fresh: selector's read of the winner's total",
        **{f"Bf_t_{t}": f"fresh: selector's read of the winner's t_{t}" for t in TYPES},
    }
    for name, expr in values:
        b.fresh(name, "select", meanings[name], max(b.ready[d] for d in [*expr, "K"]))
    head_b = head(b, "selection", {(0, "isSel"): BETA}, {(0, "K"): 1.0}, values)
    for name, _ in values:
        b.define(
            name.replace("Bf_", "sel_"),
            "select",
            f"on the selector: {meanings[name][len('fresh: selector') :]}",
            [(b.unit({name: 1.0}), 1.0)],
        )
    return head_b


def check_bounds(b: Builder) -> list[tuple[str, int, int]]:
    top = max(GATE, 1.5**WMAX * MULT_CAP + 1.0)
    rows = [
        ("mult in every fold and steel stage", MULT_CAP, FRAC),
        ("gated pre-activation in a fold stage", 4 * int(GATE), FRAC),
        ("gated pre-activation in the steel stage", 2 * int(top), FRAC),
        ("chips", CHIPS_CAP, 0),
        ("chips times mult", PRODUCT_CAP, FRAC),
        (
            "gated pre-activation in the product stage",
            2 * int(2 ** (CHIP_RADIX_BITS * 3) * MULT_CAP + 1),
            FRAC,
        ),
        (
            "floor step pre-activation, in units of 2^-8",
            int(RESOLUTION) * PRODUCT_CAP,
            0,
        ),
        (
            "chips digit step times its weight",
            2 ** (CHIP_RADIX_BITS * 3) * (CHIPS_CAP + 1),
            0,
        ),
        ("floor digit times its weight", 256**FLOOR_DIGITS, 0),
        ("selection key", 256 * PRODUCT_CAP, 0),
        ("attention score of the selector", int(BETA) * 256 * PRODUCT_CAP, 0),
    ]
    for label, peak, bits in rows:
        assert peak * 2**bits <= 2**52, label
    assert PRODUCT_CAP < 256**FLOOR_DIGITS
    return rows


def triplets(matrix: dict[tuple[int, int], float]) -> list[list]:
    return [[r, c, v] for (r, c), v in sorted(matrix.items()) if v != 0.0]


def pairs(vector: dict[int, float]) -> list[list]:
    return [[i, v] for i, v in sorted(vector.items()) if v != 0.0]


def readme(
    b: Builder, data: dict, nonzeros: int, bounds: list[tuple[str, int, int]]
) -> str:
    lines = [
        "# PlayNet",
        "",
        "Generated by `nn/build.py`; do not edit.",
        "",
        "One transformer block looped `loops` times. Every weight is set by the builder from a stated",
        "meaning; nothing is trained. It picks the play `Search.plays` would pick for a hand of 1-8 cards",
        "with any enhancement, edition and seal, no jokers, no boss, and reports its total and hand type.",
        "Every dim is a pure function of the dims and fetches of the loop before, so after `loops` loops",
        "all of them hold their final value. Attention weights are exactly 0, 1, or 1/8, and every",
        "stage is exact float64 arithmetic on integers and multiples of 2^-8.",
        "",
        f"- residual width d = {data['d']}, hidden units = {data['hidden']}, loops = {data['loops']}",
        f"- nonzero weights (matrices and biases) = {nonzeros}",
        f"- hands accepted: level mult 1..{MMAX}, level chips 0..{CMAX}, Polychrome plus Steel passes at most {WMAX}, Glass passes at most {GMAX} (a Red Seal doubles a pass)",
        "- computed dims on non-candidate tokens hold values nobody reads",
        "",
        "## Stages",
        "",
        "| Stage | Meaning |",
        "|---|---|",
    ]
    lines += [f"| {s} | {m} |" for s, m in STAGES]
    lines += [
        "",
        "## Precision bounds",
        "",
        "Worst case under the limits above; each is checked by the builder to fit exactly in float64",
        "(peak times 2^bits is at most 2^52).",
        "",
        "| Quantity | Peak | Fraction bits |",
        "|---|---|---|",
    ]
    lines += [f"| {label} | {peak} | {bits} |" for label, peak, bits in bounds]
    lines += [
        "",
        "## Dims",
        "",
        "| Index | Name | Stage | Loop | Meaning |",
        "|---|---|---|---|---|",
    ]
    lines += [
        f"| {i} | {n} | {s} | {b.ready[n]} | {m} |"
        for i, (n, s, m) in enumerate(b.rows)
    ]
    return "\n".join(lines) + "\n"


def build() -> None:
    b = Builder()
    declare_inputs(b)
    stage_card(b)
    heads = position_heads(b) + [steel_head(b)]
    stage_pairs(b)
    stage_shape(b)
    stage_type(b)
    stage_score(b)
    stage_chips(b)
    stage_digits(b)
    stage_steel(b, stage_fold(b))
    stage_product(b)
    stage_floor(b)
    stage_key(b)
    heads.append(selection_head(b))
    bounds = check_bounds(b)

    loops = max(b.ready[name] for name in b.names if name.startswith("sel_"))
    data = {
        "d": len(b.names),
        "hidden": len(b.units),
        "loops": loops,
        "limits": {"mult": MMAX, "chips": CMAX, "weight": WMAX, "glass": GMAX},
        "dims": b.index,
        "heads": [
            {"name": h["name"], **{k: triplets(h[k]) for k in ("Wq", "Wk", "Wv", "Wo")}}
            for h in heads
        ],
        "W1": triplets(b.W1),
        "b1": pairs(b.b1),
        "W2": triplets(b.W2),
    }
    nonzeros = sum(len(h[k]) for h in data["heads"] for k in ("Wq", "Wk", "Wv", "Wo"))
    nonzeros += sum(len(data[k]) for k in ("W1", "b1", "W2"))

    root = Path(__file__).parent
    (root / "playnet.json").write_text(json.dumps(data, separators=(",", ":")) + "\n")
    (root / "README.md").write_text(readme(b, data, nonzeros, bounds))
    print(
        f"d={data['d']} hidden={data['hidden']} loops={loops} heads={len(heads)} nonzeros={nonzeros}"
    )
    for label, peak, bits in bounds:
        print(f"bound {label}: {peak} * 2^{bits}")


build()
