import json
from pathlib import Path

Expr = dict[str, float]

BETA = 40.0
MMAX = 200
GATE = 10000.0
BIG = 1e9
CHIP_LOOPS = 3

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
NOMINAL = {r: min(r, 10) if r < 14 else 11 for r in RANKS}
SUITS = ["S", "H", "C", "D"]
SLOTS = 13
CARD_SLOTS = 8
ALL_FIVE = [
    "FlushFive",
    "FlushHouse",
    "FiveOfAKind",
    "StraightFlush",
    "FullHouse",
    "Flush",
    "Straight",
]
WINDOWS = [[14, 2, 3, 4, 5]] + [list(range(s, s + 5)) for s in range(2, 11)]

STAGES = [
    ("in", "-", "input features, set by the encoder"),
    (
        "S0",
        "attention A",
        "membership head: each candidate averages the five slot tokens it names",
    ),
    (
        "S1",
        "MLP of loop 1",
        "rank counts to step values, suit counts to flush, nominal sum",
    ),
    ("S2", "MLP of loop 2", "hand-type conditions and the highest rank present"),
    ("S3", "MLP of loop 3", "one-hot hand type, chips and mult (design S3-S5 merged)"),
    (
        "S6",
        "MLP of loop 4 on",
        "multiplier loop: g marks the loops 1..mult, acc adds chips on each",
    ),
    (
        "S7",
        "MLP and attention B",
        "selection key K, selector reads the winner's slots, acc and type",
    ),
]


class Builder:
    def __init__(self) -> None:
        self.names = []
        self.index = {}
        self.rows = []
        self.units = {}
        self.W1 = {}
        self.b1 = {}
        self.W2 = {}
        self.b2 = {}

    def dim(self, name: str, stage: str, meaning: str) -> None:
        self.index[name] = len(self.names)
        self.names.append(name)
        self.rows.append((name, stage, meaning))

    def unit(self, expr: Expr, bias: float = 0.0) -> int:
        key = (tuple(sorted(expr.items())), bias)
        if key not in self.units:
            h = len(self.units)
            self.units[key] = h
            for name, coef in expr.items():
                self.W1[h, self.index[name]] = coef
            if bias:
                self.b1[h] = bias
        return self.units[key]

    def add(self, name: str, h: int, coef: float) -> None:
        cell = (self.index[name], h)
        self.W2[cell] = self.W2.get(cell, 0.0) + coef

    # every stored dim is non-negative, so ReLU(own value) is its exact copy
    def update(self, name: str, terms: list[tuple[int, float]]) -> None:
        self.reset(name)
        for h, coef in terms:
            self.add(name, h, coef)

    def reset(self, name: str) -> None:
        self.add(name, self.unit({name: 1.0}), -1.0)


def sum_of(names: list[str], scale: float = 1.0) -> Expr:
    return {name: scale for name in names}


def combo(*parts: tuple[Expr, float]) -> Expr:
    total = {}
    for expr, scale in parts:
        for name, coef in expr.items():
            total[name] = total.get(name, 0.0) + scale * coef
    return total


def declare_inputs(b: Builder) -> None:
    b.dim("one", "in", "constant 1")
    b.dim("j", "in", "loop counter, starts at jStart and grows by 1 per loop")
    b.dim("isCard", "in", "1 on card tokens")
    b.dim("isNull", "in", "1 on null padding tokens")
    b.dim("isCand", "in", "1 on candidate tokens")
    b.dim("isSel", "in", "1 on the selector token")
    for s in range(SLOTS):
        b.dim(
            f"slot_{s}",
            "in",
            f"1 on the token that holds slot {s} (slots 8-12 are null slots)",
        )
    for r in RANKS:
        b.dim(f"rank_{r}", "in", f"1 on a card of rank id {r}")
    for s in SUITS:
        b.dim(f"suit_{s}", "in", f"1 on a card of suit {s}")
    b.dim("nominal", "in", "chip value of the card rank")
    for s in range(SLOTS):
        b.dim(
            f"member_{s}",
            "in",
            f"1 on a candidate that includes slot {s}; exactly 5 bits set",
        )
    b.dim(
        "idx",
        "in",
        "candidate position in the subset enumeration (size, then lexicographic)",
    )
    for t in TYPES:
        b.dim(f"C_{t}", "in", f"level chips of {t}, on candidates")
    for t in TYPES:
        b.dim(f"M_{t}", "in", f"level mult of {t}, on candidates")


def declare_computed(b: Builder) -> None:
    for r in RANKS:
        b.dim(
            f"A_rank_{r}",
            "S0",
            f"fresh: share of the candidate's 5 slots holding rank id {r}",
        )
    for s in SUITS:
        b.dim(
            f"A_suit_{s}",
            "S0",
            f"fresh: share of the candidate's 5 slots holding suit {s}",
        )
    b.dim("A_nom", "S0", "fresh: mean nominal over the candidate's 5 slots")
    for r in RANKS:
        for k in range(2, 6):
            b.dim(
                f"eq_{r}_{k}", "S1", f"1 when exactly {k} played cards have rank id {r}"
            )
    for r in RANKS:
        b.dim(f"ge1_{r}", "S1", f"1 when rank id {r} is played")
    b.dim("flush", "S1", "1 when all five played cards share a suit")
    b.dim("nomSum", "S1", "sum of the nominals of the played cards")
    for t in TYPES[:-1]:
        b.dim(f"cond_{t}", "S2", f"1 when the played cards contain {t}")
    for r in RANKS:
        b.dim(f"top_{r}", "S2", f"1 when rank id {r} is the highest rank played")
    b.dim("chips", "S3", "level chips plus the chips of the scoring cards")
    b.dim("mult", "S3", "level mult")
    for t in TYPES:
        b.dim(f"t_{t}", "S3", f"1 when the hand type is {t} (one-hot, best type first)")
    b.dim("g", "S6", "1 in the loops where j runs from 1 to mult")
    b.dim("acc", "S6", "chips added once per loop with g = 1, so chips * mult")
    b.dim("K", "S7", "selection key: 256 * acc - idx on candidates, 0 elsewhere")
    for s in range(CARD_SLOTS):
        b.dim(
            f"Bf_slot_{s}",
            "S7",
            f"fresh: selector's read of card slot {s} of the winner",
        )
    b.dim("Bf_acc", "S7", "fresh: selector's read of the winner's acc")
    for t in TYPES:
        b.dim(f"Bf_t_{t}", "S7", f"fresh: selector's read of the winner's t_{t}")
    for s in range(CARD_SLOTS):
        b.dim(
            f"sel_slot_{s}",
            "S7",
            f"on the selector: 1 when card slot {s} is in the winning play",
        )
    b.dim("sel_acc", "S7", "on the selector: total of the winning play")
    for t in TYPES:
        b.dim(f"sel_t_{t}", "S7", f"on the selector: 1 when the winning play is {t}")


# c = 5 * A is the count 0..5 of a rank; u_j = ReLU(c - j) and [c >= k] = u_(k-1) - u_k
def stage_counts(b: Builder) -> None:
    for r in RANKS:
        u = [b.unit({f"A_rank_{r}": 5.0}, -float(j)) for j in range(6)]
        b.update(f"ge1_{r}", [(u[0], 1.0), (u[1], -1.0)])
        for k in range(2, 5):
            b.update(f"eq_{r}_{k}", [(u[k - 1], 1.0), (u[k], -2.0), (u[k + 1], 1.0)])
        b.update(f"eq_{r}_5", [(u[4], 1.0), (u[5], -1.0)])
        b.reset(f"A_rank_{r}")
    flush = [b.unit({f"A_suit_{s}": 5.0}, -4.0) for s in SUITS]
    b.update("flush", [(h, 1.0) for h in flush])
    for s in SUITS:
        b.reset(f"A_suit_{s}")
    b.update("nomSum", [(b.unit({"A_nom": 5.0}), 1.0)])
    b.reset("A_nom")


def stage_conditions(b: Builder) -> None:
    has = {k: sum_of([f"eq_{r}_{k}" for r in RANKS]) for k in (2, 3, 4, 5)}
    flush = {"flush": 1.0}

    def window(w: list[int]) -> Expr:
        return sum_of([f"ge1_{r}" for r in w])

    # at most 5 cards: one triple, two pairs, so tested sums stay 0 or 1
    cond = {
        "FlushFive": [(b.unit(combo((has[5], 1), (flush, 1)), -1.0), 1.0)],
        "FlushHouse": [
            (b.unit(combo((has[3], 2), (has[2], 1), (flush, 2)), -4.0), 1.0)
        ],
        "FiveOfAKind": [(b.unit(has[5]), 1.0)],
        "StraightFlush": [
            (b.unit(combo((window(w), 1), (flush, 1)), -5.0), 1.0) for w in WINDOWS
        ],
        "FourOfAKind": [(b.unit(has[4]), 1.0)],
        "FullHouse": [(b.unit(combo((has[3], 2), (has[2], 1)), -2.0), 1.0)],
        "Flush": [(b.unit(flush), 1.0)],
        "Straight": [(b.unit(window(w), -4.0), 1.0) for w in WINDOWS],
        "ThreeOfAKind": [(b.unit(has[3]), 1.0)],
        "TwoPair": [(b.unit(has[2], -1.0), 1.0)],
        "Pair": [(b.unit(has[2]), 1.0), (b.unit(has[2], -1.0), -1.0)],
    }
    for t, terms in cond.items():
        b.update(f"cond_{t}", terms)
    for r in RANKS:
        higher = sum_of([f"ge1_{x}" for x in RANKS if x > r], -1.0)
        b.update(f"top_{r}", [(b.unit({f"ge1_{r}": 1.0, **higher}), 1.0)])


def card_chips(t: str) -> Expr:
    if t in ALL_FIVE:
        return {"nomSum": 1.0}
    if t == "FourOfAKind":
        return {f"eq_{r}_4": 4.0 * NOMINAL[r] for r in RANKS}
    if t == "ThreeOfAKind":
        return {f"eq_{r}_3": 3.0 * NOMINAL[r] for r in RANKS}
    if t in ("TwoPair", "Pair"):
        return {f"eq_{r}_2": 2.0 * NOMINAL[r] for r in RANKS}
    return {f"top_{r}": float(NOMINAL[r]) for r in RANKS}


# type T is on when cond_T = 1 and no earlier one is: ReLU(x - GATE * gap)
def stage_type(b: Builder) -> None:
    chips, mult = [], []
    for i, t in enumerate(TYPES):
        earlier = sum_of([f"cond_{e}" for e in TYPES[:i]], -GATE)
        own = {f"cond_{t}": GATE} if t != "HighCard" else {}
        bias = -GATE if t != "HighCard" else 0.0
        chips.append(
            (b.unit({f"C_{t}": 1.0, **card_chips(t), **own, **earlier}, bias), 1.0)
        )
        mult.append((b.unit({f"M_{t}": 1.0, **own, **earlier}, bias), 1.0))
        flag = sum_of([f"cond_{e}" for e in TYPES[:i]], -1.0)
        if t == "HighCard":
            b.update(f"t_{t}", [(b.unit(flag, 1.0), 1.0)])
        else:
            b.update(f"t_{t}", [(b.unit({f"cond_{t}": 1.0, **flag}), 1.0)])
    b.update("chips", chips)
    b.update("mult", mult)


# g = [j <= mult] - [j <= 0]; each loop adds chips when the previous g was 1
def stage_multiplier(b: Builder) -> None:
    steps = [
        b.unit({"mult": 1.0, "j": -1.0}, 1.0),
        b.unit({"mult": 1.0, "j": -1.0}),
        b.unit({"j": -1.0}, 1.0),
        b.unit({"j": -1.0}),
    ]
    b.update("g", list(zip(steps, [1.0, -1.0, -1.0, 1.0])))
    b.add("acc", b.unit({"chips": 1.0, "g": GATE}, -GATE), 1.0)
    b.b2[b.index["j"]] = 1.0


def stage_key(b: Builder) -> None:
    key = b.unit({"acc": 256.0, "idx": -1.0, "isCand": BIG}, -BIG)
    b.update("K", [(key, 1.0)])


def stage_selection(b: Builder) -> None:
    fresh = (
        [f"Bf_slot_{s}" for s in range(CARD_SLOTS)]
        + ["Bf_acc"]
        + [f"Bf_t_{t}" for t in TYPES]
    )
    for name in fresh:
        b.update(name.replace("Bf_", "sel_"), [(b.unit({name: 1.0}), 1.0)])
        b.reset(name)


def attention_heads(b: Builder) -> list[dict]:
    ix = b.index
    values = [f"rank_{r}" for r in RANKS] + [f"suit_{s}" for s in SUITS] + ["nominal"]
    outputs = (
        [f"A_rank_{r}" for r in RANKS] + [f"A_suit_{s}" for s in SUITS] + ["A_nom"]
    )
    membership = {
        "name": "membership",
        "Wq": {(s, ix[f"member_{s}"]): BETA for s in range(SLOTS)},
        "Wk": {(s, ix[f"slot_{s}"]): 1.0 for s in range(SLOTS)},
        "Wv": {(v, ix[name]): 1.0 for v, name in enumerate(values)},
        "Wo": {(ix[name], v): 1.0 for v, name in enumerate(outputs)},
    }

    values = (
        [f"member_{s}" for s in range(CARD_SLOTS)] + ["acc"] + [f"t_{t}" for t in TYPES]
    )
    outputs = (
        [f"Bf_slot_{s}" for s in range(CARD_SLOTS)]
        + ["Bf_acc"]
        + [f"Bf_t_{t}" for t in TYPES]
    )
    selection = {
        "name": "selection",
        "Wq": {(0, ix["isSel"]): BETA},
        "Wk": {(0, ix["K"]): 1.0},
        "Wv": {(v, ix[name]): 1.0 for v, name in enumerate(values)},
        "Wo": {(ix[name], v): 1.0 for v, name in enumerate(outputs)},
    }
    return [membership, selection]


def triplets(matrix: dict[tuple[int, int], float]) -> list[list]:
    return [[r, c, v] for (r, c), v in sorted(matrix.items()) if v != 0.0]


def pairs(vector: dict[int, float]) -> list[list]:
    return [[i, v] for i, v in sorted(vector.items()) if v != 0.0]


def readme(b: Builder, data: dict, nonzeros: int) -> str:
    lines = [
        "# PlayNet",
        "",
        "Generated by `nn/build.py`; do not edit.",
        "",
        "One transformer block looped `loops` times. Every weight is set by the builder from a stated",
        "meaning; nothing is trained. It picks the play `Search.plays` would pick for a hand of 1-8 plain",
        "cards, and reports its total and hand type.",
        "",
        f"- residual width d = {data['d']}, hidden units = {data['hidden']}, loops = {data['loops']}, jStart = {data['jStart']}",
        f"- nonzero weights (matrices and biases) = {nonzeros}",
        "",
        "## Stages",
        "",
        "| Stage | Where | Meaning |",
        "|---|---|---|",
    ]
    lines += [f"| {s} | {w} | {m} |" for s, w, m in STAGES]
    lines += [
        "",
        "## Dims",
        "",
        "| Index | Name | Stage | Meaning |",
        "|---|---|---|---|",
    ]
    lines += [f"| {i} | {n} | {s} | {m} |" for i, (n, s, m) in enumerate(b.rows)]
    return "\n".join(lines) + "\n"


def build() -> None:
    b = Builder()
    declare_inputs(b)
    declare_computed(b)
    stage_counts(b)
    stage_conditions(b)
    stage_type(b)
    stage_multiplier(b)
    stage_key(b)
    stage_selection(b)
    heads = attention_heads(b)

    data = {
        "d": len(b.names),
        "hidden": len(b.units),
        "loops": CHIP_LOOPS + MMAX + 3,
        "jStart": 1 - CHIP_LOOPS,
        "beta": BETA,
        "dims": b.index,
        "heads": [
            {"name": h["name"], **{k: triplets(h[k]) for k in ("Wq", "Wk", "Wv", "Wo")}}
            for h in heads
        ],
        "W1": triplets(b.W1),
        "b1": pairs(b.b1),
        "W2": triplets(b.W2),
        "b2": pairs(b.b2),
    }
    nonzeros = sum(len(h[k]) for h in data["heads"] for k in ("Wq", "Wk", "Wv", "Wo"))
    nonzeros += sum(len(data[k]) for k in ("W1", "b1", "W2", "b2"))

    root = Path(__file__).parent
    (root / "playnet.json").write_text(json.dumps(data, separators=(",", ":")) + "\n")
    (root / "README.md").write_text(readme(b, data, nonzeros))
    print(
        f"d={data['d']} hidden={data['hidden']} loops={data['loops']} jStart={data['jStart']} nonzeros={nonzeros}"
    )


build()
