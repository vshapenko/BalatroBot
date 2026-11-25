import json
import math
from pathlib import Path

Expr = dict[str, float]

BETA = 1024.0
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
JOKERS = 5
VALUE_MAX = 500
LEFT_MAX = 10
PLAYS_MAX = 100

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

# Edition and effect adds of one joker slot, and the largest factor of any joker
JOKER_ADD = VALUE_MAX + 10
JOKER_CHIPS = VALUE_MAX + 50
JOKER_FACTOR = 4
VALUE_GATE = float(VALUE_MAX + 1)
BANNER_GATE = float(30 * LEFT_MAX + 1)
RUN_GATE = float(PLAYS_MAX + 1)

# Worst cases under the limits: every add comes first, then every factor
ADDS = POSITIONS * 2 * (4 + 10)
MULT_CARDS_CAP = math.ceil((MMAX + ADDS) * 2**GMAX * 1.5**WMAX)


# A slot either adds or multiplies, so the integer part is the best of the two per slot
def joker_integer_cap() -> int:
    mult = MMAX + ADDS
    for _ in range(JOKERS):
        mult = max(mult + JOKER_ADD, JOKER_FACTOR * (mult + 10))
    return mult


MULT_CAP = math.ceil(joker_integer_cap() * 2**GMAX * 1.5**WMAX)
CARD_CHIPS_CAP = 100
CHIPS_CAP = CMAX + POSITIONS * 2 * CARD_CHIPS_CAP + JOKERS * JOKER_CHIPS
PRODUCT_CAP = CHIPS_CAP * MULT_CAP
FRAC = WMAX
RESOLUTION = float(2**FRAC)
CHIP_DIGITS = math.ceil(CHIPS_CAP.bit_length() / CHIP_RADIX_BITS)
FLOOR_DIGITS = math.ceil(PRODUCT_CAP.bit_length() / FLOOR_RADIX_BITS)
KEY_BIG = float(256 * (PRODUCT_CAP + 1))
GATE = float(MULT_CARDS_CAP + 1)
JOKER_GATE = float(JOKER_FACTOR * 1.5 * MULT_CAP + 1)

# Joker keys by effect: (condition dim or None, amount); {j} is the slot
ADD_CHIPS = {
    "j_stuntman": (None, 250),
    "j_sly": ("ct_Pair", 50),
    "j_wily": ("ct_ThreeOfAKind", 100),
    "j_clever": ("ct_TwoPair", 80),
    "j_devious": ("ct_Straight", 100),
    "j_crafty": ("ct_Flush", 80),
}
ADD_MULT = {
    "j_joker": (None, 4),
    "j_gros_michel": (None, 15),
    "j_jolly": ("ct_Pair", 8),
    "j_zany": ("ct_ThreeOfAKind", 12),
    "j_mad": ("ct_TwoPair", 10),
    "j_crazy": ("ct_Straight", 12),
    "j_droll": ("ct_Flush", 10),
    "j_half": ("P4_nl", 20),
}
TIMES_MULT = {
    "j_cavendish": (None, 3),
    "j_duo": ("ct_Pair", 2),
    "j_trio": ("ct_ThreeOfAKind", 3),
    "j_family": ("ct_FourOfAKind", 4),
    "j_order": ("ct_Straight", 3),
    "j_tribe": ("ct_Flush", 2),
    "j_blackboard": ("bb", 3),
    "j_flower_pot": ("fp", 3),
    "j_seeing_double": ("sd", 2),
    "j_acrobat": ("hz", 3),
    "j_card_sharp": ("cs", 3),
    "j_drivers_license": ("d16_{j}", 3),
}
VALUE_MULT = [
    "j_abstract",
    "j_erosion",
    "j_fortune_teller",
    "j_swashbuckler",
    "j_bootstraps",
]
VALUE_CHIPS = ["j_blue_joker", "j_stone", "j_bull"]
SITUATION_KEYS = ["j_banner", "j_mystic_summit", "j_supernova"]
EFFECT_KEYS = [
    *ADD_CHIPS,
    *ADD_MULT,
    *TIMES_MULT,
    *VALUE_MULT,
    *VALUE_CHIPS,
    *SITUATION_KEYS,
]
INERT_KEYS = [
    "j_misprint",
    "j_superposition",
    "j_seance",
    "j_vagabond",
    "j_matador",
    "j_credit_card",
    "j_marble",
    "j_chaos",
    "j_delayed_grat",
    "j_egg",
    "j_burglar",
    "j_faceless",
    "j_riff_raff",
    "j_cloud_9",
    "j_rocket",
    "j_luchador",
    "j_gift",
    "j_turtle_bean",
    "j_to_the_moon",
    "j_hallucination",
    "j_juggler",
    "j_drunkard",
    "j_golden",
    "j_diet_cola",
    "j_trading",
    "j_mr_bones",
    "j_troubadour",
    "j_certificate",
    "j_ring_master",
    "j_merry_andy",
    "j_invisible",
    "j_satellite",
    "j_cartomancer",
    "j_astronomer",
    "j_burnt",
    "j_chicot",
    "j_perkeo",
    "j_mail",
    "j_oops",
]
KEY_DIMS = {**{key: key for key in EFFECT_KEYS}, **{key: "inert" for key in INERT_KEYS}}

STAGES = [
    ("in", "input features, set by the encoder"),
    ("card", "per-card features: effective rank, suit matches, chip value parts"),
    (
        "fetch",
        "attention heads P1-P5 fetch the features of the candidate's k-th card, T averages held Steel and non-black cards",
    ),
    ("pair", "rank equalities, rank gaps, suit counts, held Steel count"),
    (
        "shape",
        "rank structure of the played cards, flush, straight, scoring-card hints",
    ),
    ("type", "one-hot hand type"),
    ("score", "which played cards score, level chips and mult"),
    (
        "situation",
        "no hands or discards left, a round play of the hand type, a joker value of 16 or more",
    ),
    ("contained", "the hand types the played cards contain, as Hands.containedIn"),
    (
        "suits",
        "suits the scoring cards cover (Flower Pot, Seeing Double), held cards all black",
    ),
    (
        "jokers",
        "per joker slot: chips added, mult added, the factor it multiplies by, with its Polychrome",
    ),
    (
        "chips",
        "chips of the scoring cards, total chips, chips split into base-16 digits",
    ),
    ("fold", "mult folded card by card in played order, a pass per Red Seal repeat"),
    ("steel", "held Steel: mult times 1.5 once per held Steel pass"),
    (
        "jokerfold",
        "mult folded slot by slot: Holo and its own add, then its factor, then Polychrome",
    ),
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
    b.dim("hl", "in", "hands left after this play, on candidates", 0)
    b.dim("dl", "in", "discards left, on candidates", 0)
    for t in TYPES:
        b.dim(f"run_{t}", "in", f"plays of {t} this run, on candidates", 0)
        b.dim(f"rnd_{t}", "in", f"plays of {t} this round, on candidates", 0)
    for j in range(1, JOKERS + 1):
        for name in [*EFFECT_KEYS, "inert"]:
            b.dim(
                f"j{j}_k_{name}",
                "in",
                (
                    f"1 on candidates when joker slot {j} holds {name}"
                    if name != "inert"
                    else f"1 on candidates when joker slot {j} holds a joker with no score effect"
                ),
                0,
            )
        for e in EDITIONS:
            b.dim(
                f"j{j}_e_{e}",
                "in",
                f"1 on candidates when the joker in slot {j} has the {e} edition",
                0,
            )
        b.dim(f"j{j}_v", "in", f"value of the joker in slot {j}, on candidates", 0)


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
    black = {"enh_Wild": 1.0, "suit_S": 1.0, "suit_C": 1.0, "enh_Stone": -3.0}
    b.define(
        "bk",
        "card",
        "1 on a card that counts as black: Spades, Clubs or Wild, never Stone",
        [(b.unit(black), 1.0), (b.unit(black, -1.0), -1.0)],
    )
    for s in SUITS:
        b.define(
            f"own_{s}",
            "card",
            f"1 on a card of suit {s} that is neither Wild nor Stone",
            [(b.unit({f"suit_{s}": 1.0, "enh_Wild": -1.0, "enh_Stone": -1.0}), 1.0)],
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
        "nl": ({"isNull": 1.0}, "1 for a null token: the card slot is empty"),
        "wd": ({"enh_Wild": 1.0}, "1 for the Wild enhancement"),
        "nb": (
            {"isCard": 1.0, "bk": -1.0},
            "1 for a card that does not count as black",
        ),
    }
    for s in SUITS:
        values[f"own_{s}"] = (
            {f"own_{s}": 1.0},
            f"1 for a card of suit {s} that is neither Wild nor Stone",
        )
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
    values = [
        (
            "T_sw",
            {"enh_Steel": 1.0, "steelRed": 1.0},
            "fresh: held Steel passes of all cards, divided by 8",
        ),
        (
            "T_nb",
            {"isCard": 1.0, "bk": -1.0},
            "fresh: cards that do not count as black in the whole hand, divided by 8",
        ),
    ]
    for name, expr, meaning in values:
        b.fresh(name, "fetch", meaning, max(b.ready[d] for d in expr))
    wk = {(0, f"slot_{s}"): 1.0 for s in range(CARD_SLOTS)}
    return head(
        b,
        "steel",
        {(0, "isCand"): BETA},
        wk,
        [(name, expr) for name, expr, _ in values],
    )


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


def stage_situation(b: Builder) -> None:
    b.define(
        "dz",
        "situation",
        "1 when no discards are left",
        [(b.unit({"dl": -1.0}, 1.0), 1.0)],
    )
    b.define(
        "hz",
        "situation",
        "1 when no hands are left after this play",
        [(b.unit({"hl": -1.0}, 1.0), 1.0)],
    )
    for t in TYPES:
        b.define(
            f"rs_{t}",
            "situation",
            f"1 when {t} was played this round",
            at_least(b, {f"rnd_{t}": 1.0}, 1.0),
        )
    for j in range(1, JOKERS + 1):
        b.define(
            f"d16_{j}",
            "situation",
            f"1 when the value of the joker in slot {j} is 16 or more",
            at_least(b, {f"j{j}_v": 1.0}, 16.0),
        )
    b.define(
        "cs",
        "situation",
        "1 when the hand type was already played this round",
        [(b.unit({f"rs_{t}": 1.0, f"t_{t}": 1.0}, -1.0), 1.0) for t in TYPES],
    )


def stage_contained(b: Builder) -> None:
    same = total_of([f"e_{k}_{m}" for k, m in PAIRS])
    b.define(
        "ct_Pair",
        "contained",
        "1 when some rank appears twice or more",
        at_least(b, same, 1.0),
    )
    b.define(
        "ct_ThreeOfAKind",
        "contained",
        "1 when some rank appears three times or more",
        at_least(b, same, 3.0),
    )
    b.define(
        "ct_FourOfAKind",
        "contained",
        "1 when some rank appears four times or more",
        at_least(b, same, 6.0),
    )
    b.define(
        "ct_TwoPair",
        "contained",
        "1 for Two Pair or a Full House: no lowering reaches Two Pair",
        equals(b, same, 2.0) + equals(b, same, 4.0),
    )
    b.define(
        "ct_Flush",
        "contained",
        "1 when the cards are a Flush",
        [(b.unit({"flush": 1.0}), 1.0)],
    )
    b.define(
        "ct_Straight",
        "contained",
        "1 when the cards are a Straight",
        [(b.unit({"straight": 1.0}), 1.0)],
    )


def stage_suits(b: Builder) -> None:
    for k in range(1, POSITIONS + 1):
        for s in SUITS:
            b.define(
                f"pk_{k}_{s}",
                "suits",
                f"1 when card {k} scores and is a plain card of suit {s}",
                [(b.unit({f"sc_{k}": 1.0, f"P{k}_own_{s}": 1.0}, -1.0), 1.0)],
            )
        b.define(
            f"wd_{k}",
            "suits",
            f"1 when card {k} scores and is Wild",
            [(b.unit({f"sc_{k}": 1.0, f"P{k}_wd": 1.0}, -1.0), 1.0)],
        )
    for s in SUITS:
        b.define(
            f"pr_{s}",
            "suits",
            f"1 when a scoring plain card has suit {s}",
            at_least(
                b, total_of([f"pk_{k}_{s}" for k in range(1, POSITIONS + 1)]), 1.0
            ),
        )
    wilds = total_of([f"wd_{k}" for k in range(1, POSITIONS + 1)])
    covered = merged(total_of([f"pr_{s}" for s in SUITS]), wilds)
    b.define(
        "fp",
        "suits",
        "1 when plain suits plus Wild cards reach all four suits (Flower Pot)",
        at_least(b, covered, 4.0),
    )
    b.define(
        "sdTwo",
        "suits",
        "1 when Clubs, the other plain suits and Wild cards number two or more",
        at_least(b, covered, 2.0),
    )
    b.define(
        "sdClubs",
        "suits",
        "1 when a plain Clubs card or a Wild card scores",
        at_least(b, merged({"pr_C": 1.0}, wilds), 1.0),
    )
    b.define(
        "sd",
        "suits",
        "1 when Clubs and one other suit end up covered (Seeing Double)",
        [(b.unit({"sdTwo": 1.0, "sdClubs": 1.0}, -1.0), 1.0)],
    )
    held = merged(
        {"T_nb": -8.0}, total_of([f"P{k}_nb" for k in range(1, POSITIONS + 1)])
    )
    b.define(
        "bb",
        "suits",
        "1 when every held card counts as black (Blackboard)",
        [(b.unit(held, 1.0), 1.0)],
    )


def stage_effects(b: Builder) -> None:
    def holds(j: int, key: str) -> str:
        return f"j{j}_k_{key}"

    def fires(j: int, key: str, cond: str | None) -> int:
        if cond is None:
            return b.unit({holds(j, key): 1.0})
        return b.unit({holds(j, key): 1.0, cond.format(j=j): 1.0}, -1.0)

    for j in range(1, JOKERS + 1):
        chips = [(b.unit({f"j{j}_e_Foil": 1.0}), 50.0)]
        chips += [
            (fires(j, key, cond), float(amount))
            for key, (cond, amount) in ADD_CHIPS.items()
        ]
        for key in VALUE_CHIPS:
            gate = {f"j{j}_v": 1.0, holds(j, key): VALUE_GATE}
            chips.append((b.unit(gate, -VALUE_GATE), 1.0))
        banner = {"dl": 30.0, holds(j, "j_banner"): BANNER_GATE}
        chips.append((b.unit(banner, -BANNER_GATE), 1.0))
        b.define(
            f"ca_{j}",
            "jokers",
            f"chips added by the joker in slot {j}: Foil and its own effect",
            chips,
        )

        mult = [(b.unit({f"j{j}_e_Holo": 1.0}), 10.0)]
        mult += [
            (fires(j, key, cond), float(amount))
            for key, (cond, amount) in ADD_MULT.items()
        ]
        for key in VALUE_MULT:
            gate = {f"j{j}_v": 1.0, holds(j, key): VALUE_GATE}
            mult.append((b.unit(gate, -VALUE_GATE), 1.0))
        mystic = {holds(j, "j_mystic_summit"): 15.0, "dl": -15.0}
        mult.append((b.unit(mystic), 1.0))
        for t in TYPES:
            run = {
                f"run_{t}": 1.0,
                f"t_{t}": RUN_GATE,
                holds(j, "j_supernova"): RUN_GATE,
            }
            mult.append((b.unit(run, 1.0 - 2 * RUN_GATE), 1.0))
        b.define(
            f"ma_{j}",
            "jokers",
            f"mult added by the joker in slot {j}: Holo and its own effect",
            mult,
        )

        for factor in (2, 3, 4):
            keys = [k for k, (_, f) in TIMES_MULT.items() if f == factor]
            b.define(
                f"x{factor}_{j}",
                "jokers",
                f"1 when the joker in slot {j} multiplies mult by {factor}",
                [(fires(j, key, TIMES_MULT[key][0]), 1.0) for key in keys],
            )
        poly = f"j{j}_e_Polychrome"
        others = {f"x{f}_{j}": -1.0 for f in (2, 3, 4)}
        bits = {
            (1, 0): (merged(others, {poly: -1.0}), 1.0),
            (1, 1): (merged(others, {poly: 1.0}), 0.0),
        }
        for factor in (2, 3, 4):
            bits[factor, 0] = ({f"x{factor}_{j}": 1.0, poly: -1.0}, 0.0)
            bits[factor, 1] = ({f"x{factor}_{j}": 1.0, poly: 1.0}, -1.0)
        for (factor, with_poly), (expr, bias) in bits.items():
            b.define(
                f"fb_{j}_{factor}_{with_poly}",
                "jokers",
                f"1 when the joker in slot {j} multiplies by {factor}"
                f" and {'has' if with_poly else 'lacks'} Polychrome",
                [(b.unit(expr, bias), 1.0)],
            )


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
    adds = [(b.unit({f"ca_{j}": 1.0}), 1.0) for j in range(1, JOKERS + 1)]
    b.define(
        "chips",
        "chips",
        "level chips plus the chips of the scoring cards and of the jokers",
        [(b.unit({"chipsBase": 1.0}), 1.0), (b.unit({"chipsCards": 1.0}), 1.0)] + adds,
    )


def stage_digits(b: Builder) -> None:
    radix = 2**CHIP_RADIX_BITS
    rest = "chips"
    for j in reversed(range(CHIP_DIGITS)):
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
        gate = 1.5**i * MULT_CARDS_CAP + 1.0
        terms.append((b.unit({folded: 1.5**i, f"hs_{i}": gate}, -gate), 1.0))
    b.define(
        "multCards", "steel", "mult after the played cards and the held Steel", terms
    )


def stage_jokerfold(b: Builder) -> str:
    previous = "multCards"
    for j in range(1, JOKERS + 1):
        terms = []
        for factor in (1, 2, 3, 4):
            for with_poly in (0, 1):
                scale = factor * (1.5 if with_poly else 1.0)
                expr = {
                    previous: scale,
                    f"ma_{j}": scale,
                    f"fb_{j}_{factor}_{with_poly}": JOKER_GATE,
                }
                terms.append((b.unit(expr, -JOKER_GATE), 1.0))
        previous = f"jm_{j}"
        b.define(
            previous,
            "jokerfold",
            f"mult after the joker in slot {j}: Holo, its own add, its factor, Polychrome",
            terms,
        )
    return previous


def stage_product(b: Builder, mult: str) -> None:
    radix = 2**CHIP_RADIX_BITS
    terms = []
    for name in b.names:
        if name.startswith("th_"):
            _, j, _ = name.split("_")
            base = float(radix ** int(j))
            gate = base * MULT_CAP + 1.0
            terms.append((b.unit({mult: base, name: gate}, -gate), 1.0))
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
    top = max(GATE, 1.5**WMAX * MULT_CARDS_CAP + 1.0)
    chip_base = 2 ** (CHIP_RADIX_BITS * (CHIP_DIGITS - 1))
    rows = [
        ("mult after the played cards and the held Steel", MULT_CARDS_CAP, FRAC),
        ("gated pre-activation in a card fold stage", 4 * int(GATE), FRAC + 1),
        ("gated pre-activation in the steel stage", 2 * int(top), FRAC + WMAX),
        ("mult after every joker slot", MULT_CAP, FRAC),
        ("gated pre-activation in a joker fold stage", 2 * int(JOKER_GATE), FRAC + 1),
        ("chips", CHIPS_CAP, 0),
        (
            "gated pre-activation of a joker add (value, banner, supernova)",
            2 * int(max(VALUE_GATE, BANNER_GATE, RUN_GATE)),
            0,
        ),
        ("chips times mult", PRODUCT_CAP, FRAC),
        (
            "gated pre-activation in the product stage",
            2 * (chip_base * MULT_CAP + 1),
            FRAC,
        ),
        (
            "floor step pre-activation, in units of 2^-8",
            int(RESOLUTION) * PRODUCT_CAP,
            0,
        ),
        ("chips digit step times its weight", chip_base * (CHIPS_CAP + 1), 0),
        ("floor digit times its weight", 256**FLOOR_DIGITS, 0),
        ("selection key", 256 * PRODUCT_CAP, 0),
    ]
    for label, peak, bits in rows:
        assert peak * 2**bits <= 2**52, label
    assert PRODUCT_CAP < 256**FLOOR_DIGITS
    # a power of two keeps the selector's attention score exact, and e^-BETA is 0
    assert BETA == 2 ** int(math.log2(BETA)) and BETA >= 745
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
        "with any enhancement, edition and seal and up to 5 main-phase jokers, no boss and no Plasma",
        "Deck balance, and reports its total and hand type.",
        "Every dim is a pure function of the dims and fetches of the loop before, so after `loops` loops",
        "all of them hold their final value. Attention weights are exactly 0, 1, or 1/8, and every",
        "stage is exact float64 arithmetic on integers and multiples of 2^-8.",
        "",
        f"- residual width d = {data['d']}, hidden units = {data['hidden']}, loops = {data['loops']}",
        f"- nonzero weights (matrices and biases) = {nonzeros}",
        f"- hands accepted: level mult 1..{MMAX}, level chips 0..{CMAX}, Polychrome plus Steel passes at most {WMAX}, Glass passes at most {GMAX} (a Red Seal doubles a pass; a Polychrome joker is one Polychrome pass)",
        f"- jokers accepted: at most {JOKERS}, none debuffed, every key in the lists below, value a whole number 0..{VALUE_MAX}; hands left and discards left whole 0..{LEFT_MAX}; plays of a hand type this run and this round whole 0..{PLAYS_MAX}",
        "- computed dims on non-candidate tokens hold values nobody reads",
        "",
        "## Joker keys in scope",
        "",
        "With a score effect: " + ", ".join(f"`{key}`" for key in EFFECT_KEYS),
        "",
        "No score effect, they only occupy a slot: "
        + ", ".join(f"`{key}`" for key in INERT_KEYS),
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
    stage_situation(b)
    stage_contained(b)
    stage_suits(b)
    stage_effects(b)
    stage_chips(b)
    stage_digits(b)
    stage_steel(b, stage_fold(b))
    stage_product(b, stage_jokerfold(b))
    stage_floor(b)
    stage_key(b)
    heads.append(selection_head(b))
    bounds = check_bounds(b)

    loops = max(b.ready[name] for name in b.names if name.startswith("sel_"))
    data = {
        "d": len(b.names),
        "hidden": len(b.units),
        "loops": loops,
        "limits": {
            "mult": MMAX,
            "chips": CMAX,
            "weight": WMAX,
            "glass": GMAX,
            "jokers": JOKERS,
            "value": VALUE_MAX,
            "left": LEFT_MAX,
            "plays": PLAYS_MAX,
        },
        "keys": KEY_DIMS,
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
