import itertools
import json
import math
from fractions import Fraction
from pathlib import Path

Expr = dict[str, float]

BETA = 1024.0
MMAX = 200
CMAX = 5000
WMAX = 10
POSITIONS = 5
SLOTS = 13
CARD_SLOTS = 8
SENTINEL = 20
CHIP_RADIX_BITS = 7
FLOOR_RADIX_BITS = 8
JOKERS = 5
VALUE_MAX = 500
LEFT_MAX = 10
PLAYS_MAX = 100
PASSES = 7
DOUBLES = 3
HALVES = 2
HELD_PASSES = 5
HELD_HALVES = 3

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
PARTNER = {"S": "C", "C": "S", "H": "D", "D": "H"}
ENHANCEMENTS = ["Bonus", "Mult", "Wild", "Glass", "Steel", "Stone", "Gold", "Lucky"]
EDITIONS = ["Foil", "Holo", "Polychrome", "Negative"]
SEALS = ["Red", "Blue", "Gold", "Purple"]
PAIRS = [(k, m) for k in range(1, POSITIONS + 1) for m in range(k + 1, POSITIONS + 1)]
ORDERED = [
    (k, m) for k in range(1, POSITIONS + 1) for m in range(1, POSITIONS + 1) if k != m
]

# Largest mult and chips any accepted hand reaches at any step, and of their product
MULT_CAP = 2**30
CHIPS_CAP = 2**14 - 1
PRODUCT_CAP = 4 * 10**12
FMAX = 8
FACTOR_K = 4 * FMAX
K_BITS = FACTOR_K.bit_length()
BASEBALLS = JOKERS - 1
POLY_COPIES = BASEBALLS + 1
FACTOR_GATE = float(FMAX)
TID_GATE = 15.0
VALUE_GATE = float(VALUE_MAX + 20)
BANNER_GATE = float(30 * LEFT_MAX + 1)
RUN_GATE = float(PLAYS_MAX + 1)
FRAC = WMAX
RESOLUTION = float(2**FRAC)
CHIP_DIGITS = math.ceil(CHIPS_CAP.bit_length() / CHIP_RADIX_BITS)
FLOOR_DIGITS = math.ceil(PRODUCT_CAP.bit_length() / FLOOR_RADIX_BITS)
KEY_BIG = float(256 * (PRODUCT_CAP + 1))
GATE = float(MULT_CAP + 1)
SHIFT_GATE = float(MULT_CAP * 2 ** (K_BITS - 1))
COPY_GATE = float(math.ceil(1.5**POLY_COPIES * MULT_CAP) + 1)

# One card pass: its own adds up to 14 (Mult 4, Holo 10) and each joker adds up to 8, then at most DOUBLES doublings and HALVES factors of 1.5
PASS_MULT_CAP = math.ceil((14 + 8 * JOKERS) * 2**DOUBLES * 1.5**HALVES)
PASS_GATE = float(PASS_MULT_CAP + 1)
PASS_CHIPS_CAP = 100 + 50 * JOKERS
CHIPS_GATE = float(PASS_CHIPS_CAP + 1)
HELD_MULT_CAP = math.ceil(22 * JOKERS * 1.5**HELD_HALVES)
HELD_GATE = float(HELD_MULT_CAP + 1)
RANK_GATE = float(16)

# Joker keys by effect: (condition dim or None, amount); {j} is the slot
ADD_CHIPS = {
    "j_stuntman": (None, 250),
    "j_sly": ("ct_Pair", 50),
    "j_wily": ("ct_ThreeOfAKind", 100),
    "j_clever": ("ct_TwoPair", 80),
    "j_devious": ("ct_Straight", 100),
    "j_crafty": ("ct_Flush", 80),
}
# value plus an amount that depends on the hand: (condition dim, amount per condition)
VALUE_PLUS_CHIPS = {
    "j_runner": ("ct_Straight", 15),
    "j_square": ("sz4", 4),
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
VALUE_PLUS_MULT = {
    "j_trousers": ("ct_TwoPair", 2),
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
    "j_loyalty_card": ("eloy_{j}", 4),
}
VALUE_MULT = [
    "j_abstract",
    "j_erosion",
    "j_fortune_teller",
    "j_swashbuckler",
    "j_bootstraps",
    "j_ceremonial",
    "j_red_card",
    "j_flash",
    "j_popcorn",
]
VALUE_CHIPS = ["j_blue_joker", "j_stone", "j_bull", "j_castle", "j_wee", "j_ice_cream"]
# value plus 1, and for Ride the Bus only while no scoring card is a face card
GREEN_KEY = "j_green_joker"
BUS_KEY = "j_ride_the_bus"
# the mult times a factor that is an input of the joker: its value, or its text for Ramen
FACTOR_KEYS = [
    "j_stencil",
    "j_steel_joker",
    "j_throwback",
    "j_constellation",
    "j_madness",
    "j_hologram",
    "j_campfire",
    "j_glass",
    "j_lucky_cat",
    "j_hit_the_road",
    "j_yorick",
    "j_caino",
    "j_ramen",
]
# value + 0.2 times, when another hand type was played at least as often
OBELISK_KEY = "j_obelisk"
COPY_KEYS = ["j_blueprint", "j_brainstorm"]
MIDAS_KEY = "j_midas_mask"
RULE_KEYS = [
    "j_four_fingers",
    "j_shortcut",
    "j_smeared",
    "j_splash",
    "j_pareidolia",
]
BASEBALL_KEY = "j_baseball"
SITUATION_KEYS = ["j_banner", "j_mystic_summit", "j_supernova"]

# Per-card jokers: key -> (card feature that makes it fire, amount), added to every pass of a scoring card
CARD_ADD_MULT = {
    "j_greedy_joker": ("sm_D", 3),
    "j_lusty_joker": ("sm_H", 3),
    "j_wrathful_joker": ("sm_S", 3),
    "j_gluttenous_joker": ("sm_C", 3),
    "j_onyx_agate": ("sm_C", 7),
    "j_fibonacci": ("fib", 8),
    "j_smiley": ("fc", 5),
    "j_even_steven": ("ev", 4),
    "j_scholar": ("ac", 4),
    "j_walkie_talkie": ("wk", 4),
}
CARD_ADD_CHIPS = {
    "j_scary_face": ("fc", 30),
    "j_odd_todd": ("od", 31),
    "j_scholar": ("ac", 20),
    "j_walkie_talkie": ("wk", 10),
    "j_arrowhead": ("sm_S", 50),
}
SCORED_KEYS = [
    *CARD_ADD_MULT,
    *[k for k in CARD_ADD_CHIPS if k not in CARD_ADD_MULT],
    "j_triboulet",
    "j_photograph",
    "j_idol",
    "j_ancient",
]
RETRIGGER_KEYS = [
    "j_hanging_chad",
    "j_sock_and_buskin",
    "j_hack",
    "j_selzer",
    "j_dusk",
]
HELD_KEYS = ["j_baron", "j_mime", "j_raised_fist", "j_shoot_the_moon"]
CARD_KEYS = [*SCORED_KEYS, *RETRIGGER_KEYS, *HELD_KEYS]
MAIN_KEYS = [
    *ADD_CHIPS,
    *ADD_MULT,
    *TIMES_MULT,
    *VALUE_MULT,
    *VALUE_CHIPS,
    *VALUE_PLUS_CHIPS,
    *VALUE_PLUS_MULT,
    *SITUATION_KEYS,
    GREEN_KEY,
    BUS_KEY,
]
EFFECT_KEYS = [*MAIN_KEYS, *CARD_KEYS]
# the keys whose slot is read through a copy
COPIED_KEYS = [*EFFECT_KEYS, BASEBALL_KEY]
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
    "j_8_ball",
    "j_business",
    "j_ticket",
    "j_rough_gem",
    "j_reserved_parking",
    "j_bloodstone",
    "j_hiker",
    "j_space",
    "j_dna",
    "j_todo_list",
    "j_sixth_sense",
]
# JokerRegistry.uncommon: Baseball Card multiplies by 1.5 for each of these in the other slots
UNCOMMON = {
    "j_stencil",
    "j_four_fingers",
    "j_pareidolia",
    "j_shortcut",
    "j_smeared",
    "j_mime",
    "j_ceremonial",
    "j_marble",
    "j_loyalty_card",
    "j_dusk",
    "j_fibonacci",
    "j_steel_joker",
    "j_hack",
    "j_blackboard",
    "j_sixth_sense",
    "j_burglar",
    "j_constellation",
    "j_hiker",
    "j_card_sharp",
    "j_madness",
    "j_seance",
    "j_space",
    "j_hologram",
    "j_cloud_9",
    "j_rocket",
    "j_midas_mask",
    "j_luchador",
    "j_gift",
    "j_turtle_bean",
    "j_erosion",
    "j_to_the_moon",
    "j_stone",
    "j_lucky_cat",
    "j_bull",
    "j_diet_cola",
    "j_trading",
    "j_flash",
    "j_trousers",
    "j_ramen",
    "j_selzer",
    "j_castle",
    "j_mr_bones",
    "j_acrobat",
    "j_sock_and_buskin",
    "j_troubadour",
    "j_certificate",
    "j_throwback",
    "j_rough_gem",
    "j_bloodstone",
    "j_arrowhead",
    "j_onyx_agate",
    "j_glass",
    "j_ring_master",
    "j_flower_pot",
    "j_merry_andy",
    "j_oops",
    "j_idol",
    "j_seeing_double",
    "j_matador",
    "j_satellite",
    "j_cartomancer",
    "j_astronomer",
    "j_bootstraps",
}


def key_class(key: str) -> str:
    if key in INERT_KEYS:
        return "inertU" if key in UNCOMMON else "inert"
    if key in [*FACTOR_KEYS, OBELISK_KEY]:
        return "xfU" if key in UNCOMMON else "xf"
    return key


KEY_DIMS = {
    key: key_class(key)
    for key in [
        *COPIED_KEYS,
        *INERT_KEYS,
        *FACTOR_KEYS,
        OBELISK_KEY,
        *COPY_KEYS,
        MIDAS_KEY,
        *RULE_KEYS,
    ]
}
KEY_CLASSES = sorted(set(KEY_DIMS.values()))
UNCOMMON_CLASSES = sorted({name for key, name in KEY_DIMS.items() if key in UNCOMMON})
UNCOMMON_KEYS = sorted(key for key in KEY_DIMS if key in UNCOMMON)

STAGES = [
    ("in", "input features, set by the encoder"),
    (
        "clock",
        "the number of loops run, and the bits that say a stage's inputs have settled",
    ),
    (
        "card",
        "per-card features: effective rank, suit matches, rank sets, chip value parts",
    ),
    (
        "fetch",
        "attention heads P1-P5 fetch the features of the candidate's k-th card, H0-H7 those of hand slot s, T averages non-black cards",
    ),
    (
        "copies",
        "which slots hold a joker that is not a copy, which are Uncommon, and, by rounds, which slot each slot acts as after its Blueprint and Brainstorm copies",
    ),
    (
        "effective",
        "per slot: the key, value, factor, text bits of the joker it acts as; whether a Midas Mask is held; how many slots act as Baseball Card",
    ),
    (
        "rules",
        "which rule jokers are held (Four Fingers, Shortcut, Smeared, Splash, Pareidolia), read from the raw keys",
    ),
    (
        "midas",
        "per card: face as isFace sees it, and the chips, enhancements, ranks and suit matches it has once a Midas Mask has turned face cards Gold",
    ),
    ("pair", "rank equalities, rank orders, suit matches as flushCards sees them"),
    (
        "patterns",
        "flush suits and cards, which ranks are present, every run of 4 or 5 ranks a straight can take, the ranks and cards in the straight",
    ),
    (
        "shape",
        "rank structure of the played cards, flush, straight, scoring-card hints",
    ),
    ("type", "one-hot hand type"),
    ("score", "which played cards score, level chips and mult"),
    (
        "situation",
        "no hands or discards left, a round play of the hand type, a joker value of 16 or more, exactly four cards played, another hand type played as often as this one (Obelisk)",
    ),
    ("contained", "the hand types the played cards contain, as Hands.containedIn"),
    (
        "suits",
        "suits the scoring cards cover (Flower Pot, Seeing Double), held cards all black",
    ),
    (
        "jokers",
        "per joker slot: chips added, mult added, whether it multiplies by 2, 3 or 4",
    ),
    (
        "factors",
        "per joker slot: 4 times its factor and its bits, the Baseball Card and Polychrome factors 1.5 it meets",
    ),
    ("order", "the first scoring card and the first scoring face card"),
    (
        "scored",
        "per joker slot and scoring card: target suit and rank matches, the mult and chips it adds, whether it doubles or multiplies by 1.5",
    ),
    (
        "passes",
        "per scoring card: passes (itself, Red Seal, retriggers), the affine map one pass applies to mult, one-hots of its shape",
    ),
    (
        "playedfold",
        "mult after each scoring card: the pass map applied as many times as the card has passes, in played order",
    ),
    (
        "held",
        "per hand slot: whether it is held, Raised Fist's target, the affine map one held pass applies, its pass count",
    ),
    (
        "heldfold",
        "mult after each held card in hand order, the held pass map applied as many times as it has passes",
    ),
    (
        "chips",
        "chips of the scoring cards over all their passes, total chips, chips split into base-128 digits",
    ),
    (
        "jokerfold",
        "mult folded slot by slot: Holo and its own add, then its factor by shift-and-add, then its Baseball Card factors and Polychrome",
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
        for name in KEY_CLASSES:
            b.dim(f"j{j}_k_{name}", "in", key_meaning(j, name), 0)
        for e in EDITIONS:
            b.dim(
                f"j{j}_e_{e}",
                "in",
                f"1 on candidates when the joker in slot {j} has the {e} edition",
                0,
            )
        b.dim(f"j{j}_v", "in", f"value of the joker in slot {j}, on candidates", 0)
        for suit in SUITS:
            b.dim(
                f"j{j}_ts_{suit}",
                "in",
                f"1 on candidates when the Ancient Joker or Idol in slot {j} names suit {suit}",
                0,
            )
        b.dim(
            f"j{j}_tid",
            "in",
            f"rank id the Idol in slot {j} names, 0 for none; the suit bits are all 0 when its text does not parse",
            0,
        )
        b.dim(
            f"j{j}_fx",
            "in",
            f"factor minus 1 of the joker in slot {j} when it multiplies mult by an input factor, else 0",
            0,
        )
        b.dim(
            f"j{j}_fo",
            "in",
            f"factor minus 1 of the Obelisk in slot {j}, else 0",
            0,
        )
        b.dim(
            f"j{j}_loy",
            "in",
            f"1 on candidates when the Loyalty Card text in slot {j} says it is active",
            0,
        )


def key_meaning(j: int, name: str) -> str:
    holder = f"1 on candidates when joker slot {j} holds"
    return {
        "inert": f"{holder} a joker with no score effect",
        "inertU": f"{holder} an Uncommon joker with no score effect",
        "xf": f"{holder} a joker that multiplies by an input factor",
        "xfU": f"{holder} an Uncommon joker that multiplies by an input factor",
    }.get(name, f"{holder} {name}")


def raw(j: int, name: str) -> str:
    return f"j{j}_k_{name}"


def holds(j: int, key: str) -> str:
    return f"e{j}_k_{key}"


def and_bit(b: Builder, left: str, right: str) -> int:
    return b.unit({left: 1.0, right: 1.0}, -1.0)


# the dim that is 1 when slot j resolves to slot s within n hops of copying, None when it never is
def resolved(b: Builder, n: int, j: int, s: int) -> str | None:
    name = f"real_{j}" if n == 0 else f"rs{n}_{j}_{s}"
    return name if (s == j or n > 0) and name in b.index else None


def stage_copies(b: Builder) -> None:
    solid = [name for name in KEY_CLASSES if name not in COPY_KEYS]
    for j in range(1, JOKERS + 1):
        b.define(
            f"real_{j}",
            "copies",
            f"1 when slot {j} holds a joker that is not a copy",
            [(b.unit({raw(j, name): 1.0 for name in solid}), 1.0)],
        )
        b.define(
            f"unc_{j}",
            "copies",
            f"1 when slot {j} holds an Uncommon joker, as Baseball Card counts them",
            [(b.unit({raw(j, name): 1.0 for name in UNCOMMON_CLASSES}), 1.0)],
        )
    # Blueprint copies the slot to its right, Brainstorm the first slot; a chain of copies is at most JOKERS - 1 long
    for n in range(1, JOKERS):
        for j in range(1, JOKERS + 1):
            for s in range(1, JOKERS + 1):
                terms = []
                if s == j:
                    terms.append((b.unit({f"real_{j}": 1.0}), 1.0))
                if j < JOKERS and resolved(b, n - 1, j + 1, s):
                    bit = and_bit(
                        b, raw(j, "j_blueprint"), resolved(b, n - 1, j + 1, s)
                    )
                    terms.append((bit, 1.0))
                if j > 1 and resolved(b, n - 1, 1, s):
                    bit = and_bit(b, raw(j, "j_brainstorm"), resolved(b, n - 1, 1, s))
                    terms.append((bit, 1.0))
                if terms:
                    b.define(
                        f"rs{n}_{j}_{s}",
                        "copies",
                        f"1 when slot {j} acts as the joker in slot {s} after at most {n} copies",
                        terms,
                    )


def stage_effective(b: Builder) -> None:
    last = JOKERS - 1
    for j in range(1, JOKERS + 1):
        sources = [
            (s, resolved(b, last, j, s))
            for s in range(1, JOKERS + 1)
            if resolved(b, last, j, s)
        ]
        for key in COPIED_KEYS:
            terms = [
                (and_bit(b, res, raw(s, KEY_DIMS[key])), 1.0) for s, res in sources
            ]
            b.define(
                holds(j, key),
                "effective",
                f"1 when slot {j} acts as {key}: it holds it, or copies the slot that does",
                terms,
            )
        features = [
            ("v", "value", VALUE_GATE),
            ("fx", "factor minus 1", FACTOR_GATE),
            ("fo", "Obelisk factor minus 1", FACTOR_GATE),
            ("tid", "Idol rank id", TID_GATE),
            ("loy", "Loyalty Card active bit", 1.0),
            *[
                (f"ts_{suit}", f"Ancient or Idol suit bit {suit}", 1.0)
                for suit in SUITS
            ],
        ]
        for name, meaning, gate in features:
            terms = [
                (b.unit({res: gate, f"j{s}_{name}": 1.0}, -gate), 1.0)
                for s, res in sources
            ]
            b.define(
                f"e{name}_{j}",
                "effective",
                f"{meaning} of the joker slot {j} acts as",
                terms,
            )
    b.define(
        "mid",
        "effective",
        "1 when a Midas Mask is held",
        at_least(b, total_of([raw(j, MIDAS_KEY) for j in range(1, JOKERS + 1)]), 1.0),
    )
    b.define(
        "nb",
        "effective",
        "number of slots that act as Baseball Card",
        [(b.unit({holds(j, BASEBALL_KEY): 1.0}), 1.0) for j in range(1, JOKERS + 1)],
    )


def stage_clock(b: Builder) -> None:
    b.dim("tick", "clock", "loops run so far: the MLP adds 1 every loop", 0)
    b.add("tick", b.unit({"one": 1.0}), 1.0)


# 1 once every dim in names has its final value: a gate that fires on half-settled inputs would feed a wrong mult into the folds
def settled(b: Builder, names: list[str]) -> str:
    loop = max(b.ready[name] for name in names)
    name = f"go_{loop}"
    if name not in b.index:
        b.define(
            name,
            "clock",
            f"1 from loop {loop + 1} on, when the dims ready at loop {loop} are final",
            at_least(b, {"tick": 1.0}, float(loop)),
        )
        b.ready[name] = loop + 1
    return name


def stage_rules(b: Builder) -> None:
    rules = [
        ("r_ff", "j_four_fingers", "Four Fingers: flushes and straights need 4 cards"),
        ("r_sh", "j_shortcut", "Shortcut: a straight may skip one missing rank"),
        (
            "r_sm",
            "j_smeared",
            "Smeared: Hearts and Diamonds are one suit, Spades and Clubs one",
        ),
        ("r_sp", "j_splash", "Splash: every played card scores"),
        ("r_pa", "j_pareidolia", "Pareidolia: every card is a face card"),
    ]
    for name, key, meaning in rules:
        held = total_of([raw(j, key) for j in range(1, JOKERS + 1)])
        b.define(
            name,
            "rules",
            f"1 when a joker slot holds {key} itself, a copy does not count ({meaning})",
            at_least(b, held, 1.0),
        )


def stage_midas(b: Builder) -> None:
    for k in range(1, POSITIONS + 1):
        b.define(
            f"face_{k}",
            "midas",
            f"1 when card {k} is a face card as isFace sees it: Jack, Queen or King, or any card with Pareidolia",
            [
                (b.unit({f"P{k}_fc": 1.0}), 1.0),
                (b.unit({"r_pa": 1.0, f"P{k}_nl": -1.0, f"P{k}_fc": -1.0}), 1.0),
            ],
        )
        mg = f"mg_{k}"
        b.define(
            mg,
            "midas",
            f"1 when card {k} is a face card and a Midas Mask turns it Gold",
            [(b.unit({f"face_{k}": 1.0, "mid": 1.0}, -1.0), 1.0)],
        )
        sg = f"sg_{k}"
        b.define(
            sg,
            "midas",
            f"1 when card {k} is a Stone that a Midas Mask turns Gold (Pareidolia makes it a face card)",
            [(b.unit({f"P{k}_st": 1.0, mg: 1.0}, -1.0), 1.0)],
        )
        b.define(
            f"Qst_{k}",
            "midas",
            f"1 when card {k} is a Stone once a Midas Mask has acted",
            [(b.unit({f"P{k}_st": 1.0, sg: -1.0}), 1.0)],
        )
        b.define(
            f"Q{k}_cv",
            "midas",
            f"chips of one scoring pass of card {k} once a Midas Mask has acted",
            [
                (b.unit({f"P{k}_cv": 1.0}), 1.0),
                (b.unit({f"P{k}_bo": 1.0, mg: 1.0}, -1.0), -30.0),
                (b.unit({sg: 1.0}), -50.0),
                (b.unit({f"P{k}_unom": 1.0, sg: 11.0}, -11.0), 1.0),
            ],
        )
        for name, meaning in (
            ("a", "the Mult enhancement"),
            ("gl", "the Glass enhancement"),
            ("wd", "the Wild enhancement"),
        ):
            b.define(
                f"Q{k}_{name}",
                "midas",
                f"1 when card {k} has {meaning} once a Midas Mask has acted",
                [(b.unit({f"P{k}_{name}": 1.0, mg: -1.0}), 1.0)],
            )
        for s in SUITS:
            turned = {f"P{k}_wd": 1.0, mg: 1.0, f"P{k}_su_{s}": 1.0}
            stone = {sg: 1.0, f"P{k}_us_{s}": 1.0}
            b.define(
                f"Q{k}_own_{s}",
                "midas",
                f"1 for a plain card of suit {s}, a Gold card counting as plain",
                [
                    (b.unit({f"P{k}_own_{s}": 1.0}), 1.0),
                    (b.unit(turned, -2.0), 1.0),
                    (b.unit(stone, -1.0), 1.0),
                ],
            )
        for s in SUITS:
            match = {f"P{k}_wd": 1.0, f"P{k}_su_{s}": -1.0, mg: -1.0}
            stone = {sg: 1.0, f"P{k}_us_{s}": 1.0}
            partner = {f"Q{k}_own_{PARTNER[s]}": 1.0, "r_sm": 1.0}
            b.define(
                f"Q{k}_sm_{s}",
                "midas",
                f"1 when card {k} matches suit {s} as isSuit sees it once a Midas Mask has acted",
                [
                    (b.unit({f"P{k}_su_{s}": 1.0}), 1.0),
                    (b.unit(match), 1.0),
                    (b.unit(stone, -1.0), 1.0),
                    (b.unit(partner, -1.0), 1.0),
                ],
            )
        for name in ("kq", "fib", "ev", "od", "wk", "hk", "ac"):
            b.define(
                f"Q{k}_{name}",
                "midas",
                f"1 when card {k} is in rank set {name} once a Midas Mask has acted: a Stone has none unless it was turned Gold",
                [(b.unit({f"P{k}_{name}": 1.0, f"Qst_{k}": -1.0}), 1.0)],
            )
        b.define(
            f"Q{k}_rr",
            "midas",
            f"rank id of card {k} once a Midas Mask has acted, 0 for a Stone",
            [(b.unit({f"P{k}_urr": 1.0, f"Qst_{k}": -14.0}), 1.0)],
        )


def stage_factors(b: Builder) -> None:
    for j in range(1, JOKERS + 1):
        b.define(
            f"fa_{j}",
            "factors",
            f"input factor minus 1 that the joker in slot {j} applies to this hand",
            [
                (b.unit({f"efx_{j}": 1.0}), 1.0),
                (b.unit({f"efo_{j}": 1.0, "ob": FACTOR_GATE}, -FACTOR_GATE), 1.0),
            ],
        )
        terms = [(b.unit({"one": 1.0}, 3.0), 1.0), (b.unit({f"fa_{j}": 1.0}), 4.0)]
        terms += [(b.unit({f"x{c}_{j}": 1.0}), 4.0 * (c - 1)) for c in (2, 3, 4)]
        b.define(
            f"k_{j}",
            "factors",
            f"4 times the factor of the joker in slot {j}, as a whole number",
            terms,
        )
        for a in range(5, FACTOR_K + 1):
            b.define(
                f"kt_{j}_{a}",
                "factors",
                f"1 when 4 times the factor of slot {j} is at least {a}",
                at_least(b, {f"k_{j}": 1.0}, float(a)),
            )

        def reach(a: int) -> list[tuple[int, float]]:
            if a > FACTOR_K:
                return []
            if a < 5:
                return [(b.unit({"one": 1.0}), 1.0)]
            return [(b.unit({f"kt_{j}_{a}": 1.0}), 1.0)]

        for i in range(K_BITS):
            terms = []
            for t in range(1, FACTOR_K // 2**i + 1, 2):
                terms += reach(t * 2**i) + weighted(reach((t + 1) * 2**i), -1.0)
            b.define(
                f"kb_{j}_{i}",
                "factors",
                f"bit {i} of 4 times the factor of slot {j}",
                terms,
            )
        b.define(
            f"bb_{j}",
            "factors",
            f"Baseball Card factors 1.5 that slot {j} meets: one per Baseball Card when it is Uncommon",
            [
                (
                    b.unit({"nb": 1.0, f"unc_{j}": float(JOKERS)}, -float(JOKERS)),
                    1.0,
                )
            ],
        )
        one_hot_of = {f"bb_{j}": 1.0, f"j{j}_e_Polychrome": 1.0}
        for c in range(POLY_COPIES + 1):
            b.define(
                f"cq_{j}_{c}",
                "factors",
                f"1 when slot {j} multiplies by 1.5 {c} times: Baseball Cards and Polychrome",
                equals(b, one_hot_of, float(c)),
            )


def stage_jokerfold(b: Builder) -> str:
    previous = "multCards"
    for j in range(1, JOKERS + 1):
        terms = [
            (
                b.unit(
                    {
                        previous: 2.0**i,
                        f"ma_{j}": 2.0**i,
                        f"kb_{j}_{i}": SHIFT_GATE,
                    },
                    -SHIFT_GATE,
                ),
                0.25,
            )
            for i in range(K_BITS)
        ]
        shifted = f"js_{j}"
        b.define(
            shifted,
            "jokerfold",
            f"mult after the Holo, the own add and the factor of slot {j}: shift-and-add over the bits of 4 times the factor",
            terms,
        )
        terms = [
            (
                b.unit({shifted: 1.5**c, f"cq_{j}_{c}": COPY_GATE}, -COPY_GATE),
                1.0,
            )
            for c in range(POLY_COPIES + 1)
        ]
        previous = f"jm_{j}"
        b.define(
            previous,
            "jokerfold",
            f"mult after slot {j}: its Baseball Card factors and Polychrome close the slot",
            terms,
        )
    return previous


def stage_card(b: Builder) -> None:
    rank_id = {f"rank_{r}": float(r) for r in RANKS}
    b.define(
        "rr",
        "card",
        "rank id of the card, 0 for a Stone",
        [(b.unit({**rank_id, "enh_Stone": -14.0}), 1.0)],
    )
    b.define(
        "nomNS",
        "card",
        "nominal of the card, 0 for a Stone",
        [(b.unit({"nominal": 1.0, "enh_Stone": -11.0}), 1.0)],
    )
    # a Stone never counts as a face card, King or Queen; the other sets keep Stone because a Midas Mask can turn it Gold
    rank_sets = {
        "fc": ([11, 12, 13], True, "a face card: Jack, Queen or King, never Stone"),
        "kg": ([13], True, "a King, never Stone"),
        "qn": ([12], True, "a Queen, never Stone"),
        "fib": ([14, 2, 3, 5, 8], False, "an Ace, 2, 3, 5 or 8, Stone included"),
        "ev": ([2, 4, 6, 8, 10], False, "a 2, 4, 6, 8 or 10, Stone included"),
        "od": ([3, 5, 7, 9, 14], False, "a 3, 5, 7, 9 or Ace, Stone included"),
        "wk": ([10, 4], False, "a 10 or 4, Stone included"),
        "hk": ([2, 3, 4, 5], False, "a 2, 3, 4 or 5, Stone included"),
    }
    for name, (ranks, not_stone, meaning) in rank_sets.items():
        expr = {f"rank_{r}": 1.0 for r in ranks}
        if not_stone:
            expr["enh_Stone"] = -1.0
        b.define(name, "card", f"1 on {meaning}", [(b.unit(expr), 1.0)])
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
        b.define(
            f"su_{s}",
            "card",
            f"1 on a card of suit {s} that is not a Stone, whether or not it is Wild",
            [(b.unit({f"suit_{s}": 1.0, "enh_Stone": -1.0}), 1.0)],
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
        "ac": ({"rank_14": 1.0}, "1 for an Ace, Stone included"),
        "ns": ({"isCard": 1.0, "enh_Stone": -1.0}, "1 for a card that is not a Stone"),
        "fc": ({"fc": 1.0}, "1 for a face card"),
        "kq": (
            {"rank_12": 1.0, "rank_13": 1.0},
            "1 for a King or Queen, Stone included",
        ),
        "fib": ({"fib": 1.0}, "1 for an Ace, 2, 3, 5 or 8, Stone included"),
        "ev": ({"ev": 1.0}, "1 for a 2, 4, 6, 8 or 10, Stone included"),
        "od": ({"od": 1.0}, "1 for a 3, 5, 7, 9 or Ace, Stone included"),
        "wk": ({"wk": 1.0}, "1 for a 10 or 4, Stone included"),
        "hk": ({"hk": 1.0}, "1 for a 2, 3, 4 or 5, Stone included"),
        "urr": (
            {f"rank_{r}": float(r) for r in RANKS},
            "rank id, Stone included",
        ),
        "unom": ({"nominal": 1.0}, "nominal chip value, Stone included"),
        "nl": ({"isNull": 1.0}, "1 for a null token: the card slot is empty"),
        "wd": ({"enh_Wild": 1.0}, "1 for the Wild enhancement"),
        "bo": ({"enh_Bonus": 1.0}, "1 for the Bonus enhancement"),
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
    for s in SUITS:
        values[f"su_{s}"] = (
            {f"su_{s}": 1.0},
            f"1 for a card of suit {s} that is not a Stone",
        )
    for s in SUITS:
        values[f"us_{s}"] = (
            {f"suit_{s}": 1.0},
            f"1 for a card of suit {s}, Stone included",
        )
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


def nonblack_head(b: Builder) -> dict:
    expr = {"isCard": 1.0, "bk": -1.0}
    meaning = "fresh: cards that do not count as black in the whole hand, divided by 8"
    b.fresh("T_nb", "fetch", meaning, max(b.ready[d] for d in expr))
    wk = {(0, f"slot_{s}"): 1.0 for s in range(CARD_SLOTS)}
    return head(b, "nonblack", {(0, "isCand"): BETA}, wk, [("T_nb", expr)])


def held_heads(b: Builder) -> list[dict]:
    values = {
        "card": ({"isCard": 1.0}, "1 when the slot holds a card"),
        "rank": ({"rr": 1.0}, "rank id, 0 for a Stone"),
        "nom": ({"nomNS": 1.0}, "nominal, 0 for a Stone"),
        "king": ({"kg": 1.0}, "1 for a King"),
        "queen": ({"qn": 1.0}, "1 for a Queen"),
        "steel": ({"enh_Steel": 1.0}, "1 for the Steel enhancement"),
        "red": ({"seal_Red": 1.0}, "1 for the Red Seal"),
        "ns": ({"isCard": 1.0, "enh_Stone": -1.0}, "1 for a card that is not a Stone"),
    }
    heads = []
    for s in range(CARD_SLOTS):
        fresh = []
        for v, (expr, meaning) in values.items():
            ready = max(b.ready[dim] for dim in expr)
            b.fresh(f"H{s}_{v}", "fetch", f"fresh: {meaning}, of hand slot {s}", ready)
            fresh.append((f"H{s}_{v}", expr))
        wq = {(0, "isCand"): BETA}
        wk = {(0, f"slot_{s}"): 1.0}
        heads.append(head(b, f"held{s}", wq, wk, fresh))
    return heads


def stage_pairs(b: Builder) -> None:
    for k, m in PAIRS:
        gap = {f"P{k}_rs": 1.0, f"P{m}_rs": -1.0}
        b.define(
            f"e_{k}_{m}",
            "pair",
            f"1 when cards {k} and {m} have the same rank",
            equals(b, gap, 0.0),
        )
    for k, m in ORDERED:
        gap = {f"P{m}_rr": 1.0, f"P{k}_rr": -1.0}
        b.define(
            f"gt_{k}_{m}",
            "pair",
            f"1 when card {m} outranks card {k}",
            at_least(b, gap, 1.0),
        )
    for k in range(1, POSITIONS + 1):
        for s in SUITS:
            b.define(
                f"Z{k}_{s}",
                "pair",
                f"1 when card {k} matches suit {s} as matchesSuit sees it, before any Midas Mask",
                [
                    (b.unit({f"P{k}_sm_{s}": 1.0}), 1.0),
                    (b.unit({f"P{k}_own_{PARTNER[s]}": 1.0, "r_sm": 1.0}, -1.0), 1.0),
                ],
            )


# every set of ranks a straight can take: 4 or 5 ranks, each next one 1 above, or 2 above for the gap Shortcut allows
def runs(length: int) -> list[tuple[int, ...]]:
    found = []
    for steps in itertools.product((1, 2), repeat=length - 1):
        span = sum(steps)
        for first in range(1, 14 - span + 1):
            ids = [first]
            for step in steps:
                ids.append(ids[-1] + step)
            found.append(tuple(ids))
    return found


def stage_patterns(b: Builder) -> None:
    cards = range(1, POSITIONS + 1)
    for s in SUITS:
        count = {**{f"Z{k}_{s}": 1.0 for k in cards}, "r_ff": 1.0}
        b.define(
            f"flq_{s}",
            "patterns",
            f"1 when at least 5 cards match suit {s}, or 4 with Four Fingers",
            at_least(b, count, 5.0),
        )
    for n, s in enumerate(SUITS):
        earlier = {f"flq_{t}": -1.0 for t in SUITS[:n]}
        b.define(
            f"fls_{s}",
            "patterns",
            f"1 when {s} is the first suit in the order of Hands.flushCards with enough matching cards",
            [(b.unit({f"flq_{s}": 1.0, **earlier}), 1.0)],
        )
    for k in cards:
        b.define(
            f"fcd_{k}",
            "patterns",
            f"1 when card {k} is a card of the flush",
            [(b.unit({f"fls_{s}": 1.0, f"Z{k}_{s}": 1.0}, -1.0), 1.0) for s in SUITS],
        )
    for k in cards:
        for r in RANKS:
            b.define(
                f"rk_{k}_{r}",
                "patterns",
                f"1 when card {k} has rank id {r}, never a Stone",
                equals(b, {f"P{k}_rr": 1.0}, float(r)),
            )
    for r in RANKS:
        b.define(
            f"pv_{r}",
            "patterns",
            f"1 when some card has rank id {r}",
            at_least(b, total_of([f"rk_{k}_{r}" for k in cards]), 1.0),
        )
    patterns = []
    for length in (4, 5):
        for ids in runs(length):
            gap = any(y - x == 2 for x, y in zip(ids, ids[1:]))
            ranks = [14 if i == 1 else i for i in ids]
            expr = {f"pv_{r}": 1.0 for r in ranks}
            expr["r_ff"] = 1.0 if length == 4 else -1.0
            if gap:
                expr["r_sh"] = 1.0
            name = f"pt_{'_'.join(map(str, ids))}"
            meaning = f"1 when ranks {ids} are all present and the rules allow a straight of {length} cards on them (Ace is 1 or 14)"
            bias = -(4.0 + (1.0 if gap else 0.0))
            b.define(name, "patterns", meaning, [(b.unit(expr, bias), 1.0)])
            patterns.append((name, ranks))
    for r in RANKS:
        names = [name for name, ranks in patterns if r in ranks]
        b.define(
            f"cov_{r}",
            "patterns",
            f"1 when rank id {r} is in a straight",
            at_least(b, total_of(names), 1.0),
        )
    b.define(
        "straight",
        "patterns",
        "1 when the cards hold a straight",
        at_least(b, total_of([name for name, _ in patterns]), 1.0),
    )
    for k in cards:
        b.define(
            f"ic_{k}",
            "patterns",
            f"1 when card {k} is a card of the straight",
            [(b.unit({f"rk_{k}_{r}": 1.0, f"cov_{r}": 1.0}, -1.0), 1.0) for r in RANKS],
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
    b.define(
        "flush",
        "shape",
        "1 when the cards hold a flush",
        at_least(b, total_of([f"flq_{s}" for s in SUITS]), 1.0),
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
        "Pair": unit({"pair": 1.0, "flush": -1.0, "straight": -1.0}),
        "HighCard": unit(total_of(rank + ["flush", "straight"], -1.0), 1.0),
    }
    for t in TYPES:
        b.define(f"t_{t}", "type", f"1 when the hand type is {t}", one_hot[t])


def stage_score(b: Builder) -> None:
    whole = ["FlushFive", "FlushHouse", "FiveOfAKind", "FullHouse"]
    same_rank = ["FourOfAKind", "ThreeOfAKind", "TwoPair", "Pair"]
    for k in range(1, POSITIONS + 1):
        b.define(
            f"xf_{k}",
            "score",
            f"1 when card {k} is a card of the flush and the hand scores the flush: Flush or Straight Flush",
            [
                (b.unit({f"t_{t}": 1.0, f"fcd_{k}": 1.0}, -1.0), 1.0)
                for t in ("Flush", "StraightFlush")
            ],
        )
        b.define(
            f"xs_{k}",
            "score",
            f"1 when card {k} is a card of the straight and the hand scores the straight: Straight or Straight Flush",
            [
                (b.unit({f"t_{t}": 1.0, f"ic_{k}": 1.0}, -1.0), 1.0)
                for t in ("Straight", "StraightFlush")
            ],
        )
        b.define(
            f"xm_{k}",
            "score",
            f"1 when card {k} shares its rank with another card and the hand type scores the rank groups",
            [
                (
                    b.unit(
                        {f"pl_{k}": 1.0, **total_of([f"t_{t}" for t in same_rank])},
                        -1.0,
                    ),
                    1.0,
                )
            ],
        )
        joined = merged(
            {f"P{k}_st": 1.0, f"xm_{k}": 1.0, f"xf_{k}": 1.0, f"xs_{k}": 1.0},
            total_of([f"t_{t}" for t in whole]),
        )
        terms = [
            (b.unit(joined), 1.0),
            (b.unit(joined, -1.0), -1.0),
            (b.unit({f"top_{k}": 1.0, "t_HighCard": 1.0}, -1.0), 1.0),
        ]
        b.define(
            f"s0_{k}",
            "score",
            f"1 when card {k} scores by the hand type: a Stone, the cards of its groups, flush or straight, the five cards of a five-card type, or the High Card",
            terms,
        )
        b.define(
            f"sc_{k}",
            "score",
            f"1 when card {k} scores: by the hand type, or any played card with Splash",
            [
                (b.unit({f"s0_{k}": 1.0}), 1.0),
                (b.unit({"r_sp": 1.0, f"P{k}_nl": -1.0, f"s0_{k}": -1.0}), 1.0),
            ],
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
            at_least(b, {f"ev_{j}": 1.0}, 16.0),
        )
    b.define(
        "cs",
        "situation",
        "1 when the hand type was already played this round",
        [(b.unit({f"rs_{t}": 1.0, f"t_{t}": 1.0}, -1.0), 1.0) for t in TYPES],
    )
    b.define(
        "sz4",
        "situation",
        "1 when exactly four cards are played",
        [(b.unit({"P5_nl": 1.0, "P4_nl": -1.0}), 1.0)],
    )
    b.define(
        "rcur",
        "situation",
        "plays of the hand type this run, before this hand",
        [
            (b.unit({f"run_{t}": 1.0, f"t_{t}": RUN_GATE}, -RUN_GATE), 1.0)
            for t in TYPES
        ],
    )
    for t in TYPES:
        b.define(
            f"ob_{t}",
            "situation",
            f"1 when {t} was played this run at least as often as the hand type, counting this hand",
            at_least(b, {f"run_{t}": 1.0, "rcur": -1.0}, 1.0),
        )
    b.define(
        "ob",
        "situation",
        "1 when another hand type was played at least as often as this one, counting this hand (Obelisk)",
        at_least(b, total_of([f"ob_{t}" for t in TYPES]), 1.0),
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
                [(b.unit({f"sc_{k}": 1.0, f"Q{k}_own_{s}": 1.0}, -1.0), 1.0)],
            )
        b.define(
            f"wd_{k}",
            "suits",
            f"1 when card {k} scores and is Wild",
            [(b.unit({f"sc_{k}": 1.0, f"Q{k}_wd": 1.0}, -1.0), 1.0)],
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
        "fpPlain",
        "suits",
        "1 when plain suits plus Wild cards reach all four suits (Flower Pot without Smeared)",
        at_least(b, covered, 4.0),
    )
    # with Smeared a plain red card fills Hearts, then Diamonds, a plain black card Spades, then Clubs
    for name, pair, meaning in (("rd", ("H", "D"), "red"), ("bl", ("S", "C"), "black")):
        plain = total_of([f"pk_{k}_{s}" for k in range(1, POSITIONS + 1) for s in pair])
        for n in (1, 2):
            b.define(
                f"{name}{n}",
                "suits",
                f"1 when {n} or more scoring plain cards are {meaning}",
                at_least(b, plain, float(n)),
            )
    smeared = merged(total_of(["rd1", "rd2", "bl1", "bl2"]), wilds)
    b.define(
        "fpSmeared",
        "suits",
        "1 when the suits the plain cards fill under Smeared plus Wild cards reach all four (Flower Pot with Smeared)",
        at_least(b, smeared, 4.0),
    )
    b.define(
        "fp",
        "suits",
        "1 when Flower Pot fires",
        [
            (b.unit({"fpPlain": 1.0, "r_sm": -1.0}), 1.0),
            (b.unit({"fpSmeared": 1.0, "r_sm": 1.0}, -1.0), 1.0),
        ],
    )
    for s in SUITS:
        b.define(
            f"prc_{s}",
            "suits",
            f"1 when a scoring plain card covers suit {s} for Seeing Double: its own, or its partner suit with Smeared",
            [
                (b.unit({f"pr_{s}": 1.0}), 1.0),
                (
                    b.unit(
                        {f"pr_{PARTNER[s]}": 1.0, "r_sm": 1.0, f"pr_{s}": -1.0}, -1.0
                    ),
                    1.0,
                ),
            ],
        )
    covered = merged(total_of([f"prc_{s}" for s in SUITS]), wilds)
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
        at_least(b, merged({"prc_C": 1.0}, wilds), 1.0),
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
    def fires(j: int, key: str, cond: str | None) -> int:
        if cond is None:
            return b.unit({holds(j, key): 1.0})
        return b.unit({holds(j, key): 1.0, cond.format(j=j): 1.0}, -1.0)

    for j in range(1, JOKERS + 1):
        value = f"ev_{j}"
        chips = [(b.unit({f"j{j}_e_Foil": 1.0}), 50.0)]
        chips += [
            (fires(j, key, cond), float(amount))
            for key, (cond, amount) in ADD_CHIPS.items()
        ]
        for key in VALUE_CHIPS:
            gate = {value: 1.0, holds(j, key): VALUE_GATE}
            chips.append((b.unit(gate, -VALUE_GATE), 1.0))
        for key, (cond, amount) in VALUE_PLUS_CHIPS.items():
            gate = {value: 1.0, cond: float(amount), holds(j, key): VALUE_GATE}
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
            gate = {value: 1.0, holds(j, key): VALUE_GATE}
            mult.append((b.unit(gate, -VALUE_GATE), 1.0))
        for key, (cond, amount) in VALUE_PLUS_MULT.items():
            gate = {value: 1.0, cond: float(amount), holds(j, key): VALUE_GATE}
            mult.append((b.unit(gate, -VALUE_GATE), 1.0))
        green = {value: 1.0, holds(j, GREEN_KEY): VALUE_GATE}
        mult.append((b.unit(green, 1.0 - VALUE_GATE), 1.0))
        bus = {value: 1.0, holds(j, BUS_KEY): VALUE_GATE, "nf": VALUE_GATE}
        mult.append((b.unit(bus, 1.0 - 2 * VALUE_GATE), 1.0))
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


def stage_order(b: Builder) -> None:
    for k in range(1, POSITIONS + 1):
        b.define(
            f"sf_{k}",
            "order",
            f"1 when card {k} scores and is a face card",
            [(b.unit({f"sc_{k}": 1.0, f"face_{k}": 1.0}, -1.0), 1.0)],
        )
    b.define(
        "nf",
        "order",
        "1 when no scoring card is a face card (Ride the Bus)",
        [
            (
                b.unit(
                    total_of([f"sf_{k}" for k in range(1, POSITIONS + 1)], -1.0),
                    1.0,
                ),
                1.0,
            )
        ],
    )
    for k in range(1, POSITIONS + 1):
        before = range(1, k)
        faces = total_of([f"sf_{m}" for m in before], -1.0)
        scores = total_of([f"sc_{m}" for m in before], -1.0)
        b.define(
            f"ff_{k}",
            "order",
            f"1 when card {k} is the first scoring face card",
            [(b.unit({f"sf_{k}": 1.0, **faces}), 1.0)],
        )
        b.define(
            f"fs_{k}",
            "order",
            f"1 when card {k} is the first scoring card",
            [(b.unit({f"sc_{k}": 1.0, **scores}), 1.0)],
        )


# a suit match or rank set is read after a Midas Mask has acted, a face card as isFace sees it
def card_dim(k: int, feat: str) -> str:
    if feat == "fc":
        return f"face_{k}"
    return f"Q{k}_{feat}"


def stage_scored(b: Builder) -> None:
    for j in range(1, JOKERS + 1):
        for k in range(1, POSITIONS + 1):
            suit = [
                (b.unit({f"ets_{s}_{j}": 1.0, f"Q{k}_sm_{s}": 1.0}, -1.0), 1.0)
                for s in SUITS
            ]
            b.define(
                f"tm_{j}_{k}",
                "scored",
                f"1 when card {k} matches the suit joker {j} names",
                suit,
            )
            b.define(
                f"rm_{j}_{k}",
                "scored",
                f"1 when card {k} has the rank id joker {j} names",
                equals(b, {f"Q{k}_rr": 1.0, f"etid_{j}": -1.0}, 0.0),
            )
    for j in range(1, JOKERS + 1):
        for k in range(1, POSITIONS + 1):
            add = [
                (
                    b.unit({holds(j, key): 1.0, card_dim(k, feat): 1.0}, -1.0),
                    float(amount),
                )
                for key, (feat, amount) in CARD_ADD_MULT.items()
            ]
            b.define(
                f"pa_{j}_{k}",
                "scored",
                f"mult the joker in slot {j} adds in one pass of card {k}",
                add,
            )
            chips = [
                (
                    b.unit({holds(j, key): 1.0, card_dim(k, feat): 1.0}, -1.0),
                    float(amount),
                )
                for key, (feat, amount) in CARD_ADD_CHIPS.items()
            ]
            b.define(
                f"pc_{j}_{k}",
                "scored",
                f"chips the joker in slot {j} adds in one pass of card {k}",
                chips,
            )
            idol = {
                holds(j, "j_idol"): 1.0,
                f"rm_{j}_{k}": 1.0,
                f"tm_{j}_{k}": 1.0,
            }
            double = [
                (b.unit({holds(j, "j_triboulet"): 1.0, f"Q{k}_kq": 1.0}, -1.0), 1.0),
                (b.unit({holds(j, "j_photograph"): 1.0, f"ff_{k}": 1.0}, -1.0), 1.0),
                (b.unit(idol, -2.0), 1.0),
            ]
            b.define(
                f"px2_{j}_{k}",
                "scored",
                f"1 when the joker in slot {j} doubles mult in a pass of card {k}",
                double,
            )
            ancient = {holds(j, "j_ancient"): 1.0, f"tm_{j}_{k}": 1.0}
            b.define(
                f"px15_{j}_{k}",
                "scored",
                f"1 when the joker in slot {j} multiplies mult by 1.5 in a pass of card {k}",
                [(b.unit(ancient, -1.0), 1.0)],
            )


def one_hot(
    b: Builder, name: str, source: str, top: int, stage: str, meaning: str
) -> None:
    for value in range(top + 1):
        b.define(
            f"{name}_{value}",
            stage,
            f"1 when {meaning} is {value}",
            equals(b, {source: 1.0}, float(value)),
        )


def powers(e2: int, e15: int, p: int) -> tuple[float, float]:
    a = Fraction(2**e2) * Fraction(3, 2) ** e15
    return float(a**p), float(sum(a**i for i in range(p)))


# p passes of mult -> a * mult + c, with a = 2^e2 * 1.5^e15, as a sum of gated copies of mult and c
def fold_passes(
    b: Builder,
    tag: str,
    stage: str,
    previous: str,
    c: str,
    c_gate: float,
    gates: dict[tuple[int, int, int], str],
    idle: str,
    name: str,
    meaning: str,
) -> None:
    keep = f"gm_{tag}_0"
    b.define(
        keep,
        stage,
        f"mult before {tag} when it has no pass",
        [(b.unit({previous: 1.0, idle: GATE}, -GATE), 1.0)],
    )
    terms = [(b.unit({keep: 1.0}), 1.0)]
    for (e2, e15, p), gate in gates.items():
        scale, carry = powers(e2, e15, p)
        copy = f"gm_{tag}_{e2}_{e15}_{p}"
        b.define(
            copy,
            stage,
            f"mult before {tag} when it has {p} passes of 2^{e2} * 1.5^{e15}",
            [(b.unit({previous: 1.0, gate: GATE}, -GATE), 1.0)],
        )
        add = f"gc_{tag}_{e2}_{e15}_{p}"
        b.define(
            add,
            stage,
            f"per-pass add of {tag} when it has {p} passes of 2^{e2} * 1.5^{e15}",
            [(b.unit({c: 1.0, gate: c_gate}, -c_gate), 1.0)],
        )
        terms += [(b.unit({copy: 1.0}), scale), (b.unit({add: 1.0}), carry)]
    b.define(name, stage, meaning, terms)


def stage_passes(b: Builder) -> None:
    for k in range(1, POSITIONS + 1):
        terms = [(b.unit({f"sc_{k}": 1.0}), 1.0), (b.unit({f"sr_{k}": 1.0}), 1.0)]
        for j in range(1, JOKERS + 1):
            hack = {holds(j, "j_hack"): 1.0, f"sc_{k}": 1.0, f"Q{k}_hk": 1.0}
            dusk = {holds(j, "j_dusk"): 1.0, f"sc_{k}": 1.0, "hz": 1.0}
            terms += [
                (b.unit({holds(j, "j_selzer"): 1.0, f"sc_{k}": 1.0}, -1.0), 1.0),
                (
                    b.unit({holds(j, "j_sock_and_buskin"): 1.0, f"sf_{k}": 1.0}, -1.0),
                    1.0,
                ),
                (b.unit(hack, -2.0), 1.0),
                (b.unit(dusk, -2.0), 1.0),
                (b.unit({holds(j, "j_hanging_chad"): 1.0, f"fs_{k}": 1.0}, -1.0), 2.0),
            ]
        b.define(
            f"n_{k}",
            "passes",
            f"passes of card {k}: itself, its Red Seal and each retrigger; 0 when it does not score",
            terms,
        )
        one_hot(b, f"nq_{k}", f"n_{k}", PASSES, "passes", f"the passes of card {k}")
        chips = [(b.unit({f"Q{k}_cv": 1.0}), 1.0)]
        chips += [(b.unit({f"pc_{j}_{k}": 1.0}), 1.0) for j in range(1, JOKERS + 1)]
        b.define(
            f"pch_{k}",
            "passes",
            f"chips of one pass of card {k}: its own and its jokers'",
            chips,
        )
        own = [
            (b.unit({f"Q{k}_a": 4.0}), 1.0),
            (b.unit({f"P{k}_ho": 10.0}), 1.0),
            (b.unit({f"Q{k}_a": 1.0, f"P{k}_po": 1.0}, -1.0), 2.0),
        ]
        b.define(
            f"cc_{k}_0", "passes", f"mult card {k} adds in one pass by itself", own
        )
        b.define(
            f"ce2_{k}_0",
            "passes",
            f"doublings in one pass of card {k} by itself",
            [(b.unit({f"Q{k}_gl": 1.0}), 1.0)],
        )
        b.define(
            f"ce15_{k}_0",
            "passes",
            f"factors 1.5 in one pass of card {k} by itself",
            [(b.unit({f"P{k}_po": 1.0}), 1.0)],
        )
        for j in range(1, JOKERS + 1):
            prev = f"cc_{k}_{j - 1}"
            x2, x15 = f"px2_{j}_{k}", f"px15_{j}_{k}"
            step = [
                (b.unit({prev: 1.0}), 1.0),
                (b.unit({f"pa_{j}_{k}": 1.0}), 1.0),
                (b.unit({prev: 1.0, x2: PASS_GATE}, -PASS_GATE), 1.0),
                (b.unit({prev: 1.0, x15: PASS_GATE}, -PASS_GATE), 0.5),
            ]
            b.define(
                f"cc_{k}_{j}",
                "passes",
                f"mult per pass of card {k} once joker {j} has acted",
                step,
            )
            for tag, bit, what in (
                ("ce2", x2, "doublings"),
                ("ce15", x15, "factors 1.5"),
            ):
                b.define(
                    f"{tag}_{k}_{j}",
                    "passes",
                    f"{what} in one pass of card {k} once joker {j} has acted",
                    [
                        (b.unit({f"{tag}_{k}_{j - 1}": 1.0}), 1.0),
                        (b.unit({bit: 1.0}), 1.0),
                    ],
                )
        one_hot(
            b,
            f"q2_{k}",
            f"ce2_{k}_{JOKERS}",
            DOUBLES,
            "passes",
            f"the doublings per pass of card {k}",
        )
        one_hot(
            b,
            f"q15_{k}",
            f"ce15_{k}_{JOKERS}",
            HALVES,
            "passes",
            f"the factors 1.5 per pass of card {k}",
        )
        for e2 in range(DOUBLES + 1):
            for e15 in range(HALVES + 1):
                for p in range(1, PASSES + 1):
                    shape = {
                        f"q2_{k}_{e2}": 1.0,
                        f"q15_{k}_{e15}": 1.0,
                        f"nq_{k}_{p}": 1.0,
                    }
                    shape[settled(b, list(shape))] = 1.0
                    b.define(
                        f"pg_{k}_{e2}_{e15}_{p}",
                        "passes",
                        f"1 when card {k} has {p} passes of 2^{e2} * 1.5^{e15}, once the shape has settled",
                        [(b.unit(shape, -3.0), 1.0)],
                    )


def stage_playedfold(b: Builder) -> str:
    previous = "mult0"
    for k in range(1, POSITIONS + 1):
        gates = {
            (e2, e15, p): f"pg_{k}_{e2}_{e15}_{p}"
            for e2 in range(DOUBLES + 1)
            for e15 in range(HALVES + 1)
            for p in range(1, PASSES + 1)
        }
        name = f"mp_{k}"
        fold_passes(
            b,
            f"c{k}",
            "playedfold",
            previous,
            f"cc_{k}_{JOKERS}",
            PASS_GATE,
            gates,
            f"nq_{k}_0",
            name,
            f"mult after card {k} and all its passes",
        )
        previous = name
    return previous


def stage_held(b: Builder, previous: str) -> None:
    for s in range(CARD_SLOTS):
        members = total_of([f"m{k}_{s}" for k in range(1, POSITIONS + 1)], -1.0)
        b.define(
            f"hd_{s}",
            "held",
            f"1 when hand slot {s} holds a card that is not played",
            [(b.unit({f"H{s}_card": 1.0, **members}), 1.0)],
        )
        b.define(
            f"hr_{s}",
            "held",
            f"1 when hand slot {s} is held and not a Stone",
            [(b.unit({f"hd_{s}": 1.0, f"H{s}_ns": 1.0}, -1.0), 1.0)],
        )
    for s in range(CARD_SLOTS):
        for t in range(CARD_SLOTS):
            if t != s:
                # Raised Fist keeps the last lowest card: an earlier card blocks it when lower, a later one when not higher
                c = 1.0 if t < s else 0.0
                expr = {
                    f"H{s}_rank": 1.0,
                    f"H{t}_rank": -1.0,
                    f"hr_{t}": RANK_GATE,
                }
                steps = [
                    (b.unit(expr, 1.0 - c - RANK_GATE), 1.0),
                    (b.unit(expr, -c - RANK_GATE), -1.0),
                ]
                b.define(
                    f"hx_{s}_{t}",
                    "held",
                    f"1 when held slot {t} stops Raised Fist from choosing slot {s}",
                    steps,
                )
        blocked = total_of([f"hx_{s}_{t}" for t in range(CARD_SLOTS) if t != s], -1.0)
        b.define(
            f"hf_{s}",
            "held",
            f"1 when slot {s} is the last lowest held card that is not a Stone",
            [(b.unit({f"hr_{s}": 1.0, **blocked}), 1.0)],
        )
    for s in range(CARD_SLOTS):
        b.define(
            f"he_{s}_0",
            "held",
            f"factors 1.5 in one held pass of slot {s} by itself: Steel",
            [(b.unit({f"hd_{s}": 1.0, f"H{s}_steel": 1.0}, -1.0), 1.0)],
        )
        for j in range(1, JOKERS + 1):
            baron = {holds(j, "j_baron"): 1.0, f"hd_{s}": 1.0, f"H{s}_king": 1.0}
            shoot = {
                holds(j, "j_shoot_the_moon"): 1.0,
                f"hd_{s}": 1.0,
                f"H{s}_queen": 1.0,
            }
            fist = {
                f"H{s}_nom": 2.0,
                holds(j, "j_raised_fist"): 22.0,
                f"hf_{s}": 22.0,
            }
            b.define(
                f"hb_{j}_{s}",
                "held",
                f"1 when the joker in slot {j} multiplies a held pass of slot {s} by 1.5 (Baron)",
                [(b.unit(baron, -2.0), 1.0)],
            )
            b.define(
                f"ha_{j}_{s}",
                "held",
                f"mult the joker in slot {j} adds in a held pass of slot {s} (Shoot the Moon, Raised Fist)",
                [(b.unit(shoot, -2.0), 13.0), (b.unit(fist, -44.0), 1.0)],
            )
            prev = f"hc_{s}_{j - 1}"
            step = [(b.unit({f"ha_{j}_{s}": 1.0}), 1.0)]
            if j > 1:
                step += [
                    (b.unit({prev: 1.0}), 1.0),
                    (b.unit({prev: 1.0, f"hb_{j}_{s}": HELD_GATE}, -HELD_GATE), 0.5),
                ]
            b.define(
                f"hc_{s}_{j}",
                "held",
                f"mult per held pass of slot {s} once joker {j} has acted",
                step,
            )
            b.define(
                f"he_{s}_{j}",
                "held",
                f"factors 1.5 in one held pass of slot {s} once joker {j} has acted",
                [
                    (b.unit({f"he_{s}_{j - 1}": 1.0}), 1.0),
                    (b.unit({f"hb_{j}_{s}": 1.0}), 1.0),
                ],
            )
        passes = [
            (b.unit({f"hd_{s}": 1.0}), 1.0),
            (b.unit({f"hd_{s}": 1.0, f"H{s}_red": 1.0}, -1.0), 1.0),
        ]
        passes += [
            (b.unit({f"hd_{s}": 1.0, holds(j, "j_mime"): 1.0}, -1.0), 1.0)
            for j in range(1, JOKERS + 1)
        ]
        b.define(
            f"hn_{s}",
            "held",
            f"held passes of slot {s}: itself, its Red Seal and each Mime; 0 when not held",
            passes,
        )
        one_hot(
            b,
            f"hnq_{s}",
            f"hn_{s}",
            HELD_PASSES,
            "held",
            f"the held passes of slot {s}",
        )
        one_hot(
            b,
            f"hq_{s}",
            f"he_{s}_{JOKERS}",
            HELD_HALVES,
            "held",
            f"the factors 1.5 per held pass of slot {s}",
        )
        for e15 in range(HELD_HALVES + 1):
            for p in range(1, HELD_PASSES + 1):
                shape = {f"hq_{s}_{e15}": 1.0, f"hnq_{s}_{p}": 1.0}
                shape[settled(b, list(shape))] = 1.0
                b.define(
                    f"hg_{s}_{e15}_{p}",
                    "held",
                    f"1 when slot {s} has {p} held passes of 1.5^{e15}, once the shape has settled",
                    [(b.unit(shape, -2.0), 1.0)],
                )
    for s in range(CARD_SLOTS):
        gates = {
            (0, e15, p): f"hg_{s}_{e15}_{p}"
            for e15 in range(HELD_HALVES + 1)
            for p in range(1, HELD_PASSES + 1)
        }
        name = f"mh_{s}"
        fold_passes(
            b,
            f"h{s}",
            "heldfold",
            previous,
            f"hc_{s}_{JOKERS}",
            HELD_GATE,
            gates,
            f"hnq_{s}_0",
            name,
            f"mult after held slot {s} and all its passes",
        )
        previous = name
    b.define(
        "multCards",
        "heldfold",
        "mult after the played cards and the held cards",
        [(b.unit({previous: 1.0}), 1.0)],
    )


def stage_chips(b: Builder) -> None:
    terms = []
    for k in range(1, POSITIONS + 1):
        for p in range(1, PASSES + 1):
            gate = {f"pch_{k}": 1.0, f"nq_{k}_{p}": CHIPS_GATE}
            terms.append((b.unit(gate, -CHIPS_GATE), float(p)))
    b.define("chipsCards", "chips", "chips of the scoring cards, once per pass", terms)
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
    chip_base = 2 ** (CHIP_RADIX_BITS * (CHIP_DIGITS - 1))
    rows = [
        ("mult after every fold stage and joker slot", MULT_CAP, FRAC),
        ("gated pre-activation in a played or held fold stage", 2 * int(GATE), FRAC),
        ("mult one card pass adds, with its factors", PASS_MULT_CAP, HALVES),
        ("gated pre-activation in a card pass step", 2 * int(PASS_GATE), HALVES + 1),
        ("mult one held pass adds, with its factors", HELD_MULT_CAP, HELD_HALVES),
        (
            "gated pre-activation in a held pass step",
            2 * int(HELD_GATE),
            HELD_HALVES + 1,
        ),
        (
            "gated pre-activation in the shift-and-add stage of a joker factor",
            2 * int(SHIFT_GATE),
            FRAC,
        ),
        (
            "gated pre-activation in the Baseball Card and Polychrome stage",
            2 * int(COPY_GATE),
            FRAC + POLY_COPIES,
        ),
        ("chips", CHIPS_CAP, 0),
        ("gated pre-activation of one pass of chips", 2 * int(CHIPS_GATE), 0),
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
            f"floor step pre-activation, in units of 2^-{FRAC}",
            int(RESOLUTION) * PRODUCT_CAP,
            0,
        ),
        ("chips digit step times its weight", chip_base * (CHIPS_CAP + 1), 0),
        ("floor digit times its weight", 256**FLOOR_DIGITS, 0),
        ("selection key", 2 * 256 * PRODUCT_CAP, 0),
    ]
    for label, peak, bits in rows:
        assert peak * 2**bits <= 2**52, label
    assert PRODUCT_CAP < 256**FLOOR_DIGITS
    assert MULT_CAP * CHIPS_CAP >= PRODUCT_CAP
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
        "with any enhancement, edition and seal and up to 5 jokers (main-phase, per-card, counters, input",
        "factors, Midas Mask, Blueprint, Brainstorm, Baseball Card and the rule jokers), no boss and",
        "no Plasma Deck balance, and reports its total and hand type.",
        "Every dim is a pure function of the dims and fetches of the loop before, so after `loops` loops",
        "all of them hold their final value. Attention weights are exactly 0, 1, or 1/8, and every",
        f"stage is exact float64 arithmetic on integers and multiples of 2^-{FRAC}.",
        "",
        f"- residual width d = {data['d']}, hidden units = {data['hidden']}, loops = {data['loops']}",
        f"- nonzero weights (matrices and biases) = {nonzeros}",
        f"- levels accepted: mult whole 1..{MMAX}, chips whole 0..{CMAX}",
        f"- hands accepted: the encoder bounds from the cards, jokers and levels alone the mult (level mult plus every add, times every doubling and factor 1.5, then the main-phase jokers), the chips and their product; at most {MULT_CAP}, {CHIPS_CAP} and {PRODUCT_CAP}, and at most {WMAX} factors 1.5 in all (Polychrome, Steel, Baron, Ancient Joker, Baseball Card) plus the fraction bits of the input factors, each counted once per pass",
        f"  a card has at most {PASSES} passes (itself, a Red Seal, retriggers), {DOUBLES} doublings and {HALVES} factors 1.5 per pass; a held card at most {HELD_PASSES} passes and {HELD_HALVES} factors 1.5 per pass; at most five cards are played, and Hanging Chad and Photograph act on one card",
        f"- jokers accepted: at most {JOKERS}, none debuffed, every key in the lists below, value a whole number 0..{VALUE_MAX} (for Ice Cream and Popcorn the number in their text); hands left and discards left whole 0..{LEFT_MAX}; plays of a hand type this run and this round whole 0..{PLAYS_MAX}",
        f"- input factors: the encoder derives a factor from the joker as the reference does (the value; Obelisk value + 0.2; Ramen from its text), and it counts as no effect when it is 1 or less. A factor is accepted only when 4 times it is a whole number and it is at most {FMAX}, so 1.25, 1.5, 2.5 and 8 are in scope and 1.1, 1.2, 8.25 and 9 are not (the hand is declined). Ramen, whose factor is rebuilt by subtracting 0.01 from 2 once per discarded card, is in scope only for the text X2 (no discards) and for a factor of 1 or less; every other Ramen factor is not a multiple of 1/4 and declines the hand. The net multiplies by it with shift-and-add over the {K_BITS} bits of 4 times the factor, then divides by 4",
        "- copies: Blueprint copies the slot to its right, Brainstorm the first slot, resolved as `resolve` in Scoring.fs (a chain ends at a joker that is not a copy; a loop, the last slot or a missing target gives nothing); a copy takes the key, value and text of its target and keeps its own edition; Baseball Card multiplies each Uncommon slot by 1.5 per slot that acts as it, after that slot's own effect and before its Polychrome",
        "- Midas Mask turns every scoring face card Gold before scoring: the card loses the effect of its enhancement and counts as a plain card of its suit; the hand type is read before",
        "- rule jokers act when a slot holds the key itself (`JokerRegistry.rules` reads the raw keys, so a Blueprint or Brainstorm copying one adds nothing); the net reads each rule bit from the five key one-hots:",
        "  - Four Fingers: a flush needs 4 matching cards, a straight 4 ranks; the scoring cards are the matching cards of the first suit in the order Spades, Hearts, Clubs, Diamonds that reaches the count, and every card whose rank is in the straight; Straight Flush scores the union",
        "  - Shortcut: a straight may skip one missing rank between two present ranks (an Ace is rank 1 or 14); the net tests every set of 4 or 5 ranks whose steps are 1 or 2, gated by the two rule bits, and a card scores when its rank is in a set that holds",
        "  - Smeared: a card matches the other suit of its colour, in flushes and in every suit test; Flower Pot then counts a scoring red card as Hearts, then Diamonds, a black one as Spades, then Clubs, as the reference fills the first empty suit",
        "  - Splash: every played card scores",
        "  - Pareidolia: every card is a face card, a Stone included; a Midas Mask then turns a Stone Gold too, so the card scores as a card of its rank and suit",
        "- the clock `tick` counts the loops; a gate that selects a fold of passes fires only once its inputs have settled, so no half-settled state can feed the folds a mult beyond their bounds",
        "- computed dims on non-candidate tokens hold values nobody reads",
        "",
        "## Joker keys in scope",
        "",
        "With a score effect once per hand: "
        + ", ".join(f"`{key}`" for key in MAIN_KEYS),
        "",
        "With a score effect per card (Ancient Joker and Idol read their target from the text): "
        + ", ".join(f"`{key}`" for key in CARD_KEYS),
        "",
        "Multiplying mult by an input factor: "
        + ", ".join(f"`{key}`" for key in [*FACTOR_KEYS, OBELISK_KEY]),
        "",
        "Copies and the rest: "
        + ", ".join(f"`{key}`" for key in [*COPY_KEYS, BASEBALL_KEY, MIDAS_KEY]),
        "",
        "Rule jokers: " + ", ".join(f"`{key}`" for key in RULE_KEYS),
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
    stage_clock(b)
    stage_card(b)
    stage_copies(b)
    stage_effective(b)
    stage_rules(b)
    heads = position_heads(b) + [nonblack_head(b)] + held_heads(b)
    stage_midas(b)
    stage_pairs(b)
    stage_patterns(b)
    stage_shape(b)
    stage_type(b)
    stage_score(b)
    stage_situation(b)
    stage_contained(b)
    stage_suits(b)
    stage_order(b)
    stage_effects(b)
    stage_factors(b)
    stage_scored(b)
    stage_passes(b)
    stage_held(b, stage_playedfold(b))
    stage_chips(b)
    stage_digits(b)
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
            "multCap": MULT_CAP,
            "chipsCap": CHIPS_CAP,
            "productCap": PRODUCT_CAP,
            "doubles": DOUBLES,
            "halves": HALVES,
            "passes": PASSES,
            "heldHalves": HELD_HALVES,
            "heldPasses": HELD_PASSES,
            "jokers": JOKERS,
            "factor": FMAX,
            "value": VALUE_MAX,
            "left": LEFT_MAX,
            "plays": PLAYS_MAX,
        },
        "keys": KEY_DIMS,
        "main": MAIN_KEYS,
        "factors": FACTOR_KEYS,
        "uncommon": UNCOMMON_KEYS,
        "addsMult": list(CARD_ADD_MULT),
        "addsChips": list(CARD_ADD_CHIPS),
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
