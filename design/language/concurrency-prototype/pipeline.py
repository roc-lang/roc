"""Build a program through ARC and both lowerings, with every static check."""

from __future__ import annotations

from arc import Arc, certify
from combinators import BUILTINS
from host import SUSPENDING
from ir import Program, parse
from lower import Lowering


def build(user, entry="main"):
    """`user` is IR source text or a dict of procedures."""
    procs = dict(BUILTINS)
    procs.update(parse(user) if isinstance(user, str) else user)
    arc = Arc(Program(procs, entry))
    certify(arc)
    lows = {}
    for mode in ("lss", "boxy"):
        low = Lowering(arc, SUSPENDING, mode)
        low.certify()
        lows[mode] = low
    return arc, lows
