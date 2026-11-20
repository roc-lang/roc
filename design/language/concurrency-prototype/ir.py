"""The prototype's IR: a small first-order language in A-normal form.

A procedure body is a block: a list of statements and one terminator. Branches
are terminators, so every block is a tree and the code after any statement is
the rest of its own block. That is what makes a resume entry easy to state:
"continue at statement i+1 of this block".

Statements
    ('let', x, expr)          bind x; every variable argument of expr is consumed
    ('dup', x)                one more reference (inserted by ARC)
    ('drop', x)               release a reference (inserted by ARC)

Expressions (arguments are variables)
    ('int', n) ('str', s)
    ('prim', op, [a, b])      op in add sub lt eq
    ('tup', [a...]) ('tag', name, [a...]) ('list', [a...])
    ('field', x, i) ('len', x) ('get', x, i) ('set', x, i, v) ('push', x, v)
    ('clo', f, [captures])    a closure; calling it passes captures, then args
    ('call', f, [args])       direct call
    ('callc', c, [args], eff) call through a closure value (erased under .boxy)
    ('host', name, [args])    hosted call; the host owns the arguments
    ('spawn', c)              task_spawn: start a child running closure c
    ('await', kids)           task_await_any over a list of child handles
    ('cancel', h)             task_cancel
    ('group', p)              enter a concurrent loop: start the running set
                              of plan p (RFC, "Concurrent loops"); the
                              result names it and never escapes the loop
    ('next', g)               the loop head: wait for the next arrival;
                              Arrived(v), Crashed(m), Cancelled or Ended
    ('spawn_in', g, c)        `spawn` in a loop body: add c to the running set
    ('cancel_group', g)       a loop exit: discard unstarted entries and cancel
                              running ones that are not shielded
    ('pair',)                 a one-shot session channel: (Out, In)
                              (concurrency-sessions.md)
    ('send', o, v)            send v on Out o; never waits
    ('recv', i)               wait for In i's message: Got(v) or Cancelled

Terminators
    ('ret', x)
    ('tail', f, [args])       frame-replacing call
    ('tailc', c, [args])      frame-replacing call through a closure value;
                              under .boxy a deferred erased call (design.md,
                              "Deferred Erased Calls")
    ('if', x, then_block, else_block)      x is an int; nonzero is true
    ('match', x, [(tag, [vars], block)])
    ('crash', x)              x is a Str

Surface syntax is s-expressions; see `parse`. An integer literal in argument
position is bound to a fresh variable first.
"""

from __future__ import annotations

import itertools
import re
from dataclasses import dataclass, field


@dataclass
class Block:
    stmts: list
    term: tuple


@dataclass
class Proc:
    name: str
    params: list
    body: Block
    eff: bool


@dataclass
class Program:
    procs: dict
    entry: str  # an effectful procedure of no arguments, run as the root task

    def copy(self):
        return Program(dict(self.procs), self.entry)


# ---------------------------------------------------------------------------
# S-expression reader
# ---------------------------------------------------------------------------

TOKEN = re.compile(r'\s*(?:(;[^\n]*)|(\()|(\))|("(?:[^"\\]|\\.)*")|([^\s()";]+))')


def read_all(text):
    pos, stack, out = 0, [], []
    while pos < len(text):
        m = TOKEN.match(text, pos)
        if not m or m.end() == pos:
            if text[pos:].strip() == "":
                break
            raise SyntaxError(f"bad input at {text[pos:pos + 20]!r}")
        pos = m.end()
        comment, lp, rp, string, atom = m.groups()
        if comment:
            continue
        if lp:
            stack.append([])
        elif rp:
            done = stack.pop()
            (stack[-1] if stack else out).append(done)
        elif string is not None:
            (stack[-1] if stack else out).append(("STR", string[1:-1]))
        elif atom is not None:
            v = int(atom) if re.fullmatch(r"-?\d+", atom) else atom
            (stack[-1] if stack else out).append(v)
    if stack:
        raise SyntaxError("unbalanced parentheses")
    return out


PRIMS = {"add", "sub", "lt", "eq"}


class Parser:
    def __init__(self):
        self.tmp = itertools.count()

    def fresh(self):
        return f"_k{next(self.tmp)}"

    def args(self, raw, pre):
        out = []
        for a in raw:
            if isinstance(a, int):
                t = self.fresh()
                pre.append(("let", t, ("int", a)))
                out.append(t)
            elif isinstance(a, tuple):  # a string literal
                t = self.fresh()
                pre.append(("let", t, ("str", a[1])))
                out.append(t)
            elif isinstance(a, list):  # a nested expression
                t = self.fresh()
                e = self.expr(a, pre)
                pre.append(("let", t, e))
                out.append(t)
            else:
                out.append(a)
        return out

    def expr(self, e, pre):
        op, *rest = e
        if op == "int":
            return ("int", rest[0])
        if op == "str":
            return ("str", rest[0][1])
        if op in PRIMS:
            return ("prim", op, self.args(rest, pre))
        if op == "tup":
            return ("tup", self.args(rest, pre))
        if op == "tag":
            return ("tag", rest[0], self.args(rest[1:], pre))
        if op == "list":
            return ("list", self.args(rest, pre))
        if op == "field":
            return ("field", rest[0], rest[1])
        if op == "len":
            return ("len", rest[0])
        if op == "get":
            x, i = self.args(rest, pre)
            return ("get", x, i)
        if op == "set":
            x, i, v = self.args(rest, pre)
            return ("set", x, i, v)
        if op == "push":
            x, v = self.args(rest, pre)
            return ("push", x, v)
        if op == "clo":
            return ("clo", rest[0], self.args(rest[1:], pre))
        if op == "call":
            return ("call", rest[0], self.args(rest[1:], pre))
        if op in ("callc", "callc!"):
            return ("callc", rest[0], self.args(rest[1:], pre), op == "callc!")
        if op == "host":
            return ("host", rest[0], self.args(rest[1:], pre))
        if op == "spawn":
            return ("spawn", rest[0])
        if op == "await":
            return ("await", rest[0])
        if op == "cancel":
            return ("cancel", rest[0])
        if op in ("group", "next", "cancel_group"):
            (x,) = self.args(rest, pre)
            return (op, x)
        if op == "spawn_in":
            g, c = self.args(rest, pre)
            return ("spawn_in", g, c)
        if op == "pair":
            return ("pair",)
        if op == "send":
            o, v = self.args(rest, pre)
            return ("send", o, v)
        if op == "recv":
            (i,) = self.args(rest, pre)
            return ("recv", i)
        raise SyntaxError(f"unknown expression {op}")

    def block(self, forms):
        stmts = []
        for f in forms[:-1]:
            if f[0] == "let":
                pre = []
                e = self.expr(f[2], pre)
                stmts += pre + [("let", f[1], e)]
            elif f[0] == "do":
                pre = []
                e = self.expr(f[1], pre)
                stmts += pre + [("let", self.fresh(), e)]
            else:
                raise SyntaxError(f"expected a statement, got {f}")
        t = forms[-1]
        op = t[0]
        if op == "ret":
            if isinstance(t[1], list):  # (ret (expr ...)) binds first
                x = self.fresh()
                pre = []
                e = self.expr(t[1], pre)
                stmts += pre + [("let", x, e)]
                return Block(stmts, ("ret", x))
            pre = []
            (x,) = self.args([t[1]], pre)
            return Block(stmts + pre, ("ret", x))
        if op == "tail":
            pre = []
            a = self.args(t[2:], pre)
            return Block(stmts + pre, ("tail", t[1], a))
        if op == "tailc":
            pre = []
            a = self.args(t[2:], pre)
            return Block(stmts + pre, ("tailc", t[1], a))
        if op == "crash":
            pre = []
            (x,) = self.args([t[1]], pre)
            return Block(stmts + pre, ("crash", x))
        if op == "if":
            return Block(stmts, ("if", t[1], self.block(t[2][1:]), self.block(t[3][1:])))
        if op == "match":
            arms = [(arm[0], list(arm[1]), self.block(arm[2][1:])) for arm in t[2:]]
            return Block(stmts, ("match", t[1], arms))
        raise SyntaxError(f"expected a terminator, got {t}")

    def proc(self, f):
        _, name, params, kind, *body = f
        return Proc(name, list(params), self.block(body), kind == "eff")


def parse(text):
    p = Parser()
    return {pr.name: pr for pr in (p.proc(f) for f in read_all(text))}


# ---------------------------------------------------------------------------
# Traversal and printing
# ---------------------------------------------------------------------------


def blocks(block):
    """Yield every block in a tree, outermost first."""
    yield block
    t = block.term
    if t[0] == "if":
        yield from blocks(t[2])
        yield from blocks(t[3])
    elif t[0] == "match":
        for _, _, b in t[2]:
            yield from blocks(b)


def expr_vars(e):
    k = e[0]
    if k in ("int", "str", "pair"):
        return []
    if k in ("prim", "tup", "list"):
        return list(e[-1])
    if k == "tag":
        return list(e[2])
    if k in ("field", "len", "spawn", "await", "cancel", "group", "next", "cancel_group"):
        return [e[1]]
    if k in ("spawn_in", "send"):
        return [e[1], e[2]]
    if k == "recv":
        return [e[1]]
    if k == "get":
        return [e[1], e[2]]
    if k == "set":
        return [e[1], e[2], e[3]]
    if k == "push":
        return [e[1], e[2]]
    if k in ("clo", "call", "host"):
        return list(e[2])
    if k == "callc":
        return [e[1]] + list(e[2])
    raise ValueError(e)


def show_block(b, ind=1):
    pad = "  " * ind
    out = []
    for s in b.stmts:
        out.append(pad + " ".join(map(str, s)))
    t = b.term
    if t[0] == "if":
        out.append(f"{pad}if {t[1]}:")
        out.append(show_block(t[2], ind + 1))
        out.append(f"{pad}else:")
        out.append(show_block(t[3], ind + 1))
    elif t[0] == "match":
        out.append(f"{pad}match {t[1]}:")
        for tag, vs, blk in t[2]:
            out.append(f"{pad}  {tag}({', '.join(vs)}):")
            out.append(show_block(blk, ind + 2))
    else:
        out.append(pad + " ".join(map(str, t)))
    return "\n".join(out)


def show(prog):
    return "\n\n".join(f"proc {p.name}({', '.join(p.params)}) {'eff' if p.eff else 'pure'}\n{show_block(p.body)}"
                       for p in prog.procs.values())
