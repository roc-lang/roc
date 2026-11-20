"""Type rules against cycles through shared state: the alternative that
concurrency-shared-variables.md rejects. The RFC instead releases a cell's
state when no live task holds it (V9; sessions.py, shared.py). This model
measures what the rules would have rejected.

Reference counting cannot free a cycle, and before `Shared(s)` Roc had none:
values are immutable, so no value can come to hold itself. A cell can: an
update can store, in a cell's state, a value that reaches the cell's own
handle. This file models the types a program can use, decides when a cycle is
possible, and checks those decisions against a heap that counts references.

Types are small composed constructs:

    ('prim',)                         a number
    ('rec', ((field, t), ...))        a record
    ('union', ((tag, (t, ...)), ...)) a tag union; the first tag has no payload
    ('list', t)
    ('shared', s)                     a handle, Shared(s)
    ('fn', ((t, ...), ...))           a function type. Each inner tuple is the
                                      capture types of one lambda that can flow
                                      there: the lambda set. A Roc function type
                                      does not show it
    ('var', a)                        a type parameter
    ('app', name, (t, ...))           a declared type, possibly recursive

Three decisions about whether a type's cells can form a cycle:

  precise    the truth, from the lambda sets: no `shared` node lies on a cycle
             of the type graph, where a function reaches its captures. Exact:
             a type it accepts never leaks, and for a type it rejects,
             `witness` builds a program that leaks.

  checker    what a type checker can decide, since it does not see captures:
             a function reaches nothing, so any function in a state could
             capture any handle, the state's own included. So no state type
             may hold a function (rule 1), and no `shared` node lies on a
             cycle of the graph without captures (rule 2). Run on the
             expanded type, as if every declaration were inlined.

  modular    the checker as a compiler would run it: once per declaration,
             with its parameters as leaves, and each earlier declaration
             replaced by a summary: whether it holds a function, and which of
             its parameters its values hold, and which of those end up
             inside a state. A parameter inside a state
             becomes a requirement on its argument at every use, like a
             static dispatch requirement; `Shared.new!` in generic code is
             the same requirement on a type variable. Rule 2 needs no
             requirement: every cycle passes through a recursive declaration,
             which is checked where it is declared.

The fuzzer generates declarations and types by composing the constructs at
random, then checks, for every type:
    - precise is exact: random programs over an accepted type never leak, and
      the witness for a rejected type always leaks;
    - checker is sound: it accepts nothing that precise rejects;
    - modular equals checker, so checking each declaration once suffices.
It also counts what the checker's conservatism costs: types that precise
accepts and the checker rejects, every one because a state holds a function.

Mutations, each of which the fuzzer must catch:
    no_fn_rule         the checker allows functions in states: unsound
    no_requirements    the modular checker forgets which parameters of an
                       earlier declaration end up inside a state

    python3 cycles.py [programs] [seed]
"""

from __future__ import annotations

import random
import sys
from collections import Counter

PRIM = ("prim",)

MUTATIONS = {"no_fn_rule": False, "no_requirements": False}


# ------------------------------------------------------------------ types


def subst(t, env):
    k = t[0]
    if k == "var":
        return env.get(t[1], t)
    if k == "prim":
        return t
    if k == "rec":
        return ("rec", tuple((n, subst(x, env)) for n, x in t[1]))
    if k == "union":
        return ("union", tuple((g, tuple(subst(x, env) for x in ps)) for g, ps in t[1]))
    if k in ("list", "shared"):
        return (k, subst(t[1], env))
    if k == "fn":
        return ("fn", tuple(tuple(subst(x, env) for x in alt) for alt in t[1]))
    if k == "app":
        return ("app", t[1], tuple(subst(x, env) for x in t[2]))
    raise AssertionError(t)


def unfold(decls, t):
    params, body = decls[t[1]]
    return subst(body, dict(zip(params, t[2])))


def children(t):
    k = t[0]
    if k == "rec":
        return tuple(x for _, x in t[1])
    if k == "union":
        return tuple(x for _, ps in t[1] for x in ps)
    if k in ("list", "shared"):
        return (t[1],)
    return ()


def captures(t):
    return tuple(x for alt in t[1] for x in alt)


def graph(decls, roots, see_captures, summaries=None, group=None):
    """The type graph reachable from roots. A function's captures are edges
    when see_captures, and otherwise roots of their own: the types a closure
    captures are still types the program forms, checked where it forms them.
    For the same reason, so are a declared type's arguments.

    With summaries, an `app` of a declaration outside `group` is not
    unfolded. Its arguments become its successors, each through a
    ('via', ...) node that counts as a state when the summary says that
    parameter ends up inside one, and it counts as a function when the
    summary says the declaration holds one. An argument for a parameter the
    declaration's values never hold, and that no state of it holds either
    (a phantom parameter), is only a root."""
    edges = {}
    stack = list(roots)
    while stack:
        t = stack.pop()
        if t in edges:
            continue
        k = t[0]
        if k == "fn":
            es = captures(t) if see_captures else ()
            if not see_captures:
                stack.extend(captures(t))
        elif k == "app" and summaries is not None and t[1] not in group:
            _, required, _, present = summaries[t[1]]
            es = tuple(("via", t[1], i, a, i in required and not MUTATIONS["no_requirements"])
                       for i, a in enumerate(t[2]) if i in present or i in required)
            stack.extend(t[2])
        elif k == "app":
            es = (unfold(decls, t),)
            # The arguments are types the program writes, even where the
            # declaration ignores a parameter.
            stack.extend(t[2])
        elif k == "via":
            es = (t[3],)
        else:
            es = children(t)
        edges[t] = es
        stack.extend(es)
        assert len(edges) < 50000, "irregular recursion"
    return edges


def reach(edges, starts):
    seen, stack = set(), list(starts)
    while stack:
        t = stack.pop()
        if t not in seen:
            seen.add(t)
            stack.extend(edges[t])
    return seen


def is_state(t):
    return t[0] == "shared" or (t[0] == "via" and t[4])


def holds_fn(t, summaries):
    return t[0] == "fn" or (t[0] == "app" and summaries is not None and t[1] in summaries
                            and summaries[t[1]][2])


def judge(edges, summaries=None, group=None):
    """Rules 1 and 2 over a graph. Returns (ok, why, vars inside a state)."""
    inside = set()
    for s in edges:
        if not is_state(s):
            continue
        r = reach(edges, edges[s])
        if s in r:
            return False, "cycle", set()
        if not MUTATIONS["no_fn_rule"] and any(holds_fn(n, summaries) for n in r):
            return False, "function in a state", set()
        inside |= {n[1] for n in r if n[0] == "var"}
    return True, "", inside


def precise_ok(decls, x):
    edges = graph(decls, [x], True)
    return not any(s[0] == "shared" and s in reach(edges, edges[s]) for s in edges)


def checker_verdict(decls, x):
    ok, why, _ = judge(graph(decls, [x], False))
    return ok, why


def summarize(decls, groups):
    """The modular checker: each group once, in order, with parameters as
    leaves. Returns {name: (ok, indexes of parameters inside a state, holds a
    function, indexes of parameters its values hold)}."""
    summaries = {}
    for group in groups:
        for name in group:
            params, _ = decls[name]
            me = ("app", name, tuple(("var", p) for p in params))
            edges = graph(decls, [me], False, summaries, set(group))
            ok, _, inside = judge(edges, summaries, group)
            # A declaration in a rejected group is rejected, and so is
            # anything that uses one.
            ok = ok and all(summaries[n[1]][0] for n in edges
                            if n[0] == "app" and n[1] in summaries)
            required = {i for i, p in enumerate(params) if p in inside}
            held = reach(edges, [me])
            fn = any(holds_fn(n, summaries) for n in held)
            present = {i for i, p in enumerate(params) if ("var", p) in held}
            summaries[name] = (ok, required, fn, present)
    return summaries


# ------------------------------------------------------------------ the heap


class H:
    """A handle: one reference to cell `cid`."""
    __slots__ = ("cid",)

    def __init__(self, cid):
        self.cid = cid


def handles(v, out):
    if isinstance(v, H):
        out.append(v.cid)
    elif isinstance(v, tuple):
        for x in v:
            handles(x, out)
    return out


class Heap:
    """Cells holding values. Values are immutable trees whose leaves are
    numbers and handles. The program holds some handles directly (`held`)."""

    def __init__(self, decls, rng):
        self.decls, self.rng = decls, rng
        self.state, self.kind = {}, {}
        self.held = {}  # state type -> handles the program holds

    def new(self, s, v):
        cid = len(self.state)
        self.state[cid], self.kind[cid] = v, s
        h = H(cid)
        self.held.setdefault(s, []).append(h)
        return h

    def value(self, t, fuel):
        rng, k = self.rng, t[0]
        if k == "prim":
            return rng.randrange(10)
        if k == "rec":
            return tuple(self.value(x, fuel - 1) for _, x in t[1])
        if k == "union":
            i = 0 if fuel <= 0 else rng.randrange(len(t[1]))
            return (i, tuple(self.value(x, fuel - 1) for x in t[1][i][1]))
        if k == "list":
            n = 0 if fuel <= 0 else rng.randrange(3)
            return tuple(self.value(t[1], fuel - 1) for _ in range(n))
        if k == "fn":
            i = 0 if fuel <= 0 else rng.randrange(len(t[1]))
            return (i, tuple(self.value(x, fuel - 1) for x in t[1][i]))
        if k == "shared":
            pool = self.held.get(t[1])
            if pool and (fuel <= 0 or rng.random() < 0.6):
                return rng.choice(pool)
            return self.new(t[1], self.value(t[1], fuel - 1))
        if k == "app":
            return self.value(unfold(self.decls, t), fuel)
        raise AssertionError(t)

    def leaked(self):
        """Drop every handle the program holds and free cells by counting:
        a cell whose count reaches zero is freed, and the handles in its
        state are dropped. What remains is what reference counting leaks."""
        rc = Counter()
        for v in self.state.values():
            for c in handles(v, []):
                rc[c] += 1
        live = set(self.state)
        free = [c for c in live if rc[c] == 0]
        while free:
            c = free.pop()
            live.discard(c)
            for d in handles(self.state[c], []):
                rc[d] -= 1
                if rc[d] == 0:
                    free.append(d)
        # Sanity: what is left is exactly what lies on or below a cycle.
        edges = {c: handles(self.state[c], []) for c in self.state}
        cyclic = {c for c in self.state if c in reach(edges, edges[c])}
        assert live == reach(edges, cyclic), "the counting heap is wrong"
        return live

    def run(self, x, steps):
        """A random program over values of type x: make values, store new
        states (which may reach any held handle, the cell's own included),
        read states back (`get!`, which hands their handles to the program)
        and drop handles."""
        rng = self.rng
        for _ in range(steps):
            op = rng.random()
            if op < 0.3 or not self.state:
                self.value(x, 3)
            elif op < 0.7:
                c = rng.randrange(len(self.state))
                self.state[c] = self.value(self.kind[c], 2)
            elif op < 0.85:
                c = rng.randrange(len(self.state))
                for d in handles(self.state[c], []):
                    self.held.setdefault(self.kind[d], []).append(H(d))
            else:
                pool = self.held.get(rng.choice(list(self.held)), [])
                if pool:
                    pool.pop(rng.randrange(len(pool)))
        return self.leaked()


def witness(decls, x, rng):
    """For a type precise rejects: a program that leaks. Find a `shared`
    node S on a cycle, make a cell of type S, and store in it a value built
    along the path from its state back to S, which holds its own handle."""
    edges = graph(decls, [x], True)
    target = next(s for s in edges if s[0] == "shared" and s in reach(edges, edges[s]))
    queue = list(dict.fromkeys(edges[target]))
    parent = {n: None for n in queue}
    while target not in parent:
        n = queue.pop(0)
        for m in edges[n]:
            if m not in parent:
                parent[m] = n
                queue.append(m)
    path = [target]
    while parent[path[-1]] is not None:
        path.append(parent[path[-1]])
    path.reverse()  # path[0] is a successor of target; path[-1] is target
    heap = Heap(decls, rng)
    h = heap.new(target[1], heap.value(target[1], 1))

    def along(i):
        node = path[i]
        if i == len(path) - 1:
            return h
        nxt, k = path[i + 1], node[0]
        if k == "rec":
            return tuple(along(i + 1) if x == nxt else heap.value(x, 1) for _, x in node[1])
        if k in ("union", "fn"):
            alts = node[1] if k == "fn" else tuple(ps for _, ps in node[1])
            j = next(j for j, ps in enumerate(alts) if nxt in ps)
            pos = alts[j].index(nxt)
            return (j, tuple(along(i + 1) if q == pos else heap.value(x, 1) for q, x in enumerate(alts[j])))
        if k == "list":
            return (along(i + 1),)
        if k == "shared":
            return heap.new(node[1], along(i + 1))
        if k == "app":
            return along(i + 1)
        raise AssertionError(node)

    heap.state[h.cid] = along(0)
    heap.held.clear()
    return heap.leaked()


# ------------------------------------------------------------------ generator


class Gen:
    """Declarations in two groups (a library, then a program that uses it),
    each group possibly mutually recursive, and a closed type over them.
    A reference to a declaration of the group being written appears only
    under a union payload, a list or a function, where a value can stop, so
    every type is inhabited; its argument is the declaring type's own
    parameter or a number, so recursion is regular."""

    def __init__(self, rng):
        self.rng = rng
        self.decls = {}
        self.groups = []

    def program(self):
        for prefix, n in (("L", self.rng.randrange(0, 3)), ("U", self.rng.randrange(1, 3))):
            names = [f"{prefix}{i}" for i in range(n)]
            arity = {nm: self.rng.randrange(2) for nm in names}
            for nm in names:
                self.decls[nm] = (tuple(f"{nm.lower()}a" for _ in range(arity[nm])), None)
            for nm in names:
                params = self.decls[nm][0]
                self.decls[nm] = (params, self.type(3, set(names), params, False))
            if names:
                self.groups.append(names)
        x = self.type(3, set(), (), False)
        return x

    def type(self, depth, group, params, guarded):
        rng = self.rng
        opts = ["prim"]
        if depth > 0:
            opts += ["rec", "union", "union", "list", "fn", "shared", "shared", "app_earlier", "app_earlier"]
        if params:
            opts += ["var"]
        if guarded and group:
            opts += ["app_group", "app_group"]
        k = rng.choice(opts)
        sub = lambda g=guarded: self.type(depth - 1, group, params, g)
        if k == "prim":
            return PRIM
        if k == "var":
            return ("var", rng.choice(params))
        if k == "rec":
            return ("rec", tuple((f"f{i}", sub()) for i in range(rng.randrange(1, 3))))
        if k == "union":
            tags = [("Z", ())] + [(f"T{i}", tuple(sub(True) for _ in range(rng.randrange(1, 3))))
                                  for i in range(rng.randrange(1, 3))]
            return ("union", tuple(tags))
        if k == "list":
            return ("list", sub(True))
        if k == "shared":
            return ("shared", sub())
        if k == "fn":
            alts = [()] + [tuple(sub(True) for _ in range(rng.randrange(1, 3)))
                           for _ in range(rng.randrange(1, 3))]
            return ("fn", tuple(alts))
        if k == "app_group":
            nm = rng.choice(sorted(group))
            if self.decls[nm][0]:
                arg = ("var", params[0]) if params and rng.random() < 0.7 else PRIM
                return ("app", nm, (arg,))
            return ("app", nm, ())
        earlier = [nm for nm in self.decls if nm not in group and self.decls[nm][1] is not None]
        if not earlier:
            return PRIM
        nm = rng.choice(earlier)
        return ("app", nm, tuple(sub() for _ in self.decls[nm][0]))


# ------------------------------------------------------------------ fuzzing


def modular_verdict(decls, groups, x):
    """The modular checker on a closed type x: declare it as Root, a group of
    its own after the others."""
    d = dict(decls)
    d["Root"] = ((), x)
    return summarize(d, groups + [["Root"]])["Root"][0]


def fuzz(programs, seed=0, runs=4, steps=40):
    stats = Counter()
    failures = []
    for p in range(programs):
        rng = random.Random(seed * 1_000_003 + p)
        g = Gen(rng)
        x = g.program()
        decls = g.decls
        stats["types"] += 1
        precise = precise_ok(decls, x)
        checker, why = checker_verdict(decls, x)
        modular = modular_verdict(decls, g.groups, x)
        stats["declarations"] += len(decls)
        stats["precise accepts"] += precise
        stats["checker accepts"] += checker
        if checker and not precise:
            failures.append((p, "checker accepts a type whose cells can form a cycle", x))
        if modular != checker:
            failures.append((p, f"modular {modular} but checker {checker}", x))
        if precise and not checker:
            stats["rejected only for a function in a state"] += why == "function in a state"
            if why != "function in a state":
                failures.append((p, f"checker rejects an acyclic type for {why}", x))
        if precise:
            for r in range(runs):
                heap = Heap(decls, random.Random(p * 31 + r))
                leaked = heap.run(x, steps)
                stats["programs run"] += 1
                stats["cells made"] += len(heap.state)
                if leaked:
                    failures.append((p, f"accepted type leaked {len(leaked)} cells", x))
        else:
            stats["precise rejects"] += 1
            heap = Heap(decls, random.Random(p))
            stats["random programs over a rejected type that leak"] += bool(heap.run(x, steps))
            if witness(decls, x, random.Random(p)):
                stats["witnesses that leak"] += 1
            else:
                failures.append((p, "witness for a rejected type did not leak", x))
    return stats, failures


EXAMPLES = {
    # name: (declarations, type, precise, checker)
    "a list in a handle": ({}, ("shared", ("list", PRIM)), True, True),
    "handles of handles": ({}, ("shared", ("list", ("shared", PRIM))), True, True),
    "a linked list of handles": (
        {"Node": ((), ("union", (("Nil", ()), ("Next", (("shared", ("app", "Node", ())),)))))},
        ("app", "Node", ()), False, False),
    "a cycle through a generic box": (
        {"Box": (("a",), ("rec", (("h", ("shared", ("var", "a"))),))),
         "S": ((), ("union", (("Nil", ()), ("N", (("app", "Box", (("app", "S", ()),)),)))))},
        ("app", "S", ()), False, False),
    "a job queue whose jobs capture nothing": (
        {}, ("shared", ("list", ("fn", ((),)))), True, False),
    "a callback that captures its own handle": (
        {"Cb": ((), ("union", (("None", ()), ("Some", (("fn", ((), (("shared", ("app", "Cb", ())),))),)))))},
        ("shared", ("app", "Cb", ())), False, False),
}


def main(argv):
    n = int(argv[1]) if len(argv) > 1 else 2000
    seed = int(argv[2]) if len(argv) > 2 else 0
    stats, failures = fuzz(n, seed)
    for k in sorted(stats):
        print(f"{k:<45} {stats[k]}")
    for f in failures[:10]:
        print("FAIL", f)
    print(f"{len(failures)} failures")
    return 1 if failures else 0


if __name__ == "__main__":
    sys.exit(main(sys.argv))
