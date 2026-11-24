"""Random small edits to a program value (gen.Program).

Each edit takes a Gen (for fresh names and randomness) and a program, and returns a changed deep copy and a one-line
description, or None when it does not apply to this program. `random_edit` picks one at random and keeps only an edit
whose result the reference can evaluate, so a program stays runnable unless the edit injects a type error on purpose.
Reverting to an earlier state is done by the driver, which keeps the history.
"""
import copy

import gen
from gen import I, S, B, L, R, T, F


def lit_paths(e, path=()):
    if e[0] == "lit":
        yield path, e
    for i, c in gen.children(e):
        yield from lit_paths(c, path + (i,))


def replace_at(e, path, new):
    if not path:
        return new
    i = path[0]
    lst = list(e)
    if isinstance(i, tuple):
        inner = list(lst[i[0]])
        inner[i[1]] = replace_at(inner[i[1]], path[1:], new)
        lst[i[0]] = inner
    else:
        lst[i] = replace_at(lst[i], path[1:], new)
    return tuple(lst)


class Slot:
    """A place holding an expression: a def body, an output, or an expect."""

    def __init__(self, prog, kind, idx):
        self.prog, self.kind, self.idx = prog, kind, idx

    def get(self):
        if self.kind == "def":
            return self.prog.defs[self.idx].body
        if self.kind == "out":
            return self.prog.outputs[self.idx][1]
        return self.prog.expects[self.idx].expr

    def set(self, e):
        if self.kind == "def":
            self.prog.defs[self.idx].body = e
        elif self.kind == "out":
            self.prog.outputs[self.idx] = (self.prog.outputs[self.idx][0], e)
        else:
            self.prog.expects[self.idx].expr = e

    def where(self):
        if self.kind == "def":
            d = self.prog.defs[self.idx]
            return f"{d.name} in {gen.mod_name(d.module) if d.module < self.prog.nmods - 1 else 'app'}"
        return f"{self.kind} {self.idx}"


def slots(prog):
    return ([Slot(prog, "def", i) for i in range(len(prog.defs))] + [Slot(prog, "out", i) for i in range(len(prog.outputs))]
            + [Slot(prog, "exp", i) for i in range(len(prog.expects))])


def modname(prog, m):
    return "app" if m == prog.nmods - 1 else gen.mod_name(m)


def members(prog, d):
    return [x for x in prog.defs if x.name in (d.group or [d.name])]


# --------------------------------------------------------------------------------------------- edits

def e_lit(g, prog):
    """Change one literal's value: a constant's value, a step of a function body, an output or an expect."""
    p = copy.deepcopy(prog)
    cands = [(s, path, e) for s in slots(p) for path, e in lit_paths(s.get())]
    if not cands:
        return None
    s, path, old = g.r.choice(cands)
    new = old
    for _ in range(10):
        new = g.lit(old[1])
        if new != old:
            break
    if new == old:
        return None
    s.set(replace_at(s.get(), path, new))
    return p, f"lit {s.where()}: {old[2]!r} -> {new[2]!r}"


def e_body(g, prog):
    """Replace a non-recursive definition's body with a fresh one of the same type."""
    p = copy.deepcopy(prog)
    cands = [i for i, d in enumerate(p.defs) if d.kind in ("plain", "hof", "poly", "const", "mk")]
    if not cands:
        return None
    i = g.r.choice(cands)
    d = p.defs[i]
    ctx = g.body_ctx(p.defs[:i], d.module, d.params)
    if d.kind == "poly":
        d.body = g.poly_expr("A", ctx, g.r.randint(1, 3))
    elif d.kind == "mk":
        d.body = g.lam(ctx, 2)
    else:
        if d.kind == "const":
            ctx["max_n"] = 6
        d.body = g.expr(d.ret, ctx, g.r.randint(1, 3))
    return p, f"body {d.name} in {modname(p, d.module)}"


def e_add(g, prog):
    """Add a definition (or recursive group) to a module, sometimes with an output that uses it."""
    p = copy.deepcopy(prog)
    m = g.r.randrange(p.nmods)
    new = g.new_defs(p.defs, m)
    p.defs.extend(new)
    used = g.r.random() < 0.6
    if used:
        d = new[0]
        ty = g.r.choice(gen.POLY_INST) if d.ret == "A" else d.ret
        p.outputs.append((ty, g.call(d, g.app_ctx(p), 1, want=ty)))
    return p, f"add {new[0].kind} {', '.join(d.name for d in new)} to {modname(p, m)}" + (" with an output" if used else "")


def e_remove(g, prog):
    """Remove an output, an expect, or a definition nothing references."""
    p = copy.deepcopy(prog)
    r = g.r.random()
    if r < 0.3 and len(p.outputs) > 1:
        i = g.r.randrange(len(p.outputs))
        del p.outputs[i]
        return p, f"remove output {i}"
    if r < 0.45 and p.expects:
        i = g.r.randrange(len(p.expects))
        del p.expects[i]
        return p, f"remove expect {i}"
    cands = [d for d in p.defs if (d.group is None or d.group[0] == d.name) and not gen.referrers(p, d.name)
             and not any(gen.referrers(p, n) for n in (d.group or []))]
    if not cands:
        return None
    d = g.r.choice(cands)
    names = set(d.group or [d.name])
    p.defs = [x for x in p.defs if x.name not in names]
    if p.error in names:
        p.error = None
    return p, f"remove {', '.join(sorted(names))} from {modname(p, d.module)}"


def callees(prog, d):
    grp = set(d.group or [d.name])
    out = set()
    for x in members(prog, d):
        out |= gen.refs(x.body) - grp
    return out


def e_move(g, prog):
    """Move a definition (or recursive group) to another module, keeping imports acyclic."""
    p = copy.deepcopy(prog)
    by = p.by_name()
    heads = [d for d in p.defs if (d.group is None or d.group[0] == d.name) and not d.private]
    g.r.shuffle(heads)
    for d in heads:
        cs = [by[n] for n in callees(p, d)]
        if any(c.private for c in cs):
            continue
        lo = max([c.module for c in cs], default=0)
        rs = [m for n in (d.group or [d.name]) for m in gen.referrers(p, n)]
        hi = min(rs, default=p.nmods - 1)
        opts = [m for m in range(lo, hi + 1) if m != d.module]
        if not opts:
            continue
        m = g.r.choice(opts)
        old = d.module
        for x in members(p, d):
            x.module = m
        return p, f"move {d.name} from {modname(p, old)} to {modname(p, m)}"
    return None


def e_rec(g, prog):
    """Change Rec's fields: add or remove a field nothing reads, or reorder the declared fields."""
    p = copy.deepcopy(prog)
    if g.r.random() < 0.25 and len(p.rec_fields) > 2:
        g.r.shuffle(p.rec_fields)
        return p, "reorder Rec fields: " + ", ".join(n for n, _, _ in p.rec_fields)
    name, ty, val = g.r.choice(gen.REC_EXTRA)
    have = [f for f in p.rec_fields if f[0] == name]
    if have:
        p.rec_fields.remove(have[0])
        return p, f"remove Rec field {name}"
    p.rec_fields.insert(g.r.randint(0, len(p.rec_fields)), (name, ty, val))
    return p, f"add Rec field {name} : {gen.ROC_TYPE[ty]}"


def e_tag(g, prog):
    """Change Tag's tags: add or remove a tag nothing builds, or reorder the declared tags."""
    p = copy.deepcopy(prog)
    if g.r.random() < 0.25:
        g.r.shuffle(p.tag_tags)
        return p, "reorder Tag tags: " + ", ".join(n for n, _ in p.tag_tags)
    name, ty = g.r.choice(gen.TAG_EXTRA)
    have = [t for t in p.tag_tags if t[0] == name]
    if have:
        p.tag_tags.remove(have[0])
        return p, f"remove Tag tag {name}"
    p.tag_tags.insert(g.r.randint(0, len(p.tag_tags)), (name, ty))
    return p, f"add Tag tag {name}"


def let_paths(e, path=()):
    if e[0] == "let":
        yield path, e
    for i, c in gen.children(e):
        yield from let_paths(c, path + (i,))


def e_annot(g, prog):
    """Add or drop a type annotation: on a definition, or on a let binding whose value fixes its own type."""
    p = copy.deepcopy(prog)
    if g.r.random() < 0.5:
        d = g.r.choice(p.defs) if p.defs else None
        if d is None:
            return None
        d.ann = not d.ann
        return p, f"{'annotate' if d.ann else 'unannotate'} {d.name}"
    cands = [(s, path, e) for s in slots(p) for path, e in let_paths(s.get()) if e[4] in (I, S, B)]
    if not cands:
        return None
    s, path, e = g.r.choice(cands)
    s.set(replace_at(s.get(), path, e[:5] + (not e[5],)))
    return p, f"{'annotate' if not e[5] else 'unannotate'} let {e[1]} in {s.where()}"


def e_expect(g, prog):
    """Add an expect, remove one, or change the value one expects."""
    p = copy.deepcopy(prog)
    r = g.r.random()
    if p.expects and r < 0.3:
        x = g.r.choice(p.expects)
        x.want = gen.perturb(x.want)
        return p, f"change expected value in {modname(p, x.module)} to {x.want!r}"
    if p.expects and r < 0.45:
        i = g.r.randrange(len(p.expects))
        del p.expects[i]
        return p, f"remove expect {i}"
    m = g.r.randrange(p.nmods)
    try:
        p.expects.append(g.new_expect(p, m))
    except (gen.Overflow, RecursionError):
        return None
    return p, f"add expect to {modname(p, m)}"


def e_noise(g, prog):
    """Change only comments and blank lines in one file."""
    p = copy.deepcopy(prog)
    files = [n for n in gen.Emit(p).files() if not n.endswith("main.roc")]
    name = g.r.choice(files).split("/")[-1]
    old = p.noise.get(name, 0)
    p.noise[name] = 0 if old and g.r.random() < 0.3 else g.r.randint(1, 10 ** 6)
    return p, f"noise {name}: {old} -> {p.noise[name]}"


def e_import(g, prog):
    """Add or remove an import nothing uses."""
    p = copy.deepcopy(prog)
    em = gen.Emit(p)
    m = g.r.randrange(1, p.nmods)
    lower = [x for x in range(m) if em.module_exists(x)]
    if not lower:
        return None
    x = g.r.choice(lower)
    s = p.extra_imports.setdefault(m, set())
    if x in s:
        s.remove(x)
        return p, f"drop unused import of {gen.mod_name(x)} from {modname(p, m)}"
    s.add(x)
    return p, f"add unused import of {gen.mod_name(x)} to {modname(p, m)}"


def e_private(g, prog):
    """Hide a definition from other modules, or expose it again (a change to the module's exports)."""
    p = copy.deepcopy(prog)
    app = p.nmods - 1
    cands = []
    for d in p.defs:
        if d.module == app or not (d.group is None or d.group[0] == d.name):
            continue
        if d.private or all(m == d.module for n in (d.group or [d.name]) for m in gen.referrers(p, n)):
            cands.append(d)
    if not cands:
        return None
    d = g.r.choice(cands)
    for x in members(p, d):
        x.private = not d.private
    return p, f"{'hide' if d.private else 'expose'} {d.name} in {modname(p, d.module)}"


def e_form(g, prog):
    """Switch a module outside the package between a type module and a `module [...]` header."""
    p = copy.deepcopy(prog)
    cands = [m for m in range(p.npkg, p.nmods - 1)]
    if not cands:
        return None
    m = g.r.choice(cands)
    p.forms[m] = "header" if p.forms[m] == "type" else "type"
    return p, f"make {gen.mod_name(m)} a {p.forms[m]} module"


def e_package(g, prog):
    """Move the highest package module out to the app's directory, or the lowest local module into the package. With
    no package left, Rec and Tag move beside the app and the app loses its header."""
    p = copy.deepcopy(prog)
    app = p.nmods - 1
    if p.npkg > 0 and (p.npkg == app or g.r.random() < 0.5):
        p.npkg -= 1
        return p, f"move {gen.mod_name(p.npkg)} out of the package"
    if p.npkg < app:
        p.forms[p.npkg] = "type"
        p.npkg += 1
        return p, f"move {gen.mod_name(p.npkg - 1)} into the package"
    return None


def e_error(g, prog):
    """Inject a type error into a definition, or remove the one injected."""
    p = copy.deepcopy(prog)
    if p.error:
        old, p.error = p.error, None
        return p, f"fix the error in {old}"
    if not p.defs:
        return None
    d = g.r.choice(p.defs)
    p.error = d.name
    return p, f"inject a type error into {d.name} in {modname(p, d.module)}"


EDITS = {
    "lit": (e_lit, 6), "body": (e_body, 4), "add": (e_add, 3), "remove": (e_remove, 2), "move": (e_move, 2),
    "rec": (e_rec, 2), "tag": (e_tag, 2), "annot": (e_annot, 2), "expect": (e_expect, 2), "noise": (e_noise, 2),
    "import": (e_import, 1), "private": (e_private, 1), "form": (e_form, 1), "package": (e_package, 1),
    "error": (e_error, 1),
}


def random_edit(g, prog, kinds=None):
    """A changed copy of prog and its description. The reference must still evaluate the result."""
    names = kinds or list(EDITS)
    weights = [EDITS[n][1] for n in names]
    for _ in range(50):
        name = g.r.choices(names, weights)[0]
        res = EDITS[name][0](g, prog)
        if res is None:
            continue
        p, desc = res
        try:
            gen.evaluate(p)
        except (gen.Overflow, RecursionError):
            continue
        return p, desc
    return None
