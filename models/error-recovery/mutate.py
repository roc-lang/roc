"""Typed mutations: each one injects a type error at a random position of a well-typed program.

A mutation picks a site of the right kind (any expression, a name use, a call, a match, a record operation, an
annotation, a binder) and rewrites it into one specific ill-typed construct, using the site's type and scope. The
sites and the replacement constructs are drawn at random; there is no list of handwritten programs.
"""
from gen import E, BASE, BOOL, I64, STR, show_type, is_data


def sites(prog):
    """Every expression node of the program, with its parent and slot, tagged by where it lives."""
    out = []
    for d in prog.defs + prog.methods:
        for n, par, slot in d.body.walk():
            out.append((n, par, slot, d))
        # the body root itself is replaced through the Def
    for e in prog.expects:
        for n, par, slot in e.walk():
            out.append((n, par, slot, None))
    for e in prog.main:
        for n, par, slot in e.walk():
            out.append((n, par, slot, "main"))
    return out


def replace(prog, node, par, slot, owner, new):
    new.mutated = True
    if par is not None:
        par.kids[slot] = new
        return
    for d in prog.defs + prog.methods:
        if d.body is node:
            d.body = new
            return
    for lst in (prog.expects, prog.main):
        for i, e in enumerate(lst):
            if e is node:
                lst[i] = new
                return
    raise AssertionError("site not found")


def other_type(g, t):
    while True:
        o = g.rand_type(1)
        if o != t:
            return o


def m_mismatch(g, prog, s):
    """An expression of one type where another is needed."""
    node, par, slot, owner = s
    if node.ty[0] == "Oops":
        return None
    o = other_type(g, node.ty)
    new = g.expr(o, node.env, g.rng.randint(0, 2))
    replace(prog, node, par, slot, owner, new)
    return "mismatch: %s where %s is needed" % (show_type(o, prog), show_type(node.ty, prog))


def m_unbound(g, prog, s):
    node, par, slot, owner = s
    if node.kind != "var":
        return None
    name = node.data["name"]
    new = E("var", node.ty, env=node.env, name=g.rng.choice([name + "_", "undefined_" + name.lstrip("$"), "zz"]))
    if name.startswith("$"):
        new.data["name"] = "$undefined" + name[1:]
    replace(prog, node, par, slot, owner, new)
    return "unbound name in place of %s" % name


def m_arity(g, prog, s):
    node, par, slot, owner = s
    if node.kind not in ("call", "mcall") or (node.kind == "mcall" and len(node.kids) < 2 and not node.data["qualified"]):
        return None
    new = E(node.kind, node.ty, list(node.kids), node.env, **node.data)
    if new.kids and g.rng.random() < 0.5:
        new.kids.pop(g.rng.randrange(1 if node.kind == "mcall" and not node.data["qualified"] else 0, len(new.kids)))
        what = "one argument too few"
    else:
        new.kids.insert(g.rng.randint(0, len(new.kids)), g.leaf(g.rng.choice(BASE), node.env))
        what = "one argument too many"
    replace(prog, node, par, slot, owner, new)
    return "call of %s with %s" % (node.data.get("fn") or node.data["method"], what)


def m_call_non_fn(g, prog, s):
    """Call something that is not a function: a value in scope, or a literal."""
    node, par, slot, owner = s
    vals = [n for n, t in node.env if t[0] != "Fn"]
    callee = g.rng.choice(vals) if vals and g.rng.random() < 0.7 else g.rng.choice(['"s"', "7", "True"])
    new = E("call", node.ty, [g.leaf(g.rng.choice(BASE), node.env)], node.env, fn=callee)
    replace(prog, node, par, slot, owner, new)
    return "call of non-function %s" % callee


def m_missing_method(g, prog, s):
    node, par, slot, owner = s
    if node.ty[0] in ("Oops", "Fn"):
        return None
    new = E("tmpl", node.ty, [node], node.env, fmt="({0}).nosuch_method()")
    replace(prog, node, par, slot, owner, new)
    return "call of a missing method on %s" % show_type(node.ty, prog)


def m_missing_field(g, prog, s):
    node, par, slot, owner = s
    if node.kind == "field":
        new = E("field", node.ty, list(node.kids), node.env, field="nofield")
        what = "access of a missing field"
    elif node.kind == "update":
        new = E("update", node.ty, list(node.kids), node.env, field="nofield")
        what = "update of a missing field"
    elif node.kind == "rec" and node.kids:
        i = g.rng.randrange(len(node.kids))
        kids = node.kids[:i] + node.kids[i + 1:]
        fields = node.data["fields"][:i] + node.data["fields"][i + 1:]
        if g.rng.random() < 0.5:
            kids, fields = node.kids + [g.leaf(g.rng.choice(BASE), node.env)], node.data["fields"] + ["extra"]
            what = "record literal with an extra field"
        else:
            what = "record literal missing a field"
        new = E("rec", node.ty, kids, node.env, fields=fields)
    elif node.kind == "tupidx":
        new = E("tupidx", node.ty, list(node.kids), node.env, idx=2)
        what = "tuple index out of range"
    else:
        # access a field on a non-record value of the same type
        new = E("tmpl", node.ty, [node], node.env, fmt="({0}).nofield")
        what = "field access on %s" % show_type(node.ty, prog)
    replace(prog, node, par, slot, owner, new)
    return what


def m_unknown_tag(g, prog, s):
    node, par, slot, owner = s
    if node.kind != "tag":
        return None
    new = E("tag", node.ty, list(node.kids), node.env, tag=node.data["tag"] if g.rng.random() < 0.4 else "Nope")
    if new.data["tag"] == node.data["tag"]:
        # same tag, wrong payload arity
        if new.kids and g.rng.random() < 0.5:
            new.kids.pop()
        else:
            new.kids.append(g.leaf(g.rng.choice(BASE), node.env))
        what = "tag %s with the wrong payload arity" % node.data["tag"]
    else:
        what = "tag outside the union"
    replace(prog, node, par, slot, owner, new)
    return what


def m_bad_pattern(g, prog, s):
    node, par, slot, owner = s
    if node.kind != "match":
        return None
    br = [list(b) for b in node.data["branches"]]
    i = g.rng.randrange(len(br))
    pat, bs = br[i]
    choice = g.rng.random()
    if choice < 0.3:
        br[i] = ['"lit"', []]
        what = "string pattern against %s" % show_type(node.kids[0].ty, prog)
    elif choice < 0.55:
        br[i] = ["Nope", bs]
        what = "pattern with a tag outside the union"
    elif choice < 0.8 and pat[:1].isupper():
        br[i] = [pat, bs + ["extra_b"]] if not bs or g.rng.random() < 0.5 else [pat, bs[:-1]]
        what = "pattern with the wrong payload arity"
    else:
        br[i] = ["(%s, %s)" % (g.fresh("p"), g.fresh("p")), []]
        what = "tuple pattern against %s" % show_type(node.kids[0].ty, prog)
    new = E("match", node.ty, list(node.kids), node.env, branches=[tuple(b) for b in br])
    replace(prog, node, par, slot, owner, new)
    return what


def m_bad_destructure(g, prog, s):
    node, par, slot, owner = s
    if node.kind == "dtup":
        b = node.data["binders"]
        pat = "(%s)" % ", ".join(b + [g.fresh("p")]) if g.rng.random() < 0.5 else "(%s)" % b[0]
        new = E("dtup", node.ty, list(node.kids), node.env, binders=b, pattern=pat)
        what = "tuple destructure of the wrong arity"
    elif node.kind == "drec":
        pairs = list(zip(node.data["fields"], node.data["binders"])) + [("nofield", g.fresh("p"))]
        pat = "{ %s }" % ", ".join("%s: %s" % p for p in pairs)
        new = E("drec", node.ty, list(node.kids), node.env, fields=node.data["fields"], binders=node.data["binders"],
                pattern=pat)
        what = "record destructure of a missing field"
    elif node.kind == "let":
        # destructure a value as a tuple it is not
        b = node.data["name"]
        new = E("dtup", node.ty, list(node.kids), node.env, binders=[b], pattern="(%s, %s)" % (b, g.fresh("p")))
        what = "tuple destructure of %s" % show_type(node.kids[0].ty, prog)
    else:
        return None
    replace(prog, node, par, slot, owner, new)
    return what


def m_shadow(g, prog, s):
    """Rebind a name that is already in scope."""
    node, par, slot, owner = s
    names = [n for n, t in node.env if not n.startswith("$")]
    if not names:
        return None
    n = g.rng.choice(names)
    new = E("let", node.ty, [g.leaf(g.rng.choice(BASE), node.env), node], node.env, name=n)
    replace(prog, node, par, slot, owner, new)
    return "rebinding %s, already in scope" % n


def m_question(g, prog, s):
    """`?` on a value that is not a Try, or outside a function that returns one."""
    node, par, slot, owner = s
    if node.ty[0] in ("Oops", "Fn"):
        return None
    new = E("question", node.ty, [node], node.env)
    replace(prog, node, par, slot, owner, new)
    return "? on %s" % show_type(node.ty, prog)


def m_effect(g, prog, s):
    """An effectful call in a pure function or constant."""
    node, par, slot, owner = s
    if owner == "main":
        return None
    new = E("stmt", node.ty, [node], node.env, text='echo!("x")')
    replace(prog, node, par, slot, owner, new)
    return "effectful call outside an effectful function"


def m_condition(g, prog, s):
    """A non-Bool `if` condition, or branches of different types."""
    node, par, slot, owner = s
    if node.kind != "if":
        return None
    i = g.rng.randint(0, 2)
    t = node.kids[i].ty
    o = other_type(g, t)
    new = E("if", node.ty, list(node.kids), node.env)
    new.kids[i] = g.expr(o, node.env, 1)
    new.kids[i].mutated = True
    replace(prog, node, par, slot, owner, new)
    return ("non-Bool condition" if i == 0 else "if branches of different types")


def m_annotation(g, prog, s):
    """Change a definition's annotation: another type at one position, or an undeclared type name."""
    d = g.rng.choice(prog.defs)
    t = d.ty
    undeclared = g.rng.random() < 0.35
    sub = ("Named", g.rng.choice(["Nope", "List", "Str(I64)", "Try(I64)"])) if undeclared else None
    if t[0] == "Fn":
        args, ret = list(t[1]), t[2]
        i = g.rng.randint(0, len(args) + (1 if g.rng.random() < 0.2 else 0))
        if i == len(args) + 1:
            args.append(sub or g.rand_type(0))  # one parameter too many
            what = "annotation with an extra parameter"
        elif i == len(args):
            ret = sub or other_type(g, ret)
            what = "annotation with another return type"
        else:
            args[i] = sub or other_type(g, args[i])
            what = "annotation with another parameter type"
        nt = ("Fn", tuple(args), ret)
    else:
        nt = sub or other_type(g, t)
        what = "annotation with another type"
    d.anno = show_type(nt, prog)
    if undeclared:
        what += " (%s)" % sub[1]
    return "%s on %s" % (what, d.name)


EXPR_MUTATIONS = [m_mismatch, m_mismatch, m_unbound, m_arity, m_call_non_fn, m_missing_method, m_missing_field,
                  m_unknown_tag, m_bad_pattern, m_bad_destructure, m_shadow, m_question, m_effect, m_condition]


def mutate(g, prog, count):
    """Apply `count` mutations at random positions. Returns their descriptions."""
    done = []
    tries = 0
    while len(done) < count and tries < 200:
        tries += 1
        if g.rng.random() < 0.12:
            done.append(m_annotation(g, prog, None))
            continue
        ss = [s for s in sites(prog) if not s[0].mutated]
        if not ss:
            break
        m = g.rng.choice(EXPR_MUTATIONS)
        # prefer sites the mutation applies to
        for _ in range(20):
            s = g.rng.choice(ss)
            what = m(g, prog, s)
            if what:
                done.append(what)
                break
    prog.mutations = done
    return done
