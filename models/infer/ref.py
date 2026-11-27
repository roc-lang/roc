"""Reference type inference for a Roc core fragment.

HM with Remy-style levels, extensible tag-union and record rows, and the Roc
rules from design.md that the fragment exercises.  Each rule is named in
RULES.md; the rule name appears beside the code that implements it.

The model is deliberately small and explicit.  It is an oracle for:
  * the accept/reject verdict of `roc check` on a module, and
  * the principal type of each generalized top-level function, printed in the
    same convention as Roc's TypeWriter (REPL `:t`), up to renaming.
"""
from __future__ import annotations
import itertools

# ---------------------------------------------------------------- types

GENERIC = 10**9


class TypeError_(Exception):
    def __init__(self, msg, rule="unify"):
        super().__init__(msg)
        self.rule = rule


class T:
    __slots__ = ("kind", "link", "level", "name", "rigid", "bounded",
                 "con", "args", "ret", "eff", "fields", "ext", "uid", "rowkind", "lacks")
    _ids = itertools.count()

    def __init__(self, kind, **kw):
        self.kind = kind          # var | con | fun | rec | tags | erec | etags
        self.link = None
        self.level = kw.get("level", 0)
        self.name = kw.get("name")
        self.rigid = kw.get("rigid", False)
        self.bounded = kw.get("bounded", False)
        self.con = kw.get("con")
        self.args = kw.get("args", [])
        self.ret = kw.get("ret")
        self.eff = kw.get("eff", False)
        self.fields = kw.get("fields", {})
        self.ext = kw.get("ext")
        self.rowkind = kw.get("rowkind")   # R-RowKind: None | "tags" | "rec"
        self.lacks = {}                    # R-Lacks: label -> payload a row tail must agree with
        self.uid = next(T._ids)


def find(t):
    while t.link is not None:
        t = t.link
    return t


class Infer:
    def __init__(self):
        self.level = 1
        self.erec = T("erec")
        self.etags = T("etags")
        self.question = []        # stack of per-lambda `?` contribution lists
        self.top_value_types = []  # R-TopValueClose

    # ------------------------------------------------ constructors
    def fresh(self, level=None, **kw):
        return T("var", level=self.level if level is None else level, **kw)

    def con(self, name, *args):
        return T("con", con=name, args=list(args))

    def fun(self, args, ret, eff=None):
        """eff: an effect node (R-EffectSlot): an effect variable whose
        `args` are its dependencies, or the constants epure / eeff."""
        if eff is None:
            eff = self.fresh()
        elif eff == "pure":
            eff = T("epure")
        elif eff == "eff":
            eff = T("eeff")
        return T("fun", args=list(args), ret=ret, eff=eff)

    def add_lacks(self, t, labels):
        """R-Lacks: a label a row tail turns out to carry merges with the
        same-named head when the payloads agree, and is a mismatch otherwise
        (design.md Try Return-Row Composition / Row Union Normalization)."""
        t = find(t)
        if t.kind == "var":
            nl = dict(t.lacks)
            for k, v in labels.items():
                if k in nl:
                    self.unify_payload(nl[k], v, k)
                else:
                    nl[k] = v
            t.lacks = nl
        elif t.kind in ("tags", "rec"):
            for k, v in labels.items():
                if k in t.fields:
                    self.unify_payload(t.fields[k], v, k)
            self.add_lacks(t.ext, labels)

    def unify_payload(self, p1, p2, k):
        if isinstance(p1, list):
            if len(p1) != len(p2):
                raise TypeError_("conflicting label " + k, "lacks")
            for x, y in zip(p1, p2):
                self.unify(x, y)
        else:
            self.unify(p1, p2)

    def is_eff(self, e, seen=None):
        e = find(e)
        if e.kind == "eeff":
            return True
        if e.kind != "var":
            return False
        seen = seen if seen is not None else set()
        if e.uid in seen:
            return False
        seen.add(e.uid)
        return any(self.is_eff(d, seen) for d in e.args)

    def tags(self, tags, ext):
        self.mark_row(ext, "tags", tags)
        return T("tags", fields=dict(tags), ext=ext)

    def rec(self, fields, ext):
        self.mark_row(ext, "rec", fields)
        return T("rec", fields=dict(fields), ext=ext)

    def mark_row(self, ext, kind, labels=()):
        e = find(ext)
        if labels:
            self.add_lacks(e, dict(labels))
        if e.kind == "var":
            if e.rowkind not in (None, kind):
                raise TypeError_("row kind", "rowkind")
            e.rowkind = kind
        elif kind == "tags" and e.kind not in ("tags", "etags"):
            raise TypeError_("tag row tail is %s" % e.kind, "rowkind")
        elif kind == "rec" and e.kind not in ("rec", "erec"):
            raise TypeError_("record tail is %s" % e.kind, "rowkind")

    def try_(self, ok, err):
        return self.con("Try", ok, err)

    # ------------------------------------------------ row flattening
    def flat_tags(self, t):
        t = find(t)
        if t.kind == "con" and t.con == "Try":
            # R-TryBacking: a Try read as a row is its backing [Ok(a), Err(e)]
            return {"Ok": [t.args[0]], "Err": [t.args[1]]}, self.etags
        out = {}
        while t.kind == "tags":
            for k, v in t.fields.items():
                if k not in out:
                    out[k] = v
            t = find(t.ext)
        if t.kind == "con" and t.con == "Try":
            # a row extension never resolves to a nominal in this fragment
            raise TypeError_("row tail is nominal")
        return out, t

    def flat_rec(self, t):
        t = find(t)
        out = {}
        while t.kind == "rec":
            for k, v in t.fields.items():
                if k not in out:
                    out[k] = v
            t = find(t.ext)
        return out, t

    # ------------------------------------------------ occurs / levels
    def adjust(self, v, t, seen=None):
        """Occurs check of var v in t, lowering levels to v.level."""
        t = find(t)
        if t is v:
            raise TypeError_("occurs", "occurs")
        if seen is None:
            seen = set()
        if t.uid in seen:
            return
        seen.add(t.uid)
        if t.kind == "var":
            if t.level > v.level and t.level != GENERIC:
                if t.rigid and t.level > v.level:
                    # a rigid may not escape its scope (R-RigidEscape)
                    raise TypeError_("rigid escapes", "rigid")
                t.level = v.level
            for d in t.args:
                self.adjust(v, d, seen)
            return
        for c in self.children(t):
            self.adjust(v, c, seen)

    def children(self, t):
        if t.kind == "con":
            return list(t.args)
        if t.kind == "fun":
            return list(t.args) + [t.ret, t.eff]
        if t.kind == "var":
            return list(t.args)
        if t.kind in ("rec", "tags"):
            out = []
            for v in t.fields.values():
                out.extend(v if isinstance(v, list) else [v])
            out.append(t.ext)
            return out
        return []

    # ------------------------------------------------ unification
    def bind(self, v, t):
        t = find(t)
        if t is v:
            return
        if v.lacks:
            self.add_lacks(t, v.lacks)
        if v.rowkind is not None:
            self.mark_row(t, v.rowkind)
        if t.kind == "epure":
            # R-PureAnnotation: a pure kind requires every dependency pure
            deps = list(v.args)
            self.adjust(v, t)
            v.link = t
            for d in deps:
                self.unify(d, t)
            return
        if t.kind == "var" and v.args:
            t.args = list(t.args) + list(v.args)
        if t.kind == "var":
            if t.rigid:
                if v.bounded:
                    # R-BoundedRigid: a rigid tail stands for tags the annotation
                    # does not list (design.md Polarity; roc-lang/roc#13297)
                    raise TypeError_("bounded row meets rigid tail", "bounded")
                self.adjust(v, t)
                v.link = t
                return
            # R-BoundTravels: the bound travels with the class
            t.bounded = t.bounded or v.bounded
            t.level = min(t.level, v.level)
            v.link = t
            return
        if v.bounded:
            # R-Bounded: an annotation-bounded row may close, never gain a tag
            if t.kind == "tags":
                tg, tail = self.flat_tags(t)
                if tg:
                    raise TypeError_("tag not in annotation: " + ",".join(sorted(tg)), "bounded")
                self.bind(v, tail)
                return
        self.adjust(v, t)
        v.link = t

    def unify(self, a, b):
        a, b = find(a), find(b)
        if a is b:
            return
        if a.kind == "var" and not a.rigid:
            return self.bind(a, b)
        if b.kind == "var" and not b.rigid:
            return self.bind(b, a)
        if a.kind == "var" or b.kind == "var":
            raise TypeError_("rigid mismatch", "rigid")
        # R-TryBacking: Try(a, e) is the nominal over [Ok(a), Err(e)]
        if a.kind == "con" and a.con == "Try" and b.kind == "tags":
            a, b = b, a
        if a.kind == "tags" and b.kind == "con" and b.con == "Try":
            backing = self.tags({"Ok": [b.args[0]], "Err": [b.args[1]]}, self.etags)
            self.unify_tags(a, backing)
            find(a).link = b if find(a) is not b else None
            return
        if a.kind != b.kind:
            raise TypeError_(f"{a.kind} vs {b.kind}")
        k = a.kind
        if k == "con":
            if a.con != b.con or len(a.args) != len(b.args):
                raise TypeError_(f"{a.con} vs {b.con}")
            a.link = b
            for x, y in zip(a.args, b.args):
                self.unify(x, y)
        elif k == "fun":
            if len(a.args) != len(b.args):
                raise TypeError_("arity")
            a.link = b
            self.unify(a.eff, b.eff)
            for x, y in zip(a.args, b.args):
                self.unify(x, y)
            self.unify(a.ret, b.ret)
        elif k == "tags":
            self.unify_tags(a, b)
        elif k == "rec":
            self.unify_rec(a, b)
        elif k in ("erec", "etags", "epure", "eeff"):
            a.link = b
        else:
            raise AssertionError(k)

    def unify_tags(self, a, b):
        t1, r1 = self.flat_tags(a)
        t2, r2 = self.flat_tags(b)
        a, b = find(a), find(b)
        if a is not b:
            a.link = b  # one node from now on
        for k in t1.keys() & t2.keys():
            p1, p2 = t1[k], t2[k]
            if len(p1) != len(p2):
                raise TypeError_(f"tag {k} arity")
            for x, y in zip(p1, p2):
                self.unify(x, y)
        o1 = {k: v for k, v in t1.items() if k not in t2}
        o2 = {k: v for k, v in t2.items() if k not in t1}
        self._unify_tails(r1, o1, r2, o2, self.tags, self.etags)
        # b now stands for the merged row; rebuild it flat for clean printing
        merged, tail = self.flat_tags_nolink(r1, r2, t1, t2)
        b.fields, b.ext = merged, tail
        b.kind = "tags"

    def flat_tags_nolink(self, r1, r2, t1, t2):
        merged = dict(t2)
        for k, v in t1.items():
            merged.setdefault(k, v)
        more, tail = self.flat_tags(r2)
        for k, v in more.items():
            merged.setdefault(k, v)
        return merged, tail

    def unify_rec(self, a, b):
        f1, r1 = self.flat_rec(a)
        f2, r2 = self.flat_rec(b)
        a, b = find(a), find(b)
        if a is not b:
            a.link = b
        for k in f1.keys() & f2.keys():
            self.unify(f1[k], f2[k])
        o1 = {k: v for k, v in f1.items() if k not in f2}
        o2 = {k: v for k, v in f2.items() if k not in f1}
        self._unify_tails(r1, o1, r2, o2, self.rec, self.erec)
        merged = dict(f2)
        for k, v in f1.items():
            merged.setdefault(k, v)
        more, tail = self.flat_rec(r2)
        for k, v in more.items():
            merged.setdefault(k, v)
        b.fields, b.ext, b.kind = merged, tail, "rec"

    def _unify_tails(self, r1, o1, r2, o2, mk, empty):
        r1, r2 = find(r1), find(r2)
        if not o1 and not o2:
            return self.unify(r1, r2)
        if o1 and not o2:
            return self.unify(r2, mk(o1, r1))
        if o2 and not o1:
            return self.unify(r1, mk(o2, r2))
        if r1 is r2:
            raise TypeError_("row tail cycle")
        lvl = min(r1.level if r1.kind == "var" else self.level,
                  r2.level if r2.kind == "var" else self.level)
        rho = self.fresh(level=lvl)
        self.unify(r1, mk(o2, rho))
        self.unify(r2, mk(o1, rho))

    # ------------------------------------------------ generalize / instantiate
    def generalize(self, t, seen=None):
        t = find(t)
        if seen is None:
            seen = set()
        if t.uid in seen:
            return
        seen.add(t.uid)
        if t.kind == "var":
            if t.level > self.level and t.level != GENERIC:
                t.level = GENERIC
        for c in self.children(t):
            self.generalize(c, seen)

    def instantiate(self, t, memo=None):
        t = find(t)
        if memo is None:
            memo = {}
        if t.uid in memo:
            return memo[t.uid]
        if t.kind == "var":
            if t.level == GENERIC:
                # R-InstNoBound: instantiation never copies the bound
                n = self.fresh(rowkind=t.rowkind)
                n.lacks = {k: ([self.instantiate(x, memo) for x in v] if isinstance(v, list) else self.instantiate(v, memo))
                            for k, v in t.lacks.items()}
                memo[t.uid] = n
                n.args = [self.instantiate(d, memo) for d in t.args]
                return n
            return t
        if not self.has_generic(t):
            return t
        if t.kind == "con":
            n = T("con", con=t.con)
            memo[t.uid] = n
            n.args = [self.instantiate(x, memo) for x in t.args]
        elif t.kind == "fun":
            n = T("fun")
            memo[t.uid] = n
            n.args = [self.instantiate(x, memo) for x in t.args]
            n.ret = self.instantiate(t.ret, memo)
            n.eff = self.instantiate(t.eff, memo)
        elif t.kind in ("tags", "rec"):
            n = T(t.kind)
            memo[t.uid] = n
            if t.kind == "tags":
                n.fields = {k: [self.instantiate(x, memo) for x in v] for k, v in t.fields.items()}
            else:
                n.fields = {k: self.instantiate(v, memo) for k, v in t.fields.items()}
            n.ext = self.instantiate(t.ext, memo)
        else:
            return t
        return n

    def has_generic(self, t, seen=None):
        t = find(t)
        if seen is None:
            seen = set()
        if t.uid in seen:
            return False
        seen.add(t.uid)
        if t.kind == "var":
            return t.level == GENERIC
        return any(self.has_generic(c, seen) for c in self.children(t))

    # ------------------------------------------------ R-Subsume: widening copy
    def widen_copy(self, t, top=True):
        """Copy of t with each closed tag row in reach re-opened.

        Reach (design.md Row Subsumption / Closed Result Rows): the direct
        result row, payloads of tags in it, and the error row of a Try.
        """
        t = find(t)
        if t.kind == "tags":
            tg, tail = self.flat_tags(t)
            pay = {k: [self.widen_copy(x, False) for x in v] for k, v in tg.items()}
            if tail.kind == "etags":
                return self.tags(pay, self.fresh())
            if tail.kind == "var" and tail.bounded:
                return self.tags(pay, self.fresh())
            # open row: keep its own tail (shared)
            if all(find(a) is find(b) for k in tg for a, b in zip(tg[k], pay[k])):
                return t
            return self.tags(pay, tail)
        if t.kind == "con" and t.con == "Try":
            return self.try_(t.args[0], self.widen_copy(t.args[1], False))
        return t


# ---------------------------------------------------------------- printer
class Printer:
    """Roc TypeWriter convention: positive-position anonymous flex extensions
    that occur once are hidden; single-occurrence vars print as `_x`."""

    def __init__(self, inf):
        self.inf = inf

    def count(self, t, counts, stack):
        # tree occurrences, guarding only against cycles on the current path
        t = find(t)
        counts[t.uid] = counts.get(t.uid, 0) + 1
        if t.uid in stack:
            return
        stack.add(t.uid)
        for c in self.inf.children(t):
            self.count(c, counts, stack)
        stack.discard(t.uid)

    def show(self, t):
        self.counts = {}
        self.count(t, self.counts, set())
        self.names = {}
        self.n = 0
        return self.go(t, True)

    def vname(self, v):
        if v.uid not in self.names:
            self.names[v.uid] = "v%d" % self.n
            self.n += 1
        nm = self.names[v.uid]
        if v.rigid and v.name:
            return nm   # a named annotation variable prints by name, never `_`
        return ("_" + nm) if self.counts.get(v.uid, 0) <= 1 else nm

    def go(self, t, pos, in_fun_arg=False):
        t = find(t)
        k = t.kind
        if k == "var":
            return self.vname(t)
        if k == "con":
            if not t.args:
                return t.con
            return "%s(%s)" % (t.con, ", ".join(self.go(a, pos) for a in t.args))
        if k == "fun":
            s = "%s %s %s" % (", ".join(self.go(a, not pos, True) for a in t.args),
                              "=>" if find(t.eff).kind == "eeff" else "->", self.go(t.ret, pos, True))
            return "(" + s + ")" if in_fun_arg else s
        if k in ("tags", "etags"):
            tg, tail = self.inf.flat_tags(t)
            items = []
            for name in sorted(tg):
                p = tg[name]
                items.append(name if not p else "%s(%s)" % (name, ", ".join(self.go(x, pos) for x in p)))
            if tail.kind == "var":
                hide = pos and not tail.rigid and self.counts.get(tail.uid, 0) <= 1
                if not hide:
                    if self.counts.get(tail.uid, 0) <= 1:
                        items.append("..")
                    else:
                        items.append(".." + self.vname(tail))
            return "[" + ", ".join(items) + "]"
        if k in ("rec", "erec"):
            fs, tail = self.inf.flat_rec(t)
            items = ["%s: %s" % (f, self.go(fs[f], pos)) for f in sorted(fs)]
            if tail.kind == "var":
                hide = pos and not tail.rigid and self.counts.get(tail.uid, 0) <= 1
                if not hide:
                    items.append(".." if self.counts.get(tail.uid, 0) <= 1 else ".." + self.vname(tail))
            if not items:
                return "{}"
            return "{ " + ", ".join(items) + " }"
        raise AssertionError(k)
