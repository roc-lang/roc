"""Differential fuzzing: generated modules and their annotated variants, model vs compiler."""
import os, random, collections, shutil
from concurrent.futures import ThreadPoolExecutor
from lang import Def, AHole, ATags, ARec, AVar, ACon, pp_module
from gen import Gen, type_to_ann, subterms, replace, local_lambdas, annotate_locals
from diff import compare, classify
from roc import OUT


def variants(defs, c, types, rnd):
    """Variants of a module the model accepts. Each one changes the
    annotations only; the model decides the expected verdict again.
      ann        add the model's principal annotation to an unannotated function
      hole       replace one annotation subterm with `_`
      scoped     annotate local lambdas too, sharing names with the outer annotation
      localonly  annotate only the local lambdas
      mut-*      close or open a row, add or drop a tag, make a variable
                 concrete, merge two variables, or replace two subterms with `_`
    """
    out = []
    for i, d in enumerate(defs):
        if not d.name.startswith("f") or d.ann is not None or d.name not in types:
            continue
        a = type_to_ann(c.inf, types[d.name])
        out.append(("ann:" + d.name, with_def(defs, i, Def(d.name, a, d.expr))))
        subs = [p for p, s in subterms(a) if p]
        if subs:
            p = rnd.choice(subs)
            out.append(("hole:%s:%s" % (d.name, p), with_def(defs, i, Def(d.name, replace(a, p, AHole()), d.expr))))
        locs = local_lambdas(d.expr, [])
        if locs:
            names = {}
            outer = type_to_ann(c.inf, types[d.name], names)
            anns = {}
            for n in locs:
                if n in c.def_types:
                    anns[n] = type_to_ann(c.inf, c.def_types[n], names)
            if anns:
                body = annotate_locals(d.expr, anns)
                out.append(("scoped:" + d.name, with_def(defs, i, Def(d.name, outer, body))))
                out.append(("localonly:" + d.name, with_def(defs, i, Def(d.name, None, body))))
        for kind in rnd.sample(["close", "open", "addtag", "droptag", "conc", "merge", "hole2"], 3):
            m = mutate(a, kind, rnd)
            if m is not None:
                out.append(("mut-%s:%s" % (kind, d.name), with_def(defs, i, Def(d.name, m, d.expr))))
    return out


def with_def(defs, i, d):
    v = list(defs)
    v[i] = d
    return v


def mutate(a, kind, rnd):
    subs = list(subterms(a))
    tagsubs = [(p, s) for p, s in subs if isinstance(s, ATags)]
    vars_ = sorted({s.name for p, s in subs if isinstance(s, AVar)} |
                   {s.ext[2:] for p, s in subs if isinstance(s, (ATags, ARec)) and s.ext and len(s.ext) > 2})
    if kind == "close":
        c = [(p, s) for p, s in tagsubs if s.ext]
        if not c: return None
        p, s = rnd.choice(c); return replace(a, p, ATags(s.tags, None))
    if kind == "open":
        c = [(p, s) for p, s in tagsubs if not s.ext]
        if not c: return None
        p, s = rnd.choice(c); return replace(a, p, ATags(s.tags, ".."))
    if kind == "addtag":
        if not tagsubs: return None
        p, s = rnd.choice(tagsubs); tg = dict(s.tags); tg["E"] = []
        return replace(a, p, ATags(tg, s.ext))
    if kind == "droptag":
        c = [(p, s) for p, s in tagsubs if len(s.tags) >= 2]
        if not c: return None
        p, s = rnd.choice(c); tg = dict(s.tags); del tg[rnd.choice(list(tg))]
        return replace(a, p, ATags(tg, s.ext))
    if kind == "conc":
        c = [(p, s) for p, s in subs if isinstance(s, AVar) and p]
        if not c: return None
        p, s = rnd.choice(c); return replace(a, p, ACon("U64", []))
    if kind == "merge":
        c = [(p, s) for p, s in subs if isinstance(s, AVar) and p]
        if len(vars_) < 2 or not c: return None
        p, s = rnd.choice(c)
        return replace(a, p, AVar(rnd.choice([v for v in vars_ if v != s.name])))
    if kind == "hole2":
        c = [p for p, s in subs if p]
        if len(c) < 2: return None
        p1, p2 = rnd.sample(c, 2)
        m = replace(a, p1, AHole())
        try:
            return replace(m, p2, AHole())
        except AssertionError:
            return None  # p2 was inside the subterm p1 replaced
    raise AssertionError(kind)


def compare_generated(defs, d):
    src = pp_module(defs)
    lines = pp_module(defs, header="").strip().splitlines()
    return compare(defs, src, lines, d)


def run_one(seed):
    """All rows for one seed: (seed, variant, kind, model, compiler, note)."""
    defs = Gen(seed).module()
    d = os.path.join(OUT, "fuzz", "chk")
    rows, (mv, c, types) = compare_generated(defs, os.path.join(d, "s%d" % seed))
    out = [(seed, "base", defs) + r for r in rows]
    if mv == "accept":
        rnd = random.Random(seed * 7 + 1)
        for tag, v in variants(defs, c, types, rnd):
            safe = "".join(ch if ch.isalnum() or ch in "-_" else "_" for ch in tag)[:40]
            vrows, _ = compare_generated(v, os.path.join(d, "s%d_%s" % (seed, safe)))
            out += [(seed, tag, v) + r for r in vrows]
    return out


def fuzz(n, seed, jobs):
    """Run seeds [seed, seed + n). Writes out/fuzz/results.tsv and one .roc file
    per disagreement under out/fuzz/disagree/. Returns the number of
    unclassified disagreements."""
    base = os.path.join(OUT, "fuzz")
    dis = os.path.join(base, "disagree")
    shutil.rmtree(dis, ignore_errors=True)
    os.makedirs(dis)
    agree, found = collections.Counter(), collections.defaultdict(list)
    with ThreadPoolExecutor(jobs) as ex, open(os.path.join(base, "results.tsv"), "w") as f:
        f.write("seed\tvariant\tkind\tmodel\tcompiler\tnote\tlabel\n")
        for rows in ex.map(run_one, range(seed, seed + n)):
            for s, tag, defs, kind, m, r, note in rows:
                label = classify((kind, m, r, note))
                cols = [s, tag, kind, m, r, note, label or ""]
                f.write("\t".join(str(x).replace("\t", " ").replace("\n", " ") for x in cols) + "\n")
                group = tag.split(":")[0]
                if label is None:
                    what = "skipped" if kind.startswith("type-skipped") else \
                        "uncertain" if m == "uncertain" else kind.split(":")[0] + " " + m
                    agree["%-10s %s" % (group, what)] += 1
                    continue
                name = "s%d-%s-%s" % (s, "".join(ch if ch.isalnum() else "_" for ch in tag)[:40], kind.split(":")[0])
                with open(os.path.join(dis, name + ".roc"), "w") as g:
                    g.write(pp_module(defs))
                found[label].append("%s  model=%s  compiler=%s  %s" % (name, m, r, note))
            f.flush()
    print("seeds %d..%d" % (seed, seed + n - 1))
    for k in sorted(agree):
        print("  agree  %-34s %d" % (k, agree[k]))
    for k in sorted(found):
        print("%s: %d" % (k, len(found[k])))
        for x in found[k][:5]:
            print("    " + x)
    print("results: %s" % os.path.join(base, "results.tsv"))
    return sum(len(v) for k, v in found.items() if k.startswith("unclassified"))
