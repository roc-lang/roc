"""One differential comparison: the model and the compiler on one module."""
from lang import Checker, is_generalizing
from ref import TypeError_, Printer
from roc import roc_check, roc_types, compare_types

ISSUE = "https://github.com/roc-lang/roc/issues/"


def model_module(defs):
    """Run the model on a module. Returns (verdict, checker, {name: type}, error).
    verdict is accept, reject, or uncertain (R-KnownScrutinee)."""
    c, env, types = Checker(), {}, {}
    try:
        for d in defs:
            types[d.name] = c.check_def(d.name, d.ann, d.expr, env, top=True)
        c.close_top_values()
    except TypeError_ as ex:
        if ex.rule == "known-scrutinee":
            return "uncertain", None, None, str(ex)
        return "reject", None, None, "%s: %s" % (ex.rule, ex)
    return "accept", c, types, None


def shown_types(c, defs, types):
    """The model's printed type of each generalized top-level definition."""
    pr = Printer(c.inf)
    return {d.name: pr.show(types[d.name]) for d in defs if is_generalizing(d.expr)}


def compare(defs, src, def_lines, d, filename="H.roc"):
    """Compare the model with `roc check` on `src` (written to d/filename) and,
    when both accept, with REPL `:t` fed `def_lines`.
    Returns (rows, model result); a row is (kind, model, compiler, note)."""
    mv, c, types, err = model_module(defs)
    rv, _ = roc_check(src, d, filename)
    rows = [("verdict", mv, rv, err or "")]
    if mv == "accept" and rv == "accept":
        mine = shown_types(c, defs, types)
        got = roc_types(def_lines, list(mine))
        for n, m in mine.items():
            if n not in got:
                rows.append(("type-skipped:" + n, m, "", "REPL printed no type"))
                continue
            lab = compare_types(m, got[n])
            if lab:
                rows.append(("%s:%s" % (lab, n), m, got[n], ""))
    return rows, (mv, c, types)


def classify(row):
    """None when the row is an agreement or not comparable; otherwise a short
    label naming the known issue it matches, or `unclassified`."""
    kind, m, r, note = row
    if kind.startswith("type-skipped"):
        return None
    if kind.startswith("type-names"):
        return ISSUE + "13300"
    if kind.startswith("type"):
        return "unclassified type disagreement"
    if m == "uncertain" or m == r:
        return None
    if r == "panic" and note.startswith("rowkind"):
        return ISSUE + "13296"
    if "bounded row meets rigid" in note:
        return ISSUE + "13297"
    if "rigid escapes" in note:
        return ISSUE + "13298"
    return "unclassified verdict disagreement"
