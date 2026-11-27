"""The compiler side: `roc check` verdicts, REPL `:t` types, and type canonicalization."""
import os, re, subprocess

ROC = os.environ.get("ROC", "roc")
OUT = os.environ.get("OUT", os.path.join(os.path.dirname(os.path.abspath(__file__)), "out"))


def roc_check(src, d, filename="H.roc"):
    """Write `src` to d/filename and run `roc check` on it.
    Returns (verdict, output); verdict is accept, reject, panic, timeout or crash(N)."""
    os.makedirs(d, exist_ok=True)
    with open(os.path.join(d, filename), "w") as f:
        f.write(src)
    try:
        p = subprocess.run([ROC, "check", filename], cwd=d, capture_output=True, text=True, timeout=120)
    except subprocess.TimeoutExpired:
        return "timeout", ""
    out = p.stdout + p.stderr
    if "panic" in out:
        return "panic", out
    if re.search(r"── ✗ ([^─]+?) ─", out):
        return "reject", out
    if p.returncode not in (0, 2) and "warning" not in out:
        return "crash(%d)" % p.returncode, out
    return "accept", out


def roc_types(def_lines, names):
    """Feed definitions to the REPL and read `:t name` for each name.
    Returns {name: printed type}; a name the REPL printed nothing for is absent."""
    inp = list(def_lines) + [":t " + n for n in names]
    try:
        p = subprocess.run([ROC, "repl"], input="\n".join(inp) + "\n", capture_output=True, text=True, timeout=120)
    except subprocess.TimeoutExpired:
        return {}
    res = {}
    for n in names:
        m = re.search(r"^%s : (.*)$" % re.escape(n), p.stdout, re.M)
        if m:
            res[n] = m.group(1).strip()
    return res


TOK = re.compile(r"\.\.([a-z_][A-Za-z0-9_]*)?|[A-Za-z_][A-Za-z0-9_]*|.")


def canon(s):
    """Rename type variables by first appearance. A variable that occurs once
    prints `_` however it is spelled: the model prints `_x`, Roc keeps source
    names on instantiated flex variables."""
    if s is None:
        return None
    toks = [m.group(0) for m in TOK.finditer(s)]
    occ = {}
    for j, t in enumerate(toks):
        nxt = toks[j + 1] if j + 1 < len(toks) else ""
        key = t[2:] if t.startswith("..") else t
        if key and re.match(r"[a-z_]", key) and nxt != ":" and key != "where":
            occ[key.lstrip("_")] = occ.get(key.lstrip("_"), 0) + 1
    out, names = [], {}
    for j, t in enumerate(toks):
        nxt = toks[j + 1] if j + 1 < len(toks) else ""
        if t.startswith(".."):
            v = t[2:]
            if v and occ.get(v.lstrip("_"), 0) > 1:
                names.setdefault(v, "t%d" % len(names))
                out.append(".." + names[v])
            else:
                out.append("..")
        elif re.match(r"[a-z_]", t) and nxt != ":" and t != "where":
            if t.startswith("_") or occ.get(t, 0) <= 1:
                out.append("_")
            else:
                names.setdefault(t, "t%d" % len(names))
                out.append(names[t])
        elif t != " ":
            out.append(t)
    return "".join(out)


def canon_loose(s):
    """canon with every type variable replaced by `*` (identity dropped)."""
    if s is None:
        return None
    return re.sub(r"\bt\d+\b|\b_\b", "*", canon(s))


def compare_types(model, compiler):
    """Returns None when the printed types agree up to renaming, else a label:
    `type-names` when they agree once variable identity is dropped (Roc printed
    two variables with one name, https://github.com/roc-lang/roc/issues/13300),
    `type` otherwise."""
    if canon(model) == canon(compiler):
        return None
    if canon_loose(model) == canon_loose(compiler):
        return "type-names"
    return "type"
