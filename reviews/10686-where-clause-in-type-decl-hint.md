# PR #10686 — Add hint for where clause in type declaration (#10329)

- **Author:** arkh-node (Aleksei Rybnikov) · **Draft:** no · **Base:** `main`
- **Size:** +51 / −16 across 9 files (1 source file, 8 regenerated snapshots)
- **Closes:** #10329

Adds a one-line hint to `WHERE CLAUSE NOT ALLOWED IN TYPE DECLARATION`, and
fixes a typo in the same message ("attempting **do** this" → "attempting **to
do** this").

## Verdict

The typo fix is unambiguously good. **The hint text is factually wrong on
current `main`, and in the most likely case it points the user away from the
right fix.** See finding #1 — that needs a wording change before merge. The
mechanics of the change are otherwise clean and correctly regenerated.

---

## Findings

### 1. (Blocking) The hint is inaccurate — `where` clauses *can* go on a type declaration

The hint asserts:

> `where` clauses can only go on function type annotations.

That is not true as of `main`. **Where aliases** are a type declaration whose
entire body is a `where` clause. From `test/snapshots/where_alias/where_alias_basic.md`:

```roc
a.Stringable : where [a.to_str : a -> Str]

stringify : a -> Str where [a.Stringable]
```

The canonicalizer confirms this is a deliberate carve-out rather than an
accident — all three diagnostic sites are guarded by
`if (type_decl.kind != .where_alias)` (`Can.zig:2263`, `9153` — implicitly via
the alias path, and `9247`). The parser has a dedicated `.where_alias`
`TypeDeclKind` (`AST.zig:885`) with its own error messages
(`AST.zig:532-533`).

This is not a pedantic objection, because of *which* programs hit this
diagnostic. The canonical repro in `test/snapshots/where_clause/where_clauses_1.md`
is:

```roc
Hash(a, hasher) : a
	where [a.hash : hasher -> hasher, hasher.Hasher]
```

The user is trying to name a reusable set of method constraints. That is
precisely what a where alias is for. The new hint tells them to go move the
clause onto a function type annotation — the one thing they were trying to
avoid doing at every call site. The hint doesn't just omit an option; on the
motivating example it recommends the wrong one.

Suggested wording:

> **Hint:** `where` clauses go on function type annotations. To name a reusable
> set of constraints, declare a where alias: `a.Hash : where [a.hash : hasher -> hasher]`.

If that's too long for one line (the PR body notes @Anton-4 asked to keep it
short), a shorter accurate version works too:

> **Hint:** `where` clauses go on function type annotations, or on a where alias
> like `a.Stringable : where [...]`.

Either way, drop "only."

### 2. (Minor) Vertical spacing differs from every sibling hint in the same function

The new code emits **one** `addLineBreak()` before the hint. Every other hint in
`ModuleEnv.zig` emits **two** (see `2389-2391`, `2438-2440`, `2782-2784`).

The rendered difference is real, not theoretical. Compare:

- this PR's output (e.g. `where_clauses_1.md`): one blank line before `Hint:`
- an existing sibling
  (`test/snapshots/nominal/type_module_nominal_field_depends_on_private_toplevel_type.md:28-32`):
  **three** blank lines before `Hint:`

Honestly the new spacing looks better — three blank lines is a lot. But right
now the codebase has two different answers to the same question. Either match
the neighbors, or state in the PR that one blank line is the intended house
style going forward and follow up on the others. Don't leave it undeclared.

### 3. (Nit) `addAnnotated("Hint:", …)` + separate `" "` vs. folding the space into the text

The new code does:

```zig
try report.document.addAnnotated("Hint:", .emphasized);
try report.document.addReflowingText(" ");
try report.document.addInlineCode("where");
```

Sibling sites fold the space into the following text
(`addReflowingText(" Expose the referenced type, make ")`), which isn't possible
here because the next element is inline code. I checked whether a
whitespace-only reflowing element is safe — it is; `check/report.zig:496,506`
already does exactly `addReflowingText(" ")`, and `addReflowingText` only
short-circuits on `len == 0` (`document.zig:353`). So this is fine, just noting
I verified it rather than assuming.

An alternative worth considering: `addReflowingTextWithBackticks`
(`document.zig:361`) would let the whole hint be one call —
``addReflowingTextWithBackticks(" `where` clauses go on function type annotations.")`` —
which is both shorter and harder to get spacing wrong in. It's used elsewhere
and has unit tests (`document.zig:823-869`).

## Things I checked and found fine

- **`Hint:` vs `Tip:`** — the PR body's count is the right call, and matches the
  local convention: `ModuleEnv.zig` uses `addAnnotated("Hint:", .emphasized)`
  at 1762, 2391, 2440, 2784, 2812. Consistent.
- **Snapshot regeneration is complete.** All 8 affected snapshots are updated,
  and every occurrence of the old typo string is gone from `test/`. The
  `EXPECTED` sections didn't need changes because the hint lives in the
  `PROBLEMS` body, not the headline — consistent with how `EXPECTED` works.
- **Annotation choice.** `.emphasized` matches every other `Hint:` in
  `ModuleEnv.zig`. (`reporting/report.zig:358` uses `.suggestion` for its
  generic `addSuggestion` helper, but that's a different code path with its own
  convention; not worth unifying in this PR.)
- **The typo fix** touches the one string and all 8 snapshot occurrences. Clean.

## Merge-order warning

PR **#10643** ("Replace error boxes with a simpler format") rewrites essentially
every snapshot in `test/snapshots/` (437 files, ±20k lines). These 8 files are
guaranteed conflicts. This PR is much smaller and should land first; if #10643
lands first, this one needs a snapshot regeneration rather than a merge.
