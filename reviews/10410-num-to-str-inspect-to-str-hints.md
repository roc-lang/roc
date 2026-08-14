# PR #10410 — Add hints for `Num.to_str` and `Inspect.to_str` does-not-exist errors

- **Author:** JoeJoeflyn (Tai Nguyen) · **Draft:** no · **Base:** `main`
- **Size:** +85 / −0 across 3 files
- **Fixes:** #9191, #9192

Adds two targeted hints to the `qualified_ident_does_not_exist` report:
`Num.to_str(value)` → suggest `value.to_str()`; `Inspect.to_str` → note the
rename to `Str.inspect`.

## Verdict

Small, well-targeted, and it follows the existing hint convention correctly
(including the two `addLineBreak()` calls that the dominant style in this file
uses — worth noting, since PR #10686 uses one and is therefore the inconsistent
one).

**Four things to fix, all small:** a missing period (#1), a missing snapshot for
half the change (#2), and two robustness/scaling observations (#3, #4).

I confirmed both hints give correct advice: `Str.inspect` exists
(`Builtin.roc:2756`, `inspect : _val -> Str`) and `to_str` is an associated
method on the numeric types (`Builtin.roc:232`, `to_str : a -> Str`), so
`value.to_str()` is right.

---

## Findings

### 1. (Should fix) The `Num.to_str` hint has no terminating period

```zig
try report.document.addReflowingText(", use method syntax: ");
try report.document.addInlineCode("value.to_str()");
// <- nothing here
```

versus the sibling branch, which does end properly:

```zig
try report.document.addInlineCode("Str.inspect");
try report.document.addReflowingText(".");
```

The regenerated `Color.md` shows the result — seven copies of

```
    Hint: Instead of `Num.to_str(value)`, use method syntax: `value.to_str()`
```

with no final period. One `addReflowingText(".")` fixes it.

### 2. (Should fix) There's a snapshot for `Inspect.to_str` but none for `Num.to_str`

`test/snapshots/can_does_not_exist_inspect_to_str_hint.md` is a good, minimal,
well-named snapshot — exactly what `.rules` asks for ("Focused Intent",
"Minimal Complexity", "Clear Naming"). The `Num.to_str` hint gets no equivalent;
its only coverage is incidental, inside `test/snapshots/plume_package/Color.md`,
a large unrelated fixture where the hint appears seven times among other
diagnostics.

That's fragile in both directions: if `Color.md` is ever fixed to not call
`Num.to_str`, the hint loses all coverage silently; and a reader looking for the
`Num.to_str` behavior won't find a file named for it.

Add `test/snapshots/can_does_not_exist_num_to_str_hint.md` mirroring the
`Inspect` one. It's a two-line source file.

### 3. (Should consider) The match is on raw identifier text, not on a resolved module

```zig
if (std.mem.eql(u8, ident_name, "Num.to_str")) {
```

`ident_name` is whatever the user wrote. A project with its own module named
`Inspect` — imported and used as `Inspect.to_str(x)` — that happens to lack
`to_str` will be told their function "has been renamed to `Str.inspect`," which
is false and actively misleading.

Error reporting is explicitly exempt from the AGENTS.md ban on heuristics, so
this isn't a rules violation, and `Num` is a builtin nobody will shadow. But
`Inspect` is an ordinary-looking name. If the diagnostic payload carries the
resolved module (or a "this was a builtin module" flag), gating on that instead
of on the literal string would remove the false positive. If it doesn't, a
comment noting the limitation is enough — just don't leave it looking like the
match is on identity when it's on text.

### 4. (Observation) Seven identical hints in one file is a lot of screen

The `Color.md` diff is the whole story: the same 74-character hint repeated
seven times in one compilation, because the fixture calls `Num.to_str` seven
times. Each occurrence is individually correct; collectively it's noise, and it
pushes the *other* diagnostics in that file further apart.

Not this PR's problem to solve — no "show each hint once per compilation"
mechanism exists — but it's the first hint in the codebase likely to fire many
times in one run, so it's worth knowing that's the behavior before it lands.
If a dedupe mechanism is ever added, these two are the motivating case.

## Things I checked and found fine

- **Placement.** The hints go in `.qualified_ident_does_not_exist`
  (`ModuleEnv.zig:1597`), which is the arm that produces the `DOES NOT EXIST`
  title and whose `ident_name` is the full qualified text (`Num.to_str`), so the
  comparison is against the right string. ✓
- **Emission order** matches the surrounding convention exactly: after
  `addSourceRegion`, then `addLineBreak()` ×2, then
  `addAnnotated("Hint:", .emphasized)`, then the text. Identical to the three
  other hints in this file (`:2391`, `:2440`, `:2784` in the current tree). ✓
- **`addReflowingText(" ")`** in the `Inspect` branch (needed because the next
  element is inline code) is precedented — `src/check/report.zig:496,506` does
  the same, and `addReflowingText` only short-circuits on `len == 0`. ✓
- **`addInlineCode`** is used for all four code spans, so they render as
  backticked in markdown/no-color and colored in the terminal. ✓
- **No behavior change outside reporting** — the diagnostic itself, its region,
  and canonicalization are untouched; the new snapshot's `CANONICALIZE` section
  is the expected `e-runtime-error (tag "erroneous_value_expr")`. ✓

## Merge-order warning

Two open PRs collide with this:

- **#10643** ("Replace error boxes with a simpler format") regenerates every
  snapshot in `test/snapshots/`, including both files here. Whichever lands
  second needs regeneration, not a merge.
- **#10290** ("Improve error messages: unimported module qualifiers and
  nominal-vs-record mismatches") is a draft that reworks qualified-identifier
  diagnostics — quite possibly this same arm. Worth checking whether these two
  hints survive that rework, or whether #10290 subsumes them.
