# Remaining Snapshot Failures (lir-interpreter branch)

## 1. Docs snapshot panics — "reached unreachable code"

**Files:**
- `test/snapshots/docs_static_dispatch.md`
- `test/snapshots/docs_type_module.md`
- `test/snapshots/docs_type_module_visibility.md`
- `test/snapshots/docs_transitive_modules.md`

All four involve type modules compiled through `BuildEnv`. The `unreachable` is
hit somewhere in the parsing/canonicalization/type-checking pipeline — not in the
monomorphizer or Lower. These tests pass on `main`; the regression comes from
changes in `src/check/` (2 082 lines changed on this branch) or
`src/canonicalize/` (the new `open_ext` / `#others` ident added for `..`
rigids). A stack trace from inside `BuildEnv.build()` would pinpoint the exact
location.

**Suggested investigation:**
- Run a single docs snapshot outside the parallel worker pool to get a full
  stack trace (the setjmp/longjmp panic handler swallows it).
- Check whether `CommonIdents.find` panics on `#others` for modules whose
  interner was not seeded by `CommonIdents.init`.
- Review the `src/check/` diff for switch-on-enum exhaustiveness changes that
  could hit a new case.

---

## 2. REPL interpreter segfault — `multiline_string_split_7_lines`

**File:** `test/snapshots/repl/multiline_string_split_7_lines.md`

The interpreter segfaults on `input.split_on("\n")` (input 1). The first input
(`input = "L68\nL30\nR48\nL5\nR60\nL55\nL1"`) succeeds. The OUTPUT section was
removed on this branch because the segfault prevented output generation.

This is a runtime crash in LIR-generated code, likely in the `split_on` builtin
or in how the resulting `List(Str)` is materialised. On `main` the old CIR
interpreter handled this correctly.

**Suggested investigation:**
- Use `dump_generated_code_hex = true` in helpers.zig and insert INT3 before
  `makeExecutable()` to attach gdb.
- Check `str_split_on` lowering in `cir_to_lir.zig` / `MirToLir.zig` for
  layout mismatches (similar to the `str_from_utf8` fix in 80456b794c).

---

## 3. Cross-def closure evaluation regression

**Files:**
- `test/snapshots/mono_nested_closures.md`
- `test/snapshots/mono_static_dispatch_closure.md`

These no longer panic (fixed by `ensureDefiningContextParamsBound` in Lower.zig)
but produce evaluation errors instead of folded constants. On `main` the MONO
section showed `result = 18`; now it shows `result = add_five(3)` with
`COMPTIME EVAL ERROR`.

**Root cause:** The comptime evaluator evaluates each def in an isolated
`lowerExpr` call, creating a fresh `Monomorphize` + `mir.Lower` per def.
Closures returned from one def (e.g. `add_five = make_adder(5)`) cannot be
folded to CIR constants, so the next def (`result = add_five(3)`) must
re-lower the entire call chain. The Lower instance for `result` correctly
resolves the closure's captures now, but the LIR interpreter cannot yet
evaluate the resulting closure-returning-closure pipeline end-to-end.

**Suggested investigation:**
- Check whether `tryFoldExprFromValue` can represent closure values (it
  currently can't — only scalars and tags).
- Alternatively, make the comptime evaluator batch-lower related defs in a
  single `lowerExpr` call so closure values stay live across defs.
