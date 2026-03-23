# Remaining Snapshot Failures (lir-interpreter branch)

## 1. REPL interpreter segfault — `multiline_string_split_7_lines`

**File:** `test/snapshots/repl/multiline_string_split_7_lines.md`

The LIR interpreter segfaults on `input.split_on("\n")` (input 1). The first input
(`input = "L68\nL30\nR48\nL5\nR60\nL55\nL1"`) succeeds. The OUTPUT section was
removed on this branch because the segfault prevented output generation.

The crash occurs in the LIR interpreter path (`evaluateWithInterpreter` in
`repl/eval.zig`), which lowers CIR → MIR → LIR and runs the LIR interpreter.
The `str_split_on` builtin is called via `builtins.str.strSplitOn()` and the
result is converted by `rocListToValue()`.

On `main` the old CIR interpreter handled this correctly.

**Investigation findings:**
- Layout sizes are confirmed matching: arg0=24, arg1=24, ret=24, @sizeOf(RocStr)=24,
  @sizeOf(RocList)=24. **No layout mismatch.**
- Raw bytes of both arguments (str and delimiter) are valid and correct.
- The `strSplitOn` builtin **succeeds** — returns a RocList with 7 elements.
- `rocListToValue` **succeeds** — copies the RocList into the value buffer.
- **The segfault occurs AFTER `evalLowLevel` returns** — during rendering or
  subsequent LIR interpreter processing of the `List Str` result.

**Root cause hypothesis:** The segfault is in the `Str.inspect` wrapping or the
LIR interpreter's rendering of the `List Str` value. The `strSplitOn` builtin
creates seamless slice strings (pointing into the original string's heap memory).
These slices use `incref` on the original string's refcount. If the LIR interpreter
or the rendering path doesn't handle seamless slices correctly (e.g. trying to
access a refcount that doesn't exist, or freeing the original string before
rendering the slices), this would cause a SIGSEGV.

**Suggested next steps:**
- Add tracing after `evalLowLevel` returns to see which expression the interpreter
  evaluates next (likely `Str.inspect` wrapping or a list rendering expression).
- Check if the seamless-slice RocStr values returned by `strSplitOn` have valid
  refcount headers accessible via the original string's allocation.
- Check the `evalList` or list rendering path in the LIR interpreter for how it
  iterates over `List Str` elements — it may be reading element layouts incorrectly.

---

## 2. Cross-def closure evaluation regression

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

**Key code locations:**
- `fold_type.zig:225` — closures explicitly return `.unsupported`
- `value_to_cir.zig:128,268,385` — closures rejected in `replaceExpr`/`createExpr`
- `comptime_evaluator.zig:1458-1492` — isolated per-def evaluation loop

**Suggested investigation:**
- Check whether `tryFoldExprFromValue` can represent closure values (it
  currently can't — only scalars and tags).
- Alternatively, make the comptime evaluator batch-lower related defs in a
  single `lowerExpr` call so closure values stay live across defs.
