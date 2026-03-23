# Remaining Snapshot Failures (lir-interpreter branch)

## 1. Cross-def closure evaluation regression

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
