# MIR Branch — Status

## Completed

### Replace `e_low_level_lambda` with real lambdas + `e_run_low_level`

Instead of a bespoke CIR expression variant requiring 30+ special cases across 15 files,
builtin low-level operations are now represented as normal `e_lambda` nodes whose bodies
contain `e_run_low_level`. Every pipeline stage handles them through the existing `e_lambda`
path. Net -188 lines across 18 files. All 2811 tests pass.

**Key changes:**
- Added `e_run_low_level` to CIR Expression, Node, NodeStore
- Builtin compiler emits `e_lambda` + `e_run_low_level` body instead of `e_low_level_lambda`
- Removed `e_low_level_lambda` variant from CIR entirely
- Removed special cases from: ClosureTransformer, LambdaLifter, LambdaSetInference,
  RocEmitter, DependencyGraph, Check, MIR Lower, Mono Lower, Interpreter, Comptime Evaluator
- Interpreter uses `extractLowLevelOp` helper to recognize `e_lambda` + `e_run_low_level`
  at 10 call sites (method dispatch, is_eq, dot access, etc.)

### Replace inline `maxInt(u32)` sentinels with typed `.none` + `.isNone()`

Four raw `@enumFromInt(std.math.maxInt(u32))` sentinels replaced with proper typed `.none`
variants: `StringLiteral.Idx.none`, `MIR.RestIndex.none`, `Import.ResolvedModuleIdx.none`.
All 2811 tests pass.

### Use `undefined` for `start` in empty spans

All 20 Span `empty()` functions across MIR, Monotype, MonoIR, and DataSpan now use
`start = undefined` instead of `start = 0`. Zig's safety-checked undefined catches
accidental reads of `start` on empty spans in debug/safe modes.
All 2811 tests pass.

### Fix cross-module type resolution in `lowerExternalDef`

`lowerExternalDef` switched `current_module_idx` but not `types_store`, causing type
variables from the target module to be resolved against the wrong type store. Each module
has independent type variable numbering (cross-module types are fully copied via
`copy_import.copyVar`), so the same numeric var means different things in different stores.
Fix: save/restore `types_store` and `type_var_seen` when crossing module boundaries, with
a conditional to skip the overhead for same-module calls.
All 2811 tests pass.

### Previous MIR fixes (from earlier commits)

1. `e_for` lowering — uses `for_loop` MIR node
2. `lowerBlock` — all statement types handled explicitly
3. `e_str` multi-segment — left fold with `str_concat`
4. `e_typed_frac` — routes to `dec`/`f64`/`f32` based on `type_name`
5. `e_return` — wraps in `return_expr` MIR node
6. `lowerDotAccess` — resolves methods via receiver type dispatch
7. `patternToSymbol` — propagates `Allocator.Error`
8. `findModuleForOrigin` — callers pass `source_module_idx` directly
9. Binop method symbols — type-directed dispatch on LHS operand

## Open Issues

### Brittleness

10. ~~Magic sentinel values `@enumFromInt(std.math.maxInt(u32))` scattered throughout~~ **DONE**
12. ~~`ExprSpan.empty()` with `start = 0` is ambiguous with non-empty spans at index 0~~ **DONE**
13. ~~`type_var_seen` cache shared across modules but not cleared between them~~ **DONE**
14. ~~`Monotype.Store.fromNominalType` copies resolved monotype by value (stale if mutual recursion)~~ **NOT A BUG** — value-copy is safe because all Monotype fields are indices, not pointers. Added comments explaining why.

### Performance

15. `std.ArrayList` temporaries in tight loops allocate/deallocate repeatedly
16. `lowerNotBool` duplicated across three call sites (~90 lines)
17. No deduplication in `Monotype.Store.addMonotype` (e.g. hundreds of duplicate `Bool`)
18. `findModuleForOrigin` linear scan over all modules

### Testing Gaps

19. No tests for `lowerExpr` — only store/init tests exist
20. No tests for `e_if` to match desugaring
21. No tests for `e_binop` desugaring (and/or short-circuit, operator-to-method)
22. No tests for `e_for`, `e_closure`, `e_match`, `e_block` lowering
23. No tests for `Monotype.Store.fromTypeVar`
24. Cross-module tests only verify type-checking, not MIR lowering
25. No test for recursive types in `fromTypeVar`
26. No test for `lowerExternalDef` recursion guard

### Minor

27. ~~Empty string sentinel is `@enumFromInt(std.math.maxInt(u32))`~~ **DONE (covered by #10)**
28. `lowerExpr` resolves monotype even for error paths
29. `lowerBlock` uses `@enumFromInt(@intFromEnum(expr))` no-op cast
30. `s_var` and `s_reassign` lowered identically to `s_decl` (loses mutability distinction)
