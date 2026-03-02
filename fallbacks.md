# Remaining Transitional Compromises (MIR/LIR/Codegen)

Scope note:
- This list intentionally excludes "debug panic + unreachable in release" invariant checks, because those are explicitly desired.
- This list focuses on remaining transitional behavior, silent degradation, boundary leaks, and partial implementations.

## Workarounds / Placeholder Debt (Non-Fallback)

1. `Monotype.NominalHint` stores module-aware nominal origin metadata.
- Problematic state: monotype layer carries `module_idx + ident` hints used later for dispatch recovery.
- Should look like: dispatch identity comes from checker-owned resolved targets; monotype is module-agnostic.
- References: `src/mir/Monotype.zig:184`, `src/mir/Monotype.zig:680`.

2. `MIR.Symbol` still leaks encoding details via `isReassignable()`.
- Problematic state: symbol low bits are decoded as `Ident.Idx` attributes.
- Should look like: symbol treated as opaque token; mutability tracked out-of-band.
- References: `src/mir/MIR.zig:63`.

3. Recursive external-def lowering uses unit-typed placeholder expressions and patches later.
- Problematic state: temporarily invalid types are introduced and fixed post-hoc.
- Should look like: recursion handling emits correctly typed references without patch-up phase.
- References: `src/mir/Lower.zig:2650`, `src/mir/Lower.zig:2720`.

4. Tail recursion transform injects placeholder literal arguments for wildcard params.
- Problematic state: wildcard arity is maintained via synthetic `i64 0` arg values.
- Should look like: first-class IR representation for ignored join-point args.
- References: `src/lir/TailRecursion.zig:320`.

5. Dev codegen callable captures use placeholder in data slot plus side metadata.
- Problematic state: function-typed capture stores a harmless word and relies on out-of-band `callable_stack_values`.
- Should look like: uniform runtime representation for callable captures, no side-channel workaround.
- References: `src/backend/dev/LirCodeGen.zig:11929`.

6. Dev list location keeps legacy compatibility field semantics.
- Problematic state: `list_stack.data_offset` is retained even though comment says it is no longer meaningful.
- Should look like: remove obsolete field semantics from representation.
- References: `src/backend/dev/LirCodeGen.zig:8492`.

7. Calling convention layer still has aarch64 placeholder register model.
- Problematic state: aarch64 convention uses x86_64 register table placeholder.
- Should look like: dedicated aarch64 calling convention model.
- References: `src/backend/dev/CallingConvention.zig:69`.

8. Frame builder hardcodes frame-pointer policy.
- Problematic state: `use_frame_pointer` is "always true for now".
- Should look like: explicit, policy-driven frame-pointer strategy.
- References: `src/backend/dev/FrameBuilder.zig:67`.

9. LIR symbol-def registration allows release-mode overwrite.
- Problematic state: duplicate registration is only debug-asserted, then overwritten in release.
- Should look like: duplicate symbol defs are structurally impossible or rejected in all build modes.
- References: `src/lir/LirExprStore.zig:389`, `src/lir/LirExprStore.zig:390`.

10. MIR symbol-def registration unconditionally overwrites existing entries.
- Problematic state: `registerSymbolDef` performs `put` with no duplicate guard.
- Should look like: duplicate symbol defs are rejected as invariant violations, not last-write-wins.
- References: `src/mir/MIR.zig:712`, `src/mir/MIR.zig:713`.

11. Wasm lambda-as-value and direct-call closure paths still push dummy placeholders.
- Problematic state: dummy `i32 0` values are used as stand-ins.
- Should look like: principled callable value representation at runtime.
- References: `src/backend/wasm/WasmCodeGen.zig:980`, `src/backend/wasm/WasmCodeGen.zig:6647`.

12. Wasm closure struct-capture layout still computed from ad-hoc repr sizes.
- Problematic state: comment states struct layout is TODO placeholder and runtime size is synthesized.
- Should look like: canonical layout-store-driven struct capture layout.
- References: `src/backend/wasm/WasmCodeGen.zig:6677`.

## Explicit Unimplemented Gaps

13. Dev backend `ll.crash` message forwarding not implemented.
- Problematic state: TODO panic instead of full implementation.
- Should look like: forward user-provided crash message end-to-end.
- References: `src/backend/dev/LirCodeGen.zig:3895`.

14. Dev backend `discriminant_switch` codegen not implemented.
- Problematic state: TODO panic.
- Should look like: full discriminant switch lowering/codegen.
- References: `src/backend/dev/LirCodeGen.zig:10323`.

15. CallBuilder lacks stack float argument support.
- Problematic state: panics when float args exceed register limit.
- Should look like: full ABI-compliant stack float arg lowering.
- References: `src/backend/dev/CallingConvention.zig:331`, `src/backend/dev/CallingConvention.zig:347`, `src/backend/dev/CallingConvention.zig:377`, `src/backend/dev/CallingConvention.zig:390`.

16. Register allocator still panics when registers run out (no spilling).
- Problematic state: no spill/reload path; hard panic on pressure.
- Should look like: real spilling implementation.
- References: `src/backend/dev/mod.zig:192`.

17. Wasm list ops not implemented (`list_sort_with`, `list_drop_at`).
- Problematic state: hard `unreachable` TODOs.
- Should look like: implemented operations or upstream prohibition with hard invariant.
- References: `src/backend/wasm/WasmCodeGen.zig:8765`, `src/backend/wasm/WasmCodeGen.zig:8770`.

18. Wasm composite `num_abs` path unimplemented.
- Problematic state: unary composite abs uses `unreachable`.
- Should look like: full composite numeric abs handling.
- References: `src/backend/wasm/WasmCodeGen.zig:2645`.

19. Wasm `hosted_call` expression path is not implemented in expression generator.
- Problematic state: `hosted_call => unreachable`.
- Should look like: complete hosted call support in wasm codegen or enforced absence upstream.
- References: `src/backend/wasm/WasmCodeGen.zig:1105`.
