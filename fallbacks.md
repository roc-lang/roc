# Remaining Transitional Compromises (MIR/LIR/Codegen)

Scope note:
- This list intentionally excludes "debug panic + unreachable in release" invariant checks, because those are explicitly desired.
- This list focuses on remaining transitional behavior, silent degradation, boundary leaks, and partial implementations.

## Explicit Unimplemented Gaps

1. Dev backend `ll.crash` message forwarding not implemented.
- Problematic state: TODO panic instead of full implementation.
- Should look like: forward user-provided crash message end-to-end.
- References: `src/backend/dev/LirCodeGen.zig:3895`.

2. Dev backend `discriminant_switch` codegen not implemented.
- Problematic state: TODO panic.
- Should look like: full discriminant switch lowering/codegen.
- References: `src/backend/dev/LirCodeGen.zig:10323`.

3. CallBuilder lacks stack float argument support.
- Problematic state: panics when float args exceed register limit.
- Should look like: full ABI-compliant stack float arg lowering.
- References: `src/backend/dev/CallingConvention.zig:331`, `src/backend/dev/CallingConvention.zig:347`, `src/backend/dev/CallingConvention.zig:377`, `src/backend/dev/CallingConvention.zig:390`.

4. Register allocator still panics when registers run out (no spilling).
- Problematic state: no spill/reload path; hard panic on pressure.
- Should look like: real spilling implementation.
- References: `src/backend/dev/mod.zig:192`.

5. Wasm list ops not implemented (`list_sort_with`, `list_drop_at`).
- Problematic state: hard `unreachable` TODOs.
- Should look like: implemented operations or upstream prohibition with hard invariant.
- References: `src/backend/wasm/WasmCodeGen.zig:8765`, `src/backend/wasm/WasmCodeGen.zig:8770`.

6. Wasm composite `num_abs` path unimplemented.
- Problematic state: unary composite abs uses `unreachable`.
- Should look like: full composite numeric abs handling.
- References: `src/backend/wasm/WasmCodeGen.zig:2645`.

7. Wasm `hosted_call` expression path is not implemented in expression generator.
- Problematic state: `hosted_call => unreachable`.
- Should look like: complete hosted call support in wasm codegen or enforced absence upstream.
- References: `src/backend/wasm/WasmCodeGen.zig:1105`.

8. Recursive external-def lowering uses unit-typed placeholder expressions and patches later.
- Problematic state: temporarily invalid types are introduced and fixed post-hoc.
- Should look like: recursion handling emits correctly typed references without patch-up phase.
- References: `src/mir/Lower.zig:2650`, `src/mir/Lower.zig:2720`.

9. Tail recursion transform injects placeholder literal arguments for wildcard params.
- Problematic state: wildcard arity is maintained via synthetic `i64 0` arg values.
- Should look like: first-class IR representation for ignored join-point args.
- References: `src/lir/TailRecursion.zig:320`.

10. Dev codegen callable captures use placeholder in data slot plus side metadata.
- Problematic state: function-typed capture stores a harmless word and relies on out-of-band `callable_stack_values`.
- Should look like: uniform runtime representation for callable captures, no side-channel workaround.
- References: `src/backend/dev/LirCodeGen.zig:11929`.
