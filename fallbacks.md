# Remaining Fallbacks / Transitional Compromises

Scope:
- Includes release-path fallbacks, placeholder implementations, heuristic recoveries, and TODO panics.
- Excludes intentional invariant policy (`debug panic` + `unreachable` in release).

## CIR -> MIR

No high-confidence release-path fallback/placeholder behavior found in this pass.

## MIR -> LIR

No high-confidence release-path fallback/placeholder behavior found in this pass.

## LIR -> Codegen (Dev backend)

1. `ll.crash` message forwarding is unimplemented.
- Problematic state: TODO panic path instead of real user-message propagation.
- Should look like: crash message payload is fully threaded through and emitted.
- References: `src/backend/dev/LirCodeGen.zig:3898-3901`.

2. `discriminant_switch` codegen is unimplemented.
- Problematic state: TODO panic.
- Should look like: full discriminant-switch lowering/codegen implementation.
- References: `src/backend/dev/LirCodeGen.zig:10376-10379`.

3. Multiple low-level ops are still unimplemented in production path.
- Problematic state: op bucket panics at runtime (`num_pow`, `num_sqrt`, `num_log`, `num_round`, `num_floor`, `num_ceiling`, `num_to_str`, `num_from_numeral`, `num_is_zero`, `list_drop_at`, `compare`).
- Should look like: each op has a complete lowering/codegen implementation (or is eliminated upstream by construction).
- References: `src/backend/dev/LirCodeGen.zig:3695-3709`.

4. Stack float arguments in call builder are unimplemented.
- Problematic state: panics once float args exceed register slots (Windows and SysV paths).
- Should look like: ABI-complete stack float arg emission.
- References: `src/backend/dev/CallingConvention.zig:371-372`, `src/backend/dev/CallingConvention.zig:388-389`, `src/backend/dev/CallingConvention.zig:419-420`, `src/backend/dev/CallingConvention.zig:433-434`.

5. Register allocator still has no spill/reload.
- Problematic state: out-of-register pressure panics.
- Should look like: real spill/reload strategy with ABI-correct reload points.
- References: `src/backend/dev/mod.zig:193-196`, `src/backend/dev/mod.zig:203-206`.

## LIR -> Codegen (Wasm backend)

1. `hosted_call` lowering is unimplemented.
- Problematic state: TODO panic path in expression generation.
- Should look like: complete hosted-call lowering or upstream elimination by construction.
- References: `src/backend/wasm/WasmCodeGen.zig:1065-1066`.

2. Composite `num_abs` is unimplemented.
- Problematic state: TODO panic for i128/dec composite unary abs.
- Should look like: complete composite abs implementation.
- References: `src/backend/wasm/WasmCodeGen.zig:3184-3185`.

3. `list_sort_with` is unimplemented.
- Problematic state: TODO panic.
- Should look like: full wasm implementation.
- References: `src/backend/wasm/WasmCodeGen.zig:9436-9437`.

4. `list_drop_at` is unimplemented.
- Problematic state: TODO panic.
- Should look like: full wasm implementation.
- References: `src/backend/wasm/WasmCodeGen.zig:9441-9442`.

5. Expression value typing has conservative default fallback.
- Problematic state: unhandled expr tags default to `.i64`.
- Should look like: exhaustive typing over expression variants (or hard invariant on unknown variants).
- References: `src/backend/wasm/WasmCodeGen.zig:2266`.

6. Tag payload wildcard binding uses fixed-size skip.
- Problematic state: wildcard payload pattern increments by hardcoded 4 bytes.
- Should look like: wildcard skip size derived from actual payload layout.
- References: `src/backend/wasm/WasmCodeGen.zig:1855-1859`.

7. Wildcard lambda parameters default to `.i32`.
- Problematic state: wildcard params ignore declared layout and always allocate as i32.
- Should look like: wildcard param storage/type derived from parameter layout.
- References: `src/backend/wasm/WasmCodeGen.zig:4947-4950`, `src/backend/wasm/WasmCodeGen.zig:5054-5056`.

8. `list_sublist` assumes hardcoded record field offsets/order.
- Problematic state: backend assumes `{ len, start }` sorted layout with fixed offsets (0 and 8).
- Should look like: offsets derived from record layout metadata, not hardcoded assumptions.
- References: `src/backend/wasm/WasmCodeGen.zig:9975-10003`.

9. i128/u128 to float conversions are explicitly approximate.
- Problematic state: conversion uses `high * 2^64 + low` approximation path.
- Should look like: precise and spec-aligned conversion semantics (or explicit upstream prohibition).
- References: `src/backend/wasm/WasmCodeGen.zig:11280-11320`.

10. Missing closure capture materialization falls back to zero-initialization.
- Problematic state: if capture cannot be found/materialized, code stores zero bytes instead of failing.
- Should look like: capture materialization is guaranteed; missing capture is invariant failure, not zero-fill fallback.
- References: `src/backend/wasm/WasmCodeGen.zig:7451-7462`.

## LIR -> Codegen (LLVM backend)

1. Pointer scalar bit-width queries still panic with TODO.
- Problematic state: pointer scalar width paths call `@panic("TODO: query data layout")`.
- Should look like: target data layout drives pointer width queries everywhere.
- References: `src/backend/llvm/Builder.zig:506`, `src/backend/llvm/Builder.zig:515`.

2. `targetLayoutType` is unimplemented.
- Problematic state: direct TODO panic.
- Should look like: full target-layout type mapping implementation.
- References: `src/backend/llvm/Builder.zig:679-680`.
