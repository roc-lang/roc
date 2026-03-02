# Remaining Fallbacks / Transitional Compromises

Scope:
- Includes release-path fallbacks, placeholder implementations, heuristic recoveries, and TODO panics.
- Excludes intentional invariant policy (`debug panic` + `unreachable` in release).

## CIR -> MIR

1. Method specialization silently falls back to original symbol when target def lookup fails.
- Problematic state: for some specialization paths, if `findDefExprBySymbol` fails, lowering returns the unspecialized symbol instead of enforcing a hard construction rule.
- Should look like: either resolve a concrete target definition or fail by invariant; no silent symbol fallback.
- References: `src/mir/Lower.zig:2375-2384`, `src/mir/Lower.zig:2390-2399`, `src/mir/Lower.zig:2410-2413`.

2. Unresolved typevars are defaulted to concrete monotypes (`dec`/`unit`) in monotype conversion.
- Problematic state: `.flex` / `.rigid` vars default to `dec` when numeral-constrained, else `unit`.
- Should look like: unresolved vars are impossible by construction at this stage (or represented explicitly), not defaulted silently.
- References: `src/mir/Monotype.zig:330-338`.

3. Builtin `List`/`Box` nominal conversion silently defaults to `unit` when missing type args.
- Problematic state: malformed/missing nominal args become `unit_idx`.
- Should look like: nominal arg arity is guaranteed before conversion (or hard invariant failure), with no semantic fallback value.
- References: `src/mir/Monotype.zig:681-688`, `src/mir/Monotype.zig:691-698`.

4. Recursive monotype construction still uses placeholder slots and in-place overwrite.
- Problematic state: reserves `.unit` placeholders for recursive structures/nominals, then mutates those entries later.
- Should look like: recursion-capable construction model without placeholder semantic values.
- References: `src/mir/Monotype.zig:372-383`, `src/mir/Monotype.zig:491-521`, `src/mir/Monotype.zig:524-617`, `src/mir/Monotype.zig:636-656`, `src/mir/Monotype.zig:719-737`.

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

4. `num_abs` has an unknown-layout pass-through fallback.
- Problematic state: unknown return layout returns input location unchanged.
- Should look like: exhaustive layout handling or invariant failure, never silent identity fallback.
- References: `src/backend/dev/LirCodeGen.zig:7000-7007`.

5. Pattern binding silently no-ops for unsupported states.
- Problematic state: `bindPattern` returns early in several branches (`layout_store` missing, unexpected layout tag/location, etc.), skipping bindings.
- Should look like: pattern-bind preconditions are guaranteed; violations are invariant failures, not silent return.
- References: `src/backend/dev/LirCodeGen.zig:10790-10801`, `src/backend/dev/LirCodeGen.zig:10827-10832`, `src/backend/dev/LirCodeGen.zig:10841`, `src/backend/dev/LirCodeGen.zig:11023-11035`.

6. Tag payload multi-pattern fallback uses duplicate same-location binding when payload layout is unexpected.
- Problematic state: for multi-arg tag patterns with non-struct payload layout, it "handles gracefully" by binding every arg to the same offset.
- Should look like: payload layout/pattern arity agreement is guaranteed; no graceful fallback rebinding.
- References: `src/backend/dev/LirCodeGen.zig:11060-11066`.

7. Complex parameter patterns still use register-count guessing.
- Problematic state: unsupported complex patterns "assume 1 register".
- Should look like: full ABI-correct parameter lowering for all pattern forms.
- References: `src/backend/dev/LirCodeGen.zig:15036-15041`, `src/backend/dev/LirCodeGen.zig:15621-15627`.

8. `list_sublist` record field extraction is still name-string driven in codegen.
- Problematic state: field offsets are recovered by matching `"start"`/`"len"` names in backend codegen.
- Should look like: field layout positions/offsets come from authoritative layout metadata keyed structurally, not string-name lookup.
- References: `src/backend/dev/LirCodeGen.zig:660-678`, `src/backend/dev/LirCodeGen.zig:4130-4135`.

9. RC emit helpers silently skip work for unsupported/missing context.
- Problematic state: RC helpers return without emitting RC when value location/`roc_ops` is unavailable (including `loadListDataPtrForRc` false path).
- Should look like: RC preconditions are guaranteed by construction; unexpected states fail by invariant, not no-op return.
- References: `src/backend/dev/LirCodeGen.zig:17059-17065`, `src/backend/dev/LirCodeGen.zig:17078-17083`, `src/backend/dev/LirCodeGen.zig:17117-17127`, `src/backend/dev/LirCodeGen.zig:17170-17177`, `src/backend/dev/LirCodeGen.zig:17219-17226`, `src/backend/dev/LirCodeGen.zig:17282`, `src/backend/dev/LirCodeGen.zig:17309`, `src/backend/dev/LirCodeGen.zig:17337`.

10. Layout-size resolution has fallback defaults when `layout_store` is absent.
- Problematic state: stack location/size logic falls back to hardcoded sizes and `else => 8`.
- Should look like: `layout_store` is always present where needed and layout sizing is authoritative.
- References: `src/backend/dev/LirCodeGen.zig:11126-11131`, `src/backend/dev/LirCodeGen.zig:17714-17729`.

11. Stack float arguments in call builder are unimplemented.
- Problematic state: panics once float args exceed register slots (Windows and SysV paths).
- Should look like: ABI-complete stack float arg emission.
- References: `src/backend/dev/CallingConvention.zig:371-372`, `src/backend/dev/CallingConvention.zig:388-389`, `src/backend/dev/CallingConvention.zig:419-420`, `src/backend/dev/CallingConvention.zig:433-434`.

12. Register allocator still has no spill/reload.
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
