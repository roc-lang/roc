# Fix i128 Truncation in LLVM Evaluator

## Problem

The LLVM evaluator silently truncates 128-bit integers to 64 bits, producing incorrect results for any i128/u128 value that doesn't fit in 64 bits.

## Location

`src/eval/llvm_evaluator.zig`, line 404:

```zig
const truncated: i64 = @intCast(int_value);
```

This line takes an `i128` value and casts it to `i64`, which will panic at runtime if the value is too large, or silently produce wrong results if using release mode.

## Context

The LLVM evaluator compiles Roc expressions to LLVM IR for execution. When handling numeric literals (`e_num`), it needs to emit LLVM constants for the values. Currently, the code path is:

1. `emitExprValue()` is called with a CIR expression
2. For `.e_num`, it extracts the value via `num.value.toI128()` (returns a full 128-bit integer)
3. It then truncates this to `i64` before passing to `builder.intConst()`

The `LlvmBuilder.intConst()` function from Zig's standard library can handle different integer widths based on the LLVM type passed to it.

## How the Rust Backend Handles This

The Rust LLVM backend in `crates/compiler/gen_llvm/` properly uses `ctx.i128_type()` for U128/I128 values throughout. You can see this in:

- `crates/compiler/gen_llvm/src/llvm/intrinsics.rs:64-70` - Uses `ctx.i128_type()` for both U128 and I128
- `crates/compiler/gen_llvm/src/llvm/lowlevel.rs:221,266,310` - Properly identifies i128 types

## What Needs to Change

1. The `emitExprValue()` function needs to handle i128/u128 values without truncation

2. The `LlvmBuilder.intConst()` function signature should be checked - it likely accepts a type parameter that determines the width. For i128 values, you need to pass the full 128-bit value.

3. Looking at the Zig standard library's LLVM builder (`std.zig.llvm.Builder`), you'll need to understand how it handles 128-bit integer constants. The builder likely has a way to create i128 constants directly.

4. The `getExprLlvmType()` function at line 380 already correctly returns `.i128` for u128/i128 types, so the type mapping is correct - it's just the value emission that's broken.

## Files to Modify

- `src/eval/llvm_evaluator.zig` - Fix the `emitExprValue()` function

## Testing

After the fix:
- `170141183460469231731687303715884105727i128` (i128 max) should evaluate correctly
- `-170141183460469231731687303715884105728i128` (i128 min) should evaluate correctly
- Large u128 values like `340282366920938463463374607431768211455u128` should work

The snapshot tests in `src/snapshots/` with `type=repl` will run both the interpreter and LLVM backend and compare results, so adding test cases there can verify correctness.

## Commit Guidelines

Commit your changes frequently as you make progress. However, **never commit any files in the `planning/` directory** - these planning documents are for reference only and should not be checked into version control.
