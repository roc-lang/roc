# Fix Decimal Precision Loss in LLVM Evaluator

## Problem

The LLVM evaluator converts Decimal values to f64 (64-bit floating point) for display, which loses precision. Roc's Dec type is a 128-bit fixed-point decimal that can represent values with up to 18 decimal places precisely. Converting to f64 loses this precision.

## Location

`src/eval/llvm_evaluator.zig`, lines 413-420:

```zig
.e_dec => |dec| {
    const scaled: f64 = @as(f64, @floatFromInt(dec.value.num)) / 1e18;
    return builder.doubleConst(scaled) catch return error.CompilationFailed;
},
.e_dec_small => |dec| {
    const numerator: f64 = @floatFromInt(dec.value.numerator);
    const divisor: f64 = std.math.pow(f64, 10.0, @floatFromInt(dec.value.denominator_power_of_ten));
    return builder.doubleConst(numerator / divisor) catch return error.CompilationFailed;
},
```

And similarly in `evalExprDirectly()` at lines 471-479:

```zig
.e_dec => |dec| {
    const scaled: f64 = @as(f64, @floatFromInt(dec.value.num)) / 1e18;
    return std.fmt.allocPrint(self.allocator, "{d}", .{scaled}) catch return error.OutOfMemory;
},
```

## Context

Roc's `Dec` type is internally stored as an i128 scaled by 10^18. This allows exact representation of decimal values with up to 18 decimal places. For example:
- `1.5dec` is stored as `1500000000000000000` (1.5 * 10^18)
- `0.000000000000000001dec` is stored as `1`

When you convert this to f64, you lose precision because f64 only has ~15-17 significant decimal digits of precision.

## How the Rust Backend Handles This

The Rust LLVM backend keeps Dec as i128 internally and uses dedicated bitcode functions for decimal operations:

- `crates/compiler/gen_llvm/src/llvm/lowlevel.rs:2044-2069` - The `dec_to_str()` function
- It calls `bitcode::DEC_TO_STR` which is a Zig-compiled function that properly converts the i128 representation to a string

The key insight is that the Rust backend never converts Dec to a floating-point type. It keeps it as i128 and calls into pre-compiled Zig builtins for operations like `dec_to_str`.

## What Needs to Change

There are two approaches:

### Approach A: Call the Dec-to-String Builtin

The proper fix is to call the same `dec_to_str` builtin that the Rust backend uses. This requires:

1. Loading the compiled builtins bitcode (already done in the evaluator via `compiled_builtins`)
2. Linking or calling into the `roc_builtins.dec.toStr` function
3. This function takes an i128 (the raw Dec value) and returns a RocStr

This is the correct long-term approach but requires more infrastructure.

### Approach B: Implement Dec-to-String in Zig

For the REPL evaluator specifically, you could implement decimal-to-string conversion directly in Zig:

1. Keep the value as i128
2. Format it properly: extract the integer part (value / 10^18) and fractional part (value % 10^18)
3. Handle negative numbers, trailing zeros, etc.

This is more self-contained but duplicates logic that exists in the builtins.

## Dec Representation Details

From the CIR types, Dec values are stored in two ways:

1. `e_dec` - Full Dec with `value.num: i128` representing the value scaled by 10^18
2. `e_dec_small` - Small Dec with `numerator` and `denominator_power_of_ten` fields

The interpreter's handling of Dec values (in `src/eval/mod.zig`) shows how these should be formatted.

## Files to Modify

- `src/eval/llvm_evaluator.zig` - Fix both `emitExprValue()` and `evalExprDirectly()` for Dec types

## Testing

After the fix:
- `1.123456789012345678dec` should display all 18 decimal places correctly
- `0.000000000000000001dec` should display as `0.000000000000000001`
- Large decimals near the i128 limits should work correctly

## Commit Guidelines

Commit your changes frequently as you make progress. However, **never commit any files in the `planning/` directory** - these planning documents are for reference only and should not be checked into version control.
