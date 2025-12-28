# Fix Platform-Specific i128 ABI Handling

## Problem

Different platforms represent i128 values differently when passing them to/from functions. The Zig LLVM backend doesn't account for these differences, which will cause issues when integrating with the Zig-compiled builtins bitcode.

## Platform-Specific i128 Representations

1. **Windows**: Uses `<2 x i64>` vector type for i128 values
2. **macOS ARM64**: Uses `[2 x i64]` array type for i128 values
3. **Other platforms**: Use native `i128` type

## How the Rust Backend Handles This

The Rust backend has explicit handling in `crates/compiler/gen_llvm/src/llvm/bitcode.rs:41-72`:

```rust
if env.target.operating_system() == roc_target::OperatingSystem::Windows {
    // On windows zig uses a vector type <2xi64> instead of a i128 value
    let vec_type = env.context.i64_type().vec_type(2);
    if ret.get_type() == vec_type.into() {
        return env
            .builder
            .build_bit_cast(ret, env.context.i128_type(), "return_i128")
            .unwrap();
    }
} else if env.target == roc_target::Target::MacArm64 {
    // Essentially the same happens on macos arm64, but with array type [2xi64].
    let i64_type = env.context.i64_type();
    let arr_type = i64_type.array_type(2);
    if ret.get_type() == arr_type.into() {
        // ... bitcast to i128
    }
}
```

And similarly when passing i128 arguments to functions (lines 96-100):

```rust
if env.target.operating_system() == roc_target::OperatingSystem::Windows {
    if x.get_type() == env.context.i128_type().into() {
        // Pass by reference instead
    }
}
```

## Context

The Roc builtins are compiled from Zig to LLVM bitcode. Zig's code generation for i128 varies by platform:

- On Windows, Zig uses a vector of two i64s because MSVC doesn't have native i128 support
- On macOS ARM64, Zig uses an array of two i64s due to ARM64 ABI conventions
- On x86-64 Linux/macOS, native i128 is typically used

When the LLVM backend calls into builtin functions (like decimal operations, checked arithmetic, etc.), the i128 values need to be in the correct format for that platform.

## Current State in Zig Backend

The Zig backend files that would need this handling:

- `src/eval/llvm_evaluator.zig` - When emitting i128 constants or calling functions with i128 args
- `src/llvm_compile/compile.zig` - JIT execution
- `src/cli/llvm_eval.zig` - AOT compilation

Currently none of these files have platform-specific i128 handling.

## What Needs to Change

1. **Detect the target platform**: The code already has access to `builtin.os.tag` and `builtin.cpu.arch` from Zig's `@import("builtin")`

2. **When returning i128 from a builtin call**: Check if the return type is `<2 x i64>` (Windows) or `[2 x i64]` (macOS ARM64) and bitcast to i128

3. **When passing i128 to a builtin call**: Convert the i128 to the appropriate representation for the platform

4. **Helper function**: Consider creating a helper like:
   ```zig
   fn normalizeI128Return(value: LlvmValue, target_os: std.Target.Os.Tag, target_arch: std.Target.Cpu.Arch) LlvmValue
   fn prepareI128Arg(value: LlvmValue, target_os: std.Target.Os.Tag, target_arch: std.Target.Cpu.Arch) LlvmValue
   ```

## When This Matters

This becomes critical when:
- Calling Dec operations (Dec is stored as i128)
- Using checked arithmetic that might involve i128
- Any builtin function that takes or returns i128

For now, if the backend only handles numeric literals without calling any builtins, this may not immediately cause issues. But it will become critical as more functionality is added.

## Files to Modify

- `src/eval/llvm_evaluator.zig` - Add platform-aware i128 handling
- Potentially `src/llvm_compile/bindings.zig` - May need additional LLVM type helpers

## Testing

- Test i128 operations on macOS ARM64 specifically
- Test i128 operations on Windows (if available)
- The existing dual-mode snapshot tests will help catch mismatches

## Commit Guidelines

Commit your changes frequently as you make progress. However, **never commit any files in the `planning/` directory** - these planning documents are for reference only and should not be checked into version control.
