# Bitcode for Builtins

## Adding a bitcode builtin

To add a builtin:

1. Add the function to the relevant module. For `Num` builtin use it in `src/num.zig`, for `Str` builtins use `src/str.zig`, and so on. **For anything you add, you must add tests for it!** Not only does to make the builtins more maintainable, it's the the easiest way to test these functions on Zig. To run the test, run: `zig build test`
2. Make sure the function is public with the `pub` keyword and uses the C calling convention. This is really easy, just add `pub` and `callconv(.C)` to the function declaration like so: `pub fn atan(num: f64) callconv(.C) f64 { ... }`
3. In `src/main.zig`, export the function. This is also organized by module. For example, for a `Num` function find the `Num` section and add: `comptime { exportNumFn(num.atan, "atan"); }`. The first argument is the function, the second is the name of it in LLVM.
4. In `compiler/builtins/src/bitcode.rs`, add a constant for the new function. This is how we use it in Rust. Once again, this is organized by module, so just find the relevant area and add your new function.
5. You can now use your function in Rust using `call_bitcode_fn` in `llvm/src/build.rs`!

## How it works

Roc's builtins are implemented in the compiler using LLVM only.
When their implementations are simple enough (e.g. addition), they
can be implemented directly in Inkwell.

When their implementations are complex enough, it's nicer to
implement them in a higher-level language like Zig, then compile
the result to LLVM bitcode, and import that bitcode into the compiler.

Compiling the bitcode happens automatically in a Rust build script at `compiler/builtins/build.rs`.
Then `builtins/src/bitcode/rs` staticlly imports the compiled bitcode for use in the compiler.

You can find the compiled bitcode in `target/debug/build/roc_builtins-[some random characters]/out/builtins.bc`.
There will be two directories like `roc_builtins-[some random characters]`, look for the one that has an
`out` directory as a child.

> The bitcode is a bunch of bytes that aren't particularly human-readable.
> If you want to take a look at the human-readable LLVM IR, look at
> `compiler/builtins/bitcode/builtins.ll`

## Calling bitcode functions

use the `call_bitcode_fn` function defined in `llvm/src/build.rs` to call bitcode functions.
