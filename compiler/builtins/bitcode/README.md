# Bitcode for Builtins

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
> `target/debug/build/roc_builtins-[some random characters]/out/builtins.ll`

## Calling bitcode functions

Use the `call_bitcode_fn` function defined in `llvm/src/build.rs` to call bitcode funcitons.
