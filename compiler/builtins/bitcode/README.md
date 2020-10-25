# Bitcode for Builtins

Roc's builtins are implemented in the compiler using LLVM only.
When their implementations are simple enough (e.g. addition), they
can be implemented directly in Inkwell.

When their implementations are complex enough, it's nicer to
implement them in a higher-level language like C (or eventually Zig),
compile the result to LLVM bitcode, and import that bitcode into the compiler.

Compiling the bitcode happens automatically in a Rust build script at `compiler/gen/build.rs`.
You can find the compiled bitcode in `target/debug/build/roc_gen-[some random characters]/out/builtins.bc`.

> If you want to take a look at the human-readable LLVM IR, cd into `compiler/builtins/bitcode` and
> run the following command. It should create `compiler/builtins/bitcode/lib.ll`
>
> ```bash
> zig build-obj src/main.zig -femit-llvm-ir=test.ll -fno-emit-bin --strip 
> ```
>
> NOTE: The full command that we use when generating the bitcode is:
> ```bash
> zig build-obj src/main.zig -femit-llvm-ir=test.ll -fno-emit-bin --strip -O ReleaseSafe
> ```
> This is probably less readable then the first command, because it does some mangling of 
> non-exported names, etc. But if you're hitting a bug, it may be helpful.

The bitcode is a bunch of bytes that aren't particularly human-readable.
Since Roc is designed to be distributed as a single binary, these bytes
need to be included in the raw source somewhere.

The `llvm/src/build.rs` file statically imports these raw bytes.

## Calling bitcode functions

Use the `call_bitcode_fn` function defined in `llvm/src/build.rs` to call bitcode funcitons.
