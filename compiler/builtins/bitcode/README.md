# Bitcode for Builtins

Roc's builtins are implemented in the compiler using LLVM only.
When their implementations are simple enough (e.g. addition), they
can be implemented directly in Inkwell.

When their implementations are complex enough, it's nicer to
implement them in a higher-level language like Rust, compile the
result to LLVM bitcode, and import that bitcode into the compiler.

Here is the process for doing that.

## Building the bitcode

The source we'll use to generate the bitcode is in `src/lib.rs` in this directory.

To generate the bitcode, `cd` into `compiler/builtins/bitcode/` and run:

```bash
$ cargo rustc --release --lib -- --emit=llvm-bc
```

Then look in the root `roc` source directory under `target/release/deps/` for a file
with a name like `roc_builtins_bitcode-8da0901c58a73ebf.bc` - except
probably with a different hash before the `.bc`. There should be only one `*.bc` file in that directory.

> If you want to take a look at the human-readable LLVM IR rather than the
> bitcode, run this instead and look for a `.ll` file instead of a `.bc` file:
>
> ```bash
> $ cargo rustc --release --lib -- --emit=llvm-ir
> ```

## Importing the bitcode

The bitcode is a bunch of bytes that aren't particularly human-readable.
Since Roc is designed to be distributed as a single binary, these bytes
need to be included in the raw source somewhere.

We have a script that generates this file and writes it to stdout.
To use it, run this command, replacing `bitcode.bc` with the path to the
generated file in `target/release/deps/` from earlier.

`$ ./import.pl bitcode.bc > ../../gen/src/llvm/builtins.rs`

If the script succeeds, `git status` should show that the appropriate
`.rs` file has been updated.

Before checking it in, make sure to run `cargo fmt` on the root of
the project! Otherwise that file will not be formatted properly and
will fail the build.

Once you've formatted the `builtins.rs` file, check it in and you're done!
