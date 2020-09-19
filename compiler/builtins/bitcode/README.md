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
probably with a different hash before the `.bc`. If there's more than one
`*.bc` file in that directory, delete the whole `deps/` directory and re-run
the `cargo rustc` command above to regenerate it.

> If you want to take a look at the human-readable LLVM IR rather than the
> bitcode, run this instead and look for a `.ll` file instead of a `.bc` file:
>
> ```bash
> $ cargo rustc --release --lib -- --emit=llvm-ir
> ```

**Note**: In order to be able to address the bitcode functions by name, they need to be defined with the `#[no_mangle]` attribute.

## Importing the bitcode

The bitcode is a bunch of bytes that aren't particularly human-readable.
Since Roc is designed to be distributed as a single binary, these bytes
need to be included in the raw source somewhere.

The `llvm/src/build.rs` file statically imports these raw bytes
using the [`include_bytes!` macro](https://doc.rust-lang.org/std/macro.include_bytes.html),
so we just need to move the `.bc` file from the previous step to the correct
location.

The current `.bc` file is located at:

```
compiler/gen/src/llvm/builtins.bc
```

...so you want to overwrite it with the new `.bc` file in `target/deps/`

Once that's done, `git status` should show that the `builtins.bc` file
has been changed. Commit that change and you're done!

## Calling bitcode functions

Use the `call_bitcode_fn` function defined in `llvm/src/build.rs` to call bitcode funcitons.
