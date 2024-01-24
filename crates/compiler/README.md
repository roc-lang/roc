# The Roc Compiler

For an overview of the design and architecture of the compiler, see
[DESIGN.md](./DESIGN.md). If you want to dive into the
implementation or get some tips on debugging the compiler, see below

## Getting started with the code

The compiler contains a lot of code! If you're new to the project it can be hard to know where to start. It's useful to have some sort of "main entry point", or at least a "good place to start" for each of the main phases.

After you get into the details, you'll discover that some parts of the compiler have more than one entry point. And things can be interwoven together in subtle and complex ways, for reasons to do with performance, edge case handling, etc. But if this is "day one" for you, and you're just trying to get familiar with things, this should be "good enough".

The compiler is invoked from the CLI via `build_file` in cli/src/build.rs

| Phase                                 | Entry point / main functions                     |
| ------------------------------------- | ------------------------------------------------ |
| Compiler entry point                  | load/src/file.rs: load, load_and_monomorphize    |
| Parse header                          | parse/src/module.rs: parse_header                |
| Parse definitions                     | parse/src/module.rs: module_defs                 |
| Canonicalize                          | can/src/def.rs: canonicalize_defs                |
| Type check                            | solve/src/module.rs: run_solve                   |
| Gather types to specialize            | mono/src/ir.rs: PartialProc::from_named_function |
| Solve specialized types               | mono/src/ir.rs: from_can, with_hole              |
| Insert reference counting             | mono/src/ir.rs: Proc::insert_refcount_operations |
| Code gen (optimized but slow)         | gen_llvm/src/llvm/build.rs: build_procedures     |
| Code gen (unoptimized but fast, CPU)  | gen_dev/src/object_builder.rs: build_module      |
| Code gen (unoptimized but fast, Wasm) | gen_wasm/src/lib.rs: build_module                |

For a more detailed understanding of the compilation phases, see the `Phase`, `BuildTask`, and `Msg` enums in `load/src/file.rs`.

## Debugging the compiler

Please see the [debug flags](./debug_flags/src/lib.rs) for information on how to
ask the compiler to emit debug information during various stages of compilation.

There are some goals for more sophisticated debugging tools:

- A nicer unification debugger, see <https://github.com/roc-lang/roc/issues/2486>.
  Any interest in helping out here is greatly appreciated.

### General Tips

#### Miscompilations

If you observe a miscomplication, you may first want to check the generated mono
IR for your code - maybe there was a problem during specialization or layout
generation. One way to do this is to add a test to `test_mono/src/tests.rs`
and run the tests with `cargo test -p test_mono`; this will write the mono
IR to a file.

#### Typechecking errors

First, try to minimize your reproduction into a test that fits in
[`solve_expr`](./solve/tests/solve_expr.rs).

Once you've done this, check out the `ROC_PRINT_UNIFICATIONS` debug flag. It
will show you where type unification went right and wrong. This is usually
enough to figure out a fix for the bug.

If that doesn't work and you know your error has something to do with ranks,
you may want to instrument `deep_copy_var_help` in [solve](./solve/src/solve.rs).

If that doesn't work, chatting on Zulip is always a good strategy.
