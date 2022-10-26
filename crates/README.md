
# Roc Internals

Roc has different crates for various binaries and libraries used by the toolset. These are described below to help get you started. If you see something here that is out of date, please correct it or follow up in the [Zulip chat](https://roc.zulipchat.com/).

Use `cargo doc` to generate docs for most crates; e.g. ```cargo doc --package roc_ast --open```.

## Roc AST `roc_ast`

The AST is used by the editor and (soon) docs. In contrast to the compiler, the types in AST do not keep track of a location in a file.

## Roc CLI `roc_cli` and `roc` binary

Builds the `roc` binary that brings together all of functionality in the Roc toolset.

## Roc CLI Utils `cli_utils`

Provides shared code for cli tests and benchmarks

## Roc Markup `roc_code_markup`

Roc's very own markup language. This library is used by the editor and docs.

## Roc Compiler

Take a `.roc` file and compile into an executable binary. See [compiler/README.md](./compiler/README.md) for more information.

The compiles includes the following sub-crates;
- `roc_alias_analysis` TODO - Need assistance
- `arena-pool` TODO - Need assistance
- `roc_build` TODO - Need assistance
- `roc_builtins` provdes the Roc functions and modules that are implicitly imported into every modulesee. See [README.md](./compiler/builtins/README.md) for more information.
- `roc_can` TODO - Need assistance
- `roc_collections` TODO - Need assistance
- `roc_constrain` TODO - Need assistance
- `roc_debug_flags` TODO - Need assistance
- `roc_derive` provides auto-derivers?
- `roc_exhaustive` provides exhaustiveness checking for Roc 
- `roc_fmt` TODO - Need assistance
- `roc_gen_dev` provides the development backend to generate Roc binaries extremely fast. See [README.md](./compiler/gen_dev/README.md) for more information.
- `roc_gen_llvm` provides the LLVM backend to generate Roc binaries.
- `roc_gen_wasm` provides the WASM backend to generate Roc binaries. See [README.md](./compiler/gen_wasm/README.md) for more information.
- `roc_ident` provides identifiers
- `roc_intern` provides generic interners for concurrent and single-thread use cases.
- `roc_late_solve` provides type unification and solving primitives from the perspective of the compiler backend.
- `roc_load` used to load a `.roc` file and typecheck
- `roc_load_internal` TODO - Need assistance
- `roc_module` TODO - Need assistance
- `roc_mono` TODO - Need assistance
- `roc_parse` TODO - Need assistance
- `roc_problem` provides types to describe problems that can occur when compiling `.roc` code
- `roc_region` TODO - Need assistance
- `roc_target` provides types and helpers for compiler targets such as `default_x86_64`
- `roc_serialize` provides helpers for serializing and deserializing bytes
- `roc_solve` see [Ambient Lambda Set Specialization](./compiler/solve/docs/ambient_lambda_set_specialization.md) for more information on how polymorphic lambda sets are specialized and resolved in the compiler's type solver. 
- `roc_solve_problem` provides types to describe problems that can occur during solving
- `roc_str` provides `Roc` styled collection reference counting. See [README.md](./compiler/str/README.md) for more information.
- `test_derive` TODO - Need assistance
- `test_gen` containes all of Roc's generation tests. See [README.md](./compiler/test_gen/README.md) for more information.
- `test_mono` TODO - Need assistance
- `test_mono_macros` TODO - Need assistance
- `roc_types` TODO - Need assistance
- `roc_unify` TODO - Need assistance

## Roc Docs `roc_docs`

Provides the functionality for generating html documentation from Roc files.

## Roc Docs CLI `roc_docs_cli` and `roc-docs` binary

Provides a binary that is only used for static build servers.

## Roc Editor `roc_editor`

Provdes the functionality for editing Roc files. See [README.md](./editor/README.md) for more information.

## Roc Error Macros `roc_error_macros`

Provides macros for consistent reporting of errors in Roc's rust code.

## Roc Glue `roc_glue`

The `roc_glue` crate generates rust code to connect a Roc app with a rust platform. This tool is not necessary for writing a platform in another language, however, it's an added convenience for rust platform devs.

## Roc Highlight `roc_highlight`

Provides syntax highlighting for Roc by transforming a string and to markup nodes.

## Roc Surgical Linker `roc_linker`

Provides the Roc surgical linker which linkes platforms to Roc applications. See [README.md](./linker/README.md) for more information.

## Roc REPL CLI `roc_repl_cli`

Provides the command line interface functionality for the REPL.

## Roc REPL Eval `roc_repl_eval`

Provides the functioanlity for the REPL to evaluate Roc expressions.

## Roc REPL Expect `roc_repl_expect`

TODO - Need assistance

## Roc REPL Test `repl_test`

TODO - Need assistance

## Roc REPL WASM `roc_repl_wasm`

Provides a build of the REPL for the Roc website using WASM. See [README.md](./repl_wasm/README.md) for more information.

## Roc Reporting `roc_reporting`

TODO - Need assistance

## Roc Std Library `roc_std`

Provides Rust representations of Roc data structures.

## Roc Test Utilities `roc_test_utils`

Provides testing utility functions for use throughout the Rust code base.

## Roc Tracing `roc_tracing`

Provides tracing utility functions for tracing various executable entry points.

## Roc Utilities `roc_utils`

Provides assorted utility functions used all over the code base.

## Vendored Code `./vendor/`

These are files that were originally obtained somewhere else (e.g. crates.io) but which we needed to fork for some Roc-specific reason. See [README.md](./vendor/README.md) for more information.

## Roc WASI Libc `wasi_libc_sys`

Provides a Rust wrapper for the WebAssembly test platform built on libc and is primarily used for testing purposes.

