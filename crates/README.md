
# Roc Internals

Roc has different rust crates for various binaries and libraries. These are described below to help give you an overview. If you see something here that is out of date, please correct it or discuss it in the [Zulip chat](https://roc.zulipchat.com/).

Use `cargo doc` to generate docs; e.g. ```cargo doc --package roc_ast --open```.

## `ast/` - `roc_ast`

Code to represent the [Abstract Syntax Tree](https://en.wikipedia.org/wiki/Abstract_syntax_tree) as used by the editor.
In contrast to the compiler, the types in this AST do not keep track of the location of the matching code in the source file.


## `cli/` - `roc_cli`

The `roc` binary that brings together all functionality in the Roc toolset.

## `cli_utils/` - `cli_utils`

Provides shared code for cli tests and benchmarks.

## `code_markup/` - `roc_code_markup`

Roc's very own [markup language](https://en.wikipedia.org/wiki/Markup_language). This library is used by the editor to display good-looking roc code. 

## `compile/`

Compiles `.roc` files and combine them with their platform into an executable binary. See [compiler/README.md](./compiler/README.md) for more information.

TODO explain what "compiler frontend" is
TODO explain what "compiler backend" is

The compiler includes the following sub-crates;
- `roc_alias_analysis` TODO - Need assistance
- `arena-pool` TODO - Need assistance
- `roc_build` TODO - Need assistance
- `roc_builtins` provdes the Roc functions and modules that are implicitly imported into every module. See [README.md](./compiler/builtins/README.md) for more information.
- `roc_can` TODO - Need assistance
- `roc_collections` TODO - Need assistance
- `roc_constrain` TODO - Need assistance
- `roc_debug_flags` TODO - Need assistance
- `roc_derive` provides auto-derivers?
- `roc_exhaustive` provides exhaustiveness checking for Roc 
- `roc_fmt` TODO - Need assistance
- `roc_gen_dev` provides the compiler backend to generate Roc binaries fast, for a nice developer experience. See [README.md](./compiler/gen_dev/README.md) for more information.
- `roc_gen_llvm` provides the LLVM backend to generate Roc binaries. Used to generate a binary with the fastest possible execution speed.
- `roc_gen_wasm` provides the WASM backend to generate Roc binaries. See [README.md](./compiler/gen_wasm/README.md) for more information.
- `roc_ident` provides identifiers
- `roc_intern` provides generic interners for concurrent and single-thread use cases.
- `roc_late_solve` provides type unification and solving primitives from the perspective of the compiler backend.
- `roc_load` used to load a `.roc` file and typecheck it.
- `roc_load_internal` TODO - Need assistance
- `roc_module` TODO - Need assistance
- `roc_mono` TODO - Need assistance
- `roc_parse` TODO - Need assistance
- `roc_problem` provides types to describe problems that can occur when compiling `.roc` code.
- `roc_region` TODO - Need assistance
- `roc_target` provides types and helpers for compiler targets such as `default_x86_64`.
- `roc_serialize` provides helpers for serializing and deserializing to/from bytes.
- `roc_solve` see [Ambient Lambda Set Specialization](./compiler/solve/docs/ambient_lambda_set_specialization.md) for more information on how polymorphic lambda sets are specialized and resolved in the compiler's type solver. 
- `roc_solve_problem` provides types to describe problems that can occur during solving.
- `roc_str` provides `Roc` styled collection reference counting. See [README.md](./compiler/str/README.md) for more information.
- `test_derive` TODO - Need assistance
- `test_gen` contains all of Roc's code generation tests. See [README.md](./compiler/test_gen/README.md) for more information.
- `test_mono` TODO - Need assistance
- `test_mono_macros` TODO - Need assistance
- `roc_types` TODO - Need assistance
- `roc_unify` TODO - Need assistance

## `docs/` - `roc_docs`

Generates html documentation from Roc files.
Used for [roc-lang.org/builtins/Num](https://www.roc-lang.org/builtins/Num).

## `docs_cli/` - `roc_docs_cli` library and `roc-docs` binary

Provides a binary that is only used for static build servers.

## `editor/` - `roc_editor`

For editing Roc files (Work In Progress). See [README.md](./editor/README.md) for more information.

## `error_macros/` - `roc_error_macros`

Provides macros for consistent reporting of errors in Roc's rust code.

## `glue/` - `roc_glue`

The `roc_glue` crate generates rust code to connect a Roc app with a rust platform. This tool is not necessary for writing a platform in another language, however, it's an added convenience for rust platform devs.

## `highlight/` - `roc_highlight`

Provides syntax highlighting for the editor by transforming a string to markup nodes.

## `linker/` - `roc_linker`

Surgical linker that links platforms to Roc applications.
We created our own linker for speed and because regular linkers add a lot of problems and complexity. Because we want roc to manage the build system and final linking of the executable, it is significantly less practical to use a regular linker. This can be seen in how many arbitrary libraries we link with the legacy linker because we have no idea what libraries the application is actually using.
See [README.md](./linker/README.md) for more information.

## `repl_cli/` - `roc_repl_cli`

Command Line Interface(CLI) functionality for the Read-Evaluate-Print-Loop (REPL).

## `repl_eval/` - `roc_repl_eval`

Provides the functionality for the REPL to evaluate Roc expressions.

## `repl_expect/` - `roc_repl_expect`

TODO - Need assistance

## `repl_test/` - `repl_test`

TODO - Need assistance

## `repl_wasm/` - `roc_repl_wasm`

Provides a build of the REPL for the Roc website using WebAssembly. See [README.md](./repl_wasm/README.md) for more information.

## `reporting/` - `roc_reporting`

TODO - Need assistance

## `roc_std/` - `roc_std`

Provides Rust representations of Roc data structures.

## `test_utils/` - `roc_test_utils`

Provides testing utility functions for use throughout the Rust code base.

## `tracing/` - `roc_tracing`

Provides tracing utility functions for various executable entry points.

## `utils/` - `roc_utils`

Provides utility functions used all over the code base.

## `vendor/`

These are files that were originally obtained somewhere else (e.g. crates.io) but which we needed to fork for some Roc-specific reason. See [README.md](./vendor/README.md) for more information.

## `wasi-libc-sys/` - `wasi_libc_sys`

Provides a Rust wrapper for the WebAssembly test platform built on libc and is primarily used for testing purposes.
