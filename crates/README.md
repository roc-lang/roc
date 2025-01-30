# Roc Internals

Roc has different rust crates for various binaries and libraries. Their roles are briefly described below. If you'd like to learn more, have any questions, or suspect something is out of date, please start a discussion on the [Roc Zulip](https://roc.zulipchat.com/)!

You can use `cargo doc` to generate docs for a specific package; e.g.

```
cargo doc --package roc_ast --open
```

## `cli/` - `roc_cli`

The `roc` binary that brings together all functionality in the Roc toolset.

## `cli_test_utils/` - `cli_test_utils`

Provides shared code for cli tests, cli benchmarks, glue tests, valgrind crate.

## `compiler/`

Compiles `.roc` files and combines them with their platform into an executable binary. See [compiler/README.md](./compiler/README.md) for more information.

TODO explain what "compiler frontend" is
TODO explain what "compiler backend" is

The compiler includes the following sub-crates;
- `roc_alias_analysis` Performs analysis and optimizations to remove unneeded [reference counts](https://en.wikipedia.org/wiki/Reference_counting) at runtime, and supports in-place mutation.
- `arena_pool` An implementation of an [arena allocator](https://mgravell.github.io/Pipelines.Sockets.Unofficial/docs/arenas.html) designed for the compiler's workloads.
- `roc_build` Responsible for coordinating building and linking of a Roc app with its host.
- `roc_builtins` provides the Roc functions and modules that are implicitly imported into every module. See [README.md](./compiler/builtins/README.md) for more information.
- `roc_can` [Canonicalize](https://en.wikipedia.org/wiki/Canonicalization) a roc [abstract syntax tree](https://en.wikipedia.org/wiki/Abstract_syntax_tree), [resolving symbols](https://stackoverflow.com/a/1175493/4200103), [re-ordering definitions](https://www.oreilly.com/library/view/c-high-performance/9781787120952/546b5677-9157-4333-bc90-16db696436ac.xhtml), and preparing a module for [type inference](https://en.wikipedia.org/wiki/Type_inference).
- `roc_collections` Domain-specific collections created for the needs of the compiler.
- `roc_constrain` Responsible for building the set of constraints that are used during [type inference](https://en.wikipedia.org/wiki/Type_inference) of a program, and for gathering context needed for pleasant error messages when a type error occurs.
- `roc_debug_flags` Environment variables that can be toggled to aid debugging of the compiler itself.
- `roc_derive` provides auto-derivers for builtin abilities like `Hash` and `Decode`.
- `roc_exhaustive` provides [exhaustiveness](https://dev.to/babak/exhaustive-type-checking-with-typescript-4l3f) checking for Roc.
- `roc_fmt` The roc code formatter.
- `roc_gen_dev` provides the compiler backend to generate Roc binaries fast, for a nice developer experience. See [README.md](./compiler/gen_dev/README.md) for more information.
- `roc_gen_llvm` provides the LLVM backend to generate Roc binaries. Used to generate a binary with the fastest possible execution speed.
- `roc_gen_wasm` provides the WASM backend to generate Roc binaries. See [README.md](./compiler/gen_wasm/README.md) for more information.
- `roc_ident` Implements data structures used for efficiently representing small strings, like identifiers.
- `roc_intern` provides generic interners for concurrent and single-thread use cases.
- `roc_late_solve` provides type unification and solving primitives from the perspective of the compiler backend.
- `roc_load` Used to load a .roc file and coordinate the compiler pipeline, including parsing, type checking, and [code generation](https://en.wikipedia.org/wiki/Code_generation_(compiler)).
- `roc_load_internal` The internal implementation of roc_load, separate from roc_load to support caching.
- `roc_module` Implements data structures used for efficiently representing unique modules and identifiers in Roc programs.
- `roc_mono` Roc's main intermediate representation (IR), which is responsible for [monomorphization](https://en.wikipedia.org/wiki/Monomorphization), defunctionalization, inserting [ref-count](https://en.wikipedia.org/wiki/Reference_counting) instructions, and transforming a Roc program into a form that is easy to consume by a backend.
- `roc_parse` Implements the Roc parser, which transforms a textual representation of a Roc program to an [abstract syntax tree](https://en.wikipedia.org/wiki/Abstract_syntax_tree).
- `roc_problem` provides types to describe problems that can occur when compiling `.roc` code.
- `roc_region` Data structures for storing source-code-location information, used heavily for contextual error messages.
- `roc_target` provides types and helpers for compiler targets such as `default_x86_64`.
- `roc_serialize` provides helpers for serializing and deserializing to/from bytes.
- `roc_solve` The entry point of Roc's [type inference](https://en.wikipedia.org/wiki/Type_inference) system. Implements type inference and specialization of abilities.
- `roc_solve_problem` provides types to describe problems that can occur during solving.
- `test_derive` Tests Roc's auto-derivers.
- `test_gen` contains all of Roc's [code generation](https://en.wikipedia.org/wiki/Code_generation_(compiler)) tests. See [README.md](./compiler/test_gen/README.md) for more information.
- `test_mono` Tests Roc's generation of the mono intermediate representation.
- `test_mono_macros` Macros for use in `test_mono`.
- `roc_types` Various representations and utilities for dealing with types in the Roc compiler.
- `roc_unify` Implements Roc's unification algorithm, the heartstone of Roc's [type inference](https://en.wikipedia.org/wiki/Type_inference).

## `docs/` - `roc_docs`

Generates html documentation from Roc files.
Used for [roc-lang.org/builtins/Num](https://www.roc-lang.org/builtins/Num).

## `docs_cli/` - `roc_docs_cli` library and `roc-docs` binary

Provides a binary that is only used for static build servers.

## `error_macros/` - `roc_error_macros`

Provides macros for consistent reporting of errors in Roc's rust code.

## `glue/` - `roc_glue`

The `roc_glue` crate generates code needed for platform hosts to communicate with Roc apps. This tool is not necessary for writing a platform in another language, however, it's a great convenience! Currently supports Rust platforms, and the plan is to support any language via a plugin model.

## `highlight/` - `roc_highlight`

Provides syntax highlighting for the static site gen platform which is used by the tutorial.

## `linker/` - `roc_linker`

Surgical linker that links platforms to Roc applications. We created our own linker for performance, since regular linkers add complexity that is not needed for linking Roc apps. Because we want `roc` to manage the build system and final linking of the executable, it is significantly less practical to use a regular linker. See [README.md](./linker/README.md) for more information.

## `repl_cli/` - `roc_repl_cli`

Command Line Interface (CLI) functionality for the Read-Evaluate-Print-Loop (REPL).

## `repl_eval/` - `roc_repl_eval`

Provides the functionality for the REPL to evaluate Roc expressions.

## `repl_state/` - `roc_repl_state`

Implements the state machine the to handle user input for the REPL (CLI and web)
If the user enters an expression, like `x * 2`, check it evaluate it.
If the user enters a declaration, like `x = 123`, check it and remember it, but don't evaluate.

## `repl_expect/` - `roc_repl_expect`

Supports evaluating `expect` and printing contextual information when they fail.

## `repl_test/` - `repl_test`

Tests the roc REPL.

## `repl_wasm/` - `roc_repl_wasm`

Provides a build of the REPL for the Roc website using WebAssembly. See [README.md](./repl_wasm/README.md) for more information.

## `reporting/` - `roc_reporting`

Responsible for generating warning and error messages.

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

# Building a Roc Application

Below is a simplified diagram to illustrate how a Roc application and host are combined to build an executable file. 

![Building a Roc Application using Rust](./building_a_roc_application.svg)

# Roc Compiler Stages

Below is a simplified diagram to illustrate the different stages of the Roc Compiler.

![Roc Compiler Stages](./roc_compiler_stages.svg)
