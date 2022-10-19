
# Rust `Roc` Workspace Structure

The intent of this document is to capture the structure, intent, and status at a high level for `Roc` code. Its purpose is to guide those interested in learning how `Roc` works. It provides an overview of how `Roc` is put together, and links to crate specific information. It does not provide detailed descriptions of design or implementation details as these are left to documentation comments in the code where most appropriate. 

If you see something that is not up to date, please correct it or follow up in the [Zulip chat](https://roc.zulipchat.com/).

## Roc Libaries

### Roc AST
[roc_ast](../crates/ast/src/lib.rs) - Provides an AST library for Roc and is used by `roc_editor` and (soon) `roc_docs`. In contrast to the compiler, AST types do not keep track of a location in a file. Use ```cargo doc --package roc_ast --open``` to generate docs.

[roc_cli](../crates/cli/src/lib.rs) - Provides the core CLI functionality for the `roc` binary. Use ```cargo doc --package roc_ast --open``` to generate docs.

[cli_utils](../crates/cli_utils/src/lib.rs) - Provides shared code for cli tests and benchmarks. 

## Roc Binaries

[roc](../crates/cli/src/main.rs) - Builds the `roc` binary. Use ```cargo doc --open``` to generate docs.
