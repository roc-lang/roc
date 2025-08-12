# CLI

This directory contains the source code for the `roc` command-line interface (CLI) tool.

The CLI is the main entry point for developers using Roc. Its responsibilities include:

- **Command Parsing**: Parsing commands (e.g., `build`, `run`, `fmt`, `test`) and their options through `cli_args.zig`
- **Compilation Orchestration**: Managing the compilation pipeline and integrating with the Roc compiler
- **Linker Integration**: Using an abstraction for the LLD linker as a library through `linker.zig` for code generation
- **Performance Profiling**: Integration with Tracy profiler for performance analysis
- **Testing & Benchmarking**: Built-in testing and benchmarking capabilities through `bench.zig`
- **Shared Memory Management**: Testing utilities for the shared memory system used in IPC

The CLI coordinates between the compiler frontend (parsing, type checking) and backend (code generation, linking) to provide a seamless development experience.
