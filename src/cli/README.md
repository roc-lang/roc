# CLI

This directory contains the source code for the `roc` command-line interface (CLI) tool.

The CLI is the main entry point for developers using Roc. Its responsibilities include:

- **Command Parsing**: Parsing commands (e.g., `build`, `run`, `fmt`, `test`) and their options through `cli_args.zig`
- **Compilation Orchestration**: Managing the compilation pipeline and integrating with the Roc compiler
- **Linker Integration**: Using an abstraction for the LLD linker as a library through `linker.zig` for code generation
- **Performance Profiling**: Integration with Tracy profiler for performance analysis
- **Testing**: Built-in execution of top-level `expect`s through the `test` command
- **Shared Memory Management**: Testing utilities for the shared memory system used in IPC
- **Default-App Staging**: Turning a file that runs on the built-in Echo platform—a headerless file with `main!`, or an `app` header that names no platform—into an ordinary app rooted beside a copy of that platform, through `default_app.zig`

The CLI coordinates between the compiler frontend (parsing, type checking) and backend (code generation, linking) to provide a seamless development experience.

## ELF PIE host links

Roc hosts must support relocatable code and data. Native executables always
enable PIE/ASLR, without a platform declaration or opt-out. Static Linux
platforms must supply self-relocating startup; macOS and Windows use their OS
loaders. Shared-library exports remain separate from executable relocation.

`zig build run-test-zig -Dtest-filter="ELF PIE"` exercises prelink, patching
and startup with Zig on `PATH`. Linux runs its native architecture; an x86_64
host also runs AArch64 startup when `qemu-aarch64` is on `PATH`.

## Multiline strings in the REPL

The terminal REPL keeps consecutive multiline string lines in one statement.
A blank line, a following non-string statement, or EOF submits the pending string.
String payloads retain their trailing spaces.

## Counting source lines

`roc loc [main.roc]` counts the root source and its import graph, defaulting to
`./main.roc`. It prints total physical lines and lines excluding whitespace-only
and comment-only lines (including indented `#` comments), grouped by the root
application, package, or platform and by local and HTTPS dependencies. Dependency
rows and the combined total appear only when needed. The final line reports
elapsed time in milliseconds.

The command uses dependency resolution and cached parser import inventories,
without checking types or executing code. Shared files count once. Used package
headers are included, while unimported modules, unused dependencies, non-Roc file
imports, and compiler-owned sources are excluded. `--replace-dep` and the usual
download limits work as for `roc check`; `--no-cache` disables import caching.
