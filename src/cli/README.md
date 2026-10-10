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

## REPL formatters

`roc repl --formatter /path/to/formatter.roc` loads a Roc module exposing:

```roc
module [decode, encode]

decode : Str -> Try(Str, Str)
decode = |line| Ok(line)

encode : { result : Str, stdout : Str, diagnostics : Str } -> Str
encode = |response| Json.to_str(response)
```

The formatter is compiled once and runs separately from the user's definitions.
Its sibling imports resolve relative to the formatter file. Input is UTF-8 text,
one request per line (LF or CRLF); EOF ends the process. The formatter decodes
any multiline cell representation into source. `Ok(source)` evaluates that
source; `Err(reply)` sends a response without evaluating anything. `encode`
receives the last expression's inspected value, captured `dbg` output, and plain
compiler diagnostics. Empty strings mean there is no value, output, or diagnostic.
The compiler appends one newline to each reply; formatter replies themselves
must not contain literal newlines. Formatter mode accepts `:t <identifier>`
between statements, including after definitions in the same cell, and returns
the checked type as plain result text. It does not evaluate the binding. There
are no prompts, banners, or terminal lifecycle commands in formatter mode.

Accepted definitions persist across requests. Within a cell, statements run in
order and stop at the first diagnostic; previously accepted definitions remain.
Invalid formatter signatures or formatter crashes terminate the process with a
diagnostic on stderr. User-code diagnostics are encoded replies and leave the
session usable. Formatter calls use the LIR interpreter; `--specialize` selects
the normal lowering strategy. The protocol is independent of JSON and Jupyter.

The terminal REPL keeps consecutive multiline string lines in one statement.
A blank line, a following non-string statement, or EOF submits the pending string.
Formatter cells are complete requests, so a multiline string at the end of a cell
is submitted immediately. String payloads retain their trailing spaces.
