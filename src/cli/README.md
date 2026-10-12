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

`roc repl --formatter /path/to/formatter.roc` loads a Roc module with this API:

```roc
module [decode, encode]

Request : [Evaluate(Str), Reply(Str)]
Response : {
    status : [Success, Incomplete, Failure],
    result : Str,
    stdout : Str,
    diagnostics : Str,
    value : [Other, Tag({ name : Str, payload : Try(Str, {}) })],
}

decode : Str -> Request
decode = |line| Evaluate(line)

encode : Str, Response -> Str
encode = |_input, response| Json.to_str({
    status: Str.inspect(response.status),
    result: response.result,
    stdout: response.stdout,
    diagnostics: response.diagnostics,
})
```

The formatter is compiled once and runs separately from the user's definitions.
Its sibling imports resolve relative to the formatter file. Each UTF-8 input
line is one request. Only its LF delimiter is removed; all other bytes, including
spaces and CR in CRLF input, are passed unchanged to `decode` and later `encode`.
EOF submits any remaining nonempty line, then ends the process.

`Evaluate(source)` evaluates the decoded source, which may contain multiple
lines. `Reply(reply)` sends a reply immediately, without evaluation or `encode`.
Use it for successful protocol requests as well as protocol errors. For an
`Evaluate`, `encode(input, response)` receives the original wire input, not the
decoded source. It can reparse request IDs and display preferences without
persistent formatter state or compiler-owned protocol fields.

The compiler appends LF to each reply; replies must not contain literal LF.
There are no prompts, banners, streaming output, or terminal lifecycle commands
in formatter mode. JSON and Jupyter conventions belong to plugins, not the
compiler.

Top-level statements run in source order. `Success` means all statements
completed, including empty source. `Incomplete` means the parser needs more
source. `Failure` means another user-code failure, such as a parse/type
error, failed `expect`, language crash, or unsuccessful type query. Status comes
from structured compiler outcomes, never from diagnostic text.

Processing stops at the first failure or incomplete statement. Definitions and
annotations accepted earlier remain available; a rejected definition is rolled
back individually, and subsequent statements are skipped. Incomplete text is
not buffered across requests. Send a new complete request to finish it; earlier
accepted definitions remain in scope.

`result` and `value` come from the last successful expression before processing
stopped. Later definitions and successful `expect`, `for`, or `while` statements
leave them unchanged. A standalone `dbg` evaluates to `{}`, replacing the result, and
also writes its debug text to `stdout`.
With no successful expression they are `""` and `Other`. A successful
`:t <identifier>` between statements replaces `result` with the checked type
and `value` with `Other`, without evaluating the binding. `stdout` accumulates
`dbg` output; `diagnostics` is uncolored diagnostic text. Treat status as the
execution outcome independently of either string being empty.

User-code failures produce replies and leave the process usable. Invalid
formatter modules or signatures, crashes in `decode`/`encode`, replies containing
LF, unsupported host effects, and operational failures such as allocation or
transport errors terminate the process with stderr diagnostics. No reply is
guaranteed for the interrupted request. EOF normally terminates successfully.
Formatter calls use the LIR interpreter; `--specialize` selects the normal
lowering strategy.

`value` describes the last expression's actual top-level value: `Other`, or
`Tag({ name, payload })`. The payload is `Ok(text)` for exactly one Str argument,
and `Err({})` for other types or arities. Nominal wrappers and nested tags are
not unwrapped. This information comes from the checked constant store; inspection
strings are never parsed to infer types. The expression runs once. Plugins decide
which tag names and payloads represent rich output; the compiler has no MIME rules.
A plugin can represent a rich display bundle with a tag carrying one serialized
string; this needs no recursive reflection or compiler changes.

The terminal REPL keeps consecutive multiline string lines in one statement.
A blank line, a following non-string statement, or EOF submits the pending string.
Formatter cells are complete requests, so a multiline string at the end of a cell
is submitted immediately. String payloads retain their trailing spaces.
