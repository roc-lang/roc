# Snapshots

Snapshot tests that validate compiler behavior by capturing the output of each compilation stage for specific Roc code examples.

Snapshot tests provide comprehensive validation of the compilation pipeline by showing how source code is transformed through each stage: tokenization, parsing, canonicalization, and type checking etc.

Each snapshot file contains the expected output and helps us to detect regressions when compiler behavior changes unexpectedly.

## Semantic diagnostics vs renderer output

Diagnostics are covered by two different kinds of snapshot, split so that
semantic changes and presentation changes never show up in the same files:

- **Ordinary snapshots** (`type=file`, `snippet`, `expr`, ...) capture diagnostic
  *semantics*. Their `PROBLEMS` section contains the canonical S-expression
  serialization of each `reporting.Report` (see
  `src/reporting/report_sexpr.zig`): severity, title, source regions, and the
  full document structure (text, annotations, source excerpts, underlines).
  It contains no renderer-specific details: no box-drawing characters, ANSI
  escapes, wrapping, or markup. `NIL` means the compile produced no reports.
  These snapshots answer: *did the compiler produce the correct diagnostic?*

- **Reporting snapshots** (`type=reporting`, in `reporting/`) pin renderer
  *output*. Each one compiles its `SOURCE` normally, then renders the same
  semantic reports through every user-facing format, one section per renderer:
  `REPORT` (the canonical S-expression), `CLI` (plain-text box layout),
  `MARKDOWN`, `HTML`, and `LSP`. Layout, wrapping, punctuation, and markup are
  pinned here and only here.

A renderer-only change must only affect files in `reporting/`; a change to
diagnostic semantics shows up in ordinary snapshots (and possibly in
`reporting/` too). Note that snapshot post-processing globally rewrites the
removed header keyword to `mod`, which also applies inside the S-expression
output.

## Usage

- **Generate all snapshots**: `zig build run-snapshot-tool`
- **Update specific snapshot**: `zig build run-snapshot-tool -- <file_path>`
- **Update expected from problems**: `zig build run-snapshot-tool -- <file_path> --update-expected`
- **Embed carriage-return source bytes**: add `source_escapes=true` to `META` and write each carriage return as `\r` in `SOURCE`
- **Debug REPL evaluation with trace**: `zig build run-snapshot-tool -- <repl_snapshot.md> --trace-eval`

### Trace Debugging

The `--trace-eval` flag enables detailed interpreter tracing for debugging REPL snapshots:

```bash
# Debug build (trace support enabled by default)
zig build run-snapshot-tool -- src/snapshots/repl/repl_record_field_access.md --trace-eval
```

**Requirements:**
- Only works with REPL snapshots (`type=repl`)
- Can only be used with a single snapshot file
- Trace output is automatically enabled in debug builds
- For release builds, use `-Dtrace-eval=true` to enable tracing
