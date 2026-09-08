# Reporting

Error reporting, diagnostics, and user feedback for the Roc compiler.

## Overview

The reporting module provides comprehensive error reporting and diagnostic information throughout the compilation process. It ensures that users receive clear, actionable feedback when things go wrong.

## Purpose

This module provides:

- **Error Reporting**: Structured error messages with source locations and context
- **Diagnostics**: Detailed information about compilation issues and warnings
- **Source Location**: Precise tracking of where errors occur in source code
- **User-Friendly Messages**: Clear explanations that help developers fix issues
- **Consistent Formatting**: Uniform error presentation across all compiler stages

The reporting module is used by parse, canonicalize, check, and other stages to provide consistent, helpful feedback to users when compilation fails or produces warnings.


## Rendering architecture

Compiler stages build a `Report` containing a `Document`; they do not format
individual output targets. `renderer.zig` traverses `DocumentElement` through
one walker, `renderElementAs`, for terminal, Markdown, HTML, and language-server
output. Report summaries and primary-source selection use projections of that same
walker. A source-layout hook supplies report framing for top-level secondary
blocks while nested blocks retain their document style.

Each target supplies a style with annotation delimiters, text escaping,
indentation, and hooks for source and structural formatting. Annotation style
tables are exhaustive: terminal styles select fields from `ColorPalette` in
`style.zig`, while Markdown, HTML, and language-server styles declare their
markup in `renderer.zig`. Adding an annotation requires its `semanticName` and
an entry in each target's style table. HTML classes come from `semanticName`;
`../snapshot_tool/snapshot.css` owns their colors, and the snapshot tool tests
check that every annotation has a CSS selector.

`source_region.zig` owns shared source layout. `Layout` iterates numbered source
rows and computes their underline spans without allocating; target hooks add
escaping, gutters, and markup around those computed rows. Report snippets use
`Snippet` to expand leading tabs, dedent source, and transform caret coordinates
together. This keeps the primary and secondary report snippets aligned using
the same calculations. Title uppercasing is shared with the snapshot tool
through the exported `writeShouted` helper.

`report_sexpr.zig` serializes the semantic report structure independently of
presentation layout. Ordinary compiler snapshots store that canonical form in
`PROBLEMS`. The `type=reporting` fixtures in `test/snapshots/reporting/` pin the
canonical report alongside all four rendered forms; they complement the
exhaustive model-variant unit corpus.

## Rendering checks

`parity_test.zig` enumerates the document and annotation variants at compile
time, so extending the model requires extending its fixtures. The suite checks
visible text, source rows and line counts, underline spans, nested annotations,
and exact output pins across all four targets. It also exercises the report
entrypoints and HTML escaping.

Run the reporting suite with `zig build run-test-zig-module-reporting`. Run the
CSS coverage test with `zig build run-test-zig-snapshot-tool -- --test-filter
"snapshot CSS covers"`. Changes to shared rendering should also preserve the
tracked snapshot corpus and terminal diagnostic bytes unless an output change
is intentional.
