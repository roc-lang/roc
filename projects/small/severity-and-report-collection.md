# Severity Classification and Report Collection, Once

## Problem

Two facts about the diagnostics pipeline are re-encoded per consumer
instead of living beside the types that own them:

1. **"Which severities are errors?"** `src/reporting/severity.zig`
   defines `Severity` with only `toString`/`toCode` — no
   `isError()`/`blocksCompilation()` — so the partition
   ("`runtime_error` and `fatal` set the exit code / count as errors;
   `warning` and `info` do not") is re-spelled at ~8 sites:
   `src/cli/CliCtx.zig:207-211`, four spots in `src/cli/main.zig`
   (`:4925, 6162, 13426, 13497`),
   `src/playground_wasm/main.zig:1026-1028, 2156-2162`,
   `src/lsp/syntax.zig:560-561`, and
   `src/snapshot_tool/main.zig:2863` — the last as an `else =>
   error_count += 1` that would silently misclassify any new severity
   variant. The Severity→LSP-number collapse is likewise written
   twice (`src/lsp/syntax.zig:791-794` and
   `src/playground_wasm/main.zig:2184-2188`, plus a hardcoded
   `.severity = 1` at `syntax.zig:427`).
2. **The phase-report gathering loop.** The sequence "tokenize
   diagnostics → `tokenizeDiagnosticToReport`; parse diagnostics →
   `parseDiagnosticToReport`; CIR diagnostics → `diagnosticToReport`;
   flush pending static exhaustiveness; type problems →
   `ReportBuilder`" is the compiler's real orchestration in
   `src/compile` (`compile_build.zig:1520-1524`,
   `compile_package.zig:1437-1470`, `coordinator.zig:4294-4298`) —
   and is independently re-implemented by the snapshot tool
   (`src/snapshot_tool/main.zig:249-311`, `generateAllReports`) and
   the playground (`src/playground_wasm/main.zig:1306-1459`). A new
   diagnostic phase, or a change to the exhaustiveness-flush
   ordering, must be mirrored into both or their output silently
   drifts from real `roc check` — the exact tool whose EXPECTED
   sections are supposed to certify compiler output.

## Background

The per-phase report constructors themselves are single-sourced
(`src/parse/AST.zig`'s `tokenizeDiagnosticToReport`/
`parseDiagnosticToReport` are the only renderer of parse diagnostics,
called by every consumer) — the duplication is one level up, in the
classification of severities and the driving of the phases. Both are
the cheap kind of fix: add the missing method next to the type; hoist
the loop into the module that already owns its siblings.

## Evidence

- `grep -n 'runtime_error' src/cli src/lsp src/playground_wasm
  src/snapshot_tool -r` — the eight partition sites.
- `src/snapshot_tool/main.zig:2863` — the `else` prong.
- The three `src/compile` orchestration sites versus
  `snapshot_tool/main.zig:249` and `playground_wasm/main.zig:1306` —
  the same phase sequence, three spellings.

## Solution design

1. Add to `severity.zig`: `pub fn isError(self) bool` (exhaustive
   switch, no `else`) and `pub fn toLspSeverity(self) u8`. Replace all
   ten call sites; delete the snapshot tool's `else` prong so a new
   severity variant becomes a compile error at every consumer.
2. Extract `collectAllReports(...)![]Report` (or an iterator, to
   avoid materializing) into `src/compile`, built from the existing
   coordinator logic — including the `flushPendingStaticExhaustiveness`
   call and its ordering. The snapshot tool and playground call it;
   their local sequences are DELETED. Per-consumer differences
   (playground's incremental stages, snapshot's section grouping)
   stay outside the shared function as consumers of its output.

## What success looks like

Every criterion below must hold; the project is not done until all do:

- `grep -rn '.runtime_error, .fatal\|.fatal, .runtime_error' src/`
  matches only `severity.zig`.
- Adding a `Severity` variant fails compilation at `isError`/
  `toLspSeverity` and nowhere else.
- The snapshot tool and playground contain no phase-sequence loop;
  both call the shared collector.
- Snapshot corpus unchanged; playground diagnostics for a fixture set
  byte-identical to before.

## How to evaluate the result

### Correctness ideal

Snapshot EXPECTED sections and playground output certify the actual
compiler pipeline by construction, because they run it — not a copy
of it. Severity policy changes are one-line edits.

### Performance ideal

Neutral: the shared collector is the same loop the consumers had.
If the iterator form is chosen, playground memory use may improve
slightly; nothing may regress (compare snapshot-generation wall time
over the corpus).

## Tests to add

- A parity test compiling one fixture through `roc check`'s
  collection path and through the snapshot tool, asserting identical
  report sequences (titles + regions).
- Comptime-exhaustiveness is the test for severity (no runtime test
  needed beyond one table case per variant).

## Related projects

- [../big/one-report-renderer.md](../big/one-report-renderer.md) —
  the rendering half of the same pipeline; independent but adjacent.
- [lsp-and-docs-truth-reuse.md](lsp-and-docs-truth-reuse.md) — the
  editor-side consumers of the same classification.
