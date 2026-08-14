# Snapshot Tool

This directory contains a tool for snapshot testing the Roc compiler.

Snapshot testing is a method used to verify the behavior of the compiler's different stages. The tool generates "golden snapshot" files, which are baseline outputs that are known to be correct.

During testing, the tool runs the compiler and compares its output against these golden files. If there are any differences, the test fails. This is an effective way to detect regressions and unintended changes in the compiler's behavior across a large number of test cases.

The golden snapshots are committed to the repository and are therefore tracked by Git and checked along with any changes to the codebase.

## Diagnostics

Diagnostic coverage is split between semantic and presentation snapshots:

- The `PROBLEMS` section of ordinary snapshots contains the canonical,
  presentation-independent S-expression form of each `reporting.Report`
  (serialized by `src/reporting/report_sexpr.zig`), so those snapshots change
  only when diagnostic semantics change.
- `type=reporting` snapshots (in `test/snapshots/reporting/`) render the same
  semantic reports through every user-facing renderer: `REPORT` (canonical
  S-expression), `CLI`, `MARKDOWN`, `HTML`, and `LSP`; they are the only place
  where renderer-specific layout and markup are pinned.

See `test/snapshots/README.md` for details.
