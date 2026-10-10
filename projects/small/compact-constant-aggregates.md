# Compact Constant Aggregates on the Restored-Constant Path

## Status

Most of this project's original scope has landed, partly through other
work and partly here:

- Fully constant lists are compile-time roots frozen into static data.
  Scalar and fixed-product lists are packed byte literals
  (`bytes_literal`) that never create per-element LIR, in runtime code or
  in static initializers.
- A build that continues from compile-time evaluation (every cold build)
  re-encodes the evaluation's frozen image for the runtime program. Its
  runtime program has no per-element locals or statements for any
  constant list: lists of lists, of heap strings, and of records holding
  strings cost the same LIR at 100 elements as at 5,000
  (`src/compile/test/compile_time_value_paths_test.zig`, "a constant
  aggregate costs the continued runtime program the same at any element
  count").
- A literal-count `List.repeat` of a runtime value is one builtin call; no
  lowering path unrolls it (same test).
- Three steps of the cold path were quadratic in a constant list's pointer
  count and are now linear: relocation lookup during transcoding
  (`frozen_root_transcode.zig`), identical-node merging
  (`static_data.mergeIdenticalNodes`), and data-name resolution when
  linking the evaluation image (`backend/dev/HostSplice.zig`). A 100,000
  heap-string `List.repeat` build went from 20.7 s to 1.1 s, and 100,000
  distinct string literals from 525 s to 57 s (debug compiler; the rest
  is frontend checking).
- Every executor agrees on a program exercising all regimes
  (`test/cli/constant_aggregate_conformance.roc`), and the issue 9898
  repro runs at ten times what a `u16` count holds.

## Remaining problem

A build that restores its compile-time values from a checked module's
`ConstStore` (every build after the first, on a warm cache) lowers a
representation-stable constant list through a static-initializer procedure
(`solved_lir_lower.zig` `staticDataCandidateStep` -> `listStep`). For a list
whose elements are not packable (strings, lists, records or tags holding
them) that procedure has one frame local, one statement, and one span entry
per element, and it runs through every LIR pass (lowering, ARC insertion,
ARC certification, procedure rewrites) before `StaticInitializerMachine`
evaluates it. Measured with a debug compiler: `List.repeat` of a heap
string 1,000,000 times builds in 10.5 s cold and 73 s with 4.4 GB on the
warm path. The cold and warm paths produce the same static data; only the
warm path's cost is O(elements x pipeline stages).

## Decision needed

`design.md` ("Compile-Time Evaluation And Static Storage", and
`src/static_data.zig`'s module doc) states that static data exports are
frozen from closed, construction-only LIR initializer procedures. Removing
the per-element cost means changing that contract, so pick one:

1. Freeze restored `ConstStore` values straight into target static data
   through the target program's `ConstPlan`s, as `frozen_root_transcode`
   already does for an evaluation's frozen image, so a representation-stable
   restored root never becomes an initializer procedure. This needs
   per-slot const plans on the restored path and a `ConstStore` reader in
   place of the transcoder's source-bytes reader.
2. Keep initializer procedures but give them a construct that names a
   list's elements without a local each. This touches every LIR pass's
   statement switch for a construct only static initializers use.

Option 1 matches the cold path and keeps one freezing mechanism per source
of values; option 2 keeps the current contract but still costs O(elements)
LIR span entries.

## What success looks like

- On the restored path, the LIR of a program whose constant list of heap
  strings, lists, or string records has 5,000 elements is the same size as
  at 100 elements (extend the test above; its restored-path assertions
  currently cover only packed scalars and runtime repeats).
- A warm build of `List.repeat` of a heap string 1,000,000 times costs no
  more than the cold build.
- `test/cli/constant_aggregate_conformance.roc` produces identical output on
  every executor from a warm cache as well as with `--no-cache`.
