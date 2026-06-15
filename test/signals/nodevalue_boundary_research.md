# Signals NodeValue Boundary Research

This report summarizes the first boundary-focused benchmark pass for the
signals platform. It measures the current 32-byte `NodeValue` boundary as the
control case and compares three synthetic host-side prototype shapes.

## Commands

```sh
zig build run-test-signals -Dplatform=signals
zig build run-signals-bench -Dplatform=signals
./zig-out/bin/signals-demo --bench --bench-iterations 5000
```

The 5,000-iteration run below used the existing `signals-demo` binary after
`zig build run-signals-bench -Dplatform=signals`. The Roc app is built with
`--opt=speed --debug`; the test host is built with the build's default Zig
optimization mode. Treat the timings as directional until repeated ReleaseFast
host runs are added.

## Prototype Matrix

| Prototype | API impact | ABI impact | Ownership model | Callback shape | Equality strategy | Dynamic/list support |
| --- | --- | --- | --- | --- | --- | --- |
| A. Baseline `NodeValue` | Current typed Roc API | Current 32-byte tagged union | Host stores owned `NodeValue` copies | Boxed callbacks receive `NodeValue` | Recursive `NodeValue` equality | Supported today |
| B. Scalar fast paths | Roc API can stay typed for common scalars | Adds typed primitive node/callback paths | Scalars copied directly; complex values stay `NodeValue` | Primitive callbacks use scalar args/results | Direct scalar equality | `Elem.dynamic` and `Elem.each` still use `NodeValue` |
| C. Host value handles | May require handles or borrowed views at dynamic boundaries | Replaces hot values with host-owned handles/views | Host owns storage; Roc receives handles/views | Callbacks receive handles or borrowed views | Explicit value/view equality, not raw handle identity | Needs explicit view lifetime rules |
| D. Generated boundary shims | Roc API can stay typed with generated glue | Adds per-type encode/decode/equality/callback glue | Generated code owns per-type copies/borrows | Callbacks receive app-model shapes | Generated per-type equality | Needs generated item/render glue |

## Synthetic Measurements

Elapsed time is total nanoseconds for 5,000 iterations. Speedup is relative to
A for the same case.

| Case | A baseline | B scalar | C handles/views | D generated |
| --- | ---: | ---: | ---: | ---: |
| scalar boundary | 157,916 ns | 26,292 ns, 6.0x | 50,250 ns, 3.1x | 31,667 ns, 5.0x |
| list/record boundary | 26,814,000 ns | 22,669,250 ns, 1.2x | 107,209 ns, 250x | 33,250 ns, 806x |
| equality | 724,750 ns | 751,958 ns, 1.0x | 326,041 ns, 2.2x | 300,084 ns, 2.4x |
| callback shape | 106,791 ns | 23,833 ns, 4.5x | 66,000 ns, 1.6x | 40,666 ns, 2.6x |
| deep map chain | 2,738,875 ns | 590,791 ns, 4.6x | 973,292 ns, 2.8x | 1,145,875 ns, 2.4x |
| wide fanout | 10,901,209 ns | 2,560,208 ns, 4.3x | 4,372,375 ns, 2.5x | 2,742,417 ns, 4.0x |

Important interpretation details:

- B intentionally does not improve list/record paths. Those still use
  `NodeValue`, so the list/record row stays near baseline.
- C and D list/record rows are lower-bound synthetic numbers because they do
  not yet pay for real generated Roc glue at `Elem.dynamic` or `Elem.each`.
- C must not use raw handle identity for unchanged-value suppression. The
  benchmark row uses value/view equality. A handle design needs explicit
  versioning or immutable value handles to be correct.

## Current-App Baseline Scenarios

These rows run the existing compiled demo, so they are only A/Baseline control
measurements.

| Scenario | Time | Key counters |
| --- | ---: | --- |
| counter updates, 5,000 clicks | 316,814,917 ns | 95,000 allocs, 60,000 callbacks, 85,000 evaluated nodes |
| diamond updates, 5,000 clicks | 89,465,666 ns | 0 allocs, 80,000 callbacks, 100,000 evaluated nodes |
| event merge, 5,000 clicks | 72,289,375 ns | 0 allocs, 25,000 callbacks, 25,000 evaluated nodes |
| dynamic toggle, 5,000 clicks | 643,723,917 ns | 10,000 events, 25,000 callbacks, 10,000 text updates |
| keyed reorder/remove, 10,000 clicks | 24,142,952,250 ns | 294,994 allocs, 249,997 deallocs, retained allocation delta +44,997 |

The keyed churn row is the most serious non-boundary finding. Repeated
mount/unmount deactivates scopes and nodes, but the host keeps inactive graph
and DOM storage until host teardown. It also retains Roc allocations across the
scenario. This should be fixed before using keyed-list benchmark results to
choose a boundary representation, because retained callbacks/nodes can dominate
UI-scale behavior.

## Findings

1. The current `NodeValue` boundary is expensive on scalar hot paths.

   In synthetic scalar propagation, eliminating generic tag/decode/equality
   work gives a 4x to 6x improvement. The wide-fanout baseline performs
   645,000 synthetic encodes, 1,280,000 decodes, and 640,000 equality checks
   for 640,000 label operations; the scalar path removes that boundary work
   and is 4.3x faster.

2. Representation and allocation are separate costs.

   Primitive rows are dominated by tag/decode/equality and callback shape.
   List/record rows are dominated by allocation and recursive traversal.
   B helps the primitive rows but not list/record rows; C and D help
   list/record rows only because they change the representation shape.

3. Boxed callback crossing is visible even without list allocation.

   The synthetic callback row improves from 106,791 ns to 23,833 ns with a
   scalar call shape and to 40,666 ns with generated typed shims. That supports
   the hypothesis that avoiding `NodeValue` at callbacks matters independently
   from scheduling.

4. Host handles are promising but have the highest correctness risk.

   Handles/views are good for list and record data because they avoid copying
   and allocation, but equality, lifetime, and dynamic render semantics must be
   explicit. A raw mutable-slot handle is not enough: unchanged-value
   suppression needs either immutable value handles, versioned handles, or
   value/view equality.

5. Generated shims are the strongest general boundary candidate so far.

   D is near scalar-fast-path performance for primitive rows and the best
   synthetic result for record/list rows. It preserves typed Roc APIs better
   than C, but it requires real glue generation for app model types, list item
   types, equality, and dynamic render callbacks before the current app can be
   measured end to end.

6. Current keyed list churn has a cleanup/retention issue.

   The retained allocation delta of +44,997 after 10,000 keyed clicks means
   benchmark results for dynamic/keyed UI scale are currently confounded by
   retained host state. This is not a boundary representation issue, but it is
   a production UI scale issue.

## Recommendation

Do not rewrite the runtime around handles yet.

The next implementation step should be a narrow hybrid prototype:

1. Fix scoped cleanup for inactive keyed/dynamic graph nodes and boxed
   callbacks, then rerun the current-app scenario rows.
2. Add real scalar fast paths for `I64`, `Bool`, and `Str` signal/callback
   nodes while preserving the current typed Roc API and generic `NodeValue`
   fallback for dynamic/list boundaries.
3. Add one generated-shim spike for concrete app model types used here:
   `Counter`, `App`, `Item`, and `List(Item)`.
4. Rerun the same matrix in a ReleaseFast host build with at least three
   samples per row.

If B plus the cleanup fix materially improves current-app scenarios, keep B as
the first production migration. If D also improves list/keyed scenarios after
real generated glue exists, use D for app-model and list-item boundaries while
keeping generic `NodeValue` only for heterogeneous dynamic APIs.
