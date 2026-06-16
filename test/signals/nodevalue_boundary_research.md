# Signals NodeValue Boundary Research

This report summarizes the boundary-focused prototype pass for the signals
platform. The control remains the generic 32-byte `NodeValue` representation,
but the prototype now includes scoped cleanup, scalar host paths, generated
refcount helpers from `ZigGlue.roc`, and one generated boundary payload spike.

## Commands

```sh
./zig-out/bin/roc check src/glue/src/ZigGlue.roc
./zig-out/bin/roc glue src/glue/src/ZigGlue.roc test/signals/platform test/signals/platform/main.roc
zig build run-test-cli -- --suite glue
zig build run-test-signals -Dplatform=signals
zig build run-signals-bench -Dplatform=signals
zig build build-test-hosts -Dplatform=signals -Doptimize=ReleaseFast
./zig-out/bin/roc build --opt=speed --debug --no-cache --output=zig-out/bin/signals-demo test/signals/app.roc
./zig-out/bin/signals-demo --bench --bench-iterations 5000 --bench-samples 3
```

The final matrix below is a ReleaseFast host build with three 5,000-iteration
samples. Values are median elapsed time unless noted.

## Prototype Matrix

| Prototype | API impact | ABI impact | Ownership model | Callback shape | Equality strategy | Dynamic/list support |
| --- | --- | --- | --- | --- | --- | --- |
| A. Baseline `NodeValue` | Current typed Roc API | 32-byte tagged union | Host stores owned `NodeValue` copies | Boxed callbacks receive `NodeValue` | Recursive generated/host equality | Supported |
| B. Scalar fast paths | Adds explicit scalar API helpers for hot paths | Typed primitive hosted paths | Scalars copied directly; complex values stay `NodeValue` | Primitive callbacks use scalar args/results | Direct scalar equality | Dynamic/list paths still use `NodeValue` |
| C. Host value handles | Would require handle/view APIs | Host-owned handles/views | Host owns value storage | Callbacks receive handles/views | Explicit value/view equality | Needs lifetime rules |
| D. Generated boundary | Typed Roc API can stay | Per-type boundary entrypoints | Generated type-specific ownership helpers | App-model callbacks receive typed payloads | Generated/type-specific equality | Spike covers app model/list item payloads |

## ReleaseFast Synthetic Measurements

| Case | A baseline | B scalar | C handles/views | D generated |
| --- | ---: | ---: | ---: | ---: |
| scalar boundary | 30.7 us | 1.7 us, 18x | 8.2 us, 3.8x | 19.6 us, 1.6x |
| list/record boundary | 15.7 ms | 16.0 ms, 1.0x | 12.5 us, 1259x | 19.5 us, 802x |
| equality | 265.9 us | 255.3 us, 1.0x | 20.5 us, 12.9x | 1.8 us, 152x |
| callback shape | 12.4 us | 1.8 us, 7.1x | 8.8 us, 1.4x | 20.2 us, 0.6x |
| deep map chain | 643.5 us | 341.6 us, 1.9x | 198.9 us, 3.2x | 643.2 us, 1.0x |
| wide fanout | 2.55 ms | 2.51 ms, 1.0x | 2.49 ms, 1.0x | 2.58 ms, 1.0x |
| generated boundary payload, fresh box | n/a | n/a | n/a | 53.8 ms |
| generated boundary payload, reused box | n/a | n/a | n/a | 91.7 us |

Important interpretation details:

- B now measures real scalar paths for `I64`, `Bool`, and `Str` in the host.
- The list/record rows for C and D are still synthetic lower bounds, not full
  `Elem.dynamic` or `Elem.each` replacements.
- The fresh D boundary payload row is real Roc-generated boundary glue, but it
  allocates a fresh boxed boundary payload per call. At 5,000 iterations it
  performs 40,000 allocations and 40,000 deallocations, so it proves correctness
  more than speed.
- The reused D boundary payload row keeps one boxed payload owner, retains it
  before each consuming Roc entrypoint, and consumes the owner with an explicit
  drop entrypoint after the batch. It performs 2 allocations and 2 deallocations
  with zero retained allocation delta.
- ReleaseFast can optimize pure synthetic loops aggressively. The harness now
  puts per-iteration barriers on benchmark values to keep rows loop-bound.

## Current-App Scenarios

These rows run the compiled demo. They report as the production
scalar/`NodeValue` hybrid because the current app uses the production signal
graph with scalar host paths and generated cleanup helpers.

| Scenario | Median time | Key counters |
| --- | ---: | --- |
| counter updates, 5,000 clicks | 184.1 ms | 95,000 allocs/deallocs, 60,000 callbacks |
| diamond updates, 5,000 clicks | 33.7 ms | 0 allocs, 60,000 callbacks, 100,000 evaluated nodes |
| event merge, 5,000 clicks | 31.6 ms | 0 allocs, 15,000 callbacks |
| dynamic toggle, 5,000 clicks | 118.9 ms | 10,000 events, 15,000 callbacks |
| keyed reorder/remove, 10,000 clicks | 1.06 s | 289,995 allocs, 289,998 deallocs, retained delta -3 |

The keyed churn retention issue is fixed. The earlier baseline retained tens of
thousands of Roc allocations because nested `NodeValue` list elements were not
recursively released. `ZigGlue.roc` now emits type-specific helpers such as
`decrefNodeValue`, and the host delegates recursive cleanup to generated glue.

## Glue Findings

`ZigGlue.roc` had two boundary bugs surfaced by the generated boundary payload spike:

1. Provided entrypoints were generated as an obsolete universal
   `ops, ret_ptr, arg_ptr` shape. The compiled Roc object exports natural C ABI
   entrypoints, so generated Zig now declares `pub extern fn roc_main() ...` and
   typed parameters/returns for other provided functions.
2. `Box(opaque)` generated `**anyopaque`. It now stays `*anyopaque`.

The generator also now emits recursive retain/release helpers from explicit
`TypeRepr` layout data. For lists, element release only runs when the outer list
allocation is uniquely owned, immediately before the list allocation is decrefed.

## Findings

1. Scalar hot paths are worth keeping.

   Real scalar paths remove `NodeValue` encode/decode/equality work for common
   `I64`, `Bool`, and `Str` graph nodes. The scalar microbench and callback rows
   improve materially, and the app can use them without changing the typed Roc
   programming model.

2. Recursive ownership cleanup belongs in generated glue.

   Platform host code should not know how to walk every Roc layout. Generated
   helpers improve safety and make nested list/record/tag cleanup auditable from
   the same type data used for ABI generation.

3. Reused generated boundary payloads are the next D baseline.

   The fresh payload spike validates natural entrypoint ABI and app-model calls,
   but boxed-payload-per-call shape is dominated by allocation. The reused
   payload path removes that allocation churn while keeping ownership explicit:
   host-owned payload, per-call retain, consuming Roc entrypoint, final explicit
   drop.

4. Host handles remain promising but risky.

   Handles/views are fast in synthetic list/record rows, but correctness depends
   on explicit lifetime, versioning, and equality semantics.

## Recommendation

Use a hybrid migration path:

1. Keep the scalar fast paths for common signal families.
2. Keep recursive retain/release in generated Zig glue and remove hand-written
   host cleanup for Roc layouts as helpers become available.
3. Use the reused generated boundary payload row as the next D baseline, then
   compare it against borrowed payload and typed generated-payload variants.
4. Leave generic `NodeValue` in place for heterogeneous dynamic boundaries until
   a generated or handle-based design proves ownership and equality semantics.
