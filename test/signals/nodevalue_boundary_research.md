# Signals NodeValue Boundary Research

This report summarizes the current boundary-focused prototype pass for the
signals platform. The app-facing platform no longer exposes benchmark payloads:
representative apps use ordinary Roc UI code, and benchmark instrumentation
lives in the Zig host.

## Commands

```sh
./zig-out/bin/roc check src/glue/src/ZigGlue.roc
./zig-out/bin/roc glue src/glue/src/ZigGlue.roc test/signals/platform test/signals/platform/main.roc
zig build run-test-cli -- --suite glue
zig build run-test-signals -Dplatform=signals
zig build run-signals-bench -Dplatform=signals
zig build build-test-hosts -Dplatform=signals -Doptimize=ReleaseFast
./zig-out/bin/signals-ops-dashboard --bench --bench-iterations 5000 --bench-samples 3
```

The matrix below comes from the previous ReleaseFast host run with three
5,000-iteration samples. Values are median elapsed time unless noted. Re-run the
commands above after platform changes before drawing fresh numeric conclusions.

## Prototype Matrix

| Prototype | API impact | ABI impact | Ownership model | Callback shape | Equality strategy | Dynamic/list support |
| --- | --- | --- | --- | --- | --- | --- |
| A. Baseline `NodeValue` | Current typed Roc API | 32-byte tagged union | Host stores owned `NodeValue` copies | Boxed callbacks receive `NodeValue` | Recursive generated/host equality | Supported |
| B. Scalar fast paths | Adds explicit scalar API helpers for hot paths | Typed primitive hosted paths | Scalars copied directly; complex values stay `NodeValue` | Primitive callbacks use scalar args/results | Direct scalar equality | Dynamic/list paths still use `NodeValue` |
| C. Host value handles | Would require handle/view APIs | Host-owned handles/views | Host owns value storage | Callbacks receive handles/views | Explicit value/view equality | Needs lifetime rules |
| D. Generated boundary | Typed Roc API can stay | Per-type boundary entrypoints | Generated type-specific ownership helpers | App-model callbacks receive typed payloads | Generated/type-specific equality | Needs generated glue for dynamic/list item types |

## ReleaseFast Synthetic Measurements

| Case | A baseline | B scalar | C handles/views | D generated |
| --- | ---: | ---: | ---: | ---: |
| scalar boundary | 30.7 us | 1.7 us, 18x | 8.2 us, 3.8x | 19.6 us, 1.6x |
| list/record boundary | 15.7 ms | 16.0 ms, 1.0x | 12.5 us, 1259x | 19.5 us, 802x |
| equality | 265.9 us | 255.3 us, 1.0x | 20.5 us, 12.9x | 1.8 us, 152x |
| callback shape | 12.4 us | 1.8 us, 7.1x | 8.8 us, 1.4x | 20.2 us, 0.6x |
| deep map chain | 643.5 us | 341.6 us, 1.9x | 198.9 us, 3.2x | 643.2 us, 1.0x |
| wide fanout | 2.55 ms | 2.51 ms, 1.0x | 2.49 ms, 1.0x | 2.58 ms, 1.0x |

Important interpretation details:

- B measures real scalar paths for `I64`, `Bool`, and `Str` in the host.
- The list/record rows for C and D are still synthetic lower bounds, not full
  `Elem.dynamic` or `Elem.each` replacements.
- ReleaseFast can optimize pure synthetic loops aggressively. The harness keeps
  per-iteration barriers on benchmark values to keep rows loop-bound.
- The old generated payload spike proved natural provided-entrypoint ABI and
  generated ownership helpers, but it was app-specific benchmark scaffolding.
  That path has been removed from the maintained app/platform contract.

## Glue Findings

`ZigGlue.roc` had two boundary bugs surfaced by the generated boundary spike:

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

3. Generic list/record boundaries are the major cost center.

   The generic `NodeValue` path is adequate for correctness and simplicity, but
   the synthetic list/record row shows the cost center clearly. Real UI apps need
   dynamic render payloads, keyed list items, and app model fragments, so the next
   round should compare alternatives that preserve typed Roc ergonomics.

4. Host handles remain promising but risky.

   Handles/views are fast in synthetic list/record rows, but correctness depends
   on explicit lifetime, versioning, equality, and renderer-scope ownership
   semantics.

## Recommendation

Use a hybrid migration path:

1. Keep scalar fast paths for common signal families.
2. Keep recursive retain/release in generated Zig glue and remove hand-written
   host cleanup for Roc layouts as helpers become available.
3. Leave generic `NodeValue` in place for heterogeneous dynamic boundaries until
   a generated or handle-based design proves ownership and equality semantics.
4. Research three concrete alternatives for dynamic/list payloads next:
   generated type-specific dynamic/list glue, host-owned value handles with
   explicit lifetime/version rules, and borrowed read-only views for render-only
   paths.
5. Keep benchmarking and stats in the host. Representative app Roc code should
   stay focused on real UI behavior.
