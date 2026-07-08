# Roc Glue

`roc glue` generates platform-facing bindings from checked Roc platform
metadata. A glue script such as `src/glue/src/ZigGlue.roc`,
`src/glue/src/RustGlue.roc`, or `src/glue/src/CGlue.roc` receives the
compiler-emitted type table and writes host-language source files for platform
authors.

## Why Glue Code Generates the Interface

The Roc platform ABI is not just a collection of structs that can be copied by
hand into C, Zig, or Rust. Host-visible values carry target-specific layout,
ownership, and refcounting contracts:

- records and tag unions have compiler-committed field order, padding, size,
  alignment, discriminants, payload offsets, and pointer-width differences
- `Str`, `List`, `Box`, and erased callables have runtime allocation headers
  and ownership rules that are not visible from the Roc source type alone
- lists of refcounted elements need different allocation metadata and teardown
  than lists of plain bytes
- seamless slices do not necessarily point at the allocation base that must be
  retained or released
- hosted functions receive owned refcounted arguments, and Roc-provided
  functions return owned refcounted values
- recursive refcounted data requires compiler-derived retain/release plans
- erased callables require a specific call ABI, capture teardown callback, and
  host/runtime context

For those reasons, platform code should consume generated glue instead of
hand-rolling Roc ABI bindings. The generated interface is the compatibility
layer between platform authors and Roc compiler internals. If Roc needs to
change its ABI in the future, platform consumers should be able to regenerate
glue and keep using the generated helper API rather than rewriting manual
layout and ownership code.

## Generated Helper Contract

CGlue, ZigGlue, and RustGlue should expose idiomatic helpers for their host
languages while enforcing the same underlying ABI contract. Generated helpers
should cover:

- layout assertions for ABI-visible types
- typed views and constructors for strings, lists, boxes, records, tag unions,
  and erased callables
- retain, release, and recursive teardown for refcounted public types
- explicit ownership transfer for hosted arguments, provided returns, stored
  values, and values returned back to Roc
- allocation helpers for Roc headers, alignment, overflow checks, static data,
  list element metadata, and pointer-width differences
- erased-callable invocation helpers for args buffers, return buffers, captures,
  and owned results
- unsafe or ownership-typed APIs where the host language cannot make invalid
  states impossible

Glue generators must consume compiler-emitted metadata such as `RecordRepr`,
`RecordField`, `TagUnionRepr`, `TagVariant`, `TypeRepr`, and explicit ownership
plans. They must not reconstruct runtime layout or ownership behavior from
display strings, semantic type names, source field order, or incidental data
structure shape.

## Pointer-Width Contract

The glue script input is targetless. It carries no concrete Roc build target,
OS, or architecture, and `roc glue` has no `--target` flag: running it on any
machine produces the same generated source for a given platform and glue
script. The only ABI axis a glue script may branch on is host pointer width,
modeled as `AbiWidth := [Pointer32, Pointer64]`.

What the compiler guarantees to glue scripts:

- Every layout fact arrives for both widths (`size32`/`size64`,
  `offset32`/`offset64`, `alignment32`/`alignment64`,
  `discriminant_offset32`/`discriminant_offset64`, ...), read from the layout
  store with explicit dual-width queries. Use the width-parameterized
  accessors (`AbiLayout.size(layout, width)`,
  `AbiFieldLayout.offset(field, width)`, ...).
- Everything else is width-independent: field order, field names, tag names,
  discriminant values, and refcount plans.
- `AbiRecordLayout.fields` and `AbiTagLayout.payload_fields` are emitted in
  committed layout order, which is identical at both widths with
  non-decreasing offsets at both widths (asserted in `src/glue/glue.zig`).
  Emitters must iterate fields in the given order and must not re-sort them.
- Padding fields (`is_padding`) have nonzero size at both widths; zero-sized
  padding markers such as `_ : {}` are dropped before emission.

What generators must do with that data:

- Render pointer-sized Roc values (`Str`, `List`, `Box`, erased callables)
  with the host language's pointer-sized types (`size_t`/`uintptr_t`, `usize`,
  raw pointers), not hardcoded 4- or 8-byte shapes.
- Emit one source file per language containing both width shapes, resolved by
  the host compiler: `#if UINTPTR_MAX == UINT64_MAX` in C,
  `if (@sizeOf(usize) == 4)` in Zig, `#[cfg(target_pointer_width = "32")]` in
  Rust. A future language without a compile-time width idiom may instead emit
  one file per width; the dual-width schema supports both.
- Emit size/alignment/offset assertions from the emitted facts for both
  widths, so the host compiler proves the generated shapes match the
  committed ABI.
- Never compute sizes, offsets, or padding from reflected type shape; the
  compiler owns the ABI facts.

Concrete build targets still exist, but only in the test harness and `roc
build`: the runtime matrix compiles the same generated source for native and
wasm32 hosts and runs it in both.

## Regenerating the C Header Snapshot

`glue command with CGlue generates expected C header` byte-compares CGlue
output for `test/fx/platform/main.roc` against
`test/glue/fx_platform_cglue_expected.h`. After an intentional CGlue change,
regenerate the snapshot with:

```sh
roc glue src/glue/src/CGlue.roc /tmp/cglue-out test/fx/platform/main.roc
cp /tmp/cglue-out/roc_platform_abi.h test/glue/fx_platform_cglue_expected.h
```

Treat an unexpected diff as an emitter bug, not a regen trigger.

## ABI Risk Register and Roc Glue Runtime Controls

Every risk in this section must be covered by at least one Roc glue test
platform. In this context, "runtime control" means a check executed by the Roc
glue test harness, not a production assertion inserted into ordinary generated
glue.

The glue runtime harness works by:

1. selecting realistic platform shapes,
2. generating C, Zig, and Rust glue for those platforms,
3. compiling each generated interface with a matching host implementation for
   native and wasm targets,
4. building a Roc app that imports the platform, and
5. executing the resulting native program or wasm module.

The host implementation in those runs is deliberately a contract test runner.
It implements the platform API while collecting allocator statistics,
telemetry, ownership events, layout checks, refcount checks, and scenario
results.

The control column names concrete glue runtime scenarios, platform shapes,
language matrix cells, compile checks, or source-shape assertions that enforce
the contract. `glue runtime: <platform>` means the CGlue, ZigGlue, and RustGlue
runtime matrix for both native and wasm32 targets unless the entry names a
specific language, target, or custom case.

Those controls should run through realistic glue platforms and should cover
CGlue, ZigGlue, and RustGlue. A bug found in one generated language should be
captured as a shared platform shape unless the behavior is genuinely
language-specific.

| Risk | Required generated support | Runtime control |
| --- | --- | --- |
| Owned-vs-borrowed boundary confusion for hosted args, provided returns, and stored values | Explicit retain/release/move helpers and generated hosted-function comments/signatures that make ownership transfer visible | `glue runtime: cli-main` owns/decrefs `RocStr` hosted args and `roc_main` results; `glue runtime: app-model` owns/decrefs boxed models and rendered `View`; all CGlue/ZigGlue/RustGlue native+wasm32 cells. |
| Recursive refcounting for nested records, tag payloads, lists, boxes, closures, and recursive types through `Box` | Data-driven recursive retain/release helpers emitted from compiler RC plans, not from type names | `glue runtime: type-catalog` exercises `Tree`, boxed payloads, recursive tags, and catalog unions; `glue runtime: layout-probe` exercises many boxed fields; `glue command generated Zig compiles with zig build-obj` checks emitted retain/release helpers. |
| Lists with refcounted elements, including backing element-count headers | List helpers that know element width, alignment, and whether element teardown is required | `glue runtime: cli-main` constructs `List(Str)` args and validates the refcounted element-count header; `glue runtime: app-model` renders and decrefs `View.messages : List(Msg)`. |
| Seamless slices for `Str` and `List` | Allocation-base recovery helpers that never free the visible slice pointer directly | `glue runtime: cli-main` uses generated Zig/Rust `RocStr.fromSlice` and `RocList.fromSlice` helpers and decrefs through allocation-base recovery; CGlue helper source is covered by `glue command generated C header compiles with zig cc` and `CGlue.roc expect tests pass`. |
| Roc allocation headers, static refcount zero, and deallocation alignment | Allocator wrappers and assertions for header size, alignment, canaries, and static-data no-op release | `glue runtime: cli-main` and `glue runtime: app-model` use adversarial host allocators with alignment/live-allocation checks; `glue regression: ZigGlue decrefs non-refcounted boxed payloads with payload alignment` and the Rust equivalent cover boxed payload alignment/static-release helpers. |
| `RocStr` small, big, static, and sliced representations | Safe byte/as-str views, constructors with UTF-8 policy, retain/release helpers, and no C-string assumptions | `glue runtime: cli-main` covers `roc_cli_read`, `roc_cli_log`, and app args; `glue runtime: type-catalog` covers provided/result strings in records and tag payloads; `glue runtime: app-model` covers `View.title`. |
| `RocList` representation, including different field order from `RocStr` | Typed list helpers for allocation, initialization, slicing, element access, retain, and release | `glue runtime: cli-main` validates `List(Str)` construction and teardown; `glue runtime: type-catalog` covers `List(Bool)` fields; `glue runtime: app-model` checks `List(Msg)` layout in `View`. |
| Erased callable invocation from the host | Generated callable wrapper APIs for args buffer, return buffer, capture pointer, host context, and owned result handling | `glue command with CGlue generates expected C header`, `glue regression: RustGlue succeeds on fx platform`, `glue command generated Zig compiles with zig build-obj`, and `glue regression: ZigGlue decrefs non-refcounted boxed payloads with payload alignment` cover generated callable payload, capture, call, and drop APIs. |
| Stored Roc closures and dev hot-reload image lifetime | Retain/release helpers for callable boxes and documented per-thread host-state requirements for later invocation | `glue command with CGlue generates expected C header` covers boxed callable store/call signatures; `glue regression: ZigGlue decrefs non-refcounted boxed payloads with payload alignment` checks retained callable final decref; `roc --watch hot reloads app-provided Model through Box` covers image lifetime outside the glue suite. |
| Natural C ABI target differences, including sret, aggregate returns, wasm32, and pointer width | Generated extern signatures and compile-time layout assertions per target language | Full `glue runtime:` matrix compiles/links/runs native and wasm32 hosts for CGlue/ZigGlue/RustGlue; `glue runtime: type-catalog` covers `Dec`, `I128`, `U128`, records, unions, and aggregate returns; C exact-header and compile checks catch stale C ABI signatures. |
| Compiler-emitted layout facts: field order, offsets, discriminants, size, alignment, zero-sized values, and Bool representation | Glue consumers must use emitted `AbiLayout` metadata and generated assertions rather than reconstructing layout | `glue runtime: layout-probe`, `glue runtime: duplicate-tags`, and `glue runtime: type-catalog` cover emitted layout facts across all languages/targets; `glue command generated Zig compiles with zig build-obj` checks field offsets and compile-time layout assertions. |
| Nominal vs structural records and explicit padding fields | Generated record types/opaque bytes with per-target size, alignment, field-offset, and padding assertions | `glue runtime: type-catalog` covers nominal and structural records; `glue runtime: layout-probe` covers padded/boxed layouts; `glue regression: ZigGlue quotes bang record fields` covers field names that require escaping. |
| Tag union active-payload handling, discriminant layout, single-variant unwrapping, duplicate names, and recursive tags | Generated tag constructors/accessors/destructors that only touch the active payload and assert discriminant layout | `glue runtime: duplicate-tags` covers duplicate module-local tag/result names; `glue runtime: type-catalog` covers single-payload, no-payload, recursive, boxed, and result tag unions. |
| Opaque or library containers such as `Dict` whose internals are not platform ABI | Opaque-safe helpers only; no generator or host code may infer container internals from semantic names | `glue regression: ZigGlue uses RocBox for opaque boxed app types` enforces `RocBox` for opaque boxed app values; C/Rust source-shape and compile controls cover opaque helper signatures without exposing container internals. |
| Accidental shallow copies of ownership-bearing values | Rust non-`Copy` or unsafe ownership APIs; Zig/C retain/release naming and runtime double-free/leak checks | `glue runtime: cli-main`, `app-model`, and `type-catalog` all check balanced live allocations after owned values cross the boundary; `glue regression: RustGlue succeeds on fx platform` covers Rust ownership-shaped generated APIs. |
| Error paths after ownership transfer into hosted functions | Scenario helpers that deliberately fail after receiving owned values and verify cleanup policy | `glue runtime: duplicate-tags` exercises `Try`-returning hosted functions with owned payloads; `glue runtime: cli-main` checks failure reporting paths and requires owned hosted args to be decrefed before a host-side failure is recorded. |
| Thread sharing and atomic vs thread-confined refcounting | Thread-aware contracts where supported, plus explicit documentation of host-visible value sharing requirements | Generated Zig/Rust helper compile controls cover atomic refcount operations; `glue regression: ZigGlue decrefs non-refcounted boxed payloads with payload alignment` and the Rust equivalent check final decrement behavior; runtime matrix documents that current glue hosts are single-threaded. |
| Stale generated bindings after ABI changes | Version/header assertions or harness checks that regenerated glue is used for each platform and target | `runGlueRuntimeCase` runs `roc glue` into an isolated work directory before each host compile/link/run; full `glue runtime:` matrix plus `glue command with CGlue generates expected C header` catch stale generated signatures. |
| C glue opacity and limited structural visibility | Generated C helper functions/macros for operations that C cannot safely express by struct field access alone | `glue command with CGlue generates expected C header`, `glue command generated C header compiles with zig cc`, `CGlue.roc expect tests pass`, and CGlue runtime cells for all five platform shapes cover C-visible opaque bytes, macros, and helper declarations. |

## Release Files

The checked-in glue specs under `src/glue/src/` use the source-tree platform at
`../platform/main.roc`. Keep that form for local compiler development.

Nightly releases should use generated copies instead:

```sh
zig build build-glue-release -Dglue-release-tag=nightly-YYYY-Month-DD-SHORTSHA
```

This writes `zig-out/glue-release/`:

```text
package/<hash>.tar.zst
glue/RustGlue.roc
glue/ZigGlue.roc
glue/CGlue.roc
glue/README.md
glue/env
```

The package archive is produced by running `roc bundle` in `src/glue/platform`
with `main.roc` as the root file. The release helper only copies that archive
and rewrites the release specs to point at its content-hash URL.

Upload `package/<hash>.tar.zst` to the matching `roc-lang/nightlies` release.
Include the generated `glue/` directory in every platform compiler archive for
that same release. The generated specs reference the exact package URL, so
downstream repos do not need a Roc source checkout.

When `roc-lang/setup-roc` installs a nightly that contains `glue/`, it exports:

```text
ROC_GLUE_DIR
ROC_RUST_GLUE
ROC_ZIG_GLUE
ROC_C_GLUE
ROC_GLUE_PLATFORM_URL
```

Downstream CI can then run:

```sh
roc glue "$ROC_RUST_GLUE" ./platform/main.roc --output-dir ./platform
```
