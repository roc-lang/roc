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

Glue generators must consume compiler-emitted metadata such as `AbiLayout`,
`TypeInfo`, and host RC plans. They must not reconstruct runtime layout or
ownership behavior from display strings, semantic type names, source field
order, or incidental data structure shape.

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

The control column intentionally uses `TBA` while the hardening plan is being
staged; each `TBA` must be replaced with the concrete glue runtime scenario,
platform shape, language matrix cell, compile-fail check, or source-shape
assertion that enforces the contract.

Those controls should run through realistic glue platforms and should cover
CGlue, ZigGlue, and RustGlue. A bug found in one generated language should be
captured as a shared platform shape unless the behavior is genuinely
language-specific.

| Risk | Required generated support | Runtime control |
| --- | --- | --- |
| Owned-vs-borrowed boundary confusion for hosted args, provided returns, and stored values | Explicit retain/release/move helpers and generated hosted-function comments/signatures that make ownership transfer visible | TBA |
| Recursive refcounting for nested records, tag payloads, lists, boxes, closures, and recursive types through `Box` | Data-driven recursive retain/release helpers emitted from compiler RC plans, not from type names | TBA |
| Lists with refcounted elements, including backing element-count headers | List helpers that know element width, alignment, and whether element teardown is required | TBA |
| Seamless slices for `Str` and `List` | Allocation-base recovery helpers that never free the visible slice pointer directly | TBA |
| Roc allocation headers, static refcount zero, and deallocation alignment | Allocator wrappers and assertions for header size, alignment, canaries, and static-data no-op release | TBA |
| `RocStr` small, big, static, and sliced representations | Safe byte/as-str views, constructors with UTF-8 policy, retain/release helpers, and no C-string assumptions | TBA |
| `RocList` representation, including different field order from `RocStr` | Typed list helpers for allocation, initialization, slicing, element access, retain, and release | TBA |
| Erased callable invocation from the host | Generated callable wrapper APIs for args buffer, return buffer, capture pointer, host context, and owned result handling | TBA |
| Stored Roc closures and dev hot-reload image lifetime | Retain/release helpers for callable boxes and documented per-thread host-state requirements for later invocation | TBA |
| Natural C ABI target differences, including sret, aggregate returns, wasm32, and pointer width | Generated extern signatures and compile-time layout assertions per target language | TBA |
| Compiler-emitted layout facts: field order, offsets, discriminants, size, alignment, zero-sized values, and Bool representation | Glue consumers must use emitted `AbiLayout` metadata and generated assertions rather than reconstructing layout | TBA |
| Nominal vs structural records and explicit padding fields | Generated record types/opaque bytes with per-target size, alignment, field-offset, and padding assertions | TBA |
| Tag union active-payload handling, discriminant layout, single-variant unwrapping, duplicate names, and recursive tags | Generated tag constructors/accessors/destructors that only touch the active payload and assert discriminant layout | TBA |
| Opaque or library containers such as `Dict` whose internals are not platform ABI | Opaque-safe helpers only; no generator or host code may infer container internals from semantic names | TBA |
| Accidental shallow copies of ownership-bearing values | Rust non-`Copy` or unsafe ownership APIs; Zig/C retain/release naming and runtime double-free/leak checks | TBA |
| Error paths after ownership transfer into hosted functions | Scenario helpers that deliberately fail after receiving owned values and verify cleanup policy | TBA |
| Thread sharing and atomic vs thread-confined refcounting | Thread-aware contracts where supported, plus explicit documentation of host-visible value sharing requirements | TBA |
| Stale generated bindings after ABI changes | Version/header assertions or harness checks that regenerated glue is used for each platform and target | TBA |
| C glue opacity and limited structural visibility | Generated C helper functions/macros for operations that C cannot safely express by struct field access alone | TBA |
