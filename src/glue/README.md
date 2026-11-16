# Roc Glue

`roc glue` generates platform-facing bindings from checked Roc platform
metadata. A glue script such as `src/glue/src/ZigGlue.roc`,
`src/glue/src/RustGlue.roc`, or `src/glue/src/CGlue.roc` receives the
compiler-emitted type table and writes host-language source files for platform
authors.

Compiled glue plugins carry their frozen constant graphs, including backing
allocations and callable pointers. They must remain self-contained when installed
and loaded by another process, rather than borrowing the compiler's addresses or
assuming a platform link will supply their data.

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
- 128-bit integer vectors use the target C ABI's native vector class, including
  different direct-argument, direct-result, and aggregate rules, and
  memory-class multi-vector aggregates need the union spelling in C

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
  integer vectors, and erased callables
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

For concrete Roc-owned values, Zig and Rust glue compose zero-runtime-storage
release policies. `RocList(T).deinit` in Zig selects the compiler-generated
policy for `T` at comptime. Rust provided functions whose results have complete
release policies also get an `_owned` wrapper returning `RocOwned`; its
zero-sized policy implements recursive `Drop` without adding a descriptor or
context pointer to the value. List release atomically claims the final spine
reference before reading elements, then releases the allocation's complete
element range and finally frees the spine. Opaque payloads are not assigned a
structural policy: recursive teardown across that boundary requires an explicit
compiler-owned release root or descriptor rather than host-side guessing.

### Values Only Roc Can Release

A `Box` or `List` over a `RocUnknown` entry is a Roc reference the host owns
and cannot release. Dropping the last reference means tearing the payload down
and freeing an allocation whose header size depends on the payload's alignment
and on whether the payload is refcounted. For an opaque target only compiled
Roc code knows any of that, so a host-side release either leaks the payload's
own references or hands `roc_dealloc` the wrong base pointer.

The compiler reports this as `HostRcPlan.RcRocReleased`. A type has that plan
when it is a box or list over an opaque target, or when a box payload, list
item, record field, or tag payload it owns has it. The plan comes from the type
table, never from the committed layout: the layout was selected with the
application's variables sealed, so it calls `Box(model)` a box over nothing and
`Box({ model : model, n : U64 })` a box over a struct, and the two are the same
thing to a host.

For an `RcRocReleased` type, generators emit:

- a retain (`incref`), which is sound because the refcount slot sits directly
  before the payload whatever the payload is
- no `decref`, no release policy, no `decrefListOf...` or box payload helper,
  and no owning wrapper (`_owned` in Rust)
- for a list whose items are opaque, `RocOpaqueList`: no item type, no item
  access, no allocation
- for a list whose items have a known layout and hold an opaque value,
  `RocHeldList(T)` in Zig and Rust: readable items, no allocation, because the
  allocation header depends on whether those opaque values are refcounted. C
  lists carry no item type, so C uses `RocOpaqueList` for both.

The host retains such a value, reads the parts that have a known layout,
releases those parts one at a time, and passes the opaque value back to Roc. A
platform that wants its host to drop one provides a Roc function that takes
ownership of it:

```roc
release_model_for_host : Box(Model) -> {}
release_model_for_host = |_| {}
```

Compiled Roc then performs the release with the layout it knows.

## Pointer-Width Contract

Crossing the host boundary does not make a value indirect. Arguments, results,
and provided values need a committed representation; glue rejects unresolved
by-value types before running a generator. An actual box can carry an unknown
payload, and an erased callable can remain opaque when its invocation signature
has no standalone representation.

"By value" follows the value all the way down: a record field, tuple element,
tag payload, or nominal backing is held by value by whatever holds its parent.
A `Box` payload or `List` element is held through a pointer instead, and each
one is described on its own terms. If it has a committed layout it gets its
typed description; if it does not, because it is an unresolved type variable or
holds one by value at any depth, it is one `RocUnknown` entry and the box is an
opaque `RocBox`. Generators never receive a partial struct for such a payload,
and its `AbiLayout` carries no size. A payload that reaches the variable only
through a further `Box` or `List` keeps its typed description.

The glue script input is targetless. It carries no concrete Roc build target,
OS, or architecture, and `roc glue` has no `--target` flag: running it on any
machine produces the same generated source for a given platform and glue
script. Compiler-emitted layout facts vary only by host pointer width, modeled
as `AbiWidth := [Pointer32, Pointer64]`. Generated source may additionally use
the host compiler's target conditionals to select the natural C spelling and
calling convention for a native vector leaf; that selection does not alter any
compiler-emitted record, union, or ownership fact.

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
- Render every `Vector128` leaf with the target's native vector type rather
  than a byte array or `U128` substitute. Keep all eight Roc vector names as
  distinct generated binding types.
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

## Type Names

A Roc name does not identify a host type. Two instantiations of one generic
nominal (`Wrap(U64)` and `Wrap(Str)`) share a name and have different layouts,
and the same instantiation can occupy several type table rows, because it is
reached from several signatures or from inside another nominal's backing.

The compiler therefore gives every row a `shape_id`: the id of the first row
that describes the same host type. Rows with equal shape ids agree on repr,
layout, and ownership at every depth, recursive types included, so a generator
emits one declaration for all of them. Rows that share a name but not a shape
id are different host types. `TypeTable.duplicate_record_names` and
`TypeTable.duplicate_tag_union_names` report those names, and
`TypeTable.shape_suffix` gives each shape its suffix, so the generated names
are `TypesWrapType2` and `TypesWrapType5` in every language. A name that is
not shared keeps its bare form.

An ambiguous bare name is never emitted, not even as an alias: `TypesWrap`
would denote one instantiation chosen by table order. Roots that reach such a
type are aliased by their own position instead (`Take_wrapsArg0`,
`Take_wrapsArg1`), and those aliases are the stable names for host code.

Generators must not decide type identity themselves, by comparing names,
sizes, or field lists.

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

## Host ABI Lock

The templates restate the host ABI—the `RocStr`/`RocList`/`RocDec`
layouts, the `RocHost` callback vtable, the extern runtime symbols, and the
erased-callable payload—as text in their output languages, so the compiler
cannot import those restatements. `zig build run-check-glue-abi` is the
enforcement. It generates Zig glue and compiles `test/glue/zig_abi_lock.zig`
against both the generated file and the canonical `builtins` definitions for
x86-64 and AArch64 on Linux, macOS, and Windows, plus wasm32. It also generates
the vector-heavy `layout-probe` C and Rust bindings: C is compiled over that
same seven-target matrix, while Rust is compiled for the configured native
target and wasm32 (rustc requires an installed core library for every
additional cross target).
Template drift from `src/builtins/host_abi.zig`, `str.RocStr`,
`list.RocList`, `dec.RocDec`, or `erased_callable` fails that step at compile
time. Generated C and Rust files carry matching `offsetof`/`offset_of!` static
assertions, which their compile controls and the native/wasm runtime matrix
verify per target.

`test/glue/generate_abi_lock.zig` reflects the canonical builtins declarations
into target-independent C and Rust reference declarations. C includes the
resulting `canonical_host_abi.h`; Rust compiles a copy of its generated bindings
with canonical reference structs, layout assertions, and type assignments
appended. The foreign compilers determine each target's physical layout. C
locks strings, lists, and erased-callable callbacks; Zig and Rust also lock the
runtime symbol signatures and the helper-only `RocHost` callback prefix.
`RocHost` deliberately excludes interpreter-only hosted dispatch tables and is
never passed across compiled Roc entrypoints.

## Glue platform marshalling

Glue generators carry their frozen constants in the same relocatable object as
their code, so installed generators need neither process-local addresses nor a
second data object. The image stays loaded until returned values have been
copied. Both lowering strategies publish the named field/tag schemas of their
platform boundary and complete constants using that same strategy.

`glue.zig` materializes glue input and reads its result through the checked
platform's runtime schemas and committed LIR layouts. The old
`platform/host.zig` extern records have been removed; alphabetical field order
is no longer a handwritten ABI assumption. `GlueRocValueWriter.recordField`
uses the schema's logical field index and the layout store's physical offset.

The `glue platform marshalling schema matches checked Roc records` test compiles
`DebugGlue.roc` and locks the complete marshalling protocol against the actual
checked platform, including nested list elements, records, tag payloads, and
scalar leaf types.
A second test copies the platform sources into isolated temporary directories,
mutates field names, field sets, scalar types, nested record types, and tag
payloads. Every mutated checked schema must fail the same lock. Run both with:

```sh
zig build run-test-zig-module-glue -- --test-filter 'glue platform'
```

The `frozen generator` CLI regression checks long strings, shared nested
constants, captured functions, copy-on-write, and zero-filled data under both
strategies and all three glue optimization modes. The `install/glue roundtrip`
test loads that same generator in separate processes, including after cache
removal.

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
| Owned-vs-borrowed boundary confusion for hosted args, provided returns, and stored values | Explicit retain/release/move helpers and generated hosted-function comments/signatures that make ownership transfer visible | `glue runtime: cli-main` owns/decrefs `RocStr` hosted args and `roc_main` results; `glue runtime: app-model` hands boxed models back to Roc through `roc_release_model` and releases the rest of the rendered `View` field by field; all CGlue/ZigGlue/RustGlue native+wasm32 cells. |
| Recursive refcounting for nested records, tag payloads, lists, boxes, closures, and recursive types through `Box` | Data-driven recursive retain/release helpers emitted from compiler RC plans, not from type names | `glue runtime: type-catalog` exercises `Tree`, boxed payloads, recursive tags, and catalog unions; `glue runtime: layout-probe` exercises many boxed fields; `glue command generated Zig compiles with zig build-obj` checks emitted retain/release helpers. |
| Lists with refcounted elements, including backing element-count headers | List helpers that know element width, alignment, and whether element teardown is required | `glue runtime: cli-main` constructs `List(Str)` args and validates the refcounted element-count header; `glue runtime: app-model` renders and decrefs `View.messages : List(Msg)`; issue 10451's Zig/Rust glue cases compile recursive release for a directly returned `List(Str)`. |
| Seamless slices for `Str` and `List` | Allocation-base recovery helpers that never free the visible slice pointer directly | `glue runtime: cli-main` uses generated Zig/Rust `RocStr.fromSlice` and `RocList.fromSlice` helpers and decrefs through allocation-base recovery; CGlue helper source is covered by `glue command generated C header compiles with zig cc` and `CGlue.roc expect tests pass`. |
| Roc allocation headers, static refcount zero, and deallocation alignment | Allocator wrappers and assertions for header size, alignment, canaries, and static-data no-op release | `glue runtime: cli-main` and `glue runtime: app-model` use adversarial host allocators with alignment/live-allocation checks; `glue regression: ZigGlue decrefs non-refcounted boxed payloads with payload alignment` and the Rust equivalent cover boxed payload alignment/static-release helpers. |
| `RocStr` small, big, static, and sliced representations | Safe byte/as-str views, constructors with UTF-8 policy, retain/release helpers, and no C-string assumptions | `glue runtime: cli-main` covers `roc_cli_read`, `roc_cli_log`, and app args; `glue runtime: type-catalog` covers provided/result strings in records and tag payloads; `glue runtime: app-model` covers `View.title`. |
| `RocList` representation, including different field order from `RocStr` | Typed list helpers for allocation, initialization, slicing, element access, retain, and release | `glue runtime: cli-main` validates `List(Str)` construction and teardown; `glue runtime: type-catalog` covers `List(Bool)` fields; `glue runtime: app-model` checks `List(Msg)` layout in `View`. |
| Erased callable invocation from the host | Generated callable wrapper APIs for args buffer, return buffer, capture pointer, host context, and owned result handling | `glue command with CGlue generates expected C header`, `glue regression: RustGlue succeeds on fx platform`, `glue command generated Zig compiles with zig build-obj`, and `glue regression: ZigGlue decrefs non-refcounted boxed payloads with payload alignment` cover generated callable payload, capture, call, and drop APIs. |
| Stored Roc closures and dev hot-reload image lifetime | Retain/release helpers for callable boxes and documented per-thread host-state requirements for later invocation | `glue command with CGlue generates expected C header` covers boxed callable store/call signatures; `glue regression: ZigGlue decrefs non-refcounted boxed payloads with payload alignment` checks retained callable final decref; `roc --watch hot reloads app-provided Model through Box` covers image lifetime outside the glue suite. |
| Natural C ABI target differences, including sret, aggregate returns, wasm32, and pointer width | Generated extern signatures and compile-time layout assertions per target language | Full `glue runtime:` matrix compiles/links/runs native and wasm32 hosts for CGlue/ZigGlue/RustGlue; `glue runtime: type-catalog` covers `Dec`, `I128`, `U128`, records, unions, and aggregate returns; C exact-header and compile checks catch stale C ABI signatures. |
| Native 128-bit vector ABI, including SysV SSE/SSEUP, Win64 indirect arguments, AAPCS64 Q-register placement, memory-class multi-vector aggregates, wasm `v128`, mixed aggregates, and register exhaustion | Distinct generated vector leaf types, C-layout aggregates, exact assertions, and extern declarations that use the target C ABI directly | `glue runtime: layout-probe` calls every vector and aggregate shape in both directions from generated C/Zig/Rust hosts; `run-check-glue-abi` compile-locks generated C and Zig across x86-64/AArch64 Linux/macOS/Windows plus wasm and Rust across native+wasm. |
| Compiler-emitted layout facts: field order, offsets, discriminants, size, alignment, zero-sized values, and Bool representation | Glue consumers must use emitted `AbiLayout` metadata and generated assertions rather than reconstructing layout | `glue runtime: layout-probe`, `glue runtime: duplicate-tags`, and `glue runtime: type-catalog` cover emitted layout facts across all languages/targets; `glue command generated Zig compiles with zig build-obj` checks field offsets and compile-time layout assertions. |
| Nominal vs structural records and explicit padding fields | Generated records with concrete committed fields plus explicit padding and per-target size, alignment, field-offset, and padding assertions | `glue runtime: type-catalog` covers nominal and structural records; `glue runtime: layout-probe` covers padded/boxed/vector layouts; `glue regression: ZigGlue quotes bang record fields` covers field names that require escaping. |
| Tag union active-payload handling, discriminant layout, single-variant unwrapping, duplicate names, recursive tags, and payload ownership transfer | Generated tag constructors/destructors, unsafe borrowed references, and unsafe consuming payload projections that only touch the validated active payload | `glue runtime: duplicate-tags` covers duplicate module-local tag/result names; `glue runtime: type-catalog` covers single-payload, no-payload, recursive, boxed, and result tag unions and consumes Rust owning payloads through `take_payload_*_unchecked`. |
| Opaque or library containers such as `Dict` whose internals are not platform ABI | Opaque-safe helpers only; no generator or host code may infer container internals from semantic names | `glue regression: ZigGlue uses RocBox for opaque boxed app types` enforces `RocBox` for opaque boxed app values; `glue regression: a value holding an opaque Roc value has no host release in any generator` enforces that nothing holding one can be released or allocated by the host; C/Rust source-shape and compile controls cover opaque helper signatures without exposing container internals. |
| Accidental shallow copies of ownership-bearing values | Rust non-`Copy` or unsafe ownership APIs; Zig/C retain/release naming and runtime double-free/leak checks | `glue runtime: cli-main`, `app-model`, and `type-catalog` all check balanced live allocations after owned values cross the boundary; `glue regression: RustGlue succeeds on fx platform` covers Rust ownership-shaped generated APIs. |
| Error paths after ownership transfer into hosted functions | Scenario helpers that deliberately fail after receiving owned values and verify cleanup policy | `glue runtime: duplicate-tags` exercises `Try`-returning hosted functions with owned payloads; `glue runtime: cli-main` checks failure reporting paths and requires owned hosted args to be decrefed before a host-side failure is recorded. |
| Thread sharing and atomic vs thread-confined refcounting | Thread-aware contracts where supported, plus explicit documentation of host-visible value sharing requirements | Generated Zig/Rust helper compile controls cover atomic refcount operations; `glue regression: ZigGlue decrefs non-refcounted boxed payloads with payload alignment` and the Rust equivalent check final decrement behavior; runtime matrix documents that current glue hosts are single-threaded. |
| Stale generated bindings after ABI changes | Version/header assertions or harness checks that regenerated glue is used for each platform and target | `runGlueRuntimeCase` runs `roc glue` into an isolated work directory before each host compile/link/run; full `glue runtime:` matrix plus `glue command with CGlue generates expected C header` catch stale generated signatures. |
| C representation of records and tag unions | Concrete C fields, named discriminants, concrete payload storage, typed constructors/accessors, and helpers for ownership operations that field access alone cannot safely express | `glue command with CGlue generates expected C header`, `glue command generated C header compiles with zig cc`, `CGlue.roc expect tests pass`, and CGlue runtime cells for all five platform shapes cover concrete records, tags, payload access, macros, and helper declarations. |

## Release Files

The checked-in glue specs under `src/glue/src/` use `platform glue`, a
compiler-owned platform embedded in the Roc compiler. Keep that form for local
compiler development and releases.

Nightly releases should use generated copies instead:

```sh
zig build build-glue-release -Dglue-release-tag=nightly-YYYY-Month-DD-SHORTSHA
```

This writes `zig-out/glue-release/`:

```text
glue/RustGlue.roc
glue/ZigGlue.roc
glue/CGlue.roc
glue/README.md
glue/env
```

Include the generated `glue/` directory in every platform compiler archive for
that same release. The generated specs rely on the compiler-owned glue platform
embedded in that compiler build, so downstream repos do not need a Roc source
checkout or a separate glue platform package.

When `roc-lang/setup-roc` installs a nightly that contains `glue/`, it exports:

```text
ROC_GLUE_DIR
ROC_RUST_GLUE
ROC_ZIG_GLUE
ROC_C_GLUE
```

Downstream CI can then run:

```sh
roc glue "$ROC_RUST_GLUE" ./platform/main.roc --output-dir ./platform
```
