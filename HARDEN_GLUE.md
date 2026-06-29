# Harden Glue Runtime Contracts

This document is an implementation plan for hardening Roc glue generation by
turning generated glue into executable contracts. The goal is to prove that each
language glue spec produces code that compiles, links, and behaves correctly
when used by a platform host under adversarial runtime checks.

The current glue tests already generate C, Zig, and Rust glue for test
platforms and compile some of the generated output. That is a good baseline, but
it does not fully exercise ownership, allocator layout, refcount behavior,
seamless slices, boxed payload teardown, or wasm32 pointer-width behavior. The
next step is to link generated glue, platform host code, and Roc apps into
contract executables or wasm modules that can be run.

The intent is not to create a separate class of artificial contract-only
platforms. Every glue test platform should look like a legitimate Roc platform
shape. The parallel test harness supplies the glue generation, host build
plumbing, instrumentation, telemetry, runtime assertions, and reporting
underneath that normal platform/app workflow.

## Goals

1. For every glue test platform, run every glue spec:
   - `src/glue/src/CGlue.roc`
   - `src/glue/src/ZigGlue.roc`
   - `src/glue/src/RustGlue.roc`

   The platform is shared. The language-specific part is only the host
   code that consumes the generated glue.

2. Link the host code with Roc apps for native and wasm targets, then execute
   the resulting program/module.

3. Exercise both directions of the platform boundary:
   - Roc calls hosted functions implemented by the host.
   - The host calls Roc-provided entrypoints.

4. Use adversarial platform hosts that validate runtime invariants:
   - allocation size, alignment, base pointer recovery, canaries, and balanced
     deallocation
   - string/list/box layouts on 64-bit native and 32-bit wasm
   - ownership transfer, retain/decref behavior, and finalizers
   - nested refcounted payload cleanup
   - tag union layout, discriminant offsets, payload access, and aliases
   - static data and seamless slice handling
   - overflow checks before allocation

5. Keep failures actionable. A failing contract should identify the glue spec,
   target, platform shape, scenario, and invariant that failed, and preserve the
   generated work directory.

6. Make tests red-green. Every hardening scenario should fail against a known
   broken glue implementation and pass after the generator/runtime helper is
   fixed.

7. Let linked runtime contracts become the primary glue correctness signal.
   Snapshot and compile-only tests should be kept when they catch distinct
   failures cheaply, and trimmed when the runtime matrix covers the
   same behavior with stronger evidence.

8. Unify all glue test platforms under one parallel harness. There should be no
   abandoned compile-only platform directories, no language-owned platform
   directories, and no separate "contract-only" platform tree. Shared host
   helper code is fine, but the Roc source in each platform directory should
   remain a plausible local platform.

9. Generate idiomatic helper APIs for C, Zig, and Rust so platform authors do
   not need to know Roc compiler internals. The generated interface should be
   sufficient for ordinary platform code to construct, retain, release, inspect,
   pass, return, and call Roc ABI values correctly. This is also the ABI
   upgrade path: platform consumers should depend on regenerated glue instead
   of hand-rolled bindings.

## Non-Goals

1. Do not delete existing snapshot or compile-only glue tests blindly in the
   first hardening PR. Migrate deliberately: keep tests that still provide
   distinct shape, diagnostic, or portability value, and remove or shrink tests
   once linked runtime contracts make them redundant.

2. Do not rely only on source-string assertions for behavioral contracts. Use
   string assertions only when the emitted source shape itself is the contract,
   such as Rust compile-fail safety boundaries or required compile-time layout
   assertions.

3. Do not require all adversarial checks to run on every target immediately.
   Native and wasm32 should be required first. Full cross-target compilation can
   remain compile-only until runners are available.

4. Do not make language-specific glue artificially identical. The contract is
   ABI behavior, not stylistic equivalence.

5. Do not add language-only glue platforms. A platform like "Zig layout
   platform" or "Rust ownership platform" is the wrong abstraction for this
   suite. If a shape exposes a ZigGlue bug, RustGlue bug, or CGlue bug, the
   shape belongs in a generic glue platform shape and all three glue specs must
   run against it.

6. Do not keep legacy glue directories outside the unified process. If a
   platform is worth keeping, it should be migrated into the same generate,
   compile-host, build-app, run, report pipeline as every other glue platform.

## Current Starting Point

Relevant paths:

- `src/cli/test/parallel_cli_runner.zig`
  - owns the CLI glue suite
  - generates glue into isolated work directories
  - already has a glue matrix over C, Zig, Rust, platforms, and targets
  - already compiles generated C/Zig/Rust in some cases

- `src/glue/src/CGlue.roc`
- `src/glue/src/ZigGlue.roc`
- `src/glue/src/RustGlue.roc`
  - language-specific glue generators

- `test/glue/platform-shapes/*`
  - current reflection platforms for broad type shape coverage
  - these should be treated as ordinary glue platforms and migrated into the
    unified runtime pipeline

- `test/glue/rust-duplicate-tag-platform`
- `test/glue/zig-layout-platform`
  - current language-named regression platforms
  - these should be converted into generic platform shapes, or folded into an
    existing generic platform shape such as a type catalogue or layout coverage
    platform
  - after conversion, CGlue, ZigGlue, and RustGlue must all run against them

- `test/wasm/main.zig`
  - existing bytebox-based wasm runner pattern
  - already invokes wasm exports and reads result data/allocation counters

- `test/wasm/platform/*`
  - existing wasm platform shape for linking host wasm and Roc app wasm

The hardening work should build on these pieces instead of inventing a parallel
test framework.

## High-Level Test Model

Each runtime test should follow this flow:

1. Discover every glue test platform under `test/glue/`, excluding common
   helper libraries and harness-only support files.

2. For each platform, run every glue spec:
   - `roc glue src/glue/src/CGlue.roc <output_dir> <platform/main.roc>`
   - `roc glue src/glue/src/ZigGlue.roc <output_dir> <platform/main.roc>`
   - `roc glue src/glue/src/RustGlue.roc <output_dir> <platform/main.roc>`

3. Compile a language-specific platform host library/object that
   imports/includes the generated glue:
   - C: `#include "roc_platform_abi.h"`
   - Zig: `const abi = @import("roc_platform_abi.zig");`
   - Rust: `#[path = "roc_platform_abi.rs"] mod abi;`

4. Link that host artifact with a Roc app built for the same test platform.

5. Execute the result:
   - native executable for native targets
   - wasm module through the existing bytebox-style runner for wasm32

6. Collect structured pass/fail data, timings, allocation counters, target
   metadata, and scenario telemetry from the executable or wasm module.

There is one runtime contract path: generated glue is consumed by a platform
host, the host is linked with a Roc app, and the resulting executable or wasm
module is run. Compile-only checks remain useful as preflight diagnostics, but
they are not a substitute for runtime contract execution.

## Unified Platform Shape Strategy

All glue test platforms should be first-class platform shapes. They describe
Roc platform APIs, entrypoints, hosted effects, provided functions, and ABI
types in ways that could plausibly exist in a real local platform. They do not
belong to C, Zig, or Rust, and they are not abandoned directories for one old
compile-only check.

Recommended layout:

```text
test/glue/
  common/
    alloc.c
    alloc.zig
    alloc.rs
    report.c
    report.zig
    report.rs
    wasm_runner.zig
  app-model/
    main.roc
    Msg.roc
    View.roc
    host.c
    host.zig
    host.rs
    state_update.roc
    view_roundtrip.roc
  cli-main/
    main.roc
    host.c
    host.zig
    host.rs
    args_and_exit.roc
  type-catalogue/
    main.roc
    host.c
    host.zig
    host.rs
    records.roc
    tags.roc
    ownership.roc
  hosted-nominal-return/
    main.roc
    host.c
    host.zig
    host.rs
    nominal_roundtrip.roc
```

The exact names can change, but the structure should stay flat: platform
directories live directly under `test/glue/`, while shared allocator, report,
telemetry, wasm runner, and language helper code lives under `test/glue/common/`.

Every platform directory under `test/glue/` is eligible for every glue spec,
unless it is explicitly marked as common harness support. A platform can have
checked-in host implementations in C, Zig, and Rust because the generated glue
is language-specific. Those host implementations are ordinary platform host
code, not generated host source. They may import common test helper libraries,
but they must implement the same Roc platform contract and must all be run
against the same Roc platform and app scenarios.

Each Roc app should use its platform by local path, for example:

```roc
app [main] { pf: platform "main.roc" }
```

The harness may copy a platform directory into an isolated work directory so
generated glue and host build artifacts can live next to it, but the build flow
should still be the normal local-platform flow:

1. generate glue for the platform,
2. compile that platform's host using the generated glue,
3. build the Roc app that imports the platform by local path,
4. run the resulting executable or wasm module.

The full platform-shape suite should intentionally cover different entrypoint
and hosted-effect shapes:

- app-model platforms with messages, view/state values, and update-style
  entrypoints
- CLI-style platforms with `main`, arguments, exit codes, stdout/stderr effects,
  and error returns
- hosted-effect platforms with multiple hosted functions, nominal returns, and
  callbacks into Roc-provided functions
- type-catalogue platforms with broad record, tag, list, string, box, numeric,
  alias, and duplicate-name coverage
- resource-oriented platforms with boxed handles, finalizers, and ownership
  transfer

Across those realistic platform shapes, the suite should intentionally expose
awkward ABI values:

- small and big `Str`
- `List(U8)`
- `List(Str)`
- `List({ name : Str, value : List(U8) })`
- records with padding and mixed alignment
- anonymous records
- tuples
- tag unions with no-payload, single-payload, multi-payload, and many variants
- recursive tag unions through `Box`
- `Box(U64)` host resource handles
- `Box` payloads with and without refcounted fields
- opaque boxes where the payload is not known to the glue consumer
- `I128`, `U128`, `Dec`, `F64`, `Bool`
- function boxes/erased callables if the current platform ABI supports them

The Roc scenario apps should call hosted functions with values designed to stress
ownership and layout. The host should also call Roc-provided entrypoints with
adversarial values and validate the returned values.

## Matrix Requirements

The glue runtime matrix must be exhaustive across these dimensions:

```text
glue test platform
  x glue spec language: CGlue, ZigGlue, RustGlue
  x target: native, wasm32, and compile-only cross targets where configured
  x runtime form: native executable or wasm module where runnable
```

Rules:

- A platform must not be skipped because it was introduced to catch a bug in a
  particular glue language.
- A language may have a temporary skip only for a target/runtime capability that
  genuinely does not exist yet, and the skip must name the missing capability.
- A runtime skip is not a compile skip. If runtime support is blocked, the
  generated glue should still be compiled for that platform and target.
- When a language-specific regression is discovered, add or extend a generic
  platform shape, then verify all three glue specs against it.
- Test names and failure reports must include platform, glue spec, target, and
  scenario.
- Language-named directories such as `rust-duplicate-tag-platform` and
  `zig-layout-platform` should be renamed or folded into generic platform
  shapes, then run through the same matrix as every other platform.

Example generated cases:

```text
CGlue    app-model       native  executable
ZigGlue  app-model       native  executable
RustGlue app-model       native  executable
CGlue    app-model       wasm32  linked-wasm
ZigGlue  app-model       wasm32  linked-wasm
RustGlue app-model       wasm32  linked-wasm
CGlue    type-catalogue  native  executable
ZigGlue  type-catalogue  native  executable
RustGlue type-catalogue  native  executable
```

This is the central policy of the hardening work: platforms are shared, glue
specs vary.

## Platform Host Runtime Checks

The linked executable or wasm module is the test runner. The platform host code
inside it is the runtime oracle, and it should be more aggressive than normal
platform code.

### Allocation Instrumentation

Each language host should use a contract allocator with the same semantics:

- record requested length and alignment
- verify returned data pointers satisfy the requested alignment
- over-allocate guard bytes before and after user memory
- fill new memory with a non-zero poison byte
- record allocation ids in a table
- reject unknown frees
- reject double frees
- verify dealloc alignment matches allocation alignment
- verify guard bytes on free
- verify realloc preserves old bytes up to min(old_len, new_len)
- optionally poison freed memory
- expose `alloc_count`, `dealloc_count`, `live_alloc_count`, and
  `allocator_error_count`

For wasm, the same counters should be exported so the bytebox runner can assert
that the module ended with no leaks and no allocator errors.

### Assertion Reporting

Avoid panicking across FFI boundaries. Platform hosts should write failures into
a small in-memory report buffer and return a non-zero code.

Recommended common report format:

```text
FAIL <scenario-id>: <short invariant name>: <details>
```

Native hosts can print the report to stderr. Wasm hosts should export:

- `contract_run_all() -> i32`
- `contract_result_ptr() -> usize`
- `contract_result_len() -> usize`
- `contract_alloc_count() -> usize`
- `contract_dealloc_count() -> usize`
- `contract_live_alloc_count() -> usize`

The wasm runner reads the report from wasm memory and checks the counters.

### Scenario Isolation

Each scenario should reset allocator counters and failure state. At the end of a
scenario, assert:

- no failure was recorded
- expected destructors ran
- live allocation count is zero, unless the scenario intentionally returns
  ownership to Roc or tests static data
- all retained values were balanced by decrefs

## Durable ABI Risk Register

The enduring ABI risk register and Roc glue runtime-control definitions live in
`src/glue/README.md`. This hardening document is a temporary implementation
plan, so it must not be the only place that records long-term glue risks or the
controls that enforce them.

During this work, every `TBA` runtime-control slot in the README must be
replaced with the concrete glue test-harness scenario, platform shape, language
matrix cell, compile-fail check, or source-shape assertion that enforces the
contract.

## Generated Helper Requirements

The glue generators must make the platform-facing API complete enough that
ordinary platform code does not need to hand-roll Roc runtime knowledge.

Required helper families:

- layout assertions for every exported/hosted ABI type, including target pointer
  width differences
- constructors and views for `RocStr`, `RocList`, boxes, records, tag unions,
  and erased callables
- retain/release/drop helpers for every refcounted public type, including
  recursively refcounted fields and active tag payloads
- explicit owned-value operations for hosted arguments, provided returns, stored
  values, and values returned back to Roc
- allocation helpers that account for Roc headers, static data, list element
  metadata, alignment, overflow, and wasm32 pointer width
- erased-callable invocation helpers that build the args buffer, receive the
  result buffer, and balance ownership of captures, arguments, and returns
- unsafe or ownership-typed APIs where the host language cannot make invalid
  states impossible

C, Zig, and Rust may expose these helpers differently, but each generated API
must cover the same ABI requirements. A language-specific helper may be more
idiomatic than the others; it must not make a weaker contract.

## Contract Categories

### 1. Layout and ABI Shape

Check generated language types against Roc-reflected layout:

- size and alignment for records and tag unions
- tag discriminant offset
- payload offset and alignment
- wasm32-specific record field order where applicable
- pointer-width-specific `RocStr`, `RocList`, `RocBox`, and function pointer
  layout
- single-variant tag union unwrapping
- no-payload tag unions
- duplicate type names from separate modules
- anonymous record argument and return wrappers

Compile-time assertions should exist in generated glue where possible, but the
linked executable or wasm module should also pass actual values across the
boundary and validate field contents.

### 2. String Contracts

Scenarios:

- empty string
- small string at every boundary:
  - 0 bytes
  - max small length
  - max small length plus 1
- big string with refcount 1
- static string with refcount 0
- seamless slice into a larger string
- UTF-8 with multibyte scalars
- string returned from host to Roc and back
- string passed from Roc to host and retained by host
- invalid UTF-8 construction should be impossible from safe generated APIs, or
  explicitly unsafe if exposed

Assertions:

- `len` matches bytes
- `as_slice` points at the expected bytes
- small string bytes are inline
- big string rc slot is read and updated correctly
- final decref frees exactly once
- static strings are not freed
- seamless slices decref the backing allocation, not the slice pointer

### 3. List Contracts

Scenarios:

- empty list
- `List(U8)` small and large
- `List(Str)` with every element big enough to allocate
- `List(record-with-refcounted-fields)`
- seamless slice of `List(Str)`
- list returned from host and consumed by Roc
- list passed from Roc and retained by host

Assertions:

- length and capacity encoding are correct
- element pointer alignment is correct
- refcount slot is initialized
- refcounted-element allocation header contains the backing element count
- `allocation_items` walks the backing allocation, not just the slice
- final decref of a unique `List(Str)` decrefs every element exactly once
- shared lists do not decref elements until final release
- `from_slice` for non-refcounted elements is safe and shallow
- shallow copy of refcounted elements is either impossible from safe APIs or
  requires explicit retain

This category directly catches bugs like uninitialized refcounted-list element
counts and safe shallow copies of `RocStr` elements.

### 4. Box Contracts

Scenarios:

- `Box(U64)` host resource handle
- `Box(I64)` on wasm32, where payload alignment can exceed pointer alignment
- `Box(record-with-Str)`
- `Box(tag-union-with-refcounted-payload)`
- opaque boxed app type
- boxed function/erased callable if supported

Assertions:

- allocation base is recovered using the same header size used for allocation
- payload alignment, not pointer alignment, is used for known boxed payloads
- `payload_contains_refcounted` controls header size independently from
  whether a teardown callback exists
- payload finalizer runs exactly once
- nested refcounted fields are decref'd before freeing the box allocation
- opaque boxes use the opaque-safe decref path

### 5. Refcount and Ownership Contracts

Scenarios:

- owned argument is consumed by hosted function
- host stores an argument by retaining it
- host returns an owned value
- host returns a stored value after retaining
- Roc passes same allocation through multiple aliases
- final release of nested values
- static data with rc 0

Assertions:

- incref/decref counts are balanced
- final release uses acquire/release ordering where applicable
- destructors run after all writes to payloads are visible
- values that own references are not `Copy` in Rust unless the API makes the
  ownership hazard explicit
- destructive Rust helpers are `unsafe` or ownership-typed
- generated recursive decref helpers do not double-drop copied values

For Rust, add compile-fail snippets as part of the runtime suite:

- moving `RocStr` and using the moved value should fail if `RocStr` is not
  `Copy`
- calling `decref`, `incref`, `allocate`, `from_slice`, or `decref_box_with`
  without `unsafe` should fail if those operations are unsafe contracts
- constructing invalid raw states through public fields should either be
  impossible or marked unsafe by API design

### 6. Overflow and Bounds Contracts

Scenarios:

- list length that overflows `length * size_of::<T>()`
- allocation size that overflows header plus payload
- shifted capacity that cannot fit
- erased callable capture size overflow
- zero-sized payloads

Assertions:

- overflow is detected before allocator entry
- failure is deterministic:
  - native: linked executables should report a runtime assertion failure instead of
    unwinding across FFI
  - wasm: trap or explicit non-zero failure is acceptable, but must be checked
    by the wasm runner

### 7. Tag Union Contracts

Scenarios:

- all no-payload variants
- all single-payload variants
- mixed payload sizes and alignments
- nested payloads containing strings/lists/boxes
- duplicate tag union names from separate modules
- aliases to structurally identical tag unions
- wasm32 payload layout

Assertions:

- discriminant values match Roc tags
- active payload access reads correct bytes
- inactive payloads are never read by generated cleanup helpers
- recursive decref follows only active payloads
- aliases and duplicate names resolve to the expected generated type names

### 8. Native and Wasm Execution

Native requirements:

- compile the selected platform host as the host artifact for the target
- build a Roc app that imports the selected platform by local path
- run the linked executable
- run with stdout/stderr captured by `parallel_cli_runner.zig`
- preserve work dir on failure
- optionally enable sanitizers where available:
  - C via `zig cc -fsanitize=address,undefined` where supported
  - Rust via miri is useful but should be optional, not required in default CI
  - Zig safety checks in debug mode

Wasm requirements:

- build wasm32 variants for each runnable glue platform
- run them through a bytebox-based runner similar to `test/wasm/main.zig`
- validate wasm32 pointer width and layout assumptions at runtime
- read exported report buffers from wasm memory
- assert allocator counters after each scenario
- avoid OS/thread scenarios in wasm

The wasm runner should support at least:

```text
zig build run-test-glue-runtime-wasm -- --language rust --scenario all
zig build run-test-glue-runtime-wasm -- --language zig --scenario all
zig build run-test-glue-runtime-wasm -- --language c --scenario all
```

The exact CLI can differ, but the runner must report language, target, and
scenario names.

## Harness Changes

### 1. Add a Glue Runtime Case Type

Extend `src/cli/test/parallel_cli_runner.zig` with a runtime-specific body
rather than squeezing everything into the compile-only glue matrix.

Suggested shape:

```zig
const GlueRuntimeCase = struct {
    language: GlueLanguage,
    target: GlueTarget,
    execution_mode: GlueExecutionMode,
    platform: GluePlatformShape,
    app: GlueScenarioApp,
    scenario_filter: ?[]const u8,
};
```

Then add a `glue_runtime` case body or a new custom case dispatcher. The case
source should be the discovered glue platform matrix, not a hand-maintained
list of special cases.

### 2. Keep Compile-Only Checks Separate During Migration

The compile-only matrix should answer:

- did the generator run?
- did the generated file compile for this language and target?

The runtime matrix should answer:

- did generated glue behave correctly when compiled into the selected platform
  host and linked with a Roc app?
- did the Roc app that imports that platform by local path run correctly?

Keeping these separate during migration makes failures easier to triage. Once a
linked runtime contract covers the same behavior, prefer the runtime contract
and shrink the compile-only or snapshot coverage to the smallest remaining
diagnostic value.

### 3. Use Checked-In Platform Hosts

Each glue platform should contain real host implementations for each glue
language. The harness does not generate host source files. It makes generated
glue available to the checked-in host source and then compiles that host source
as the platform host artifact.

Examples:

- C host source includes generated glue with an include path:
  `#include "roc_platform_abi.h"`
- Zig host source imports generated glue with a module path:
  `const abi = @import("roc_platform_abi.zig");`
- Rust host source imports generated glue with a path or module configuration:
  `#[path = "roc_platform_abi.rs"] mod abi;`

The host implementations contain allocator instrumentation, hosted symbols, and
any calls to Roc-provided symbols needed by the scenarios.

### 4. Build Native Linked Contracts

Add helper functions:

- `prepareGlueContractWorkdir(...)`
- `generateGlueForPlatform(...)`
- `compilePlatformHostArtifact(...)`
- `buildRocAppWithLocalPlatform(...)`
- `runGlueContractExecutable(...)`
- `checkGlueContractOutput(...)`

Native commands:

- C: `zig cc -std=c11 -Wall -Wextra -Werror ...`
- Zig: `zig build-obj` or `zig build-lib` for the host artifact
- Rust: `rustc --edition=2021 -D warnings ...`

Flow:

1. Create an isolated work directory and copy or reference the selected glue
   platform and selected Roc app.

2. Run the selected glue spec for that platform into the host build directory.

3. Compile the platform's checked-in host source for the selected language,
   using the generated glue from step 2.

4. Build the Roc app. The app imports the platform by local path; the platform's
   target inputs resolve to the host artifact produced in step 3 and `app`.

5. Run the resulting executable.

6. Check stdout/stderr and allocator report.

This avoids committing generated glue or generated host artifacts while still
using the same workflow real platforms use.

### 5. Build Linked Wasm Contracts

Flow:

1. Generate glue for the wasm-compatible glue platform for each glue spec.

2. Compile the language host for wasm32:
   - Zig host: `zig build-obj -target wasm32-freestanding`
   - C host: `zig cc -target wasm32-freestanding -c`
   - Rust host: `rustc --target wasm32-unknown-unknown`, or another documented
     wasm Rust target if the platform host requires it

3. Build the Roc app for wasm32. The app imports the platform by local path; the
   platform's wasm32 target inputs resolve to the host wasm object and `app`.

4. Run the linked wasm through a bytebox runner.

5. Read exported report and allocator counters.

6. Fail the CLI test if any scenario fails or leaks.

The final wasm runtime requirement applies to CGlue, ZigGlue, and RustGlue
against the same platform set. If Rust wasm runtime support needs staging, keep
that as an explicit temporary blocker with a tracking issue; do not encode it as
a permanent platform or matrix exception.

### 6. Preserve Work Directories on Failure

On any runtime failure, include:

- generated glue path
- generated host source path
- compile command
- run command
- target
- scenario id
- report text

Do not delete the isolated work directory when a runtime check fails.

## Language-Specific Requirements

### C Glue

Minimum:

- compile the platform's `host.c` with `zig cc`
- use `-std=c11 -Wall -Wextra -Werror`
- include generated header from an external translation unit
- assert struct sizes, offsets, and tags where the C header exposes enough data
- build and run the linked native Roc executable
- build and run the linked wasm32 Roc module

Additional:

- optional sanitizer build for native platforms that support it
- wasm32 runtime through bytebox

### Zig Glue

Minimum:

- compile the platform's `host.zig` as the host artifact
- build and run the linked native Roc executable
- build and run the linked wasm32 Roc module through bytebox
- use debug/safe builds for runtime tests
- assert `@sizeOf`, `@alignOf`, and `@offsetOf`

Additional:

- keep existing wasm32 layout checks, but move behavioral checks into runtime
  scenarios where possible

### Rust Glue

Minimum:

- compile generated glue with `rustc --edition=2021 -D warnings`
- compile the platform's `host.rs` as the host artifact
- build and run the linked native Roc executable
- build and run the linked wasm32 Roc module
- add compile-fail snippets for unsafe ownership boundaries as an additional
  source/API gate

Compile-fail harness:

- write each snippet to the output directory
- run `rustc` expecting non-zero exit
- assert stderr contains a stable substring such as:
  - `use of moved value`
  - `call to unsafe function`
  - `requires unsafe`

Avoid asserting entire stderr output.

Wasm:

- run wasm32 runtime checks for every runnable glue platform, the same as
  CGlue and ZigGlue
- keep compile-only wasm32 checks as an additional diagnostic layer, not as a
  replacement for runtime contracts

## Red-Green Test Plan for Known Failure Classes

These scenarios should be added first because they correspond to real issues
found in generated Rust ABI helpers.

1. Refcounted values are safe `Copy`.
   - Red: generated Rust allows copying `RocStr` and double-decref in safe code.
   - Green: ownership-bearing values are not `Copy`, or destructive operations
     are unsafe/ownership-typed and compile-fail snippets catch misuse.

2. Refcounted list element count header is uninitialized.
   - Red: allocate `RocList<RocStr>` with poisoned allocation memory, create a
     seamless slice, then final-decref; the helper reads poison as element count.
   - Green: allocation writes backing element count and the scenario passes.

3. `RocList::allocate` returns initialized-looking uninitialized memory.
   - Red: safe API permits `as_slice`, `Debug`, or `decref` before element
     initialization.
   - Green: allocation is unsafe or uses an initialization builder API.

4. `RocList::from_slice` shallow-copies refcounted elements.
   - Red: safe `from_slice(&[RocStr])` produces two owners without incref.
   - Green: the API is unsafe for refcounted elements, or it increments element
     refs.

5. Raw bytes can become `RocStr` and later `as_str` uses unchecked UTF-8.
   - Red: safe code constructs invalid UTF-8 and calls safe `as_str`.
   - Green: raw-byte constructor validates, returns a fallible result, or is
     explicitly unsafe.

6. Allocation size arithmetic overflows.
   - Red: huge length wraps, allocator is called with a smaller size.
   - Green: overflow is detected before allocator entry.

7. Final release uses relaxed atomics only.
   - Red: generated source contains final-release `fetch_sub(... Relaxed)`.
   - Green: final release uses release/acquire ordering. Use a source-shape
     assertion plus targeted native stress tests if practical.

8. Box payload alignment and header-size recovery.
   - Red: known non-refcounted payloads use pointer alignment, or teardown
     callback presence changes header size.
   - Green: payload alignment and explicit payload-refcounted flag are used.

## CI and Developer Workflow

Add or extend build steps:

```text
zig build run-test-cli -- --suite glue
zig build run-test-glue-runtime-native
zig build run-test-glue-runtime-wasm
zig build run-test-glue-runtime
```

Recommended default CI during migration:

1. existing glue matrix compile checks that still provide distinct diagnostics
2. native runtime checks for C, Zig, Rust
3. wasm runtime checks for C, Zig, Rust
4. compile-only cross-target checks where configured

Recommended default CI after migration:

1. native runtime checks for C, Zig, Rust
2. wasm runtime checks for C, Zig, Rust
3. a reduced compile-only cross-target smoke matrix for targets that cannot run
   yet
4. a small set of source-shape checks only where runtime execution cannot
   express the contract

Recommended full CI/nightly:

1. all default checks
2. full cross-target compile matrix
3. optional sanitizer variants
4. optional Rust miri checks for generated helper APIs, if useful

Developer filtering:

```text
zig build run-test-cli -- --suite glue --filter "RustGlue runtime"
zig build run-test-cli -- --suite glue --filter "wasm boxes"
zig build run-test-glue-runtime -- --language zig --target wasm32 --scenario lists
```

Exact flags can differ, but the runner should allow filtering by language,
target, platform shape, and scenario.

## Implementation Phases

### Phase 1: Harness Skeleton

- Add runtime case types to `parallel_cli_runner.zig`, or split glue runtime
  execution into a helper module imported by the CLI runner.
- Pick one existing realistic glue platform shape, such as `app-model`,
  `cli-main`, or `type-catalogue`, and wire it into the linked runtime flow.
- Generate glue for that platform for CGlue, ZigGlue, and RustGlue.
- Compile each platform host (`host.c`, `host.zig`, `host.rs`) against its
  generated glue.
- Build a Roc app that imports the platform by local path.
- Run the linked native executable for each glue spec.
- Preserve work dirs on failure.

Acceptance:

- one scenario runs for each language on native
- failure report names glue spec, target, platform shape, and scenario

### Phase 2: Native Runtime Matrix

- Migrate the remaining existing glue platform shapes into the same native
  linked flow.
- Compile host artifacts for all three glue specs.
- Build Roc apps against those host artifacts through local platform paths.
- Run native executables for all three glue specs.
- Exercise hosted calls and provided entrypoints.
- Convert `rust-duplicate-tag-platform` and `zig-layout-platform` into generic
  platform shapes, or fold their coverage into an existing generic platform
  shape, then run all three glue specs against the result.

Acceptance:

- host receives Roc values and validates them
- host sends adversarial values into Roc and validates returns
- allocator counters balance at scenario end

### Phase 3: Core Runtime Invariants

Add scenarios for:

- strings
- lists
- boxes
- nested records
- tag unions
- duplicate names and aliases
- overflow/bounds
- static data
- seamless slices

Add or tighten generated helpers for CGlue, ZigGlue, and RustGlue as needed so
these scenarios can be written as ordinary platform code. Host code should call
generated APIs for retain, release, recursive teardown, list allocation,
string/list views, tag payload access, box payload cleanup, and erased callable
invocation rather than duplicating Roc ABI internals.

Acceptance:

- every known failure class listed above has a red-green test
- every `TBA` control in the ABI risk register has either a named runtime
  scenario, named compile-fail/API check, or named source-shape assertion
- each helper required by the covered risks exists for every glue language where
  that operation is part of the public platform surface

### Phase 4: Wasm Runtime Matrix

- Add bytebox runner for glue runtime wasm modules.
- Build wasm32 runtime modules for CGlue, ZigGlue, and RustGlue against the
  same glue platform shapes used by native runtime checks.
- Export report/counter functions from wasm host.
- Add wasm scenarios for pointer-width-sensitive layouts.
- If one language needs additional runtime plumbing, record that as a temporary
  implementation blocker while keeping compile-only coverage for that same
  platform/language/target cell.

Acceptance:

- CGlue, ZigGlue, and RustGlue all run wasm32 runtime checks for every runnable
  glue platform required by the suite.
- wasm32 runtime run validates lists, strings, boxes, and tag unions
- wasm32 allocation counters balance
- failures preserve wasm module and generated sources

### Phase 5: Wasm Runtime Hardening

- Tighten the wasm runtime host implementations for all three glue languages.
- Decide any language-specific wasm host details without changing the glue
  platform matrix:
  - Rust may use `no_std` plus a custom allocator, or another documented wasm
    target if needed.
  - C and Zig should use freestanding-compatible host code.
- Add wasm scenarios for all pointer-width-sensitive cases, including layout,
  list headers, boxed payload alignment, seamless slices, and static data.

Acceptance:

- The wasm runtime matrix has no permanent language-specific holes.
- Any temporary skip names a missing host/runtime capability, not a
  language-specific platform exemption.

### Phase 6: Cleanup and Policy

- Document how to add new glue runtime scenarios.
- Make failures concise.
- Add CI labels or steps.
- Ensure there are no glue test platforms left behind as compile-only platform
  directories or language-specific directories.
- Remove or shrink redundant snapshot and source-string checks once runtime
  contracts cover the same behavior.

## Suggested First PR

Keep the first PR small:

1. Choose one existing realistic platform shape, preferably the smallest one
   that still exercises hosted and provided calls. Good candidates are:
   - `test/glue/platform-shapes/type-catalog`
   - `test/glue/platform-shapes/cli-main`
   - `test/glue/platform-shapes/app-model`

2. Move or adapt it into the unified flat layout if desired, for example
   `test/glue/type-catalogue/`, without changing the Roc source into a
   test-only artifact.

3. Add checked-in platform hosts for that same platform:
   - `host.c`
   - `host.zig`
   - `host.rs`

4. Add the first shared helper code under `test/glue/common/`:
   - allocator counters
   - guard bytes
   - in-memory report buffer
   - scenario result formatting

5. Add one Roc app scenario in the platform directory that imports `main.roc`
   by local path.

6. Add the linked native harness flow:
   - discover the selected platform shape
   - generate CGlue, ZigGlue, and RustGlue for its `main.roc`
   - compile the matching checked-in host against the generated glue
   - build the selected Roc scenario app
   - run the executable

7. Add Rust compile-fail snippets as a separate API gate:
   - safe `decref` is rejected
   - safe uninitialized list allocation is rejected
   - shallow `from_slice` on `RocStr` is rejected or unsafe

8. Add allocator instrumentation for:
   - alignment
   - double free
   - guard bytes
   - live allocation count

9. Add one red-green runtime scenario:
   - allocate `RocList<RocStr>` and verify the refcounted backing element count
     header is initialized.

This first PR establishes the pattern for one real platform shape. Later PRs
should bring every existing glue platform into the same process, including
generalizing `rust-duplicate-tag-platform` and `zig-layout-platform`.

## Open Decisions

1. Should Rust generated refcounted value types be non-`Copy`, or should they
   remain `Copy` with all ownership-affecting operations marked unsafe?

2. Should generated helper constructors expose uninitialized memory directly, or
   should they use builder APIs that track initialized length?

3. Which Rust wasm runtime target should the platform host use:
   `wasm32-unknown-unknown`, `wasm32-wasip1`, or another documented target?

4. Should linked runtime runs abort immediately on first failure or collect
   multiple failures per run? Collecting multiple failures is better for local
   iteration, but first-failure abort is simpler for wasm.

5. Which runtime scenarios should be required in default CI versus nightly or
   full target CI?

## Definition of Done

Glue hardening is complete when:

- C, Zig, and Rust glue are generated from every glue test platform.
- Generated glue compiles for native and wasm32.
- Native linked executables run and pass for C, Zig, and Rust.
- Wasm32 linked modules run and pass for C, Zig, and Rust.
- Linked Roc apps exercise hosted and provided calls.
- The instrumented allocator catches alignment, double-free, wrong-free, canary,
  and leak failures.
- Known failure classes have red-green coverage.
- CI preserves enough artifacts to debug failures without re-running locally.
- Every row in the ABI risk register names its enforcing glue control.
- CGlue, ZigGlue, and RustGlue generate idiomatic helper APIs for all supported
  host-visible Roc ABI operations, so platform authors do not need to hand-roll
  layout, ownership, or refcount logic.
