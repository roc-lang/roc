# Compact LIR Materialization for Constant Aggregates

## Problem

LIR has no compact way to materialize a large or repeated aggregate
value: list construction is lowered one element per frame local.
`lowerListInto` (`src/postcheck/solved_lir_lower.zig:2192`, and the
typed variant at `:2231`) allocates a `LocalId` per element and emits a
single `assign_list` over the whole span. A compile-time-known
`List.repeat(List.repeat(1, 260), 260)` therefore expands to 67,600
frame locals; issue #9898 was the `u16` span-length overflow this
caused, and PR #9924 fixed it by widening `LocalSpan.len` to `u32`
(`src/lir/LIR.zig:103`) — raising the ceiling without touching the
O(n) explosion.

The costs scale with generated code, which is exactly where they hurt:

- Compile time: O(n) locals per constant aggregate through LIR, ARC
  insertion, and every backend's frame layout.
- Generated code: O(n) frame size and O(n) initialization stores for
  values that are constants; the dev backends ship the naive shape
  verbatim.
- The `u32` ceiling is still a ceiling; a large enough constant is still
  a compiler abort rather than a slow path.

Strings already solved this class: PR #9425 stores big string literals
as static data. Lists (and by extension records/tuples of constants
inside them) have no equivalent — neither a static-data form nor a
bounded-loop initializer.

## Background

LIR is the shared lowering target consumed by ARC insertion
(`src/lir/arc.zig`), the certifier, and five executors (LLVM, dev
x86-64, dev aarch64, wasm, interpreter). Adding a construct here means
each executor implements it once against one definition — the shape
that worked for checked arithmetic (`src/lir/checked_arithmetic.zig`)
and TRMC join-point loops (`src/lir/trmc.zig`), where the construct is
defined once and no backend rediscovers the pattern.

Two regimes need distinct treatments:

- **Fully constant aggregates** (every element a compile-time constant):
  belong in static data like big strings, with refcount-frozen headers
  so ARC never touches them. This is the #9898 shape.
- **Repeated dynamic values** (`List.repeat(x, n)` with runtime `x` or
  `n`): already a runtime builtin call when written directly; the
  problem case is lowering paths that unroll a *known* `n` at compile
  time into per-element locals instead of emitting the builtin call or
  a bounded loop.

ARC interaction is the design-risk center: a static constant list of
refcounted elements must either be refcount-frozen end-to-end (elements
included, as static strings are) or is not eligible for static data.
The eligibility predicate must be decided once, in lowering, not
per-backend.

## Evidence

- `src/postcheck/solved_lir_lower.zig:2192` `lowerListInto` /
  `:2231` `lowerListIntoAtType`: per-element `LocalId` allocation into
  `assign_list`.
- `src/lir/LIR.zig:101-103` `LocalSpan` with `len: u32` — the widened
  ceiling from PR #9924, with the repro
  (`List.repeat(List.repeat(1, 260), 260)`) in that PR's test.
- PR #9425: the static-data precedent for strings, including the
  refcount-frozen header treatment.

## Solution design

1. **Static constant lists.** Extend the static-data facility from
   strings to lists: when every element of a list literal (or
   fully-constant `List.repeat`) is a compile-time constant of a
   layout with no runtime-managed interior (or one that can be
   recursively frozen), emit the buffer into static data with a frozen
   header and lower the expression to a static reference. Define the
   eligibility predicate once in the lowering (`solved_lir_lower.zig`),
   with the layout store answering the "freezable" question.
2. **Bounded-loop or builtin materialization for the rest.** A known-`n`
   repeat of a non-constant value lowers to the existing runtime
   `List.repeat` builtin call (one call, no unrolling) — no new LIR
   construct is needed for this regime, just the decision to stop
   unrolling above a small threshold (a handful of elements, where
   unrolling genuinely wins).
3. **Keep `assign_list` for the small case.** Small literal lists stay
   as they are; define the size threshold as a named constant with its
   rationale, not an inline magic number.
4. All executors: implement static-list references (pointer to static
   data + length/capacity per the frozen header rules); the interpreter
   included. ARC insertion and the certifier treat static references as
   refcount-inert, mirroring static strings.

## What success looks like

Every criterion below must hold; the project is not done until all do:

- The #9898 shape (`List.repeat(List.repeat(1, 260), 260)` at module
  scope) lowers with O(1) frame locals: assert an explicit local-count
  bound in a LIR-level unit test, not just "does not crash."
- A large fully-constant list literal (e.g. 100k integers) compiles to a
  static-data reference: assert no `assign_list` with a span over the
  elements exists in the proc's LIR, and the emitted binary contains
  the buffer once.
- A known-count repeat of a runtime value emits one `List.repeat`
  builtin call, not per-element stores (LIR shape assertion).
- Cross-executor agreement: interpreter, dev x86-64, dev aarch64, LLVM
  (speed and size), and wasm produce identical output for a program
  exercising all three regimes (extend the cross-engine conformance
  suite in `src/cli/test/parallel_cli_runner.zig`).
- Refcount correctness: a static constant list of strings passed
  through functions that retain/release it runs under the debug
  allocator with zero balance errors (frozen headers never reach the
  allocator).
- Compile-time measurement: lowering + ARC + codegen time for the
  100k-element constant list improves by an order of magnitude vs the
  unrolled baseline (record numbers in the PR); CI benchmarks show no
  regression elsewhere.
- The `u32` span ceiling is no longer reachable by constant aggregates:
  the previous overflow repro family compiles regardless of element
  count (bounded by static data size, not local count).

## How to evaluate the result

### Correctness ideal

One eligibility predicate in one lowering decides the regime; every
executor consumes the decision. No backend re-derives constancy or
freezability. ARC's treatment of static references is a rule about the
construct, not per-site judgment.

### Performance ideal

Generated code: constant aggregates cost zero initialization at runtime
and zero frame space. Compile time: constant-aggregate lowering is
O(bytes emitted), not O(elements × pipeline stages). Nothing regresses
for small literals (threshold keeps the fast path).

## Tests to add

- LIR-shape unit tests per regime (static reference / builtin call /
  small unrolled), with explicit local-count assertions.
- The cross-engine conformance program above.
- Debug-allocator balance test for frozen refcounted elements.
- Keep PR #9924's overflow repro green and extend it one order of
  magnitude past the old `u16` ceiling.
