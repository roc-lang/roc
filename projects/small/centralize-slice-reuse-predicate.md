# Centralize the Seamless-Slice Allocation-Reuse Predicate

## Problem

Roc lists and strings use "seamless slices": a value may be a window into a
larger backing allocation. For such values the runtime predicate
"refcount == 1 ⇒ safe to destructively reuse the allocation" is **false**:
a unique slice still owns cleanup for the *entire* backing allocation,
including elements outside its window. Any builtin fast path that keys
reuse on `isUnique` alone can duplicate, prematurely release, or leak the
refcounted elements outside the window, or resize/free an allocation whose
true extent it does not know.

Issue roc-lang/roc#9742 was exactly this class: `roc test` and `roc run`
disagreed because refcounted elements outside a slice window were
mishandled. PR roc-lang/roc#9841 fixed `listSublist`, `listDropAt`,
`listConcat`, and `RocList.reallocate` by adding per-site
`can_reuse_allocation = !list.isSeamlessSlice() and (update_mode == .InPlace
or list.isUnique(roc_ops))` locals, plus a new
`decrefAfterMovingSliceElements` consume path (dec the elements outside the
window, then decref the raw allocation), threading the element-dec callback
ABI through the four backend surfaces (LLVM, dev, interpreter, wasm — plus
the wasm signature table). But the predicate now exists as several
open-coded copies, other sites are safe only by *encoding accident*, and at
least one `isUnique` fast path has no slice guard at all (see Evidence).
Because borrow inference deliberately drives refcounts to 1 to enable
in-place mutation, every unique-fast-path builtin is a live hazard — and
this class produces silently wrong program output, not compiler panics.

## Background

The compiler pipeline: parse → canonicalize → type-check → postcheck IRs
(Monotype → Lifted → Lambda Solved → Lambda Mono) → LIR lowering → ARC
insertion (`src/lir/arc.zig`, from the whole-program borrow solution in
`src/lir/arc_solve.zig`; `src/lir/arc_certify.zig` is the debug-build
certifier; `design.md` sections "ARC Borrow Inference" and "Debug Borrow
Certifier" are authoritative) → backends (LLVM / dev x86+aarch64 / wasm /
interpreter). Runtime list/str builtins live in `src/builtins/` and use
`refcount == 1` checks for in-place mutation. Borrow inference (PRs
roc-lang/roc#9595 and roc-lang/roc#9604) keeps refcounts at 1 across
read-only uses precisely so those in-place paths fire — which makes any
uniqueness-predicate bug in the runtime highly reachable from ordinary
programs.

Representation (`src/builtins/list.zig`, `RocList`): `capacity_or_alloc_ptr`
holds capacity shifted left by one for normal lists, or the backing
allocation pointer with the low bit (`SEAMLESS_SLICE_TAG = 1`) set for
slices. `isSeamlessSlice()` tests that bit; `getCapacity()` returns the
*length* for slices; `getAllocationDataPtr()` returns the backing
allocation; `getAllocationElementCount()` reads the whole-allocation
element count from the heap header for slices with refcounted elements
(written by `setAllocationElementCount` when a slice is created from a
unique list). `RocStr` mirrors this, plus a small-string inline form.

The static side already models slices specially: slice-producing low-level
ops (`list_sublist`, `list_drop_at`, ...) use
`RcEffect.runtimeUniquenessMaybeSharedResult` in `src/base/LowLevel.zig` —
their results alias their argument and are *not* marked `result_unique`, so
slice results are excluded from the born-unique analysis; `RcEffect` also
has an explicit `result_shares_args` mask; and PR roc-lang/roc#9610 added
`listMapCanReuse` (`src/builtins/list.zig`) so `List.map` never reuses a
slice's allocation. The runtime predicate is the remaining gap.

## Evidence

Verified against the current tree (`src/builtins/list.zig` unless noted):

Sites already carrying the correct compound predicate (PR 9841), each as a
separate open-coded copy:

- `listSublist` — `can_reuse_allocation` local with explanatory comment.
- `listDropAt` — `can_reuse_allocation` local.
- `listConcat` — `can_consume_a`/`can_consume_b` plus
  `a_reuses_allocation`/`b_reuses_allocation` (slice-guarded).
- `RocList.reallocate` — `isUnique(...) and !isSeamlessSlice()`; the copy
  path `reallocateFresh` computes `move_slice_elements` and calls
  `decrefAfterMovingSliceElements` (asserts `isSeamlessSlice()` and
  `isUnique()`; decs elements outside the window, then raw decref).
- `listMapCanReuse` — `!isSeamlessSlice() and isUnique(...)`.

Sites safe only by encoding accident:

- `listReserve` — `(update_mode == .InPlace or list.isUnique(roc_ops)) and
  cap >= desired_cap`: no slice guard; safe only because `getCapacity()`
  returns the length for slices, so a unique slice passes only when
  `spare == 0`, where returning it unchanged happens to be harmless.
- `listReleaseExcessCapacity` — in-place test compares
  `list.capacity_or_alloc_ptr == RocList.encodeCapacity(old_length)`: safe
  only because `encodeCapacity` yields even values and a slice's tag bit
  is set, so the equality can never hold for a slice (the comment says as
  much, but nothing enforces it).

`isUnique` fast paths without a slice guard, to audit and convert (or
annotate with an assert explaining why they are safe):

- `RocList.decref` — `elements_refcounted and self.isUnique(roc_ops)` then
  decs `getAllocationElementCount` elements from the allocation base:
  correct for slices, but only via the heap-stored count contract.
- `RocList.makeUnique` — returns `self` for any unique value including a
  unique slice; safe for element writes inside the window, not for
  allocation reuse; audit callers.
- `listIsUnique` (exported) — `list.isEmpty() or list.isUnique(roc_ops)`;
  audit what generated code uses it to justify.
- `RocList.increfWithAtomicity` — slice-guarded already (writes the
  allocation element count); keep, but document against the predicate.

`src/builtins/str.zig` (`RocStr.isUnique` takes no `roc_ops`):
`RocStr.reallocate`, `fromSubListUnsafe`, and `strReleaseExcessCapacity`
are slice-guarded; `substringUnsafe`, `strTrim`, `strTrimStart`,
`strTrimEnd`, `strWithAsciiLowercased`, `strWithAsciiUppercased`, and
`strJoinWithC` take `isUnique` fast paths with no slice guard — strings
have no refcounted elements, so window-local length/byte edits are
believed safe, but each site must be audited and annotated.

## Solution design

1. Add one predicate method on `RocList`:
   `canReuseAllocation(self, update_mode, roc_ops) = !isSeamlessSlice() and
   (update_mode == .InPlace or isUnique(roc_ops))`, and the `RocStr`
   equivalent (no `roc_ops`; also `false` for small strings, which have no
   heap allocation to reuse — callers handle the inline form first). The
   doc comment states the contract: *unique means sole owner of the WHOLE
   allocation; that equals "safe to reuse" only when the window IS the
   allocation.*
2. Convert every predicate copy from PR 9841 (`listSublist`, `listDropAt`,
   `listConcat`, `RocList.reallocate`, `listMapCanReuse`) to call it.
3. Audit every `isUnique` call site in `list.zig` and `str.zig` (the lists
   above enumerate them) and either convert it to the predicate or attach
   a `std.debug.assert` plus comment stating exactly why the site is safe
   without the slice guard. The accidental-safety sites (`listReserve`'s
   `getCapacity`-returns-length, `listReleaseExcessCapacity`'s tag-bit
   inequality) must stop being accidents: use the predicate, or assert
   the encoding fact they rely on next to the reliance.
4. Make `decrefAfterMovingSliceElements` the single
   consume-a-unique-slice path: any site that takes ownership of a unique
   slice's backing allocation goes through it (today `reallocateFresh` is
   the only caller; the audit may find more).
5. Longer-term option, explicitly **not** this project: encode the
   window/allocation relationship in the list header so the predicate
   becomes a single flag test.

Nothing is deleted structurally; the deletions are the open-coded
`!isSeamlessSlice() and (... or isUnique(...))` expressions replaced by
predicate calls.

## What success looks like

- No direct `isUnique`-based reuse decision remains in `list.zig` or
  `str.zig` outside `canReuseAllocation`; every remaining bare `isUnique`
  call site carries an assert/comment documenting why the slice guard is
  unnecessary there.
- `listReserve` and `listReleaseExcessCapacity` either use the predicate
  or assert the encoding fact they depend on.
- `decrefAfterMovingSliceElements` is the only code path that consumes a
  unique slice's backing allocation.
- The issue 9742 reproduction passes identically under `roc test` and
  `roc run` across all backends.

## How to evaluate the result

### Correctness ideal

For every list/str builtin with a fast path, a test triple:
(a) a unique slice of a shared backing allocation,
(b) a unique slice that is the sole owner of its backing allocation,
(c) refcounted elements (e.g. strings) outside the slice window —
asserting both value correctness and refcount balance: no leak, no
double-free, using the existing `utils.TestEnv` shadow-refcount leak
detection (`src/builtins/utils.zig` reports allocations whose refcount
never reaches 0, with operation history).

### Performance ideal

The predicate inlines to the same instructions as today's open-coded
checks — spot-check ReleaseFast codegen for `listSublist` and `listDropAt`
before/after. In-place fast paths are still taken in every legitimate
case: add a counter (test builds) or a test asserting reuse actually
happens for case (b)-style inputs and for plain unique non-slice lists,
so the fix never silently degrades to copy-always. Generated-code
performance and release compile time must not regress; this is a
runtime-builtins change only, so compile time is unaffected by
construction.

## Tests to add

- The (a)/(b)/(c) triples above for each converted builtin, in the
  existing Zig test blocks of `list.zig` / `str.zig`.
- An end-to-end reproduction of issue 9742 (slice of a list of strings,
  mutated through a unique-fast-path builtin, output compared between
  interpreter and compiled backends).
- A randomized (fuzz-ish) sequence test: build a list of refcounted
  elements, apply a random sequence of sublist/dropAt/concat/reserve/
  releaseExcessCapacity operations with random sharing (extra increfs),
  compare against a naive reference implementation, and assert zero
  leaks via `TestEnv`.

## Related projects

- [ARC Certifier Lattice Join and Centralized Ownership-Transfer Keying](../big/arc-certifier-lattice-join.md)
  — the compile-time twin of this cleanup: both replace per-site
  open-coded ownership reasoning with one shared, documented definition.
