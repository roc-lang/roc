# Debug Generation Counters on Growable Stores

## Problem

Postcheck and LIR passes mutate flat `std.ArrayList`-backed stores in place.
Those passes often take a span or pointer into one list, then call code that can
append to the same list. If the append reallocates, the old borrow points at
freed storage.

This has already recurred in several forms:

- `src/postcheck/monotype_lifted/spec_constr.zig` held `program.fns` and span
  slices while specialization appended more functions or cloned children.
- `src/postcheck/monotype/lower.zig`,
  `src/postcheck/lambda_mono/lower.zig`, and
  `src/postcheck/monotype_lifted/lift.zig` had stale result-location writes of
  the form `list.items[i] = .{ .field = try appendToSameList(...) }`.
- `src/postcheck/monotype/lower.zig` had recursive type lowering paths that had
  to copy spans because recursion could append to `types.spans` or
  `types.fields`.
- `src/lir/arc.zig` carried comment-plus-refetch discipline around proc rewrites
  because rewriting can append mode-specialized proc variants.

Per-site comments, manual pointer asserts, and local copy decisions have not
held as an invariant. The store itself needs to make stale borrows observable in
Debug builds, and it must do that without false positives.

## Final Design

The implementation uses `src/collections/GuardedList.zig`.

`GuardedList.List(T, name)` wraps a `std.ArrayList(T)` and gives each list its
own Debug-only generation counter. In non-Debug builds the wrapper has the same
size and alignment as `std.ArrayList(T)`, and borrow helpers return raw
slices/pointers. The intended release overhead is zero.

The counter tracks only definite invalidation:

- `append`, `appendSlice`, `ensureUnusedCapacity`, and reserve APIs compare the
  backing pointer before and after a successful operation.
- The generation increments only when the pointer actually changes.
- Appends that fit in spare capacity do not invalidate existing borrows.
- Failed growth does not invalidate existing borrows.
- `appendAssumeCapacity` cannot move storage and therefore does not bump.

Span guards store the list, start, length, and generation. Every Debug access
checks:

- the borrowed element index is inside the borrowed span;
- the borrowed range still fits in the current list length;
- the generation still matches.

That makes shrink/restore range-precise. Restoring a list to a length that still
contains the borrowed prefix does not trip. Restoring below the borrowed range
does trip. Empty-span validation is also precise: an empty borrow has no element
that can be stale, so validating it must not produce a false positive.

Pointer guards store the list, element index, and generation. They do not expose
a long-lived Debug raw pointer. Each access revalidates and then fetches the
slot from the current backing list. Code that needs to write a slot after
fallible or appending work must use store-level setters/update helpers so the
destination is resolved after the work finishes.

Operations that transfer or free ownership, such as `takeArrayList`,
`toOwnedSlice`, `clearAndFree`, and `deinit`, invalidate non-empty outstanding
borrows.

## Store Scope

The guarded list abstraction is applied to the mutable stores that have this
bug shape:

- `src/lir/LirStore.zig` growable LIR arrays, including proc specs, CF
  statements, spans, local storage, source-location side arrays, and debug
  names.
- `src/postcheck/monotype/ast.zig` `ProgramBuilder` lists.
- `src/postcheck/monotype/type.zig` type-store lists.
- `src/postcheck/monotype_lifted/ast.zig` lifted-program lists.
- `src/postcheck/lambda_mono/ast.zig` lambda-mono program lists.
- `src/postcheck/lambda_mono/type.zig` lambda-mono type-store lists.

Mutable pass code uses store APIs for reads and writes. Raw list access is kept
inside store implementations and immutable/frozen view construction, where it is
named explicitly with helpers such as `unsafeRawItemsForView`.

## Enforcement

`ci/semantic_audit.pl` rejects:

- use of `__guarded_backing` outside `GuardedList.zig`;
- direct `.items` access on guarded program storage from mutation-capable
  postcheck pass files;
- direct `.items` access on guarded `LirStore` fields from LIR pass files.

This is intentionally mechanical. Pass code should not be able to accidentally
reintroduce the old raw-borrow pattern.

## Tests

The reusable abstraction has normal unit tests for no-false-positive behavior:

- no-move append keeps span and pointer borrows valid;
- no-move reserve keeps borrows valid;
- no-move `appendSlice` keeps borrows valid;
- restore that still contains a borrowed prefix keeps the borrow valid;
- empty span validation does not trip after clear;
- non-Debug builds prove release layout and raw borrow return types.

Debug-only violations are tested by a subprocess harness:

- `src/collections/guarded_list_violation_test.zig` intentionally triggers
  definite invalidation cases;
- `zig build run-test-guarded-list-violations` runs each case and expects stderr
  to contain the guarded-list panic;
- `zig build run-test-zig` depends on that violation step.

The violation cases cover span/pointer invalidation by append, reserve,
append-slice, restore below the borrowed range, clear, and ownership transfer.

The existing historical regression tests must remain green:

- `test/wasm/issue_9801_spec_constr_realloc/`;
- `test/cli/Issue9717SpecConstrSpanInvalidation.roc`;
- the nested list-pattern coverage for the recursive type-span lowering issue.

## Success Criteria

- A Debug panic means the borrow is definitely invalid under the list contract.
- Appends that do not move backing storage do not trip existing borrows.
- Range-preserving shrink/restore does not trip prefix borrows.
- Direct raw `.items` access to guarded stores is rejected by audit in the pass
  files where this bug class has occurred.
- Stale result-location writes are replaced by setter/update APIs or by
  computing new values before resolving the destination slot.
- `ReleaseFast` storage has the same representation as `std.ArrayList(T)`, and
  release borrows are raw slices/pointers.
- LIR and postcheck tests pass with guards active.

## Related Projects

- [A Shared Cycle-Guarded Checked-Type Traversal](../small/shared-checked-type-traversal.md)
  — the sibling "make the invariant structural" project for traversals.
