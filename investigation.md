# Iter Branch Performance Investigation

This branch routes more `roc run` work through the LIR interpreter, so interpreter
costs that used to be minor became visible on real programs. The clearest local
stress case was `aoc_day2.roc`: it performs many small calls, branches, list and
string operations, and reference-counting statements.

The investigation found several independent costs. The biggest were repeated
per-call setup work in the interpreter and repeated layout walks while executing
explicit LIR reference-counting statements.

## 1. Switch Branches Re-entered The Interpreter Recursively

### What I Saw

`execStmtChain` already had an iterative shape:

```zig
var current = start_stmt;
while (true) {
    const stmt = self.store.getCFStmt(current);
    // ...
}
```

But `switch_stmt` selected a branch and then called `execStmtChain` recursively
for that branch.

That meant normal LIR branching created extra Zig calls. Branch-heavy programs
could grow the native stack and pay call overhead even though a switch is just
control flow inside the same LIR proc.

### What Changed

The switch case now assigns the selected branch to `current` and stays inside the
same interpreter loop.

### Why It Helped

This removes one Zig call per switch branch and avoids native stack growth from
ordinary LIR control flow. It is a direct control-flow fix with no behavior
change.

## 2. Call Frames Were Sized To The Whole Program

### What I Saw

Every LIR proc call allocated a `LocalSlot` array with one slot for every local
in the entire LIR store. A tiny hot proc paid for all locals in the whole
program.

That was pathological for `aoc_day2.roc`, where small helper procs are called
many times while processing input.

### What Changed

LIR procs now carry `frame_locals`: the exact local ids that proc can touch.
LIR lowering records those ids while lowering each proc and stores a sorted
`LocalSpan` on the proc.

The interpreter builds one `FramePlan` per proc at startup. A frame now allocates
slots only for the locals in that proc's plan.

### Why It Helped

Per-call frame allocation and initialization is now proportional to the called
proc, not the whole compiled program.

The old whole-program array allowed direct indexing by `LocalId`. The new compact
frame uses a sorted local list and binary search. That lookup cost is much
smaller than repeatedly allocating and initializing whole-program-sized frames in
hot call paths.

## 3. Frame Slot Arrays Are Reused

### What I Saw

After frames were narrowed to per-proc locals, hot procs could still allocate a
fresh slot array on each call.

### What Changed

Each `FramePlan` owns a free list of slot arrays for that proc. When a call
returns, the slot array goes back to the plan. The next call to the same proc can
reuse it.

### Why It Helped

Hot calls avoid repeated allocator traffic. The slots are still reset on entry,
but their backing memory can be reused across calls.

## 4. Join Points Were Rediscovered On Every Call

### What I Saw

Each interpreter frame used to build a hash map of join points by walking the
proc body. That happened on every call.

Join points are part of the final LIR control-flow shape. They do not change
between calls, so rebuilding that map per frame was pure overhead.

### What Changed

After ARC insertion finishes rewriting control flow, it walks each final proc
body once and stores the final join points in LIR.

The LIR store and LIR image include a flat join-point array. The interpreter
reads each proc's join-point span into its `FramePlan`. A `jump` now looks up the
target in that already-built list.

### Why It Helped

This removes one control-flow traversal and one hash-map build from every proc
call. It also makes the data flow cleaner: the interpreter consumes explicit LIR
data instead of rediscovering the proc's control-flow targets.

## 5. Reference-Counting Plans Were Recomputed Too Often

### What I Saw

LIR already carries explicit `incref`, `decref`, and `free` statements. That part
was correct. The interpreter was still doing too much work while executing those
statements.

For each RC statement, the interpreter repeatedly asked layout questions such as:

- Does this layout contain refcounted values?
- What should this operation do for this layout?
- Does this struct field need child RC work?
- Does this tag payload need child RC work?
- Does this boxed or list element need child RC work?

Those answers are deterministic for a given layout and operation, but they were
being recomputed in hot paths.

### What Changed

The interpreter now caches:

- whether each layout contains refcounted values;
- RC plans for layout/op pairs;
- struct-field child RC plans;
- tag-variant child RC plans.

The caches are populated from exact layout identities and operation identities.
They are also pre-sized from layout-store counts to avoid repeated hash-map
growth in hot RC paths.

### Why It Helped

RC-heavy programs stop recursively walking the same layout data for repeated
operations. This matters for strings, lists, structs, tags, boxes, and erased
callables.

The interpreter still follows the explicit LIR RC statements. The caches only
speed up executing those statements.

## 6. Box And List Paths Use The Same RC Presence Cache

### What I Saw

Some interpreter paths for boxed values, lists, and erased-callable support
called generic layout helpers to answer whether the contained value needed child
RC work.

That duplicated work already answered by the interpreter's RC presence cache.

### What Changed

Those paths now route through the interpreter's cached `layoutContainsRc` logic.

### Why It Helped

This is a smaller win than frame narrowing or join-point storage, but it keeps
allocation and conversion paths from doing extra recursive layout walks.

## 7. Interpreter Shim Cache Names Include The Shim Bytes

### What I Saw

`roc run` links an interpreter shim. Reusing linked output is useful, but the
cache has to change when the embedded shim changes.

### What Changed

The shim cache name includes a digest of the embedded shim bytes.

### Why It Helped

When the shim is unchanged, `roc run` can avoid extra extraction and linking
work. When the shim changes, the cache name changes too, so stale linked output
is not reused.

This helps startup/link churn. It is separate from the interpreter inner-loop
fixes above.

## Summary

The largest improvement came from per-proc frame locals. The second major win was
moving join-point storage into final LIR so proc calls no longer rebuild that
data. RC-plan caching then removed repeated layout walks from hot RC paths.

The switch change was smaller but very clean: LIR branch execution now stays
inside the existing interpreter loop. The shim cache change helps `roc run`
startup and link behavior rather than the inner interpreter loop.

Together these changes make the LIR interpreter do substantially less repeated
work in branch-heavy, call-heavy, RC-heavy programs like `aoc_day2.roc`.
