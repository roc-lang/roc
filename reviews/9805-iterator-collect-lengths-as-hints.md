# PR #9805 — Treat iterator collect lengths as hints

- **Author:** eluvane · **Draft:** no · **Base:** `main`
- **Size:** +55 / −49 across 4 files

Reframes `Iter.len_if_known` / `Stream.size_hint` from a *guarantee* to an
*allocation hint*. `Iter.collect` / `Stream.collect!` still pre-reserve from
`Known(n)`, but now append through the bounds-checking `List.append` instead of
`list_append_unsafe`, and `Iter.custom` degrades a depleted `Known(0)` to
`Unknown` instead of computing `Known(0 - 1)`.

## Verdict

**The bug is real and serious** — a user-supplied `Known(n)` that under-counts
lets `list_append_unsafe` write past a `with_capacity(n)` allocation, which is
heap corruption reachable from safe Roc code. The fix works and the regression
test is well aimed.

**But the fix is defensive rather than root-cause**, and the repo's stated
standard ("workarounds are categorically forbidden"; "find and fix root causes")
argues for at least considering the other end (#1). I also found what looks like
an **incomplete fix**: two sibling producers have the same underflow (#2).
Plus one piece of unrelated scope (#3).

---

## Findings

### 1. (Design question) Making every collect pay a bounds check treats the symptom

The root cause is stated plainly in the PR's own new comment:

> `len_if_known` is a size hint exposed through public iterator constructors.

`Iter.custom(state, Known(8), advance)` lets a user *assert* a length the
compiler cannot verify, and everything downstream trusted that assertion. The
fix removes the trust everywhere it was consumed.

The cost is a capacity check per element on **every** `Iter.collect` and
`Stream.collect!`, forever — including the overwhelmingly common case where the
hint came from a compiler-owned producer (`List.iter`, ranges via
`steps_between`) and is provably exact. The old comment was explicit that this
was deliberate:

> `Known(n)` guarantees exactly n items (count-changing combinators report
> `Unknown`), so reserve up front and use the unchecked append.

That guarantee was true for every *compiler-generated* hint. It became false
only when `Iter.custom` exposed the hint slot to users.

The alternative worth weighing: fix the constructor rather than the consumers.
Options, roughly:

- **Don't let `Iter.custom` take a `Known`.** It would always report `Unknown`;
  users who want a pre-sized collect use `List.with_capacity` themselves. This
  restores the guarantee for every hint the compiler produces and costs nothing
  at runtime.
- **Give `Iter.custom` a separate, explicitly-unverified hint constructor** so
  the type distinguishes "compiler-proved length" from "user claim," and only
  the latter forces the checked append.

The counter-argument for the current approach is legitimate: `List.append`'s
capacity check is a well-predicted branch, and the compiler may hoist or
eliminate it after inlining. If measurements say that's true, the current fix is
the right trade and the PR should say so — right now it reads as if the cost
wasn't considered. Either way, this decision deserves a sentence in the PR body
rather than only appearing as a changed comment.

### 2. (Likely incomplete) `Iter.exclusive_range` and `Iter.inclusive_range` have the same underflow

The PR guards exactly one of the three `Known(l - 1)` decrements in
`Builtin.roc`:

| site | guarded by this PR? | hint source |
|---|---|---|
| `Iter.custom` (`:3038`) | **yes** — `Known(0) => Unknown` added | user-supplied |
| `Iter.exclusive_range` (`:3087`) | no | third parameter |
| `Iter.inclusive_range` (`:3126`) | no | third parameter |

Both range producers take `len_if_known` as an explicit third argument and
decrement it the same way. Their *item count* is determined by `start`/`end`, so
a caller who passes a too-small `Known` will walk the hint down to `Known(0)`
while the range keeps yielding, then evaluate `Known(0 - 1)` on an unsigned
type — the exact crash this PR fixes for `Iter.custom`.

The question is whether those two are reachable from user code. They're
associated methods on `Iter` with no visibility marker, and they're registered
in `static_dispatch_registry.zig` as compiler-owned producers
(`iter_exclusive_range` / `iter_inclusive_range`), which suggests the compiler
is the intended caller — but "intended" isn't "enforced."

If they are user-callable, the same one-line `Known(0) => Unknown` belongs on
both. If they aren't, a comment saying the hint is compiler-computed from
`steps_between` and therefore cannot underflow would document why they're
deliberately different from `Iter.custom`.

### 3. (Should split) The hot-reload test-helper change is unrelated

Removing `testingHotReloadDescriptor` and threading `try
hotReloadDescriptorForWrite(...)` through eight call sites is a good change on
its own merits — it turns a `catch unreachable` into a propagated error, so a
real Windows `SEC_RESERVE` commit failure in a test surfaces as a test failure
instead of a panic. But it has nothing to do with iterator length hints, it's a
third of the diff, and it overlaps the subject matter of the author's own
**#9795** ("Prevent hot-reload descriptor slots from overlapping image bytes").

Split it out. As-is, a reviewer who only cares about the hot-reload change has
to read a `Builtin.roc` iterator diff to find it, and vice versa. The PR body
mentions it in one trailing sentence, which understates that it's the majority
of the changed lines.

### 4. (Brittleness) `lir_inline_test.zig` shape numbers are exact equalities

```zig
.generic => shape.arg_count == 1 and
    shape.direct_call_count == 3 and
    shape.switch_count == 6 and
    shape.join_count == 9 and
    shape.jump_count == 11 and
```

Four hand-updated magic numbers pinning optimizer output exactly. Note the
inconsistency with the `.specialized` arm right above, which uses `>=` for the
same quantities — so one shape tolerates optimizer improvements and the other
breaks on them.

This is pre-existing, and the *updated comment* is genuinely better than what it
replaced (it now describes what the code does rather than the removed branch).
But since these numbers are being touched anyway, it's worth asking whether
`.generic` should relax to `<=` bounds, so a future optimizer win doesn't
present as a test failure that someone "fixes" by bumping constants without
understanding why they moved.

## Things I verified

- **The vulnerable sites are exactly the two that were fixed.** I checked every
  `list_append_unsafe` call in `Builtin.roc`. All the others
  (`Iter.join`, `intersperse`, `List.append`'s own reserve-then-write,
  `List.reverse`, the `fold_rev` collector) derive their capacity from
  `List.len()` arithmetic on data they own — none of them trusts an
  externally-supplied `len_if_known`. So the fix is complete with respect to
  *this* hazard. ✓
- **`Known(0) => Unknown` is necessary independently of the append change.**
  With the checked append in place, an over-producing custom iterator no longer
  corrupts memory — but `Known(0 - 1)` on a `U64` is still a checked-subtraction
  crash. Both halves are needed; the PR body says so and it's right. ✓
- **The regression test exercises both halves at once.**
  `Iter.custom(0.U64, Known(8), adv)` with `adv` yielding nine items walks the
  hint to `Known(0)` on the eighth item (exercising the new `Unknown`
  degradation) and appends a ninth past the reserved capacity (exercising the
  checked append). Expected `[0 … 8]` is nine items — correct. ✓
- **`Stream.collect!` got the symmetric treatment**, not just `Iter`. Easy half
  to forget. ✓
- **Comment quality.** Both replaced comments correctly describe the *new*
  invariant without referencing the old behavior, which matches the repo's
  comment rules. ✓

## Test coverage gap

One case isn't covered: an **over-reserving** hint —
`Iter.custom(0, Known(100), adv)` yielding three items. That should produce a
three-element list (with slack capacity), and nothing currently pins that the
result's *length* comes from the items rather than the hint. It's a one-line
addition next to the existing test and it guards the other direction of the same
"hint is not a guarantee" claim.
