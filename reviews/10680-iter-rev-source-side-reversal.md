# PR #10680 — Replace `Iter.rev` with source-side reversal: List/Dict/Set `iter_rev` and descending ranges

- **Author:** rtfeldman · **Draft:** yes · **Base:** `main`
- **Size:** +1,584 / −211 across 17 files

## Verdict

The core idea is right and well argued — the PR body's reasoning about why an
`Iter` can't be double-ended (a PRNG or Collatz walk has no inverse, so the
capability could never be total) is the correct justification, and moving
reversal to the source is the right consequence.

**Two blocking findings:**

1. **`Dict.iter` / `Dict.iter_rev` / `Set.iter` / `Set.iter_rev` are never
   actually registered as iterator producers** — the enum members and all the
   downstream handling exist, but nothing can ever produce them. This
   contradicts the PR body's central claim about registration being
   load-bearing. (Finding #1)
2. **`Iter.take_last` and `Iter.drop_last` are removed** without being mentioned
   anywhere in the title or body, taking two tests with them — including the
   regression repro for a closed issue. Neither method depended on `Iter.rev`.
   (Finding #2)

Everything else I checked holds up, including the dev-backend register fix,
which I verified rather than took on faith.

---

## Findings

### 1. (Blocking) Dict/Set iterator producers are unreachable — the registration the PR says is load-bearing was never wired up

The PR body states:

> Every new iterator is registered as an iterator producer with its own
> representation kind. That registration is load-bearing rather than an
> optimization: a plain procedure reached through method-syntax dispatch takes
> the sealed public-interface path, so `dict.iter()` would step through the
> public `Iter` boundary once per item and lose adapter fusion.

The diff adds, for Dict and Set:

- `IteratorProcedureId.{dict_iter, dict_iter_rev, set_iter, set_iter_rev}`
  (`static_dispatch_registry.zig:152-156`)
- those four members in the producer-classification switch (`:190-194`)
- `IteratorKind.{dict, dict_rev, set, set_rev}` in **both**
  `check/const_store.zig` and `postcheck/monotype/type.zig`
- full lowering support in `monotype/lower.zig` mapping
  `.dict_iter → .dict`, `.dict_iter_rev → .dict_rev`, etc., with an arity
  invariant and "the receiver is the single component" handling
- corresponding arms in `monotype/solve.zig`

What it does **not** add is any entry in `iteratorProcedureForDef`, the one
function that turns a Builtin definition into an `IteratorProcedureId`. I pulled
the file at the PR head (`b844688`) to be sure. The exact-name table gains
exactly three entries:

```zig
.{ "Builtin.List.iter_rev", .list_iter_rev },
.{ "Builtin.Iter.descending_exclusive_range", .iter_descending_exclusive_range },
.{ "Builtin.Iter.descending_inclusive_range", .iter_descending_inclusive_range },
```

and the numeric loop gains `down_to` / `down_until` / `to` / `until`. There is no
`"Builtin.Dict.iter"`, no `"Builtin.Set.iter_rev"`, and no Dict/Set analogue of
the numeric-type loop. So `iteratorProcedureForDef` can never return any of the
four Dict/Set ids, and by extension `IteratorKind.dict`/`dict_rev`/`set`/`set_rev`
are never minted.

The PR's own test confirms this, and its comment reads as if the outcome were
intended:

> The `Dict.iter` / `Set.iter_rev` procedures themselves may survive as a call:
> their bodies unwrap the nominal and hand back the backing list, which happens
> once when the iterator is built rather than once per step.

That describes Dict/Set iteration working *because the inner `List.iter` is the
registered producer and gets inlined* — i.e. exactly the "compiler-side
projection into their backing fields" that the PR body says was rejected. The
test passes because it only asserts `Iter.next` / `iter_from_step` are
unreachable, which the inner list producer already guarantees.

So one of these is true, and the PR should say which:

- **(a) The wiring was forgotten.** Add the four table entries. The
  `IteratorKind`s then do what the body describes.
- **(b) The design changed to piggyback on the list producer** and the body is
  stale. Then `dict_iter`/`dict_iter_rev`/`set_iter`/`set_iter_rev`, the four
  `IteratorKind`s in two files, and their `lower.zig`/`solve.zig` arms are all
  dead code and should come out — along with cache-version comment 52, which
  claims "Checked iterator identity includes Dict/Set iteration."

Option (b) also weakens the stated rationale for a separate `dict` kind ("rather
than reusing the list kinds through a compiler-side projection into their
backing fields, which would put their internal layout in the compiler") —
because as shipped, that projection is precisely what happens, just via
inlining.

### 2. (Blocking) `Iter.take_last` and `Iter.drop_last` are removed with no mention, and take two tests with them

Neither the title nor the body mentions removing them. The diff deletes:

- `Iter.take_last : Iter(item), U64 -> Iter(item)`
- `Iter.drop_last : Iter(item), U64 -> Iter(item)`
- `test/cli/issue_10178_take_last_from_iter.roc` — **the regression repro for
  closed issue #10178** — plus its `parallel_cli_runner.zig` entry
- `test/snapshots/repl/range_take_drop_last.md`

Crucially, **neither method depended on `Iter.rev`.** Their `Known`-length fast
paths were `Iter.drop_first(iterator, len - n)` and
`Iter.take_first(iterator, len - n)`; the `Unknown` paths were
`List.iter(List.take_last(Iter.fold(...), n))`. Not a `rev` call in either. So
removing `rev` did not force this — it's an independent API removal riding along.

Consequences:

- `Iter.take_last`/`drop_last` over a **range** now have no replacement.
  `List.take_last` doesn't help: the deleted REPL snapshot tested
  `Iter.take_last(5.I64..<10, 2)` specifically because a range's elements were
  never in memory. Users now have to recompute the bound by hand.
- Issue #10178's repro is gone. If the underlying lowering bug can recur through
  any other `Known`-length adapter, nothing catches it.

If the removal is intended, it belongs in the title and body as a breaking
change with a migration note, and #10178 deserves either a reworked repro or an
explicit "no longer reachable" note. If it isn't intended, restore both methods
— they cost nothing now that `rev` is gone.

### 3. (Should mention) `Num.X.to` / `Num.X.until` gain producer registration — an undisclosed fix

The numeric loop adds four names, not two:

```zig
if (Ident.textEql(text, "Builtin.Num." ++ numeric ++ ".down_to"))    return .numeric_down_to;
if (Ident.textEql(text, "Builtin.Num." ++ numeric ++ ".down_until")) return .numeric_down_until;
if (Ident.textEql(text, "Builtin.Num." ++ numeric ++ ".to"))         return .numeric_to;
if (Ident.textEql(text, "Builtin.Num." ++ numeric ++ ".until"))      return .numeric_until;
```

`to` and `until` are pre-existing. Registering them now means that by the PR's
own argument, `n.to(m)` reached through method dispatch was **previously losing
adapter fusion** while `Num.U64.range_inclusive(n, m)` was not. That's a
user-visible performance fix on existing code, and it's only visible in this PR
as a cache-version comment ("53: Checked iterator identity includes the numeric
to/until ranges"). Worth a sentence in the body — it's good news.

### 4. Dev-backend register fix — verified correct

I chased the thing a reviewer should worry about here: `ensureInGeneralReg`
returns the operand's *home* register unchanged in its `.general_reg` case
(`LirCodeGen.zig:16521`), so `freeGeneral` on its result would be wrong if the
value were a live local in a register.

It can't be. `emitValueLocal` (`:1536`) routes through `stabilize` (`:1542`),
which spills any bare `.general_reg` to the stack and frees it before returning.
So `cond_loc` is always `.stack`, `ensureInGeneralReg` always takes the
`allocTempGeneral()` path (`:16535`), and `cond_reg` is unconditionally a
temporary. Freeing it is correct.

Placement is also fine: `emitJumpIfNotEqual` (`:15805`) emits a bare `b.ne` /
`jne` on both arches and allocates nothing, so the flags set by `emitCmpImm`
survive the intervening `freeGeneral`. And the pattern matches the existing site
at `:1648-1655`.

Two notes:

- The comment says "The compare is the last use." That's true, but the *reason*
  it's safe is that `emitValueLocal` guarantees a temp — which is a much stronger
  and more durable justification. Consider saying that instead; "last use" invites
  the exact wrong question.
- The PR body says this "reproduced on `expect 1 == 1` and predates this work."
  There's no test. A dev-backend case with a dozen `expect`s in one procedure
  would be cheap and would pin the fix. Right now nothing stops it regressing.
  This is also a good candidate for its own PR — it's unrelated to iterators and
  currently blocked behind a draft.

### 5. `iter_rev`'s no-allocation claim is tested; the implementation looks right

```roc
make = |remaining|
    iter_from_step(
        Known(remaining),
        || if remaining == 0 { Done }
           else { One({ item: list_get_unsafe(list, remaining - 1), rest: make(remaining - 1) }) },
    )
make(List.len(list))
```

The recursive `make(remaining - 1)` sits inside the step thunk, so each `next`
builds exactly one successor rather than the whole chain up front — the thing
that would otherwise blow the stack on a long list. `Known(remaining)` staying
exact as it counts down is the property the doc comment calls out, and it's
correct: `remaining` is simultaneously the count left and the index just past
the next item.

`test/alloc-count/app.roc` asserts zero allocations for both the fold and the
`find_last` built on it, which is the right way to pin this — it would catch a
regression that reintroduced closure boxing.

**Small nit on the test:** `expect found == List.last(bytes)` only holds because
the last byte of the input's UTF-8 happens to be `> 0`. It's an assertion whose
truth depends on the fixture's input rather than on `find_last`'s contract. Not
wrong, just fragile to a future change of the harness input.

### 6. Cache versioning is handled correctly

`IteratorKind` gains 7 members inserted mid-enum in **two** files
(`check/const_store.zig:144-148,154-155` and `postcheck/monotype/type.zig`), which
shifts every subsequent discriminant. `CACHE_VERSION` goes 50 → 53 and the
`MODULE_ENV_VERSION_HASH` golden bytes are updated. I checked whether the
Monotype specialization cache (`postcheck/monotype/serialize.zig`,
`FORMAT_VERSION`) also needed a bump and it does not — `IteratorKind` does not
appear in that file, and `IteratorKind` only reaches `lower.zig`/`solve.zig`
in-memory. Good.

Three version numbers for one PR (51, 52, 53) reads like they were added
incrementally during development. Harmless, and arguably better documentation
than one lumped bump.

### 7. Test coverage is genuinely thorough

The eval tests cover the edge cases that actually matter for descending ranges:
`down_to` reaching zero on an **unsigned** type, `down_to` reaching the
**signed minimum**, single-point ranges, start-below-end, `down_until`
exclusivity, exact `len_if_known` for both, and composition with
`keep_if`/`step_by`/`take_first`. Plus a `Mixed` nominal whose `iter` returns
`iter()` from one branch and `iter_rev()` from the other — that's the case where
two distinct minted identities meet at a control-flow join, and it's the right
thing to test. Dict/Set insertion-order tests are present too.

### 8. Nits

- **Docs:** `docs/langref/loops.md` gains a "Looping backwards" section for
  `List.iter_rev` but says nothing about `down_to`/`down_until`, even though the
  same page documents `1..<5` ranges in `for`. A descending-range example
  belongs right next to it — it's the more discoverable half of this PR.
- **PR body typo:** "`Set` gains the `fold` it was missing — `for x in dict` was
  previously a missing-method error" — should be `for x in set`.
- **`Builtin.roc` size:** `down_to`/`down_until` are hand-written per numeric
  type across 11 types (~22 near-identical blocks). That matches the existing
  `to`/`until` pattern so it's not a regression, but this file is now carrying a
  lot of mechanical duplication. Not this PR's problem to solve; worth noting
  the trend.
- **Stale comment correctly updated:** `Builtin.roc:3067`'s reference to
  "`Iter.take_last`/`drop_last` rely on" `Known(0)` is trimmed to just
  "`Known(0)`". I checked the PR head for any other surviving reference to the
  three removed methods and found none. Clean removal — the problem is the
  disclosure, not the execution.
