# PR #10366 — Experiment: `?` re-raises errors (open-row widening at the desugar)

- **Author:** jaredramirez (Jared Ramirez) · **Draft:** yes (explicitly an experiment) · **Base:** `main`
- **Size:** +571 / −575 across 34 files — **net −118 lines including new tests**

Makes `?` re-raise rather than pass through: the desugared `Err` branch wraps its
payload in a new `e_reraise_err` CIR node whose type is the operand's tags at a
fresh open extension. Deletes the hosted-only special case (`RedirectRule.hosted_try_question_widening`,
9 checker functions, 7 monotype-lowering functions) that did the same thing for
hosted callees only.

## Verdict

**This is the best-argued PR in the open set**, and the process is exemplary:
the rule is declared in design.md *before* the code, the replaced rule is
deleted from the same document, and the Rewrite Inventory **shrinks** — a
`RedirectRule` member is removed rather than added. AGENTS.md's
solver-mutating-rewrite protocol is usually invoked to justify adding a rewrite;
running it in reverse to retire one is exactly what it's for.

The mechanism is also convincingly more general than what it replaces: the same
node handles hosted and non-hosted callees, so 16 functions of special-casing
disappear and hosted specialization gets an *invariant* ("requests always arrive
at the declared ABI type") where it previously had an adapter.

**One design issue is disqualifying as a language rule in its current form**
(#1): the typing of `?` depends on solver *scheduling*, not on the program. The
PR body flags it and proposes the fix; I'd make that fix a condition of moving
out of experiment status. Two smaller gaps below.

---

## Findings

### 1. (Design-blocking) `?`'s typing depends on check order, which is not a property of the program

From the PR body, and stated in the design.md rule:

> The widening applies when the operand's row is *resolved closed at the point
> the node is checked*. A row that is still flexible there shares the var
> (today's behavior).

and in the code:

```zig
if (!try self.gatherClosedTagRow(operand_var)) {
    _ = try self.unify(expr_var, operand_var, env);
    return;
}
```

So whether `?` widens is decided by what the solver happens to have resolved by
the time it reaches that node. That means a program's acceptance can change
under def reordering — and, more sharply, **mutually recursive definitions have
no source order to appeal to**:

```roc
f = |x| { y = g(x)?  ; ... }   # g's error row still flex here → pass-through
g = |x| { y = f(x)?  ; ... }   # f's row now resolved closed → widens
```

Whichever member the solver visits first gets pass-through semantics and the
other gets widening. That's a typing rule you cannot state without referring to
the implementation's traversal order, which is the definition of a heuristic in
AGENTS.md terms — and it lives in checking, not parsing or error reporting.

The PR body already names the remedy:

> could be moved to a deferred constraint like literal defaulting if it ever bites

I'd argue it bites at the moment it's written down, not later: the difference
between "this rule is order-dependent" and "this rule is order-dependent *and
someone has hit it*" is only luck. Literal defaulting is the right precedent, and
it already has the machinery (a boundary at which every relevant row is final).

For an experiment whose purpose is to compare against polarity, shipping the
order-sensitive version is fine — but the comparison should be honest that
polarity's rule is order-independent and this one isn't, which the "Cons" section
does say. If re-raise wins the bake-off, deferring the widening should be part of
landing it.

### 2. (Gap in the rule) The empty error row falls to bare `flex`

```zig
if (tags_slice.len == 0) {
    // `?` on a `Try` whose error row is empty: the Err branch is
    // unreachable and the re-raise imposes nothing on the return row.
    try self.unifyWith(expr_var, .{ .flex = Flex.init() }, env);
    return;
}
```

The reasoning is right — `Try(v, [])` has no reachable `Err` branch — but a bare
flexible var unifies with *anything*, including a non-tag-union return type. So

```roc
f : I64 -> Try(I64, [])
g : I64 -> Try(I64, Str)
g = |x| Ok(f(x)?)
```

would now typecheck, where before the rows had to agree. That may well be
harmless (the branch is dead), but it's a real widening of what `?` accepts and
the design.md rule doesn't mention it: the rule enumerates the pass-through cases
(flexible, rigid-open, non-tag-union, nominal, poisoned) and the widening case,
and the empty-row case is neither.

Either state it in the rule ("an empty error row imposes no constraint on the
return row, because the `Err` branch is uninhabited") with a test pin, or
constrain it to an open tag union rather than a bare flex, which keeps `Try(_, Str)`
rejected.

### 3. (Question) "Shallow" widening shares payload vars rather than copying them

`gatherClosedTagRow` reuses the operand's payload ranges verbatim:

```zig
for (names, args) |name, tag_args| {
    try self.scratch_tags.append(.{ .name = name, .args = tag_args });
}
```

so the fresh row's tags point at the *same* payload vars as the callee's row. The
design says:

> Widening is shallow: occurrences of the row inside tag payloads keep the
> operand's type.

Sharing achieves that, but it also means unifying the re-raise node against the
enclosing return row will unify the **callee's payload vars** with the caller's.
The row is not mutated; the payloads underneath it can be. For

```roc
f : I64 -> Try(I64, [BadInput(Str)])
g : I64 -> Try(I64, [BadInput(a), TooBig])
```

the caller's `a` gets pinned to `Str` — probably desirable. But it's a different
statement from "keep the operand's type," and it means the callee's instantiated
type *is* reachable from the widening after all, which slightly undercuts the
"the operand's own row is never mutated" claim in the design text.

Worth one sentence in the rule saying payload vars are shared (not copied) and
therefore participate in the outer unification. If copying is what was intended,
that's a code change.

### 4. Mechanics I verified and found correct

- **The tag sort is a verbatim copy of the existing convention** at
  `Check.zig:11446` — same `std.mem.order` over `getText(name)`. So the row is
  canonically ordered the same way every other row in the checker is, and this
  isn't a new text-comparison pattern in `src/check/`. ✓
- **The scratch-buffer hazard is handled.** The neighbouring code at `:11452`
  carries an explicit warning that `tags_slice` points into `scratch_tags` and
  that generating the ext can recurse and reallocate it. The new code does
  `appendTags(tags_slice)` *before* `self.fresh(env, region)`, in that order — so
  it follows the same discipline. Easy one to get wrong; it wasn't. ✓
- **`gatherClosedTagRow` walks with an `IterationGuard`**, so a cyclic `ext`
  chain can't hang the checker. ✓
- **No dangling slices in the walk.** `names`/`args` come from
  `self.types.getTagsSlice(...)` (the types store) while the loop appends to
  `self.scratch_tags` (a different buffer), so the reallocation can't invalidate
  the iteration. ✓
- **`scratch_tags` is restored on every path** via
  `defer self.scratch_tags.clearFrom(scratch_tags_top)`, including the early
  pass-through return. ✓
- **Serialization is handled.** A new CIR node means new layouts; the PR bumps
  both `Constants.CACHE_VERSION` and the checked-artifact
  `serialized_layout_version` with fresh golden hashes, and adds the
  `node_store_test.zig` round-trip. The node is threaded through
  `DependencyGraph`, `RocEmitter`, `lsp/cir_visitor`, `lsp/scope_map`,
  `hoist_roots`, and `markHoistInvalidatedExpr` — I checked the file list for
  missed visitors and didn't find one. ✓

### 5. On the design trade itself

Since this exists to be compared against #10434 (polarity), the substantive
opinion:

**The strongest argument for re-raise isn't in the Pros list as stated.** It's
that it *retires an existing special case* rather than adding a mechanism.
Polarity adds a type-system feature; re-raise removes 16 functions and a
`RedirectRule` while covering a strict superset of what they covered. A change
that makes the compiler smaller while accepting more programs has a very
different risk profile from one that makes it larger.

**The strongest argument against is the one the Cons section undersells.** "Only
helps where a `?` appears" is framed as a coverage gap, but the tail-position
case is the one users will hit second:

```roc
g = |x| if x > 100 Err(TooBig) else f(x)   # still a type error
```

The suggested fix, `Ok(f(x)?)`, is described as "one character of ceremony" —
it's actually four, and more importantly it requires the user to know that
wrapping-then-unwrapping is the idiom. Someone who just learned that `?`
composes error rows will reasonably expect the direct return to work too, and
the error message they get won't mention `?` at all. If re-raise is adopted,
that diagnostic should suggest `Ok(… ?)` explicitly — otherwise the feature
teaches an inconsistency.

**The "two-way door" argument is correct and is the decisive one for an
experiment.** Re-raise only accepts more programs at `?` sites, so it can be
layered under polarity later; polarity cannot be removed once code depends on
it. Landing re-raise first and measuring how many remaining complaints are
*not* `?`-shaped is a better sequencing than choosing now.

### 6. Process nits

- The PR body ends with a `🤖 Generated with [Claude Code]` trailer and a
  `claude.ai/code/session_…` link — neither belongs in a PR description per repo
  convention. (Same on #10290.)
- The body reports three test failures that also fail on clean `main` (json
  decoder allocation count, fx boxed-erased host boundary ×2). Third PR in this
  set to independently rediscover pre-existing platform-test failures (#10043 and
  #10290 report overlapping ones). Somebody should file them; right now the same
  bugs are being re-diagnosed by every contributor who runs the full suite.
- **Coordinate with #10434.** These two drafts are alternative solutions to the
  same problem, by the same author, both open. Whichever way the decision goes,
  the other should be closed with a note pointing at the winner, so a future
  reader doesn't find two live designs for one problem.
