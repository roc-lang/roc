# PR #10702 — Preserve exact produced Monotypes directly

- **Author:** rtfeldman · **Draft:** yes · **Base:** `main`
- **Size:** **+12,961 / −4,199 across 19 files** (~23k diff lines)

## Scope note — read this first

This is the largest open PR after Boxy. I could not exhaustively verify 5,038
added lines in `monotype/solve.zig` plus 3,889 in `monotype/lower.zig` plus
2,141 in `checked_artifact.zig` at the level I'd want. What I **did** do:

- Read the **entire** `design.md` diff (+739/−126) closely — that's the spec, and
  for a change of this shape it's the part that matters most.
- Fully reviewed all nine small/medium files: `LowLevel.zig`,
  `static_dispatch_registry.zig`, `cli/main.zig`, `serialize.zig`, `type.zig`,
  `lambda_solved/ast.zig`, `lambda_mono/lower.zig`, `lift.zig`,
  `spec_constr.zig`, `solved_lir_lower.zig`.
- Independently **completeness-checked** the new `ProducedTypeFlow` table
  against `Builtin.roc` signatures and the full `LowLevel` enum.
- Traced the new `produced_type_source` field through every pass that touches
  `.low_level` nodes.
- Enumerated the new tests.

Everything below is grounded in something I actually read. The two big
lowering/solving files are **not** covered and should get a second pair of eyes.

## Verdict

The architecture is right and the design.md work is genuinely excellent — this
replaces "scan the finished type graph and guess which nodes are generated" with
"the producer says what it produced." That's the direction AGENTS.md demands
("consume explicit data produced by earlier stages rather than trying to
recover, guess, reconstruct, approximate"), and the diff deletes the containment
scans rather than leaving them as a fallback, which is the part that makes it
credible.

**Blocking concerns: one design question (#1) and one unexplained behavior
change (#2).** Everything else is nits or follow-ups.

---

## Findings

### 1. (Design question — would want answered before merge) `produced_type_source` should not be an `ExprId` past the Monotype boundary

The new `low_level.produced_type_source` is an `ExprId` that is explicitly
**not a runtime operand**. design.md is emphatic: it is "carried and rewritten
alongside the node, but it is never evaluated, counted as a runtime use, or
turned into a capture."

I verified the three walkers in `lift.zig` honor that split, and they do —
deliberately and correctly:

| site | what it is | touches `produced_type_source`? |
|---|---|---|
| `lift.zig:717` `rewriteExpr` | id remapping | **yes** (added by this PR) |
| `lift.zig:1400` capture finalization | capture analysis | no |
| `lift.zig:2277` `CaptureGraphBuilder.collectExprSpan` | capture graph | no |

That asymmetry is exactly what the design asks for. But it is an invariant
enforced only by three functions independently remembering to disagree with each
other, and it creates a latent hazard:

`enclosingFunctionTypeSource` builds the node as `.{ .local = source.local }`
against `self.enclosing_function_args`. After lifting and SpecConstr, the
containing function's ABI may no longer include that local — design.md
acknowledges this directly ("Lambda Solved ... must not look up an argument
position in the node's post-lift containing function, whose ABI may have been
changed by specialization or inlining"). So the field can legitimately reference
a local that is **not live in its containing function**. That is safe only for
as long as every consumer reads exclusively `.ty` off the node and never `.local`.

**Suggestion:** collapse `produced_type_source` to the sealed type at the
Monotype → Monotype Lifted boundary, so Lifted and everything downstream carry a
`TypeId` (or a solved type var), not an `ExprId`. That would:

- make the misuse structurally impossible instead of documentation-enforced,
- delete the field from `lift.zig:717`, both `spec_constr.zig` clone paths, and
  the `serialize.zig` bounds check,
- remove the possibility of a dangling local reference surviving into LIR.

The only reason I can see for `ExprId` is that at Monotype-lowering time the
type is still an unsealed graph cell — which is a good reason *up to sealing*,
and no reason at all after it.

**Related, smaller:** `spec_constr.zig:3402` does
`(try self.cloneExprFresh(source, renames)) orelse return null` — abandoning the
entire specialization when the type-only source can't be cloned. A field the
design says is "never a runtime use" now has veto power over specialization. If
that's intended (the type info is genuinely lost, so specializing would be
wrong), say so in a comment. If it isn't, this is a silent
specialization-suppression bug that would show up only as a performance
regression. Note `Cloner` at 5757 does *not* have the `orelse return null` —
the two paths differ, which is worth a second look either way.

### 2. (Unexplained) Mint-depth cap raised from 16 to 254

design.md changes "a hard minted depth limit of 16" to "exact minted depths 1
through 254, with 255 reserved as the forced-dynamic sentinel in the serialized
`u8` metadata."

That's a 16× increase in how deep an iterator adapter chain can go before
collapsing to the dynamic fixed point, and every level is a **distinct nominal
identity with its own layout and its own specializations**. The justification
offered is entirely about representability ("the `u8` can hold it"), not about
what compile time and code size do at depth 200.

The old value 16 reads like a deliberate budget. If it was arbitrary, say so. If
254 was chosen because some real program needed more than 16, say which. As
written, the new bound is "whatever fits in the field," which is the one
justification that doesn't answer the question. This is the change in the PR most
likely to produce a compile-time blowup on a pathological input, and there's no
test pinning behavior anywhere near the new cap.

### 3. `ProducedTypeFlow` table — I checked it for completeness and correctness; it holds

I verified every entry against `src/build/roc/Builtin.roc` and
`src/canonicalize/BuiltinLowLevel.zig`. All arities and argument indices are
right, including the non-obvious ones:

- `list_append_sublist : List(item), List(item), U64, U64` → `left_arg=0,
  right_arg=1, arity=4` ✓
- `list_sublist : List(a), { start, len }` → arity **2**, not 3, because the
  range is a record ✓ (this is the entry most likely to be wrong, and it isn't)
- `list_set_unsafe`/`list_replace_unsafe`/`list_map_write_unsafe :
  List, U64, item` → `item_arg=2, arity=3` ✓
- `list_map_cast_unsafe : List(input) -> List(output)` arity 1, output taken
  from enclosing function arg 1's *return* ✓
- `list_map_extract_unsafe : List(output), U64 -> input` arity 2, item taken
  from enclosing function arg 0's *list element* ✓

I also checked every list/box op **absent** from the table to see whether any is
a real gap. None are:

- `list_first`, `list_last`, `list_split_first`, `list_split_last` —
  `rc_conformance_tests.zig:645-650` records these as having no producer
  (they lower through `list_get_unsafe`, or are written in Roc). Never reach
  `producedTypeFlow()`.
- `list_set_in_place_unsafe`, `list_append_range_within_unsafe`,
  `list_owned_unique`, `list_slack_unique`, `box_prepare_update`,
  `list_sublist_borrowed` — all introduced by **LIR passes**
  (`loop_append_promote.zig`, `box_reuse.zig`, `arc.zig`), long after Monotype.
- `list_with_capacity`, `list_len`, `list_map_can_reuse` — element type comes
  from the request or the result isn't a container. `.none` is right.

So the table is complete for everything Monotype can actually see. Good.

**Nit A:** `.list_insert` vs `.list_replace` looks like a distinction without a
difference from the table alone (`list_set` and `list_replace_unsafe` have
identical shapes). The reason only becomes visible 8,000 diff-lines later, in
`lowLevelReplaceResultNode` — `list_replace_unsafe` returns
`{ list, prev }`, not a list. Two words of doc comment on the variant would save
the next reader that trip.

**Nit B:** `function_arg` in `list_from_enclosing_function_arg_result` /
`enclosing_function_list_item` indexes the **enclosing function's** arguments,
while `list_arg`/`item_arg`/`arg` elsewhere in the same union index the
**operation's** arguments. Two index spaces, similar-looking field names, no
type distinction. Rename to `enclosing_function_arg`, or wrap in a distinct
one-field struct type. This is the kind of thing that produces an
off-by-one-index bug that type-checks fine.

**Nit C (positive):** duplicating `arity` into each variant initially looked
like a second source of truth, but both consumers use it as a *check*
(`checked_artifact.zig:2965`, `lower.zig:11925` both invariant-fail on
mismatch), and there is no pre-existing `LowLevel` arity table to conflict with.
That's the right call — keep it.

### 4. `solved_lir_lower.zig` — uninhabited short-circuit

I initially flagged the `lowerStructExprsIntoAtTypes` path as bypassing the new
check, then confirmed it doesn't: the hunk at old-line 2937 *is* inside
`lowerStructExprsIntoAtTypes`, and the box-backing early return recurses into
the same function, so the check runs on re-entry. Lists, structs, single-payload
tags, and nominal backings are all covered. No gap found.

Two things to raise:

- **Terminology.** design.md says the constructor "emits the
  **compiler-impossible continuation**"; the code emits
  `.{ .runtime_error = {} }`. If `runtime_error` surfaces a user-facing "runtime
  error" message rather than an unreachable, that's a semantic mismatch with the
  spec, and it's reachable only via a path the compiler has *proven*
  unreachable. Either rename the design term or use whatever the
  compiler-impossible statement actually is.
- **Negative-query caching.** design.md correctly requires that a negative
  inhabitation answer crossing an active recursive type edge stay uncached
  ("a different entry into that recursive component can still reach a closed
  empty union"). This is the same shape as the `.visiting → cycle_hits`
  discipline in `ExactGraphProducerAnalysis.exprProduces`
  (`checked_artifact.zig`, `.unknown`/`.visiting`/`.yes`/`.no` with
  `cycle_hits_before`), which I did read and which looks right. I did **not**
  verify the Lambda Solved memo implements the same rule. Worth confirming —
  it's the classic place this pattern gets implemented once correctly and once
  not.

### 5. `boxBackingLayoutForDirectConstruction` in `lowerFiniteCallableValueInto` is unexplained scope

`solved_lir_lower.zig:3935` gains a box-backing early return that mirrors the
existing one in `lowerStructExprsIntoAtTypes:2929`. It's a correct-looking
consistency fix, but it has nothing to do with exact produced Monotypes and
isn't mentioned in the PR description. Either call it out or split it.

### 6. The `LayoutGraphBuilder` fix is real and deserves its own mention

Removing the `knownLayoutForEquivalentNamedType` consult from
`LayoutGraphBuilder.inputForType` (`solved_lir_lower.zig:8812`) is a genuine
correctness fix, not cleanup: as the new comment says, substituting a committed
equivalent root for one edge of an *active* recursive layout graph cuts the
back-edge and yields a finite unrolling whose depth depends on commit order —
i.e. **two different layouts for the same recursive value depending on
compilation order.** That's a miscompile class, not an optimization. I confirmed
the function is still live at `8769` (the `layoutOfType` entry), so nothing went
dead.

This is buried in a 23k-line diff under a description about Monotypes. It should
be its own bullet in the PR body at minimum, and arguably its own PR with its own
regression test — I don't see one.

### 7. design.md nits

- The `IteratorKind` listing gains both `str` and `join`, but only `join` gets a
  bullet in the "fields have these meanings" list. `str` already existed in
  `type.zig:107` and was simply missing from the doc; since the doc is being
  synced anyway, give it a line.
- Inserting `join` before `forced_dynamic` shifts `forced_dynamic`'s `enum(u8)`
  value 16 → 17, changing the serialized encoding. Correctly covered by the
  `FORMAT_VERSION` 10 → 12 bump. Good catch by whoever did it — but the version
  comment block documents 12/11/10 as three separate reasons, which means three
  format-affecting changes rode in on one PR. Fine, just noting the density.
- The "Exact Produced Monotypes" section is ~460 lines of dense prose with no
  subheadings. It is the authoritative spec for the largest subsystem change in
  the tree and it will be read under pressure by someone debugging a
  miscompile. Please add `#####` subheadings — producer identity / content
  addressing / request substitution / control flow joins / result flow columns
  are all clearly separable, and right now finding any one of them means
  scanning the whole thing.

### 8. Testing

31 new tests, and they're well aimed rather than decorative — the eval tests in
particular hit precisely the scenarios the design calls out: the compiler-owned
`from_interpolation` protocol (both an inline block-local target *and* one bound
by reference to a top-level `join_parts`, which is the exact aliasing case
design.md singles out), two independent `Iter(I64)` parameters that must keep
distinct identities, callables stored into lists via `List.concat`/`append`, and
iterators returned through a record field. That's the right set.

Gaps I'd want filled:

- Nothing near the new 254 mint-depth cap, or at the forced-dynamic transition
  under the new bound (see #2).
- No regression test for the `LayoutGraphBuilder` recursive-layout fix (#6) — and
  that one is order-dependent, so it will not reproduce reliably once it
  regresses.
- The new counter test only spot-checks three indices
  (`graph[16]`, `graph[20]`, `graph_identity[1]`) of the 33 counters. Fine as a
  smoke test; just don't mistake it for coverage of the counters themselves.

### 9. Reviewability

Even granting that this change is inherently cross-cutting, `monotype/solve.zig`
(+5,038/−1,030) and `monotype/lower.zig` (+3,889/−2,448) in one commit is past
what anyone can review with confidence. If any of these can be landed
separately ahead of the main change, they would each be independently
reviewable and independently revertable:

- the `ProducedTypeFlow` table + its `LowLevel.zig` home (self-contained, and I
  verified it in isolation),
- the `LayoutGraphBuilder` recursive-layout fix (#6),
- the `expr_is_uninhabited` column + its `solved_lir_lower.zig` consumers,
- the `boxBackingLayoutForDirectConstruction` consistency fix (#5).

That's ~600 lines peeled off the front, and it would let the remaining review
concentrate on the graph identity work, which is where the risk actually is.
