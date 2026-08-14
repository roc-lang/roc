# PR #10671 — Demote dispatch-chain vars to their outer receiver's rank before boundary defaulting

- **Author:** Anton-4 · **Draft:** no · **Base:** `main`
- **Size:** +187 / −2 across 4 files (design.md, `Check.zig`, integration tests, 1 snapshot)

Fixes `top_str.split_on(",").get(0)` failing at module finalize because the
index literal `0` committed its `Dec` default at the inner def's generalization
boundary, before the chain's still-flex outer receiver could pin it to `U64`.

## Verdict

**The diagnosis is right, the fix is the right shape, and the process
compliance is exemplary** — this is exactly what AGENTS.md's "Solver-Mutating
Rewrites" clause asks for: the rule is declared in design.md first, the rewrite
is named for it, the Rewrite Inventory is updated in the same change, and both
the accepted and rejected sides are pinned by tests. Reviewers of checker
changes should hold this up as the template.

**One question I'd want answered before merge** (finding #1): the pass is gated
behind a literal-defaulting candidacy check, but the rule it implements is
stated as unconditional rank discipline. Everything else is nits, and I
disproved my own two biggest concerns by reading the code (both written up
below, because they're the things a reviewer will worry about).

---

## Findings

### 1. (Question) The demotion is gated on `has_candidate`, but the declared rule isn't conditional

`demoteOuterRootedDispatchChains` is called from inside
`defaultLiteralsAtGeneralizationBoundaryMultiRoot`, **after** its early return:

```zig
    if (!has_candidate) return;

    try self.demoteOuterRootedDispatchChains(pool_vars, rank, env);
```

`has_candidate` is true only if some var at *exactly* this rank is flex **and**
`varLiteralKind(...) != null` (`Check.zig:19601-19606`). So a boundary with an
outer-rooted dispatch chain but no literal in it never runs the pass.

That's inconsistent with how design.md states the rule:

> Before a generalization boundary defaults anything, every still-flex var
> reachable from the constraint signatures of an outer-owned flex receiver in
> this rank's pool is demoted to that receiver's rank.
>
> Demotion is exactly the min-ranking an eagerly-fired dispatch would have
> performed through ordinary unification.

If the justification is "eager unification would have min-ranked these," that
argument does not depend on a literal being present. And the consequence of
skipping demotion isn't only "no defaulting happens" — it's that the chain's
vars stay at this rank and get **generalized here**. A generalized var is
instantiated per use, which is precisely the wrong thing for a var that a
single pending outer dispatch will later pin once.

Concretely: `|_x| top_str.split_on(",").len()` has an outer-rooted chain and no
literal argument. Does its intermediate `List(Str)` var get generalized at the
inner boundary today? If yes, is that harmless only because the chain happens to
resolve to concrete types, or is there a latent second bug here?

Two clean resolutions:

- **If the rule really is unconditional:** hoist the call above the
  `has_candidate` return (or out of the defaulting function entirely, into the
  boundary driver). The name and doc comment already describe a rank-discipline
  pass, not a defaulting helper — its current home is the odd part.
- **If it really is only about defaulting:** narrow design.md to say so, and
  say why generalizing an outer-rooted chain var at the inner boundary is safe.

Right now the code is narrower than the declared rule, which is the failure mode
the AGENTS.md clause exists to prevent ("indistinguishable at review time from a
change to the language's typing rules").

### 2. Scratch-buffer safety — I checked both maps; both are fine, but the doc comment misstates why

The function reuses two `Self`-owned scratch maps, which is the first thing to
audit. My conclusions:

- **`literal_defaulting_seen_roots`** — cleared at the top of this function, and
  its only other user clears it at each round's gather (`Check.zig:19078`).
  Safe both directions. ✓
- **`boundary_leak_vars`** — the doc comment says it is "cleared by each leak
  check," implying this function depends on a *later* user to clean up. That's
  not what makes it safe. What makes it safe is that
  `collectConstraintSignatureReachable` itself begins with
  `out.clearRetainingCapacity()` (`Check.zig:19501`). That single line rules out
  **both** hazards I went looking for:
  1. stale entries left by a previous `boundaryDefaultLeaksIntoSignature` call
     being demoted here, and
  2. accumulation *across loop iterations*, where root A's chain vars would
     still be in the map while root B is processed and would get demoted to B's
     rank despite not being reachable from B (a real over-demotion, since the
     `<=` guard only blocks raising).

  Neither can happen. But the safety argument in the comment points at the wrong
  mechanism, and if someone later "optimizes" the redundant-looking clear out of
  `collectConstraintSignatureReachable`, hazard (2) becomes live and silent.
  Please state the actual reason: *the collector clears its output.*

I also confirmed the iteration is sound: `setDescRank` and `addVarToRank` don't
touch `boundary_leak_vars`, so the `keyIterator()` isn't invalidated mid-walk.
And `pool_vars` (this rank's pool) isn't mutated during the scan, because
`addVarToRank` targets `receiver_rank`, which the `>= rank` guard proves is a
strictly outer pool.

### 3. Rank-only mutation — correctly scoped, correctly excluded from `RedirectRule`

The rewrite touches `setDescRank` + `addVarToRank` only. It does not call
`setVarContent`, `dangerousSetVarDesc`, or `dangerousSetVarRedirect`, so it
correctly needs no `RedirectRule` member — and the Rewrite Inventory entry says
exactly that ("It rewrites rank metadata only; no descriptor content, no
redirect"). That's the right classification.

The demoted var is added to the outer pool but not removed from this one. That's
fine because every consumer re-checks `desc.rank` (`Check.zig:19606`:
`if (resolved.desc.rank != rank) continue;`), so a stale pool entry is inert.
Worth one clause in the comment, since "added to a pool it isn't removed from"
looks like a leak to a fresh reader.

### 4. Termination argument holds

`if (@intFromEnum(chain_resolved.desc.rank) <= @intFromEnum(receiver_rank)) continue;`
means ranks only ever lower, and `rank`/`.generalized` are both excluded as
roots, so each var can be demoted at most (rank − 1) times across the whole
pass. Combined with the per-root dedup via `literal_defaulting_seen_roots`, the
single pass terminates and is order-independent for vars genuinely reachable
from multiple roots — as design.md claims. I have no concern here.

### 5. (Nit) design.md/comment names a function the code doesn't call

The doc comment says "`collectReachableVars` recurses through nested constraint
signatures, so a multi-hop chain ... is walked whole from its root." The code
calls `collectConstraintSignatureReachable`, which filters out `from_literal`
constraints *before* delegating to `collectReachableVars`. That filtering is
load-bearing — it's the same predicate as `rangeHasNonLiteralConstraint`, and
the two agreeing is what keeps a pure literal from being treated as a chain
root. Naming the outer function would make that connection visible.

### 6. (Nit) Performance at every boundary

The pass runs at every generalization boundary that has a literal candidate, and
for each it scans the full rank pool with a `resolveVar` +
`rangeHasNonLiteralConstraint` per var, then does a full reachability recursion
per qualifying root. Nested closures produce many boundaries, and this repo
already has known super-linear behavior in finalization. Probably fine —
`rangeHasNonLiteralConstraint` short-circuits and outer-owned flex receivers are
rare — but it's worth a counter or a measurement on a large module before this
lands, rather than after someone notices.

### 7. Testing

Four tests, and the pairing is the right one: two accepted cases (weak
top-level root, and an enclosing-frame root that is outer-but-not-outermost —
good, those exercise different rank arithmetic) and two guard cases proving
weak-literal semantics are untouched (a chainless weak literal still pins to one
module-wide type; a second use at a different type is still a mismatch). That
second guard is the important one, because the easiest way to get this rewrite
wrong is to accidentally widen weak values into RFC-0010 generalized numerals,
and the test says explicitly that the exception "was deliberately not adopted."

Two gaps:

- **No test for the `has_candidate`-skipped case** (finding #1) — e.g.
  `|_x| top_str.split_on(",").len()`, an outer-rooted chain with no literal. If
  the gating is intentional, that program's inferred type is the thing pinning
  the decision and it should be a test.
- **No test that a genuinely conflicting chain still reports its mismatch at the
  outer boundary.** design.md asserts this ("A chain that genuinely conflicts
  still reports its mismatch there") but nothing pins it. Since the whole change
  is "defer this decision to a later boundary," the case that matters most is
  the one where the later boundary must still say no.

### 8. Snapshot impact is plausible

Exactly one snapshot changed, and only by fresh-var renumbering
(`constraint-fn-var 239 → 235`, `231 → 227`) — consistent with "fewer boundary
default vars are minted."

One thing to double-check, though: design.md says the change means "no `LITERAL
DEFAULTED` warning is reported for" a trapped literal. If any existing snapshot
contained a `LITERAL DEFAULTED` warning for a literal behind an outer-rooted
chain, it should have lost that warning here. None did. That's most likely
because the pattern is rare enough that no snapshot exercised it — but it's
worth confirming rather than assuming, since "the pass never fires on the
corpus" and "the pass fires and changes nothing" look identical from the
snapshot diff.

## Presentation nit

Both the PR body and the design.md section are excellent — they state the
mechanism, the alternative that was rejected (using the deferred-dispatch list,
which is empty at boundary time), and the semantics that are deliberately
preserved. The one thing missing from the PR body is the honest headline that
design.md does state: **this makes programs check that the checker previously
rejected.** That's a language-visible change and it deserves to be in the first
paragraph, not the fourth.
