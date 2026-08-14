# PR #10434 — Implicitly open tag unions in output positions (polarity)

- **Author:** jaredramirez (Jared Ramirez) · **Draft:** yes · **Base:** `main`
- **Size:** +3,208 / −1,160 across 180 files (`Builtin.roc` alone is +533/−533; `Check.zig` +735/−42)

Makes every extensionless tag union in a *positive* (output) annotation position
implicitly open. Polarity starts positive at the annotation root and flips at
function argument positions; everything else preserves it. Aliases defer the
decision to the use site via a marker rigid; `[]` is exempt; host boundaries opt
out; an explicit `..` in output position now warns.

## Scope note

This is a language-semantics change touching 180 files. I read the full
`design.md` section, the new `closeUnquantifiedTagRowExts` /
`closeTagRowsForDerivation` machinery in `Check.zig`, the derived-parser
eligibility change, and the file-level shape of the rest. I did **not** verify
`instantiate.zig`'s polarity threading, the `TypeWriter` display suppression, or
the Monotype `nodeAdmitsImportedMono` fallback in detail. Those need their own
pass.

## Verdict

The design work is serious and the polarity rule itself is the textbook-correct
formulation (root positive, negate under arrows, preserve elsewhere) — including
the subtle right answers: args-of-args flip back to positive, `[]` is exempt to
preserve uninhabitedness, and named extensions are always honored.

**My central concern is that the feature's cost is concentrated in machinery
that exists only to undo it.** `closeUnquantifiedTagRowExts` is a new
whole-module probe-then-mutate sweep whose entire job is to re-close the rows
polarity opened but nobody consumed — and its correctness rests on a
hand-enumerated list of seven "keep open" root categories with no completeness
argument. That's the shape of thing that works today and produces a baffling bug
in six months.

Three specific findings below, then the comparison against #10366 — which
matters, because these two drafts are alternative solutions to the same problem
by the same author, and the design.md diffs make *opposite* edits to the same
section.

---

## Findings

### 1. (Rules) `closeUnquantifiedTagRowExts` is a probe-then-mutate rewrite that design.md never names

AGENTS.md:

> Never add a probe-then-mutate rewrite to the checker (a structural probe over
> solved types whose result gates a mutation of the solved graph …) without a
> rule declared in design.md's "Solver-Mutating Rewrites" section. Such a
> rewrite is indistinguishable at review time from a change to the language's
> typing rules, so the rule must be declared first, **the rewrite named for it**,
> and both its accepted and rejected sides pinned by tests.

`closeUnquantifiedTagRowExts` is exactly that: a structural reachability probe
over the solved graph (`keep_open` bitset, worklist closure over every alias,
record, tuple, nominal, function, and tag-union child) whose result gates

```zig
const empty_tu_var = try self.freshFromContent(.{ .structure = .empty_tag_union }, env, ext_region);
_ = try self.unify(ext, empty_tu_var, env);
```

on every unmarked flexible tag-row extension in the module.

The new design.md section *is* correctly placed inside "Solver-Mutating
Rewrites", and it does name `closeTagRowsForDerivation`,
`Instantiator.PolarityVarBehavior`, `Check.annotation_scheme_has_vars`, and
`InstGraph.groundUnresolvedDefaults`. But it **never mentions
`closeUnquantifiedTagRowExts`** — the single largest and most consequential
mutation in the PR. Its justification appears only in the PR body:

> Without this pass, Monotype requests carry unresolved row extensions, which
> have no durable specialization identity, and recursive helpers re-specialize
> without bound.

That's a good justification and it belongs in design.md, along with the
keep-open root taxonomy and accepted/rejected test pins for the sweep's boundary
(one row that must stay open, one that must be closed).

Strictly, the `RedirectRule` signature gate isn't tripped — the sweep goes
through ordinary `unify`, not `dangerousSetVarRedirect`/`setVarContent`. That's
the letter of the rule; the spirit is squarely engaged.

### 2. (Fragility) The "keep open" root list is a hand-maintained allowlist with no completeness argument

The worklist is seeded from seven categories, each with its own prose rationale:

1. top-level defs (`all_defs` + `global_value_defs`) — def, pattern, annotation vars
2. generalized local `s_decl` patterns
3. generalized `expr_lambda` / `expr_closure` vars
4. `statement_alias_decl` / `statement_nominal_decl` / `statement_type_anno`
5. `e_lookup_local`s that resolve to an `e_anno_only` Builtin **intrinsic**
6. `scheme_uses` roots and both sides of every `scheme_use_pairs` entry
7. `intrinsic_dispatch_fn_vars`, nominal decl backings, `requires_types`

Miss a category and a row that a later stage needs open gets grounded to `[]`.
The failure mode is not a clean error — it's a Monotype specialization that
silently can't absorb a tag, surfacing far from the cause.

Category 5 is the tell: it exists because "Monotype implements annotation-only
Builtin intrinsics and retypes rows their instantiated signatures reach (e.g.
the derived tag-union parser widens a `ParseTagUnionSpec.parse` options row with
the parent parser's errors)". That is a very specific piece of downstream
knowledge encoded as a root filter in the checker. Whoever adds the *next*
row-retyping intrinsic has to know to come back here.

The structural alternative is to make openness **derivable rather than
rediscovered**: a polarity-minted ext is open iff the generalizer quantified it.
The generalizer already computes that. If a minted ext were tagged at creation
(`from_polarity`) and the sweep grounded exactly the untagged-and-unquantified
ones, the seven-category reachability walk — and its completeness obligation —
would go away. That's a larger refactor, but it's the difference between an
invariant and an allowlist.

### 3. (Possible bug) The sweep's `unify` results are discarded, and class merges can outrun the precomputed marks

Two related concerns in the sweep loop:

**(a) Discarded unification result.**

```zig
_ = try self.unify(ext, empty_tu_var, env);
```

The `_ =` throws away a `Result` that can be `.problem`. The guard above is
`ext_resolved.desc.content != .flex → continue`, and a bare flex unifies with
anything — but a flex var in this checker can carry **static-dispatch
constraints**. Unifying a constrained flex against `empty_tag_union` would have
to satisfy those constraints, which an empty union generally won't. If that can
happen, the result is a spurious `TYPE MISMATCH` appended to the module's
problem store by a cleanup pass, with a region pointing at an extension the user
never wrote.

Either assert the unification succeeds (`if (result == .problem) Common.invariant(...)`)
or skip exts that carry constraints. Right now a failure is invisible until a
user reports an inexplicable error.

**(b) The `keep_open` marks are computed once, before any mutation.**

The comment says fresh vars sit past `num_vars` and are never tag unions, which
is accurate. But each sweep `unify` **merges union-find classes**. A var marked
keep-open can end up in the same class as one that an earlier iteration already
unified with `empty_tag_union`. The marks were computed against the pre-sweep
graph, so nothing re-checks that after a merge.

Whether that's reachable depends on whether two distinct tag-row exts can become
one class mid-sweep. I couldn't rule it out. A cheap guard: re-resolve and
re-check `keep_open` on the *current* root immediately before unifying (the code
does check `keep_open.isSet(raw_ext)` on the resolved ext, so this may already be
handled — but only if `resolveVar` reflects merges from earlier iterations, which
it should). Worth confirming explicitly, ideally with a comment, because the
ordering argument is doing real work here.

### 4. (Cost) A new full-module pass on every module

`closeUnquantifiedTagRowExts` allocates a bitset over every type var, scans
**every CIR node** (`while (raw_node < num_nodes)`), and walks a reachability
closure over the whole reachable type graph — unconditionally, for every module,
whether or not polarity opened anything.

It's linear, so not catastrophic. But this repo already has known super-linear
finalization behavior, and this adds a guaranteed full-module traversal to the
end of checking. It should be measured on a large module before landing (the
profiling harness in `CONTRIBUTING/profiling/` exists for exactly this), and the
number should be in the PR body.

### 5. (Behavior change, unflagged) Derived-parser eligibility: `.flex => .supported` → `.unresolved`

```diff
-        .err, .flex => .supported,
+        .err => .supported,
+        .flex => .unresolved,
```

A flexible tag extension that previously qualified a type for derived parsing
now **defers**. The reasoning is stated (`closeTagRowsForDerivation` grounds
reachable rows first, so a survivor is genuinely unresolved), and the design.md
"Derived Parser Tag-Row Closure" section is updated to match — good.

But the PR body doesn't mention it, and the question it raises isn't answered
anywhere: *deferred to when?* If nothing later resolves the row, does the user
get "parser not supported for this type," a hang, or silence? A previously-
deriving program that now fails to derive would be a regression, and the only
thing standing between the two outcomes is whether `closeTagRowsForDerivation`
reaches every row that matters — which is the same completeness question as #2.

### 6. (Ergonomics) `Builtin.roc` ±533 lines, and openness becomes invisible

The builtins change is exactly balanced: 533 lines each dropping a now-redundant
`, ..]`. After this, a reader of `Builtin.roc` can no longer tell from the source
whether a row is open — they must compute the polarity of the position. For a
file that is both the standard library and the primary worked example of Roc
style, that's a real readability cost.

Which connects to:

### 7. (Design opinion) The `REDUNDANT OPEN TAG UNION` warning is the wrong lever

Making `[InvalidU8, ..]` in output position a *warning* means the codebase
actively discourages the explicit spelling, and every existing Roc program with
correct annotations now emits warnings.

I'd push back on this specifically. The explicit `..` is documentation: it tells
the reader "open" without requiring them to derive polarity. Under the new rule
it's not wrong, just unnecessary — and "unnecessary but clearer" is not usually
warning-worthy. Compare: nobody warns on redundant parentheses in a type
annotation. Consider dropping the warning (or making it opt-in) and letting the
explicit form remain a valid style choice. That also removes the migration
burden entirely: existing annotations keep working *and* keep reading the same
way.

### 8. (Coupling) Two mechanisms must stay in sync

`InstGraph.groundUnresolvedDefaults` "commits those defaults early, **matching
what final sealing would materialize**." Two independent pieces of code that must
produce identical results, with the correspondence maintained only by that
sentence. If final sealing's defaulting ever changes, the early commit silently
diverges and stored codec restores get a different type than the rest of the
program.

At minimum, a shared helper. Failing that, a debug-only assertion that the early
commit and the sealed result agree, on the model of the existing
witness-assertion pattern (`numeralCandidateStructurallyRefuted` is
"witness-asserted against the probe it pre-filters in safety builds" — same idea).

### 9. (Question) Do `#polarity` marker rigids survive serialization?

Alias bodies store `types.polarity_var_text` as a marker rigid, resolved at
instantiation. `TypeWriter` (+60/−20) and `snapshot.zig` (+16/−1) are updated to
hide it. But an alias declared in module A and used in module B has to carry that
marker across the checked-artifact boundary. Does it? And if a marker ever
escapes into a user-visible type (an error message, `roc docs`, an LSP hover) via
a path that wasn't updated, the user sees `#polarity`.

A negative test — assert that no rendered type in the snapshot corpus contains
`polarity_var_text` — would be cheap insurance.

---

## Comparison with #10366 (`?` re-raises errors)

These two drafts solve the same reported problem and are mutually exclusive in
their design.md edits: **#10366 deletes** the "Hosted Try Question Widening"
section; **this PR keeps it** and adds a paragraph saying polarity makes it
rarer. Both cannot land. Some observations that may help the decision:

| | #10434 (polarity) | #10366 (re-raise) |
|---|---|---|
| Diff | +3,208 / −1,160, 180 files | +571 / −575, 34 files (**net −118**) |
| Solver-mutating rewrites | adds a full-module probe-then-mutate sweep | **removes** a `RedirectRule` |
| Coverage | every output position | only where a `?` appears |
| Order sensitivity | none (polarity is syntactic) | yes (widens only if the row resolved closed by check time) |
| Runtime cost | none | a cold-path re-tag match when rows differ |
| Reversibility | one-way (removing it breaks code) | two-way (only accepts more at `?`) |
| Source readability | openness becomes invisible in annotations | annotations unchanged |

The honest summary is that **polarity is more principled and more complete;
re-raise is dramatically cheaper and reversible.** #10366's ordering sensitivity
(its finding #1) is a genuine defect but has a known fix; polarity's
`closeUnquantifiedTagRowExts` allowlist (this PR's #2) is a genuine fragility
with no fix short of a refactor.

Given #10366's two-way-door argument holds — it strictly accepts more programs
at `?` sites and can be layered under polarity later — the lower-risk sequencing
is to land re-raise, measure how many remaining complaints are *not* `?`-shaped,
and only then decide whether polarity's cost is warranted. That's the author's
own framing in #10366's Pros section, and I think it's right.

## Process nits

- **`Try(a, [])` exemption creates a discontinuity**: `[A]` in output position is
  open, `[]` is closed, so deleting the last tag from a union silently flips its
  openness. Correct (uninhabitedness must be preserved) but worth a note in the
  docs users read, not just design.md.
- **The known-issue note** (`fx platform runtime stack overflow` times out
  locally, traced to the host's own self-test) is the fourth independent report
  of a pre-existing platform-test failure across these PRs (#10043, #10290,
  #10366). These should be filed.
- **Cache version bumped to 41** with fresh golden hashes, and the serialization
  tests updated. ✓
