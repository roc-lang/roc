# #13165: unfinished bindings escape self-reference checking

Issue: https://github.com/roc-lang/roc/issues/13165

The issue is assigned to `rtfeldman`. The reproduction and root-cause
sections record the pre-fix investigation.

## Reproduction

From the repository root:

```sh
zig build run-test-zig-module-can -j2 -Dtest-filter='issue 13165' -- --test-filter 'issue 13165'
```

The regression in `src/canonicalize/test/local_let_scoping_test.zig` expects
one `self_referential_definition` diagnostic. It fails with `expected 1,
found 0`. It needs no evaluation or platform dependency.

The standalone checker reproduction is:

```sh
zig build roc -j2
./zig-out/bin/roc check --no-cache --no-color test/repros/issue-13165/Repro.roc
```

It incorrectly exits zero and reports `No errors found`. The invalid read is
the `return val` inside the initializer that is supposed to produce `val`.
Constant-folding the condition is not needed to reproduce the missing
diagnostic.

## Root cause

Canonicalization introduces the destructuring binders before walking their
initializer. `beginDefiningBoundVars` records the fresh binder identities,
and `canonicalizeUnqualifiedIdentExpr` calls `isDefiningBoundVar` to reject
reads of those unfinished bindings.

The match-branch traversal in `src/canonicalize/Can.zig:14484-14521` clears
`defining_bound_vars` before canonicalizing the guard and body. A match branch
is still eagerly evaluated as part of the enclosing initializer, but that
reset treats it as a deferred function body. The lookup therefore becomes
an ordinary `e_lookup_local` instead of a diagnostic.

This is not specific to `match`: `beginBlockState` (`Can.zig:9668-9678`)
and the `for` traversal (`Can.zig:11981-11999`)
also clear the state. Replacing the match with
`if take_early { return val } else { (123, Unchanged) }` was also accepted by
`roc check`. In addition, the current membership check considers only the
innermost declaration's span (`Can.zig:6087-6092`), not all active eager
initializers.

## Implemented rule

Make unfinished-binding tracking an explicit, scoped canonicalization state
for the entire eager initialization context. Nested eager blocks, matches,
guards, loops, and initializers must retain the enclosing unfinished
binders. Entering an initializer adds its fresh binders; leaving removes only
that initializer's additions. Branch/block cleanup must never pop an
enclosing initializer's scratch span.

Keep deferred lambda-body boundaries and existing recursive-function rules
explicit, rather than clearing the state at ordinary lexical scopes.
Continue excluding existing `var` reassignment targets: their old values
are available while computing the replacement.

Singleton contexts compare their one canonical binder directly, without
allocating or maintaining an index. Multi-binding contexts keep a sparse
index of simultaneously active binders
beside the existing initializer scratch stack. It does not enlarge the
binding records touched by every name lookup, allocate a module-wide
pattern-id column, or retain entries for completed declarations.

The current context is a span in that stack. Ordinary scopes preserve it;
nested initializers extend it. A lambda suspends the span and restores it
without copying or clearing the index. Indexed positions distinguish
suspended outer binders from binders initialized in the lambda body.
Initializer completion retires only its additions. Qualified and
unqualified names use the same canonical pattern identity.

This chooses bounded active-state storage over the earlier proposal to
enlarge every binding record. It is not a claim of universal performance
optimality; ordinary sequential initializers and wide destructuring are
both included in the before/after compiler checks.

Add coverage for guards, nested eager initializers, loops, permitted
recursive functions, aliases, and reassignment. Do not repair the invalid
lookup in a backend or initialize the unread binding to a dummy value.
