# Canonicalization single-pass refactor

This branch is the working space for collapsing the multi-phase canonicalization in `Can.zig` down to a single walk over each statement list. It is expected to take several sessions and land as one PR.

## Goals, in order

1. **Correct.** All existing snapshot tests in `test/snapshots/` must still produce byte-identical output after the refactor. `zig build minici` must pass.
2. **Better tested.** New edge-case tests covering forward refs, mutual recursion, deeply nested associated blocks, anno-only defs interleaved with matching anno+decl pairs, and qualified refs to forward-declared types. Some of these are partially covered today — make the coverage exhaustive.
3. **Faster.** The 200k-symbol stress should land below current numbers. The contributing wins are: eliminating ~4 redundant walks of the statement list per `processAssociatedBlock`; removing duplicate `insertQualifiedIdent` calls between phases; eliminating duplicate placeholder pattern creation for anno+decl pairs.

## What the multi-phase structure actually does

`canonicalizeFile` has these phases for module-level statements:
- Phase 0: register import module aliases (so qualified type refs can find their module)
- Phase 1a: process top-level `type_decl`s WITH associated blocks — first pass (creates placeholders, defers associated bodies)
- Phase 1.5.5: introduce types without associated blocks
- Phase 1.5.6: anno-only top-level annotations early (type-modules only)
- Phase 1.6: process deferred associated blocks
- Phase 1.7: process type aliases (uses topological sort, since aliases can reference each other)
- Phase 2 (second pass): canonicalize all other statements

`processAssociatedBlock` has these sub-phases for each associated block's items:
- `FirstPass` (Phase 2a/2b): creates placeholder patterns for every item, registers them in the **parent** scope under their fully-qualified name (so `Module.T.foo` lookups work from outside the block)
- FIRST loop: after `scopeEnter`, adds unqualified + type-qualified aliases for `.decl` items in the **child** scope (so in-block references and parent visibility from nested blocks work)
- SECOND loop: processes nested `type_decl`s with their own associated blocks (recursive call)
- THIRD loop: adds type-aliases for nested types into the child scope
- `SecondPass`: actually canonicalizes the bodies, creating real defs that reuse FirstPass's placeholders
- PostAnno: for anno-only defs (no matching decl), adds aliases to the child scope

## Why it's structured this way

The phases are not algorithmically necessary — they accumulated as features got added. The semantic requirement is more permissive than the implementation suggests:

- **Forward references must work.** `foo = bar; bar = 42` is legal. The current code handles this by pre-creating placeholders in FirstPass.
- **In Roc the existing infrastructure** *already* supports lazy placeholders via `Scope.forward_references` (see `Can.zig:5317` — when an associated block lookup misses, it creates a placeholder and adds it to `scope.forward_references`). When the actual def is later introduced via `scopeIntroduceInternal`, line 12587 "upgrades" the forward reference. This is exactly the design used in many compilers.

The catch: the lazy mechanism is keyed by unqualified name; FirstPass placeholders are keyed by fully-qualified name. They don't talk to each other. That's why FIRST loop exists — to bridge the qualified placeholder into an unqualified alias in the child scope. Without FIRST loop, body refs that go through `forward_references` would create a *second* placeholder that wouldn't unify with FirstPass's qualified one.

## Refactor plan

### Phase 1: Make forward_references the single source of placeholders

Modify `canonicalizeAssociatedDecl` and `canonicalizeAssociatedDeclWithAnno` so that when they create a def, they:
1. Look up by **qualified** ident in parent scope (current behavior — finds FirstPass placeholder)
2. If not found, look up by **unqualified** decl_ident in current scope (catches body-created forward_references entries)
3. If neither found, create a new pattern
4. Register the resulting pattern under both qualified and unqualified names

When introducing under the unqualified name, also clear `forward_references[decl_ident]` if present (mirrors line 12587's "upgrade" semantics).

This makes the two placeholder mechanisms unify. Forward refs from body canon and FirstPass-created placeholders now resolve to the same pattern.

### Phase 2: Eliminate FirstPass

Once Phase 1 holds, FirstPass is no longer strictly needed for forward-ref correctness within a block. The remaining work FirstPass does:

- Place qualified placeholder in parent scope (for external lookups). **Move into the def-creation code** — register under qualified name in parent scope at creation time.
- Update `placeholder_idents` map. **Likely removable** — only `createAnnoOnlyDef.isPlaceholder` uses it. With single-pass, an anno-only def can be detected directly by lookahead.
- `registerUserFacingName` (puts user-facing-qualified name in scope[0]). **Move into the def-creation code.**
- Phase 2b: recursive setup for nested types. **Move into the type_decl handling** in the single-pass walk.

### Phase 3: Collapse processAssociatedBlock to single walk

```
fn processAssociatedBlock(...) {
    scopeEnter
    defer scopeExit
    parent_type_alias_setup

    var i = 0;
    while (i < statements.len) {
        switch (statements[i]) {
            .type_anno => {
                if next is matching decl: process anno+decl pair fully, i += 2
                else: process anno-only def, i += 1
            }
            .decl => process decl alone, i += 1
            .type_decl => process nested type decl (register + recurse), i += 1
            else => i += 1
        }
    }
}
```

Each helper does ALL the per-stmt work: scope aliases (qualified in parent, unqualified + type-qualified in current), `setExposedNodeIndexById`, `registerMethodIdent`, body canon, def creation. Forward refs inside bodies hit the `forward_references` path which now produces unified placeholders.

The SECOND/THIRD setup loops collapse into the `.type_decl` branch (nested-type alias setup happens there, after the nested block is processed).

The PostAnno loop is no longer needed — anno-only defs are handled inline; matching-decl annos register their unqualified alias inline.

### Phase 4: Apply same pattern to canonicalizeFile

The module-level phases (0, 1a, 1.5.5, 1.5.6, 1.6, 1.7, 2) collapse similarly. The tricky one is **Phase 1.7's topological sort for type aliases**. With on-demand placeholders this falls out naturally: when alias `A : B` is processed and `B` isn't yet declared, register a placeholder for `B`; when `B := ...` is later processed, the alias resolves to it. Mutually recursive aliases (`A : B; B : A`) work as long as both eventually resolve.

## Risk areas to test exhaustively before relying on the refactor

The single-pass design fundamentally changes ordering of side effects. New test coverage needed for:

1. **Forward refs across mixed kinds:** value-ref-to-type, type-ref-to-value (where allowed), all combinations.
2. **Mutual recursion in associated items.** Two methods that call each other.
3. **Nested associated block referencing outer block's items** (both before and after the outer item is defined textually).
4. **Anno-only def between two matching anno+decl pairs**, in various orders.
5. **Type aliases referencing types defined later in the same file.**
6. **Qualified references to forward-declared types** (e.g., `foo : SomeNested.Inner` where `SomeNested` is defined later).
7. **Errors:** identifier never defined, type referenced but never declared, qualified ref where the qualifier doesn't exist.
8. **Shadowing edge cases:** forward ref shadowed by later inner-scope definition.

Each of these should be a snapshot test, ideally in both the type-module and non-type-module forms.

## Execution

Build tests for every edge case before touching the refactor. Then drive the four phases in order. Each phase ends with snapshots regenerated cleanly and the full test suite green.
