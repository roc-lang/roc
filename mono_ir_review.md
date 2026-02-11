# MonoIR Comparison & Review: Old Rust vs New Zig

## Executive Summary

The new Zig compiler does not have a separate "MonoIR" data structure. Instead,
monomorphization works by duplicating CIR (Canonical IR) expression trees in-place
via `Monomorphizer.zig` and `ClosureTransformer.zig`. The old Rust compiler has a
fully separate IR (`Stmt`/`Expr` in `crates/compiler/mono/src/ir.rs`) with ANF form,
explicit refcounting, decision tree compilation, join points, layout computation, and
memory reuse optimizations.

This review focuses on **correctness and robustness** issues in the new implementation.

---

## Table of Contents

1. [Architectural Differences](#1-architectural-differences)
2. [Hash Collision Risk in structuralTypeHash](#2-hash-collision-risk-in-structuraltypehash)
3. [swapRemove Index Invalidation During Lambda Set Resolution](#3-swapremove-index-invalidation-during-lambda-set-resolution)
4. [Incomplete Type Substitution Building](#4-incomplete-type-substitution-building)
5. [Alias Type Hashing Does Not Recurse Into Type Arguments](#5-alias-type-hashing-does-not-recurse-into-type-arguments)
6. [ExternalSpecializationRequest.eql Ignores concrete_type](#6-externalspecializationrequesteql-ignores-concrete_type)
7. [Closure Duplication Does Not Duplicate Captures List](#7-closure-duplication-does-not-duplicate-captures-list)
8. [e_type_var_dispatch Passed Through Without Duplication](#8-e_type_var_dispatch-passed-through-without-duplication)
9. [Linear Pending Specialization Search](#9-linear-pending-specialization-search)
10. [LIFO Processing Order for Pending Specializations](#10-lifo-processing-order-for-pending-specializations)
11. [assert Instead of Error for Recursion Depth](#11-assert-instead-of-error-for-recursion-depth)
12. [Region Tracking Uses u8 (Max 255 Nesting Levels)](#12-region-tracking-uses-u8-max-255-nesting-levels)
13. [Type Substitution Map Not Populated Before Use Comment](#13-type-substitution-map-not-populated-before-use-comment)
14. [Flattened Lambda Sets Leak Ownership](#14-flattened-lambda-sets-leak-ownership)
15. [constCast in getSpecializedName and isTagUnion](#15-constcast-in-getspecializedname-and-istagunion)
16. [Missing Architectural Pieces (Known, Not Bugs)](#16-missing-architectural-pieces-known-not-bugs)
17. [compareByRegionDesc Fallback Value Hides Bugs](#17-comparebyregiondesc-fallback-value-hides-bugs)
18. [Tag Union Substitution Skips Payload Types](#18-tag-union-substitution-skips-payload-types)
19. [Block Statement Duplication Falls Through for Unknown Variants](#19-block-statement-duplication-falls-through-for-unknown-variants)
20. [duplicateExpr Sharing Optimization May Cause Aliasing Issues](#20-duplicateexpr-sharing-optimization-may-cause-aliasing-issues)

---

## 1. Architectural Differences

These are not bugs, but fundamental design differences worth understanding.

### The old compiler's MonoIR is ANF; the new one keeps tree structure

**Old (Rust):** Converts to A-Normal Form where every subexpression is bound:
```
Let(x, Literal(42), InLayout,
  Let(y, Call(add, [x, x]), InLayout,
    Ret(y)))
```

**New (Zig):** Keeps the tree structure from CIR:
```
e_call(add, [e_num(42), e_num(42)])
```

ANF makes analysis passes (refcounting, alias analysis, register allocation)
straightforward because every intermediate value has a name. The tree form is
simpler but will need to be lowered to something ANF-like before native codegen.

### The old compiler computes full memory layouts; the new one doesn't yet

The old compiler computes `InLayout` for every value (sizes, alignments, field
orderings, discriminant types, nullable pointer optimizations for recursive
unions). This is needed for codegen but not for interpretation.

### The old compiler has 5 optimized union layout variants; the new doesn't

`UnionLayout` in the old compiler has: `NonRecursive`, `Recursive`,
`NonNullableUnwrapped`, `NullableWrapped`, `NullableUnwrapped`. These are
critical optimizations (e.g. `ConsList` uses a null pointer for `Nil` instead
of a tag byte). The new compiler will need these when targeting native code.

### The old compiler has explicit refcounting ops; the new tracks ownership declaratively

Old: `Stmt::Refcounting(ModifyRc::Inc/Dec/DecRef/Free, continuation)` nodes in
the IR. New: `ArgOwnership` enum (borrow/consume) on low-level operations. The
old approach is more explicit; the new approach pushes responsibility to the
backend.

---

## 2. Hash Collision Risk in structuralTypeHash

**File:** `Monomorphizer.zig:1991-2088`
**Severity:** Medium-High (silent wrong codegen)

The `structuralTypeHash` function is the sole mechanism for deduplicating
specializations. Two types that are structurally different but produce the same
hash will be treated as the same specialization, silently generating wrong code.

### Issues:

**a) No equality check after hash match.** `requestSpecialization` (line 832) and
`requestExternalSpecialization` (line 902) compare only `type_hash`, never
verifying actual structural type equality. This means any hash collision produces
a silent wrong-code bug. The old Rust compiler uses both hash-based lookup AND
structural comparison for its layout interner.

**b) Flex vars hash by name, not identity.** Lines 2072-2077: two different flex
vars with the same name (or both unnamed, `name == null`) will hash identically.
If `identity : a -> a` is called with two different unnamed flex vars
(representing different types), they'll get the same specialization.

**c) Alias hashing doesn't recurse into arguments.** Line 2082-2085: an alias is
hashed only by its `ident_idx`, not its type arguments. So `Result U64 Str` and
`Result Bool I32` would hash the same if they share the same alias ident.

**d) Tag union hashing skips extension variable.** Lines 2044-2053: tag union
payloads are hashed correctly, but the extension variable is not hashed. Two
open tag unions `[A]ext1` and `[A]ext2` with different extensions would collide.

### Recommendation:

After a hash match, perform a full structural type equality check before reusing
a specialization. The hash is a fast filter; equality is the correctness check.

---

## 3. swapRemove Index Invalidation During Lambda Set Resolution

**File:** `Monomorphizer.zig:617-641`
**Severity:** High (incorrect resolution, skipped entries)

In `resolveEntriesForTypeVar`, entries are collected into `all_entries` with
indices into their respective lambda sets. When an entry is resolved (line 639),
`swapRemove` is called on the lambda set's unspecialized list.

`swapRemove` replaces the removed element with the last element, changing the
index of that last element. But other entries in `all_entries` that pointed to
that last element still hold the old (now stale) index.

The check on line 619 (`entry_ref.index >= entry_ref.lambda_set.unspecialized.items.len`)
only catches indices that are now out of bounds. It does NOT catch the case where
an index now points to a different entry (the one that was swapped in).

### Example scenario:

Lambda set has unspecialized entries at indices [0, 1, 2].
- Process entry at index 0 -> resolves, swapRemove(0) -> entry at index 2 moves to 0
- Process entry at index 1 -> works fine
- Process entry at index 2 -> index >= len (now 2), skipped
- Result: the entry that was originally at index 2 (now at index 0) is never processed

The old Rust compiler handles this by processing all entries first, collecting
results, and then doing a single pass to remove resolved entries.

### Recommendation:

Either process in reverse index order within each lambda set, or collect all
removals and do them in a batch after processing, or use a different removal
strategy that doesn't invalidate other indices.

---

## 4. Incomplete Type Substitution Building

**File:** `Monomorphizer.zig:1121-1263`
**Severity:** Medium (missed substitutions = wrong specialization)

`buildTypeSubstitutions` recursively walks polymorphic and concrete types to
build a var map. Several type structures are handled incompletely:

**a) Tag union payloads are not compared.** Lines 1237-1245: for tag unions,
only the extension variable is compared. The actual tag payloads (which may
contain type variables) are not walked. If you have:

```roc
f : [Ok a, Err e] -> ...
```

called with `[Ok U64, Err Str]`, the substitutions `a -> U64` and `e -> Str`
will NOT be discovered.

**b) Record field matching is positional, not by name.** Lines 1204-1221: record
fields are compared by positional index (`min_len`), not by field name. If the
polymorphic record has fields `{x: a, y: b}` and the concrete one has
`{y: U64, x: Str}` (different order), the substitutions will be wrong.

**c) Mismatched structure types silently do nothing.** When the polymorphic type
is a structure but the concrete type is a different content kind (e.g., one is
`.structure` and the other is `.alias`), the function silently returns without
building any substitutions. This could hide real type mismatches.

### Recommendation:

Walk tag union payloads by matching tag names. Match record fields by name, not
position. At minimum add debug assertions for unexpected structure mismatches.

---

## 5. Alias Type Hashing Does Not Recurse Into Type Arguments

**File:** `Monomorphizer.zig:2082-2085`
**Severity:** High (silent wrong specializations)

```zig
.alias => |alias| {
    hasher.update("alias");
    hasher.update(std.mem.asBytes(&alias.ident.ident_idx));
},
```

This hashes only the alias name, not its type arguments. Two instantiations of
the same alias with different type arguments (e.g., `List U64` vs `List Str`,
if `List` is stored as an alias) will produce the same hash.

The old Rust compiler's layout computation handles aliases by first looking
through the alias to its underlying type, then computing the layout of that
underlying type. This naturally differentiates `List U64` from `List Str`.

### Recommendation:

Hash the alias arguments and backing variable as well:
```zig
.alias => |alias| {
    hasher.update("alias");
    hasher.update(std.mem.asBytes(&alias.ident.ident_idx));
    var iter = self.types_store.iterAliasArgs(alias);
    while (iter.next()) |arg| {
        self.hashTypeRecursive(hasher, arg, seen);
    }
    const backing = self.types_store.getAliasBackingVar(alias);
    self.hashTypeRecursive(hasher, backing, seen);
},
```

---

## 6. ExternalSpecializationRequest.eql Ignores concrete_type

**File:** `Monomorphizer.zig:152-156`
**Severity:** Medium (wrong external specialization matching)

```zig
pub fn eql(a: ExternalSpecializationRequest, b: ExternalSpecializationRequest) bool {
    return a.source_module == b.source_module and
        a.original_ident == b.original_ident;
    // Note: concrete_type comparison would require type equality check
}
```

This equality function ignores the concrete type entirely. Two external
specialization requests for the same function but different concrete types will
be considered equal. This `eql` function is not currently used in hot paths (the
code uses hash-based lookups instead), but if anyone calls it, it's wrong.

### Recommendation:

Either include `concrete_type` comparison (perhaps via structural type hash), or
remove the `eql` function to prevent misuse.

---

## 7. Closure Duplication Does Not Duplicate Captures List

**File:** `Monomorphizer.zig:1381-1408`
**Severity:** Medium (shared mutable state between specializations)

When duplicating an `e_closure`, the captures span is reused without duplication:

```zig
.e_closure => |closure| {
    // ...
    return try self.module_env.store.addExpr(Expr{
        .e_closure = .{
            .lambda_idx = new_lambda,
            .captures = closure.captures,  // <-- NOT duplicated
            .tag_name = closure.tag_name,
        },
    }, base.Region.zero());
},
```

If the captures span references mutable state or if later passes modify captures
(e.g., adding capture entries), the original and specialized closures would share
the same captures. The old Rust compiler explicitly clones `CapturedSymbols`
when creating specialized procs.

### Recommendation:

Duplicate the captures span when creating the specialized closure expression.

---

## 8. e_type_var_dispatch Passed Through Without Duplication

**File:** `Monomorphizer.zig:1743`
**Severity:** Medium (static dispatch calls left unresolved in specialized code)

`e_type_var_dispatch` is listed in the pass-through group (line 1743) that
returns the expression index unchanged. But `e_type_var_dispatch` contains:

```zig
e_type_var_dispatch: struct {
    type_var_alias_stmt: CIR.Statement.Idx,
    method_name: Ident.Idx,
    args: Expr.Span,
}
```

The `args` are expression spans that should be duplicated with type
substitutions. The `type_var_alias_stmt` may reference a type variable that has
been substituted. By passing it through unchanged, the specialized function body
will contain unresolved static dispatch references.

### Recommendation:

Handle `e_type_var_dispatch` explicitly in `duplicateExpr` - at minimum duplicate
the args, and ideally resolve the dispatch to a direct call when the type
variable has a known substitution.

---

## 9. Linear Pending Specialization Search

**File:** `Monomorphizer.zig:829-838`
**Severity:** Low (performance, not correctness)

When checking if a specialization is already pending, `requestSpecialization`
does a linear scan through ALL pending specializations:

```zig
for (self.pending_specializations.items) |pending| {
    if (pending.original_ident == original_ident) {
        const pending_hash = self.structuralTypeHash(pending.concrete_type);
        // ...
```

This is O(n) per request AND recomputes `structuralTypeHash` for each pending
entry on every check. For programs with many specializations, this could be
quadratic.

### Recommendation:

Use a hash set for pending specializations keyed by `SpecializationKey`, or at
least cache the type hash in `PendingSpecialization`.

---

## 10. LIFO Processing Order for Pending Specializations

**File:** `Monomorphizer.zig:1014-1016`
**Severity:** Low-Medium (could affect resolution correctness)

```zig
while (self.pending_specializations.items.len > 0) {
    var pending = self.pending_specializations.pop() orelse break;
```

Pending specializations are processed in LIFO order (pop from end). If
specialization A depends on specialization B (e.g., A calls B), and A was
requested first, B will be processed before A. This works for simple cases, but
for mutual recursion or diamond dependencies, processing order could affect
which specializations are seen as "in progress" vs "already made".

The old Rust compiler processes specializations in a more controlled manner,
using a fixed-point iteration that handles the interaction between specialization
and lambda set resolution.

---

## 11. assert Instead of Error for Recursion Depth

**File:** `Monomorphizer.zig:1049`
**Severity:** Medium (crash instead of graceful error)

```zig
std.debug.assert(self.recursion_depth < self.max_recursion_depth);
```

In a release build, `std.debug.assert` is removed entirely (it's only checked in
debug/ReleaseSafe modes). In a debug build, it will crash with an assertion
failure rather than producing a user-facing error message.

The old Rust compiler reports this as a `RuntimeError` that the user can see:
"This function is too recursive for the type system to handle."

### Recommendation:

Return an error (or emit a diagnostic) instead of asserting.

---

## 12. Region Tracking Uses u8 (Max 255 Nesting Levels)

**File:** `ClosureTransformer.zig` (based on agent reports)
**Severity:** Low (unlikely but possible overflow)

The `current_region` field is a `u8`, limiting closure nesting depth to 255.
Deeply nested closures (e.g., from heavy combinator-style code or generated
code) could overflow. The old Rust compiler doesn't have this limitation since
it uses different mechanisms for ordering.

### Recommendation:

Use `u16` or `u32`, or add a checked increment that returns an error on overflow.

---

## 13. Type Substitution Map Not Populated Before Use Comment

**File:** `Monomorphizer.zig:852-853`
**Severity:** Informational (currently works, but fragile)

```zig
// Create type substitutions by unifying the polymorphic type with concrete
const type_subs = types.VarMap.init(self.allocator);
// For now, we'll populate this during body duplication
```

The type substitution map is created empty in `requestSpecialization` and only
populated later in `makeSpecialization` (line 1075). Between these two points,
the map is stored in `PendingSpecialization`. If any code tries to use
`pending.type_substitutions` between creation and population, it will find an
empty map.

This is not a bug today, but the comment "For now, we'll populate this during
body duplication" suggests this design may change. The fact that the map is
created but unused for potentially a long time is a code smell.

---

## 14. Flattened Lambda Sets Leak Ownership

**File:** `Monomorphizer.zig:564-586`
**Severity:** Medium (memory management)

```zig
var flattened_lambda_sets = std.ArrayList(*ClosureTransformer.LambdaSet).empty;
defer flattened_lambda_sets.deinit(self.allocator);

// ...
var additional_sets = try entry_ref.lambda_set.flattenForConcreteType(
    self.allocator,
    concrete_type,
    self.types_store,
);
defer additional_sets.deinit(self.allocator);  // <-- frees the ArrayList

// Track the additional lambda sets for processing
for (additional_sets.items) |new_set| {
    try flattened_lambda_sets.append(self.allocator, new_set);
}
```

`additional_sets` is deferred for deinitialization at the end of each loop
iteration, but the `new_set` pointers extracted from it are kept in
`flattened_lambda_sets`. If `flattenForConcreteType` allocated the `LambdaSet`
objects themselves (not just the ArrayList), deiniting `additional_sets` might
not free them. But if `additional_sets.deinit` frees the pointed-to lambda sets,
then `flattened_lambda_sets` will contain dangling pointers.

This depends on the contract of `flattenForConcreteType` which I couldn't fully
verify. The code should be clear about ownership transfer.

---

## 15. constCast in getSpecializedName and isTagUnion

**File:** `Monomorphizer.zig:2211, 2115`
**Severity:** Low (code smell, potential UB)

```zig
pub fn getSpecializedName(self: *const Self, ...) ?base.Ident.Idx {
    const mutable_self: *Self = @constCast(self);
    const type_hash = mutable_self.structuralTypeHash(type_var);
```

`@constCast` is used to call `structuralTypeHash` from a `*const Self` method.
This is technically undefined behavior if the original object was declared
`const`. The `structuralTypeHash` function allocates and frees a `seen` map
internally, which mutates the allocator.

### Recommendation:

Make `structuralTypeHash` take `*const Self` and use a stack-allocated seen set,
or change the calling functions to take `*Self`.

---

## 16. Missing Architectural Pieces (Known, Not Bugs)

These are features the old compiler has that the new one doesn't yet. They're
presumably planned for the future.

| Feature | Old Rust | New Zig | Notes |
|---------|---------|---------|-------|
| ANF conversion | Yes (`Stmt::Let` chains) | No (tree expressions) | Needed for native codegen |
| Decision tree compilation | Yes (`decision_tree.rs`, 2762 lines) | No (interpreter does linear scan) | Needed for native codegen |
| Join points | Yes (`Stmt::Join`/`Stmt::Jump`) | No | Needed for code dedup, tail calls |
| Tail call optimization | Yes (self-recursive → `Jump` to join point) | No | Needed for functional loops |
| Explicit refcounting IR | Yes (`ModifyRc::Inc/Dec/DecRef/Free`) | No | Needed for native codegen |
| Layout computation | Yes (`LayoutRepr`, sizes/alignments) | No | Needed for native codegen |
| Union layout optimization | Yes (5 variants of `UnionLayout`) | No | Needed for native codegen |
| Memory reuse (Reset/Reuse) | Yes (`Expr::Reset`/`ReuseToken`) | No | Performance optimization |
| Alias analysis | Yes (`CallSpecId`/`UpdateModeId`) | No | In-place mutation optimization |
| Type erasure | Yes (`ErasedMake`/`ErasedLoad`/`Erased`) | No | For platform interop |
| Higher-order builtins | Yes (`HigherOrderLowLevel`) | No | `List.sortWith` etc. |
| Niche-based lambda dispatch | Yes (`Niche`/`LambdaName`) | No | Multiple closures same name, different captures |

---

## 17. compareByRegionDesc Fallback Value Hides Bugs

**File:** `Monomorphizer.zig:653-668`
**Severity:** Low-Medium (silently wrong ordering)

```zig
fn compareByRegionDesc(_: void, a: ..., b: ...) bool {
    const a_region = if (a.index < a.lambda_set.unspecialized.items.len)
        a.lambda_set.unspecialized.items[a.index].region
    else
        0;  // <-- Fallback to 0 for invalid entries
```

When an entry's index is out of bounds (stale from swapRemove), it falls back
to region 0 instead of signaling an error. This means stale entries will sort
to the end (since sorting is descending), and then be "caught" by the bounds
check on line 619. But the fallback could mask bugs where indices are wrong
for reasons other than swapRemove.

### Recommendation:

At least add a debug assertion or log a warning when the fallback is hit, to
distinguish expected staleness from unexpected corruption.

---

## 18. Tag Union Substitution Skips Payload Types

**File:** `Monomorphizer.zig:1237-1245`
**Severity:** High (wrong specialization for polymorphic tag unions)

```zig
.tag_union => |poly_union| {
    const concrete_union = switch (concrete_flat) {
        .tag_union => |u| u,
        else => return,
    };
    // Compare extension types
    try self.buildTypeSubstitutions(poly_union.ext, concrete_union.ext, var_map);
},
```

Only the extension variable is compared. The actual tag payloads are not walked.
This means for a function like:

```roc
f : [Ok a, Err e] -> a
```

called with `f (Ok 42)` where the concrete type is `[Ok U64, Err Str]`, the
substitution `a -> U64` will NOT be discovered. The specialized function body
will still reference the unresolved type variable `a`.

The old Rust compiler handles this in its type unification/layout computation
which fully resolves all type variables before generating code.

### Recommendation:

Walk each tag's payload types as well. Match tags by name between the
polymorphic and concrete unions, then recursively build substitutions for
each tag's arguments.

---

## 19. Block Statement Duplication Falls Through for Unknown Variants

**File:** `Monomorphizer.zig:1443-1446`
**Severity:** Low-Medium (silently skips duplication)

```zig
else => {
    try self.module_env.store.scratch.?.statements.append(stmt_idx);
},
```

When duplicating a block, any statement variant that isn't `s_decl` or
`s_decl_gen` is passed through unchanged. This includes:

- `s_type_var_alias`: Contains type variable references that may need updating
- `s_crash`: Contains expressions that may need duplication
- `s_runtime_error`: Contains diagnostics
- `s_expect`: Contains expressions that should be duplicated

Any expression within these statements will reference the original (pre-duplication)
expression indices, which may be incorrect in the context of the specialized function.

### Recommendation:

Handle all statement variants explicitly, duplicating contained expressions.
An exhaustive switch (without `else`) would catch new variants at compile time.

---

## 20. duplicateExpr Sharing Optimization May Cause Aliasing Issues

**File:** `Monomorphizer.zig:1369-1371, 1388-1389, 1491-1493, etc.`
**Severity:** Low-Medium (depends on downstream mutation)

Several cases in `duplicateExpr` check if the duplicated sub-expression is the
same as the original and return the original index if so:

```zig
if (new_body == lambda.body and new_args.span.start == lambda.args.span.start) {
    return expr_idx;
}
```

This is an optimization to avoid unnecessary copies. But it means the
specialized function may share expression nodes with the original function.
If any later pass mutates expressions in-place (e.g., lambda lifting, closure
transformation), the mutation would affect both the original and specialized
versions.

The old Rust compiler avoids this by using an arena allocator where all nodes
are immutable once created. The new compiler uses a store that supports mutation.

### Recommendation:

Verify that no downstream pass mutates CIR expressions in-place. If any does,
this sharing optimization needs to be removed.

---

## Summary of Severity Rankings

| # | Issue | Severity |
|---|-------|----------|
| 2 | Hash collision → silent wrong codegen | **High** |
| 3 | swapRemove index invalidation | **High** |
| 5 | Alias hashing misses type args | **High** |
| 18 | Tag union substitution skips payloads | **High** |
| 4 | Incomplete type substitution building | **Medium-High** |
| 8 | e_type_var_dispatch not duplicated | **Medium** |
| 7 | Closure captures not duplicated | **Medium** |
| 11 | assert instead of error for recursion | **Medium** |
| 14 | Flattened lambda set ownership | **Medium** |
| 6 | ExternalSpecializationRequest.eql wrong | **Medium** |
| 19 | Block statement fallthrough | **Low-Medium** |
| 17 | compareByRegionDesc fallback hides bugs | **Low-Medium** |
| 20 | Expression sharing aliasing | **Low-Medium** |
| 10 | LIFO processing order | **Low-Medium** |
| 9 | Linear pending search (perf) | **Low** |
| 12 | u8 region limit | **Low** |
| 15 | constCast UB | **Low** |
| 13 | Empty type_substitutions map | **Informational** |
| 16 | Missing architecture (known) | **Informational** |

---

## Top Priority Fixes

1. **Add structural equality check after hash match** (Issue #2) - Without this,
   any hash collision silently produces wrong code. This is the highest-impact fix.

2. **Fix alias hashing to include type arguments** (Issue #5) - `List U64` vs
   `List Str` producing the same hash is almost guaranteed to hit in practice.

3. **Walk tag union payloads in buildTypeSubstitutions** (Issue #18) - Polymorphic
   tag unions are extremely common in Roc; not building substitutions for their
   payloads will cause widespread incorrect specialization.

4. **Fix swapRemove index invalidation** (Issue #3) - Either process in reverse
   order, batch removals, or use a different data structure.

5. **Handle e_type_var_dispatch in duplicateExpr** (Issue #8) - Static dispatch
   calls with arguments need their args duplicated.
