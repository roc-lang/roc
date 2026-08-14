# PR #9245 — Error message for missing parentheses on static dispatch call

- **Author:** fpsvogel (Felipe Vogel) · **Draft:** no · **Base:** `main` · **Head:** `4f087a7`
- **Size:** +248 / −70 across 5 files
- **Resolves:** #9193
- **Opened:** 2026-03-07 — the oldest non-draft PR in the set

Turns the unhelpful "This is not a record, so it does not have any fields to
access" into `` `my_method` is a method, not a record field. Did you forget the
parentheses? `` with a `Use \`my_method()\` instead.` suggestion, pointing at the
`.method` region rather than the receiver.

## Verdict

**The user-facing improvement is clearly worth having**, and the enabling
refactor — splitting `unifyRaw` out so the failure context can be built lazily —
is a genuinely good piece of design, not a hack around one error message. The
`RecordAccessContextBuilder` pattern means the expensive
`isMethodOnNominalType` lookup runs only when unification has already failed.

**It will not compile on current `main`** (#1). Five months of drift in
`src/check/` means this needs a rebase before anything else can be assessed, and
one of the changed signatures breaks the new code directly.

---

## Findings

### 1. (Blocking) Stale: `try std.fmt.bufPrint` no longer compiles against `render`'s signature

At the PR head, the Doc renderer is:

```zig
fn render(doc: Doc, builder: *ReportBuilder, report: *Report) !void {
```

— an **inferred** error set, so `try std.fmt.bufPrint(...)` (which can return
`error.NoSpaceLeft`) is fine.

On current `main` it has tightened to:

```zig
fn render(doc: Doc, builder: *ReportBuilder, report: *Report, out: *Document) Allocator.Error!void {
```

`error.NoSpaceLeft` is not in `Allocator.Error`, so the new `.ident_call` branch
won't build after rebase. Note that every *other* `bufPrint` in this same file
already handles it without `try` — `catch "?"` at `:2636`,
`catch indent` at `:2814`, `catch " tag's payload"` at `:2855`. The new code is
the only `try bufPrint` in the file.

More broadly: `Check.zig` and `unify.zig` have moved a lot since March (the
deferred-dispatch rank work in #10671 touches the same `unifyInContext`
neighborhood), so the rebase is likely to be substantial rather than mechanical.

### 2. (Should fix) `[512]u8` fixed buffer for the suggestion string

```zig
var buf: [512]u8 = undefined;
const with_parens = try std.fmt.bufPrint(&buf, "{s}()", .{builder.can_ir.getIdent(i)});
const owned = try report.addOwnedString(with_parens);
```

Once the `try` becomes a `catch` (per #1), an identifier longer than 510 bytes
silently loses its `()` — which is the entire point of the message. Since the
result is immediately copied into `report.addOwnedString` anyway, the buffer buys
nothing:

```zig
const with_parens = try std.fmt.allocPrint(report.allocator, "{s}()", .{builder.can_ir.getIdent(i)});
```

(or whatever the report's owned-string allocator is). Same number of lines, no
cap, no error-set problem.

For the record, I checked whether the concatenation is even necessary: it is.
Emitting `D.ident(name)` followed by `D.bytes("()")` would produce
`` `my_method`() `` rather than `` `my_method()` ``, and getting the annotation
to span the parens is exactly the second of the two tweaks the PR describes. So
`ident_call` as a Doc variant is the right shape — just not the fixed buffer.

### 3. (Should fix) A documented heuristic was deleted, not replaced

The old `Check.unifyInContext` carried this:

```zig
// We assign all fresh variables the region of `b` (the "actual" type), since `a` is
// typically the "expected" type from an annotation. This heuristic works well for
// most cases but can be imprecise for deeply nested unifications where fresh variables
// are created for sub-components (e.g., record fields, tag payloads). In those cases,
// error messages may point to the outer expression rather than the specific field.
//
// A more precise solution would track the origin of each fresh variable during
// unification and propagate that back, but the current approach is sufficient for
// typical error reporting scenarios.
```

The new code replaces all of it with:

```zig
// Process fresh vars and deferred constraints regardless of success or failure.
```

The code it documented is unchanged — `const region = self.getRegionAt(b);` and
the loop are still there. So a real, non-obvious design decision plus its known
limitation and the sketch of a better solution have been dropped for no reason.
`.rules` asks documentation to focus on the WHY; this was one of the better
examples of that in the file. Restore it.

### 4. (Question) The clobber now happens *after* fresh-var processing

The order changed:

| | old | new |
|---|---|---|
| 1 | unify; **on failure: append problem + `types.union_(a, b, .err)`** | `unifyRaw` — no problem, no clobber |
| 2 | process fresh vars (rank, region) | process fresh vars (rank, region) |
| 3 | copy deferred constraints | copy deferred constraints |
| 4 | — | **on failure: build context, append problem, `types.union_(a, b, .err)`** |

Step 2 reads `self.types.resolveVar(fresh_var).desc.rank`. Previously, on the
failure path, `a` and `b` had already been unioned into an `.err` descriptor with
`rank = Rank.generalized`. Any fresh var in that class therefore resolved to
`generalized`; now it resolves to its own pre-clobber rank, and
`addVarToRank(fresh_var, fresh_rank)` files it in a different pool.

That may well be an improvement — the pre-clobber rank is arguably the true one,
and generalizing a fresh var because an *unrelated* unification failed sounds
wrong. But it's a behavior change on the error path that the PR body doesn't
mention and no test covers. Given how rank discipline interacts with boundary
defaulting (see #10671, which is entirely about exactly this), it deserves an
explicit answer: is the new ordering intended, and is it observable?

If it's intended, one sentence in the `unifyBuildingContext` doc comment ("fresh
vars are ranked before the mismatch clobbers `a`/`b`, so they keep their own
rank") would make it deliberate rather than incidental.

### 5. (Minor) `isMethodOnNominalType` interns idents as a side effect

```zig
for (self.imported_modules) |imported_env| {
    const imported_name = ...;
    const imported_module_ident = try self.cir.insertIdent(base.Ident.for_text(imported_name));
    if (imported_module_ident == origin_module_ident) break :blk imported_env;
}
```

This inserts every imported module's name into the *current* module's ident
store, on every call, as a side effect of building an error message. It's
mitigated by laziness (only on unification failure) and it's the documented
existing approach (`checkStaticDispatchConstraints` does the same), so I'm not
asking for a redesign — but "reporting an error mutates the ident store" is worth
a comment at the site, because it's surprising and it makes the function
non-idempotent in a way a reader wouldn't expect from its name.

**Credit:** the comparison is on `Ident.Idx` values, not on text. That's the
right call and it's what keeps this clear of the `type-checker-patterns` ban on
`eql`/`find*` in `src/check/`. The inline comment explaining why
(`insertIdent` converts the string so the comparison can be index-based) is
exactly the kind of note that survives review.

### 6. (Should add) No snapshot test

Coverage is one `type_checking_integration.zig` case with a `.fail_with` exact
string. That's a good test — it pins the message, the region
(`test:6:6:6:16`, which I verified is `.my_method`, 10 columns starting at the
dot) and the caret alignment.

But error-message changes in this repo normally land with a snapshot too, and a
snapshot would cover the pieces the unit test can't: how the message renders in
the terminal box/header layout, and whether the `.region` `ProblemRegion` variant
interacts correctly with the source-region renderer. Given PR #10643 is about to
rewrite that renderer entirely, a snapshot is also the thing that would catch a
bad interaction between the two.

### 7. (Minor) `is_method: bool = false` defaults silently

Adding a defaulted field to `Context.record_access` means every existing
construction site compiles unchanged and gets `false`. That's convenient here
(there's only one site that should set it), but a defaulted boolean that changes
which report is produced is the kind of field a new construction site forgets.
Worth considering a non-defaulted field so the compiler forces the decision — it's
a two-line change at the one other site.

## What's good about this

Worth saying explicitly, because it's the part most likely to get lost in a
rebase:

- **`unifyRaw` is a clean seam.** Separating "unify and snapshot" from "record
  the problem and clobber" is the right factoring, and it's now available to any
  future caller that needs to inspect live vars before they're destroyed. The
  doc comment spells out the caller's obligations (build context, append
  problem, clobber) precisely.
- **`unifyInContext` was refactored to go through it** rather than duplicated,
  so there's still one implementation of the mismatch path.
- **The laziness is the point, and it's real.** `isMethodOnNominalType` walks
  imported modules and does a method lookup; running that on every successful
  record access would be a measurable regression. The `build()`-on-failure
  pattern avoids it entirely.
- **The comptime `@hasDecl` check** on `ctx_builder` gives a readable error
  instead of a deep template failure. Nice touch for an `anytype` parameter.

The PR body's suggestion to review commit-by-commit is good advice and I'd
repeat it to the next reviewer — the two message-polish commits are separable
from the refactor and could land independently if the rebase gets hairy.
