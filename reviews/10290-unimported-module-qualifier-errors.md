# PR #10290 — Improve error messages: unimported module qualifiers and nominal-vs-record mismatches

- **Author:** jaredramirez (Jared Ramirez) · **Draft:** yes · **Base:** `main`
- **Size:** +1,055 / −481 across 36 files

## Verdict

The first half — turning a bare `` `Stdin.line!` does not exist. `` into an
explanation plus a concrete `import pf.Stdin` suggestion — is a clear
improvement and is implemented sensibly.

**But the PR body does not describe the PR.** The second change it advertises is
not in the diff at all, and a substantial *unadvertised* change to platform
publication is (#1). And there's a behavior change in qualified-name resolution
that looks like a correctness bug (#2).

---

## Findings

### 1. (Blocking) The described second change is absent; an undescribed one is present

The PR body's section 2 describes nominal-vs-record field hints, `SnapshotNominalType`
gaining `is_opaque`, and a rule mirroring `types.NominalType.canLiftInner`, plus a
new snapshot `test/snapshots/nominal/nominal_record_mismatch_field_hint.md`.

None of it is in the diff. `is_opaque`, `SnapshotNominalType`, and `canLiftInner`
have **zero occurrences** across all 2,799 diff lines, and the claimed snapshot
file is not in the changed-files list.

What *is* in the diff, and is mentioned nowhere:

| file | change |
|---|---|
| `src/compile/coordinator.zig` | +211 / −94 |
| `src/check/Check.zig` | −19 (deletes `requiresTypesContainError`) |
| `src/compile/compile_package.zig` | +4 / −6 |
| `src/compile/cache_config.zig` | `CACHE_VERSION` bump |
| `src/compile/cache_module.zig` | new golden `MODULE_ENV_VERSION_HASH` |
| `test/serialization_size_check.zig` | +2 / −2 |

That coordinator change tightens platform-root publication: it removes the
"either a checked artifact **or** a deferred continuation" alternative in favor
of requiring the deferred continuation, and deletes
`selectedHoistedRootInputsFromArtifact` — a function that reconstructed selected
hoisted roots by walking a finished artifact's `compile_time_roots`.

On the merits I like it. Deleting a "recover the information from a later
artifact" path in favor of `coordinatorInvariant("...lost its deferred checking
state")` is precisely what AGENTS.md asks for. The new comment states the reason
well:

> A relation-less platform artifact cannot substitute for that state:
> finalization needs the owning checker's problem store, selected hoisted roots,
> requirement context, and CTFE options.

But it has nothing to do with error messages, it changes when compilation
*fails*, and it bumps the cache version. It should be its own PR with its own
description, and this one should either regain its section 2 or drop the claim.

Most likely the branch was force-pushed and the body wasn't updated. Either way,
a reviewer reading the body will review the wrong thing.

### 2. (Likely bug) A failed qualified lookup now falls back to unqualified scope

`canonicalizeQualifiedIdentExpr` changed from

```zig
return try self.canonicalizeModuleQualifiedIdent(module_name, ident, region, qualifier_tokens);
```

to

```zig
if (try self.canonicalizeModuleQualifiedIdent(module_name, ident, region, qualifier_tokens)) |expr| {
    return expr;
}
...
return try self.canonicalizeUnqualifiedIdentExpr(ident, region, .{ … });
```

`canonicalizeUnqualifiedIdentExpr` starts with `switch (self.scopeLookup(.ident, ident))`
and, on `.found`, returns a lookup of that local binding. So when the module
resolves but the value doesn't, the compiler now **searches the current module's
scope for the bare name**.

Concretely:

```roc
import pf.Stdin      # Stdin exists but does not expose `read!`

read! = |_| "local"

main! = || Stdin.read!()   # ← resolves to the LOCAL `read!`?
```

If that's what happens, a qualified reference silently binds to an unrelated
local — a wrong-program-accepted bug, not just a worse error message.

I couldn't fully confirm it without reading `canonicalizeModuleQualifiedIdent`'s
new null conditions, which the diff doesn't show. Two things would settle it:

- state in the code (and ideally a comment at the call site) exactly when
  `canonicalizeModuleQualifiedIdent` returns `null`, and
- add a snapshot for the shadowing case above.

If the fallback is only meant to reach the *diagnostic* path in
`canonicalizedIdentNotFoundExpr`, then it should call that directly rather than
re-entering the full unqualified resolver, which does scope lookup, exposed-value
lookup, and declaration-scope walking before getting there.

### 3. (Design) The suggested package shorthand can be confidently wrong

```zig
.app => self.header_suggested_package = self.headerPackageShorthand(h.platform_idx) orelse
    self.firstHeaderPackageShorthand(h.packages),
.package, .platform => self.header_suggested_package = self.firstHeaderPackageShorthand(h.packages),
```

For an app the platform shorthand is a good guess. For a `package` or `platform`
header with several dependencies, **"the first one"** is arbitrary. A package
with `{ json: "...", parser: "...", http: "..." }` that references an
unimported `Decode` will be told:

```
    import json.Decode
```

which is a specific, confident, and quite possibly wrong instruction. A user who
follows it gets a second error.

Error reporting is explicitly exempt from the AGENTS.md heuristic ban, so this
isn't a rule violation — but "one plausible package" and "three packages, picked
the first" deserve different treatment. Suggest falling back to bare
`import Stdin` when the header declares more than one package, or listing the
shorthands (`import <pkg>.Stdin`, where `<pkg>` is one of …). A vaguer suggestion
beats a wrong one.

### 4. (Minor) The suggestion is interned into the ident store during canonicalization

```zig
const suggested_import = if (self.header_suggested_package) |package|
    try self.insertQualifiedIdent(self.env.getIdent(package), self.env.getIdent(module_alias))
else
    module_alias;
```

`insertQualifiedIdent` interns a brand-new ident (`"pf.Stdin"`) into the module's
ident store, purely so an error report can print it — and it happens eagerly at
canonicalization time, whether or not the report is ever rendered.

Two smaller options: store the two component idents in the diagnostic payload
and format them at report time (`ModuleEnv.zig` already does
`std.fmt.allocPrint` for `import {s}` there anyway), or keep the interning but
note in a comment that it's deliberate. PR #9245 has the same pattern in
`isMethodOnNominalType` and solved it by deferring to the failure path; the same
idea applies.

### 5. (Minor) `value_region` uses a magic `+1` to skip the dot

```zig
const raw_value_region = self.parse_ir.tokens.resolve(e.token);
const value_region = if (raw_value_region.end.offset > raw_value_region.start.offset)
    Region{ .start = .{ .offset = raw_value_region.start.offset + 1 }, .end = raw_value_region.end }
else
    raw_value_region;
```

`+1` skips the `.` in a `NoSpaceDotLowerIdent` token. Correct today, undocumented,
and silently wrong if the token's text ever gains a character. A one-line comment
("the token text is `.name`; skip the leading dot") would make it maintainable —
or better, take the region from the parser rather than reconstructing it by
arithmetic.

### 6. Serialization changes are handled correctly

`Diagnostic.qualified_ident_does_not_exist` gains a `context` union, which means
new CIR node payloads. The PR correctly follows through:

- `Node.zig` +8, `NodeStore.zig` +56/−5 for the round-trip,
- `node_store_test.zig` +15 for a round-trip test,
- `cache_config.zig` `CACHE_VERSION` bump and `cache_module.zig` golden hash
  update,
- `serialization_size_check.zig` adjusted.

That's the full checklist and none of it was skipped. ✓

### 7. Report structure

The report is assembled as **three separate `switch (data.context)` blocks** —
one for the headline, one for the pre-region body, one for the post-region body.
It works, and it keeps each block next to the layout element it contributes to,
but a reader has to hold three switches in their head to know what a
`.missing_exposed_value` report looks like end to end. Consider one switch with
two arms, each building its whole report.

Also: the `.missing_exposed_value` headline changes from
`` `Stdin.line!` does not exist. `` to `` `line!` was not found in `Stdin`. `` —
a nicer message, but note the report's *title* stays `DOES NOT EXIST`, which now
reads oddly against "was not found in". Worth considering `NOT EXPOSED` for that
variant.

## Merge-order warnings

- **#10410 conflicts directly.** It adds `Num.to_str` / `Inspect.to_str` hints to
  the *same* `.qualified_ident_does_not_exist` arm this PR restructures into two
  context variants. Whoever merges second has to decide which variant those hints
  belong to — and it isn't obvious: `Num` *is* a type in scope, so
  `Num.to_str` may not take the `missing_module_or_type` path at all. Worth
  checking that #10410's snapshot still shows its hint after this lands.
- **#10643** regenerates every snapshot in `test/snapshots/`; this PR touches 24
  of them.

## Process nits

- The PR body ends with a `🤖 Generated with [Claude Code]` trailer and a
  `claude.ai/code/session_…` link. Per the repo's conventions neither belongs in a
  PR description.
- The body reports that `json_decoder` / `http_header_decoder` platform steps
  fail with a pre-existing ARC-certifier panic, verified byte-identical on clean
  `main`. That's a useful observation and should be filed as an issue rather than
  left in a draft PR body — same note as on #10043, which independently
  rediscovered platform-test failures.
