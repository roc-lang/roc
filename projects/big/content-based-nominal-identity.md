# Content-Based Canonical Identity for Nominal Types and Methods

## Problem

A nominal type in this compiler has no single identity. What it has is a
bundle of name strings — a module name, a display module name, a
package-qualified module name, a type name, and sometimes a statement index —
interned separately into every artifact that mentions the type, and re-matched
BY TEXT at every artifact boundary. Whether `Json.Value` from one package is
the same type as `Json.Value` from another is decided by string comparison,
and the strings being compared were assigned by whichever code path happened
to build that artifact (build, check, run, glue, LSP — each with its own
naming scheme; see `../small/single-package-identity-function.md`).

Concretely, `MethodOwner` in `src/check/static_dispatch_registry.zig` is a
union over `NominalTypeKey` (`module_name: ModuleNameId`, `type_name:
TypeNameId`, `source_decl: ?u32` — a statement index) and a `source_decl`
variant of (`ModuleNameId`, statement index). Those `NameId`s are per-artifact
interner indices, so every cross-artifact lookup round-trips through text:
`methodOwnerInImportedNames` calls
`imported_names.lookupModuleName(source_names.moduleNameText(...))`; monotype
lowering re-interns every name and resolves a type's declaring module by
trying `Ident.textEql` against three different spellings of each candidate
module's name.

This has produced a sustained bug cluster: types that should unify and don't,
dispatch that resolves to the wrong module or nothing, cache keys polluted by
per-run strings, and glue indexing the wrong artifact by name. Every fix so
far has patched one text-matching site; the sites keep multiplying.

## Background

The compiler pipeline is: parse → canonicalize → type-check (producing
"checked artifacts" per module) → postcheck: Monotype IR (monomorphization,
`src/postcheck/monotype/`) → Monotype Lifted (closure lifting,
`src/postcheck/monotype_lifted/`) → Lambda Solved → Lambda Mono → LIR
lowering → ARC insertion → backends. `design.md` at the repo root is the
authoritative design. Two parts of it govern this project:

- The Core Principles paragraph "Identity provenance follows meaning
  provenance": an identity may be derived from module content (module name,
  source bytes, identities of imports) only for definitions whose entire
  meaning is determined by that content; externally-bound definitions (hosted
  functions, `provides` entrypoints) are identified by their platform-header
  symbol strings instead, and may never be merged.
- The Cache Boundary section: the checked module cache id is `CheckedModuleId
  = source_hash + compiler_build_hash + module_identity +
  checking_context_identity + direct_import_checked_module_ids`, and
  `module_identity` includes the module's name because a type module's main
  type takes its name from the module's file name.

Caching constraint (this document must not violate it anywhere):
canonicalization output must be serializable keyed on a pure function of
(module name, module source bytes) and NOTHING else; type-checking output is
keyed on that hash combined with the type-checked hashes of the module's
imports — a Merkle DAG (`CheckedModuleArtifactKey` in
`src/check/checked_artifact.zig` already has this shape, including
`direct_import_artifact_keys_hash`). Any id serialized in a stage's artifact
must be a deterministic function of that stage's cache-key inputs. Global
uniqueness comes from pairing module-local ids with module identity, never
from global counters or coordinator state. Name-based resolution is
legitimate in exactly one place: resolving imports at the canonicalize→check
boundary; everything downstream consumes resolved ids.

## Evidence

- `MethodOwner` and `methodOwnerInImportedNames`,
  `src/check/static_dispatch_registry.zig`: cross-artifact owner remapping via
  `lookupModuleName(moduleNameText(...))` / `lookupTypeName(typeNameText(...))`.
- `NominalTypeKey`, `src/check/canonical_names.zig`: (`module_name`,
  `type_name`, `source_decl: ?u32` statement index).
- `moduleViewNameMatches` and `moduleDigestForOrigin`,
  `src/postcheck/monotype/lower.zig`: declaring-module resolution by
  `Ident.textEql` against three name spellings (`module_name`,
  `display_module_name`, `qualified_module_name`); `methodOwnerInProgramNames`
  re-interns owner names per program.
- `sameNominalIdentity`, `src/check/unify.zig`: compares `origin_module`
  (an `Ident.Idx` of a coordinator-assigned qualified name string), then
  `source_decl` / ident. `ownerEnvForOriginModule` in `src/check/Check.zig`
  resolves owner envs from `origin_module` ident text.
- `computeStableModuleIdentityHash`, `src/check/checked_artifact.zig`: hashes
  the module name plus display and `qualified_module_ident` strings — the
  latter assigned as `"{package_name}.{module_name}"` in
  `src/compile/coordinator.zig`, where `package_name` is computed
  independently by at least three call sites
  (see `../small/single-package-identity-function.md`).
- Associated items resolve through concatenated qualified-name strings
  (`insertQualifiedIdent` in `src/canonicalize/ModuleEnv.zig` builds
  `"{parent}.{child}"`; PR 9714 fixed exposed nested-type associated items by
  adjusting this string plumbing).
- `moduleEnvNamesMatch`, `src/compile/compile_package.zig` (introduced around
  PR 9430): dedups owner envs by comparing qualified/display/bare name strings
  with fallbacks.
- Bug history: PR 9627 fixed "Json.Value is not Json.Value" (same package
  under two aliases produced non-unifying nominal types); issue 9875 (static
  dispatch lost through `ThingAlias : Thing` re-exports); issue 9864
  (platform methods on package-owned nominals); issue 9865 / PR 9894 (glue
  resolved a declaring module by name text, fell back to the wrong artifact,
  and indexed a statement of the wrong kind — `src/glue/glue.zig` still does
  module-name prefix matching on def names); PR 9811 (per-run temp-dir-derived
  package identity broke cache keys for default apps).

## Solution design

Identity becomes content: a nominal type or method owner is identified by
**(deep content hash of its declaring module, declared name)**.

The deep hash is `H(module name ‖ source bytes ‖ identity hashes of resolved
imports)` — SHA-256, the codebase digest standard (used throughout
`src/postcheck/monotype/type.zig` and the cache keys). It is a pure function
of the transitive closure's module names and source bytes, so it satisfies the
caching constraint, and it is computable at import-graph load time, BEFORE
type-checking begins — the unifier only ever compares precomputed identities.
It mirrors the Merkle discipline of `CheckedModuleArtifactKey` but excludes
`compiler_build_hash` and checking-context identity: those invalidate caches;
they do not change what a type *is*. The module name must be part of the hash:
per design.md's Cache Boundary section, a type module's main type takes its
name from its file name, so source bytes alone underdetermine meaning.

Deep (not shallow) hashing is required for soundness, not just hygiene: two
byte-identical modules over different transitive dependencies could otherwise
share identity while their field layouts differ. This is reachable in
practice because PR 9627's resolver allows different major versions of a
package to coexist in one build.

The local component is the declared **name**, not a statement index. The
current `source_decl: ?u32` statement index breaks under declaration
reordering and was part of 9894's fragility (glue indexed a statement of the
wrong kind in the wrong artifact). A decl name is unique within its module
namespace and stable under reordering.

Representation and memory:

- 32-byte hashes exist only at rest and at artifact boundaries. In memory,
  every identity is a dense `u32` (`IdentityId`) into a per-build identity
  table: an append-only array of 32-byte hashes plus a hash→id map. 10k
  distinct types ≈ 0.5 MB. Optionally truncate to 128 bits for in-memory map
  keys only; serialized bytes stay 256-bit.
- Serialized artifacts store a local identity table (array of 32-byte hashes)
  plus `u32` references into it — never inline hashes at each use site.
- Artifact load performs a **rebase**: one map lookup per distinct identity
  per artifact, producing an old-u32 → build-u32 remap array. Rebase is the
  SINGLE cross-artifact identity resolution point in the whole compiler,
  replacing every text-remapping site. The map compares full 256-bit values
  on insert, so a hash collision is detected, not silently merged.

Exception, per design.md "Identity provenance follows meaning provenance":
hosted functions and `provides` entrypoints are identified by the symbol
strings in the platform header, NEVER by content hash. design.md's Debug
Invariants section already requires that no deduplication or merging step
collapse two `hosted` or `provides` identities even when their declaring
modules are byte-identical. This project must preserve that carve-out
verbatim.

Semantic consequence to document in user-facing docs: two types whose
declaring modules have byte-identical transitive closures unify — across
package versions, mirror URLs, and vendored copies. This is desirable:
a type that did not change interoperates across a version bump.

Migration order:

1. Compute the deep hash bottom-up during import-graph loading in the
   coordinator (sources and the import DAG are already in hand there).
   Store it on `ModuleEnv` / the checked artifact's module identity.
2. Replace the string inputs of `computeStableModuleIdentityHash` with the
   deep hash. This absorbs `../small/single-package-identity-function.md`:
   package display names stop participating in identity entirely.
3. Introduce the per-build identity table and `IdentityId`. Change
   `types.NominalType.origin_module` from `Ident.Idx` to `IdentityId`;
   `sameNominalIdentity` becomes integer equality on (identity, decl name).
4. Change `NominalTypeKey` / `MethodOwner` to (`IdentityId`, decl-name id),
   dropping the statement index. Static-dispatch lookups become exact keys.
5. Add rebase to artifact load; route monotype lowering, glue, and the LSP
   through rebased ids.
6. Delete the text paths (below) and add a debug assertion that no post-check
   stage calls any name interner for identity purposes.

What gets DELETED:

- `methodOwnerInImportedNames` (`src/check/static_dispatch_registry.zig`).
- `moduleViewNameMatches` and the three-spelling `textEql` matching in
  `moduleDigestForOrigin` (`src/postcheck/monotype/lower.zig`).
- Concatenated qualified-name ("M.T.method"-style) probing for associated
  items in canonicalize/check resolution paths (PR 9714's plumbing).
- `moduleEnvNamesMatch` (`src/compile/compile_package.zig`).
- Glue's module-name/prefix probing in `src/glue/glue.zig`.

## What success looks like

- `grep` finds zero call sites that compare module or type name TEXT to
  decide identity anywhere downstream of import resolution.
- `sameNominalIdentity` is two integer comparisons; no ident-store access.
- The same module reached via check, run, glue, and LSP pipelines produces
  bit-identical serialized identities.
- Cache keys contain no coordinator-assigned display strings and no paths.
- Issues 9875, 9864, and 9865 repros pass; PR 9627's two-alias scenario
  unifies; PR 9811's scenario gets cache hits across runs.
- Two byte-identical hosted modules wired to different platform symbols
  remain distinct identities.

## How to evaluate the result

### Correctness ideal

Identity equality is decidable by a single integer compare whose answer never
depends on which pipeline built the artifact, what the coordinator named a
package, what directory the build ran in, or declaration order within a
module. Every cross-artifact identity question is answered at rebase time or
not at all. A collision between distinct 256-bit hashes is detected at rebase
and reported as a compiler bug rather than merging types.

### Performance ideal

Given perfect correctness, the ceiling is: zero per-comparison string work in
unification and dispatch (measure unifier time on a workspace with many
cross-package nominal types, before/after); rebase cost linear in distinct
identities per artifact, one hash-map probe each (measure artifact load time);
identity table memory ~32–48 bytes per distinct type (measure RSS delta on a
10k-type synthetic workspace). Deep-hash computation is amortized into I/O at
load time and must not appear in check-phase profiles.

## Tests to add

- Cross-package unification: the same package content fetched via two
  different URLs; a nominal type from each must unify.
- Version coexistence: two majors of a package in one build (per PR 9627's
  resolver); unchanged modules unify across versions, changed ones do not.
- Repros for issues 9875 (alias re-export dispatch), 9864 (platform methods
  on package-owned nominals), 9865 (glue on package nominal API, extending
  `test/glue/package-nominal-api/`).
- Cache-key purity: build the same default app from two different temp
  directories under two coordinator naming schemes; identity bytes and cache
  keys are bit-identical (PR 9811's repro).
- Hosted distinctness: two byte-identical hosted modules bound to different
  platform-header symbols stay distinct through every merging pass.
- Decl reordering: reordering declarations in a module changes no identity
  except via the module's own source hash input.

## Related projects

- `../small/single-package-identity-function.md` — interim fix; absorbed by
  this project (step 2 above).
- `../big/total-dispatch-plans.md` — dispatch plans consume these identities.
- `../big/immutable-specialization-identity.md` — specialization keys build on
  stable nominal identity.
- `../small/glue-consumes-committed-layouts.md` — glue stops re-deriving what
  identity plus committed layouts already state.
