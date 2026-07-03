# One Package-Identity Function for Every Pipeline

## Problem

The string that names a package — "app", "pf", a resolved package identity, a
shorthand alias — is an input to type identity and to the checked-module cache
key. It is computed independently, with different rules, by at least three
places in the compiler. Nothing enforces that they agree except a code
comment. When they disagree, the failure is silent and structural: the same
module gets two different identities (types stop unifying, dispatch misses)
or two different cache keys (cache never hits), depending on which entry
point — build, check, run, glue, LSP — happened to compile it.

This has already produced shipped bugs in both directions. Alias-keyed
identity merged unrelated packages that shared a shorthand and split one
package imported under two aliases into non-unifying nominal types (fixed by
PR 9627's global version resolution). Then the run path derived a default
app's package identity from a per-run temp staging directory, so its cache
key never matched across runs (fixed by PR 9811 hand-mirroring the
coordinator's naming). The mirror is still by hand today.

## Background

The compiler pipeline is: parse → canonicalize → type-check (producing
"checked artifacts" per module) → postcheck: Monotype IR (monomorphization,
`src/postcheck/monotype/`) → Monotype Lifted (closure lifting,
`src/postcheck/monotype_lifted/`) → Lambda Solved → Lambda Mono → LIR
lowering → ARC insertion → backends. `design.md` at the repo root is the
authoritative design; its Cache Boundary section defines the checked-module
cache id as `CheckedModuleId = source_hash + compiler_build_hash +
module_identity + checking_context_identity +
direct_import_checked_module_ids`, and its Core Principles include "Identity
provenance follows meaning provenance."

How the package name reaches identity today: when the coordinator creates a
module's `ModuleEnv`, it sets `env.qualified_module_ident` to
`"{package_name}.{module_name}"` (`src/compile/coordinator.zig`, parse-task
setup). `computeStableModuleIdentityHash` in `src/check/checked_artifact.zig`
hashes that string (with the bare and display names) into the module identity
hash, which feeds both the checked-module cache key
(`CheckedModuleArtifactKey`) and nominal-type origin identity — the
`origin_module` ident compared by `sameNominalIdentity` in
`src/check/unify.zig` is this qualified name.

Caching constraint (stated here because this project touches cache-key
inputs): canonicalization output must be serializable keyed on a pure
function of (module name, module source bytes) and NOTHING else;
type-checking output is keyed on that hash combined with the type-checked
hashes of imports — a Merkle DAG. Any id serialized in a stage's artifact
must be a deterministic function of that stage's cache-key inputs. Global
uniqueness comes from pairing module-local ids with module identity, never
from global counters or coordinator state. Name-based resolution is
legitimate in exactly one place: resolving imports at the canonicalize→check
boundary; everything downstream consumes resolved ids. A package display
string computed by a coordinator violates this constraint in spirit; this
project cannot remove it (that is
`../big/content-based-nominal-identity.md`), but it can make it a single
deterministic function so every pipeline computes the same value.

## Evidence

The three independent computation sites, each with its own scheme:

- BuildEnv package materialization, `src/compile/compile_build.zig`: names
  the root package `"app"` for executables and `"module"` otherwise
  (`discovered_pkg_name`), registers dependency packages under their
  workspace shorthand aliases via `ensurePackage`, and uses `"pkg"` for a
  generated package in one path.
- `Coordinator.discoverAppFromPath`, `src/compile/coordinator.zig`: names the
  root `"app"`, names the platform package `"pf"` (via
  `registerPlatformPackage` — see its doc comment), and registers inline
  packages under their header shorthands.
- The run path, `lowerLirWithCoordinator` in `src/cli/main.zig`: after
  PR 9627 it used `package.identity` from version resolution — which, for
  default apps, derived from a per-run temp staging dir, so cache keys never
  matched across runs. PR 9811 fixed it by hand-building a `package_names`
  array mapping root→`"app"` and platform deps→`"pf"`, with a comment
  ("matching Coordinator.discoverAppFromPath") as the only enforcement.

Consumers: `env.qualified_module_ident` assignment in
`src/compile/coordinator.zig`; `computeStableModuleIdentityHash` and
`CheckedModuleArtifactKey` in `src/check/checked_artifact.zig`;
`sameNominalIdentity` in `src/check/unify.zig`; `moduleEnvNamesMatch` in
`src/compile/compile_package.zig` (dedups owner envs by these strings).

Bug history: PR 9627 (version-resolution) — alias-keyed identity both merged
unrelated packages and split same-package-two-aliases into non-unifying
nominal types; PR 9811 (fix/default-app-checked-cache) — per-run temp-dir
identity meant issue 9788's default apps missed the cache on every run.

## Solution design

One function, one owner. Add to `src/compile` (or `src/base` if the CLI must
call it without depending on compile):

```zig
pub const PackageIdentity = struct {
    /// Stable identity string used in qualified module idents and
    /// therefore in module identity hashes and cache keys.
    identity: []const u8,
    /// Short display name for diagnostics ("app", "pf", "json").
    shorthand: []const u8,
};

pub fn packageIdentityFor(role: PackageRole, resolved: ?ResolvedPackage)
    PackageIdentity;
```

where `PackageRole` distinguishes: root app, root non-app module, platform of
the root app, URL dependency (uses the resolver's content-derived
`package.identity`), path dependency, and synthesized/default platform.
Rules:

1. Root app is always `identity = "app"`; the root platform is always
   `identity = "pf"`. These are the defined synthetic identities.
2. URL packages use the version-resolver identity (content-derived), never
   the shorthand alias under which a workspace imported them.
3. No rule may consult an absolute path, a temp directory, or any per-run
   state. The default/synthesized platform gets a fixed synthetic identity,
   not one derived from its staging location.
4. The function is total: every entry point (BuildEnv, coordinator discovery,
   the run path, glue, LSP, playground, future ones) calls it; none computes
   a name locally.

Migration order:

1. Add the function with exhaustive tests over `PackageRole`.
2. Convert `Coordinator.discoverAppFromPath` and `registerPlatformPackage`
   to call it.
3. Convert BuildEnv materialization in `src/compile/compile_build.zig`.
4. Convert `lowerLirWithCoordinator` in `src/cli/main.zig`; DELETE the
   hand-built `package_names` array and its "matching
   Coordinator.discoverAppFromPath" comment.
5. Audit glue (`src/glue/glue.zig`) and the LSP for locally computed package
   names; route them through the function.

What gets DELETED: the `package_names` array construction in
`src/cli/main.zig`; the literal `"app"`/`"pf"`/`"module"` strings at each
former site (they move inside the one function).

Scope statement: this is the interim fix.
`../big/content-based-nominal-identity.md` supersedes it by removing display
strings from identity entirely — identity becomes a content hash and the
shorthand becomes diagnostics-only. This project is still worth doing first:
it is days-scale, it immediately stops the "three sites drift" bug class, and
it leaves exactly one call site to rewrite when content-based identity lands.

## What success looks like

- Exactly one function in the tree returns a package identity or shorthand;
  every pipeline entry point calls it.
- `grep -rn '"pf"' src` and `grep -rn '"app"' src` (as package names) match
  only that function and tests.
- A default app compiled twice in a row gets a checked-module cache hit on
  the second run.
- The same module compiled via `roc check` and via `roc run` produces the
  same `computeStableModuleIdentityHash` output.
- No cache-key input contains an absolute path or temp-dir component.

## How to evaluate the result

### Correctness ideal

Package identity is a deterministic function of (role, resolved package
identity) — nothing else. Two pipelines given the same workspace can never
disagree on any package's identity, so no module can acquire two identities
and no cache key can silently change between entry points. Drift is
structurally impossible rather than comment-enforced.

### Performance ideal

Given perfect correctness: cache hit rate for unchanged default apps is 100%
across consecutive runs (measure: second-run wall time for issue 9788's
repro app approaches cache-hit floor). The function itself is negligible
(string selection, no I/O); the win to measure is eliminated recompilation —
compare cold vs warm `roc run` times before/after on a default app and on a
multi-package workspace.

## Tests to add

- Issue 9788 repro: default app, run twice, assert the second run loads the
  checked module from cache (cache-hit counter or log assertion).
- Identity convergence: compile the same module through the check pipeline
  and the run pipeline; assert identical
  `computeStableModuleIdentityHash` values.
- Purity: build the same app from two different temp working directories;
  assert bit-identical cache keys, and assert no cache-key input string
  contains either directory's path.
- Unit tests for `packageIdentityFor` covering every `PackageRole`,
  including the synthesized default platform.

## Related projects

- `../big/content-based-nominal-identity.md` — supersedes this project by
  removing name strings from identity entirely; do this one first.
