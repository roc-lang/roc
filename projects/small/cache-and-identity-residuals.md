# Close the Cache and Identity Residual Seams

## Problem

The identity and cache-hardening work of July 2026 (centralized package
identity, `owner_module` carried on checked named types, comptime
serialization contracts) retired the large generators in this area. A
July 11 audit of what survived found four small seams — each individually
minor, each the exact pattern that produced the last round of bugs, and
each cheap to close while the context is fresh.

1. **A name-text fallback in auto-import synthesis.**
   `getOrCreateAutoImportedTypeImport` (`src/canonicalize/Can.zig:787`)
   resolves a package-qualified type's source module through the
   authoritative scope binding (`scopeLookupModule`) — PR #9902's fix —
   but keeps an `else` arm that falls back to the raw alias text
   (`source_module_ident`) when the scope lookup misses. A scope miss
   means the binding does not exist; synthesizing an import from the
   alias's spelling instead of diagnosing is exactly the name-keyed
   resolution the fix was removing, one branch over.
2. **Serialization contracts enroll by hand.**
   `assertBidirectionalFieldSet` (`src/collections/serde_validation.zig:40`)
   makes owner/`Serialized` drift a compile error — but only for structs
   that remember to invoke it with a hand-maintained allowlist. Two
   top-level structs are enrolled today (`CheckedModuleArtifact`,
   `ModuleEnv.Serialized`); a third hand-written serialization root added
   next month starts unguarded, reopening the silently-dropped-field
   generator (the pre-#9978 `exhaustiveness_sites` bug shape).
3. **Three parallel version-hash helpers.**
   `computeCheckedModuleEntryVersionHash` (`src/compile/coordinator.zig:218`),
   `layoutVersionHash` (`src/check/artifact_serialize.zig:366`), and
   `cache_module.computeVersionHash` (`src/compile/cache_module.zig:27`)
   compose correctly today, but the composition rule (what folds into the
   cache admission check, in what order) exists only as the bodies of
   three functions in three files. A fourth fingerprint added to one and
   not the composition is a silent stale-cache-hit bug.
4. **`type_name` rides in nominal identity keys.**
   `checkedNominalTypeKey` (`src/check/checked_artifact.zig:2916`) builds
   `canonical.NominalTypeKey` with a `type_name` component alongside the
   owner digest and source decl. The digest components already determine
   the type; the name component is either redundant (harmless but
   identity-by-name-smelling) or load-bearing (in which case renaming a
   type changes identity in a way the digest components would not). Which
   one it is has never been established.

## Background

design.md's Cache Boundary section states the contract: ids serialized in
artifacts are deterministic functions of stage inputs; name-based
resolution is legitimate only at canonicalization import-resolution and
at the symbol-keyed host boundary. Items 1 and 4 are about the last
name-shaped facts near identity; items 2 and 3 are about keeping the
comptime cache contracts as airtight as #9951/#9978 intended.

## Evidence

- `src/canonicalize/Can.zig:787` — the `orelse`/`else source_module_ident`
  arm (PR #9902 diff retained it).
- `src/collections/serde_validation.zig:40` — enrollment is a manual
  comptime call per struct; nothing enumerates serialization roots.
- The three version-hash helpers cited above; the admission fold lives in
  `computeCheckedModuleEntryVersionHash` alone.
- `src/check/checked_artifact.zig:2916, 5785-5792` — `type_name`
  constructed into and read out of the key payloads.

## Solution design

1. Make the scope-miss arm a diagnostic (or an invariant, if
   canonicalization order provably guarantees the binding exists —
   determine which, and write the proof as the comment). DELETE the text
   fallback either way.
2. Auto-enroll serialization roots: a comptime registry (one list of
   every hand-written `Serialized` pair) that `serde_validation` iterates,
   so adding a serialization root means adding it to the registry — and a
   root that serializes without registering is itself detectable (the
   cache write path accepts only registered types).
3. One `cache_versioning` module owning the composition: the three
   helpers become one exported function each for their fingerprint, plus
   one composition function that the coordinator's admission check calls.
   Adding a fingerprint without threading it into admission becomes
   impossible because the composition is the only consumer.
4. Resolve `type_name`'s status: write the test that renames a nominal
   type (same structure, same decl position) and observe whether identity
   must change (associated-item resolution, diagnostics, glue naming).
   If redundant: remove it from the key, keep it as display metadata. If
   load-bearing: document at the key definition why a name belongs in
   identity, citing the consumer that needs it.

## What success looks like

Every criterion below must hold; the project is not done until all do:

- `grep -n "source_module_ident" src/canonicalize/Can.zig` shows no
  fallback arm — the scope-miss path is a diagnostic or a documented
  invariant, with a test for whichever it is.
- A new hand-written `Serialized` struct cannot ship unguarded:
  demonstrated by adding a dummy unregistered root in a test build and
  observing the failure.
- One composition function for cache admission; the coordinator calls
  it; `grep` finds no other site folding version fingerprints.
- `type_name`'s key status is resolved with the rename test in-tree and
  a comment at the key definition stating the verdict and its consumer
  (or the field removed).
- Fresh-vs-cached byte-identity tests (the #9883-era ones) stay green
  through all four changes.

## How to evaluate the result

### Correctness ideal

The cache contracts hold by construction for future code, not only for
the structs that existed when the contracts were written. No identity
component's purpose is undocumented.

### Performance ideal

Neutral: these are comptime/structure changes. Cache hit rates
unchanged (assert via the existing cache CLI tests).

## Tests to add

- Scope-miss behavior test (diagnostic or invariant).
- Unregistered-serialization-root compile failure.
- The nominal-rename identity test.

## Related projects

- Carry the platform-app relation from checking — landed: checking records
  requirement solutions in the app's checked artifact and finalization
  consumes them, so the one *large* name-keyed resolution this doc's item 1
  is the small sibling of no longer exists.
- [../small/silent-drift-guards.md](../small/silent-drift-guards.md) —
  same philosophy: every mirrored or composed fact gets a structural
  guard.
