# Cache Hardening at the Enforcement Edges

## Problem

PR 9720 gave the checked-module cache a mechanically enforced
serialization framework: comptime pointer audits, a strict bidirectional
field-set audit over every slice-backed sub-store, a load-time structural
layout fingerprint, and bounds validation of relocation markers. Inside
that framework, forgetting to serialize a field is a compile error and a
corrupt blob is a clean cache miss.

The enforcement stops at four edges, and each edge has either already
shipped a bug or is one refactor away from one:

- (a) The hand-written ~34-field top-level artifact `Serialized` struct
  has NO field-set audit. `exhaustiveness_sites` is a live proof: it is a
  field of `CheckedModuleArtifact` but absent from `Serialized`, so every
  cache-loaded artifact silently gets an empty table — while
  `ImportedModuleView` still exposes it to postcheck consumers whose
  mismatch handling is a `finalizationInvariant` panic. Historically the
  same gap shipped dead-stub deserialization for dispatch-plan tables
  (fixed during PR 9459's wiring).
- (b) The `ModuleEnv` blob inside a cache entry has NO layout-version hash
  and NO bounds-validation pass. PR 9768 removed body checksums as
  redundant with artifact-side checks — correct for the artifact blob,
  but the env blob now has neither a checksum nor bounds checking. An
  env-layout change without a manual format bump relocates stale bytes
  into a mismatched struct.
- (c) The layout fingerprint hashes DECLARED structure (field names and
  type names), not `@sizeOf`/`@offsetOf`/endianness, and the cache
  directory is compiler-version-scoped but not arch-scoped. A cache
  directory shared across architectures (network home dir, container
  volume) could admit a blob whose bytes mean something else.
- (d) Cache failures are silent. Every load/store error degrades to
  compile-from-source with only `CacheStats` counters (printed only under
  verbose); one failure path records no stat at all. A systematic store
  failure — read-only cache dir, serialization bug — looks like a slow
  compiler forever. That is exactly how issue 9788 presented.

## Background

The checked-module cache stores one entry per type-checked module under
`~/.cache/roc/<version>/mod/`, written via atomic temp+rename
(`CacheManager.storeRawBytes`, `src/compile/cache_manager.zig`). The key
is a Merkle DAG: SHA-256 over the module's source hash (including
ingested deps), the compiler artifact hash, the module identity hash, the
checking-context hash, and the ordered artifact keys of its direct
imports — so a hit implies matching imports by construction. `design.md`'s
"Cache Boundary" section is authoritative: cache reads validate only the
header, format version, key, serialized layout, and ordinary binary
decoding; they must not re-run checked validation.

Both build-orchestration paths feed this cache — `roc check`/`roc build`
via `BuildEnv` (`src/compile/compile_build.zig`) and the run path via
`lowerLirWithCoordinator` (`src/cli/main.zig`); both wrap the
`Coordinator` (`src/compile/coordinator.zig`), which owns the load/store
functions this document hardens.

An entry is `[header][env blob][artifact blob]`. The header
(`writeCheckedModuleCacheHeader` / `decodeCheckedModuleCacheEntry` in
`src/compile/coordinator.zig`) is: magic, format version (u64), the
artifact layout hash `SERIALIZED_VERSION_HASH` (32 bytes), the artifact
key (32 bytes), and the two body lengths. PR 9720's relocate-on-load
framework then materializes the artifact blob with memcpy + pointer
fixups, with these mechanical guarantees (`src/check/artifact_serialize.zig`):

- `assertSerializedRelocatable` — comptime walk proving every field is a
  relocatable marker or relocation-invariant POD.
- `SliceStoreSerde(Store, Serialized)` — a comptime BIDIRECTIONAL
  field-set audit: a field present in one struct but unhandled in the
  other is a compile error. Used by the slice-backed sub-stores
  (`ConstStore`, `StaticDispatchPlanTable`, `CanonicalNameStore`, ... —
  on the order of 29 across the artifact).
- `layoutVersionHash` — a comptime structural fingerprint of the
  `Serialized` type tree (plus a manual bump discriminant and the
  relocatable-pointer count), stored in the header and checked on load.
- `validate` (the "L-10" pass) — bounds-checks every relocation marker's
  `(offset, len)` against the blob before any slice aliases it; the
  artifact load path calls it (`installCachedCheckedArtifact`).

PR 9768 then removed body checksums as redundant with the above — the
header comment in `coordinator.zig` documents the reasoning. The removal
is sound exactly where the above enforcement applies; the edges below are
where it does not.

## Evidence

- (a) `src/check/checked_artifact.zig`: `CheckedModuleArtifact` declares
  `exhaustiveness_sites: CheckedExhaustivenessSiteTable = .{}` but the
  top-level `pub const Serialized` (~34 fields, `key` through
  `const_store`) has no such field; `serialize` and
  `deserializeWithBacking` never touch it, so the default `.{}` survives
  every cache load. `ImportedModuleView` (same file) exposes
  `exhaustiveness_sites: *const CheckedExhaustivenessSiteTable` built from
  `&artifact.exhaustiveness_sites`. Consumers in
  `src/eval/compile_time_finalization.zig` (`comptimeSiteMayResolvePending`,
  `discardUnreachedRootComptimeSites`, and the empirical-exhaustiveness
  path) call `.get(checked_site)` on it and treat inconsistency as
  `finalizationInvariant` panics ("compile-time exhaustiveness failure had
  an impossible site policy"). The only existing top-level comptime check
  is `relocatablePointerCount(Serialized) == 181`, which audits fields
  that ARE present — it cannot see an absent one.
- (b) `decodeCheckedModuleCacheEntry` checks the env body only for
  `env_body.len < @sizeOf(ModuleEnv.Serialized)`; the load path
  (`installCachedCheckedArtifact` in `src/compile/coordinator.zig`) then
  memcpys the blob and calls `ModuleEnv.Serialized.deserializeWithMutableTypes`
  (`src/canonicalize/ModuleEnv.zig`) with no marker bounds pass and no
  env-layout hash in the header. A suitable hash already exists:
  `MODULE_ENV_VERSION_HASH` in `src/compile/cache_module.zig` (validated
  by the separate `CacheModule` path) — the checked-module entry header
  simply does not include it.
- (c) `serializedLayoutFingerprint` in `src/check/artifact_serialize.zig`
  hashes field names and `@typeName` for scalars ("the name fully
  captures its size/representation" — untrue for `usize` across
  pointer widths) and never folds `@sizeOf`, `@offsetOf`, or endianness.
  `src/compile/cache_config.zig`: `getVersionCacheDir` /
  `getCompilerVersionDir` scope the cache by
  `build_options.compiler_version` only — no target/arch component.
- (d) `CacheStats` (`src/compile/cache_config.zig`) counts hits, misses,
  invalidations, stores, and store failures; `CacheManager.printStats`
  and `verboseLog` are gated on `config.verbose`. In
  `Coordinator.tryLoadCachedCheckedModule`, the key-derivation failure
  path is `self.checkedModuleCacheKey(...) catch return false` — no stat
  recorded at all. Issues 9787 and 9789 track surfacing these stats.
- PRs (roc-lang/roc): 9720, 9768, 9459. Issues: 9788, 9787, 9789.

## Solution design

1. **(a) Extend the field-set audit to the two hand-written top-level
   structs.** Add a `SliceStoreSerde`-style comptime bidirectional audit
   binding `CheckedModuleArtifact` ↔ its `Serialized` (with an explicit,
   audited allowlist for intentionally unserialized fields such as
   `module_env` and `serialized_backing`, each carrying a comment saying
   why), and the same for `ModuleEnv` ↔ `ModuleEnv.Serialized`. A field
   added without serde handling (or an allowlist entry) becomes a compile
   error. Fix `exhaustiveness_sites` serialization as the proof case: add
   it to `Serialized`, `serialize`, `deserializeWithBacking`, and bump
   `serialized_layout_version`. DELETE nothing here — this edge is
   missing code, not excess code.
2. **(b) Give the env blob the artifact blob's protections.** Add the env
   layout hash (reuse/relocate `MODULE_ENV_VERSION_HASH`) as a third
   32-byte field in the checked-module entry header, rejected in
   `decodeCheckedModuleCacheEntry`; add a `validate(backing_len)`
   marker-bounds pass over `ModuleEnv.Serialized` mirroring the artifact
   side, called before `deserializeWithMutableTypes`. Bump
   `checked_module_cache_format_version`.
3. **(c) Bind the fingerprint to the physical layout.** Fold the target
   arch triple (or `@sizeOf(usize)` + endianness + `@sizeOf`/`@offsetOf`
   of each fingerprinted struct) into `layoutVersionHash`, or — simpler
   and sufficient — add the arch to the cache directory path next to the
   compiler version in `getVersionCacheDir`. Either closes the cross-arch
   shared-dir hazard; doing both is cheap.
4. **(d) Make failure visible.** A rate-limited (once per process)
   non-verbose warning when cache stores fail repeatedly (threshold on
   `CacheStats.store_failures`), e.g. "cache writes are failing
   (<reason>); compilation will not be cached". Record a stat in the
   `checkedModuleCacheKey(...) catch return false` path. Surface
   hit/miss/store-failure counters in `--verbose` output, wired into the
   issues-9787/9789 stats work so both efforts land one surface.

## What success looks like

- All four edges have the same mechanical enforcement as the framework
  interior: adding a field to `CheckedModuleArtifact` or `ModuleEnv`
  without serde support does not compile.
- A cache-loaded artifact's `exhaustiveness_sites` equals the freshly
  compiled one's (assertable in a round-trip test).
- A cache entry written by a compiler with a different `ModuleEnv` layout
  is a recorded cache miss, never a misread.
- A read-only cache directory produces a visible warning on the first
  affected compile, not a permanently slower compiler.
- No re-hashing of blob bodies is reintroduced (PR 9768's win preserved).

## How to evaluate the result

### Correctness ideal

Canary tests prove each edge mechanically: (1) in a throwaway branch, add
a dummy field to `CheckedModuleArtifact` and to `ModuleEnv` with no serde
handling and verify the build fails at comptime; (2) corrupt and truncate
env blobs at every byte-range boundary and verify rejection (recorded as
invalidation), never a misread or crash; (3) mutate the stored env-layout
hash and verify a miss; (4) simulate cross-arch by flipping the arch
component and verify a miss. No possible cache entry — corrupt, stale,
foreign — can reach `deserializeWithMutableTypes` with mismatched bytes.

### Performance ideal

The env bounds pass is O(number of markers) once per load — measure that
it stays a small fraction of cache-load time (the artifact-side `validate`
is the existing baseline; the env side should cost proportionally the
same). The warning path costs nothing when healthy (a counter compare).
Header grows by 32 bytes per entry — negligible. Cache hit-path wall time
before/after must be within noise; no body hashing on read or write.

## Tests to add

- The four canaries above (comptime-failure canaries live as documented
  manual checks or a build-mode test harness if comptime-failure testing
  is available; the runtime ones as ordinary tests).
- Round-trip equality for `exhaustiveness_sites` (compile, store, load,
  compare against fresh compile).
- Corrupted-entry rejection matrix over the entry format: bad magic, bad
  format version, bad artifact layout hash, bad env layout hash, bad key,
  truncation at each section boundary, oversized lengths — each must be a
  recorded miss/invalidation with a from-source fallback.
- Store-failure warning test: point the cache at a read-only directory,
  compile, assert the warning appears exactly once and compilation
  succeeds.
- Stats-surfacing assertion: verbose compile of a warm project reports
  nonzero hits; with the read-only dir it reports store failures.

## Related projects

- [Unify the build pipelines](../big/unify-build-pipelines.md) — makes
  every entry path use this cache, multiplying the value of hardening it.
- [Content-based nominal identity](../big/content-based-nominal-identity.md)
  — the identity inputs to the cache key; this project hardens the value
  side, that one the key side.
