# Rules

The model checks one judgment: after any sequence of edits, a command run with the warm cache behaves exactly like the
same command run on the same sources with an empty cache directory. "Behaves" means the same command output (reports,
summary line, test results, with timings and private paths removed), the same exit status, and, for `roc build`, the
same output and exit status from the program it built.

Each rule below is a reason the compiler gives for why reuse is sound. The model does not test the rules one by one; a
disagreement means one of them failed, and the replayed sequence (`./run.sh replay`, `./run.sh reduce`) narrows which.

| Rule | What it says | design.md | Compiler code |
|---|---|---|---|
| `checked-key` | A checked module entry is reused only for the same key: the module's source inputs, its content identity, its checking context (each import's published artifact, the platform relation, explicit roots) and its direct imports' keys. A change anywhere in a module's import closure is a different key. | [Cache Boundary](../../design.md#cache-boundary) | `checkedModuleKeyFromModuleEnv` in `src/check/checked_artifact.zig`; reuse in `src/compile/coordinator.zig` and `src/compile/checked_cache_entry.zig` |
| `typing-scope` | Inference results may be reused when the checking dependencies are equal even though implementations differ; that reuse never extends to evaluated constants, procedure templates or lowering inputs. | [Checked cache identity and native checking interfaces](../../design.md#checked-cache-identity-and-native-checking-interfaces) | `typingCacheKeyFromModuleEnv` in `src/check/checked_artifact.zig` |
| `reports-replay` | A hit reports the stored problems (import resolution, checking, evaluation) rendered against the current file, in the same order and with the same exit code as a miss. Problems an entry cannot reproduce block the write. | [Cache Boundary](../../design.md#cache-boundary) | `src/check/checked_reports.zig`, `src/compile/messages.zig` |
| `canonicalized-key` | A canonicalized entry is keyed by the module's source bytes, basename, entry flag, validation mode, role and compiler version only, so it may be shared by identical modules in different packages or directories; location-dependent import normalization is redone on every hit. | [Canonicalization output cache](../../design.md#canonicalization-output-cache) | `canonicalizedModuleCacheKey` in `src/compile/cache_key.zig`; `src/compile/canonicalized_cache_entry.zig` |
| `pairing-key` | A platform/app pairing entry is keyed by the platform's and the app's checked keys. | [Cache Boundary](../../design.md#cache-boundary) | `platformPairingCacheKey` in `src/compile/cache_key.zig` |
| `boxy-object` | Under `--specialize=no`, a module's object (and its unit image) is a function of its own source and stored values and the interfaces of its imports, never their implementations. An edit that keeps a module's interface rebuilds only that module's object. | [Caching Under `.boxy`](../../design.md#caching-under-boxy) | `src/compile/boxy_interface.zig` (`Digests`), `key` in `src/compile/boxy_object_cache.zig`, `src/compile/boxy_unit.zig` |
| `test-outcomes` | `roc test` reuses a module's recorded outcomes only under the same checked key, compiler, target, strategy and optimization mode. | [Test outcome files](../../design.md#test-outcome-files) | `Inputs.key` in `src/cli/test_outcome_file.zig` |
| `admission` | An entry whose format version, length framing or decoding fails is a miss, never an error. | [Cache storage](../../design.md#cache-storage), [Cache Admission Versioning](../../design.md#cache-admission-versioning) | `src/compile/cache_versioning.zig`, `src/compile/artifact_store.zig` |

Every rule is exercised only through what a command prints and what a built program does. A wrong reuse that changes
neither (for example, a stale object whose code happens to compute the same value) is not seen, except under
`ROC_VERIFY_BOXY_OBJECT_REUSE`, which the driver sets on the warm runs of half the sequences: it rebuilds every restored
`.boxy` object and compares it with the cached one, so an object key that misses an input is caught even when the
output agrees.

`admission` is reached only incidentally: the model never corrupts a cache file or switches compilers.

## Specialized builds

Under `--specialize=yes` (`dev-spec`, `run-spec`, `test-spec`, `speed`, `size`), design.md says specialized compilation
keeps no per-module objects or post-check IR ([Cache storage](../../design.md#cache-storage)), so for those configs
the checked, canonicalized, pairing and test-outcome rules are the ones in play.
