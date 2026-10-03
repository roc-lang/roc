# Shared `Node.null` lowering

## What is measured, not inferred

The shared constant slowdown is dominated by **completion-relation checks of
the constant's type**, not copying its null payload or primarily hashing its
stored type. A symbolicated macOS `sample` profile places 85% of the sampled
specialization-worker stacks inside `BodyContext.requestCompletionRelation`.
Its recursive inhabitation checks account for another large portion of the
same inclusive stacks; those percentages must not be added together.

All package measurements below use the same unmodified baseline ReleaseFast
compiler with debug symbols, dev output, two jobs, and no experimental flags:

```
/Users/rtfeldman/code/roc/.delta/worktrees/w0bxep5v818j/roc/.tmp/roc-profile-rf
```

The experiment copies the entire final package and `examples/small` into
separate ignored directories, preserving relative imports. The shared variant
is regenerated from copies of the final generator by changing only these
emissions:

- `Translate.zero` for `NodePtr`;
- missing-RHS node value;
- `lfirst` and loop-element fallback values;
- the dispatcher's missing-value fallback.

Patterns, `Err(Null)`, hand-written helpers, and Node's record defaults are not
rewritten. Regeneration reports zero problems. The generated Actions patch
changes 162 lines; Node is byte-for-byte identical.

| Input | SHA-256 |
| --- | --- |
| Final inline Actions | `d04e9915ab6971f52932a088202cf5500781855ed4c689f88ee22740ea2536c8` |
| Regenerated shared Actions | `2575bfbbb0387545c0977e44a97de29aa50b054dc5b561980f95797a313181b3` |
| Node, both variants | `f1bf21d8ec43fe8d5d4a0a049d77caad34f6579c0eac53fc48e9a734369f0603` |

## Profile evidence

The exact workflow is:

```
ROC_CACHE_DIR=<isolated-cache> <baseline> build <variant>/examples/small/main.roc \
  --opt=dev --jobs=2 --timings --verbose --output=<variant>/small
```

Each variant is built once to warm the checked-module and object-pack caches,
then rebuilt with `sample` attached directly to the compiler PID (not the PID
of a `time` wrapper). Sampling interval is 5 ms.

| Variant | Warm-up Monotype | Profiled warm Monotype | Profiled total |
| --- | ---: | ---: | ---: |
| Shared | 103.946 s | 92.849 s | 95 s |
| Inline | 15.433 s | 16.581 s | 18.9 s |

Shared was sampled for 90 seconds starting at process launch, covering most of
Monotype. Inline was sampled until process exit. Counts below are inclusive,
with a recursively repeated symbol counted only at its outermost occurrence
in each stack; they are not the recursive-counted summary printed by `sample`.

| Symbol | Shared samples | Inline samples |
| --- | ---: | ---: |
| `Builder.runSpecJobTask` | 29,371 | 4,424 |
| `BodyContext.constUseTypeNode` | 25,989 | 1,113 |
| `BodyContext.requestCompletionRelation` | 25,012 | 459 |
| `BodyContext.storedConstRootTypeNode` | 13,868 | 1,151 |
| `BodyContext.nodeIsProvenUninhabitedInner` | 8,341 | 204 |
| `BodyContext.lowerConstCaptureType` | 1,318 | 887 |

Shared self samples, excluding callees, include 14,336 in
`requestCompletionRelation` and 8,331 in `nodeIsProvenUninhabitedInner`.
Thus caching only `lowerConstCaptureType` does not target the dominant cost.

### The hot inner operation

Disassembling the symbolicated baseline identifies the dominant self-sample
locations inside `requestCompletionRelation`:

| Function offset | Instruction | Operation |
| --- | --- | --- |
| +448 | `ldrb w15, [x8, x11]` | Load the next hash-table slot's metadata |
| +472 | `and w16, w15, #0x7f` | Inspect the slot fingerprint |
| +420 | `and w15, w15, #0xff` | Handle a nonmatching/deleted slot |

These are the inlined `AutoHashMap.getOrPut` probing loop, not type-graph
edge traversal. The loop increments and wraps the slot index until it finds
an empty slot, a matching key, or exhausts the table. Most of the 14,336
completion self samples are in this loop.

The active-path map inserts each pair on descent and removes it on return.
Zig's ordinary hash map uses tombstones for removal. Consequently, wide
sibling traversal leaves deleted slots although only a shallow active path
is live. This is the concrete data-structure mismatch suggested by the
profile. A dense `AutoArrayHashMap` repairs its index during deletion and
preserves the same active-path/cycle-cutoff semantics without introducing a
cache of context-dependent relation results.

Two separate relation checks contribute almost equally:

1. `constUseTypeNode -> storedConstRootTypeNode ->
   relateCheckedNodeToProducedValueInner -> resultCompletesRequest`;
2. `constUseTypeNode -> relateCheckedNodeToProducedValueInner ->
   resultCompletesRequest`.

For example, beneath `relateLookupExprAtNode` these paths have 6,023 and 6,004
samples respectively. This is explicit evidence of both checks doing substantial
work, not an assumption that every source reference does one whole-graph copy.

## Reduced workload

A small app importing only the real generated Node module reproduces the
problem. It contains ten expressions of the form:

```roc
n0 : Node
n0 = if x == 0 Node.null else Node.Integer({ ival: 1.I64 })
```

It calls `Node.is_null` on the values and evaluates `answer = work(1)`.
No parser, grammar actions, catalog, database, or platform-specific PG code is
required. Changing only the value expression to `Null` reduces shared Monotype
from approximately 570 ms to 5–6 ms.

Further reduction establishes that the declared type shape is not the whole
cause:

| Node module contents, same ten-use app | Shared Monotype |
| --- | ---: |
| Original generated Node | ~570 ms |
| Same union and record aliases; only null, factory, and is_null methods | 10 ms |
| Same reduced module, with generated record-default constants restored | 549 ms |

The aliases-only inline and factory controls take 5 ms and 6 ms respectively.
Unused accessor/helper methods are therefore unnecessary, while the defaults
introduce material checked/lowering state. This experiment does **not** yet
establish whether defaults alter graph equivalence, constant scheduling, or
both. These small runs are exploratory and still need repeated matched warm
comparisons; unlike the full-app profile, they do not yet isolate cache
scheduling from source-shape effects.

`shared-null-repro.py` generates these experiments and synthetic recursive
record fans. Its exploratory options deliberately distinguish identical versus
distinct record shapes and the presence of default constants. A broad nominal
type by itself is insufficient to explain the regression.

## Correctness constraints for a compiler change

The current completion relation keeps an active recursion-path set, removing
entries on return. It is not a cache of completed pair results. Its
inhabitation predicate likewise returns a provisional conservative result on
an active cycle. Persisting such provisional answers is unsound.

A fix must:

- keep the direction of requested versus produced types;
- preserve nominal backing authority and inspectability;
- preserve the distinction between unchanged, completed, and mismatching
  relations, including stricter row-extension requirements;
- scope graph-dependent caches to a mutation-free query or explicitly track
  invalidation;
- resolve recursive dependencies correctly rather than caching cycle cutoffs;
- leave checked output and runtime null values unchanged.

The first implementation changes only the active-path container, behind
`-Dperf-const-completion`: ordinary tombstone-based hashing when disabled,
`std.array_hash_map.Auto` with `swapRemove` when enabled. There is no
completion-result or recursive inhabitation memo. Enter, leave, and cycle-cutoff
results remain identical.

Two focused unit tests cover thousands of distinct sibling visits while
retaining an ancestor, and recursive/shared-DAG relations whose outcomes are
unchanged, completed, or mismatching. The enabled tests and ReleaseFast compiler
build pass.

### Paired container-change measurement

The retained off/on ReleaseFast binaries differ in the `const_completion`
build flag. Both contain the same temporary diagnostic machinery, disabled for
these runs. Each uses an independent APFS clone of the same starting cache.
There are two warmups before the measured build, dev output, and two jobs.

| Variant | Flag | Second warmup Monotype | Measured Monotype | Total | Peak RSS |
| --- | --- | ---: | ---: | ---: | ---: |
| Shared | off | 95.111 s | 94.599 s | 96.72 s | 6.146 GB |
| Shared | on | 65.945 s | 65.975 s | 68.25 s | 6.126 GB |
| Inline | on | 15.152 s | 15.162 s | 17.35 s | 6.370 GB |

Both measured shared builds explicitly report **61 modules cached, zero built,
100% cache hit**, and 61 cached canonicalizations. Source hashes were checked
again after the runs; no experimental environment flags were present, and
`ROC_COMPLETION_DIAGNOSTIC`, `ROC_SPEC_CENSUS`, and `ROC_PROFILE_CACHE` were
unset.

Changing only the path-container implementation removes **28.624 seconds,
30.3% of Monotype time**, with effectively unchanged peak RSS. It does not
eliminate the entire shared-versus-inline gap.

The first warmups reveal an important cache-state distinction: their Monotype
times were only 15.063 s/off and 14.828 s/on, despite total build times near
70 seconds. Subsequent cached-module builds rise to 95/66 seconds in Monotype.
Comparing a cold shared build against a warm control would miss or misdescribe
the regression. This also reinforces the caveat on the exploratory tiny cases.

The source has a corresponding representation distinction.
`constUseTypeNode` handles an `eval_template` by instantiating its requested
checked type, whereas a `stored_const` goes through `storedConstRootTypeNode`:
lower the stored root's captured type, import an independent graph, and relate
it to the requested type. `constUseTypeNode` then performs its outer relation
as well. `restoreConstUseAtNode` also distinguishes stored values from evaluation
templates. Consequently, caching the checked constant changes the lowering
route even though the Roc value is still the same null tag. This is not merely
a difference in native object-cache warmth.

The enabled shared build was separately sampled for 65 seconds at 5 ms
intervals, directly on compiler PID 1156. Its sampled Monotype time was 70.559 s;
this is **not** the headline timing. Out of 20,801 inclusive specialization-worker
samples, 16,544 were in completion relations and 8,846 in recursive
inhabitation. Completion self samples fell to 3,697; inhabitation had 6,658.
The recursive inhabitation helper uses an active bitset, not the tombstone-based
pair map. Its residual cost must therefore be addressed separately, not
attributed to remaining hash-map deletion problems.

There is a measurement limitation: disabled diagnostic hooks still require
thread-local lookups. `_tlv_get_addr` accounts for 3,243 self samples in this
profile. Both paired binaries contain that plumbing, but the merged production
source does not. Therefore the paired result establishes the container's
benefit; **final absolute performance requires the clean combined compiler**.

Compiler SHA-256 identities:

```
off 4e1df35c378eafbfaefd08fb6a25ea38268eeba68f995293c2035345189d97c2
on  8049201764a57557d79bbc2afc90277401c151d07660486c7c8337b1cc5c2525
```

If inhabitation remains dominant after the container change, a separate
candidate is query-local reuse of **completed top-level inhabitation calls**.
Such a call begins and ends with an empty recursion path; caching those answers
is distinct from caching provisional answers inside the recursive helper.
It should not be added without measuring the remaining cost.

## Raw evidence

Raw profiles, build logs, source hashes, generated patch, and deduplicated
inclusive summaries are retained under:

```
/Users/rtfeldman/code/roc-pg/.delta/worktrees/vme4vvh6eg3a/roc-pg/local/null-study/results/
```

Reduced source trees and their logs are under the isolated compiler checkout:

```
.tmp/null-repro/{shared-actual,inline-actual,shared-reduced,inline-reduced,
factory-reduced,shared-defaults-reduced}/
```

Paired compiler/source manifests, warmup/measured logs, the enabled profile, and
deduplicated summaries are in `.tmp/null-instrument/paired-results/`.
The temporary instrumentation patch and disassembled probing loop are retained
in `.tmp/null-instrument/`; none of that instrumentation remains in production
source.

Exploratory short synthetic runs are not rigorous benchmark observations; only
the explicitly reported controlled comparisons above support conclusions.
