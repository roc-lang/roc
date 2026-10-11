# Compiler Efficiency Inventory

An inventory of needless inefficiency in the compiler: data structures that
can be packed tighter, array-of-structs stores whose hot loops touch one
column, per-call allocation and freeing where a reusable scratch buffer or an
arena already exists, hash maps keyed by dense ids, and work proportional to a
whole module done inside a per-procedure path. Every entry names the file and
line it was found at, the frequency at which that code runs, the fix, and the
status of that fix on this branch.

Measurements use a ReleaseFast compiler pinned to the P-cores with
`perf stat -e instructions:u`. Wall-clock timings are not used anywhere in this
document.

## Baseline

Commit `847aeab60b` (main at the start of this work), ReleaseFast, `--no-cache`:

| Workload | Instructions |
| --- | ---: |
| `roc check -j1 package/main.roc` (roc-pg, 63.5k lines) | 266.28 G |
| `roc build --opt=dev examples/query.roc` (roc-pg app on basic-cli 0.28.0) | 244.83 G |
| `roc build examples/query.roc` (LLVM, speed) | 524.75 G |
| `roc test test/fx/issue8826_full.roc` | 141.4 M |

## Results

ReleaseFast, `perf stat -e instructions:u` pinned to P-cores, `--no-cache`,
measured with every fix below applied:

| Workload | Baseline | This branch | Change |
| --- | ---: | ---: | ---: |
| `roc check -j1 package/main.roc` (roc-pg) | 266.28 G | 218.24 G | -18.0% |
| `roc build --opt=dev examples/query.roc` | 244.83 G | 222.54 G | -9.1% |
| `roc build examples/query.roc` (LLVM) | 524.75 G | 344.94 G | -34.3% |
| `roc test test/fx/issue8826_full.roc` | 141.4 M | 139.9 M | -1.1% |

Peak resident memory (`/usr/bin/time -f %M`) of `roc check -j1` on roc-pg:
3.16 GB on the baseline, 2.66 GB on this branch (-16%).

Validation: the module unit tests of every touched module, the lint gates,
and all CLI suites (platforms, echo, subcommands, glue: 2198 tests) pass.

## Method

The whole of `src/` was audited directory by directory. Each `.zig` file on
the production pipeline (parse, canonicalize, check, post-check, LIR, backends,
compile coordinator, CLI, eval, LSP) was read for the patterns below. Test
blocks, fuzzers, debug dumps, and snapshot tooling were excluded.

Categories:

- A: temporary allocation that a scratch buffer, reusable column, or arena
  should serve.
- B: hash map keyed by a dense id where a column, bitset, or `DenseMap` is
  the right structure.
- C: struct packing (padding, oversized unions, inline rare payloads, i128
  alignment, optionals where a sentinel index suffices).
- D: redundant copy (`dupe`, `toOwnedSlice`, clone-then-free).
- E: work proportional to a large domain inside a hot path.
- F: per-element heap objects (map of lists, list of lists, boxed nodes).
- G: missing capacity reservation.
- H: other.

## Fixes applied on this branch

Each entry names the finding it resolves (by the heading used in the
per-stage sections below), what changed, and what guarantees the change does
not alter behavior. Every fix was validated with the module unit tests of the
stages it touches; the instruction-count effect is reported in "Results".

1. `src/postcheck/lambda_mono/lower.zig:403-409`: the four whole-program
   node maps were `@memset` to null per function specialization. The lowerer
   now records every slot it writes in four touched-index lists and resets only
   those slots. Same map contents at every read; the reset cost is proportional
   to the body just lowered instead of the whole program.
2. `src/types/instantiate.zig:890/772`: the marker pre-walk created a
   worklist, a binder list, and a hash set per instantiation. The lists and a
   pool of visited sets now live in the store's instantiate scratch; a walk
   takes them out while it runs so a nested instantiation never sees them. The
   visited set is a `DenseMap(Var, u8)` holding one bit per polarity/reach pair,
   replacing the hashed `{var, polarity, reach}` key.
3. `src/check/occurs.zig:400`: the occurs-check scratch kept two hash maps
   keyed by dense ids (`Var`, `DescStoreIdx`); both are `DenseMap`s now, so
   clearing per check costs what the last check touched.
4. `src/check/unify.zig:4616`: `visited_index.clearRetainingCapacity()` ran on
   every unify even when the index was empty; it is skipped when empty.
5. `src/check/unify.zig:2900`: record fields were sorted with text comparisons
   on every record unify; an ordered pass now returns early when the gathered
   run is already sorted (tags already did this).
6. `src/check/unify.zig:3822`: constraint index lists were sorted per
   constrained-var unify; `std.sort.isSorted` skips the sort when they already
   are.
7. `src/check/Check.zig:18091/25009`: each `checkExpr` created one
   `ExprKernelActivity` on the heap and grew an activity stack and a task stack
   from zero. Finished kernels and activity stacks are pooled on the checker
   with their capacity retained; a run takes its own stack, so a nested run
   never moves the activity an outer step is working on.
8. `src/check/Check.zig:6719`: `Env.clean_index` was a hash map of heap
   lists walked on every env reset; it is now a map of chain heads into one
   flat link column, and a reset clears two tables.
9. `src/canonicalize/Can.zig:14107/14524/14573`: a hash set over the bound
   vars was built per lambda and per match branch; a reusable `DenseMap`
   index on the canonicalizer replaces it, and the now-unused `Scratch.SetView`
   is removed.
10. `src/postcheck/monotype/lower.zig:17694-17714`: the draft reset
    `deinit`ed and re-`init`ed every lookup table per specialization job; they
    are cleared retaining capacity, and `DraftSpecLookup` gained a
    `clearRetainingCapacity` that frees only its inner lists.
11. `src/postcheck/monotype/lower.zig:31174`: `runLower` grew a fresh frame
    stack per nested run; finished stacks are pooled on the `Builder` with
    their capacity retained.
12. `src/postcheck/monotype/solve.zig:4915/5000/6056`: opaque row pairing and
    the request-container join built a name-keyed hash map per relation; they
    use the graph's generation-stamped label slots, reading the pairing out
    before any nested relation can claim the next generation. The three label
    columns became one `RowLabelSlot` column grown by explicit reallocation.
13. `src/lir/LirStore.zig:68`: rewrite keys were probed through a hash map on
    every statement read of a procedure rewrite; once `finish` sorts the ids, a
    binary search over them answers lookups.
14. `src/eval/compiler_host.zig:80`: every Roc allocation during compile-time
    evaluation inserted and removed a hash-map entry; size and alignment now
    live in a header before the block and live blocks form a list, so
    `deinit` still frees leftovers and a debug build still checks that a
    released pointer was allocated here.
15. `std.heap.ArenaAllocator` -> `base.SingleThreadArena` in the post-check,
    LIR, checker, layout, canonicalizer, and backend stages (53 files). The
    std arena pays an atomic read-modify-write per allocation; every arena in
    these stages is owned by one thread. `SingleThreadArena.reset` now
    reallocates when a retained chunk cannot grow in place, matching the std
    arena, so a reset never silently shrinks the retained capacity.
16. `src/postcheck/monotype/solve.zig` test "issue 11362": the allocation
    failure check ran over `std.testing.allocator`, whose in-place growth
    depends on heap layout; it runs over `base.DeterministicAllocator` like the
    other allocation-failure tests.


17. `src/lir/arc_dismantle.zig:242` and the pass's per-local read map, per-proc
    join map, parameter-body map, and flow-state map were hash maps keyed by
    dense statement and local ids; all are `DenseMap`s now, and `DenseMap`
    gained a const entry iterator.
18. `src/eval/interpreter.zig:600` found a frame local's slot by binary search
    on every read and write; each procedure now has a direct slot index built
    on first entry (`slot_of[id - base]`) and checked against the frame-local
    span it was built from.
19. `src/postcheck/boxy/lower.zig:41891` allocated and zeroed two columns over
    every pattern binder in the module per procedure body; the body keeps one
    `DenseMap` of binder bindings instead, and the module-context swap moves
    the map rather than allocating and freeing columns.
20. `src/backend/dev/LirCodeGen.zig:955` hashed every local read and bind
    through `AutoHashMap(u32, ValueLocation)`; it is a `DenseMap` now, with a
    `clone` for the nested-procedure save (an empty map clones without
    allocating).
21. `src/layout/store.zig:3028` dropped every memoized layout digest on every
    in-place layout update; only an update to a layout the memo already holds
    drops the memo, since a memoized digest memoizes every layout it depends
    on first.

22. `src/postcheck/monotype_lifted/spec_constr.zig:5400`: every `Cloner`
    built about twenty containers from empty; finished cloners return their
    containers to the pass, which keeps up to eight sets with their capacity.
23. `src/check/Check.zig:9379-9470` and `24840`: the expected-type backup's
    ground hash and identity walks built a list and a hash map per call; they
    use checker-owned scratch with a `DenseMap` visit index, and retired
    operand sequences are a `DenseMap`.
24. `src/check/Check.zig:24716` and `44787`: a static-dispatch drain with no
    relation past its clean prefix and nothing held returns before any setup,
    and the duplicate relation leave is gone.
25. `src/check/Check.zig:9888`: the evidence-parameter check reuses a
    checker-owned scratch instead of building one per call.
26. `src/eval/interpreter.zig:3243`: a low-level statement with up to eight
    arguments keeps its argument arrays on the stack instead of in the
    evaluation arena, which is never released during an evaluation.
27. `src/eval/value.zig:52` and `boxy_runtime.zig:6691`: value copies use
    `@memcpy` when the ranges do not overlap, and a local that is copied in
    full is not zeroed first.
28. `src/eval/interpreter.zig:2580`: frame setup marks zero-sized locals from a
    per-procedure list instead of looking up every local's layout per call.
29. `src/base/SmallStringInterner.zig:100`: a probe that reaches a candidate
    compares the needle and the entry's terminator directly, without first
    scanning the entry for its length.
30. `src/check/Check.zig:785`: the binder source map is a `DenseMap`.

31. `src/lir/arc_dismantle.zig:906`, `body_clone.zig:246`: the dismantle
    future-field walk and the successor walk read statements through a
    read-only pointer instead of copying each one.
32. `src/lir/arc_dismantle.zig` whole-store scans and `src/lir/use_order.zig`
    topology walks: same change.
33. `src/postcheck/monotype/ast.zig:1417`: the procedure debug-name map indexes
    symbols in a `DenseMap` instead of scanning its list.

34. `src/postcheck/monotype_lifted/spec_constr.zig:5889`: `runClone` frame
    stacks return, cleared, to a fixed pool in the cloner scratch.
35. `src/check/exhaustive.zig:1226-1259`: the inhabitation graph's roots,
    rows, and assumed-empty set are `DenseMap`s over the root var, and parent
    edges are chained through one shared list instead of a list per node.

36. `src/lir/arc_dismantle.zig` `FutureFields`: statement successors that
    do not depend on the candidate are recorded once and reused by every
    later candidate's walk.

37. `src/lir/arc_dismantle.zig` `FutureFields`: each procedure's field-walk
    graph is reversed once, and every candidate solves its observations
    backward from its own reads, bounded by its redefinition, instead of
    walking forward from each consuming read to the end of the procedure.
    The LLVM build of `query.roc` drops from 416.0 G to 367.2 G instructions
    and `roc check` on roc-pg from 249.8 G to 236.8 G; the generated code of
    both backends is byte-identical.

38. `src/postcheck/monotype/type.zig` `resolveCyclicScc`: the store records
    every reduced recursive group by its shape (member labels in discovery
    order plus positional in-group references), so a later group of the same
    shape takes each member's digest without refinement rounds, the group
    rendering, or the unfolding renders. `roc check` on roc-pg drops from
    236.8 G to 221.4 G instructions, the dev build of `query.roc` from
    239.5 G to 224.4 G, and the LLVM build from 367.2 G to 352.3 G; the
    generated code is byte-identical.

39. `src/types`: the store's frozen-class anchors and `TypeWriter`'s five
    var-keyed maps are `DenseMap`s, a nominal opening's rigid substitutions
    are a stack slice searched linearly, and `detachOccurrence` reuses store
    scratch instead of reserving a whole-store-sized buffer per detach.

40. `src/check/Check.zig`: an expected tag payload's rigid substitutions are
    a stack slice instead of a hash map; `checkPattern` frame stacks are
    pooled with their capacity; `exprAlwaysCrashes` follows blocks and `else`
    branches without a worklist entry, so a plain expression allocates
    nothing; and a generalization boundary borrows its single root from the
    suspended kernel activity instead of allocating a one-element slice.

41. `src/postcheck/monotype`: nominal-backing declaration scopes come from a
    builder pool and are emptied with their storage kept (`VersionedMap`
    gained a sole-owner `clearRetainingCapacity`); body-context scratch
    arenas come from the graph's arena pool; evidence equality keeps its
    worklist on the stack; a transaction seal keeps its working state in one
    arena with digest groups chained through one offset column instead of a
    list per digest; and record and tag rows already in label order are
    appended without a sorting copy. Together with fixes 39 and 40 this is
    about 1 G instructions on each roc-pg workload.

42. `src/lir/arc_dismantle.zig` take flow walk: a store-wide column holds the
    one successor of every straight-line statement, so a candidate's walk
    decodes only branching and rebinding statements instead of loading every
    144-byte statement it passes. The LLVM build of `query.roc` drops from
    351.2 G to 345.5 G instructions and `roc check` from 220.4 G to 218.6 G.

43. Every statement read that is used immediately (a `switch` on it, one
    field read, or a tag comparison) goes through
    `LirStore.getCFStmtImmediate` instead of copying the 144-byte statement:
    677 sites across the LIR passes, Boxy lowering, solved-to-LIR lowering,
    and their tests. Switch captures still copy their payloads, so a prong
    that grows the store reads nothing through the pointer.
    Read-only helpers over a `*const LirStore` also bind statement locals by
    pointer (23 more sites).

44. `src/check/checked_artifact.zig`: the read forms of checked alias and
    nominal payloads point at the stored 192-byte owner module id instead of
    copying it, like their slices alias the pools, so materializing a payload
    no longer copies it. Build forms (`CheckedAliasTypeBuild`,
    `CheckedNominalTypeBuild`) own the id, so a build survives the store
    growing under it. `roc check` drops from 218.6 G to 216.9 G and the LLVM
    build from 345.5 G to 343.4 G; generated code is byte-identical.

45. `src/postcheck/monotype/type.zig`: the type column stores a 16-byte
    `StoredNode` per type; named payloads and erased digests live in side
    lists that the node indexes, and `get` assembles the full content. Each
    type was 168 bytes. Peak memory of `roc check` on roc-pg drops from
    2.96 GB to 2.66 GB; instruction counts rise about 0.5% for the assembly
    in `get`, and pinned cycle counts are unchanged within run-to-run noise.

46. `src/postcheck/monotype/solve.zig` `InstNode`: an erased node's 32-byte
    digest is stored out of line in the graph arena, like a named node's
    payload, so every graph node is 32 bytes instead of 40.

47. `src/layout/store.zig`: interned layout keys live back to back in one
    byte buffer and the intern map holds spans of it, instead of one heap
    string per layout; struct and tuple field scratch starts on the stack;
    the structural field sort reads each field's layout once instead of per
    comparison; `ensureZstLayout` returns the fixed zero-sized index instead
    of scanning the layout table; and a canonical-root commit returns empty
    slices without allocating.

48. `LirStore.getProcSpecImmediate` returns a read-only pointer to a stored
    procedure specification; the 244 reads that used one or two fields of a
    by-value `getProcSpec` copy go through it.

49. `src/lir/LirStore.zig`: `addCFStmt` and `appendBodyShard` append into the
    capacity they reserve up front without re-checking it per element.

50. `src/lir/LIR.zig` `LiteralValue`: the 128-bit literal payloads are 8-byte
    aligned fields, so `CFStmt` aligns to 8 instead of 16 and shrinks from 144
    to 136 bytes per statement.

51. Backend tables keyed by dense ids: the LLVM backend's procedure
    registry, static refcounted backings, and Boxy dictionary thunks, and the
    wasm backend's pending function bodies, are `DenseMap`s (the dev, LLVM,
    and wasm per-procedure maps were converted earlier). Generated code is
    byte-identical.

52. `src/eval/interpreter.zig`: the statement loop records only the active
    statement id and switches on the statement by reference, reading its
    location, region, and inline scope from the store when a call or failure
    needs them; a frame slot marks "unassigned" with a sentinel value pointer
    instead of a padded bool, so slots are 16 bytes instead of 24; integer
    `to_str` formats into a stack buffer; entrypoint argument layouts start
    on the stack; and hosted calls keep their argument offsets and C-ABI
    scratch in one retained arena reset when the outermost hosted call
    returns.

53. `src/compile`: package resolution's arenas are single-thread arenas (each
    belongs to one task or result), and imported-artifact dedupe keys a
    `DenseMap` by the dense resolved module index.

54. `src/parse`: scratch-to-span copies reserve once and append without
    per-element checks through one helper (seven copies of the loop); a
    field-access extension writes its two changed node columns instead of the
    whole row; the token payload drops the identifier underscore flags no
    consumer read, so every token's extra column is 4 bytes; the declaration
    index keys its statement maps by `DenseMap`; destructuring and import
    path scratch is parser-owned; and empty numeric digit runs are empty
    slices instead of zero-length allocations.

55. `src/parse/tokenize.zig` reserves a third of a token per source byte
    instead of one token per byte (13 bytes of token columns per source byte),
    and `CommonEnv.calcLineStarts` finds newlines with the vectorized scalar
    search instead of a byte loop.

56. `src/canonicalize`: the canonicalizer's ten maps keyed by pattern,
    statement, and parser-scope ids, the alias-cycle Tarjan state, and the
    dependency graph's Tarjan state, pattern-to-def map, def positions, and
    def-to-group map are `DenseMap`s.

57. `collections.DenseMapUnmanaged`: a `DenseMap` that holds no allocator,
    with the standard unmanaged surface (`.empty`, allocator arguments on
    calls that allocate or free). Every unmanaged hash map keyed by a dense
    `...Id` (85 sites across postcheck, LIR, eval, bump, glue, and the
    checker) uses it, and the ID-keyed hash-map lint now also matches
    `HashMapUnmanaged(` (skipping anonymous struct keys), so the rule is
    enforced for both forms. The unmanaged map wraps the managed one and
    lends it the caller's allocator per call: a single implementation that
    takes the allocator as a parameter measured 1.7% slower on `roc check`
    than the managed map that reads it from a field. Generated code is
    byte-identical; `roc check` on roc-pg is 0.2% above the commit before,
    from short-lived maps over large id domains that design.md says should
    become reusable scratch.

58. `src/postcheck/monotype/lower.zig` `BodyContext`: the per-body memo maps
    (uninhabitedness entries and settled answers, explicit-binding answers)
    come from a builder pool and return to it emptied with their pages kept,
    since a body's node keys are scattered over a large graph; and the
    body-level direct-call request map nothing read is gone. This recovers
    the cost of fix 57: `roc check` on roc-pg is 218.16 G.

59. `src/check/Check.zig`: 116 hash maps keyed by `Var` or a CIR index
    (fields and per-call scratch) are `DenseMap`s or `DenseMapUnmanaged`s.
    Thirteen stay hashed: they back reserve-then-commit insertions, where a
    capacity reservation must cover the insert, and a dense map cannot reserve
    the page of a key it has not seen. Snapshots are unchanged and `roc
    check` on roc-pg is within 0.05% (218.24 G).

## Findings from profiling

These were found by profiling the ReleaseFast compiler with
`perf record -e cpu_core/instructions/u --call-graph=fp` rather than by
reading code, and were not in the audit above.

### src/postcheck/monotype/ast.zig:1417, `ProcDebugNameMap` is a list searched linearly
- category B/E; runs per debug-name lookup during solved-to-LIR lowering (twice per lowered procedure and once per inline scope), and per insert
- Finding: `get` and `put` scanned every recorded name, so lowering a program with N named procedures did O(N^2) work. In `roc build` of the roc-pg query example this function alone was 11.5% of all front-end instructions.
- Fix: keep the names in insertion order and index each symbol's position in a `DenseMap`; Monotype and lifted program views hold the map instead of a slice.
- Status: fixed (fix 33)

### src/lir/arc_dismantle.zig:906 and callers, statement copies in the dismantle walks
- category D; runs per statement visited per dismantle candidate
- Finding: `getCFStmt` copies the 144-byte statement union; the dismantle future-field walk, its whole-store scans, and the successor and use-order walks it drives called it per visited statement. These copies were a third of all memcpy time in `roc check` and half of it in `roc build`.
- Fix: `LirStore.getCFStmtImmediate` returns a read-only pointer, and these read-only walks switch on it.
- Status: fixed (fixes 31 and 32)

### Phase breakdown of `roc check` on roc-pg
- Compile-time evaluation is 89% of all instructions, Monotype lowering 69% inclusive, Monotype solving 56%, interface capture and identity 21%, type digests 13% (6% in SHA-256 inside cyclic SCC resolution), ARC insertion 14% (dismantle 10%), and the type checker proper 9%. The remaining cost is concentrated in the specialization engine's interface capture and digest algorithms rather than in container choice.

## Findings by stage

## Checker, type store, canonicalizer, parser, base, collections, layout
### src/types/store.zig:197: `Slot` is an 8-byte tagged union holding one u32
- category C; runs one per type variable; read on every union-find resolve (`resolveVar`, `resolveStorageRoot`, every unify step); impact medium (halves slot-column memory and bandwidth for the …; risk medium (touches serialization layout; …
- Finding: `pub const Slot = union(enum) { root: DescStore.Idx, redirect: Var };` is 4 bytes payload + 1 byte tag, padded to 8. The slot column is the single most-touched column in the checker (every resolve walks a redirect chain through it) and it is serialized into the checked artifact. Half of each slot's cache footprint is padding.
- Fix: keep the two u32 domains full-width (invariant: no narrowing below 32 bits) and move the tag into a parallel `std.DynamicBitSetUnmanaged` (1 bit per slot) or a parallel `[]u8` column; `Slot` accessors become `(is_root_bit, u32)`. Both `SlotStore.Serialized` and the savepoint `SlotUndo` trail (which stores whole `Slot`s) need the same split. Verify by the existing path-compression and savepoint rollback tests; no …
- Status: open

### src/types/types.zig:147: `Descriptor.content` column is 28 bytes because `Alias` is the widest payload
- category C; runs one per descriptor; read on every resolve; impact medium (14% of the content column; the column is the …; risk medium
- Finding: `Content = union(enum(u8))` has payloads Flex 16 (`?Ident.Idx` = 8 + Range 8), Rigid 12, Alias 24 (`TypeIdent` 4 + `NonEmptyRange` 8 + `source_arg_count` u32 + `origin_module` u32 + `SourceDecl` u32), FieldPresence 12, FlatType 24 (NominalType 20 = 4+8+4+4, Func 20 = 8+4+8, +tag). So Content is 24 + 1 tag -> 28 bytes with 4-byte alignment. `DescStore` is a `SafeMultiList(Desc)` so rank (4) and flags (1) are already …
- Fix: (a) `Flex.name: ?Ident.Idx` -> `Ident.Idx` using the existing `Ident.Idx.NONE` sentinel saves 4 bytes in Flex but not the union; (b) the real win is moving `Alias.source_decl` + `origin_module` + `source_arg_count` behind the alias' declaration (the `nominal_decls`-style table already exists for nominals; aliases could get a parallel `alias_decls` table and store only `(decl_idx: u32, vars: NonEmptyRange)` = 12 …
- Status: open

### src/types/types.zig:1064: `StaticDispatchConstraint` is ~88 bytes, inflated by an inline `NumeralInfo`
- category C; runs one per numeric literal, per operator use, per method call, per where-clause; copied by every instantiation (`Scratch.pending_constraints`), by `copy_import`, …; impact medium-high (3.6x smaller constraint rows; constraints are …; risk medium
- Finding: `fn_name` 4 + `fn_var` 4 + `origin: Origin` (union whose `from_literal: LiteralInfo` variant carries `NumeralInfo` = `[16]u8` magnitude + bool + u32 scale + `FitSet` (u16) + 4 bools + `Region` 8 = ~36, so LiteralInfo ~40, Origin ~44) + `provenance` 4 + `derived_map_plan: ?DerivedMapPlan` (8+1 -> 12) + `interpolation: InterpolationMetadata` (OptRegion 8 + Var 4 + Range 8 = 20). Total ~88 bytes per constraint in a …
- Fix: put `NumeralInfo` in its own `SafeList` on the `Store` (`numeral_infos`) and store a `u32` index in `LiteralInfo.numeral`; move `interpolation` (already keyed by "present") and `derived_map_plan` into side tables indexed by constraint idx or a single `u32` "extra" index with a sentinel. Constraint shrinks to ~24 bytes. Instantiation copies (`instantiate.zig` `pending_constraints`), `copy_import`, unify merge …
- Status: open

### src/types/store.zig:241: `frozen_anchors: AutoHashMapUnmanaged(u32, Var)` keyed by dense `DescStore.Idx`
- category B; runs `put` per frozen class; `get` on every write to a frozen class (`isolateFromFrozenClassSlow`, line 882); `count()` check on every write …; impact low-medium (one hash per frozen-class write; the map is …; risk low
- Finding: the key is the raw backing int of `DescStore.Idx` (a dense store-local id; the lint is bypassed by storing it as `u32`). This is exactly the "hash a dense id" pattern design.md forbids. Frozen classes are the ground-instance replay classes, so the map is populated once per settled ground dispatch instance per module.
- Fix: `collections.DenseMap(DescStore.Idx, Var)` (paged, cleared in O(live)). `frozenAnchors()` iteration (line 853) becomes DenseMap iteration; `thawFrozenClasses` (line 859-866) uses the live list. No semantics change.
- Status: fixed (fix 39)

### src/types/store.zig:2043: `detachOccurrence` allocates and scans the ENTIRE slot store per detach
- category E, A; runs per write aimed at a member of a frozen class (`isolateFromFrozenClassSlow` line 889), per `union_` that merges a frozen class (lines 1999, 2011), per …; impact high (quadratic in module size when frozen classes are …; risk medium
- Finding: lines 2048-2057 allocate `class_members` with capacity `self.len()` (the whole store) and loop `raw_var` from 0 to `self.len()` calling `resolveStorageRoot` on EVERY variable to find the members of one class. A module with 500k vars pays 500k resolves and a 2MB allocation per detach. `isolateFromFrozenClassSlow` is reached on any write to a variable that unified into a frozen (ground replay) class, which happens …
- Fix: track class membership explicitly: either a per-class intrusive circular list (`next_member: Var` column parallel to slots, O(1) to splice, O(members) to enumerate) or restrict the detach to the occurrence's own storage subtree (children pointing at it) by walking only vars whose redirect target is the occurrence. If an explicit member list is undesirable, at minimum keep a reusable `class_members` buffer in the …
- Status: partly fixed (fix 39): the member buffer is reused store scratch grown to the class size; the whole-store scan stays, since frozen-class detaches drew no samples in any profile and an explicit member list would cost a column on every variable

### src/types/instantiate.zig:890: fresh `AutoHashMapUnmanaged(Var,bool)` + three fresh containers per instantiation
- category A, B; runs per `instantiateVar` / `instantiateTypeScheme` / `instantiateDeferredCallable` call: i.e. per use of any polymorphic binding, per nominal backing open, per …; impact high (every instantiation pays ~4 mallocs + ~4 frees + hash …; risk low
- Finding: `instantiateVarHelp` (line 890) creates `marker_choices: AutoHashMapUnmanaged(Var, bool)` on the stack and `collectMarkerChoices` (lines 772-780) creates `pending: ArrayList(Item)`, `visited: AutoHashMapUnmanaged(Item, void)` (keyed by {Var, polarity, reach}: a Var plus two small enums), and `binders: ArrayList(Var)`, all `deinit`ed at return. That is four heap-allocated containers (hash maps grow by rehash) …
- Fix: move all four into `instantiate.Scratch` (already on the store and already holds `reach_*` DenseMaps for exactly this purpose) with `clearRetainingCapacity` semantics proportional to the last walk; replace `visited` with `DenseMap(Var, u8)` where bit `(polarity*2+reach)` marks visitation, and `marker_choices` with `DenseMap(Var, bool)`. Also `collectMarkerChoices` returns early when `polarity_var_ident == null`, so …
- Status: fixed (fix 2)

### src/types/instantiate.zig:97: `rigid_subs` AutoHashMap(Ident.Idx, Var) rebuilt per nominal-backing open
- category A; runs per `openNominalBacking` call (every time the checker needs a nominal's backing: unify against nominal, field access through nominal, method dispatch, derive); impact medium (1 alloc + hash growth per backing open; backing …; risk low
- Finding: lines 96-107 clear the caller's `var_map` DenseMap and then build a fresh `std.AutoHashMapUnmanaged(Ident.Idx, Var)` of formal-name -> arg, `deinit` at return. Formal counts are tiny (0-4), so a hash map is overkill: a linear scan of a small slice would be faster than hashing, and the allocation is pure overhead.
- Fix: pass `formals`/`args` slices (already available) and have `RigidBehavior.substitute_rigids_fresh` accept a `[]const struct{name: Ident.Idx, var_: Var}` pair-slice with linear lookup, or keep a reusable `ArrayListUnmanaged` of pairs in `Scratch`. Verify with nominal instantiation tests.
- Status: fixed (fix 39)

### src/types/instantiate.zig:137: `Scratch.pending_constraints` buffers 88-byte constraints by value
- category D; runs per instantiation of any var carrying constraints; impact low-medium; risk low
- Finding: the copy machine stages `StaticDispatchConstraint` values (see size finding above) into `pending_constraints` and then appends them to `store.static_dispatch_constraints`. Each staged constraint is copied twice (into the pending list, then into the store). With the constraint shrunk (finding above) this halves; alternatively, the machine could reserve the destination range in the store up front …
- Fix: reserve `range.count` undefined slots in `static_dispatch_constraints` when the frame starts, write children directly into them; drop `pending_constraints`. Same for `pending_tags`/`pending_fields`/`pending_parts` where the child count is known from the source range. Must keep the ordering identical (it is: source order).
- Status: open

### src/types/TypeWriter.zig:54-82: five `AutoHashMap(Var, ...)` keyed by dense `Var`
- category B; runs per rendered type (every diagnostic, every LSP hover, every snapshot); `occurrence_counts` is filled by a full walk per root; impact low-medium in `roc check` of a clean program (only …; risk low
- Finding: `seen_set`, `seen_count_set`, `occurrence_counts`, `ext_seen`, `flex_var_names_map` are all keyed by `Var`, a dense store-local id. The file already notes the cost: "a single deep render leaves these maps holding tens of thousands of slots, and `clearRetainingCapacity` walks all of that metadata every time" and adds `clearMapIfUsed` as a mitigation. That is the symptom of hashing a dense id.
- Fix: `collections.DenseMap(Var, void)` / `DenseMap(Var, u32)` / `DenseMap(Var, FlexVarNameRange)` (paged; clear is O(live)). The membership sets that mirror a stack (`seen`/`seen_set`) could be a single `ScopedBitSet`.
- Status: fixed (fix 39)

### src/types/generalize.zig:156: `VarPool.ranks` is an `ArrayList(ArrayList(Var))`, one heap list per rank
- category F; runs per rank push/pop (per let-nesting level entered), `mergeFrom` copies rank by rank; impact low-medium; risk medium (the out-of-order `addVarToRank` …
- Finding: `ranks: std.ArrayList(VarArrayList)` with each rank initialized at capacity 16 (`ensureRanksThrough`, `pushRank`). `popRank` clears the current rank list. Vars are appended one at a time to the rank's list. This is a list-of-lists where the inner lists are short-lived per scope.
- Fix: a single `ArrayListUnmanaged(Var)` plus a `rank_starts: ArrayListUnmanaged(u32)` stack; `pushRank` pushes the current len, `popRank` truncates to the start. `addVarToRank` for a rank below current (rare path, vars unified from an outer scope) needs an insertion into an earlier segment: keep a small per-pool overflow list of `(rank, var)` consumed when that rank is popped/generalized. Verify generalization order is …
- Status: open

### src/types/store.zig:2048 (secondary): `ensureTotalCapacity(self.len())` sizes a scratch to the whole store
- category G, A; runs same as detachOccurrence; impact covered above; risk covered above
- Finding: even when the class has 2 members the scratch list reserves `len()` entries (4 bytes x every var in the module) and frees it on return.
- Fix: covered by the detachOccurrence fix.
- Status: fixed (fix 39)

### src/check/Check.zig:130-1352: ~95 `std.AutoHashMap(Unmanaged)` fields keyed by dense ids (`Var`, `CIR.Expr.Idx`, `CIR.Pattern.Idx`, `CIR.Def.Idx`, `CIR.Statement.Idx`, `CIR.Annotation.Idx`, `CIR.TypeAnno.Idx`, `u32` node ids)
- category B; runs per-module state; individual maps are hit per expression / per var / per dispatch edge (see hot list below); impact high in aggregate (this is the dominant allocation/hashing …; risk low per map; medium for iterated maps …
- Finding: the checker struct holds, by direct count of its field list, roughly 95 hash maps whose key is a dense store-local index. `Var`, `CIR.Expr.Idx`, `CIR.Pattern.Idx`, etc. are named `...Idx`/`Var` rather than `...Id`, so the `ci/zig_lints.zig` lint ("no HashMap keyed by `...Id`") never fires on any of them, yet every one is exactly the "hash a dense id" pattern design.md forbids. The checker already has …
- Fix: convert in priority order of access count to `collections.DenseMap(K, V)` (paged sparse index + compact live column, O(live) clear) or, for the `void`-valued sets that are cleared per def/per scope, to `collections.ScopedBitSet`/`DynamicBitSetUnmanaged` sized to the owning store (`types.len()`, `cir.store.nodes.len()`). Sets that are only ever grown (never cleared) over node ids (`erroneous_value_exprs`, …
- Status: fixed (fix 59): 116 maps keyed by `Var` and CIR indices are dense maps; 13 stay hashed because they back reserve-then-commit insertions, which a dense map cannot reserve for unseen keys

### src/check/Check.zig:564: `scheme_requirement_candidate_indices_by_owner: AutoHashMapUnmanaged(Var, ArrayListUnmanaged(u32))`
- category F, B; runs per scheme owner with requirement candidates; 8 access sites; impact medium; risk low
- Finding: map whose values are heap-allocated lists; every owner gets its own ArrayList allocation and the map key is a dense `Var`.
- Fix: flat `candidates: ArrayListUnmanaged(SchemeRequirementCandidate)` (already exists at line 563) plus a per-owner intrusive `next: u32` chain or a `(start,len)` span assigned when the owner is finalized; owner lookup via `DenseMap(Var, u32)` head index.
- Status: open

### src/check/Check.zig:1021,1079: `dispatch_replay_sources` / `use_replay_sources: AutoHashMapUnmanaged(u64, ArrayListUnmanaged(...))`
- category F; runs per replay shape hash (per dispatch use that can be replayed); impact low-medium; risk low
- Finding: map of hash -> heap list. Same list-per-key pattern. The u64 key is a content hash so hashing is legitimate, but the value should be a chain into a flat list.
- Fix: flat `ArrayListUnmanaged(DispatchReplaySource)` with `next: u32` per entry and the map storing only the head index.
- Status: open

### src/check/Check.zig:6719: `Env.clean_index: AutoHashMapUnmanaged(u64, ArrayListUnmanaged(u32))` per env, cleared on every `Env.reset`
- category F, E; runs `Env.reset` runs on every `env_pool.acquire()` (per top-level group/def check, line 11670/16768); `clearClean` iterates every list in the map (line 6746) and …; impact medium; risk low
- Finding: each env carries a map of trigger-key -> heap list of positions; `clearClean` walks all values calling `clearRetainingCapacity` on each (O(number of distinct keys ever seen by that env)) rather than O(live entries). Lists are never freed until env deinit, so an env that once saw many keys keeps paying the walk on every reset.
- Fix: flat `positions: ArrayListUnmanaged(u32)` + `heads: AutoHashMapUnmanaged(u64, u32)` (hash key is legitimate) with `next` chaining; reset becomes two `clearRetainingCapacity` calls. Or track the live-key list and clear only those.
- Status: fixed (fix 8)

### src/check/Check.zig:18091 + 25009: per-`checkExpr` heap `create(ExprKernelActivity)` + fresh `activities` ArrayList + fresh `tasks` ArrayList
- category A; runs per `checkExpr`/`checkExprWithFunctionOwner` call (per top-level def body via `DefActivity` line 17022, plus every other checkExpr entry: expect bodies, …; impact medium (allocation per def; amortized growth repeated per …; risk low
- Finding: `runCheckActivities` (18091) creates a stack-local `std.ArrayList(CheckActivity)` and `newExprKernelActivity` (25009) does `gpa.create(ExprKernelActivity)`; the activity's `tasks: std.ArrayList(ExprTask)` starts `.empty` and grows by doubling on every entry. Each def body therefore pays: 1 create + 1 destroy + 2 ArrayList growth sequences (activities, tasks) from zero capacity. These should be single reusable stacks …
- Fix: add `activity_stack: ArrayListUnmanaged(CheckActivity)` and `expr_kernel_pool: ArrayListUnmanaged(ExprKernelActivity)` (or a single `tasks` stack shared by all kernels since kernels nest strictly) to `Self`; `runCheckActivities` records the base length and truncates on exit (including the errdefer abort path). Verify the abort path (`abortCheckActivity`) still sees exactly the activities pushed by this call.
- Status: fixed (fix 7)

### src/check/Check.zig:24932: `ExprTask` carries a full `ExprCheckFrame` plus a `union(enum)` state sized by its largest variant
- category C; runs one `ExprTask` pushed per CIR expression that has children (every call, block, lambda, match, if, record, list, binop, ...); impact medium-high (the task stack is pushed/popped once per …; risk medium
- Finding: `ExprTask = { frame: ExprCheckFrame, expected: Expected, function_owner, does_fx, state: ExprTaskState }`. `ExprCheckFrame` (24551) holds `checker: *Self`, `env: *Env` (16 bytes of pointers identical for every frame on the stack), `expr: CIR.Expr` (a copy of the full CIR expression union; its largest variant is well over 32 bytes), `expr_region` 8, four Vars, `mb_anno_vars: ?AnnoVars`, `nested_expected: Expected`, …
- Fix: (1) drop `checker`/`env` from the frame (pass them as parameters; they are constants of the kernel); (2) replace `expr: CIR.Expr` with the `CIR.Expr.Idx` plus a re-fetch through `store.getExpr` at resume (the store is immutable during the check: verify that no code path mutates the node between push and resume; `replaceExprWithRuntimeError*` does mutate, so keep the cached tag or re-fetch only when needed), or …
- Status: open

### src/check/Check.zig:9371 / 9452: `groundTypeHash` / `groundTypesIdentical` allocate a fresh `AutoHashMap(Var,...)` + ArrayList per call
- category A, B; runs per polarized ground instance lookup (`instantiateVarPolarized` -> `groundExpectedInstance` at 9353-9356: once per hash computation and once per cache …; impact medium; risk low
- Finding: both functions build a `std.AutoHashMap(Var, u32)` / `AutoHashMap(Pair, void)` and an `ArrayListUnmanaged` on entry and `deinit` on exit; the walk itself is over a ground type that is usually small, so allocation dominates.
- Fix: reuse `self.var_map` (DenseMap(Var,Var)) / `self.var_set` or add a `scratch_ground_hash_seen: DenseMap(Var, u32)` and `scratch_type_pairs: ArrayListUnmanaged(Pair)` to `Self`; for the pair set use a `DenseMap(Var, Var)` keyed by left var when the mapping is 1:1 (it is for identical ground types) with a fallback bitset for repeats.
- Status: fixed (fix 23)

### src/check/Check.zig:9067: `GroundMarks.parents: ArrayListUnmanaged(ArrayListUnmanaged(u32))` + `index_of: AutoHashMapUnmanaged(Var, u32)`
- category F, B; runs `computeGroundMarks`/`computeGroundMarksKnowing` is called per polarized instance (`maximalGroundSubtreeCopies` 9184, `groundSubtreeEntries` 9208), per …; impact medium; risk low
- Finding: one heap ArrayList per visited var for its parent list, plus a hash map from dense Var to discovery index. `deinit` walks every inner list.
- Fix: CSR layout: `parent_edges: ArrayListUnmanaged(u32)` appended during the walk with `(child, parent)` pairs, then counting-sort into `parent_starts` (the pattern already used at Check.zig:7975-7995 for type-decl dependents). `index_of` -> `DenseMap(Var, u32)` retained on `Self` and cleared per call.
- Status: open

### src/check/Check.zig:9200 / 9215: `maximalGroundSubtreeCopies` / `groundSubtreeEntries` return `toOwnedSlice` that the caller immediately stores in `polarized_instance_subtrees`
- category D, A; runs per polarized instance; impact low-medium; risk low
- Finding: `entries` is built, `toOwnedSlice`'d (9200), then `groundSubtreeEntries` allocates `roots`, recomputes marks, builds `kept`, and `toOwnedSlice`s again (9215); the first slice is then freed. Two heap slices + one intermediate per instance where one filtered pass into a single reserved slice would do.
- Fix: compute `kept` directly from `marks` in one pass; store spans into a shared `ArrayListUnmanaged(SharedSubtree)` on `Self` instead of per-instance owned slices (the map `polarized_instance_subtrees` then holds `(start,len)`).
- Status: open

### src/check/Check.zig:772-790 (instantiate) / Check.zig:20237: `applicationArgumentReaches` allocates `result`, `seen_formals`, `pending`, and an `AutoHashMapUnmanaged(Item, void)` per type-annotation application
- category A, B; runs per `TypeAnno.Apply` node generated (every `List(a)`, `Try(a, b)`, `Dict(k, v)` written in an annotation: line 21980); impact medium (annotations are dense in library code); risk low
- Finding: four allocations and a hash-keyed visited set (`Item = {var_, reach}` where `reach` is a 2-bit enum) for a walk over the declaration template. The `result` slice is returned to the frame (owned), the others are freed on return.
- Fix: visited -> `DenseMap(Var, u8)` bitmask scratch on `Self`; `pending` -> `self.scratch_vars`-style reusable stack; `seen_formals` -> a `u64` bitmask when `formals.len <= 64` else a scratch `DynamicBitSet`; `result` can be a span into a shared `ArrayListUnmanaged(AdapterReach)` retained for the annotation generation pass.
- Status: open

### src/check/Check.zig:20688: `declareWhereAliasConstraints` builds `AutoHashMapUnmanaged(Ident.Idx, Var)` per where-alias use
- category A; runs per where clause referencing a where alias (per annotation with `where [a.Eq]`-style aliases); impact low-medium; risk low
- Finding: `subs` is a tiny map (receiver + params, 1-4 entries) created and destroyed per call; the consumer `instantiateWhereAliasConstraint` passes it to the instantiator's `substitute_rigids_*`.
- Fix: same as the instantiate.zig:97 finding: a reusable pair list with linear lookup, or a `DenseMap(Ident.Idx, Var)` scratch (`rigid_var_substitutions` at line 245 already exists for this purpose and could be reused with a save/restore of its live entries).
- Status: open

### src/check/Check.zig:24360: `projectExpectedTagPayload` builds `AutoHashMapUnmanaged(Ident.Idx, Var)` per tag expression with an expected nominal type
- category A; runs per `e_tag` checked against an expected nominal (line 26598), i.e. per constructor application in a position with a known type; impact medium (tags are everywhere); risk low-medium
- Finding: `substitutions` (formal rigid name -> actual) is created, filled with `decl.formals.len()` entries (typically 0-3), handed to the instantiator, and destroyed.
- Fix: as above: reusable scratch; better, pass the `formals`/`actuals` slices and let the instantiator substitute by resolved root via the `var_map` DenseMap that is already seeded two lines earlier (the rigid-name route is only needed for associated-type rigids; verify with the nominal payload projection tests at 24402-24470).
- Status: fixed (fix 40)

### src/check/Check.zig:21359: `beginAliasDeclBody` `gpa.create(ArrayListUnmanaged(Var))` per alias declaration
- category A, F; runs per type alias declaration (`hidden_exts` boxed so `alias_hidden_exts: ?*ArrayListUnmanaged(Var)` can point at it; destroyed at 21116); impact low; risk low
- Finding: a heap box holding a list that lives only for the declaration's body generation; the pointer indirection exists only because the frame stores an optional pointer.
- Fix: keep one `alias_hidden_exts: ArrayListUnmanaged(Var)` on `Self` with a stack of start offsets (`alias_hidden_ext_starts`) for nesting; `freshAliasHiddenExt` appends, `finishAliasDeclarationParameters` slices from the start offset.
- Status: open

### src/check/Check.zig:21399: `finishAliasDeclarationParameters` builds `args` ArrayList + `seen` AutoHashMap per alias decl
- category A; runs per alias declaration; impact low; risk low
- Finding: copies alias args into a fresh list and dedups hidden vars through a fresh hash set, then calls `mkAlias...` which copies the slice again into the store.
- Fix: `self.scratch_vars` for `args`; `self.var_set` (DenseMap) for `seen`.
- Status: open

### src/check/Check.zig:21837: `stepRigidAnnoGen` builds `AutoHashMap(Ident.Idx, usize)` of constraint representatives per rigid var with where clauses
- category A; runs per rigid type variable that carries where-clause constraints (per constrained annotation); impact low-medium; risk low
- Finding: `constraint_representatives` is created with capacity `scratch_constraints_end - scratch_constraints_start` (usually 1-3) and destroyed; the fn_name dedup over a handful of constraints is cheaper as a linear scan of the scratch slice.
- Fix: linear scan over `scratch_static_dispatch_constraints.items[start..end]` comparing `fn_name` (n is tiny); if n can be large, use `DenseMap(Ident.Idx, u32)` scratch.
- Status: open

### src/check/Check.zig:7305-7370: `validateSettledValueRows` walks every CIR node and uses `AutoHashMap(Var, void)` sets over the whole published type graph
- category B, E; runs once per module check (plus the per-row loop at 7361-7370 which clears two small hash sets per row root); impact medium (once per module but proportional to module size …; risk low
- Finding: a module-wide walk with `settled_visited: AutoHashMap(Var, void)` (the comment at 7308-7310 explains it was moved off the shared set because clearing it afterwards would be O(module)), `semantic_row_roots: AutoHashMapUnmanaged(Var, Var)`, `seen_names: AutoHashMapUnmanaged(Ident.Idx, void)`, `seen_parts: AutoHashMapUnmanaged(Var, void)`, `row_roots` sorted with `std.mem.sort` (7355). It also iterates all …
- Fix: visited/row roots -> a `DynamicBitSetUnmanaged` sized `types.len()` (one bit per var; O(n/64) clear) and `DenseMap(Var, Var)`; `seen_parts`/`seen_names` -> epoch-stamped `[]u32` columns over vars/idents. The seed loop could use the node-tag column directly (`nodes.items.items(.tag)`) rather than `nodes.get(idx).tag` per node.
- Status: partly fixed (fix 59): its dense-keyed maps are dense maps

### src/check/Check.zig:7494: `normalizeRowUnion` creates six containers (incl. `AutoHashMapUnmanaged(Ident.Idx, RowLabelOccurrence)`) per call
- category A; runs per row root reached by `validateSettledValueRows` (7441) and per row normalized at unification time (7744): i.e. per record/tag-union row with an extension …; impact medium; risk low
- Finding: `parts`, `part_labels` (holding full `Content` values, 28 bytes each), `labels`, `latest` (hash map), `omitted`, `pairs` are allocated per call and freed at return; the inner `while (true)` loop clears them per iteration. Parts are deduplicated by a linear scan (`for (parts.items) |part| if (part == resolved.var_)`).
- Fix: move all six into a `RowNormalizeScratch` on `Self`; `latest` -> `DenseMap(Ident.Idx, RowLabelOccurrence)`; store `Content` tags/ranges rather than whole `Content` in `part_labels`.
- Status: open

### src/check/Check.zig:30747: `collectAbsentCtorPayloadBlockers` creates a `std.heap.ArenaAllocator` and a `NominalOpenCache` per call
- category A; runs per lambda/def whose constructed tags are checked for absent payloads (30803, 30829: per `closeAbsentConstructedPayloadVars*` call, i.e. per lambda body and …; impact medium; risk low
- Finding: `std.heap.ArenaAllocator.init(self.gpa)` (the std arena pays atomics per alloc; `base.SingleThreadArena` exists and is already used by `exhaustive.checkMatch`) plus a fresh `exhaustive.NominalOpenCache` per call, both discarded after one query.
- Fix: use `base.SingleThreadArena` retained on `Self` with `reset(.retain_capacity)` per call; keep one `NominalOpenCache` per module (it caches nominal backing openings, which are module-stable) and reuse it in `checkMatch`/`checkDestructure` call sites (29602, 30900) too.
- Status: fixed (fix 15: arena); cache reuse open

### src/check/Check.zig:29602: `exhaustive.NominalOpenCache.init` per match expression
- category A; runs per `match` and per destructuring pattern (29602, 30900); impact medium; risk medium (must respect the cache's read …
- Finding: a fresh open-cache per analysis site; the cache's purpose is to avoid re-opening nominal backings, but a per-site lifetime means the same `Try`/`List` opening is redone at every match in the module.
- Fix: one cache on `Self`, invalidated only when the type store mutates in a way that changes a cached opening (the cache's `reader`/`finishRead` protocol already brackets uses). Verify `backfillRegionsForAnalysisVars` still covers vars minted by shared openings.
- Status: no change: `NominalOpenCache.init` allocates nothing; its type view and memo are created on the first read

### src/check/Check.zig:35395: `defaultMaterializationIsRecursive` allocates seven containers (five hash sets) per pending default check
- category A, B; runs per record-field default expression pending a recursion check (35068, once per default per module); impact low-medium (defaults are rare, but each walk can reach a …; risk low
- Finding: `expr_work`, `visited_defaults: AutoHashMap(u32,void)`, `visited_exprs: AutoHashMap(Expr.Idx,void)`, `seed_work`, `visited_seed_vars: AutoHashMap(Var,void)`, `visited_instantiations: AutoHashMap(u32,void)`, `invoked_work`, `visited_invoked: AutoHashMap(Expr.Idx,void)`, `local_pattern_to_expr: AutoHashMap(Pattern.Idx, Expr.Idx)`: all created and destroyed per call.
- Fix: a `DefaultRecursionScratch` struct on `Self` with `DenseMap`/bitset members cleared per call.
- Status: open

### src/check/Check.zig:37196-37300: `deduplicateGeneralizedDispatchRequirements` builds four hash maps and three lists per generalized scheme
- category A, B; runs per generalized scheme with more than one dispatch requirement (predeclared schemes 17371, methods 18470, decls 27317); impact medium; risk low
- Finding: `anchors: AutoHashMap(Var, void)`, `callable_shapes: AutoHashMap(Var, [32]u8)` (32-byte values hashed per callable), `retained_by_key: AutoHashMap(GeneralizedAttachedConstraintKey, Var)`, `seen: AutoHashMap(GeneralizedSchemeRequirementKey, SeenRequirement)` plus `pending_receivers`, `snapshot: ArrayList(StaticDispatchConstraint)` (88-byte elements), `retained_constraints`. The key maps are legitimately structural …
- Fix: `anchors` -> `self.var_set`; `callable_shapes` -> `DenseMap(Var, [32]u8)` scratch on `Self`; keep `retained_by_key`/`seen` but retain them on `Self` with `clearRetainingCapacity` so their tables are not rebuilt per scheme; `snapshot`/`retained_constraints` -> `self.scratch_static_dispatch_constraints` ranges.
- Status: open

### src/check/Check.zig:50158: `recordDerivedMapPlan` scans every static-dispatch constraint in the store, resolving each `fn_var`
- category E; runs per derived `map`/`map!` constraint accepted (50232); impact medium-high for codec/derive-heavy modules (quadratic in …; risk low-medium (must preserve the "every …
- Finding: `for (self.types.static_dispatch_constraints.items.items) |*constraint| if (self.types.resolveVar(constraint.fn_var).var_ == fn_root) constraint.derived_map_plan = plan;`: O(all constraints in the module) with a union-find resolve per constraint, to update the (usually one) constraint sharing a callable class. Each derived-map dispatch in a module pays a full scan.
- Fix: the constraint being validated is known (`constraint.fn_var` at 50232); record the plan on exactly the constraints the checker reached (the validated constraint's own slot, found by index since `sliceStaticDispatchConstraints(range)` gives its position), or keep a `DenseMap(Var /*fn root*/, u32 /*first constraint idx*/)` with a `next` chain built when constraints are appended. If "every constraint in the same …
- Status: open

### src/check/Check.zig:50031-50065, 50134, 52667-52767: `gpa.dupe` of nominal/alias/tag arg slices to survive store growth
- category A, D; runs per derived-map analysis (`analyzeDerivedMap`), per mapped tag (`mappedStructuralTagUnion`: one `dupe` per tag plus `mapped_tags` alloc), per derived-codec …; impact medium for derive-heavy modules; risk low
- Finding: the comment says "the argument list points into the types store, which the comparison and the walk both read through", so each function `dupe`s the `[]Var` slice to a heap copy before calling anything that may append to `store.vars`. These are tiny slices (0-4 vars) allocated and freed per call; in `takeDerivedCodecBackingWalk` the `assumed: AutoHashMap(DerivedCodecVarPair, void)` map is also created per call.
- Fix: copy into `self.scratch_vars` (a `base.Scratch(Var)` already on `Self`, with `top()`/`clearFrom()` for nesting) instead of `gpa.dupe`; `assumed` -> a scratch `DenseMap`/pair list retained on `Self`.
- Status: open

### src/check/Check.zig:50487: `beginImplicitParse` `gpa.create(ImplicitParseRun)` per implicit parse request
- category A; runs per implicit parse constraint validated (50463, 53009 nested); impact low; risk low
- Finding: a heap box for a per-request state record freed in `endImplicitParse`.
- Fix: a stack `ArrayListUnmanaged(ImplicitParseRun)` on `Self` (requests nest LIFO at 53009); indices instead of pointers.
- Status: open

### src/check/Check.zig:17754: `relatePredeclaredUse` creates `AutoHashMap(Var, Var)` per predeclared-scheme use
- category A, B; runs per use of a predeclared (recursive-group / annotated-before-body) scheme: every recursive call site; impact medium; risk low
- Finding: `images` maps body slot var -> image var for one relation pass; created and destroyed per use.
- Fix: `self.link_var_map` / a dedicated `DenseMap(Var, Var)` scratch cleared per call.
- Status: partly fixed (fix 59): its dense-keyed maps are dense maps

### src/check/Check.zig:17392 / 614: `hole_shared_schemes: AutoHashMapUnmanaged(Var, []const Var)` with `gpa.dupe`'d owned slices
- category F, B; runs per predeclared annotation with live holes; impact low; risk low
- Finding: an owned heap slice per scheme, keyed by dense Var.
- Fix: spans into a shared `ArrayListUnmanaged(Var)`; `DenseMap(Var, Span)`.
- Status: open

### src/check/Check.zig:4384-4470: `rejectCapturingMethods` `LocalFunctionCaptures.refs: AutoArrayHashMapUnmanaged(Pattern.Idx, ArrayListUnmanaged(Pattern.Idx))`
- category F, B; runs once per module that declares local methods; `collectLexicalScope` (4570) allocates a `stack` ArrayList per method and `LexicalScope.patterns` is an …; impact low (once per module); risk low
- Finding: list-per-key map plus hashed dense pattern ids; `collectPatternSubtree` (4592) allocates a `pending` ArrayList per pattern visited.
- Fix: CSR spans for refs; bitset over `nodes.len()` for `patterns`; reusable `pending` stack.
- Status: open

### src/check/Check.zig:4601: `isModuleTypeDecl` lazily fills `module_type_decls: AutoHashMapUnmanaged(Statement.Idx, void)`
- category B; runs per local type declaration check; impact low; risk low
- Finding: a set of dense statement ids; `cir.type_decls` is already a contiguous span of statement indices, so membership could be a sorted-span binary search or a bitset.
- Fix: `DynamicBitSetUnmanaged` over `nodes.len()` or binary search in the span (if sorted).
- Status: partly fixed (fix 59): its dense-keyed maps are dense maps

### src/check/Check.zig:8148: `validateNominalDeclArgumentGrowth` `AutoHashMap(NominalDecl.Idx, u32)`
- category B; runs once per module; impact low; risk low
- Finding: `slot_by_decl` keyed by a dense `NominalDecl.Idx` (named `Idx`, so the lint does not catch it).
- Fix: a `[]u32` column sized `nominalDeclCount()` with a sentinel.
- Status: open

### src/check/Check.zig:12118: `poisonCheckedStrictDemandCycles` `AutoHashMapUnmanaged(Def.Idx, void)` (error path)
- category B; runs only when a cycle is reported; impact low; risk low
- Finding: dense-id-keyed set on an error path; harmless but inconsistent with the dense-id rule.
- Fix: bitset over defs.
- Status: partly fixed (fix 59): its dense-keyed maps are dense maps

### src/check/Check.zig:15918-15945: `checkPlatformHostedSection` `AutoHashMap(HostedTarget, []const u8)` with `allocPrint`'d names + `StringHashMap(void)` for symbols
- category A, D; runs once per platform module; impact low; risk low
- Finding: every declared hosted function's display name is `allocPrint`'d (16027, 16055) and stored in the map, freed at the end; symbol strings are hashed by content.
- Fix: store `(module ident, func ident)` pairs and format names only when a diagnostic is emitted; symbols could be interned once into the ident store and compared by `Ident.Idx`.
- Status: open

### src/check/Check.zig:36100: `collectInstanceRequirements` `AutoHashMapUnmanaged(Var, void)` + `pending` per instance
- category A, B; runs per use-instance settled (36072); impact low-medium; risk low
- Finding: visited set and pending stack created per call.
- Fix: `self.var_set` + `self.type_visit_stack`.
- Status: partly fixed (fix 59): its dense-keyed maps are dense maps

### src/check/Check.zig:39091: `snapshotPreDefaultType` `AutoArrayHashMapUnmanaged(Var, PreDefaultGroup)` whose values own `ArrayListUnmanaged(StaticDispatchConstraint)`
- category F; runs per diagnostic about a defaulted type (error path); impact low; risk low
- Finding: list-per-group with 88-byte constraint copies; dedup by nested linear scan.
- Fix: low priority (diagnostic path); flat scratch with spans.
- Status: open

### src/check/Check.zig:39327: `closeValueRowTailsCarryingDerivations` `AutoHashMap(Var, void)` reused across all defs
- category B; runs once per module, `clearRetainingCapacity` per top-level def (walks the whole table each time: O(defs x table capacity)); impact low-medium; risk low
- Finding: `visited` is cleared per def; after one deep def it holds a large table and every subsequent clear memsets it.
- Fix: `DenseMap(Var, void)` (clear is O(live)) or an epoch column.
- Status: partly fixed (fix 59): its dense-keyed maps are dense maps

### src/check/Check.zig:11978: `withdrawSupersededLiteralDefaultWarnings` `AutoHashMap(Var, void)` + full problem-list compaction
- category B; runs once per module when any undetermined type was reported; impact low; risk low
- Finding: builds a resolved-root set then scans all problems.
- Fix: `self.var_set`.
- Status: partly fixed (fix 59): its dense-keyed maps are dense maps

### src/check/Check.zig:35076: `checkDefaultRestrictions` `gpa.dupe` of the whole `record_omitted_defaults` table
- category D; runs once per module with pending default checks; impact low; risk low
- Finding: copies the entire table so `retireRejectedDefault` can mutate the live one while reading the copy.
- Fix: iterate by index with the table's original length captured, or retire after collecting indices.
- Status: open

### src/check/Check.zig:13876: `replaceExprWithRuntimeErrorAfterOperands` `gpa.dupe` of kept operands (error path)
- category D; runs error recovery only; impact low; risk low
- Finding: dupes `recorded[0..kept_len]` to pass to a visitor and then `appendExprSpan(kept)` copies it again.
- Fix: `self.scratch_exprs`-style buffer.
- Status: open

### src/check/Check.zig:16262: `processAppPlatformRequirements` `AutoHashMap(Var, Var)` + `AutoHashMap(AliasSource, Var)` once per app
- category B; runs once per app module; impact low; risk low
- Finding: alias substitution maps built per compile; small.
- Fix: `DenseMap(Var, Var)`; low priority.
- Status: open

### src/check/Check.zig:379: `ident_to_var_map: AutoHashMap(Ident.Idx, Var)` and `rigid_var_substitutions` / `type_decl_rigid_vars: AutoHashMapUnmanaged(Ident.Idx, Var)`
- category B; runs per rigid-var annotation lookup (every `a` in `List(a) -> a`), `clearRetainingCapacity` per declaration (21374); impact medium (annotation generation is hot in library modules); risk low
- Finding: `Ident.Idx` is a dense 29-bit id in the module's ident store; these maps hash it. `type_decl_rigid_vars.clearRetainingCapacity()` walks the table per decl.
- Fix: `DenseMap(Ident.Idx, Var)` (keyIndex = idx bits).
- Status: open

### src/check/Check.zig:23116: `Expected` struct is copied into every `ExprTask` twice (`task.expected` and `frame.nested_expected`)
- category C; runs per expression with children; impact low; risk low
- Finding: `ExprTask` carries `expected: Expected` and `frame.nested_expected: Expected`; `Expected` is a union with a payload (variant with `Var` + context). Duplicated storage per task.
- Fix: store `expected` once in the task and derive `nested_expected` on resume (it is computed from `expected` + annotation in `beginExprCheckFrame`), or keep only the delta.
- Status: open

### src/check/Check.zig:2590-2592, 12595-12642, 33841-33844, 35344-35352: nested helper structs with `AutoHashMapUnmanaged(Expr.Idx|Pattern.Idx|u32, ...)` fields
- category B, F; runs hoist selection (per hoist candidate set), callable stability (`callable_stability`, `local_procedure_stability`), settled-value causes, instantiation index …; impact medium (the scheme-use index is rebuilt from the records …; risk low
- Finding: more dense-id-keyed maps, two of them list-per-key (`node_to_scheme_uses`, `instantiations_by_var` at 35347/35352).
- Fix: DenseMap / bitsets; CSR spans for the two list-valued maps (build by counting pass over `scheme_uses`, which is the producer's own table).
- Status: open

### src/check/unify.zig:2900-2903: `partitionFields` sorts both gathered field runs with `std.mem.sort` on every record unification
- category E; runs per record-vs-record unify (every record literal, every record access against a record type, every row extension); impact medium-high (records are ubiquitous; each sort does O(n log …; risk low (order must be identical: rank …
- Finding: "Sort the fields (gathering maintains partial order, but unification may create unsorted unions)": both runs are sorted with a comparison sort that calls `Ident.textOrder(store.getText(a), store.getText(b))`, i.e. two string lookups and a byte comparison per comparison. The tag path (3585-3594) at least checks `ordered` first and skips the sort; the record path does not.
- Fix: (1) add the same "already ordered" pre-check used for tags; (2) compare by the ident store's precomputed text ranks (`base.TextRankCache` / `Ident.Store.textRanks` exist for exactly this; design.md says row ordering uses rank comparison) instead of re-reading text; (3) when rows come from the store already sorted (the common case: `mkRecord` sorts), skip entirely.
- Status: fixed (fix 5)

### src/check/unify.zig:3822-3829: `std.mem.sort` of constraint index runs on every constraint-bearing flex/rigid unification
- category E; runs per unify of two constrained variables (every method call on a polymorphic receiver, every numeric literal unify); impact medium; risk low
- Finding: both `a_indices` and `b_indices` are sorted with `ConstraintOrder.lessThan` (which reads `fn_name` text and origin tags) so same-name groups can be matched linearly. Constraint lists are short (1-5) but this runs extremely often; a comparison sort with text lookups is heavier than an insertion sort on precomputed name ranks.
- Fix: precompute the sort key once per constraint (`(name_rank: u32, is_method_call: u1, producer_index)`) into a scratch `[]u64` and sort that (or insertion-sort for n <= 8); or keep constraint ranges sorted at append time so only merges are needed.
- Status: fixed (fix 6)

### src/check/unify.zig:2980: `demandPureFunction` allocates `std.ArrayList(PureDemand)` per call
- category A; runs per function-effect demand (per call whose callee effect must be pure; per `fn_unbound` resolution); impact low-medium; risk low
- Finding: fresh list per call, `deinit` on return; depth is usually 1-2 entries.
- Fix: reuse a `pending_pure_demands` list in `Scratch` with a base-length protocol (the function already handles nested errors via `undoPureDemands`).
- Status: open

### src/check/unify.zig:4376: `Scratch.visited_index: AutoHashMapUnmanaged(VisitedPairKey, u32)`
- category B; runs per unify call that reaches the pair guard (cyclic/recursive types; `visited_index_generation` bumps per call); impact low; risk low
- Finding: pair of dense vars hashed. Acceptable as a `...Key` but the file also keeps `visited_vars` as a list; the index is rebuilt (`clearRetainingCapacity` at 622/4616) per call.
- Fix: `DenseMap(Var, u32)` keyed by the `a` var with a short chain for the `b` var (pairs per `a` are few), or leave as is (it is a genuine pair key) but avoid the full-table clear by using the generation stamp already present.
- Status: open

### src/check/unify.zig:4329-4410: `Scratch` holds ~25 separate SafeLists/ArrayLists that are all cleared per unify call (`reset`, lines 4594-4623)
- category E; runs per `unify` call (the hottest function in the checker); impact medium (unify is called millions of times on large modules); risk low
- Finding: `reset` performs ~28 `clearRetainingCapacity` calls (each a store of `len = 0`, cheap) plus `visited_index.clearRetainingCapacity()` (a memset over the hash table's metadata, O(capacity)) and `effect_dependency_seen.clearRetainingCapacity()` on a DenseMap. The hash-map clear is the only non-O(1) one but it runs on every unify even when no pair was recorded.
- Fix: clear `visited_index` only when `visited_index_pairs != 0` (the field exists at 4378): a one-line guard; same pattern as TypeWriter's `clearMapIfUsed`.
- Status: open

### src/check/occurs.zig:400-401: `seen_positions: AutoHashMapUnmanaged(Var, usize)` and `visited: AutoHashMapUnmanaged(DescStoreIdx, void)`
- category B; runs per occurs check (per unify that merges structure: the occurs check runs after each structural union), `reset` clears both maps (O(capacity) each) per check; impact medium-high (occurs runs per structural unify); risk low
- Finding: two dense-id-keyed hash maps; `visited` is pre-sized to 64 at init and grows with the deepest type ever checked; every `reset` walks the whole table.
- Fix: `DenseMap(DescStoreIdx, void)` + `DenseMap(Var, u32)` (O(live) clear) or an epoch-stamped `[]u32` column over descs/vars owned by the scratch and grown to `store.len()`.
- Status: fixed (fix 3)

### src/check/copy_import.zig: no per-node allocations found beyond store appends
- category, ; runs per imported var
- Finding: reviewed; the copy walk uses store appends and the caller's maps. No finding.
- Status: open

### src/check/exhaustive.zig:594-631: `NumeralKeyInterner` builds a `StringHashMapUnmanaged(u32)` with a heap `ArrayList(u8)` key per numeric literal pattern
- category A, D; runs per numeric literal pattern in every `match` (per branch pattern); impact low-medium; risk low
- Finding: for each literal pattern, allocates a key buffer (`ensureTotalCapacity` 4+before+after+5 bytes), fills it, and `getOrPut`s it into a string-keyed map (the arena keeps the bytes). The key is a serialization of `(before, after, scale, flags)` that the module env already identifies by `NumeralLiteral` index (`numeralLiteralForNode`).
- Fix: key the interner by the `NumeralLiteral.Idx` (dense) when two patterns resolve to the same literal entry, and only fall back to byte comparison for distinct entries with identical digits (rare); or hash `(before, after)` slices directly with a `HashMap` context that borrows the env's digit bytes instead of copying them.
- Status: open

### src/check/exhaustive.zig:1031, 2155, 1572, 1694, 1777: `AutoHashMap(Var, void)` ext/seen sets created per union gather
- category A, B; runs per tag union gathered during a match analysis (`gatherTagUnion` 1031: per column resolution), per payload scan; impact low-medium; risk low
- Finding: `seen_exts = std.AutoHashMap(Var, void).init(allocator)` created and destroyed per gather; extension chains are 1-3 long so the map is pure overhead; the arena allocator at least makes `deinit` cheap but `init`+growth still cost.
- Fix: a linear scan over a small `ArrayList(Var)` on the arena (chains are bounded by the store's extension depth and in practice tiny), or a bitset over `types.len()` reused across the analysis.
- Status: open

### src/check/exhaustive.zig:1226-1259: `InhabitedGraph` nodes own `parents: ArrayList(usize)` each; `roots`/`known_empty` keyed by `Var`; `rows` keyed by `RowKey{root, role}`
- category F, B; runs per inhabitedness query (per match column with a possibly-uninhabited type; memoized by `inhabitedness_memo` but each miss builds a graph); impact low-medium; risk low
- Finding: list-of-lists for parents (`deinit` walks all nodes), and `usize` for indices/`remaining` where u32 suffices (not an invariant issue since these are graph-local, but doubles memory).
- Fix: CSR edges (append `(child,parent)` then group) or an intrusive `first_parent/next_parent` chain; `roots` -> `DenseMap(Var, u32)`.
- Status: fixed (fix 35)

### src/check/exhaustive.zig:2429-2457, 2601-2652, 3808: per-column `ArrayList(ListArity)` built and `toOwnedSlice`'d
- category A, D; runs per list-pattern column specialization; impact low; risk low
- Finding: arities collected into a list then moved into an owned slice on the arena; both allocations are arena-backed, so cheap, but each `toOwnedSlice` on an arena allocates a second copy (arena cannot shrink in place).
- Fix: count first then `arena.alloc` exact and fill (the matrix row count is known).
- Status: open

### src/check/exhaustive.zig:2486, 2695-2825, 3419-3635: matrix specialization allocates a new row slice per row per specialization step
- category A; runs per row per constructor specialization (the core of the usefulness algorithm; O(rows x ctors x depth)); impact medium (large matches with many constructors); risk medium
- Finding: `new_row = try allocator.alloc(UnresolvedPattern, c.args.len + rest.len)` per surviving row; rows are rebuilt by concatenation at every step. All on the per-match `SingleThreadArena`, so the cost is bump allocation + copy, not malloc; still memory grows with the product of rows x specialization depth and the whole match's arena is held until the result is consumed.
- Fix: standard: represent a row as `(prefix_slice, rest_slice)` pairs or as a persistent list with shared tails so specialization does not copy `rest`; or at least `arena.reset(.retain_capacity)` between matches (the arena is created per `checkMatch`, so capacity is not retained across matches: keep one arena per module and reset it per match).
- Status: open

### src/check/exhaustive.zig:3968: `checkMatch` creates a fresh `base.SingleThreadArena` per match; result owns it
- category A; runs per `match` expression; impact low-medium; risk low
- Finding: a new arena (first chunk malloc) per match; the result struct owns it so the capacity cannot be reused by the next match in the module.
- Fix: copy the (small) result out into checker-owned storage and reset the arena with `retain_capacity` for the next match; one arena per checker.
- Status: open

### src/check/exhaustive.zig:2023, 2041, 2715: `AutoHashMapUnmanaged(u29, ...)` keyed by raw `Ident.Idx.idx`
- category B; runs per tag-union constructor gather / per record-field alternative gather; impact low; risk low
- Finding: tag ids keyed by the 29-bit dense ident index through hash maps (`TagIdsByName`, `seen`, `pat_positions`).
- Fix: `DenseMap(Ident.Idx, TagId)` or an arena `[]u32` indexed by ident (sized `idents.len()`: too big per gather) -> use `DenseMap`.
- Status: open

### src/check/dispatch_evidence.zig:227-246: `Scratch.visited: AutoHashMapUnmanaged(Var, void)`, `emitted_methods: AutoHashMapUnmanaged(Ident.Idx, u32)`, `contract_classes: AutoHashMapUnmanaged({param,callable:Var}, void)`
- category B; runs per evidence enumeration (per dispatch edge whose evidence is minted; `scratch_method_mint_evidence` on the checker); impact medium; risk low
- Finding: dense-id keyed sets cleared per enumeration.
- Fix: `DenseMap(Var, void)`, `DenseMap(Ident.Idx, u32)`; `contract_classes` keyed by `callable` Var with param stored as value bitmask.
- Status: open

### src/check/dispatch_evidence.zig:181: `PathMemo.values: AutoHashMapUnmanaged({root:u32, node:u32}, T)`
- category B; runs per path resolution per root; impact low-medium; risk low
- Finding: a (root, path node) pair memo; both halves are dense indices.
- Fix: `DenseMap(u32 /*node*/, {root, value})` with chain, or a per-root epoch column over path nodes (nodes are `path_nodes` indices, dense and small).
- Status: open

### src/check/static_dispatch_registry.zig:2189-2217, 2856-2862: plan table build uses `AutoHashMapUnmanaged(CIR.Expr.Idx|CIR.Node.Idx|Var, ...)`
- category B; runs once per module (artifact publication), each map holds one entry per dispatch expression; impact low-medium; risk low
- Finding: `by_expr`, `numeral_by_node`, `quote_by_node`, `iterator_for_by_node`, `StaticDispatchConstraintIndex.by_fn_var`, `live_fn_vars` are dense-id keyed; they are built by iterating `0..nodeCount()` anyway, so a `[]u32` column with a sentinel (one per node) costs 4 bytes/node and no hashing. These tables are later serialized (`plan_sources`), so verify the serialized form is unchanged (it is a sorted entry list, …
- Fix: `[]PlanId` column over `nodeCount()` (or `DenseMap`) during the build; keep the serialized sorted tables as they are.
- Status: open

### src/check/static_dispatch_registry.zig:2361: `role_candidates: AutoHashMap(GeneratedCodecRoleKey, ArrayListUnmanaged(GeneratedCodecRoleCandidate))`
- category F; runs per generated codec call during plan-table build; impact low; risk low
- Finding: list-per-key map; `deinit` walks every list.
- Fix: flat candidates list + `next` chain; the key is structural so the map itself is fine.
- Status: open

### src/check/static_dispatch_registry.zig:2243: `allocator.alloc(StaticDispatchOperand, explicit_args.len + 1)` per dispatch call, freed after `pushOperands`
- category A, D; runs per dispatch-call expression in the module; impact low-medium; risk low
- Finding: builds the operand array on the heap just to copy it into `operand_pool` via `pushOperands`.
- Fix: `operand_pool.ensureUnusedCapacity(len+1)` then append directly, returning the span.
- Status: open

### src/check/checked_artifact.zig:18741-18785: `CheckedEvidenceBuilder` holds 13 `AutoHashMap(u32|Var, ...)` keyed by raw node ids / vars, plus `callable_contract_buckets: AutoHashMapUnmanaged(u64, ArrayListUnmanaged(Span))`
- category B, F; runs artifact publication, once per module; maps are hit per scheme-use record / per dispatch site; impact medium (publication is a measurable fraction of `roc check` …; risk low
- Finding: `value_use_by_node`, `target_by_fn_var`, `where_method_use_by_fn_var`, `where_method_use_by_fn_root`, `source_by_checked_expr`, `local_value_scheme_by_var`, `value_use_record_by_pattern`, `node_by_record`, `record_in_progress`, `site_seen` are all `AutoHashMap(u32, ...)` whose keys are CIR node indices, record indices, or raw `Var` ints (lint bypassed by using `u32`). `callable_contract_buckets` is list-per-key. …
- Fix: columns over `nodeCount()` / `scheme_uses.len()` / `types.len()` with sentinels (the builder iterates all nodes anyway), `DenseMap` for sparse ones; CSR spans for `callable_contract_buckets` and `DispatcherParams`.
- Status: open

### src/check/checked_artifact.zig:18346-18380, 12527-12545, 17112, 24088, 28745: per-publication local `AutoHashMap(u32|CIR.Pattern.Idx|CIR.Def.Idx|Var, ...)` sets built by scanning all nodes
- category B; runs once per module each; impact low-medium; risk low
- Finding: `local_schemes: AutoHashMap(u32, Var)`, `nested_function_uses: AutoHashMap(u32,u32)`, `instantiated_scheme_roots: AutoHashMap(Var, void)`, `promoted_patterns` (built twice, 12527 and 18375), `recursively_referenced_patterns`, `rejected_method_decls`, `recursive_reference_nodes`, `site_by_expr: AutoHashMapUnmanaged(CheckedExprId, u32)` (a dense `...Id` key: this one the lint should flag; it uses …
- Fix: bitsets / columns over the node domain; share `promoted_patterns` between the two builders.
- Status: open

### src/check/checked_artifact.zig:4024-4036: `CheckedTypePublication.identity_origins / synthetic_variable_roots: AutoHashMapUnmanaged(CheckedTypeId, ...)`
- category B; runs per published type; impact low-medium; risk low
- Finding: dense `CheckedTypeId` keys in hash maps (named `...Id`; whether the lint catches `AutoHashMapUnmanaged(CheckedTypeId,` depends on its regex: flag for review).
- Fix: `DenseMap(CheckedTypeId, CheckedTypeId)` / bitset.
- Status: fixed (fix 57)

### src/check/checked_artifact.zig:18803, 706, 27231: `std.heap.ArenaAllocator` for enumerated paths / export table / platform pairing
- category A; runs once per module / per pairing; impact low; risk low
- Finding: std arena (atomic bump) where `base.SingleThreadArena` is the project's non-atomic arena.
- Fix: `base.SingleThreadArena`.
- Status: open

### src/check/checked_artifact.zig:3180-3635: `assumed: AutoHashMap(CheckedTypeExactPair, void)` created per exact-equality query
- category A; runs per checked-type exact comparison (3324: a fresh map per call in the non-memoized path); impact low-medium; risk low
- Finding: co-inductive pair set allocated per comparison.
- Fix: retain one on the comparer/view and clear per query (DenseMap keyed by left id with right id chain).
- Status: open

### src/check/canonical_type_keys.zig:2321-2333: `EqualityOracle` with three `AutoHashMap` (`[2]Var`, `Var->Var`, `Var->Var`)
- category B, A; runs per structural type-equality query (used by scheme interning and tests); impact low; risk low
- Finding: `forward`/`backward` are dense-Var keyed; `assumed` is a pair set.
- Fix: `DenseMap(Var, Var)` x2; pair set as DenseMap keyed by left with chain.
- Status: open

### src/check/type_key_engine.zig:364: `appendIdentityOrder` builds `AutoHashMapUnmanaged(u32, void)` + `Store` + frames per call
- category A, B; runs per identity-order request (per scheme key computation with identities); impact low-medium; risk low
- Finding: fresh visited set keyed by dense node id, fresh `Store`, fresh frames.
- Fix: scratch on the engine (`Engine` already owns ~30 scratch lists at 448-489); visited as `DenseMap(u32, void)`.
- Status: open

### src/check/type_key_engine.zig:481-482, 1029: `local_groups: StringHashMapUnmanaged(u32)` + `std.heap.ArenaAllocator` holding `dupe`'d signature bytes
- category D, A; runs per refinement round of the canonical-key partition (per key computation, per round); impact low-medium; risk low
- Finding: every new signature is `dupe`'d into an arena to serve as a string key; the arena is reset per key (1004) with `retain_capacity`, so the main cost is the per-signature copy + string hashing. Signatures are already in `walk_bytes`/`signature` buffers.
- Fix: key the map by `(offset, len)` into a retained `signature_bytes` buffer with a custom context (hash/eql read the buffer), so no copy; use `base.SingleThreadArena` if an arena stays.
- Status: open

### src/check/const_store.zig:904, 1121: `blob_index: StringHashMap(ConstBlobDataId)` with `dupe`'d keys
- category D; runs per constant blob added (per string/bytes constant evaluated at compile time); impact low-medium; risk low
- Finding: `addBlobData` copies the bytes into `blob_backing` (the owned copy) AND `dupe`s them again as the map key, kept in `blob_index_keys` for freeing.
- Fix: hash-map context keyed by `ConstRange` into `blob_backing` (bytes are append-only, so views are stable); drop `blob_index_keys`.
- Status: open

### src/check/const_store.zig:1224-1240: `verifyComplete` allocates four whole-store columns (debug-only)
- category, ; runs debug builds only (`if (builtin.mode != .debug) return;`)
- Finding: fine for a debug invariant check; noted for completeness.
- Status: open

### src/check/inhabitedness_memo.zig:45: `Memo.put` `dupe`s the `known_empty` var slice as part of the key
- category D; runs per memoized inhabitedness answer; impact low; risk low
- Finding: keys own heap slices; `deinit` frees each.
- Fix: intern `known_empty` sets once (they are sorted canonical root lists: 55-63) into a shared `ArrayListUnmanaged(Var)` and key by span.
- Status: open

### src/base/annotation_positions.zig:273-286: `Solver` allocates `inherited`, `adjacency: []ArrayListUnmanaged(u32)`, `components` per declaration analysis
- category F, A; runs per nominal/alias declaration analyzed (cached by declaration afterwards); impact low (once per declaration); risk low
- Finding: adjacency is an array of per-formal ArrayLists (list-of-lists); Tarjan scratch (344-356) allocates `index_of`, `low_of`, `on_stack`, `component_stack`, `walk_frames` per call.
- Fix: CSR adjacency; Tarjan scratch retained on the solver.
- Status: open

### src/check/annotation_positions.zig:154: `analyze` returns `allocator.alloc(Positions, args.len)` per builtin application, stored into `nominal_positions: AutoHashMapUnmanaged(NominalPositionKey, ?[]Positions)` (Check.zig:722)
- category F, A; runs per annotation application analyzed; impact low; risk low
- Finding: owned heap slice per application cached in a map.
- Fix: spans into a shared positions buffer.
- Status: open

### src/canonicalize/Scope.zig:68-82 + Can.zig:21096: every scope owns eight `AutoHashMapUnmanaged(Ident.Idx, ...)`; a scope is pushed per block, lambda, match branch, for-loop, etc.
- category F, B; runs per lexical scope entered (`scopeEnter` at 931, 3433, 9047, 9281, 9332, 10887 and the type-var variant at six more sites; i.e. per block / lambda / branch / …; impact high (canonicalization's scope operations run per …; risk medium (shadowing/warning semantics …
- Finding: `Scope` = `idents`, `aliases`, `type_bindings`, `type_vars`, `type_var_aliases`, `module_aliases`, `exposed_items`, `imported_modules`: eight hash maps keyed by the dense `Ident.Idx`: plus two scalars. `Scope.init` is allocation-free (maps start empty) but the first `idents.put` in every block allocates a table, and `scopeExit` (21102) `deinit`s all eight. Lookups walk the scope stack (`scopes: ArrayList(Scope)`) …
- Fix: one flat binding stack per namespace (`ArrayListUnmanaged(struct{ident: Ident.Idx, pattern: Pattern.Idx, prev: u32})`) with a `DenseMap(Ident.Idx, u32)` "current binding" index whose entries are restored from the `prev` link on scope exit (the file already has exactly this design for rigid type vars: `TypeVarScopes` at 21000 keeps `active` + `changes` undo log: but `active` is still a hash map; make it a …
- Status: open

### src/canonicalize/Scope.zig:50-53 + Can.zig:2309-2320: `ForwardReference.reference_regions: std.ArrayList(Region)` heap list per forward-referenced name
- category F; runs per forward reference recorded; impact low; risk low
- Finding: each forward reference owns a heap ArrayList of regions; freed at 802/823.
- Fix: flat `forward_reference_regions: ArrayListUnmanaged(Region)` with `next: u32` chain or `(start,len)` span per reference.
- Status: open

### src/canonicalize/Can.zig:14107, 14524, 14573: `scratch_bound_vars.setViewFrom` builds an `AutoHashMapUnmanaged(Pattern.Idx, void)` per lambda and per match branch
- category A, B; runs per lambda (14107) and per match branch (14524/14573) canonicalized: i.e. per function literal and per branch in the module; impact medium; risk low
- Finding: `base.Scratch.SetView.init` allocates a hash set sized to the bound-var run, then it is probed per free variable and destroyed. Bound-var runs are typically 1-5 entries; a linear scan (`containsFrom`, which the same loop already uses for captures) would beat hashing for those sizes, and for large runs a bitset over `nodes.len()` is O(1) per probe without allocation.
- Fix: use `containsFrom` when the run is short (<= 16) and otherwise a reusable `DynamicBitSetUnmanaged`/`DenseMap(Pattern.Idx, void)` on `Self` cleared via the entries list; remove `SetView` if no other caller remains.
- Status: fixed (fix 9)

### src/canonicalize/Can.zig:11355-11380: `runExprKernel` builds an 8 KiB `BufferFirstAllocator` + `std.heap.ArenaAllocator` + `ExprKernelWork` (~60 ArrayLists) per `canonicalizeExpr` call
- category A; runs per `canonicalizeExpr` entry (18 call sites: per top-level def body, per annotation default, per associated item, per expect body, ...); impact medium; risk medium (nesting of kernel entries must …
- Finding: each entry sets up a fresh fallback buffer, a std arena (atomic bump) on top of it, and a `ExprKernelWork` struct whose ~60 `std.ArrayList` stacks all start empty; `child_slots`, `field_work`, `unset_work`, `branch_work` are then grown from zero inside the kernel, and `toOwnedSlice(frame_allocator)` (11758, 11916, 12022) copies each finished list into the arena. When the 8 KiB buffer is exhausted everything spills …
- Fix: keep one `ExprKernelWork` + one `base.SingleThreadArena` on `Self`, reset per entry (`reset(.retain_capacity)`); nested entries (an expression canonicalized from inside a kernel) record base lengths. Replace `toOwnedSlice` copies with `(start,len)` spans into the retained work lists (the consumers only read them before the frame finishes).
- Status: open

### src/canonicalize/Can.zig:12109: `block_state_allocator.create(BlockStateData)` per block expression
- category A; runs per block canonicalized; impact low; risk low
- Finding: a heap box per block in the arena (cheap bump) but it exists only because `ExprKernelLabel` stores pointers; a `u32` index into a `block_states` stack is the same lifetime.
- Fix: index into a retained `ArrayListUnmanaged(BlockStateData)`.
- Status: open

### src/canonicalize/Can.zig:9653-9680: definite-assignment `InitState` boxes `create`d / `clone`d per branch
- category A, F; runs per `if`/`match` branch and per short-circuit operand during uninitialized-`var` analysis; impact low-medium; risk low
- Finding: `cloneState` allocates a box and deep-clones the state (which itself owns a list) for every branch; `settleBranchState` copies the box contents into `normal: ArrayList(InitState)` and frees the box.
- Fix: store states by value in a stack with indices; clone into the stack slot directly (no box), so a branch costs one append.
- Status: open

### src/canonicalize/Can.zig:3425: `gpa.create(AssociatedItemsState)` per associated-items block + `dupe` of alias sinks at 4286
- category A; runs per type declaration with associated items, per nested type; impact low; risk low
- Finding: boxed state because `ExprKernelWork.associated_next/exit` hold `*AssociatedItemsState`; nested alias-sink slices are `dupe`d to the heap.
- Fix: index into a retained `ArrayListUnmanaged(AssociatedItemsState)`; alias sinks as spans into `scratch_assoc_alias_sinks` (already a `base.Scratch`).
- Status: open

### src/canonicalize/Can.zig:16093: interpolation canonicalization `gpa.dupe`s the expression span before normalizing
- category D, A; runs per interpolated string literal; impact low-medium; risk low
- Finding: "The span's backing store grows as we add expressions, so copy it first": a heap dupe + `segments: ArrayList(?Expr.Idx)` per string.
- Fix: copy into `scratch_expr_ids` (a `base.Scratch(Expr.Idx)` on `Self`) with `top()`/`clearFrom()`.
- Status: open

### src/canonicalize/Can.zig:270-368: ~30 `AutoHashMapUnmanaged` keyed by `Ident.Idx`, `Pattern.Idx`, `Statement.Idx`, `AST.Statement.Idx`, `DeclIndex.ScopeIdx`, `AssocValue` on the canonicalizer
- category B; runs per module; `used_patterns` / `globally_resolvable_patterns` / `import_indices` / `active_decl_*` are hit per identifier reference; impact medium; risk low
- Finding: dense ids hashed: `used_patterns`, `globally_resolvable_patterns` (Pattern.Idx -> void; probed per lookup at `isGloballyResolvablePattern`), `active_decl_values/types/import_aliases` (Ident.Idx -> usize), `exposed_idents/types`, `import_indices`, `parser_type_decl_states`, `forward_type_scope_eligibility`, `alias_cycle_*`, `assoc_*`, `type_decl_paths`, `forward_prepared_import_aliases`, `active_decl_scopes`, …
- Fix: bitsets over `nodes.len()` for the pattern sets; `DenseMap(Ident.Idx, ...)` for the ident-keyed ones; `exposed_ident_texts` -> keyed by `Ident.Idx`.
- Status: partly fixed (fix 56): the ten maps keyed by pattern, statement, and scope ids are `DenseMap`s; maps keyed by `Ident.Idx` stay hashed, since the identifier's attribute bits make its raw value sparse

### src/canonicalize/Can.zig:2407-2409: `AliasCycleContext` Tarjan state in three `AutoHashMapUnmanaged(AST.Statement.Idx, ...)`
- category B; runs once per module (alias cycle detection over type decl statements); impact low; risk low
- Finding: index/lowlink/on_stack keyed by dense statement ids through hash maps.
- Fix: three `[]u32`/bitset columns sized to the statement count (the pattern used by `finalizeTypeDeclarationValidity` in Check.zig).
- Status: fixed (fix 56)

### src/canonicalize/Can.zig:535: `SeenLabels.positions: AutoHashMapUnmanaged({run_top, ident}, u32)` for duplicate record-field/tag detection
- category B; runs per record literal field / per tag in a union annotation; `clearFrom` removes entries one by one (hash remove per label); impact low-medium; risk low
- Finding: duplicate-label detection keyed by (run, ident) through a hash map with per-label `remove` on scope exit.
- Fix: `DenseMap(Ident.Idx, u32)` storing the entry index; a label belongs to the current run iff `entries[idx].run_top == run_top`, so no removal is needed on `clearFrom` beyond truncating `entries` (stale map entries are detected by the run check). Or a linear scan for runs < 8 labels (the overwhelmingly common case).
- Status: open

### src/canonicalize/DependencyGraph.zig:27: `DependencyGraph.edges: AutoHashMapUnmanaged(Def.Idx, std.ArrayList(Def.Idx))`
- category F, B; runs once per module; one heap list per def with dependencies; impact medium (once per module, but O(defs) hash maps and per-def …; risk low
- Finding: adjacency as hash-map-of-lists keyed by dense def ids; Tarjan (`TarjanState` 1921-1936) keeps `indices`, `lowlinks`, `visited`, `on_stack` as four more hash maps keyed by `Def.Idx`; `computeCheckOrder` (1743-1812) builds `pattern_to_def`, `def_position`, `def_to_group`, `refs`, `seen_edges: AutoHashMapUnmanaged(u64, void)` and `successors: []std.ArrayList(u32)` (list per group).
- Fix: CSR adjacency (`edge_starts: []u32` over the def list + flat `edges: []Def.Idx`), Tarjan columns as `[]u32`/bitset indexed by the def's position in `nodes` (a `DenseMap(Def.Idx, u32)` position index built once). `successors` -> CSR.
- Status: partly fixed (fix 56): Tarjan state, the pattern-to-def map, and def positions are `DenseMap`s; the adjacency is still a map of lists

### src/canonicalize/DependencyGraph.zig:316, 738: `DemandAnalyzer.resolved_literal_targets: AutoHashMapUnmanaged(Var, ArrayListUnmanaged(Def.Idx))` and `create(DemandSummary)` per lambda
- category F, B, A; runs per lambda summarized (per local function in the module); per literal requirement; impact low-medium; risk low
- Finding: `startSummaryJob` heap-allocates a `DemandSummary` frame per lambda (738) and frees them in `walk.deinit` (626); summaries are stored in `summaries: AutoHashMapUnmanaged(Expr.Idx, DemandSummary)` by value anyway. `resolved_literal_targets` is a list-per-key map keyed by dense Var.
- Fix: frames by value in `walk.frames` (an `ArrayListUnmanaged(DemandSummary)` indexed by position); `resolved_literal_targets` as spans into a flat list keyed via `DenseMap(Var, Span)`.
- Status: open

### src/canonicalize/DependencyGraph.zig:241-250: `DemandSummary` holds four `AutoHashMapUnmanaged` (deps, deps_from, called_patterns, literal_requirements)
- category F, B; runs one summary per lambda; folding (`FoldCursor` 750) iterates the maps; impact low-medium; risk low
- Finding: a summary is four hash maps (each a separate allocation) keyed by dense ids; folding a callee summary into a caller iterates and `put`s each entry again.
- Fix: sorted `ArrayListUnmanaged` spans into shared pools (dedupe by sort+unique at finish), or bitsets over defs/patterns when the module is small enough; folding becomes a merge.
- Status: open

### src/canonicalize/NodeStore.zig:4965-5025: where-clause grouping builds `AutoArrayHashMapUnmanaged(TypeAnno.Idx, ArrayListUnmanaged(WhereClause.Idx))` plus three `AutoHashMapUnmanaged(TypeAnno.Idx, void)` per annotation with where clauses
- category F, B; runs per annotation with a where clause (per constrained function signature); impact low-medium; risk low
- Finding: list-per-key grouping plus `locally_declared`, `reachable`, `dependencies` sets keyed by dense anno ids, all created and destroyed per call.
- Fix: scratch on the store (`NodeStore.scratch` already exists) with `DenseMap`/bitset over `nodes.len()`; group by sorting clause indices by owner.
- Status: open

### src/canonicalize/ModuleEnv.zig:3836: `import_mapping_reserved: [6]u64` reserved for an `AutoHashMap` in the serialized module env
- category C, H; runs per serialized module env; impact low; risk low
- Finding: 48 bytes of reserved space in the serialized struct for a hash map that is rebuilt at runtime; harmless but it means the serialized struct carries dead bytes and the import mapping is rebuilt per load.
- Fix: keep (low); note that the import mapping could be a serialized sorted table.
- Status: no change: reserved serialized bytes; the finding recommends keeping them

### src/canonicalize/ImportResolution.zig:225-250: `outcomes: StringHashMap(Resolution)` keyed by import path text; `scratch`/`path_buf` ArrayLists; `dupe` of relative path (1000)
- category D, B; runs per import resolved (per module x imports); impact low; risk low
- Finding: import outcomes keyed by path string (hashing the full path per lookup) and path text dupe'd per resolution; `receiver_extensions: AutoHashMap(MethodKey, ...)` is structural (fine).
- Fix: key `outcomes` by the interned `Ident.Idx` the parser already produced for the import path (`importModulePathIdent` in Parser.zig interns it) instead of by text.
- Status: open

### src/canonicalize/BuiltinLowLevel.zig:146-204: `LowLevelMap = AutoHashMap(Ident.Idx, LowLevel)` + `name_scratch` rebuilt per builtin module load
- category B; runs once per Builtin module canonicalization; impact low; risk low
- Finding: a map from ident to low-level op built by formatting names into a scratch buffer and interning.
- Fix: low priority; a sorted table by `Ident.Idx` with binary search, or `DenseMap`.
- Status: open

### src/canonicalize/CIR.zig:1010: `std.StringHashMap(u32)` name_to_idx built during import store setup
- category B; runs once per module; impact low; risk low
- Finding: string-keyed map where the names are already interned string literals (`map: AutoHashMapUnmanaged(StringLiteral.Idx, Import.Idx)` at 856 exists alongside).
- Fix: use the `StringLiteral.Idx` keyed map only.
- Status: open

### src/canonicalize/Node.zig:314 + NodeStore.zig:286-313: CIR `Node` is tag + 16-byte extern-union payload in a `SafeMultiList`; typed side tables for larger data
- category C (reviewed, mostly good); runs per CIR node; impact, ; risk, - Finding: `Payload` is asserted to be exactly 16 bytes (test at 1381) and `Node.List = SafeMultiList` splits the 1-byte tag into its own column, so nodes cost 17 bytes with no padding; larger payloads go to typed `SafeList`s (`match_data`, `if_data`, `method_call_data`, ...). `regions: Region.List` (8 bytes/node) is parallel. This is already the SoA layout the brief asks for. One note: `CIR.Expr` (Expression.zig:70) is the …
- Fix: none for the store; for consumers, pass `Expr.Idx` and re-decode on demand rather than caching the decoded union in long-lived frames.
- Status: no change: the finding recommends none for the store

### src/parse/DeclIndex.zig:66-82, 327-341: every parser scope owns three `AutoHashMapUnmanaged(Ident.Idx, NameBucket)`, each bucket with an `overflow: ArrayListUnmanaged(DeclIdx)`; one scope per block
- category F, B; runs per block/lambda/associated scope entered during parsing (`enterScope` from Parser.zig:387); impact medium (the parser creates a scope per block; each costs …; risk medium (forward-reference policy …
- Finding: `scopes.append(.{ .value_decls = .{}, .type_decls = .{}, .import_alias_decls = .{} ... })` plus `scope_decl_builders.append(.empty)` (an `ArrayList(ArrayListUnmanaged(DeclIdx))`: list of lists) per scope; `addDeclToBucket` does `getOrPut` + `bucket.append` (second heap list on the first duplicate name). `deinit` walks every scope's three maps and every bucket's overflow list. Lookup (`decl_by_statement: …
- Fix: a single flat `decls` list sorted by `(scope, namespace, ident)` with per-scope spans (`scope_decl_ids` already exists as the flattened form built by `exitScope`), so name lookup is a binary search within the scope's span, no per-scope maps; `decl_by_statement`/`type_path_by_statement` -> `[]u32` columns over statement node count.
- Status: open

### src/parse/Parser.zig:50-75: `BracketMatches` allocates three `u32` columns over all tokens + three open stacks per parse
- category A, C; runs once per file; impact low; risk low
- Finding: 12 bytes per token of bracket-match data (`any`, `curly`, `round`) memset and filled in one pass; the three open-bracket stacks are separate ArrayLists.
- Fix: one `[]u32` match column (any bracket) plus a 2-bit kind derived from the token tag at use time, when `curly`/`round` are only used to skip to a matching bracket of that kind (verify usages); saves 8 bytes/token.
- Status: open

### src/parse/Parser.zig:781-830: `importModulePathIdent` / `nestedImportPathIdent` build the path in a fresh `ArrayList(u8)` per import
- category A; runs per import statement; impact low; risk low
- Finding: fresh heap buffer per import to assemble the path text before interning.
- Fix: reuse `self.store.scratch_bytes`-style buffer or `CommonEnv` string builder.
- Status: fixed (fix 54)

### src/parse/Parser.zig:692: `recordDestructuredValueNames` allocates a `pending` ArrayList per destructuring declaration
- category A; runs per destructure pattern declaration; impact low; risk low
- Finding: fresh worklist per call.
- Fix: `self.scratch_nodes` or a retained pattern stack.
- Status: fixed (fix 54)

### src/parse/Parser.zig:2839-2892: `OpenSyntaxStack` holds ~55 separate `std.ArrayList` payload stacks
- category H; runs per parse (stacks are reused within a file); impact low; risk, - Finding: one ArrayList per state kind means ~55 separate allocations that grow independently; the pattern is sound (typed stacks avoid a giant union) and capacity is retained for the file, so the cost is per-file allocations, not per-node. No change recommended beyond `deinit` cost.
- Status: no change: the finding recommends none; typed stacks retain capacity for the file

### src/parse/NodeStore.zig:3909: `collectRedundantReturns` allocates a `DynamicBitSet` over all nodes + a pending list per parse
- category A; runs once per file; impact low; risk, - Finding: fine (one bitset per file); noted for completeness.
- Status: no change: one bitset per file

### src/parse/tokenize.zig:22-41: `Token` = tag(1) + region(8) + extra(4) in a `MultiArrayList`
- category C (reviewed, good)
- Finding: columns are split, so no padding is paid per token; `extra` is an untagged union over u32-sized members. Nothing to fix.
- Status: no change: the columns are split, so no padding is paid

### src/parse/Node.zig:9-14: parse `Node` = tag + data(8) + main_token(4) + region(8) + collection_layout(1) in `SafeMultiList`
- category C (reviewed, good)
- Finding: SoA via SafeMultiList, so the 1-byte fields do not pad the 8-byte ones. Nothing to fix.
- Status: no change: stored as a struct of arrays

### src/parse/NumericLiteral.zig:209-210, 391, 517-603: `allocator.alloc(u8, 0)` for empty digit slices
- category A; runs per integer literal without a fractional part (the `after` slice is empty for every integer literal); impact low; risk low
- Finding: zero-length allocations through the allocator for `before`/`after` digit slices; Zig allocators return a valid empty slice without touching the heap for len 0, but the call still goes through the allocator vtable; freeing them later also does. Also `bytes_le`/`limbs_le` ArrayLists per big-literal conversion.
- Fix: use `&.{}` (an empty constant slice) for empty digit runs and skip the `free` when len == 0 (or give the owner a flag).
- Status: fixed (fix 54)

### src/base/Scratch.zig:55-83: `Scratch.SetView` builds an `AutoHashMapUnmanaged(T, void)` on demand
- category B, A; runs per call site (Can.zig lambda/branch capture analysis; see the Can finding); impact medium (via its callers); risk low
- Finding: a hash set built from a scratch range per query.
- Fix: see Can.zig:14107 finding; remove `SetView` once unused.
- Status: fixed (fix 9)

### src/base/Ident.zig:173-179: `Ident.Store.attributes: SafeList(Attributes)` is a parallel 1-byte column duplicating bits already packed in `Ident.Idx`
- category C, H; runs per ident inserted; read by `lookup` to reconstruct an `Idx` from text; impact low (1 byte/ident) but removes a serialized column; risk medium (the `reserved` bit is not …
- Finding: `Idx` packs `attributes` (3 bits) with the 29-bit interner index, and the store also keeps `attributes` per entry so that `lookup(text)` can produce the packed `Idx`. The attributes are a pure function of the text (`Attributes.fromString`: ends with `!`, starts with `_`), so the column is derivable from the interned bytes at lookup time.
- Fix: compute `Attributes.fromString(text)` in `lookup` and drop the column (and its serialization). Verify no caller inserts an ident with attributes that differ from its text (`insert` takes `ident.attributes` as given: check `Ident.for_text` always derives them and nothing overrides `reserved`).
- Status: open

### src/base/TextRankCache.zig:56-80: rank rebuild allocates `entries` + `scratch` (16 bytes each per ident) per rebuild
- category A; runs per ident-store growth followed by a rank query (lazy rebuild; by design); impact low (unless interleaved); risk medium
- Finding: two `Entry{index,text}` arrays over the whole ident set plus a `runs` list per rebuild; fine as a once-per-growth cost. Noted: the rebuild is O(n) in idents each time the interner grows and a rank is requested, so interleaved insert/rank patterns (e.g. unify sorting fields by rank while canonicalizing derived code inserts idents) degrade to O(n) per insert.
- Fix: incremental rank assignment with gaps (rank = position x stride, rebalanced only when a gap closes) if interleaving is observed; otherwise none.
- Status: open

### src/base/annotation_positions.zig:65-71, 96-101: `Solver` allocates `positions`/`used` per declaration and keeps `by_declaration: AutoHashMapUnmanaged(Adapter.Key, usize)`
- category A, B; runs per declaration analyzed (cached afterwards); impact low; risk, - Finding: structural key map (owner pointer + statement): legitimate; per-declaration arrays: fine.
- Status: open

### src/base/SerialStringInterner.zig:126-132: deserialization `dupe`s bytes and ranges
- category D; runs per module env load from cache; impact low; risk medium (lifetime)
- Finding: copies interned bytes out of the mapped buffer rather than viewing them (contrast `SmallStringInterner`'s zero-copy `deserializeInto`).
- Fix: zero-copy view like the other interners when the buffer outlives the env.
- Status: open

### src/base/SmallStringInterner.zig / InternedBytes.zig: reviewed; open-addressed index with fingerprints, SafeList-backed
- category: (good)
- Finding: no per-insert allocations beyond the byte append and amortized table growth.
- Status: open

### src/collections/DenseMap.zig:55: `active_indices: ArrayList(usize)` where keys are u32-derived
- category C; runs every DenseMap entry (used throughout the checker); impact low-medium (DenseMap is the recommended replacement for …; risk low
- Finding: dense positions are stored as `usize` (8 bytes) although `keyIndex` produces a `u32`-range value; halving to `u32` keeps the 32-bit floor the invariant requires and halves the live-index column.
- Fix: `ArrayList(u32)` for `active_indices` and `u32` positions in sparse chunks (check `SparseChunk` element type).
- Status: open

### src/collections/VersionedMap.zig:188: `VersionedHashMap` variant hashes keys (`AutoHashMap(K, usize)`), `Store` boxed with `create`
- category B; runs per versioned memo (used by check/canonical_type_keys and ModuleEnv); impact low; risk low
- Finding: the `VersionedDenseMap` variant exists; callers that use the hash variant with dense keys should switch. The boxed `Store` is a single allocation per map (fine).
- Fix: audit the two users (ModuleEnv.zig, canonical_type_keys.zig) for dense keys.
- Status: open

### src/collections/SortedArrayBuilder.zig:121, 136: string keys `dupe`d per entry
- category D; runs per exposed item / per entry when `K == []const u8`; impact low; risk low
- Finding: owns a copy of every key string.
- Fix: intern keys (`Ident.Idx`/`StringLiteral.Idx`) before building; the builder then needs no ownership.
- Status: open

### src/collections/GuardedList.zig:200: `dupeSpan` returns a heap copy of a span
- category D; runs depends on callers (post-check code, not in this audit's directories); impact low; risk, - Finding: helper that copies out of a guarded list; a borrowed view with generation check would avoid the copy where the list is not mutated during the borrow.
- Fix: caller-side; note only.
- Status: open

### src/collections/safe_list.zig / CompactWriter.zig / ScopedBitSet.zig / IndexedStack.zig: reviewed
- category: (good)
- Finding: SafeList/SafeMultiList are thin wrappers over `ArrayList`/`MultiArrayList` with amortized growth; `deserializeWithCopy` allocates full capacity (`item_capacity`) rather than `len` (safe_list.zig:222): a minor over-allocation when the serialized capacity exceeds length (design.md says serialization writes only live rows, so capacity == len in practice).
- Status: open

### src/layout/store.zig:129-130, 396-402: `interned_layouts: StringHashMap(Idx)` with a `dupe`'d key per interned layout
- category D, B; runs per distinct non-scalar layout inserted (`insertLayout`, `internStructShape`, `putTagUnion`, ...); lookups per `insertLayout` call hash the serialized key; impact low-medium; risk low
- Finding: the intern key is built into `scratch_intern_key`, hashed for lookup, and on miss `dupe`d to the heap and stored as the map key; the layout's own columns (`layouts`, `struct_fields`, `tag_union_variants`) already contain everything the key encodes.
- Fix: a hash map whose context hashes/compares the layout *by index* (reading the store's columns) with the scratch key compared against the candidate's rebuilt key, or store the key bytes in one shared `key_bytes` buffer keyed by `(offset,len)`; eliminates the per-layout `dupe` and `blob_index_keys`-style ownership.
- Status: fixed (fix 47)

### src/layout/store.zig:662-690, 710-720, 781: `putStructFields`/`putTuple`/`putNominalStructFields` copy fields into a fresh `ArrayList(StructField)` per call to sort them
- category A; runs per record/tuple layout computed (per distinct record type reaching layout, plus every re-query that misses the intern cache); impact low-medium; risk low
- Finding: `temp_fields` is allocated, `appendSlice`'d, sorted with a comparison sort that calls `getLayout(field.layout).sortKey()` per comparison, then freed.
- Fix: a retained `scratch_fields: ArrayListUnmanaged(StructField)` on the store; precompute each field's `SortKey` once into a parallel scratch before sorting (keys are recomputed O(n log n) times now).
- Status: fixed (fix 47)

### src/layout/store.zig:1453-1460, 1603-1610: `commitGraph` with a `.canonical` root allocates two zero-length slices
- category A; runs per graph commit whose root is already canonical (the cache-hit path of every LIR layout request: postcheck/solved_lir_lower.zig:13917, boxy/layouts.zig:383); impact low; risk low
- Finding: `raw_layouts = try self.allocator.alloc(Idx, 0)` x2 so that `GraphCommit.deinit` can `free` unconditionally; these go through the allocator on the hot cache-hit path.
- Fix: return `&.{}` and have `deinit` skip zero-length frees (or make the slices optional).
- Status: fixed (fix 47)

### src/layout/store.zig:1603-1640, 2100-2160: `commitGraphUncached` allocates ~14 whole-graph columns + CSR parents + pending list per commit
- category A; runs per uncached layout graph commit (per distinct recursive/compound type in LIR lowering); impact medium for programs with many distinct compound types; risk low
- Finding: `raw_layouts`, `value_layouts` (returned), `resolved`, `recursive_nodes`, `component_ids`, `visit_index`, `lowlink`, `on_stack`, `tarjan_stack`, `parent_starts`, `parents`, `parent_fill`, `pending`, `finalize_state`, `raw_used`: ~13 scratch allocations freed at return, each sized to the graph's node count; plus per-node `fields`/`variants` ArrayLists in `tryResolveNode` (2036, 2065) and `RecursiveGraphAnalysis` …
- Fix: a `CommitScratch` struct retained on the store (cleared per commit) holding all of these as `ArrayListUnmanaged`s; the per-node `fields`/`variants` lists become spans into scratch.
- Status: open

### src/layout/work.zig:46-52: `in_progress_vars: AutoArrayHashMap(ModuleVarKey, void)` / `in_progress_nominals: AutoArrayHashMap(NominalKey, ...)`
- category B; runs per type var visited during layout computation; impact low-medium; risk low
- Finding: `ModuleVarKey = {module_idx: u32, var_: Var}`: a (module, dense var) pair hashed per visit; the layout work is per-module so a `DenseMap(Var, void)` per module (or an epoch column) would avoid hashing. `NominalKey` is structural (fine).
- Fix: `DenseMap(Var, void)` keyed by var with the module index as a separate small map (or stamp) since nearly all vars come from the current module.
- Status: open

### src/layout/digest.zig:84, 206: per-digest `pending` ArrayList; `DigestCache` boxed with `create`
- category A; runs per `contentDigest` call (per layout digest request); impact low; risk low
- Finding: a fresh worklist per digest computation; the cache box is once per store (fine).
- Fix: retain the worklist on the `DigestCache`.
- Status: open

### src/layout/abi/x86_64.zig:169, aarch64.zig:126/179, abi/call.zig:803: `std.ArrayList` worklists per classification
- category A; runs per call-ABI classification (per function signature lowered; results cached by callers); impact low; risk low
- Finding: fresh `pending` lists per classification walk; the rest of `abi/call.zig` correctly allocates results in a caller-provided arena.
- Fix: scratch on the classifier or use the caller's arena.
- Status: open

### src/layout/layout.zig:683: `Layout` is a packed struct (data + 4-bit tag)
- category C (reviewed, good); impact, ; risk, - Finding: compact packed representation; `Idx` is `layout_bit_size - 4` bits wide (verify it is >= 32 bits per the representation-capacity rule: `@Int(.unsigned, layout_bit_size - @bitSizeOf(LayoutTag))`: if `layout_bit_size` is 32 this makes `Idx` 28 bits, which would violate the "at least 32 bits for program-size-dependent indices" invariant; flagging for the owner to confirm `layout_bit_size`).
- Status: open


## Checker: deep pass over Check.zig
### src/check/Check.zig:24900,24915 + 9379-9470: `expectedTypeBackup` deep-copies the expected type for EVERY expression frame that has an expected type or annotation (deepens first-pass 9371/9452)
- category A, D, E; runs per expression frame whose `Expected` carries `annotation` or `expected_type`: every call argument (`fn_call_arg` expected), every annotated binding RHS, …; impact high (two full type walks + a structural copy + 2-4 heap …; risk medium for (2) (error-path semantics …
- Finding: `expectedTypeBackup` first runs `groundTypeHash` (a full walk of the expected type with a fresh `std.ArrayListUnmanaged(Var)` + fresh `std.AutoHashMap(Var,u32)` per call, 9393-9397), and then, unless the type is fully ground AND already cached, runs `instantiateVarOrphan` (9339), i.e. a complete structural copy of the expected type into fresh vars, with the Instantiator's own `var_map` clear and walk. The copy …
- Fix: (1) make `groundTypeHash`/`groundTypesIdentical` use checker-owned scratch (`self.var_map` DenseMap for `seen`, `self.type_visit_stack` for `pending`): no per-call allocation; (2) avoid the eager orphan copy when it is provably unnecessary: if `!self.types.mayContainErrorState()` is true at frame begin AND remains true at frame finish, the backup is never read (the only consumer is the `varContainsError` branch, …
- Status: partly fixed (fix 23: the ground hash and identity walks use checker scratch; the eager orphan copy remains)

### src/check/Check.zig:24840 + 29889-29911: `retired_operand_sequences.remove(expr_idx)` hash removal on every frame begin
- category B, E; runs per expression frame (`beginExprCheckFrame` 24840) and again per erroneous finishing frame (29899); impact medium (one hash probe per expression node; the map is …; risk low
- Finding: `retired_operand_sequences: AutoHashMapUnmanaged(CIR.Expr.Idx, RetiredOperandSequence)` (706) is a sparse map populated only for erroneous call-like expressions, yet every frame begin pays a hash + probe + possible tombstone write to remove a key that is almost never present. The neighbouring `call_operand_type_error_exprs` is already a dense `[]bool` column over node slots (24839): the removal exists only to keep …
- Fix: a `DenseMap(CIR.Expr.Idx, RetiredOperandSequence)` (O(1) remove without hashing, live-entry iteration) or, better, guard the remove with the dense column: `if (self.call_operand_type_error_exprs.items[slot]) _ = remove(...)` is not sufficient (the map may hold an entry while the column is false after a re-check), so instead keep a `has_retired_operand_sequence` bit in the same dense column (make the column a …
- Status: fixed (fix 23)

### src/check/Check.zig:33732-33860: `recordRelationOperands` copies every call/list/if/match operand span into `relation_operand_pool` and stores a `(start,len)` under a hashed `relation_operands` map (deepens first-pass 130-1352)
- category B, D; runs per non-leaf expression of a relation-owning kind: every call, method call, binop, list literal, if, match, record update, field access, interpolation, string …; impact medium (an O(args) copy and a hash insert per call-like …; risk low
- Finding: For `.e_list` / `.e_call` the function `appendSlice`s the CIR span (`sliceExpr(list.elems)`, `sliceExpr(call.args)`) into `relation_operand_pool`, duplicating a span the CIR node store already holds contiguously; for the other kinds it first appends to `retired_operand_pool` through `appendEvaluationOperands`, then copies that run into `relation_operand_pool` (33830-33834: two copies). The result is keyed in …
- Fix: store for list/call kinds the CIR `Expr.Span` itself (start+len into the node store's extra data) rather than copying; for the composed kinds append directly into `relation_operand_pool` (make `appendEvaluationOperands` take the target pool as a parameter) and drop the intermediate `retired_operand_pool` copy. Replace the map with a `DenseMap(CIR.Expr.Idx, RelationOperands)` or a column over `nodes.len()` with a …
- Status: open

### src/check/Check.zig:25149-25159: `finishExprFrame` heap-allocates a one-element `BoundaryRoot` slice per generalization boundary
- category A; runs once per group boundary reached through an expression frame (every singleton def RHS that generalizes: every top-level function, every annotated value in a …; impact low-medium (one alloc/free per generalizing def); risk low
- Finding: `const roots = try self.gpa.alloc(BoundaryRoot, 1); ... .boundary = .{ .roots = roots, .owns_roots = true }` creates a 1-element heap slice so `BoundaryActivity` can own it; the sibling path (`GroupActivity`) passes borrowed slices. The frame is also copied by value into `state.finishing` (an `ExprCheckFrame` of ~200+ bytes) and then read back through `state.finishing.?.frame.*` three times.
- Fix: give `BoundaryActivity` an inline `single: BoundaryRoot` + `roots: []const BoundaryRoot` pointing at it (or a `union { inline: BoundaryRoot, slice: []const BoundaryRoot }`) so a singleton root never allocates; build the root from the local `frame` before the copy.
- Status: fixed (fix 40)

### src/check/Check.zig:24687-24691, 24662, 24706: `varContainsError` walks the finished expression's type up to three times per frame
- category E; runs per finishing expression frame in any module whose type store `mayContainErrorState()` (i.e. any module with at least one type error or invalid declaration: …; impact medium for LSP / erroring builds, zero for clean builds; risk low
- Finding: `finishBeforeBoundary` calls `varContainsError(self.expr_var)` once before the annotation unification (24662), again after it (24679), and (for unannotated frames) a third variant at 24706. Each is a full DFS over the expression's resolved type with `var_set.clearRetainingCapacity()`; the second walk repeats the first unless the unification introduced an error. For error-free stores the early return at 54814 makes …
- Fix: cache the first walk's answer and re-walk after unification only when the unify result is not `.ok` (an accepted unification between two error-free types cannot introduce `.err` content); for the unannotated path reuse the same cached answer.
- Status: open

### src/check/Check.zig:24716: `checkStaticDispatchConstraints(env, false)` runs a full deferred-queue drain on every finishing expression frame
- category E; runs per finishing expression frame (every node in the module); impact high (this is the dominant per-expression fixed cost after …; risk medium ((2) and (3) touch ordering …
- Finding: Each finished frame enters `resumeStaticDispatchDrain` (44679), which sets/clears ~6 fields, calls `syncCleanDeferredPrefix`, `enterRelation(.none)` / `leaveRelation` (twice: see next finding), creates a `use_positions` map (see below), and, when any entry past the clean prefix exists, copies each retained entry into `scratch_deferred_static_dispatch_constraints` and back into …
- Fix: (1) early-out in `checkStaticDispatchConstraints` when `env.deferred_static_dispatch_constraints.items.items.len == env.clean_len` (everything is clean) and `scratch` is empty: skip the drain setup and the double `enterRelation`; (2) make the retained-entry handling in place: mark entries retained in the existing list and compact once (`write` cursor) instead of copy-out/copy-back; (3) let the clean prefix be a …
- Status: partly fixed (fix 24: a drain with nothing past the clean prefix returns before any setup)

### src/check/Check.zig:44787-44789: `leaveRelation(drain_relation)` is called twice in a row
- category H; runs per drain (per finishing expression frame); impact low; risk none
- Finding: `try self.leaveRelation(drain_relation);` appears on two consecutive lines after the drain loop; the second call repeats `claimRecordedProblems` and the owner store. Harmless but pure waste on a per-node path.
- Fix: delete the duplicate line.
- Status: fixed (fix 24)

### src/check/Check.zig:44737-44745: `resumeStaticDispatchDrain` builds a `use_positions: AutoHashMapUnmanaged(u32, ArrayListUnmanaged(usize))` per drain
- category F, B, A; runs per drain (per finishing expression frame); populated whenever a use settles (per scheme use whose relations are processed); impact medium in modules with many scheme uses; risk low
- Finding: a hash map keyed by dense use index whose values are heap lists, created and torn down (iterating all values to deinit) per drain. `indexDeferredEntryUses` (43440) appends positions per constraint of each entry past `indexed_to`.
- Fix: keep one `DenseMap(u32, Span)`-style structure on `Self`: positions appended to a shared `ArrayListUnmanaged(usize)` with per-use intrusive `next` chains (`use_position_heads: DenseMap(u32, u32)` + `use_position_links: ArrayListUnmanaged({position, next})`), cleared per drain by truncation; positions are consumed in insertion order so a singly linked chain in reverse plus a cursor is enough, or a two-pass counting …
- Status: open

### src/check/Check.zig:44481-44505 + 37549-37581: `retireResolvedTypeSchemeRequirements` rescans EVERY type scheme and EVERY requirement for every settled deferred relation
- category E; runs per settled deferred dispatch relation (`recordSettledDeferredDispatchRelation` 44504 calls it unconditionally) and again at the end of every drain that …; impact high for modules with many generalized definitions carrying …; risk medium (requirement retirement timing …
- Finding: the function walks `self.type_schemes` (all live schemes in the module, each with its `dispatch_requirements` list) and, per requirement, does `settled_static_dispatch_constraint_fns.contains` (hash) + `staticDispatchConstraintIsInactive` (resolve + content inspection), compacting in place and `swapRemove`-ing emptied schemes (which rewrites `type_scheme_by_var` for every indexed var of the moved scheme). With S …
- Fix: index requirements by resolved `constraint.fn_var` root (`DenseMap(Var, {scheme_idx, req_idx})` maintained on insert) and retire exactly the requirement(s) whose callable was just settled; keep the full sweep only once per boundary (where `staticDispatchConstraintIsInactive` can change) rather than per relation. Verify ordering: `removeTypeSchemeAt` uses swapRemove so scheme order is already not semantically …
- Status: no change: one sample on roc-pg, and retiring by index or once per boundary changes when requirements retire, which dispatch observes

### src/check/Check.zig:18962-18970 + 18939-18955: `waitingObligationUnchanged` recomputes a 7-field context per retained waiting entry per drain
- category E; runs per deferred entry that waits on an unchecked target def, on every drain that reaches it (every expression finish while the entry is past the clean prefix); impact medium; risk medium (correctness depends on the …
- Finding: `waitingObligationContext` performs `topLevelPattern(getDef(target).pattern)`, `activeDeferredDispatchObligationOwnerFrame`, `currentFrameAdoptsDeferredDispatchObligation`, `dispatchTargetFrameNestedInsideObligationOwner` (which may scan the group stack) and a `waiting_obligations.get` hash lookup, then `std.meta.eql`. The `waiting_epoch == waiting_context_epoch` short-circuit at 44828 avoids this only when the …
- Fix: have the trigger log (`waiting_trigger_log`, already used by the clean prefix) be the sole invalidation source: an entry whose triggers did not fire since its last context computation is unchanged by construction, so skip the recomputation; keep the epoch only for numeric-default passes.
- Status: open

### src/check/Check.zig:24852-24858: `getPattern(pattern_idx) == .underscore` re-fetch per binding-RHS frame
- category E; runs per binding RHS expression frame; impact low; risk none
- Finding: trivial but on the hot frame path: the pattern node is fetched from the store a second time (the def/statement checker that set `checking_binding_rhs_pattern` already had the pattern in hand). Also `isFunctionDef(&checker.cir.store, checker.cir.store.getExpr(self.expr_idx))` at 24683 re-fetches the expression the frame already holds in `self.expr`.
- Fix: pass an `is_underscore_binding` flag with the pattern; use `self.expr` at 24683.
- Status: open

### src/check/Check.zig:3114-3130: `HoistFrameGuard` carries a redundant `checker: *Self` pointer in every `ExprCheckFrame`
- category C; runs one per expression frame on the task stack; impact low-medium (part of the ExprTask size problem the first …; risk low
- Finding: `HoistFrameGuard = { checker: *Self, expr: CIR.Expr.Idx, active: bool }` lives inside `ExprCheckFrame.hoist_frame: ?HoistFrameGuard` (16 bytes + optional tag -> 24 bytes) while the frame already has `checker`. The `expr` is also the frame's own `expr_idx`. The guard's only state is the `active` bit.
- Fix: replace with a single `hoist_frame_active: bool` on the frame and call `checker.finishHoistFrame(self.expr_idx, ...)`/`abortHoistFrame(self.expr_idx)` directly; saves ~24 bytes per task-stack element and removes an optional unwrap on every finish.
- Status: open

### src/check/Check.zig:4699-4760: `finishHoistFrame` re-fetches the expression and re-resolves its type var three times per expression
- category E; runs per expression frame finish (every node); impact medium (fixed per-node overhead); risk low
- Finding: `exprCanBeHoistedRoot(expr)` (6364) and `exprCanCoverHoistedChildren(expr)` (6436) each do `hoistExprInvalidated` (hash `contains` on `hoist_invalidated_exprs`), `varIsFunctionType(varFrom(expr))` (a `resolveVar` + content switch), and `self.cir.store.getExpr(expr)` (node decode); the binding-RHS branch at 4719-4722 does a further `isFunctionDef(getExpr(expr))` + `varIsFunctionType(...)`. So three node decodes, …
- Fix: compute `invalidated`, `is_function_type`, and the decoded expr once at the top of `finishHoistFrame` and pass them to both predicates (make them take the decoded `CIR.Expr` + the two bools); the `ExprCheckFrame` already holds the decoded `expr`, so `HoistFrameGuard.finish` can pass it down.
- Status: open

### src/check/Check.zig:2595-2615 + 4752-4778: `HoistSelectionTransaction` allocates three hash maps + two lists per flushing hoist frame
- category A, B, F; runs per hoist frame that flushes child candidates or deferred roots: every outermost frame (`frame_index == 0`) with candidates, and every ineligible (effectful / …; impact medium (allocation bursts on every effectful call site in a …; risk low
- Finding: `HoistSelectionTransaction.init` starts `staged_roots`, `known_updates` lists and `staged_exprs: AutoHashMapUnmanaged(CIR.Expr.Idx,u32)`, `staged_bindings: AutoHashMapUnmanaged(CIR.Pattern.Idx,u32)`, `staged_pattern_validations: AutoHashMapUnmanaged(CIR.Pattern.Idx,u32)` from empty and `deinit`s all five at the end of the flush. The keys are dense node ids. Inside, `runRoot` (2792) and `stageBindingRoot` (2777) each …
- Fix: keep one transaction scratch on `Self` (lists cleared by truncation; the three maps as `DenseMap`s or as epoch-stamped columns over `nodes.len()`), and a reusable `StageRun` stack; `HoistedDependencyContext` contents should be spans into shared buffers.
- Status: open

### src/check/Check.zig:5590-5598: `retireHoistBinders` allocates a fresh `std.ArrayList(Pattern.Idx)` per retired statement
- category A; runs per retired statement (statements following an erroneous one / rejected destructures): error path, but the LSP sits on it; impact low; risk low
- Finding: a pending stack is created and freed per call; `hoist_retired_binders` is an `AutoHashMapUnmanaged(Pattern.Idx, void)`.
- Fix: `self.scratch_patterns`-style reusable stack (the checker already has `pattern_visit_stack`-like scratch lists); bitset over `nodes.len()` for the set.
- Status: open

### src/check/Check.zig:4897-4908: `combineHoistPromotionDependencies` appends a `both` node per (parent, child) eligible pair
- category H; runs per eligible child frame finish whose parent has a different dependency; impact low; risk low
- Finding: every combination mints a new `HoistPromotionDependency` entry even when the same `(a,b)` pair was already combined (common when many siblings share one procedure dependency); the table only grows for the module.
- Fix: memoize `(a,b)` -> id in a small `DenseMap`/last-pair cache; or make dependencies a sorted small-set so identical sets dedupe.
- Status: open

### src/check/Check.zig:27568-27579: `ForLoopCheck` embeds a full `Expected` (plus Region, Node.Idx, 3 node ids) and sizes every `ExprTaskState`
- category C; runs one `ExprTaskState` per suspended expression (every non-leaf node on the task stack); the union is sized by its largest variant; impact medium (shrinks every task-stack push/pop and the …; risk low
- Finding: `ForLoopCheck` holds `expected: Expected` (~112 bytes: two `?ExpectedType`, each `Var` + `problem.Context` (~32 bytes, sized by `MatchAltBinderContext`'s 7 u32s) + `?Ident.Idx` + bool, plus `?Var`, two enums), `loop_region: Region` (8), `loop_expr: ?IteratorLoopExpr`, `loop_node`, `pattern`, `iterable`, `body`. The task's frame already carries `nested_expected`, from which `startForLoop` only derives …
- Fix: (1) drop `ForLoopCheck.expected`, read `task.frame.nested_expected` at resume; (2) store `loop_expr`/`loop_region` lazily (they are re-derivable from the CIR node); (3) replace `LambdaCheck.anno_context: problem.Context` by the 1-byte `AnnoVars.context` tag it is built from (24893: `.context = .type_annotation` or `expected_type.context`), i.e. keep a `?CIR.Annotation.Idx`/flag and rebuild the context on demand; (4) …
- Status: open

### src/check/Check.zig:27914 + 28066 + 28692 + 28762 + 28824: `LambdaCheck.arg_vars` / `MethodCallCheck.arg_vars` are `gpa.alloc`'d per lambda and per method call
- category A; runs per lambda expression (27914: `state.arg_vars = try self.gpa.alloc(Var, arg_count)`, freed at 28066 in `releaseLambdaCheck`) and per method call (28692, freed …; impact medium (two malloc/free pairs removed per lambda and per …; risk low
- Finding: a heap slice of 0-8 vars per node, with a matching `free`. The lifetime is strictly nested in the task (allocated when the task starts its args phase, released when the task finishes), exactly the discipline `AggregateCheck` already uses with `scratch_top` into `self.scratch_vars` (26196-26205). Zero-length lambdas (`|| ...`) still call `alloc(Var, 0)`.
- Fix: record `arg_vars_top: u32 = self.scratch_vars.top()` and append the vars to `self.scratch_vars`; slice `sliceFromStart(top)` at use and `clearFrom(top)` on release/abort. Verify no nested child check reads `scratch_vars` across the suspension in a way that assumes its own base (children use `top()`/`clearFrom` so nesting is already LIFO-safe; `abortAggregateCheck` shows the abort protocol).
- Status: open

### src/check/Check.zig:28714-28731 + 28700: `resumeMethodCallCheck` runs `checkStaticDispatchConstraints` eagerly per method call with a known receiver, and `methodCallArgContext` is built twice per argument
- category E; runs per `e_method_call` whose receiver type is already known (`varResolvesToKnownType`), i.e. most method calls in annotated code; plus per argument; impact medium; risk low-medium (the eager drain's …
- Finding: after `mkMethodCallConstraint` and `replaceExprWithDispatchCall`, the task runs a full deferred-queue drain (28729) to resolve the method before the arguments are checked, on top of the drain every finishing frame already runs (24716). `methodCallArgContext(...)` (a `problem.Context` construction) is computed once when requesting the child (28746) and again when relating it (28773).
- Fix: drain only the entry just enqueued (`checkStaticDispatchConstraintsFrom(env, false, start)` with `start` = the queue length before `mkMethodCallConstraint`); the early-out proposed for the frame-finish drain also applies. Build the context once per argument and keep it in the task (or recompute only on the error path, since `unifyOwnedRelation` needs it only to report).
- Status: open

### src/check/Check.zig:28313-28360: call argument relation scans operands twice and re-decodes each argument expression
- category E; runs per call expression (`resumeCallCheck` finish), per argument; impact medium (calls are the most common non-leaf node); risk low
- Finding: `callLikeOperandsContainErroneousValue(call_arg_expr_idxs)` (28314) walks the args (column read + `getExpr` decode per arg + `valueIsErroneous` with 2-3 hash lookups per `e_lookup_local` arg), and `retireCallLikeExprWithErroneousOperands` (28336) walks them again with the same function. Inside the relation loop, `exprIsFreshRecordConstruction(call_arg_idx)` decodes each argument node a third time. …
- Fix: compute `operands_erroneous` once and pass it to the retire helper; cache the `getExpr` decode of each argument in the loop (or make `exprIsFreshRecordConstruction` take the node tag read directly from `nodes.items(.tag)[slot]`, avoiding the full decode); defer `getExprPatternIdent` to the diagnostic path by storing `call.func` in the contexts instead of the ident.
- Status: open

### src/check/Check.zig:34111-34129 + 34094-34100 + 25477-25480: `valueIsErroneous` / `binderIsErroneous` do 2-3 dense-id hash probes per local-name operand and per lookup leaf (deepens first-pass 130-1352)
- category B; runs per `e_lookup_local` leaf (25477: `rejected_value_annotations.contains` + `binderIsErroneous` → `erroneous_value_patterns.contains` + …; impact medium-high (several hash probes per name use, in …; risk low
- Finding: four `AutoHashMapUnmanaged` keyed by `CIR.Pattern.Idx`/`CIR.Expr.Idx` (686, 768, 776, 799) are probed on every name use. `erroneous_value_exprs`/`erroneous_value_patterns`/`rejected_value_annotations` are grow-only sets over node ids; `binder_source_exprs` is a sparse node->node map populated per binding.
- Fix: the three sets become bits in one `u8` flag column over `nodes.len()` (the checker already allocates `call_operand_type_error_exprs: []bool` and `top_level_ptrns`/`def_group`/`predeclared_scheme_vars` columns over node slots: extend that column family); `binder_source_exprs` becomes a `u32` column with a sentinel. Iteration sites of these maps (`withdrawSupersededLiteralDefaultWarnings`, `poisonErroneousValueUses`, …
- Status: open

### src/check/Check.zig:25462-25474 + 50887-51006: every local lookup of a top-level name decodes the target def and its expression three times to classify it
- category E; runs per `e_lookup_local` leaf that names a top-level def (every reference to a top-level function or constant); impact medium (3-4 node decodes per top-level name reference); risk low
- Finding: `localLookupDerivedMethod` (50887) does `getDef` + `getExpr` to test `.e_derived_method`; `localLookupHasNoValue` (50997 -> `annotationOnlyValueDef` 30394) does `getDef` + `getExpr` again to test `.e_anno_only`; `localLookupIsNonEffectfulHostedDeclaration` (51002 -> 30410) does `getDef` + `getExpr` a third time to test `.e_hosted_lambda`. Then 25490 does `getDef` a fourth time (`referenced_def`). The classification …
- Fix: add a `kind: enum(u8) { ordinary, derived_method, anno_only, hosted_not_effectful, hosted }` to `DefProcessed` (already a column over pattern slots, 1368) computed once in `setTopLevelPattern`/`setupCheckOrder`; the lookup leaf then switches on it with zero decodes.
- Status: open

### src/check/Check.zig:25785-25796 + 32023: `varIsGround(pat_var)` walks the whole type of a processed top-level value on EVERY use of it
- category E; runs per `e_lookup_local` leaf whose target is a processed, non-generalized top-level def (every use of a top-level constant: strings, numbers, records, lists, …; impact medium (uses of constants are extremely common; the walk is …; risk low-medium
- Finding: `checked_ground_def` is computed by a full DFS of the def's resolved type (`varIsGround`, clearing `var_set` twice) before deciding whether to orphan-copy it. The groundness of a processed def's type can only change if an error is written into it later.
- Fix: memoize per def in `DefProcessed` (`ground: enum(u2) { unknown, yes, no }`) at the first query, invalidated only when `types.mayContainErrorState()` flips (or simply never cached when the store may contain errors, keeping today's exact behavior for erroring modules).
- Status: open

### src/check/Check.zig:25658-25690: the lookup leaf's hoist bookkeeping probes four more pattern-keyed hash maps
- category B; runs per `e_lookup_local` leaf of a non-top-level name; impact medium; risk low
- Finding: `local_procedure_candidates.getPtr` (884, AutoArrayHashMap keyed by Pattern.Idx), `hoist_known_values.get` (840) twice, `local_processing_ptrns.get` (436), `predeclared_local_annotations.get` (614), `local_value_processing_ptrns.contains`, and inside `ensureHoistedBindingRoot`/`hoistKnownBindingAvailable` further `hoist_selected_bindings.get` (850). Each is a hash + probe on a dense node id.
- Fix: `DenseMap(CIR.Pattern.Idx, ...)` for the scoped ones (`local_processing_ptrns`, `local_value_processing_ptrns` are push/pop scoped per local def), a node-slot column for `hoist_known_values`/`hoist_selected_bindings` (sentinel `u32`), and `local_procedure_candidates` as a column of candidate indices.
- Status: open

### src/check/Check.zig:10308-10312, 10302-10306, 10314-10318: `freshStr` / `freshBool` / `freshU64` run a full scheme instantiation to mint one zero-argument nominal
- category E, A; runs per string literal segment (`.e_str_segment` 25380), per string pattern, per `if` condition (`freshBool`), per comparison binop result, per `.e_str`, per …; impact medium (string literals and conditions are ubiquitous; each …; risk low
- Finding: each calls `instantiateVar(self.str_var, ...)`, which goes through `instantiateVarHelpFromScheme` (9770): `enqueueLocalSchemeRequirements` (a `type_scheme_by_var` hash probe), `var_map.clearRetainingCapacity()`, the Instantiator state-machine walk, a fresh `instantiated_requirements: ArrayListUnmanaged` (9828) init+deinit, `typeSchemeIndexForRoot` (second hash probe), …
- Fix: `freshStr`/`freshBool`/`freshU64` = `freshFromContent(try self.mkBuiltinNominalContent(.str|.bool|.u64), env, region)` (mirror `mkNumberTypeContent`, using `sourceDeclForBuiltinNominal`). Verify the copied `str_var`'s descriptor flags (`nominal_backing_structure`, etc.) are reproduced by the `mkNominalWithSourceDeclAndBuiltinOrigin` path and that no scheme-use record is expected for these builtins (evidence is …
- Status: open

### src/check/Check.zig:9828-9838 + 2147-2185: `instantiated_requirements: ArrayListUnmanaged(InstantiatedSchemeDispatchRequirement)` (~120-byte elements) created and destroyed per instantiation
- category A, C; runs per instantiation of any binding (every use of a generalized name, every dispatch target instantiation, every `freshStr`/`freshBool` today); impact medium (no allocation in the common case, but the `.empty` …; risk low
- Finding: the list starts `.empty`, is filled by `copySchemeDispatchRequirements` only for schemes with explicit requirements (rare), and `deinit`s on exit. When used, each element inlines an 88-byte `StaticDispatchConstraint` plus 5 vars + 2 bools.
- Fix: a checker-owned `scratch_instantiated_requirements` with a `top` mark (instantiations nest only through `copySchemeDispatchRequirements` -> `instantiateTypeScheme`, which does not call back into `instantiateVarHelpFromScheme`, so a simple clear-per-call suffices; verify with a debug depth counter). Store the constraint by its `StaticDispatchConstraint.SafeList.Range`/index into `types.static_dispatch_constraints` …
- Status: open

### src/check/Check.zig:10169-10187 + 38612-38625: `enqueueLocalSchemeRequirements` linearly scans the whole deferred queue per requirement per instantiation
- category E; runs per instantiation of a local scheme that has explicit dispatch requirements (every use of a `where`-constrained or codec-deriving local function), per …; impact medium (quadratic in dispatch-heavy modules); risk medium (the queue is …
- Finding: `deferredDispatchRelationIsQueued(env.deferred_static_dispatch_constraints.items.items, receiver, fn_var)` walks every queued entry and, for matching receivers, every constraint in its range. With Q queued entries and U uses this is O(Q * U * R).
- Fix: index queued relations by `(receiver root, fn_var)` in a `DenseMap(Var, u32)` chain maintained by `enqueueDeferredDispatchConstraint` / drain compaction, or keep a per-requirement `queued_in_epoch` stamp on the scheme requirement.
- Status: open

### src/check/Check.zig:9888-9892 + 10189-10198: `schemeHasEvidenceParams` allocates a `dispatch_evidence.Scratch` + `params` list and walks the scheme for every use whose substitution is empty
- category A, E; runs per scheme use of a binding whose instantiation produced no substitution pairs: every use of a monomorphic (ground) top-level function or constant that went …; impact medium-high (a hashed DFS over the callee's whole type per …; risk low-medium
- Finding: `if (self.scratch_evidence_pairs.items.len > 0 or try self.schemeHasEvidenceParams(scheme_source))` runs `enumerateEvidenceParams` with a fresh `dispatch_evidence.Scratch` (which holds `visited: AutoHashMapUnmanaged(Var, void)`, `emitted_methods`, `contract_classes`: first-pass dispatch_evidence.zig:227) and a fresh `params` ArrayList, then discards everything, returning only `params.len != 0`.
- Fix: memoize per resolved scheme root in a `DenseMap(Var, bool)` (`scheme_evidence_params_memo`), cleared when the store's generalized region changes (generalization boundary): a generalized scheme's structure is immutable, so a per-root memo is sound between boundaries; keep one `dispatch_evidence.Scratch` on `Self`.
- Status: fixed (fix 25)

### src/check/Check.zig:8695-8700 + 9831, 10170, 10201: `type_scheme_by_var: AutoHashMapUnmanaged(Var, u32)` probed three times per instantiation (deepens first-pass 130-1352)
- category B; runs per instantiation: `enqueueLocalSchemeRequirements` (10170), `typeSchemeIndexForRoot` at 9831, `schemeHasExplicitRequirements` (10201); impact low-medium; risk low
- Finding: three identical hash probes of the same key per use of a binding.
- Fix: resolve once at the top of `instantiateVarHelpFromScheme` and pass the `?usize`; make the map a `DenseMap(Var, u32)`.
- Status: open

### src/check/Check.zig:10020-10175: `registerInstantiatedVar` is `inline` and runs per copied var with 2 `resolveVar`s, a `constraintIsLiteralConversion` double pass and region bookkeeping
- category E; runs per var copied by every instantiation (the dominant inner loop of instantiation); impact medium (inner loop of every instantiation); risk low
- Finding: for every `(old, fresh)` pair the function resolves both vars, computes `fresh_constraints_len`, re-resolves `old` for the identity test, then for flex copies iterates the constraints twice (`has_literal_constraint`/`has_other_constraint`, then `recordOpenLiteralVar` iterates again and `constraintIsLiteralConversion` -> `literal_defaulting.constraintLiteralKind(self.literalMethodIdents(), ...)` rebuilds the ident …
- Fix: the Instantiator already knows the descriptor of the fresh var at copy time: have it emit the pair list with the content tag and constraint range, avoiding the re-resolve; compute `literalMethodIdents()` once per instantiation; grow `regions` with `appendNTimesAssumeCapacity`.
- Status: open

### src/check/Check.zig:10941-10982 + 6683: `open_numeral_literals` stores an 88-byte `StaticDispatchConstraint` copy per numeric literal occurrence, mutated only in `provenance`
- category C, D; runs per numeric literal occurrence and per instantiated copy of one; impact low-medium (memory and scan bandwidth of the defaulting …; risk low
- Finding: `var occurrence_constraint = constraint; occurrence_constraint.provenance = ...; append(.{ .var_, .constraint = occurrence_constraint })` copies the whole constraint (first-pass types.zig:1064 measured it at ~88 bytes because of the inline `NumeralInfo`) into a worklist that the defaulting passes scan repeatedly.
- Fix: store `{ var_, constraint_idx: u32 (index into types.static_dispatch_constraints), provenance }`; the defaulting passes read the constraint through the index.
- Status: open

### src/check/Check.zig:2022-2063: `recordSchemeRequirementCandidate` copies an 88-byte constraint into `scheme_requirement_candidates` per attached-dispatch registration
- category C; runs per non-literal constraint on every instantiated flex receiver and per scheme requirement copy (2297, 2252); impact low-medium; risk low
- Finding: `SchemeRequirementCandidate` (1619) inlines `constraint: StaticDispatchConstraint`; the candidates list is scanned per owner at generalization (`captureEscapedSchemeDispatchRequirements`).
- Fix: store the constraint's store index/range; the owner-grouping is the first-pass F finding (564).
- Status: open

### src/check/Check.zig:23364-23366: `checkPattern` allocates a fresh `std.ArrayList(PatternCheckFrame)` per pattern checked
- category A; runs per pattern root: every def pattern, every lambda parameter (27919), every `for` pattern (27584), every match branch pattern, every local `s_decl`/`s_var` …; impact medium (one malloc/free per binder-introducing construct); risk low
- Finding: `var frames: std.ArrayList(PatternCheckFrame) = .empty; defer frames.deinit(self.gpa);`: the first `append` mallocs, and the list is freed on return. `PatternCheckFrame` is ~40 bytes. Patterns are shallow, so the whole cost is the malloc/free pair.
- Fix: `self.pattern_check_frames: ArrayListUnmanaged(PatternCheckFrame)` with a base index (`checkPattern` can recurse only through `checkNominalTypeUsage`?: verify it does not; if it can nest, use the base-offset discipline that `type_visit_stack` uses).
- Status: fixed (fix 40)

### src/check/Check.zig:23443-23447 + 23696-23705: `stepPatternCheck` re-decodes the pattern node at every resume, and a record destructure decodes each `RecordDestruct` twice
- category E; runs per child of a compound pattern (one extra decode per child); per record destructure field (two decodes); impact low; risk low
- Finding: `const pattern = self.cir.store.getPattern(pattern_idx)` runs on every step of a frame (N+1 times for N children); the record arm calls `getRecordDestruct(destruct_idx)` when requesting the child (23697) and again when the child returns (23648).
- Fix: cache the decoded pattern (or the span it iterates) in `PatternCheckFrame` on first entry; decode the destruct once and keep `kind`/`label` in the frame state.
- Status: open

### src/check/Check.zig:4191-4195: `recordPatternCandidateDepth` hash-`put`s every pattern checked inside a local function candidate (deepens first-pass 130-1352)
- category B; runs per pattern node checked while `local_procedure_candidate_stack` is non-empty (every parameter and binder inside any named local lambda); impact low-medium; risk low
- Finding: `local_pattern_candidate_depths: AutoHashMapUnmanaged(Pattern.Idx, u32)` receives a `put` per pattern; `noteLocalLookupForLocalProcedures` (4199) `get`s it per local lookup.
- Fix: a `u32` column over node slots (0 = none), written at the same point.
- Status: open

### src/check/Check.zig:17177-17182: `isExecutableRootDef` / `isCompileTimeExecutableRootDef` are linear scans per def
- category E; runs per def begin (17204) and per function def finish (17144); scans `executable_root_defs` / `compile_time_executable_roots`; impact low; risk low
- Finding: O(roots) per def, O(defs * roots) per module; roots are the `main`/`expect`/provides entries so usually small, but platform modules with many provides pay it on every def.
- Fix: a bit in the per-def column (`def_group` already is a column over def slots).
- Status: open

### src/check/Check.zig:30628-30654: `exprAlwaysCrashes` allocates a fresh `std.ArrayList` per call; called up to twice per unannotated value def
- category A; runs per unannotated top-level value def (17117, 17121) and per `exprIsAllCrashConditional` query; impact low; risk low
- Finding: a pending stack is created/freed per call; the two calls at 17117/17121 walk the same expression with different `erroneous_values` modes.
- Fix: reuse `self.scratch_exprs` (a `base.Scratch(CIR.Expr.Idx)`-style stack already exists for operands: `retired_operand_pool`); fold the two modes into one walk returning both answers.
- Status: fixed (fix 40)

### src/check/Check.zig:17064-17066, 17093, 17110-17114, 17129, 17135, 17142, 17157: `finishDef` re-decodes the def's pattern and expression 4-6 times
- category E; runs per top-level def; impact low; risk low
- Finding: `getPattern(def.pattern)` is decoded at 17110, 17129, 17157; `getExpr(def.expr)` at 17066 (`def_expr`), 17135, 17142, 17149; `appliedAnnotation` (a `rejected_value_annotations` hash probe) is called in `beginDef` twice and in `finishDef` once; `platform_required_defs.get(def_idx)` (hash keyed by Def.Idx) in both begin and finish.
- Fix: decode once into locals; carry `applied_annotation` and `platform_required` in `DefActivity`; make `platform_required_defs` a column over def slots.
- Status: open

### src/check/Check.zig:16936-16938 + 18380-18384: recursive `GroupActivity` allocates `member_roots` per group and `beginGroup` decodes each member's def + expr
- category A, E; runs per recursive binding group (and once more per def inside `stepGroup .members` where `getDef` + `getExpr` + `isFunctionDef` run per member before pushing its …; impact low (per group); risk low
- Finding: `state.member_roots = try self.gpa.alloc(BoundaryRoot, scc.defs.len)` is justified by nesting ("the boundary can nest other groups' checks, which use the scratch lists themselves"), but a checker-owned stack with a base offset handles nesting without allocation.
- Fix: `self.boundary_roots_stack: ArrayListUnmanaged(BoundaryRoot)` + `(start, len)` in the activity (`BoundaryActivity.roots` becomes a span into it); the `finishExprFrame` one-element alloc (above) uses the same stack.
- Status: open


## Parser internals, interners, layout store, runtime builtins
### src/parse/tokenize.zig:1264: Token buffer reserves one token slot per source byte
- category G/H; runs once per module (per tokenize call), but the reservation is proportional to source size; impact medium (memory per module; MultiArrayList capacity is …; risk low
- Finding: `Tokenizer.init` calls `TokenizedBuffer.initCapacity(env, gpa, text.len)` (line 1264, with a `TODO: tune this` comment). `Token` is a `MultiArrayList` of `tag: u8`, `region: Region` (8 bytes), `extra: union` (8 bytes: `IdentWithFlags` is `Ident.Idx` + 2 bools padded to 8), so the three columns reserve ~17 bytes per source byte. Real Roc has roughly one token per 4-6 source bytes (comments, whitespace, multi-byte …
- Fix: reserve `text.len / 4 + 16` (any under-estimate just takes the amortized `append` growth path, which is already there because empty `StringPart` tokens can make tokens exceed bytes), or do a cheap first pass; alternatively `shrinkAndFree` the three columns in `finishAndDeinit` once the final count is known so the AST does not pin the slack. No semantics change: `append` already handles growth.
- Status: fixed (fix 55)

### src/parse/tokenize.zig:1316-1340: Identifier tokens recompute and store underscore flags that duplicate `Ident.Idx.attributes`
- category C/D; runs per identifier token (every `LowerIdent`/`NamedUnderscore`); impact low-medium (4 bytes × tokens, plus one fewer scan per ident …; risk low
- Finding: `pushTokenInternedHere` calls `base.Ident.for_text(text)` (line 1316), which runs `Attributes.fromString` (`startsWith "_"` and `endsWith "!"`), then separately recomputes `starts_with_underscore = text[0] == '_'` (line 1321) and `ends_with_underscore` (line 1322) and stores both bools next to the `Ident.Idx` in `Token.Extra.ident_with_flags`. `starts_with_underscore` is exactly `id.attributes.ignored`, which is …
- Fix: drop `starts_with_underscore` (read `idx.attributes.ignored` at the consumer), and store `ends_with_underscore` in the `reserved` bit of `Ident.Attributes` or recompute it at its single consumer from the ident text (it is only needed for type-variable `a_` checks). That shrinks `Token.Extra` to 4 bytes, i.e. 4 bytes per token across the whole token stream. Verify by grepping consumers of `ident_with_flags` (Parser …
- Status: fixed (fix 54): the flags are gone; nothing read them

### src/parse/tokenize.zig:967-975: Keyword recognition via `StaticStringMap.get` on every lowercase identifier
- category E; runs per lowercase identifier token; impact low-medium (identifiers are the most common token class); risk low
- Finding: `chompIdentLower` calls `Token.keywords.get(ident)` for every lowercase identifier. `std.StaticStringMap` buckets by length and then does a linear `memcmp` over all keys of that length (35 keywords, e.g. 7 keys of length 2-3, 6 keys of length 5-8), so a typical 4-8 letter identifier pays several `memcmp` calls before failing. Nearly all lowercase identifiers are not keywords.
- Fix: gate on first byte + length with a comptime-built `[26][16]` table, or a comptime perfect hash over (len, first, last) before the final memcmp. Semantics unchanged; verify with the existing `keyword text is the inverse of keyword recognition` test.
- Status: no change: `StaticStringMap` already buckets keywords by length, and a hand-built table would duplicate the keyword list

### src/parse/tokenize.zig:792-820: `chompTrivia` consumes comments and whitespace one byte at a time
- category H; runs per byte of whitespace/comment (a large fraction of real source bytes); impact low-medium; risk low
- Finding: the comment branch (`'#'`, lines 808-812) loops `while (pos < len and buf[pos] != '\n' and buf[pos] != '\r')` one byte per iteration; the whitespace branches likewise. Comments and indentation routinely dominate source byte counts in Roc files.
- Fix: `std.mem.indexOfAnyPos(u8, buf, pos, "\n\r")` (SIMD-vectorized in std) for comment bodies, and a tight run-skip for `' '` using `@Vector` comparisons or `indexOfNonePos`. Diagnostics for CR and ASCII control bytes must still be reported at the same offsets, so keep the per-byte path for bytes < 32 other than `\t`/`\n`/`\r`.
- Status: no change: the standard multi-byte search is itself a scalar loop, so it would not beat this one, and the control-byte diagnostics need the per-byte walk outside comments

### src/parse/tokenize.zig:1872-1915: String-literal body validated codepoint-by-codepoint with `isPrint` on every ASCII byte
- category H; runs per byte inside string literals; impact low; risk low
- Finding: `tokenizeStringLikeLiteralBody` calls `chompUTF8CodepointWithValidation` (line 1900) for each byte, which for ASCII does `std.ascii.isPrint(c)` and a branch, and the loop separately tests `$`, `\n`, `"` per byte. Multi-line strings and long literals pay 4-5 branches per byte.
- Fix: scan runs of plain ASCII (`0x20..0x7E` excluding `"`, `\\`, `$`) with `indexOfNone`/vector compare and only fall into the per-codepoint path at the first special byte; keep the diagnostics identical (they fire only on non-printables, which are exactly the bytes the fast scan stops at).
- Status: no change: string bodies are validated per codepoint to report each diagnostic at its offset

### src/parse/tokenize.zig:1349-1360 + src/base/CommonEnv.zig:361-373: Three separate full passes over the source bytes (bidi scan, tokenizer, line starts)
- category E; runs 3 × source length per module; impact low-medium (calcLineStarts is scalar and touches every byte); risk low
- Finding: `tokenize` first runs `base.bidi.Iterator` over the whole buffer (SIMD 16-byte blocks, cheap but a full extra pass with its own cache traffic), then the main loop, and `CommonEnv.calcLineStarts` later walks every byte again with a scalar `for (source) |c| if (c == '\n')` loop and an allocation-checking `append` per newline (lines 361-373).
- Fix: compute line starts with `std.mem.indexOfScalarPos(u8, source, pos, '\n')` (vectorized) and pre-size by `std.mem.count(u8, source, "\n")`; alternatively record line starts inside `chompTrivia` where the tokenizer already sees every `\n`. The bidi pass is cheap enough to keep.
- Status: partly fixed (fix 55): line starts are found with the vectorized newline search; the bidi pass and the tokenizer remain separate passes

### src/base/SmallStringInterner.zig:100-140 + src/base/InternedBytes.zig:144-166: Identifier interner probes without fingerprints: every collision costs a strlen + memcmp
- category E; runs per identifier insertion (every identifier token in every module, plus every `findByString`/`lookup`/`contains` from later stages, plus every entry on resize); impact high for the tokenizer/canonicalizer identifier path (the …; risk medium (touches the serialized …
- Finding: `InternedBytes.Index` supports a 1-byte fingerprint column (`use_fingerprints`), and `StringLiteralPolicy` enables it, but `SmallStringInterner.Policy` does not (no `use_fingerprints` decl → `false` at line 38). So in `findStringOrSlot` every occupied probe slot that is not the target runs `Policy.textForId` = `std.mem.sliceTo(bytes[off..], 0)` (a null-terminator scan, line 119) followed by `std.mem.eql` (line 160). …
- Fix: set `pub const use_fingerprints = true` in `SmallStringInterner.Policy` and persist the fingerprint column alongside `index` (serialize it, or rebuild it from the cells on `deserializeInto`/`enableRuntimeInserts` at 1 byte/slot). Even better, store the low 32 bits of the hash per slot (4 bytes/slot) which also removes the per-entry Wyhash in `resize`. `SmallStringInterner.insert` currently rebuilds …
- Status: partly fixed (fix 29: a probe compares the needle and the terminator without first scanning the entry; fingerprints not added)

### src/base/SmallStringInterner.zig:253-257: `getText` scans for the null terminator on every identifier text fetch
- category E; runs every `env.getIdent(idx)` call in every stage (canonicalize scope lookups, type printing, dispatch, digests, codegen symbol names, ...); impact medium (O(len) → O(1) on a function called extremely often; …; risk medium (changes the serialized cache …
- Finding: `Ident.Idx.idx` is a byte offset, and the entry format is `bytes\0`, so `getText` must run `std.mem.sliceTo(bytes[start..], 0)` (strlen) each time (line 256). The file header (lines 19-22) says the size "uses only 1 byte to encode", which is not what the code does: it uses a terminator, so the length is rediscovered by scanning on every access. `TextRankCache.ensure` even comments (line 63-64) that it must cache …
- Fix: store a length prefix (`u8` length byte, with 0xFF escaping to a 4-byte length for the rare long name) immediately before the text, so `getText` is `bytes[idx-1]` + slice. `Idx` stays the content offset so every existing consumer and serialized module is unaffected except the writer/reader of the one prefix byte; `textForId` in the hash probe gets the same win. Verify via the existing roundtrip tests (expected …
- Status: open

### src/base/Ident.zig:175,347-353: `Ident.Store.attributes` column is written only by `genUnique` and never read
- category H/C; runs per generated unique name; serialized with every module; impact low; risk low
- Finding: `Store.attributes: collections.SafeList(Attributes)` is appended in `genUnique` (line 348) with an `expected_idx` check that compares the append index with its own length (trivially true), but `insert` (line 260) never appends to it, so the column is not parallel to the interner and nothing indexes into it (only `clone`, `deinit`, `Serialized.serialize`/`deserializeInto`, and `deserializeOwned`'s clone touch it). …
- Fix: delete the column and the serialized field (cache version bump), or stop appending in `genUnique`. Verify nothing reads `idents.attributes` (grep shows only the sites above).
- Status: open

### src/base/TextRankCache.zig:120-121 + src/base/Ident.zig:381,398,404: Rank column is sized by interner *byte* length, not entry count
- category C/E; runs rebuilt once per interner generation (any insert invalidates it); indexed per row-sort comparison; impact medium if interleaved regeneration happens; otherwise low …; risk low
- Finding: because `Ident.Idx.idx` is a byte offset, `Store.textRanks` passes `limit = interner.bytes.len()` and `ensure` does `self.ranks.resize(limit)` (line 120) then writes `ranks[entry.index]` only at entry offsets. The column is therefore 4 bytes per *byte* of identifier text (≈ 4-5× more than one u32 per identifier), and every regeneration re-allocates/zeroes that much. `ensure` also allocates `entries` (24 bytes × …
- Fix: keep a dense per-entry ordinal (the interner can store the entry's ordinal in a parallel u32 column keyed by byte offset / 2 or a small side map built once at `ensure`), or retain `entries`/`scratch` capacity across generations in the cache (it already owns an allocator). The callers at `src/check/canonical_type_keys.zig:767,832,1687` call `textRanks` lazily; if any ident insertion happens between calls in a loop, …
- Status: open

### src/base/StringLiteral.zig:248-277: `appendFresh` does three growth-checked appends per literal and stores an 8-byte header
- category G/C; runs per string literal insertion; impact low; risk low
- Finding: `appendFresh` appends the 4-byte length, then the 4-byte alignment, then the bytes, each via `buffer.appendSlice` with its own capacity check (lines 255, 263, 272). The alignment word is written as `1` for every literal and only bumped by `requireAlignment`; a u32 for a power-of-two alignment is 4 bytes per literal where a single byte (log2) would do.
- Fix: `ensureUnusedCapacity(entry_header_size + string.len)` once then `appendSliceAssumeCapacity` ×3. The header shrink is a cache-format change (keep unless the format is being revised anyway).
- Status: open

### src/base/CommonEnv.zig:195-215: `rebuildStringBuilder` iterates the store twice and re-hashes every literal on every owned deserialization
- category E; runs once per `deserializeOwned` (loaded cached module that will be appended to); impact low; risk low
- Finding: a counting pass and a registering pass over every entry (lines 203-212); each registration re-hashes the literal bytes with Wyhash (`insertExistingAssumeCapacity`). The probe table could be serialized (as the identifier interner does) instead of rebuilt.
- Fix: serialize the `BuilderState` index cells + fingerprints next to the store, or at least count from a stored entry count rather than a full pass. Only matters when many cached modules are loaded in owned mode.
- Status: open

### src/base/CommonEnv.zig:36-50 + src/parse/NodeStore.zig:318-368: Fixed per-module up-front allocations (~45 KB) regardless of module size
- category G/H; runs once per module (and once per REPL/snapshot/LSP parse call, which can be very frequent in the LSP); impact low (medium for LSP-style repeated parses); risk low
- Finding: `CommonEnv.init` reserves a 1024-ident interner (4 KB bytes + 2048×4 B probe table + a heap-allocated `TextRankCache`), a 4 KB string buffer, and 256 line starts; `NodeStore.initCapacity` creates 20 `base.Scratch` stacks each `initCapacity(64)` (20 allocations), plus `numeric_literals` (capacity/8 × ~64 B), `numeric_literal_bytes` (capacity bytes), `pattern_string_parts`, `field_access_segments`. For the many tiny …
- Fix: lazily allocate the rarely-used scratch stacks (target entries, target files, target config entries/values, symbol map entries, for-clause aliases, requires entries are only used by platform headers), and size numeric/pattern buffers from the token count rather than a fixed share. Semantics unchanged.
- Status: open

### src/parse/tokenize.zig:27-37,1319-1332: `Token.Extra.ident_with_flags` is never read anywhere (dead 8-byte column payload)
- category C/H; runs per identifier token (storage), every token (column width); impact medium (memory and cache footprint of the token stream, …; risk low
- Finding: grep over `src/` for `ident_with_flags`, `starts_with_underscore`, `ends_with_underscore`, `IdentWithFlags` finds no consumer outside `tokenize.zig`; `TokenizedBuffer.resolveIdentifier` (line 688-698) only extracts `.ident` from it. So the tokenizer computes two bools per `LowerIdent`/`NamedUnderscore` (lines 1321-1322) and the `extra` union is widened from 4 to 8 bytes for every token in every module, purely for …
- Fix: delete `IdentWithFlags`, `hasUnderscoreFlags`, and the branch at line 1319; make `Extra = union { interned: Ident.Idx, none: u32 }` (4 bytes). `HTML.zig:24` reads `items(.extra)` only via `resolveIdentifier`-style access, so it keeps working. Token column memory drops by 4 bytes/token (~20-25% of the token stream).
- Status: fixed (fix 54)

### src/parse/NumericLiteral.zig:202-234 + src/parse/NodeStore.zig:507-534: Every numeric literal makes 4-10 heap allocations that are copied and freed immediately
- category A/D; runs per numeric literal token (expressions and patterns; `Parser.zig:3529,3566,6543,6588`); impact medium (literals are 3-8% of tokens in typical code; each …; risk low-medium
- Finding: `NodeStore.addNumericLiteral` calls `NumericLiteral.parse`, which returns `Owned` with heap-allocated `before`/`after` slices, then copies both into `numeric_literal_bytes` (lines 516-517) and frees them (`parsed.deinit`). Inside `parse`: `parseExactInteger` builds a temporary `ArrayList` in `intDigitsToBase256` (line 873) and then `bytesLeToBe` allocates the result (line 975); even the empty `after` is …
- Fix: give `NumericLiteral.parse` an output writer that appends base-256 digits directly to `NodeStore.numeric_literal_bytes` (reserve, write little-endian into the tail, reverse in place), reuse two store-owned scratch `ArrayList(u8)` for `before_raw`/`after_raw`, compute `compactFrac` from the already-computed `DecimalParts` instead of reparsing, and pass the suffix split from the parser into `parse`. Verify with the …
- Status: open

### src/parse/NumericLiteral.zig:117-143 + src/parse/NodeStore.zig:361: `NumericLiteral.Stored` is ~64 bytes with 16-byte alignment, pre-reserved at tokens/8
- category C/G; runs per numeric literal (storage); per module (reservation); impact low-medium (memory); risk low
- Finding: `Stored` holds `compact: Compact` (a `union(enum)` whose `int`/`dec` payloads are 17/16 bytes with `i128` alignment), `after_decimal_digit_count: u64`, three `u32`s and two `u8`s → 64 bytes/entry at 16-byte alignment. `NodeStore.initCapacity` reserves `capacity / 8` entries (line 361) where `capacity` is the token count, i.e. 8 bytes per token up front, and `numeric_literal_bytes` reserves `capacity` bytes (line …
- Fix: side-table the rare `.dec`/`.int` 128-bit payloads (or store `compact` as a 17-byte `extern` blob with u8 alignment), make `after_decimal_digit_count` a u32 unless > u32 (then force `.exact`), and size the two buffers from a running count instead of the token count. No output change.
- Status: open

### src/parse/NodeStore.zig:2969-2980,3007-3018,3038-3049,3081-3092,3133-3143,3278-3288,3650-3659: Scratch-to-extra_data span copies append one u32 at a time
- category G/D; runs per collection node (every call-argument list, list literal, tuple, block statement list, lambda args, match branches, record fields, type args, qualifier …; impact low-medium (these run for every multi-child node; the …; risk low
- Finding: `exprSpanFrom`, `statementSpanFrom`, `patternSpanFrom`, `patternStringPartSpanFrom`, `dataSpanFromScratch`, `tokenSpanFrom` and `symbolMapEntrySpanFrom` all loop `while (i < end) try store.extra_data.append(gpa, scratch.items[i])`, paying a capacity check and a bounds-checked store per element. The scratch items are u32-backed enums, so the whole range is one contiguous `[]const u32`.
- Fix: `try extra_data.ensureUnusedCapacity(gpa, end - start); extra_data.appendSliceAssumeCapacity(@ptrCast(scratch.items.items[start..end]));` in one shared helper (there are seven near-identical copies). Identical output.
- Status: fixed (fix 54)

### src/parse/NodeStore.zig:1169-1186,1258-1266,1619-1630: `lambda`, `apply`, `match`, `ty_fn` store one child index behind an extra_data indirection even though a node word is free
- category C/E; runs per lambda / apply / match / function-type node (apply is one of the most common expression nodes), on every `getExpr` of those nodes; impact low-medium; risk low
- Finding: `addExpr(.apply)` writes `fn` into `extra_data` and stores the extra_data index in `node.main_token` (lines 1183-1185); `getExpr(.apply)` then does `store.extra_data.items[node.main_token]` (line 2531). Same for `lambda.body` (1174-1176 / 2524), `match.expr` (1263-1265 / 2568) and `ty_fn.ret` (1627-1629 / 2931). `main_token` is a plain `u32` that these node kinds do not use for a token, so the child index could live …
- Fix: store `@backingInt(app.fn)` in `node.main_token` and read it back directly; update the S-expr/formatter consumers that go through `getExpr` (they do not touch `main_token` directly for these tags; verify with grep for `.apply` handling of `main_token` in `src/fmt` and `AST.zig`).
- Status: open

### src/parse/NodeStore.zig:939-954,2177-2203: Tag patterns spend 6 extra_data words, three of them for bools
- category C; runs per tag pattern; impact low; risk low
- Finding: `addPattern(.tag)` appends `args.len`, `qualifiers.start`, `qualifiers.len`, and three `@intFromBool` words (lines 942-947); `rhs` holds the extra_data start and `lhs` the args start. The three flags fit in 3 bits of any of these words.
- Fix: pack `backing_value`/`has_args`/`record_shorthand` into the high bits of `args.len` (or into `rhs` alongside a 29-bit extra_data index, mirroring `ImportRhs`), dropping three extra_data words per tag pattern.
- Status: open

### src/parse/Parser.zig:3345-3377: `braceHasFieldComma` rescans every token to the brace's close without using the bracket table
- category E; runs every `{ ident = ...` block opening (lambda bodies and blocks whose first statement is a declaration: very common), plus the sibling lookahead at lines …; impact medium on deeply nested code, low otherwise; risk low
- Finding: `braceHasFieldComma` walks forward token by token from `self.pos + 2`, maintaining its own depth counter over every nested group until it reaches the brace's close, and returns early only in the `record_comma and LowerIdent OpAssign` case. Inside nested groups only depth changes matter (all other branches require `depth == 0`), yet every nested token is still visited. For nested blocks each level rescans the whole …
- Fix: on an opener in `.OpenRound/.NoSpaceOpenRound/.OpenSquare/.OpenCurly`, jump `token = bracket_matches.any[token] + 1` (treat `no_bracket_match` as "scan to EOF" exactly like the current depth logic does); keep the per-token walk only for `StringStart..StringEnd`/interpolation, which the bracket table does not track. Results are identical because no depth>0 token influences the return value.
- Status: open

### src/parse/Parser.zig:50-108: Three full-length bracket-match tables (12 bytes per token) built up front
- category C/H; runs once per parse; proportional to token count; impact low-medium (memory bandwidth per parse); risk medium (unbalanced-input semantics)
- Finding: `BracketMatches.init` allocates `any`, `curly`, `round` as `[]u32` of `tags.len` each, `@memset`s all three, and keeps three open-bracket ArrayLists while scanning. For a 200K-token module that is 2.4 MB of transient tables of which only the entries at opener positions (typically 10-15% of tokens) are ever meaningful; `curly`/`round` are consulted only at a handful of sites (`looksLikeTypeDecl`, …
- Fix: keep `any` as is (it is used in hot lookaheads) and derive the `curly`/`round` answers on demand from `any` by walking only the opener's own match chain (the close of a `{` in `any` is already the matching `}` when brackets are balanced; the separate tables only differ on unbalanced input, which can be detected by checking the closer's tag), or store them as a single `u32` table plus a 2-bit kind. Must preserve …
- Status: open

### src/parse/Parser.zig:435-601,622-679: Declaration recording reconstructs whole `Pattern`/`Expr` unions just to read a tag
- category E; runs per declaration statement (every `x = ...` in every block); impact low; risk low
- Finding: `recordStatementDecl` and `recordValueDecl` call `self.store.getPattern(decl.pattern)` and `self.store.getExpr(decl.body)` (lines 447-452, 632-637) only to test `pattern == .ident` and `activeTag(body) == .lambda`, then `patternSlice(body.lambda.args).len`. `getExpr` is a ~60-arm switch building a multi-word tagged union. The same pattern appears at `Parser.zig:3974` (`getExpr(...) == .tag`), `4165` (`== …
- Fix: add `NodeStore.nodeTag(idx)` / `exprIsLambda`-style accessors that read `nodes.items(.tag)[idx]` (and `data.rhs` for the arg count), and use them at these sites.
- Status: open

### src/parse/Parser.zig:1300-1358,780-823: Import statements build the module path in a temporary ArrayList and intern it again from text
- category A/D; runs per import statement (also per `import` for `addExplicitUnqualifiedImport`, line 1352-1355); impact low; risk low
- Finding: `importModulePathIdent` and `nestedImportPathIdent` allocate a fresh `std.ArrayList(u8)`, append the token texts with separators, and then `insertIdent` copies the bytes into the interner and frees the list. Fine per import, but imports run once per module and there is already a `scratch_idents`; a reusable byte scratch on the parser avoids the alloc/free pair.
- Fix: keep one `std.ArrayList(u8)` scratch on `Parser` (clear per use).
- Status: fixed (fix 54)

### src/parse/Parser.zig:686-694: `recordDestructuredValueNames` allocates a worklist per destructuring declaration
- category A; runs per module-scope destructuring declaration (rare); impact low; risk low
- Finding: a fresh `std.ArrayList(AST.Pattern.Idx)` is created and freed per call (lines 692-693); the parser owns `scratch_nodes`/`scratch_idents` that could serve.
- Fix: reuse a parser-owned scratch list.
- Status: fixed (fix 54)

### src/parse/Parser.zig:7255-7267: Type-dependency worklist uses a 4 KB stack buffer + fallback allocator per annotation
- category A/H; runs per tag-union item and per type declaration annotation when `collect_type_dependencies` is on; impact low; risk low
- Finding: `recordTypeDependenciesFromAnnoWorklist` declares a 4096-byte stack buffer and a `BufferFirstAllocator` on each call (line 7260-7262), so the common case does not heap-allocate, but it still constructs the allocator state and the function's frame carries 4 KB. Each call also calls `getTypeAnno` (full union) per visited node.
- Fix: reuse `parser.scratch_nodes`-style persistent worklist; minor.
- Status: open

### src/parse/DeclIndex.zig:66-77,327-341: Every parser scope owns three hash maps keyed by `Ident.Idx`
- category B/F; runs per scope (every block expression and lambda body creates a `.block` scope: `Parser.zig:3747,3769`), per declaration (`addDecl` → `getOrPut`); impact medium (allocation count per module scales with scope …; risk medium
- Finding: `Scope` embeds `value_decls`, `type_decls`, `import_alias_decls: std.AutoHashMapUnmanaged(Ident.Idx, NameBucket)`. Each is 24+ bytes of header inline and allocates its own table on first insert; `NameBucket` carries an inline `?DeclIdx` plus an `ArrayListUnmanaged` (24 bytes) for the overflow, so one declaration costs a 40-byte map slot plus hashing. Most block scopes hold 0-5 declarations; a linear scan of the …
- Fix: one flat `std.ArrayList(struct{scope: ScopeIdx, ident: Ident.Idx, kind: u8, decl: DeclIdx})` sorted (or a single module-wide map keyed by `(scope, ident)` → first DeclIdx with an intrusive `next` chain stored in `Decl.next_same_name`), so there is one allocation per module instead of three per scope and no `NameBucket` ArrayLists. Lookups by `(scope, ident)` keep the same results and source order.
- Status: open

### src/parse/DeclIndex.zig:237-238,339,354-361: Per-scope `scope_decl_builders` ArrayLists copied into `scope_decl_ids` on exit
- category F/D; runs per scope enter/exit and per declaration; impact low-medium; risk low
- Finding: `enterScope` appends an empty `ArrayListUnmanaged(DeclIdx)` to `scope_decl_builders` (line 339), `addDecl` appends to that per-scope list (384), and `exitScope` `appendSlice`s it into `scope_decl_ids` (356): an ArrayList-of-ArrayLists whose contents are copied once and whose storage lives until `deinit` (279-281). Scopes are strictly nested and exit LIFO, which is exactly the "scratch + spanFrom" pattern the …
- Fix: a single scratch `ArrayList(DeclIdx)`: on `addDecl` push; on `exitScope` move the entries above the scope's recorded scratch top into `scope_decl_ids` and truncate. One allocation instead of one per scope, no retained per-scope lists.
- Status: open

### src/parse/DeclIndex.zig:239,244,385,399: `decl_by_statement` / `type_path_by_statement` hash maps keyed by dense statement node index
- category B; runs per declaration (`put`) and per canonicalized statement (`get`); impact low-medium; risk low
- Finding: `decl_by_statement: AutoHashMapUnmanaged(u32, DeclIdx)` and `type_path_by_statement: AutoHashMapUnmanaged(u32, TypePathIdx)` are keyed by the statement's `Node.Idx` (a dense id into `NodeStore.nodes`). design.md "Dense IDs and structural keys" calls for a column or `DenseMap` here; the key is a `u32` so the lint does not catch it.
- Fix: since every statement node can have at most one decl, store the `DeclIdx` in the statement node itself (the `statement` node kinds leave `main_token` free for `decl`/`expr`, or add a parallel `nodes` column) or use `collections.DenseMap(Node.Idx, DeclIdx)`.
- Status: fixed (fix 54)

### src/parse/DeclIndex.zig:215-232: `Decl` record is ~100 bytes because ten fields are `?u32`-shaped optionals
- category C; runs per declaration (stored for the AST's lifetime); impact low (memory); risk low
- Finding: `name_tok: ?Token.Idx`, `name_ident: ?Ident.Idx`, `pattern: ?u32`, `anno: ?u32`, `paired_decl`, `paired_anno: ?DeclIdx`, `associated_scope: ?ScopeIdx`, `owner_type_path`, `type_path: ?TypePathIdx`: each `?u32` is 8 bytes in Zig (4-byte payload + tag + padding), so these nine optionals cost 72 bytes where sentinel-encoded `u32`s (`maxInt`) would cost 36. Plus `kind`, `value_form` (1 byte each) between 4-byte fields.
- Fix: use `enum(u32) { none = maxInt(u32), _ }` sentinel types for the optional ids (the file already uses that style for `Idx` enums), halving the record.
- Status: no change: a module holds a few thousand declarations, and `?T` keeps every consumer's null handling checked by the compiler

### src/parse/NodeStore.zig:3906-4002: `collectRedundantReturns` rebuilds full `Expr` unions for every lambda and walks via a fresh worklist
- category A/E; runs once per parse (`mod.zig:82`), proportional to node count; impact low; risk low
- Finding: scans every node tag, calls `getExpr` for each lambda to fetch `body` (line 3914), then processes with `getExpr`/`getStatement` union reconstruction per visited node, allocating a `DynamicBitSet` of `nodes.len()` bits and a `pending` ArrayList each call. The bit set is 1 bit/node (fine); the union reconstruction is the cost.
- Fix: read `extra_data[main_token]` for lambda bodies directly (or, after the apply/lambda fix above, `main_token` itself) and switch on `nodes.items(.tag)` with direct column reads for `block`/`if`/`match`.
- Status: open

### src/parse/NodeStore.zig:1723-1758: `addOrExtendFieldAccess` reads and writes the whole node row to bump two fields
- category H; runs per `.field` segment after the first in a field-access chain; impact low; risk low
- Finding: `store.nodes.get(node_idx)` copies all five columns into a `Node` and `store.nodes.set(node_idx, receiver_node)` writes all five back (lines 1730, 1744) to update `main_token += 1` and `region.end`.
- Fix: write the two columns directly via `nodes.items.items(.main_token)[i] += 1` and `items(.region)[i].end = ...`.
- Status: fixed (fix 54)

### src/parse/Parser.zig:3528-3531,3565-3568: Number token text classified three times
- category E; runs per numeric literal; impact low; risk low
- Finding: `NumericLiteral.deprecatedSuffixFromSource(text)` runs `numberTextEnd` for `.int` and again for `.frac` (lines 253-275 of NumericLiteral.zig), then `addNumericLiteral` → `parse` → `splitDeprecatedSuffix` runs `numberTextEnd` a third time, and `deprecatedSuffixFromText` does an inline-for over 13 `memcmp`s twice.
- Fix: compute the split once in the parser and pass `Split` into `parse`.
- Status: open

### src/layout/store.zig:3028-3033 + src/layout/digest.zig:216-220: Every `updateLayout` discards the whole memoized digest cache
- category E; runs `updateLayout` runs once per graph node in every `commitGraphUncached` (lines 1997-2082 resolve phase, 2364-2367 and 2402-2405 finalize): i.e. several times …; impact high on codegen of programs with many types (each refcount …; risk low (the memo semantics are unchanged: …
- Finding: `updateLayout` unconditionally calls `self.digest_cache.invalidate()`, which `deinit`s the entire `Digests` memo (every SHA-256 digest computed so far, plus the `recursive_keys` and `visiting` DenseMaps). `contentDigest` is queried by `rc_helper.zig:102` (every refcount helper symbol name), `solved_lir_lower.zig:5237,5931` and `LirCodeGen.zig:190`. Lowering interleaves commits and digest queries, so after each …
- Fix: the only updates that can change a digest are to layouts that were *reserved* (raw slots) and are still being resolved within one commit; a committed, interned layout is never rewritten. Invalidate only the digests of the specific updated `Idx` (and nothing else when it was not yet memoized, which is the case for fresh raw slots), or defer invalidation to the end of `commitGraphUncached` and only when a …
- Status: fixed (fix 21)

### src/layout/store.zig:1863-1871,2402-2412,3011-3024: Every graph node reserves a raw layout slot that is usually abandoned
- category H/C; runs per graph node per `commitGraph`; impact medium (memory and every per-layout column; also inflates …; risk medium
- Finding: `commitGraphUncached` first `reserveLayout`s a placeholder for every node (1863-1871), resolves provisional (uninterned) layouts into those slots (resolve phase), then in the finalize phase computes `value_layout` through the interning `put*`/`insert*` path (2331-2360). When the interned layout already existed, `value_layouts[i] != raw_layouts[i]` and the raw slot is left in `layouts` holding a copy …
- Fix: reserve raw slots lazily: only when a node is first observed as `raw_used` (a back-edge target or a recursive slot), which the finalize phase already tracks; non-recursive nodes can be finalized straight into interned layouts without a placeholder. Alternatively truncate `layouts`/`resolved_list_layouts` back to the pre-commit length for the trailing run of unused raw slots at the end of the commit (they are always …
- Status: open

### src/layout/store.zig:1988-2089,774-854: The resolve phase builds uninterned struct/tag-union data that the finalize phase rebuilds interned
- category E/D/H; runs per struct and tag-union node per `commitGraph`; impact medium-high (every struct/union commit does its row work …; risk medium
- Finding: `tryResolveNode` calls `buildUninternedStructLayout` / `buildUninternedTagUnionLayout` (lines 2056, 2081), which append the fields to the global `struct_fields` + `struct_field_offsets` + `struct_field_original_order` + `struct_data` (or `tag_union_variants` + `tag_union_data`) tables (774-854) and sort them; then `finalizeNode` rebuilds the same node through `putStructFields`/`putTagUnion` (2355-2359), which copies …
- Fix: have the resolve phase compute only what it needs (sizes/alignment and `isKnownZeroSized`, which `structSizes`/`tagUnionMetricsAt` provide from field layouts without committing rows) and write `Layout` values whose `StructIdx`/`TagUnionIdx` are allocated once in finalize; or make finalize reuse the rows the resolve phase appended when `raw_used`. Keep one store-owned scratch `ArrayList(StructField)` instead of …
- Status: open

### src/layout/store.zig:1453-1601,1603-1660,892-933,1039-1073: ~30 heap allocations per `commitGraph`
- category A; runs per layout graph commit; impact medium (allocator traffic per type lowered); risk low
- Finding: `commitGraph` allocates `mapping`, `pending_recursive` (AutoHashMap), `working` graph, `initialized`, `raw_layouts`, `value_layouts`, the duped `digests`; `RecursiveGraphAnalysis.init` allocates `keys`, `node_digests`, and the `Engine` allocates `digests`, `component`, `position`, `visit_index`, `low_link`, `on_stack` plus six ArrayLists, and each `strongConnect` call allocates `frames` and `walk_children`; …
- Fix: a `CommitScratch` struct owned by the `Store` with `clearRetainingCapacity` columns sized by `graph.nodes.items.len` (one `MultiArrayList` of per-node scratch fields would replace ~12 of the slices), reused across commits. Semantics unchanged.
- Status: open

### src/layout/store.zig:904-920 + 1845-1860: Tarjan's SCC is run twice per commit
- category E; runs per graph commit; impact low-medium; risk medium (the second pass also decides …
- Finding: `RecursiveGraphAnalysis.init` runs `Engine.strongConnect` over the original graph to assign recursive keys; `commitGraphUncached` then runs a second, independent Tarjan (`CycleFinder`) over the translated `working` graph to compute `recursive_nodes`/`component_ids`. The working graph is a node-subset renaming of the original (`mapping`), so the component ids from the first pass can be translated instead of …
- Fix: carry `component`/`recursive` facts through `mapping` into `commitGraphUncached` (pass them as inputs), dropping `CycleFinder`.
- Status: open

### src/layout/store.zig:129-130,399-403,3018-3023: Layout intern keys are heap-duplicated strings in a `StringHashMap`
- category A/F; runs per distinct non-scalar layout (every `insertLayout`/`internStructShape`/`internTagUnionShape` miss) plus one key hash per lookup; impact low-medium; risk low
- Finding: `rememberScratchInternKey` does `allocator.dupe(u8, scratch_intern_key.items)` per new layout (line 400) and `interned_layouts` is a `std.StringHashMap(Idx)`, so each layout costs an individual heap string, and `deinit` walks every key to free it (363-366). Lookups hash the whole key with Wyhash each time (`lookupInternedScratchKey`), including the per-element `appendInternKeyValue` byte appends that build it (each …
- Fix: store all keys contiguously in one `ArrayList(u8)` and key the map by `(start: u32, len: u32)` with a context that hashes/compares the slice (one allocation for all keys, no per-key free); build keys with `ensureUnusedCapacity` + `appendSliceAssumeCapacity`.
- Status: fixed (fix 47)

### src/layout/store.zig:2787-2801: `ensureZstLayout` scans the layout table for a sentinel that has a fixed index
- category E; runs every empty struct / zero-sized tag union commit (`putStructFields`, `putTagUnion`, `putNominalStructFields`, `getEmptyStructLayout`); impact low; risk low
- Finding: loops over `layouts` from index 0 comparing tags until it finds `.zst`; the zst primitive is pre-populated at a fixed sentinel index (`Idx.zst`, inserted in `init` lines 308-312) and `insertLayout` already returns `.zst` directly (line 3016).
- Fix: `return .zst;` (assert in debug that `getLayout(.zst).tag == .zst`).
- Status: fixed (fix 47)

### src/layout/store.zig:725-747,2902-2909,2854-2867: Struct sort recomputes `getLayout(..).sortKey()` per comparison and sizes walk fields twice
- category E; runs per struct commit; impact low; risk low
- Finding: `sortStructFields` compares with `ctx.store.getLayout(field.layout).sortKey()` on both sides of every comparison (O(n log n) layout fetches), `structFieldsSortKey` walks the fields again, and `structSizes` walks them twice (`.u32` and `.u64`) each calling `getLayout` and `sizeAt`/`alignment` per field; `appendStructFieldPlacement` walks a fourth time with `layoutSizeAlignAt`.
- Fix: compute `(sort_key, size32, align32, size64, align64)` per field once into a small scratch array, then sort/size from it.
- Status: partly fixed (fix 47): the sort reads each field's layout once; sizing still walks the fields per width

### src/layout/store.zig:114,500,3032: `resolved_list_layouts` is an `ArrayList(?Idx)` (8 bytes/layout) recomputed on every reserve/update
- category C/E; runs per layout reserve and per `updateLayout`; impact low; risk low
- Finding: the column stores `?Idx` (optional u32 → 8 bytes) per layout and `computeResolvedListLayoutIdx` walks the box chain on each `reserveLayout` and each `updateLayout` (the same raw slot is updated several times during a commit, recomputing each time).
- Fix: `Idx` with a `none` sentinel (4 bytes), compute once at the end of each commit for the touched range.
- Status: open

### src/layout/digest.zig:222-235: `DigestCache.get` spins on a mutex per query and re-points `store` each call
- category H; runs per `contentDigest` call (every RC helper symbol, cache key, erased-callable name); impact low-medium (parallel backends); risk low
- Finding: every query takes a spin lock (`tryLock` + `spinLoopHint` loop, no parking) even on memo hits, then writes `digests.store = store`. Code generation threads naming helpers contend on this lock.
- Fix: fast path: read the memo under a shared/reader lock or use an `RwLock`; only take the exclusive lock when a digest must be computed. The store pointer can be fixed at `Digests.init`.
- Status: open

### src/layout/store.zig:1341-1346,1381,1396-1398,1422: Cyclic component resolution allocates six temporaries and a hash map per SCC
- category A; runs per recursive SCC per commit (rare relative to acyclic nodes); impact low; risk low
- Finding: `resolveCyclicComponent` allocates `labels`, `next_labels`, `distinct_labels` (AutoHashMap of 32-byte keys), `sorted_labels`, `rank_of_member`, `block_rep`, `block_digest`.
- Fix: engine-owned scratch lists cleared per component.
- Status: open

### src/builtins/utils.zig:364-366,569-571,625-627,750-752,788-792: Debug refcount tracker hooks are compiled into every release incref/decref/alloc/free
- category H; runs every refcount operation and every refcounted allocation/free at program run time; impact medium (hot path of every Roc program; small per-op cost …; risk low
- Finding: `DebugRefcountTracker.onIncref/onDecref/onFree/trackAlloc/onRealloc` are called guarded only by `comptime builtin.os.tag != .freestanding`, not by `builtin.mode == .debug` (the struct's own comment says "zero overhead in release", but the gate is a runtime `if (!active) return;` on a global). `enable()` is called only from `src/eval/rc_conformance.zig:216` (a conformance checker), never in a shipped program, yet …
- Fix: gate the hooks with `comptime (builtin.mode == .debug or build option)` so release builtins contain no tracker code; expose the tracker to the conformance tool through a debug build.
- Status: open

### src/builtins/list.zig:181-198,1287-1289,1384-1389: Every incref of a list with refcounted elements performs a refcount read plus a heap store
- category H; runs per list incref when `elements_refcounted`; impact low-medium; risk medium (every length-changing path must …
- Finding: `increfWithAtomicity` calls `canReuseAllocation(.Immutable)` → `isExclusive` → `isUnique` → `refcount()` (reads the heap refcount) and, when unique and not a slice, writes the whole-allocation element count into the header word at `ptr - 2` (lines 189-196), then `increfDataPtr` reads the refcount again and does the atomic add. So an incref is two refcount loads, one dependent heap store and one atomic RMW. The …
- Fix: maintain `(ptr - 2)[0] = length` at the sites that set a unique list's length (`list_allocate`, `reallocate`, `appendRangeWithinCore`, `listAppendSublist`, `listSublist` in-place truncation, etc.), so `incref` becomes a single atomic add; or keep the store but skip the extra `isUnique` read by checking the refcount value returned from the atomic add. Verify with the seamless-slice teardown tests (`listConcat …
- Status: open

### src/builtins/list.zig:1659-1673: `List.sort_with` comparator copies both elements and increfs them on every comparison
- category H/D; runs per comparison in every `List.sort_with` (O(n log n) per sort); impact medium for sorts of refcounted elements; risk medium (ABI change)
- Finding: `compareErasedCallable` memcpy's both elements into a scratch args buffer and, for refcounted elements, increfs both (lines 1663-1670) because the erased callable ABI consumes its arguments; the callable then decrefs them. For a list of strings that is four atomic RMW operations and two copies per comparison, none of which change the data.
- Fix: an erased-callable calling convention variant that borrows its arguments (the compiler already distinguishes borrowed vs owned parameters for ordinary calls), so the sort comparator passes element pointers without copying or RC traffic. Falls under the same design as `List.sort_with` taking a borrowed comparator.
- Status: open

### src/builtins/list.zig:1048-1088,1503-1528: `List.release_excess_capacity` and shared `List.drop_at` incref every element and then decref every element
- category H; runs per element of the list, per call; impact low-medium (these ops are not in tight loops, but the …; risk low
- Finding: `listReleaseExcessCapacity` on a unique list with slack allocates, memcpy's, calls `inc` on every element, then `list.decref` which (because the list is unique) calls `dec` on every element before freeing (the `TODO` at line 1068 acknowledges this). The net effect on each element is +1−1 with two indirect calls and two atomic RMWs. The same pattern is in `listDropAt`'s fresh-allocation branch when the list is shared …
- Fix: when `list.isUnique`, move the elements (memcpy) and free the old allocation with `freeRcPtrC`/`utils.decref` *without* element teardown (as `reallocateFresh` already does for unique slices via `decrefAfterMovingSliceElements`), and only `dec` the dropped elements.
- Status: open

### src/builtins/list.zig:1125-1193: `List.prepend` shifts the whole list with `copyBackwards` even when spare capacity exists at the front of a unique slice (handled) but reserves via `listReserveForAppend` otherwise
- category H; runs per prepend; impact low-medium (prepend loops); risk medium
- Finding: after the seamless-slice fast path, a prepend grows the list at the *end* (`listReserveForAppend`) and then shifts every element up by one with `copyBackwards`, so a loop of prepends is O(n²) bytes moved. Lists have no front slack in their allocation, so the only improvement is to amortize the shift by reserving front slack when the list is reallocated for a prepend (grow by a geometric step and place the data at …
- Fix: in the `reallocateFresh` path taken from `listPrepend`, allocate `amortizedCapacity` and copy the elements to the *end* of the new buffer, returning a slice view; all the machinery (`decodeSliceAllocationPtr`, slot-before-window reuse) already exists.
- Status: open

### src/builtins/str.zig:1376-1383: `Str.concat` copies the appended bytes one byte at a time, backwards
- category D; runs per `Str.concat` (and every non-literal interpolation segment assembled through concat); impact medium (concat is one of the most common string operations; …; risk low
- Finding: after `reallocateForAppend`, the second string's bytes are copied with `var i = src.len; while (i > 0) { i -= 1; dest[i] = src[i]; }`. A hand-written reverse byte loop does not vectorize (the compiler cannot prove `dest` and `src` do not overlap, and the reverse order defeats the memcpy idiom recognizer), so every concat pays one load/store per byte. The reverse order exists because `arg2` may be a seamless slice …
- Fix: replace the loop with `@memmove(dest, src)` (Zig's memmove is exactly the overlap-safe primitive and lowers to the platform `memmove`, which is already in `shim_symbols.c_memory_set`). When the realloc moved the buffer, `src` cannot alias `dest`, so a `@memcpy` fast path gated on `result.asU8ptr() != arg1_original_ptr` is also possible, but `@memmove` alone captures nearly all the win.
- Status: open

### src/builtins/str.zig:1133-1153,1311-1331: `startsWith` / `endsWith` compare bytes in scalar loops
- category H; runs per `Str.starts_with` / `Str.ends_with` call; impact low (short prefixes dominate; but it is free); risk low
- Finding: both functions compare `prefix_len` bytes with an explicit `while (i < prefix_len) { if (bytes_ptr[i] != prefix_ptr[i]) return false; }` loop. `std.mem.eql` (which lowers to `memcmp`, in `c_memory_set`) uses word-at-a-time comparison.
- Fix: `return std.mem.eql(u8, bytes_ptr[0..prefix_len], prefix_ptr[0..prefix_len]);` and the analogous suffix slice for `endsWith`. Both already check the length bound first, so semantics are unchanged.
- Status: open

### src/builtins/dec.zig:1095-1255: `Dec` division is a 256-iteration bit-serial shift-subtract loop
- category H; runs per `Dec./`, and per iteration inside `decLnPositive`/`decExp`/`atan`/`tan`/`pow` (lines 453, 571, 584, 635-639) which call `RocDec.div` in loops; impact medium-high for Dec-heavy programs (financial code, the …; risk medium (arithmetic routine; needs …
- Finding: `RocDec.div` (502-555) forms a 256-bit numerator (`self.num * 10^18`) and calls `div_u256_by_u128`, whose main loop (1191-1249) performs one shift, two multi-word subtractions with overflow detection, and a 256-bit mask per *bit* of the quotient (`while (sr > 0) ... sr -= 1`), i.e. up to 256 iterations each with ~20 128-bit operations. For the overwhelmingly common case where both operands fit in 64 bits (|value| < …
- Fix: (1) fast path at line 530: if `numerator_u256.hi == 0`, return `divTrunc_u128(numerator_u256.lo, denominator_u128)` with the same rounding adjustment; (2) replace the bit-serial loop with a limb-wise `udivmod` over `[4]u64 / [2]u64` (Knuth algorithm D, the same algorithm `compiler_rt_128.zig:231` already implements for 128/128). Semantics are an exact integer quotient, so a differential test against the existing …
- Status: open

### src/builtins/dec.zig:25-54 + src/builtins/dev_wrappers.zig:2140-2159 + src/builtins/compiler_rt_128.zig:609-623: Integer-to-decimal-text does two full 128-bit divisions per digit (and three copies of the routine exist)
- category E; runs per `Num.to_str` / `Dec.to_str` / interpolation of any integer (every width: `roc_builtins_int_to_str` at dev_wrappers.zig:2184-2214 widens i8..u64 to i128 …; impact medium (integer formatting is ubiquitous in output and …; risk low
- Finding: each digit loop does `rem_u128(v, 10)` and then separately `divTrunc_u128(v, 10)`, so every digit costs two `udivmod(u128)` calls (each several hardware divides plus branches), up to 39 digits. `printI128Decimal` (dec.zig), `u128ToStr` (dev_wrappers.zig) and `u128_to_str_inner` (compiler_rt_128.zig) are the same routine written three times. For widths <= 64 the value is forced through 128-bit arithmetic needlessly.
- Fix: one shared routine that (a) for `v < 2^64` uses native `u64` `/ 10` and `% 10` (the compiler fuses them into one `div`), and (b) for larger values splits once with `udivmod(v, 10^19)` into two u64 halves and formats each half natively; alternatively the two-digits-at-a-time lookup table. Delete the two duplicate copies and have `roc_builtins_int_to_str` dispatch on width to the u64 path. Output bytes are identical, …
- Status: open

### src/builtins/hash.zig:207-216: `hasherFeed` runs a buffered `Wyhash` with three `update` calls for every scalar write
- category H; runs per `Hasher.write_*` call, i.e. per field of every hashed value (every `Dict` insert/lookup hashes each key field this way); impact low-medium (per-field constant factor in every Dict/Set …; risk low
- Finding: `hasherFeed` constructs `Wyhash.init(seed)` and calls `update` three times (1-byte domain, 8-byte length, the payload) then `final`. Each `update` goes through the buffered-state path (copy into the 48-byte internal buffer, length bookkeeping); `final` then processes the buffer. For scalar writes the total input is 10-25 bytes, so the three updates and the buffer copy dominate the actual mixing. Since the length is …
- Fix: in `hasherFeed`, if `bytes.len <= 16` (all scalar callers), build `[1+8+16]u8` on the stack and return `Wyhash.hash(seed, buf[0..9+bytes.len])`; keep the buffered path for `hasher_write_bytes` with long inputs. The produced hash is the same function of the same byte sequence, so results are bit-identical (both paths are the Wyhash of the concatenation).
- Status: open

### src/builtins/crypto.zig:142-148,266-272: Streaming hashers serialize into a fresh list allocation on every `write`
- category A; runs per `Sha256.Hasher.write` / `Blake3.Hasher.write` call; impact low-medium (only hashing loops; large per-call constant); risk low
- Finding: `sha256HasherWrite` deserializes the 106-byte state list, updates, and `serializeSha256` returns `RocList.fromSlice` which allocates a new 106-byte list; the caller then decrefs the input state list. For BLAKE3 the state is up to `blake3_state_base_len + 55*32` = 1.8 KB copied and reallocated per write. A loop that writes small chunks pays an alloc+free and a full state copy per chunk, which dwarfs the actual …
- Fix: thread the caller's update mode / uniqueness through the wrapper (the list wrappers already take `update_mode`), and when the state list is unique and has the required capacity, deserialize from and serialize back into the same allocation in place, returning the same `RocList`. Observable bytes are unchanged (same serialized format).
- Status: open

### src/builtins/sort.zig:71-95: Indirect sort path allocates two full-size scratch buffers and copies every element twice
- category A/D; runs per `List.sort_with` on elements wider than `MAX_ELEMENT_BUFFER_SIZE`; impact low (only wide elements; but saves one allocation and one …; risk low
- Finding: for wide elements the sort builds a pointer array (`alloc(len*8)`), sorts the pointers, then allocates a second `collect_ptr` buffer of `len*element_width`, copies every element into it in sorted order, and finally `@memcpy`s the whole buffer back over the original array: two allocations and two full copies of the element data. The pointer array alone determines the permutation, so the elements can be permuted in …
- Fix: after sorting `arr_ptr`, convert pointers to source indices (`(ptr - array) / element_width`) and apply the permutation in place with the standard cycle walk using one `element_width` stack/heap scratch; or at minimum drop the final `@memcpy` by swapping which buffer becomes the list's backing allocation (requires the list allocation header, so cycle-following is simpler).
- Status: open

### src/builtins/num.zig:134-154 + src/builtins/decimal_parse.zig: Integer prefix parsing scans the token twice
- category E; runs per `Num.from_str_prefix` / `from_utf8_prefix` call; impact low; risk low
- Finding: `parseIntPrefix` first calls `intPrefixLen` (a full scan of the accepted token with `decimal_parse.prefixLen` or `radixIntPrefixLen`), then `parseIntToken` which calls `decimal_parse.parseInt` / `parseIntNoFmt`, each of which re-scans the same bytes to accumulate the value. The scanner in `decimal_parse.zig` already produces the token end as part of accumulating; a single pass can return both the consumed length and …
- Fix: a `scanAndParse` entry in `decimal_parse.zig` that returns `{ consumed, value: ?T }` from one pass (the grammar check and accumulation are the same loop), used by `parseIntPrefix`. Results are identical since both passes accept exactly the same token.
- Status: open

### src/builtins/numeric_conversions.zig:19-26: `powerOfTwo` multiplies in a loop up to 128 times per float-range check
- category E; runs per `truncatedFloatFitsTarget` call (every checked float→int conversion); impact low; risk low
- Finding: `powerOfTwo` computes 2^exponent by `while (i < exponent) result *= 2.0`, up to 127 iterations, called for every float-to-integer range check.
- Fix: construct the power directly from the IEEE exponent bits (`@bitCast` of `(exponent + bias) << mantissa_bits`) or `std.math.ldexp(1.0, exponent)`; both are exact for the exponent ranges used, so results are identical.
- Status: open

### src/builtins/dev_wrappers.zig:820-848 + src/builtins/list.zig:22-24: Element incref/decref callbacks go through a context trampoline, two indirect calls per element
- category H; runs per element, for every list operation on refcounted elements (concat, drop, reverse, release_excess_capacity, decref of a unique list, ...); impact low-medium (per-element constant factor on RC-heavy list …; risk low-medium (touches the builtin/backend …
- Finding: the dev wrappers receive a bare `RcIncFn`/`RcDropFn` from generated code, wrap it in a stack `CallbackElement*Context`, and pass `callbackListElementIncref`/`Decref` as the `list.Inc`/`list.Dec` callback with the context pointing at the struct. Each element visit is then an indirect call to the trampoline, which loads the real pointer out of the context and makes a second indirect call. The context carries nothing …
- Fix: store the generated helper's function pointer directly in the `?*anyopaque` context slot (`@ptrCast(@constCast(callback))`) and have a single comptime-known adapter per ABI shape, saving the load; or, better, make `list.Inc`/`list.Dec` accept the `rc_callback_abi` signatures directly so the builtin calls the generated helper with one indirect call (the only non-generated user is `strListElementDecref`, which can be …
- Status: open

### src/builtins/dev_wrappers.zig:2592-2605: `list_str_eq` passes both `RocStr` values by copy to `strEqual`
- category D; runs per element of every `List(Str)` equality; impact low; risk low
- Finding: `strEqual(a_str.*, b_str.*)` is a `callconv(.c)` function taking two 24-byte `RocStr` by value, so each element comparison copies 48 bytes to the stack before comparing lengths; a `std.mem.eql` on `asSlice()` of the two pointers compares in place.
- Fix: compare `a_str.len() != b_str.len()` then `std.mem.eql(u8, a_str.asSlice(), b_str.asSlice())` directly through the pointers (same observable result as `strEqual`).
- Status: open

### src/builtins/simd.zig:145-155,226-240: Oracle SIMD evaluation re-extracts and re-inserts every lane with 128-bit shifts
- category H; runs per SIMD low-level op in the interpreter (`src/eval/interpreter.zig:8257`), compile-time evaluation (`src/postcheck/lambda_mono/eval.zig:2391`), and the dev …; impact low (only hot when a Roc program runs SIMD code under the …; risk low
- Finding: `mapBinary` loops over up to 16 lanes, and for each lane does two `getLane` (128-bit variable shift + mask) and one `withLane` (two 128-bit shifts, mask, and/or), so a single `add_wrap` on `u8x16` costs ~80 128-bit shift/mask operations. Zig's `@Vector(16, u8)` arithmetic (`+%`, `-%`, `@min`, `@max`, saturating `+|`) has exactly the bit-exact wrap/saturate semantics the oracle defines and compiles to one instruction …
- Fix: for the lane-wise ops (`add_wrap`, `sub_wrap`, `add_sat`, `sub_sat`, `mul_wrap`, `min`, `max`, `eq/gt/gte_lanes`, shifts, `neg/abs`), `@bitCast` the `u128` to the `@Vector` of the kind and use vector operators, keeping the current scalar code as the test oracle for the differential tests that already exist in this file. Output bits are pinned by those tests.
- Status: open

### src/builtins/list.zig:1812-1822,1853-1862,1878-1889: `List.concat` increfs every element of a consumed list and then decrefs them all again when that list was unique
- category H; runs per element of the consumed operand, per `List.concat`; impact medium (concat of refcounted elements is common in …; risk low-medium (must keep seamless-slice …
- Finding: in every `listConcat` branch the elements copied out of the operand that is *not* kept are increfed one by one (`while (i < list_b.len()) inc(...)`), and then that operand is `decref`ed. When the operand is unique (the common case for `a = List.concat(a, b)` with a fresh `b`), `RocList.decref` finds refcount 1 and runs `dec` on every element before freeing, so each element gets +1 then −1: two indirect calls and two …
- Fix: if `list_b.isUnique(roc_ops)` (and not a seamless slice, or a unique slice whose out-of-window elements are handled by `decrefAfterMovingSliceElements`), move the bytes and free the operand's allocation with `utils.freeRcPtrC`-style teardown that skips the element loop; only run the `inc` loop when the operand is shared. Same change for the `can_consume_b` branch's `list_a` and the fresh-allocation branch's two …
- Status: open


## Post-check: Monotype, Lambda Solved, Lambda Mono, SpecConstr, Boxy lowering
### src/postcheck (68 sites): `AutoHashMapUnmanaged(<...Id>, ...)` dodges the dense-Id lint
- category B; runs 68 declarations across postcheck, most of them per-procedure or per-specialization scratch maps (NodeId, TypeId, TypeRepId, LocalId, ExprId, TermId, …; impact medium (many are on hot per-node paths; see individual …; risk low
- Finding: `ci/zig_lints.zig:508` searches for the literal `"HashMap("`, so `std.AutoHashMapUnmanaged(NodeId, usize)` (lower.zig:19428), `AutoHashMapUnmanaged(checked.CheckedExprId, DirectCallRequest)` (lower.zig:19476), `AutoHashMapUnmanaged(TypeRepId, ...)` (boxy/plan.zig x20), `AutoHashMapUnmanaged(LIR.LocalId, ...)` (boxy/lower.zig:15617, 34468, 34593), `AutoHashMapUnmanaged(Ast.LocalId, std.ArrayList(...))` …
- Fix: convert each to `collections.DenseMap(K, V)` (same `get/put/getOrPut/remove/iterator` surface) or to a column on the owning store; for the `void`-valued sets use `collections.ScopedBitSet`/`DynamicBitSetUnmanaged` with an epoch. Also extend the lint to match `HashMapUnmanaged(` so the contract is enforced. Semantics are unchanged as long as iteration order is not observable (check each `iterator()` consumer; …
- Status: fixed (fix 57)

### src/collections/VersionedMap.zig:17-30 (used by every BodyContext via BinderMap/TypedBinders/LocalProcContexts): 72-byte change records and usize slots for u32 payloads
- category C; runs one `Change` per `BinderMap.put`/`restore` (per pattern binder bound, per child context fork), one `Slot` per distinct key; impact medium; risk low
- Finding: `Change = { parent: ?usize, depth: usize, slot: usize, position: usize, before: ?V, after: ?V, redo_next: ?usize }` is 16+8+8+8+8+8+16 = 72 bytes for `V = DraftLocalId`, and `Slot = { key, value: ?V, position: usize }` is 24 bytes for a u32 key. The version chain is walked by `activate` on every `get`, so the record size directly affects cache behaviour of binder lookups, which happen for every checked binder use in …
- Fix: store `parent`, `slot`, `position`, `redo_next` as `u32` with `maxInt(u32)` sentinel (counts are bounded by the number of changes, which is a program-size count so u32 is the allowed floor), `depth: u32`, and `before/after` as `V` plus a 2-bit presence mask: Change shrinks to ~28-32 bytes, Slot to 12. Nothing observable changes; `replayed_changes` counter semantics stay.
- Status: open

### src/postcheck/monotype/lower.zig:22267-22300: `nestedRelationBase` allocates and sorts every binder on every nested-context creation
- category A/E; runs per nested instantiation context (every nested lambda / local function / dispatch call context created by `nestedInstantiationContext`, called from 22 sites); …; impact medium; risk low
- Finding: `const entries = try self.binders.sortedEntries(self.allocator); defer self.allocator.free(entries);` (22267) materialises and sorts all live binders just to compare against the existing `relation_base.related` DenseMap and relate the new ones. `BinderMap.sortedEntries` (3090) allocates an `Entry` slice and `std.mem.sort`s it. Most calls find the base still valid and only need the delta.
- Fix: keep a monotonically increasing "binder generation" on `BinderMap` (bump on put/restore/remove) and store the generation the relation base was built from; when unchanged return `existing` without materialising anything. When a delta is needed, iterate `self.binders.iterator()` directly (VersionedMap iteration order is deterministic per version) and only sort if the relate order is semantically relevant (it is only …
- Status: open

### src/postcheck/monotype/lower.zig:22700-22730: `captureLexicalBinderEntries` allocates + sorts 44-byte entries per capture, 6 callers do it just to hash
- category A/D; runs per nested local-procedure declaration, per codec lexical context key (8096, 22439, 25670, 34201, 43441, 45299, 52285, 57920), per restored local proc scope; impact medium; risk low
- Finding: Every call allocates `count` x `LexicalBinderEntry` (u8 + u32 + u32 + [32]u8 = 44 bytes) by iterating two versioned maps, sorts with a comparator that does a 32-byte `std.mem.order` on the digest, then `codecLexicalContextKey` (22437-22455) immediately hashes the entries and frees them. `localProcContextsDigest` (22546-22570) does the same with `DraftLocalProcAddress` (40 bytes) and then re-looks-up every address …
- Fix: give `BodyContext` a reusable scratch `ArrayListUnmanaged(LexicalBinderEntry)` and `ArrayListUnmanaged(struct{addr, ctx_id})`; iterate `local_proc_contexts.iterator()` (key+value) so the second lookup disappears; for the digest-only callers hash directly from the sorted scratch without the `dupe`. Sorting is required for digest stability so keep it, but entries only need ordering by (kind, binder) for the …
- Status: open

### src/postcheck/monotype/lower.zig:20466-20516: `BodyContext.initBodyState` builds 5 managed `AutoHashMap`s + 7 DenseMaps per context; contexts are created per call site / per match branch
- category A; runs `body_contexts_created` counter: per specialization body, per nested lambda, per direct-call expansion (`call_ctx` 27455), per match branch context …; impact medium; risk low
- Finding: `inspect_defs`, `equality_defs`, `hash_defs`, `encoder_defs`, `parser_defs` are five separate `std.AutoHashMap(...Address, DraftGeneratedHelperDefEntry)` initialised per context; `childContextWithTypeCells` (22344-22386) then immediately `deinit`s and re-forks `binders`, `typed_binders`, `local_proc_contexts`, and `instantiation.node_map` (allocated in `initBodyState` then thrown away), and `appendSlice`s …
- Fix: (1) merge the five helper-def maps into one map keyed by `(kind, address)` or a tagged key, or move them to the shared `BodyDraftStore` (they are keyed by sealed addresses and the draft already owns the def tables). (2) Add a `childFrom(parent)` constructor that forks parent maps directly instead of init-then-deinit-then-fork. (3) Allocate contexts in place (`create` + init into pointer) rather than returning by …
- Status: open

### src/postcheck/monotype/lower.zig:14654-14770: `DraftExprData` is 16-byte aligned because of `dec_lit: builtins.dec.RocDec` (extern i128)
- category C; runs one `DraftExpr` per lowered expression per specialization (the largest draft table); impact medium (25% less memory and cache traffic in the …; risk low
- Finding: `DraftExprData = union(enum(u8))` has `dec_lit: RocDec` (extern struct with `i128`, alignment 16) while every other payload is 4/8-aligned; largest ordinary payload is `DraftTryRecordSequence` (~36 bytes) / `DraftInitializedPayloadSwitch` (32, contains `u64` mask). Alignment 16 rounds the union to 48 bytes and `DraftExpr = {ty: DraftTypeCell(8), data}` to 64 bytes; with 8-byte alignment it would be 40 + 8 = 48. …
- Fix: store `dec_lit` as `[16]u8` (or `extern struct { bytes: [16]u8 }`) in the draft/final expression union and convert with `@bitCast` at the single consumer; alternatively keep a `dec_lits: ArrayList(RocDec)` side table and store a u32 index. Pure representation change; verify `@sizeOf(DraftExpr)` drops to 48 and the snapshot/eval tests pass.
- Status: open

### src/postcheck/monotype/lower.zig:16591-16593, 16600: `expr_impossibility_proofs`/`pat_impossibility_proofs`/`stmt_impossibility_proofs: ArrayList(?RuntimeImpossibilityProofId)`
- category C; runs one slot per draft expr/pat/stmt; impact low; risk low
- Finding: `?enum(u32)` is 8 bytes; a sentinel `RuntimeImpossibilityProofId.none = maxInt(u32)` halves these three columns.
- Fix: add a `none` sentinel to `RuntimeImpossibilityProofId` and compare against it. No semantic change.
- Status: open

### src/postcheck/monotype/lower.zig:16558: `template_specs_by_template: AutoHashMap(names.ProcTemplate, std.ArrayList(u32))`
- category F; runs per draft store; one heap ArrayList per template that has specs in the body; impact low; risk low
- Finding: a map whose values are independently allocated `ArrayList(u32)`s. Each template gets its own growable buffer with its own allocator traffic and deinit loop.
- Fix: replace with `AutoHashMap(ProcTemplate, u32 head)` + an intrusive `next: u32` column on `template_specs` (singly linked by index, newest first), or a `MultiArrayList` of `(template, spec_index)` sorted lazily. Verify the consumers only need membership/iteration over a template's specs (grep `template_specs_by_template`).
- Status: open

### src/postcheck/monotype/lower.zig:24927, 24949, 25374, 25391, 50510, 50614, 50710, 1174, 4831: `std.heap.ArenaAllocator.init` per call
- category A; runs `importInterfaceSummary`/`insertInterfaceSummary` run per interface-summary cache hit/insert (per template request); `valuePositionMask` (1174) runs per …; impact medium; risk low
- Finding: each site creates a fresh `std.heap.ArenaAllocator` (std's arena does atomic ops on each alloc and its first allocation always goes to the backing allocator) and `deinit`s it on exit, so every call pays at least one backing alloc + free even for tiny copies. `base.SingleThreadArena` exists for exactly this.
- Fix: keep one `SingleThreadArena` scratch per `Builder`/`SpecJobWorkspace` (`scratch.reset(.retain_capacity)` at call entry), as 4831 already does inside its loop. Verify nothing stores a slice from the scratch past the call (the summary copies at 24940/24962 are passed to `insert`, which must copy again; check `InterfaceSummaryCache.insert` copies into its own `evidence_arena` - it has one at 19111).
- Status: partly fixed (fix 41): the seven body-context scratch arenas come from the graph's arena pool; the two builder-level arenas remain

### src/postcheck/monotype/lower.zig:2697-2711: `specEvidenceEql` allocates a worklist ArrayList for a one-pair comparison
- category A; runs 13 call sites of `specEvidenceEql`, 10 of `specEvidenceVectorEql`; the hot ones are inside candidate scans: 49556-49558 (per cached vector x per element), 8203 …; impact medium; risk low
- Finding: `specEvidenceEql(allocator, a, b)` wraps into `specEvidenceVectorEql(&.{a}, &.{b})`, which creates `var pending = std.ArrayList(SpecEvidencePair).empty` and `append`s every pair (heap allocation on the first append; `SpecEvidencePair` is two `SpecEvidence` unions, each roughly 80-100 bytes because `.structural` carries a `StructuralDerivation`, an optional `ModuleView`+`StructuralEvidence`, and a slice). For leaf …
- Fix: compare the top-level pair shallowly first and only create the worklist when `specEvidenceShallowEql` queues a nested pair (pass a `?*ArrayList` and allocate lazily), or thread a `BodyContext`/`Builder`-owned scratch `ArrayListUnmanaged(SpecEvidencePair)` (clearRetainingCapacity per call). Identical results; only allocation behaviour changes.
- Status: fixed (fix 41)

### src/postcheck/monotype/lower.zig:8195-8213: linear scan over every draft spec of a template with per-candidate evidence/type equality
- category E; runs per template request whose request type resolved (every direct call to a template inside a specialization body); impact medium; risk low
- Finding: `template_specs_by_template.get(template_ref)` yields every spec created so far for that template in this draft, and for each the code runs `specEvidenceVectorEql` (allocates, see above), `optionalTypeDigestEql`, `draftTemplateSpecVisibleFrom`, `draftCodecContractSpecializationEql`, `specializationLookupTypeForNode` and finally `typeEql` on the full function type. A template with k specializations in one body costs …
- Fix: key the candidate scan by the request type's specialization digest (`specializationTypeDigestIn`) so only candidates with equal digest are compared; or order comparisons cheapest-first (`lexical_context_key` and `codec_contract` byte compares before the allocating evidence compare). Verify the same spec is selected (selection must be unique; `selection.add` already asserts).
- Status: open

### src/postcheck/monotype/lower.zig:17694-17714: draft reset `deinit`s and re-`init`s its lookup maps, discarding capacity per spec job
- category A/E; runs per ordinary specialization job (every reuse of a `BodyDraftStore` by the queued spec-job driver); impact medium; risk low
- Finding: `template_spec_lookup`, `nested_spec_lookup`, `nested_spec_families`, `template_specs_by_template` (and each of its value ArrayLists), `template_spec_by_fn`, and `closed_direct_specializations` are all `deinit`ed and re-created with `.init(self.allocator)`, so every job regrows every table from zero capacity. The preceding lines (17680-17692) do the same for the proof columns (`.deinit` then `= .empty`).
- Fix: `clearRetainingCapacity()` on each (DenseMap and AutoHashMap both support it; for the map-of-ArrayLists either clear each value list in place and keep them, or replace the structure per the entry below). Verify `assertGraphStateDiscarded` still sees `count() == 0`.
- Status: fixed (fix 10)

### src/postcheck/monotype/lower.zig:49546-49566: `resolved_callable_vectors`: map of ArrayLists with linear candidate scan
- category E/F; runs per callable evidence resolution with replacements (per dispatch edge through a callable parameter); impact low-medium; risk low
- Finding: key is a Wyhash of module bytes + pointer; value is a heap `ArrayList([]const SpecEvidence)`; every lookup scans all cached vectors for that key and compares element-wise with `specEvidenceEql` (allocating per element).
- Fix: hash the resolved vector content into the key (module, params ptr, plus a digest of the replacement entries) so the lookup is O(1) and comparison happens once; store `[]const SpecEvidence` directly as the value. The equality check must stay for collision safety but only on one candidate.
- Status: open

### src/postcheck/monotype/lower.zig (64 sites), spec_constr.zig (47), solved_lir_lower.zig (26): `GuardedList.dupe(self.allocator, T, self.typeStore().span(...))` + `defer free` to iterate a type-store span
- category D/A; runs per type-shape walk step (per tuple/tag-union/record visited while lowering codecs, structural eq/hash, inspect, pattern lowering, SpecConstr shape analysis); …; impact medium (hundreds of thousands of small alloc/free pairs on …; risk low for (1), medium for (3)
- Finding: Each site copies the span out of the type store into a fresh heap slice solely because the loop body may append to the store (which could reallocate the contiguous span buffer). In many of the sites (e.g. 20143, 37202, 37391, 37571, 39225, 53573) the loop body only `append`s to a local `pending` list and never touches the type store, so the copy is pure waste; in the others only a few calls can allocate types.
- Fix: (1) for loops that do not mutate the store, iterate by index through the guarded borrow (`for (0..span.len) |i| self.typeStore().span(span)[i]`) or take the slice directly; (2) for the rest, push onto the worklist by `(span.start + i)` positions and re-read `span()` per element; (3) for Type.Store spans in general, consider a segmented (chunked, address-stable) span buffer so borrowed spans are never invalidated by …
- Status: open

### src/postcheck/monotype/lower.zig:33938-33951 and 16240-16250: impossibility-proof evaluation allocates two columns over every proof in the draft
- category E/A; runs `activeImpossibilityProofHolds` per deferred inspect with an uninhabited value node; `FrozenRuntimeImpossibilityProofEvaluator.init` per body seal (11870, …; impact low-medium; risk low
- Finding: `memo = alloc(?bool, impossibility_proofs.len)` + `active = alloc(bool, len)` + two `@memset`s, sized by the whole draft's proof table, to evaluate one proof. `?bool` is 2 bytes; the pair could be a single `u8` state column, and it could be epoch-stamped and retained on the BodyContext.
- Fix: keep one `ArrayListUnmanaged(u8)` scratch on the BodyContext/Builder grown to `proofs.len` lazily, with a `u32` epoch array or a touched-list to reset only visited entries; merge `memo`/`active` into one enum(u8) {unseen, active, proven, disproven}. Same answers.
- Status: open

### src/postcheck/monotype/lower.zig:33963: `inhabitation_entered.clearRetainingCapacity()` on an AutoHashMap per uninhabited scan
- category E/B; runs per `nodeIsProvenUninhabited` call (13 callers of `typeIsProvenUninhabited`, plus per produced runtime value), i.e. per lowered expression whose type is …; impact low-medium; risk low
- Finding: `AutoHashMapUnmanaged(NodeId, usize)` clear is proportional to the map's capacity, which stays at the high-water mark of the largest scan. `inhabitation_visiting` is already a bitset; `inhabitation_entered` is a dense-id keyed map (NodeId) that dodges the lint (see cross-cutting entry).
- Fix: replace with `collections.DenseMap(NodeId, u32)` (clear is proportional to live entries) or an epoch-stamped column on the graph (`entered_epoch[node] == scan_epoch`), and store the count as `u32`.
- Status: open

### src/postcheck/monotype/lower.zig (methodTargetContext, 6 callers; e.g. 50705-50707): a full `BodyContext` is built and torn down to instantiate one node
- category A; runs per method-contract relation / per dispatch target instantiation; impact medium; risk medium
- Finding: `var target_ctx = try self.methodTargetContext(lookup); defer target_ctx.deinit(); ... try target_ctx.instNode(lookup.target.callable_ty)` constructs the whole BodyContext (five AutoHashMaps, DenseMaps, versioned maps, TypeInstantiationContext) for a single checked-type instantiation. The comment on `TypeInstantiationContext` (19320) says it exists precisely so "operations that only need a fresh type instantiation …
- Fix: route these sites through a `TypeInstantiationContext`-only instantiation (a `Builder.instNodeIn(view, graph, ty)` helper), or pool BodyContexts on the Builder and reuse (clear) rather than init/deinit. Verify instantiation identity semantics (each needs a fresh `InstantiationScopeId`, which `allocateInstantiationScope` gives without a BodyContext).
- Status: open

### src/postcheck/monotype/lower.zig:46787-46790, 63808-63812: `drain*` returns an `allocator.dupe` of a tail that is immediately consumed
- category D; runs per match branch (pattern literal guards and optional destruct binds are drained for each branch body/guard); impact low; risk low
- Finding: `drainPatternLiteralGuards` and `drainOptionalDestructBinds` `dupe` the tail of a BodyContext-owned list and shrink it, returning an owned slice the caller frees after wrapping the branch body. The tail is at a stable position until the next push.
- Fix: return the slice `items[start..]` and have the caller (which lowers the guards before anything else pushes) call `shrinkRetainingCapacity(start)` after use; or pass a callback. Verify that the lowering of the drained guards (`lowerPatternLiteralEq`) cannot push more guards; if it can, keep a two-stack approach with a reusable swap buffer rather than a dupe.
- Status: open

### src/postcheck/monotype/lower.zig:66535-66538 (CheckedTypeAddress), 4147 type_cache, 4197/4200 contains_error/representation_fixed, 3439 workspace checked_type_cache: 36-byte hashed keys for per-checked-type memos
- category B; runs per checked-type instantiation (type_cache lookup on every `instNode` of a checked type), per `checkedTypeContainsError`/`checkedTypeRepresentationFixed` query; impact medium; risk low
- Finding: `CheckedTypeAddress = { module_bytes: [32]u8, type_id: u32 }`; every probe hashes 36 bytes and the tables store 40-byte keys plus values. `Builder.lowering_module_ids` already assigns a dense `LoweringModuleId` to every module in the lowering input, so the pair `(LoweringModuleId, CheckedTypeId)` identifies the same thing in 8 bytes, and per-module `DenseMap(CheckedTypeId, V)` columns (one per lowering module) make …
- Fix: replace with a per-module column vector `[]DenseMap(CheckedTypeId, Type.TypeId)` indexed by `LoweringModuleId`, falling back to an 8-byte key map where the module id is unknown. Same results; iteration order of these maps is not observable (verify no `iterator()` consumer).
- Status: open

### src/postcheck/monotype/lower.zig:31174-31208: `runLower` allocates a fresh `ArrayList(LowerFrame)` per entry; `lowerExpr`/`lowerExprAtTypeCell`/`lowerDispatchOperandAtType` re-enter it from inside tasks
- category A/G; runs per re-entrant lowering call: `lowerExpr` (33662), `lowerExprAtTypeCell` (46202, 7 callers), `lowerDispatchOperandAtType` (46128, 8 callers), …; impact medium; risk low
- Finding: `var frames: std.ArrayList(LowerFrame) = .empty; defer frames.deinit(...)`: `LowerFrame` is `{ctx ptr, cursor u8, task: LowerTask}` and `LowerTask` is a union whose largest inline members (`MatchTask` ~150 B with an embedded `ArrayListUnmanaged`, `DispatchLowerTask` ~140 B with an embedded `ArrayList`, `CallLowerTask`, `BlockTask` already boxed) make each frame on the order of 200-300 bytes; every `runLower` …
- Fix: keep one `frames` stack on the `BodyContext` (or the Builder, since nested contexts share the allocator) and run each `runLower` on top of it from a base index (`frames.items.len` at entry; pop back to it on exit/errdefer). Shrink the union by boxing `MatchTask`/`DispatchLowerTask` like `BlockTask`/`NestedFnTask`, or by moving their growable members (`pending`, `pre_lowered`) into context-owned scratch stacks …
- Status: fixed (fix 11)

### src/postcheck/monotype/lower.zig:31162-31166 (boxLowerTask, 20 sites), 23642-23646 (boxInstTask, 5 sites): per-node heap boxes for block/nested-fn/hosted-evidence/loop tasks
- category F; runs per block expression, per nested function value, per loop, per nominal instantiation (`InstNominalTask`, `InstDeclBackingTask`, `InstDeclaredOrderTask`); impact low-medium; risk low
- Finding: `allocator.create(T)` + `destroy` for every task of those kinds; these are the most common structural nodes (every `let` chain is a block). The boxes exist to keep the frame union small.
- Fix: a per-BodyContext free-list/slab for each boxed task type (`std.heap.MemoryPool(BlockTask)` or a hand-rolled `ArrayList(T)` + free index list), or store the task in a `SegmentedList` side stack indexed by `u32` so frames hold indices. Pure allocation-strategy change.
- Status: open

### src/postcheck/monotype/lower.zig:28722-28740 + 28860 (`saveSourceLocation`): line/column computed for every lowered expression and stored beside the region
- category D/C; runs per lowered expression (the `at_type_cell`/`expr_inner`/`with_type` tasks call `saveSourceLocation`, and every draft expr gets an `expr_locs` entry plus an …; impact low-medium; risk low
- Finding: `sourceLocFor` binary-searches `line_starts` for the region start and `columnIdx`s it, per expression, and the result (`SourceLoc` = file + line + column, 12 B) is stored in a column parallel to `expr_regions` (`Region`, 8 B) from which it is fully derivable given `source_file_id`. The builder also keeps `current_loc`/`current_region` and `SavedSourceLocation` (20 B) inside three task variants to restore them.
- Fix: store only `(file, region)` per expression and compute line/column lazily at the consumer (LIR debug info / diagnostics), or at least compute it once per distinct region start (regions repeat heavily across nested expressions). Verify the consumers of `Ast.Program.expr_locs` accept a lazily computed value (they get identical numbers).
- Status: open

### src/postcheck/monotype/ast.zig:861-875 (`Local`), lower.zig:14401-14408 (`DraftLocal`): three `?u32` optionals per local
- category C; runs one row per local per specialization; impact low; risk low
- Finding: `binder: ?PatternBinderId`, `capture_id: ?CaptureId`, `checked_capture_id: ?CaptureId` are 8 bytes each (24 B of a ~40 B row). `Common.Symbol` is `enum(u32)`.
- Fix: give the three id types a `none = maxInt(u32)` sentinel (or store a 3-bit presence mask in one `u8`), bringing `Local` to 20-24 B. No semantic change.
- Status: open

### src/postcheck/monotype/lower.zig:19109-19157 and 19177-19199: `InterfaceSummaryCache.buckets`/`InterfaceReplayState.buckets`: `AutoHashMap(InterfaceReplayAddress, ArrayList(u32))` with ~170-byte keys
- category B/F; runs per template request / per interface replay lookup (every direct call to a template with open interface); impact low-medium; risk low
- Finding: `InterfaceReplayAddress` = kind + `DraftTemplateFamilyAddress` (three `[32]u8` digests + two u32) + `evidence_digest [32]u8` + `input_digest [32]u8`; every probe hashes ~170 bytes, each bucket is a separately heap-allocated `ArrayList(u32)`, and the same key is hashed twice when `insert` follows a failed lookup. `DraftSpecLookup` in the same file (15233+) already shows the better shape: `Candidates { first: u32, …
- Fix: intern `InterfaceReplayAddress` once into a dense `AddressId` (one `AutoArrayHashMap(Address, void)` whose index is the id; the digest could be pre-hashed to a `u64` key with the full bytes kept for equality) and key both tables by that id with the `Candidates` first/overflow layout. Same lookups, no per-bucket heap list.
- Status: open

### src/postcheck/monotype/lower.zig:44331, 44347, 44368-44375, 59672-59674: per-ConstStore-restore `dupe` of one-element slices and dupe+sort of record fields
- category D/E; runs per restored constant value node (nominal/box: 1-element dupe; record: dupe + `std.mem.sort` by name rank), per record-rest restoration; impact low; risk low
- Finding: `frame.child_nodes = try self.allocator.dupe(NodeId, &.{backing.node})` allocates a 4-byte slice on the heap; the record paths copy `graph_fields`/`rest_fields` and sort them by `instRecordFieldLessThan` on every restore even though the graph's record field order for a given node is fixed once the node is built.
- Fix: give `frame` an inline `single_child_node: NodeId` for the one-element cases; have `InstGraph` keep record fields sorted by name rank at construction (or cache the sorted order per record node) so consumers never re-sort. Verify order-dependent consumers (LIR const plan) still see the lexicographic order.
- Status: open

### src/postcheck/monotype/lower.zig:4258 (`Builder.evidence_arena`), 19111 (`InterfaceSummaryCache.evidence_arena`): `std.heap.ArenaAllocator` for the hottest long-lived arena
- category A; runs every materialised `SpecEvidence`, every stored evidence conversion, every interface summary copy; impact low-medium; risk low
- Finding: both are `std.heap.ArenaAllocator`, which the brief notes pays atomics per allocation; `base.SingleThreadArena` exists. The graph arena (`graph.arena()`) should be checked the same way in solve.zig.
- Fix: switch to `base.SingleThreadArena`; identical semantics as long as the Builder is single-threaded per worker (it is: workers own their builders).
- Status: open

### src/postcheck/monotype/lower.zig:58717-58745: `patternNeedsExplicitBinding` allocates two worklists per unmemoised pattern root
- category A; runs per match-branch pattern root (memoised per pattern afterwards); impact low; risk low
- Finding: `pending`/`children` are fresh `ArrayListUnmanaged`s on every call; the memo map `explicit_binding_memo` is an `AutoHashMapUnmanaged(CheckedPatternId, bool)` (dense id key, lint-dodging).
- Fix: reuse BodyContext scratch lists; memo as `DenseMap(CheckedPatternId, bool)` or a 2-bit column.
- Status: open

### src/postcheck/monotype/solve.zig:247-281: `InstNode` carries a 32-byte `erased: names.TypeDigest` inline, inflating every graph node
- category C; runs one `InstNode` per graph node (`nodes: ArrayList(InstNode)`), tens of thousands per specialization; also embedded in every `InterfaceConstraints.OpenNode` …; impact medium (20% smaller node column, better `find` locality); risk low
- Finding: payload sizes: `tag_union {tags: []InstTag(16), ext(4), tags_sorted(1)}` ~24, `record` ~20, `func` ~20, `leaf` ~12, `unresolved: InstVariable` (u8 + two optionals) ~8-12, `named: *const InstNamed` 8 (already boxed "because a named payload is an order of magnitude larger"), but `erased: names.TypeDigest` is 32 bytes, so the union is 32 + tag -> 40 bytes instead of 24 + tag -> 32 (or 28 if `tags_sorted` moved to a bit …
- Fix: store erased digests out of line (`erased: *const names.TypeDigest` in the graph arena, like `named`), or intern them into a graph-owned `ArrayList(TypeDigest)` and hold a `u32`. Verify `instNodeEql` (9150) compares bytes through the pointer.
- Status: fixed (fix 46)

### src/postcheck/monotype/solve.zig:3781-3798: union-find `find` walks and rewrites 40-byte `InstNode` entries
- category C/H; runs per `find` (every graph read, unify, sameClass, nominal-backing hash/eql at 1873-1891 does one per argument); impact medium; risk medium (touches every …
- Finding: the redirect edge is the `.redirect` variant of the node union, so each hop loads a 40-byte element (one cache line per hop) and path compression overwrites whole union values; `countDiagnostic("union_find_resolutions")` runs a branch per call.
- Fix: keep a dedicated `parent: ArrayList(NodeId)` column (4 bytes/entry; self-parent = root) and leave `.redirect` out of `InstNode` entirely (the `versions`/`class_member_*` columns already model per-node side data). Same class semantics; the only consumers of `.redirect` are `find`-like walks (check `instNodeEql` and the capture writer which must never see a redirect).
- Status: open

### src/postcheck/monotype/solve.zig:3545-3570 (`newNodeUnchecked`): eleven parallel columns, each with its own `ensureUnusedCapacity`, and optional/bool columns stored wide
- category C/G; runs per graph node creation; impact low-medium; risk low
- Finding: `versions: u32`, `class_member_next: ?NodeId` (8 B), `class_member_head/tail: NodeId`, `containment_visit_epochs: u32`, `request_source_interfaces: ?NodeId` (8 B), `constructor_evidence_requests: bool`, `private_backing_roots: bool`, `representation_membership: packed{2 bits}` (1 B), `class_finished_monos: ?Type.TypeId` (8 B). Eleven `ensureUnusedCapacity(1)` calls (eleven branches + potential eleven reallocations) …
- Fix: a `std.MultiArrayList(NodeSide)` (one `ensureUnusedCapacity`, one append) with `?NodeId`/`?TypeId` as `maxInt` sentinels and the three flags folded into one `packed struct(u8)`; `nodes` itself can stay separate. `reset`/`assertPermanentNode` simplify accordingly.
- Status: open

### src/postcheck/monotype/solve.zig:4915-4921, 5000-5006, 6056-6061: fresh name-keyed hash maps per row relation although generation-stamped label columns already exist
- category B/A; runs per public/private tag-row pairing, per record-row pairing, per request-container join (each relation of two rows); impact medium (row relations are the dominant unify work for …; risk low
- Finding: `var private_positions: AutoHashMapUnmanaged(names.TagNameId, u32) = .empty; ... ensureTotalCapacity(...)` allocates and frees a hash table per relation and hashes dense `TagNameId`/`RecordFieldNameId`s. The graph already owns `row_label_right_generation`/`row_label_right_index`/`row_label_left_generation` + `row_label_generation` (2110-2116) for exactly this "pair labels without clearing" job.
- Fix: reuse the generation-stamped label columns (grow to `max(label id)+1` once, bump `row_label_generation` per relation) at these three sites. Same pairing result.
- Status: fixed (fix 12)

### src/postcheck/monotype/solve.zig:755-770 (`containsIterator`), 7831 (`TypeUninhabitedScan.visiting`), 636-642 (`EquivalenceRenumberer` maps): per-walk `AutoHashMapUnmanaged` keyed by dense `Type.TypeId`/`NodeId`
- category B/A; runs `containsIterator` per open node renumbered in `equivalenceIdentityInto`; the type-uninhabited scan per finished-type inhabitation query; renumberer maps per …; impact low-medium; risk low
- Finding: dense-id keyed hash sets built from `.empty` on each call (first `getOrPut` allocates), discarded at the end; the graph already has `type_set_pool`/`node_set_pool` (`DenseMapPool`) for pooled visited sets.
- Fix: acquire from `type_set_pool`/`node_set_pool`; for `visiting: AutoHashMapUnmanaged(Type.TypeId, usize)` use `DenseMap(Type.TypeId, u32)` from the pool. Semantics unchanged.
- Status: open

### src/postcheck/monotype/solve.zig:611-623 (`equivalenceIdentityInto`): per-call `std.heap.ArenaAllocator` + fresh `IdentityWriter` lists
- category A; runs per interface equivalence identity (per replay verification / summary comparison); impact low; risk low
- Finding: `std.heap.ArenaAllocator.init` per call plus `writer.bytes`/`writer.leaves` deinit'd per call, while `identityInto` (590-605) right above reuses `graph.identity_bytes_scratch`.
- Fix: `graph.acquireArena()`/`releaseArena` (3479-3494) for the renumberer and the same scratch-swap pattern as `identityInto` for the writer.
- Status: open

### src/postcheck/monotype/solve.zig:2021-2033, 2054-2066, 2110, 2166 (InstGraph), 1934-1948 (`ContainmentCacheEntry`): DenseMap values that are themselves `ArrayList`s
- category F; runs `node_snapshots: DenseMap(NodeId, ArrayList(Type.TypeId))` one list per snapshotted node; `leaf_alias_origins`, `nominal_backings_by_root`, …; impact medium; risk low
- Finding: each value list is a separate heap allocation with its own growth; reset frees them all and the next specialization reallocates.
- Fix: intrusive chains into one shared buffer (`first: u32` in the map value + `next: u32` in a shared `ArrayList(Entry)`), or a `spans` layout (`start/len` into a shared list that is cleared with `clearRetainingCapacity`). For `node_snapshots` a single `ArrayList(struct{node, ty})` with a per-node head suffices; for the containment dependency lists a shared buffer with `(start, len)` per query.
- Status: open

### src/postcheck/monotype/solve.zig:2002, 2131, 3479-3494: graph arenas are `std.heap.ArenaAllocator`
- category A; runs every row merge (`6895`, `6971`, `7093`, `7244` dupe merged rows into the arena), every `InstNamed`, every instantiated slice (`lower.zig` `graph.arena().dupe` …; impact low-medium; risk low
- Finding: the graph arena is the hottest allocator in specialization; std's arena pays atomics per allocation. `base.SingleThreadArena` exists for this.
- Fix: switch `arena_impl` and `spare_arenas` to `base.SingleThreadArena`; identical semantics (graph is single-threaded per worker).
- Status: open

### src/postcheck/monotype/solve.zig:283-291 (`PureInstanceKey`), 234-238 (`ClosedCheckedKey`): 72-byte / 36-byte hashed memo keys
- category B; runs per pure-instance record/lookup (`pure_instances`, per checked type instantiated under an evidence chain); per closed checked type (`closed_checked_monos`); impact low; risk low
- Finding: `PureInstanceKey = {module_bytes [32]u8, checked_ty u32, scheme_vars_ptr usize, subst_ptr usize, subst_len usize, parent usize}`; the module bytes and pointers could be dense ids (lowering module id from `Builder.lowering_module_ids`; evidence-frame id instead of three pointers).
- Fix: key by `(LoweringModuleId, CheckedTypeId, EvidenceFrameId)` = 12 bytes; identical lookups once frames carry a dense id.
- Status: open

### src/postcheck/monotype/solve.zig:320-330 (`InterfaceConstraints.OpenNode`): ~72-byte open node records with four optionals and four bools
- category C; runs per captured open node per interface capture; copied twice per summary insert (capture into scratch arena, then `.copy` into the cache arena at …; impact low; risk low
- Finding: `content: InstNode (40)`, `source: ?NodeId (8)`, three bools, `related_group: ?u32 (8)`, `private_backing: bool`, `finished: ?Type.TypeId (8)`.
- Fix: sentinel ids and a flags byte: ~52 bytes; capture directly into the destination arena when the entry is going to be inserted (the eql check at 19135 could run on the scratch copy first only when a bucket candidate exists).
- Status: open

### src/postcheck/monotype/type.zig:263-290: `MonoTypeNode.named` payload (~160 B) makes every stored type ~168 bytes
- category C; runs one `Content` per Monotype type (`types: StoreList(Content)`), copied by every `types.get(ty)` (returns by value), cloned by `cloneFrozen`, relocated per spec …; impact high (type store is read on every graph seal, digest, …; risk medium
- Finding: `named` holds `NamedType {module: [32]u8, ty: u32}` (36 B), `TypeDef {module u32, type_name u32, source_decl ?u32 (8), generated ?TypeDigest (33), iterator_representation/kind/depth (3), iterator_topology ?IteratorTopology (37)}` (~92 B), `kind`, `builtin_owner ?BuiltinOwner`, `args Span (8)`, `backing ?NamedBacking (8)`, `declared_order Span (8)`: about 160 B. `erased: names.TypeDigest` is 32 B. Every other variant …
- Fix: store the named payload out of line, as `InstGraph` already does with `named: *const InstNamed`: a `named_payloads: StoreList(NamedPayload)` column with `named: u32` index in the union, and move the rare `generated` digest and `iterator_topology` into optional side columns keyed by the payload index. The union drops to ~16 B (tag + `func`). `erased` becomes an index into a digest column. All consumers go through …
- Status: fixed (fix 45)

### src/postcheck/monotype/type.zig:300-304, 308-309: four `StoreList(?names.TypeDigest)` columns (33 B each) and two `StoreList(?bool)` columns per type
- category C; runs one slot per type per column; `cloneFrozen` and spec-job relocation copy them all; impact medium; risk low
- Finding: `type_digests`, `specialization_digests`, `equality_digests`, `representation_digests` are each `?[32]u8` = 33 bytes per type, 132 B/type of mostly-null memo; `iterator_interface_cache`/`generated_private_cache: StoreList(?bool)` are 2 B each where 2 bits suffice.
- Fix: a presence bitset per digest kind plus a dense `DenseMap(TypeId, TypeDigest)` (only types whose digest was requested occupy a slot), or a single `digests: StoreList(TypeDigest)` with a `u32` index column per kind; the `?bool` caches become a 2-bit packed column. Same answers.
- Status: no change: the optional costs one byte per 32-byte digest; a separate presence column would split every cache read across two columns to save 3% of them

### src/postcheck/monotype/type.zig:3519-3535 (`resolveSccs`), 3637-3775 (SCC labelling): per-digest-request allocation of `visit_index`, `low_link`, `on_stack`, `scc_stack`, `frames`, `scc_members`, and (for cyclic types) `labels`, `next_labels`, `distinct_labels` map, `sorted_labels`, `rank_of_member`, `block_rep`, `block_digest`, `unfoldings`
- category A; runs per uncached digest computation (per type x digest kind; specialization keys, equality digests, representation digests are all requested per specialization …; impact medium; risk low
- Finding: `DigestEngine.Storage` (3246-3268) already retains `nodes`, `node_lookup`, `link_pool`, `child_offsets`, `scalar_bytes`, `render_buf` across requests, but `resolveSccs` allocates three `node_count`-sized arrays plus three fresh ArrayLists every call and the cycle-labelling path allocates ~10 more temporaries. `child_offsets: ArrayList(usize)` could be `u32`.
- Fix: add the SCC scratch (as one `ArrayList(SccNode{visit_index, low_link, on_stack})` and the three stacks) and the labelling scratch to `Storage`, cleared with `clearRetainingCapacity` per request. No output change.
- Status: open

### src/postcheck/monotype/type.zig:345, 1457, 2162: `AutoHashMap(DigestBucketKey, std.ArrayList(TypeId|u32))` map-of-lists for digest interning
- category F; runs per interned type (`full_digest_interned`) and per commit suffix (`suffix_by_digest`, allocated fresh per commit at 1457); impact low-medium; risk low
- Finding: each digest bucket owns a heap `ArrayList`; `cloneFrozen` (424-434) deep-copies every bucket list.
- Fix: `first: u32` + intrusive `next_in_bucket: StoreList(u32)` column on the store (one allocation total), or `(start,len)` spans into a shared list for the per-commit suffix map.
- Status: open

### src/postcheck/monotype/type.zig:221-227 (`MonoTypeField`): `value_ty: ?TypeId` + `default: ?FieldDefault` optionals
- category C; runs per record field in the `fields` column; impact low; risk low
- Finding: `{name u32, ty u32, value_ty ?u32 (8), kind_state u8, default ?FieldDefault (12)}` = 32 B; with `maxInt` sentinels for `value_ty` and `default.expr_node` it is 20-24 B.
- Fix: sentinel values; `InstField` (solve.zig:97-107) has the same shape and the same fix.
- Status: open

### src/postcheck/lambda_solved/solve.zig:3097-3135, 3248-3260: `tag_row_indexes`/`record_row_indexes`/`lambda_set_indexes`: hash map of (len + inner `AutoHashMapUnmanaged(NameId, usize)`) per row span
- category F/B; runs per row-span relation (every record/tag-union unification builds or re-validates the index for both sides; rebuilt whenever `span.len` changed); impact medium; risk low
- Finding: outer `AutoHashMapUnmanaged(u32 span.start, TagRowIndex)`; each value owns an inner hash map keyed by dense `TagNameId`/`RecordFieldNameId` with `usize` positions. Two heap tables per indexed row, hashing of dense name ids, and `usize` where `u32` suffices.
- Fix: a solver-owned generation-stamped label column (`label_generation: []u32`, `label_position: []u32`, bump per relation), exactly like `InstGraph.row_label_*` in monotype/solve.zig:2110-2116, which needs no per-row map at all; or at least `DenseMap(NameId, u32)` values in a `DenseMap(u32, ...)`. Same pairing semantics.
- Status: open

### src/postcheck/lambda_solved/solve.zig:3163-3167, 3200-3202, 2282-2320, 2368, 2454: fresh ArrayLists/hash sets per row/lambda-set merge step
- category A; runs per tag-row merge (`merged` + `left_names` set), per lambda-set merge (`added` + `added_positions: AutoHashMapUnmanaged(Common.Symbol, usize)`), per …; impact medium; risk low
- Finding: each unification step creates `.empty` lists/maps and deinit's them; `Common.Symbol` is a dense `enum(u32)` so `added_positions` is another lint-dodging dense-id map.
- Fix: solver scratch fields (`merge_tags`, `merge_members`, `capture_pairs`, `payload_pairs` lists; `left_names`/`added_positions` as generation-stamped columns or pooled `DenseMap`s via the existing `solved_set_pool`/`solved_position_pool`). Verify re-entrancy: `mergeLambdaSets` is not recursive, so one scratch per kind suffices (use a start offset otherwise).
- Status: open

### src/postcheck/lambda_solved/solve.zig:960-963, 1085-1088, 1107-1110, 500, 542: per-expression `alloc(TypeVarId, n)` + `free` to build a span
- category A; runs per low-level call expression, per loop, per join point, per closure (`captures`), per function (`args`); impact low-medium; risk low
- Finding: a temporary slice is allocated to collect child type vars, passed to `addSpan`/`bindLowLevelTypes`, and freed. A solver scratch `ArrayList(TypeVarId)` (cleared per use, or `addSpan` fed from `items[start..]`) avoids the heap round trip. The expression driver is an explicit stack (`expr_stack`), so re-entrancy is not an issue when a start offset is used.
- Fix: `self.span_scratch.clearRetainingCapacity()` + append + `addSpan(self.span_scratch.items)`.
- Status: open

### src/postcheck/lambda_solved/solve.zig:129-150: program-wide `[]?TypeVarId` and `[]bool` columns
- category C; runs once per program, sized by lifted locals/exprs/pats and type var count; impact low; risk low
- Finding: `local_tys`, `expr_tys`, `pat_tys: []?TypeVarId` (8 B per slot, 4 wasted), `expr_done`, `generated_backing_pats`, `contains_callable`, `contains_forced_dynamic`, `reaches_cycle: []bool` (1 B per bit), `done: []bool` per `closeUnfilledCallableSlots` call (635) sized by all type vars.
- Fix: `maxInt(u32)` sentinel for the optional columns; `DynamicBitSetUnmanaged` for the bool columns. 50% / 87% memory reduction on those columns.
- Status: open

### src/postcheck/lambda_solved/solve.zig:50 (`UnifyPairSet = std.HashMap(UnifyPair, void, ...)`), 2094-2095: active-unification set cleared by capacity when it empties
- category E/B; runs per unification (insert/remove) and `clearRetainingCapacity` whenever the set drains to zero (per top-level unify); impact low-medium; risk low
- Finding: `std.HashMap` clear is proportional to capacity; at the end of every top-level unification the set is empty and gets cleared over its high-water capacity. The keys are pairs of dense type vars.
- Fix: track the inserted pairs in a stack and remove them explicitly (the unify driver already pops its stack), or use a generation-stamped `DenseMap(TypeVarId, u32 partner)` keyed by the smaller var. Same membership semantics.
- Status: open

### src/postcheck/monotype_lifted/spec_constr.zig:5400-5500 (`Cloner.init`, 35 construction sites; 2714 per specialization, 1415/2037/4506/4633 per function per phase): each Cloner builds a `std.heap.ArenaAllocator`, a `Subst` (one DenseMap + three `AutoHashMap(BinderIdentity, ...)`), five DenseMaps, one AutoHashMap and eight ArrayLists
- category A; runs per call-pattern specialization written, per function per phase (discovery, unused_loop_results, iterator_fusion), per nested rewrite; impact medium; risk low
- Finding: `Cloner` owns ~20 containers all created empty and `deinit`ed per clone; `Subst` adds four maps (`binder_subst`, `binder_aliases`, `loop_carried_binders` are hash maps keyed by `BinderIdentity`). None of this capacity survives to the next clone of the same pass, so every clone regrows from zero. `arena` is `std.heap.ArenaAllocator` (atomics).
- Fix: pool `Cloner` state on the `Pass` (a `spare_cloners: ArrayList(ClonerScratch)` holding the containers; `init` pops one and `deinit` clears-and-returns it), and switch the arena to `base.SingleThreadArena` reset with `retain_capacity`. No semantic change: every container is logically empty at `init`.
- Status: fixed (fix 22)

### src/postcheck/monotype_lifted/spec_constr.zig:1383-1400, 1536: per-function worker allocates two `std.heap.ArenaAllocator`s, an `Output`, a `callable_workers` AutoHashMap and a body-shard `Program`
- category A; runs per function per phase (three phases), in waves of 32; impact low-medium; risk low
- Finding: `Work.execute` creates `output.requests.arena`, `pass.arena`, `callable_workers`, `static_data_structure`, `loop_body_contents` from scratch for every function; the serial path (1536) also creates a fresh `std.heap.ArenaAllocator` per work item. `cloneForSpecConstrBody` (ast.zig:465) is a prefix borrow, so the program clone itself is cheap, but `ProcDebugNameMap.init` and the borrowed name/type stores are re-created …
- Fix: keep a per-worker `SpecConstrWorkerState` (arena + maps) reused across waves with `reset(.retain_capacity)`/`clearRetainingCapacity`; in the serial path hoist the scratch arena out of the loop and reset it per item.
- Status: open

### src/postcheck/monotype_lifted/spec_constr.zig (47 sites: 2029, 2031, 2595, 2885, 3466, 3690, 3705, 4332, 4372, 4380, 4627, 6241-6275, 6516-6532, 7302, 7908, 7912, 8094, 8117, 8340, 8800, 8801, 9463, 9548, 10501, 10506, 10780-10784, 10931-10941, 11272, 11298, 11354, 11589, 11660, 11726, 11890, 12098, 12683): every span walk copies the span out of the program before iterating
- category D/A; runs per cloned expression with children (call args, block statements, match/if branches, loop params/initials, record fields, captures) - i.e. per node of every …; impact medium; risk low
- Finding: `GuardedList.dupe(self.allocator | arena, T, self.program.xSpan(span))` copies the span because cloning appends to the same program store (the file header at line 210 documents this). Branch/field/typed-local records are 12-24 bytes each, so match-heavy bodies copy a lot; the `self.allocator` variants also pay a `free` each.
- Fix: for walks that only read ids (2595, 2885, 4627, 8094, 8117, 12683, ...), iterate by index through the borrow (`program.exprSpan(span)` re-read per element, or `span.start + i` into the frozen prefix: for a body shard, spans that fall in the frozen `body_prefix` are address-stable and never need copying, which `isFrozenExpr` already distinguishes). For walks that must keep the copy while appending, allocate from the …
- Status: open

### src/postcheck/monotype_lifted/spec_constr.zig:15071-15076: `registered: AutoHashMapUnmanaged(Ast.LocalId, std.ArrayList(LocalRegistration))` per loop-body walk
- category B/F; runs per `walkLoopBodyContents` call (per loop analysed in unused-loop-result projection and iterator fusion); impact low; risk low
- Finding: dense `LocalId` keys hashed, one heap ArrayList per registered local, all freed at the end of each walk.
- Fix: `DenseMap(Ast.LocalId, u32 head)` + one shared `ArrayList(struct{reg: LocalRegistration, next: u32})`; or sort registrations by local once at the end.
- Status: fixed (fix 57)

### src/postcheck/monotype_lifted/spec_constr.zig:8467-8472, 8959-8961, 9102, 9316, 9467, 9616, 10641: `arena.create(std.ArrayList(...))` then grown with the GPA
- category F/A; runs per let-case join, per loop rewrite, per block clone task; impact low-medium; risk low
- Finding: an `ArrayList` header is allocated in the arena so a task can hold it by pointer; the list's buffer is then allocated/grown from `self.pass.allocator` (`task.params.*.append(allocator, ...)`) and must be freed separately (or leaks into the arena lifetime). Each task therefore has 1 arena allocation + N GPA reallocations.
- Fix: give the task a `u32` start offset into a Cloner-owned scratch list (`scratch_typed_locals`, `scratch_expr_ids`, `scratch_stmt_ids`), and slice `items[start..]` when the task finishes; the explicit task stack guarantees LIFO use.
- Status: open

### src/postcheck/monotype_lifted/spec_constr.zig:5124-5141 (`Subst`): `AutoHashMap(BinderIdentity, Value)` x2 + `AutoHashMap(BinderIdentity, u32)` with `Value` values (~48 B)
- category C/B; runs per binder substitution during cloning (every pattern binder bound while cloning a body); impact low; risk low
- Finding: `Value` is a union of `TagValue`/`RecordValue`/`CallableValue` etc. (slices + ids, roughly 40-48 bytes) stored inline as hash-map values alongside a `BinderIdentity` key; `exact: DenseMap(LocalId, Value)` is fine but the three binder maps are hashed per lookup.
- Fix: if `BinderIdentity` is `(fn, binder index)` it can be made dense per source function (a per-function `DenseMap` or a column over the function's binder count); store `*const Value` from the cloner arena (values are already arena-stored at 1794/6611) so map rows are 8 B.
- Status: open

### src/postcheck/solved_lir_lower.zig:919, 2480-2495, 2624-2627, 2733-2741, 4416-4425: `ProcLocalSet = AutoArrayHashMapUnmanaged(u32, void)` built from scratch per procedure, then copied + sorted
- category B/A/E; runs per lowered procedure body and per static initializer (3 construction sites); impact low-medium; risk low
- Finding: every local id a procedure touches is inserted into a fresh hash set keyed by the dense `LIR.LocalId` backing int; `writeFrameLocals` then allocates a `[]LocalId` copy, sorts it, and appends it as the frame span. Hashing dense ids, per-proc table growth, plus an alloc+sort per procedure.
- Fix: LIR local ids within a procedure are allocated monotonically, so record `first_local`/`next_local` at proc entry and emit the contiguous range (if locals are always added in that window), or keep a Lowerer-owned `DynamicBitSetUnmanaged` sized by `store.localCount()` and iterate set bits in order (already sorted, no sort needed) clearing only the bits set during this proc (keep a touched list). Verify `addLocalSpan` …
- Status: open

### src/postcheck/solved_lir_lower.zig:7748, 8092, 7825, 7818: per-call `allocator.dupe`/`alloc` of argument id lists
- category A/D; runs per direct call, per erased/variant call, per inlined call; impact medium (calls are the most common expression after locals); risk low
- Finding: `const args = try self.allocator.dupe(Lifted.ExprId, task.args)` copies the already-borrowed argument expression slice (owned by the lifted program, which is immutable during LIR lowering) into a heap slice that lives in the task; `arg_locals` and `substituted` are likewise per-call allocations.
- Fix: keep `[]const Lifted.ExprId` borrowed from the lifted program (it is frozen for the whole of LIR lowering; `Lifted.Program` is `*const` on the Lowerer); for `arg_locals`/`substituted` use a Lowerer scratch stack indexed by a start offset (the task engine is an explicit stack). Verify no task outlives the frame (`releaseLowerFrame`-style cleanup).
- Status: open

### src/postcheck/solved_lir_lower.zig:8398-8405: a heap-allocated `std.heap.ArenaAllocator` object per `match` expression
- category A; runs per match expression lowered; impact low-medium; risk low
- Finding: `allocator.create(std.heap.ArenaAllocator)` + `init` per match so `match_tree` can allocate decision-tree nodes; the arena is std's (atomics) and is destroyed with the match. `lowerConstructionExprInto` (3259) and the uniform-list path (7015) do the same per static-data candidate.
- Fix: one `base.SingleThreadArena` on the Lowerer/worker reset per match (matches do not nest inside the same task frame without finishing, but verify: branch bodies are lowered as child tasks while the tree is live, so a stack of arenas or an arena-with-mark/reset is needed; `arena_state` by pointer suggests the task keeps it across children, so use a pooled arena list like `InstGraph.spare_arenas`).
- Status: open

### src/postcheck/solved_lir_lower.zig:4748, 5416-5442: label text duplicated per runtime schema entry
- category D; runs per runtime record/tag schema materialised (per schema request, per field/tag); impact low; risk low
- Finding: `try self.allocator.dupe(u8, names.tagLabelText(tag.name))` copies interned label bytes the name store already owns for the compile's lifetime.
- Fix: borrow the interned slices (the `RuntimeSchemaStore` lives no longer than the solved program's names); if the schema store must own its bytes for serialization, intern once per label in a schema-local `AutoArrayHashMap([]const u8)` instead of per use.
- Status: open

### src/postcheck/solved_lir_lower.zig:1695-1705, 2468-2472, 2546-2550: per-body `clearRetainingCapacity` on `AutoHashMap`s (`inline_scope_rebases`, `typed_local_map`) sized by the largest body
- category E/B; runs per function body (worker deinit and two body-switch paths); impact low; risk low
- Finding: clearing a `std.AutoHashMap` touches its whole capacity; `typed_local_map: AutoHashMap(TypedLiftedLocal{local, ty}, LocalId)` is keyed by a (dense local, dense type) pair and `inline_scope_rebases` by two dense scope ids. The DenseMaps alongside them clear proportionally to live entries.
- Fix: `typed_local_map` -> `DenseMap(Lifted.LocalId, small list of (ty, local))` (a local rarely has more than one type binding), `inline_scope_rebases` -> `DenseMap(Lifted.InlineScopeId, ...)` keyed by the source scope with the outer scope compared in the value. Same lookups.
- Status: open

### src/postcheck/solved_lir_lower.zig:1006, 1030, 1034: module-wide `[]?PayloadCondition`, `[]?ComptimeSiteId`, `[]?CaptureSpanId` columns
- category C; runs once per run (sized by all lifted locals / comptime sites / fns), shared by workers; impact low; risk low
- Finding: optionals of 4-byte ids cost 8 bytes; `?PayloadCondition` depends on its payload size.
- Fix: sentinel-valued ids; no behaviour change.
- Status: open

### src/postcheck/monotype_lifted/lift.zig:1070-1074, 1774-1815: arrays of `ArrayList`s and per-node/per-edge lists in the capture dependency graph
- category F; runs once per program: one `ArrayList(TypedLocal)` per function (`allocateCaptureTable`), per `CaptureGraphNode` two lists (`direct`, `edges`), per …; impact low (runs once per program); risk low
- Finding: a lifted program with F functions and E capture edges allocates 3F + E + ... independent growable buffers; the graph is rebuilt by `recomputeCaptures` (224) each time it is requested.
- Fix: flat edge-list representation (`first_edge: u32` per node + `next: u32` per edge, supplies as `(start,len)` spans into one `ArrayList(CaptureSupply)`), captures per fn as spans into one shared `ArrayList(TypedLocal)` finalized with a prefix-sum pass.
- Status: open

### src/postcheck/monotype_lifted/lift.zig:398-404, 437-451: `[]bool` done-columns and `[]?FnId` maps over every expr/stmt/fn
- category C; runs once per program; impact low; risk low
- Finding: `expr_done`/`stmt_done: []bool` (bitset-able), `fn_map`/`def_map`/`nested_def_map: []?Ast.FnId` (8 B optionals).
- Fix: `DynamicBitSetUnmanaged` and `maxInt` sentinels.
- Status: open

### src/postcheck/monotype_lifted/lift.zig:1832-1849: capture graph arena is a heap-allocated `std.heap.ArenaAllocator`
- category A; runs per `CaptureDependencyGraph` build; impact low; risk low
- Fix: `base.SingleThreadArena` inline in the struct.
- Status: open

### src/postcheck/lambda_mono/eval.zig:2897-2910 (`list_append_unsafe`, `list_prepend`), 2914 (`list_concat`), 2924-2976, 3018: every list operation copies the whole list into the arena
- category E/H; runs per evaluated list builtin; a compile-time loop that appends n elements does O(n^2) copying and retains O(n^2) arena bytes (values are 32 B each) until the …; impact medium-high for CTFE-heavy programs, low otherwise; risk medium
- Finding: values are immutable arena slices, so `append` allocates `len + 1` and `@memcpy`s. `list_set`/`list_swap`/`drop` likewise copy. The arena never frees, so a loop building a 10k-element table allocates ~1.6 GB of 32-byte values.
- Fix: give `Value.list` a capacity-carrying representation (`ptr, len, cap` with the arena owning `cap`) and append in place when `len == cap_used` for the unique last writer (the evaluator's `refcount == 1` analogue: a list built by `list_append_unsafe` whose source slice ends exactly at the arena's last allocation can be extended in place with `arena.resize`), or evaluate list builtins over a growable `ArrayList(Value)` …
- Status: no change (the Lambda Mono evaluator is a debug-only differential oracle, not on any production path)

### src/postcheck/lambda_mono/eval.zig:78-96 (`Value`): 32-byte values because of `i128` payloads
- category C; runs every evaluated value, every list element; impact low-medium; risk low
- Finding: `int: i128` and `dec: i128` force 16-byte alignment, making `Value` 32 B; `[]const Value` lists spend 32 B per `bool`/`u8` element.
- Fix: store 128-bit payloads as `[16]u8` (align 1) with `@bitCast` on read, bringing `Value` to 24 B (16 B slice + tag), or box 128-bit ints in the arena for a 16/24 B value. No semantic change.
- Status: no change (debug-only differential oracle)

### src/postcheck/lambda_mono/eval.zig:148, 193: `std.heap.ArenaAllocator` for all evaluator allocation; `ops_alloc_sizes: AutoHashMap(usize, usize)` keyed by pointer
- category A/B; runs every evaluator allocation / every host-ops allocation; impact low; risk low
- Fix: `base.SingleThreadArena`; store the size in a header before the allocation instead of a pointer-keyed map.
- Status: open

### src/postcheck/match_tree.zig:319, 1160-1163, 1180: hashed maps keyed by `OccDigest` (13 B struct), `ScopeOwner`, `u32`, and dense `OccId`
- category B/F; runs per match expression (the compiler and emitter are created per match in an arena): `occ_map` lookup per pattern column, `Env.locals: …; impact low; risk low
- Finding: `OccId` is a dense id the same compiler assigns (`occ_entries` column), so `Env.locals` and `uses` (already a DenseMap) should both be columns over `occ_entries`; the arena absorbs the allocations but hashing and table growth remain per match.
- Fix: `Env.locals` -> `DenseMap(OccId, EmitLocals)` or a column sized by `occ_entries.len` (the undo log already exists); `scoped_uses` -> spans into one list; `exit_joins` keyed by what `u32` is (a branch index?) -> column.
- Status: partly fixed (fix 57): the `OccId`-keyed map is a `DenseMapUnmanaged`; the digest- and owner-keyed maps are structural

### src/postcheck/boxy/plan.zig:59-78 (`CheckedTypeIdentity`, `CheckedExprIdentity`), 172-181, 6209-6213, 6928, 6991, 3800-3820 (Builder maps): every identity carries a 128-byte `checked.ModuleId` (`CheckedModuleArtifactKey` = 4 x [32]u8), and per-expression visited sets hash it
- category B/C; runs `stepExprTypes` runs per checked expression per worker and does `body_exprs_seen.getOrPut({expr: {module: view.key (128 B), expr}, worker})` - a 136-byte key …; impact high (Boxy plan memory drops several-fold; per-expression …; risk medium (wide mechanical change)
- Finding: `checked.ModuleId` is `CheckedModuleArtifactKey`, an extern struct of four 32-byte hashes. Every plan row that names a checked type/expr (`RepChild`, `TypeRepBinding`, `TypeRepresentation.source_type`, `DeclaredField`, `LiteralSite`, `CheckedExprIdentity` lists such as `alias_calls`, `dictionary_method_call_types`, `generated_codec_call_types`, ...) stores 128 bytes of module key inline, so `TypeRepresentation` is …
- Fix: intern `checked.ModuleId` once per program plan into a dense `PlanModuleId = enum(u32)` (an `AutoArrayHashMap(CheckedModuleArtifactKey, void)` whose index is the id; the plan already has a module list in `ProgramInput`), and store/hash `(PlanModuleId u32, id u32)` everywhere; expose the full key through the module table for consumers that need bytes. For the three body visited sets, use a per-(module, worker) …
- Status: open

### src/postcheck/boxy/plan.zig:375-395 (`TypeRepresentation`): three `?u32` optionals and seven bools beside five spans
- category C; runs per representation row; impact low; risk low
- Finding: `descriptor: ?DescriptorRequirementId`, `sealed_default: ?TypeRepId`, `presence_slot_present_discriminant: ?u32` (8 B each) + 7 separate bools; with sentinels and a `packed struct(u8)` of flags the row loses ~18 B on top of the module-key fix.
- Fix: sentinels + flags byte.
- Status: open

### src/postcheck/boxy/plan.zig:114, 124, 1255, 2165-2177, 2675, 2824, 3030, 3435, 3630, 11887, 14338, 16219-16221, 18663-18708 (20 sites): `AutoHashMapUnmanaged(TypeRepId | TermId | SiteId | EdgeId, ...)` over dense plan ids
- category B; runs per representation/literal-term/site during planning (several of the 18663-18708 maps are rebuilt per planning pass over all reps); impact medium; risk low
- Finding: dense ids produced by the same builder are hashed; `TypeRepId` indexes `representations`, `TermId`/`SiteId`/`EdgeId` index `LiteralRequirements` columns.
- Fix: `collections.DenseMap` or columns on the owning table; for the `void`-valued sets a bitset sized by the id count.
- Status: fixed (fix 57)

### src/postcheck/boxy/plan.zig:2578, 2613, 9981, 10817, 11050, 11098: record children / tag variants re-sorted per representation build
- category E; runs per record/tag-union representation built, per nominal backing, per declared-field order computation; impact low-medium; risk low (verify the checked rows' order …
- Finding: children/variants are sorted by name text (`std.mem.sort` with a names-context comparator calling `lessThan` on label text) for each representation, even though the checked module already stores rows in canonical order (design.md: record and tag rows use exact lexicographic name ranks).
- Fix: if the checked input rows are already canonical, drop the sort (assert sortedness in debug); otherwise sort by precomputed label ranks (`u32` compare) instead of text compares.
- Status: open

### src/postcheck/boxy/lower.zig:41891-41930 (`ensureBinderLocals`, `ensureDescriptorLocals`, `ensureDictionaryLocals`): per procedure body: 11 columns allocated and memset to the size of the whole module's binder count and the whole program plan's descriptor/dictionary counts
- category E/A (the headline "once per procedure, proportional to the module" case); runs per `ProcBodyBuilder` that touches a binder/descriptor/dictionary - i.e. per worker procedure, per synthetic adapter (10 construction sites at 4065-15014), per …; impact high (dominant per-procedure overhead in Boxy lowering for …; risk low-medium
- Finding: `binder_locals: []?LocalId` + `binder_reps: []?TypeRepId` sized by `module.checked_bodies.patternBinderCount()` (every binder in the module), six descriptor columns sized by `plan.descriptors.items.len` (every descriptor requirement in the program) and three dictionary columns sized by `plan.dictionaries.items.len`, all `@memset` per body. A module with B binders and P descriptors pays O(B + P) allocation + zeroing …
- Fix: keep the columns on the `ProcedureBuilder` (one allocation for the run), scoped by a per-body epoch stamp (`binder_epoch[i] == current_body_epoch` means bound) exactly as design.md prescribes for store-indexed local columns ("A short-lived scope over a larger ID domain uses a paged, reusable, epoch-based ... dense column so that clearing and iteration remain proportional to the live scope"), or use …
- Status: fixed (fix 19)

### src/postcheck/boxy/lower.zig:33443-33446, 33607-33612, 26229-26231: per-query scratch sized by `plan.representations.items.len`
- category E/A; runs `descriptorTemplateNeedsCapturesForKnownRep` allocates and memsets a `[]bool` over every representation in the program per call (per descriptor template …; impact medium; risk low
- Fix: reuse a `DenseMapPool(TypeRepId, void)` / epoch-stamped visited column on the `ProcedureBuilder`; for `forced_refs`/`exact_reps` use `DenseMap(TypeRepId, ...)` populated only with `nominal_formal_bindings` (the loop at 33613 writes only those).
- Status: open

### src/postcheck/boxy/lower.zig:815-860 (`RecordLabelPositions`): `StringHashMapUnmanaged(usize)` keyed by label text for cross-module records, `AutoHashMapUnmanaged(RecordFieldLabelId, usize)` for same-module
- category B; runs per record construction/destructure lowered across modules (per record expression); impact low-medium; risk low
- Finding: cross-module label pairing hashes the label text (`recordFieldLabelText`) per field; same-module pairing hashes a dense label id.
- Fix: resolve cross-module labels through the canonical-name interner once (`names.CanonicalNameStore` gives one id per text; if the two modules' stores differ, map through a per-module-pair `DenseMap(label -> label)` built once), and use a generation-stamped position column for the same-module case.
- Status: open

### src/postcheck/boxy/lower.zig:1377 (`type_desc_ids: []?BoxyTypeDescId`), 1370-1372 (`worker_procs`, `erased_worker_procs`, `hosted_external_procs: []?LirProcSpecId`), 1378 (`rep_holds_callable: []?bool`), 15700-15720 (`binder_locals`, `descriptor_*`, `dictionary_*` optionals): optional-id columns at 8 bytes per slot and `?bool` at 2 bytes
- category C; runs program-wide columns and per-body columns; impact low; risk low
- Fix: `maxInt` sentinels / 2-bit tristate for `?bool`.
- Status: open

### src/postcheck/boxy/lower.zig:820, 15617, 15648, 34468, 34593: `AutoHashMapUnmanaged(LIR.LocalId | PatternBinderId | RecordFieldLabelId, ...)` over dense ids
- category B; runs per body (`recursive_value_slots: AutoHashMapUnmanaged(PatternBinderId, LocalId)`, `descriptor_local_template_positions: AutoHashMapUnmanaged(LocalId, u32)`), …; impact low; risk low
- Fix: `DenseMap` or columns scoped by the body epoch.
- Status: fixed (fix 57)

### src/postcheck/boxy/lower.zig:5497, 43932, 7484: per-record sort of declared fields / checked record fields by label text
- category E; runs per nominal record representation emitted and per `constRecordFields` call (per constant record restore); impact low; risk low
- Finding: `std.mem.sort(checked.CheckedRecordField, fields.items, SortContext{names}, ...)` compares label text per comparison; checked record rows are already produced in canonical rank order (design.md).
- Fix: assert the input is sorted in debug and drop the sort, or compare precomputed ranks.
- Status: open

### src/postcheck/lambda_mono/lower.zig:403-409 (`lowerFnSpec`): four whole-program `?Id` columns memset per function specialization
- category E (per procedure, proportional to the whole program); runs per Lambda Mono function specialization (`lowerQueuedFns` calls `lowerFnSpec` for every queued spec); impact high for large programs with many specializations (the …; risk low
- Finding: `local_map`, `expr_map`, `pat_map`, `stmt_map` are allocated once at `init` (277-291) over every lifted local/expr/pat/stmt in the program, then `@memset(..., null)` at the start of every function spec, so lowering F specializations costs O(F x (locals + exprs + pats + stmts)) bytes of zeroing regardless of body size; with 8-byte optionals that is 32 bytes per node per spec.
- Fix: epoch-stamped columns (`map_epoch: []u32` + value column; a slot is live when its epoch equals the current spec's), or `collections.DenseMap` per spec (clearing proportional to the entries the spec wrote), or at least restrict the memset to the id ranges the source function's body occupies (lifted bodies are contiguous ranges? verify with `Lifted.Fn` layout). Same results.
- Status: fixed (fix 1)

### src/postcheck/lambda_mono/lower.zig:1372-1441: per-node `alloc`+`free` of a `lowered` slice just to call `addXSpan`
- category A/D; runs per lowered list/tuple/record/call/block/pattern/branch node (every node with children); impact medium; risk low
- Finding: results already live in `task.results.items` (an ArrayList on the task); the code allocates a fresh typed slice, copies ids out of the result union, calls `program.addExprSpan(lowered)` (which copies again into the program column) and frees. Two copies and one heap allocation per node with children.
- Fix: `addExprSpan` could take an iterator/`anytype` or the lowerer can keep per-kind scratch `ArrayList`s (cleared per use) so no heap allocation happens; or append directly to the program column via `ensureUnusedCapacity` + `appendAssumeCapacity` in the loop (span = start/len recorded before/after). No semantic change.
- Status: open

### src/postcheck/lambda_mono/lower.zig:264: `folded_matches: AutoHashMapUnmanaged(Lifted.ExprId, Lifted.ExprId)` keyed by a dense id
- category B; runs per folded match lookup (per match expression lowered); impact low; risk low
- Fix: `DenseMap(Lifted.ExprId, Lifted.ExprId)`.
- Status: fixed (fix 57)

### src/postcheck/monotype/ast.zig:98-110 (`StringLiteral`), `addStringView` (~line 1900), lower.zig:17813-17820: one heap allocation per string literal plus a 40-byte record with an optional refcounted backing
- category F/C; runs per string literal per sealed specialization body (the draft already keeps literals in one shared `string_bytes` buffer, lower.zig:17460-17465; sealing dupes …; impact medium (allocation count ~ number of literals x …; risk low
- Finding: `addStringView` does `allocator.dupe(u8, backing)` per literal; `StringLiteral = {backing: []const u8 (16), shared: ?*SharedLiteralBacking (8), offset u32, len u32}` = 32-40 B; `SharedLiteralBacking` adds an atomic refcount per const-blob literal.
- Fix: a program-owned `string_bytes: ArrayList(u8)` with `StringLiteral = {start: u32, len: u32}` (const-blob views can copy their bytes into the same buffer, or index a separate blob table by id); `cloneForSpecConstrBody`/`cloneSolvedProgram` copy one buffer instead of N allocations. Verify `stringLiteralText` consumers only need a slice.
- Status: open

### src/postcheck/monotype/ast.zig (`local_names: ProgramList([]const u8)`), lower.zig:17922-17923: one heap string per local
- category F/D; runs per local per sealed specialization body; impact medium; risk low
- Finding: `program.local_names.appendAssumeCapacity(try program.allocator.dupe(u8, local_name))` allocates a separate heap string per local (the draft already holds them as `DraftSpan(u8)` into `source_text_bytes`), and the column stores 16-byte slices. Boxy/LIR consumers only need the text for debug names.
- Fix: `local_names: ProgramList(Span(u8))` into a shared `local_name_bytes` buffer (append the draft's `source_text_bytes` once per seal and offset the spans). Zero per-local allocations.
- Status: open

### src/postcheck/lambda_solved/type.zig:76-120 (`Content.named`), src/postcheck/lambda_mono/type.zig:93-120 (`Content.named`): the same ~160-byte inline `named` payload as Monotype, in stores read on every `link` walk
- category C; runs `lambda_solved` `vars: ArrayList(Content)` holds one entry per type variable of the whole program and every unification `link` chase reads a full entry; …; impact high (whole-program type-variable table shrinks ~8x; …; risk medium
- Finding: both `named` variants embed `MonoType.NamedType` (36 B) + `MonoType.TypeDef` (~92 B with its optional digest and iterator topology) + spans/optionals, so each `Content` is ~170 B while `link`, `primitive`, `list`, `record` need 4-8 B. The `erased`/`erased_fn` variants carry a 32-byte digest inline as well. The lambda-solved `link` chain walk (union-find) therefore touches one cache line per hop.
- Fix: same as the Monotype store: a `named_payloads` column with a `u32` index in the union, and the rare `generated`/`iterator_topology` fields in side columns; keep `link` in a dedicated `parent` column for lambda-solved so union-find never loads content. Verify `TypeCloner`/`cloneSolvedTypeStore` (solved_lir_lower.zig:15066) copy the new columns.
- Status: open

### src/postcheck/monotype/specialize.zig:203-215, 302, 306: `SpecLookupAddress` (~240 B) hashed per specialization lookup; `lookup`/`owned_evidence_by_digest` are maps of `ArrayList`s
- category B/F; runs per specialization request/reservation/refinement (`find`, `findLocal`, `reserve`; each request computes the address and hashes it; refinements re-register …; impact medium; risk low
- Finding: seven 32-byte digests plus ids are hashed (Wyhash over ~240 bytes) for every lookup, and each address bucket owns a heap `ArrayList(SpecId)` even when it holds one record (the common case). `owned_evidence_by_digest: AutoHashMap(EvidenceDigest, ArrayList(u32))` has the same shape.
- Fix: pre-hash the address to a `u64` once (the digests are already uniformly distributed, so folding them with a cheap mix is sound) and store the full address only in the record for equality on hit; use the `Candidates{first, overflow}` layout from lower.zig's `DraftSpecLookup` so single-record buckets need no list.
- Status: open

### src/postcheck/proc_identity.zig:526-620 (`resolveCyclicComponent`): per cyclic component: `position_of_node` sized by all nodes, plus `labels`, `next_labels`, `distinct_labels` map, `sorted_labels`, `rank_of_member`, `block_rep` allocated fresh
- category A; runs per cyclic SCC per procedure identity digest (recursive types are common in real programs: every recursive nominal reaches here); impact low-medium; risk low
- Finding: unlike `type_digest.zig` (which keeps all of these as retained scratch on its `Graph`, 26-54), `proc_identity.zig` allocates and frees them per component and memsets a node-count-sized `position_of_node` per component.
- Fix: port the `type_digest.zig` scratch layout (retained lists + `clearRetainingCapacity`) into `proc_identity.Engine`; `position_of_node` can be a retained column cleared only at the member indices touched.
- Status: open

### src/postcheck/monotype_lifted/body_shard.zig:214, src/postcheck/solved_lir_lower.zig:14967: per-local `dupe` of local names when shards are appended / programs cloned
- category D/F; runs per local per committed body shard; per local per `cloneLiftedProgram`; impact low-medium; risk low
- Finding: same per-local string allocation as the seal path (see monotype/ast.zig entry); every program clone re-dupes every name.
- Fix: shared name bytes buffer + spans (one `appendSlice` per clone).
- Status: open

### src/postcheck/comptime_scalar_values.zig:83, src/postcheck/monotype_lifted/loop_exit_demand.zig:39, src/postcheck/monotype/worker_inputs.zig:170: `std.heap.ArenaAllocator` instances
- category A; runs per completed scalar value set / per loop-exit inventory / per worker-input index; impact low; risk low
- Fix: `base.SingleThreadArena` (the worker_inputs trie is intentionally lock-free and read concurrently; keep its arena thread-safe or confirm single-writer before switching).
- Status: open

### src/postcheck/boxy/literal_requirements.zig:108, 223, 233-234, 248, 252: dense `TermId`/`RequirementId`/`SiteId` keys in `AutoHashMapUnmanaged`s, `by_requirement: AutoHashMapUnmanaged(Requirement, RequirementId)`
- category B; runs per literal requirement term/edge during Boxy literal planning; impact low; risk low
- Fix: `DenseMap`/columns on the `entries`/`requirements` tables; keep `by_requirement` as the one structural interner.
- Status: partly fixed (fix 57): the maps keyed by dense term, requirement, and site ids are `DenseMapUnmanaged`s

### src/postcheck/solved_inline.zig:161-192: `[]Decision`, `[]MaterializationState`, `[]?ExprId`, `[]InlineKind` columns over all functions; fine (once per program), but `?Lifted.ExprId` costs 8 B
- category C; runs once per program; impact low; risk low
- Fix: sentinel id.
- Status: open

### src/postcheck/monotype_lifted/normalize.zig:520, 530: `program.allocator.alloc(FieldExpr | CaptureOperand, span.len)` per record/closure normalisation step
- category A; runs per record expression / closure with capture operands normalised; impact low; risk low
- Fix: normalizer-owned scratch lists.
- Status: open


## LIR passes, ARC, and the dev/LLVM/wasm backends
### src/lir/LIR.zig:1138: `CFStmt` is ~144 bytes / align 16 because of rare Boxy and i128 variants; common statements need <= 48
- category C; runs per statement, for every store (coordinator + every body shard + every proc-rewrite overlay), and every pass that reads statements (the union is copied by …; impact high (3x less statement memory in every shard and the …; risk medium (mechanical but wide: every …
- Finding: `CFStmt` is a `union(enum)` stored in `LirStore.cf_stmts` (src/lir/LirStore.zig:852). Its size is set by `assign_call_erased` (target 4 + closure 4 + 3 spans 24 + LocalSpan 8 + `?BoxyDescRef` 28 + `?LocalId` 8 + plan 4 + 3 bools + `?LocalId` 8 + `drive` 1 + `?PendingReturn` 36 + next 4 = ~132 bytes) and its alignment by `assign_literal.value: LiteralValue`, whose `i128_literal`/`dec_literal`/`boxy_dynamic_*` …
- Fix: (1) Move the i128-bearing literal payloads out of line: store `i128_literal`/`dec_literal`/`boxy_dynamic_*` values in a dedicated `i128s` column (or as two entries in the existing `u64s` pool) and keep a `U64Span`/index in `LiteralValue`; this drops `@alignOf(CFStmt)` from 16 to 8 (or 4) and `LiteralValue` from 64 to ~32 bytes. (2) Side-table the Boxy-only call metadata: give …
- Status: partly fixed (fix 50): 128-bit literal payloads are 8-byte aligned, so the union is 136 bytes at alignment 8; the largest call variants still size it, and moving them to side tables would end the by-reference statement reads every pass now uses

### src/lir/LIR.zig:1402: `assign_low_level` embeds a 56-byte `RcEffect` that is a pure function of `op` plus a transfer mask
- category C/D; runs per `assign_low_level` statement (the most common statement kind after `assign_ref`/`set_local` in lowered bodies).; impact medium; risk low
- Finding: `assign_low_level` stores `rc_effect: LowLevel.RcEffect` (6 u64 masks + 4 bools = 56 bytes) next to `op: LowLevel` (u16). `LirStore.addLowLevelStmt` (src/lir/LirStore.zig:1758) fills it from the op's declared effect, and ARC's `LowLevelSelection` (src/lir/arc.zig:1834) may replace `op`+`rc_effect` together. Also `unique_args: u64`, `interchangeable: WidthValues(bool)`, `simd_concat_count: ?u5`. The struct is ~88 …
- Fix: store only the data that is not derivable from `op`: keep `op` and a small `rc_effect_id: u32` into an interned `rc_effects` column (there are only a few dozen distinct `RcEffect` values across all ops, and ARC's rewrites produce a bounded set). Or keep `rc_effect` as an index into a comptime table keyed by op and have ARC's selection write a different op/effect pair that is itself interned. Verify: every consumer …
- Status: open

### src/lir/LIR.zig:1246: `LirProcSpec` is ~300 bytes and `getProcSpec` returns it by value in per-statement loops
- category C/D; runs per `assign_call` statement in many passes (54 call sites in arc.zig, 19 in loop_append_promote, 18 in LirCodeGen, 17 in arc_solve, 15 in trmc, ...; see count …; impact medium; risk low
- Finding: `LirProcSpec` has ~45 fields including `identity: [32]u8`, `hosted: ?HostedProc` (12), `ret_desc: ?BoxyDescRef` (28), ten `?LocalId`/`?u32` optionals (8 each), five `BoxySpan`s, `tail_calls: ?TailCalls` (12), four u64 masks, ~15 bools and small enums: roughly 300 bytes. `LirStore.getProcSpec` (src/lir/LirStore.zig:1953) returns a copy; callers typically read one field (`.args`, `.ret_layout`, `.body`, `.external`). …
- Fix: add `getProcSpecConst(id) *const LirProcSpec` (the proc_rewrite overlay only ever replaces the whole spec of `proc_id`, so a pointer into the coordinator or the overlay is well defined) and migrate hot callers; repack the spec by moving the Boxy-only fields (`ret_desc`, `runtime_ret_desc`, `erased_*` x6, `static_erased_capture_layout`, `boxy_runtime_entry`) into an optional side record indexed by a u32, and pack the …
- Status: partly fixed (fix 48): immediate reads take a pointer; the specification itself is unchanged

### src/lir/LirStore.zig:1717: `getCFStmt` copies the whole 144-byte union; nearly every pass switches on the copy
- category D/H; runs per statement visit in every LIR pass (≈700 call sites across src/lir; loop_append_promote 98, scalarize_joins 85, arc 63, tag_case_fusion 63, box_reuse 44, …; impact high (reduces the per-statement cost of every pass; …; risk low
- Finding: `pub fn getCFStmt(self, id) CFStmt` returns by value. In release the `GuardedList` is a plain `ArrayList`, so a pointer return is sound as long as the caller does not append statements while holding it. Many callers do `switch (store.getCFStmt(id)) { .assign_call => |call| ... }` which copies 144 bytes then extracts 1-3 fields. The three-way lookup (proc_rewrite overlay hash probe -> body_coordinator prefix check -> …
- Fix: provide `getCFStmtConst(id) *const CFStmt` for read-only traversals (the overlay case returns a pointer into `rewrite.cf_stmts.data.rows` or into the coordinator; both are stable during a traversal that does not mutate). For the overlay lookup, replace the `HashMap(u32,u32)` probe with a `collections.DenseMap`/paged column (see next finding) so the common prefix read is an array index. Verify: no caller retains the …
- Status: partly fixed (fix 43): immediate reads take a pointer; reads bound to a local still copy

### src/lir/LirStore.zig:68: `RewriteKeys.indices` is a `std.HashMap(u32,u32)` keyed by dense statement ids, probed on every statement read of a proc rewrite
- category B; runs per statement read/write in every per-procedure pass that uses `cloneForProcRewrite` (branch_expectation, trmc, forwarding_join, tag_fusion, known_tag_jump, …; impact high; risk low
- Finding: `RewriteKeys` maps a global `CFStmtId` to a dense position among the rows this rewrite prepared. The rows are exactly the statements reachable from one procedure, which are (after lowering) mostly a contiguous range, and the ids are sorted in `finish()`. A hash map is used so the worker does not allocate a global-sized index; but the per-read probe plus `getOrPut` in `prepare` is paid on every access. …
- Fix: since ids are sorted and unique after `finish`, replace the map with a binary search over `ids` for lookups (log n with no hashing) or, better, an epoch-stamped dense column shared per worker lane (`lane_state`) sized to the coordinator's `cf_stmts.len()` once per phase: `position_of[id]` with a stamp per entry; clearing is O(prepared ids). The design.md "short-lived scope over a larger ID domain uses a paged, …
- Status: fixed (fix 13)

### src/lir/LirStore.zig:297: `cloneForProcRewrite` prepares and later materializes the whole reachable statement set even when the pass changes nothing
- category A/E; runs per procedure per phase (10 phases x every procedure with a body).; impact medium; risk medium
- Finding: `cloneForProcRewrite` walks every reachable statement of the procedure through `prepareValue` (hash `getOrPut` per id, plus a `getCFStmt` 144-byte copy per statement to find successors) and allocates `rows`/`dirty` for all four columns (`RewriteRows.finish` appends `undefined` + `false` per id, element by element). `RewriteRows.mark` then copies **every** prepared row of a column from the coordinator on the first …
- Fix: (1) keep `keys`/`rows`/`dirty` as lane-local (`LaneState`) buffers that are reset per task instead of allocated per task: the shard already lives only until commit; (2) in `RewriteRows.finish` use `resize`/`appendNTimes` instead of a per-element append loop; (3) replace `dirty: ArrayList(bool)` by a `DynamicBitSetUnmanaged`; (4) the pass admission (`phaseAdmits` via `ProcShapes`) already skips most procedures, so …
- Status: open

### src/lir/LirStore.zig:852: per-statement provenance is four parallel columns totalling 36 bytes, of which `OriginKind` (12 bytes) is a tagged union with a 5-byte payload
- category C; runs per statement.; impact low; risk low
- Finding: `cf_stmt_locs` (SourceLoc 12), `cf_stmt_regions` (Region 8), `cf_stmt_inline_scopes` (4), `cf_stmt_origin_kinds` (`OriginKind` = union with `ArcRc{LocalId, RcReason}` payload = 5 bytes -> 8 + tag -> 12, asserted at LIR.zig:449). Non-ARC kinds carry no payload at all.
- Fix: pack `OriginKind` as a `packed struct(u64)`/extern pair {kind: u8, reason: u8, subject_local: u32} (8 bytes) or split the rare `subject_local` into the `u32s` pool. Saves 4 bytes/stmt; low priority but free.
- Status: open

### src/lir/LirStore.zig:757: `appendBodyShard` appends relocated rows one at a time after reserving capacity
- category G/H; runs per statement/local/join of every emitted body (ARC emission of every procedure, and every proc-rewrite commit).; impact low-medium; risk low
- Finding: After `ensureUnusedCapacity` for every column, the loops do `try self.cf_stmts.append(self.allocator, relocateBodyValue(CFStmt, item, ...))` per element (lines 806-840): each `append` re-checks capacity and goes through the `try` path. `relocateBodyValue(CFStmt, ...)` is a comptime-recursive field walk that rebuilds the 144-byte union by value (`@unionInit`) for every statement.
- Fix: use `appendAssumeCapacity`/`addManyAsSliceAssumeCapacity` and relocate in place in the destination slice (`@memcpy` then patch id fields). Verify: identical results; error handling already guaranteed by the up-front reservation.
- Status: fixed (fix 49)

### src/lir/LirStore.zig:1648: `addCFStmt` calls `ensureUnusedCapacity(1)` on five columns then `append` on each, per statement
- category G; runs per statement appended by every lowering and pass.; impact low; risk low
- Finding: five `ensureUnusedCapacity(…, 1)` followed by five `try …append` (which re-checks capacity). `noteStmtShapes` also does `getLocal` (and for joins a span walk) per append. Minor per-call overhead on the hottest append path.
- Fix: `ensureUnusedCapacity` then `appendAssumeCapacity` for the five columns; callers that know the statement count (`appendBodyShard`, body_clone) should reserve once.
- Status: fixed (fix 49)

### src/lir/LirStore.zig:474: `ownStringEntryCount`/`stringEntryCountFrom` iterate every interned string to count them
- category E; runs per body shard append (`appendBodyShard` calls `stringEntryCountFrom`, line 749) and per `captureBodyShard` (`bodyOwnedStringCount`).; impact low; risk low
- Finding: counts string entries by walking the whole `strings` store iterator (`while (iterator.next() != null) count += 1`). For shards this is the shard's own strings only (small), but `stringEntryCountFrom` on a coordinator-side source walks all strings of the program each time.
- Fix: keep an entry count in `base.StringLiteral.Store`/`BuilderState` (it already tracks `byteCount()`), and store the entry count in `BodyPrefix`.
- Status: open

### src/lir/LirStore.zig:509: `captureBodyPrefix` and `captureBodyShard` recompute 23 lengths and re-validate them on every append
- category E; runs per ARC emission commit and per proc-rewrite commit (`appendBodyShardWithJoinRelocation` calls `captureBodyShard` again at line 726, which repeats the 23-field …; impact low; risk low
- Finding: cheap individually (constant), but done many times per procedure; mentioned for completeness. No fix needed beyond caching the prefix in the shard.
- Status: open

### src/lir/arc.zig:836: ARC tasks never use `worker.scratch`; every per-proc transient arena is a fresh `std.heap.ArenaAllocator` over the output allocator
- category A; runs per procedure x 3 phases (source prep, planning, emission), plus per `prepareSourceLiveness`/`solveLoopLiveness` call.; impact high (ARC is the largest post-check pass; every allocation …; risk low
- Finding: `runArcTasks`'s callback (line 838) passes only `worker.allocator` to `context.execute`; `SourceCache.execute` (line 913) and `EmissionOwner.plan` (line 970) each do `std.heap.ArenaAllocator.init(allocator)`. `SourceLiveness.scratch_allocator` is that same output allocator, and `prepareSourceLiveness` (line 7320), the loop-liveness builder (line 7728 `LivenessCertificate`, line 7882) create further …
- Fix: thread `worker.scratch` into `execute(worker)` for the three ARC task contexts; use it for `scratch_arena`, `work`, `loop_edge_nodes`, `pred_counts`, `pred_writes`, `reached`, the materialize task list (line 2133) and solver task list (line 2879); switch the retained arenas (`SourceCache.arena`, `EmissionOwner.arena`) to `base.SingleThreadArena`. Verify: nothing retained in `Completion.value` points into scratch …
- Status: partly fixed (fix 15: arenas are single-thread; scratch threading still open)

### src/lir/arc.zig:1107: `computeBoxyRcDescs` duplicates the `boxy_desc` column of every local
- category D; runs once per `insert` call (per program), O(locals) x 28 bytes.; impact low (one-off, but 28 bytes x locals; on a large program …; risk low
- Finding: allocates `[]?BoxyDescRef` (28 bytes each) for every local and copies `store.getLocal(id).boxy_desc` into it; `boxyDescForLocal` then reads it. `LirStore.locals` is already a dense column of `Local{layout_idx, boxy_desc}`; the copy only exists to avoid the `getLocal` three-way lookup.
- Fix: read through `store.getLocal(local).boxy_desc` (or expose `store.getLocals()` which already exists as a raw slice for a coordinator) and drop the column; `BoxyDescUsers.init` can take the store.
- Status: open

### src/lir/arc.zig:1854: `ArcPlanStep` is ~230 bytes per planned statement, mostly four embedded `ArrayList`s that are almost always empty
- category C/F; runs per statement per emission (stored in `ArcPlan.steps: ArrayList(ArcPlanStep)` for every plan slot; a statement under several structured contexts has several …; impact medium-high; risk low
- Finding: fields: `pre_release_extra`, `pre_retain`, `transfer_positions`, `post_release` (4 x 24-byte ArrayList headers = 96 bytes, each with its own heap block when non-empty), `?ReleaseDecision` (ReleaseDecision = 24-byte union -> 32 with optional), `?LowLevelSelection` (u16 + RcEffect 56 -> 64 + opt), `call_demanded: RcSig` (~16), five u64 masks, `?LirProcSpecId`, `?VariantRequestId`, ~10 bools. `reset()` clears the lists …
- Fix: keep per-emission shared buffers in the emission arena (`release_pool: ArrayList(ReleaseDecision)`, `retain_pool`, `position_pool`) and store `{start,len}` u32 spans in the step; since a step is re-planned by overwriting, append the new contents and point the span at them (the arena frees everything at emission end). Drop `low_level_selection`'s inline `RcEffect` by referencing the interned effect (see LIR.zig …
- Status: open

### src/lir/arc.zig:1990: `ArcPlanMetadata` carries three `?OwnedSet` (48 bytes each) per plan slot; `OwnedSet` and `ExactBitSet` embed a 16-byte `Allocator` and a redundant `domain` pointer / `bit_len`
- category C; runs per plan slot (per structured path), per join summary (6 OwnedSets + `jump_states: []?OwnedSet`), per liveness node (2 `ExactBitSet`s), per loop (`carried`, …; impact medium; risk low
- Finding: `OwnedSet` = `domain: *const ProcArcDomain` (8) + `unique: bool` + `entries: ArcSnapshot` {`allocator: Allocator` 16, `depth: u8`, `root` 8} ≈ 40 bytes; `?OwnedSet` 48. `ExactBitSet` = `bit_len: usize` 8 + snapshot 32 = 40. Within one emission every set shares the same `domain`, the same arena allocator and the same `bit_len`; the per-set copies are dead weight. `ReadBeforeRebindNode` (line 7041) holds two …
- Fix: make `ArcSnapshot` store only `root` (+ `depth: u8` if needed) and take the allocator as a parameter of `put`/`putUnique`/`meetWith`/`joinWith` (the only mutators); make `OwnedSet` = `{root, flags}` with the domain passed by the Inserter (it already has `current_domain`). `ExactBitSet` = `{root, depth}` with `bit_len` in the graph. This halves `ReadBeforeRebindNode` (128 -> ~64) and shrinks every `?OwnedSet` from 48 …
- Status: open

### src/lir/arc.zig:8974: `OwnedEntry` is 16 bytes for a bool + u64; snapshot leaves are 8 x 16 = 128 bytes and every `put` of a shared set copies one leaf per update
- category C; runs per ownership-set update (per statement transfer during the join-summary fixpoint, per join/switch meet).; impact medium; risk low-medium (the field-index bound …
- Finding: `OwnedEntry{present: bool, residual_mask: u64}` padded to 16 bytes; `ArcSnapshot(OwnedEntry).Leaf` = 128 bytes, `Branch` = 64 bytes; `putNode` allocates a fresh leaf + one branch per level on every shared update (`self.allocator.create(Leaf)`), i.e. per-element heap objects from an atomic arena. `residual_mask` is bounded by `field.index >= 64 => reject` (src/lir/arc_dismantle.zig:537), and a resource with no …
- Fix: encode `OwnedEntry` as a single `u64` (`0` = absent; `present` = mask | top bit, requiring the dismantle gate to reject field index 63 as well as >= 64, which currently rejects >= 64), halving leaf size to 64 bytes and halving the bytes copied per update; allocate leaves/branches from the single-thread arena (previous finding).
- Status: open

### src/lir/arc.zig:660: `owners` (`EmissionOwner`, each embedding a full `Inserter` copy of ~600 bytes) are heap-allocated and freed per wave, and each `EmissionOwner` keeps 5 `ArrayList` scratch buffers that are re-grown per procedure
- category A/G; runs per wave (program procedures / `emission_wave_size`), and per procedure for the scratch lists.; impact low-medium; risk low
- Finding: `const owners = try store.allocator.alloc(EmissionOwner, options.emission_wave_size)` inside the wave loop; `owners[count] = .{ .inserter = inserter, ... }` copies the ~40-field `Inserter` per proc. `death_scratch`, `transfer_position_scratch`, `retain_arg_scratch`, `dismantle_temps`, `loop_liveness_caches` start `.empty` for each owner, so each procedure re-grows them from zero (allocating in the GPA, since they …
- Fix: allocate `owners` once outside the wave loop (`emission_wave_size` is fixed) and reuse; move the scratch lists into lane-local state (the executor's `LaneState`) so they keep capacity across procedures on the same lane.
- Status: open

### src/lir/arc.zig:1218: `computeEmissionContainsRefcounted` builds an `AutoHashMap(layout.Idx, void)` visited set and walks every statement of the program to find `erased_capture_load` ops
- category B/E; runs once per `insert` (per program), O(statements) with a 144-byte copy per statement.; impact low; risk low
- Finding: scans all `cfStmtCount()` statements (`store.getCFStmt(stmt_id)` by value) to find `assign_low_level` with `op == .erased_capture_load`, then uses a hash map keyed by dense `layout.Idx` as the visited set for `layoutMayContainBoxyDynamic`. `arc_dismantle.Analysis` keeps a `DenseMap(layout.Idx,bool)` memo plus another `AutoHashMap(layout.Idx, void)` `layout_visited` for the same question …
- Fix: a `ProcShapes` bit (`erased_capture_load`) set in `noteStmtShapes` would let this skip procedures without the op; use a `DynamicBitSetUnmanaged` sized to `layouts.layoutCount()` (or an epoch column) for the visited set and share the memo between the two callers.
- Status: open

### src/lir/arc.zig:1288: `VariantTable.map` is an `AutoHashMap(VariantSelector, …)` whose key embeds the 32-byte `ProcIdentity`-derived data; `variantIdentity` builds a temporary `ArrayList(u8)` key per request
- category A/D; runs per variant request (per call site demanding a non-base ownership vector), bounded by procedures x signatures.; impact low; risk low
- Finding: `variantIdentity` (line 1273) allocates an `ArrayList(u8)` with `initCapacity`, fills it, hashes, frees: per request. A fixed stack buffer (`[8 + 6*max_outcomes]u8`) or feeding the hasher directly (`hasher.update` per field; `derived()` already takes a `key` slice, so a `derivedParts` taking several slices would avoid the buffer) removes the allocation.
- Fix: hash fields directly into `TypeDigestHasher` (add `ProcIdentity.derivedWith(role, fn(*Hasher))`), or use a stack buffer when `outcomes.len` is bounded.
- Status: open

### src/lir/arc.zig:1093: `EmissionOwner.emit` builds the new frame span through a temporary `ArrayList` then `addLocalSpan` copies it again
- category D; runs per emitted procedure that synthesized dismantle temporaries.; impact low; risk low
- Finding: `frame` ArrayList is filled from the old span and `dismantle_temps`, then `store.addLocalSpan(frame.items)` copies into `local_ids`. `addLocalSpan` could take two slices, or the store could expose `addLocalSpanConcat(old_span, extra)`.
- Fix: `addLocalSpan2(existing: LocalSpan, extra: []const LocalId)` appending directly to `local_ids` with one reservation.
- Status: open

### src/lir/arc.zig:6335: `PlaceUseFacts` build allocates ~12 arrays + two sorted lists per emission, using `std.mem.sort` on `PlaceUser`/`SideEntry`
- category A/E; runs per emission (per procedure per ownership variant); the sorted inputs are produced by walking the node list in index order, so `place_user_list` is already …; impact medium (this is per emission, and emissions = procedures x …; risk low
- Finding: lines 6335-6470: `succ_starts`, `pred_starts`, `succs`, `preds` (the graph already has `successors`/`predecessor_starts`/`predecessors` in `ReadBeforeRebindGraph`: the succ/pred CSR is rebuilt here from the same data, and again at lines 7891-7905 in the loop-liveness builder), `rebinds`, `rebound_roots`, `every_place`, `place_user_list` + sort, `place_leaders`, `place_user_starts`, `place_users`, …
- Fix: build the CSR successor/predecessor arrays once per source procedure in `prepareSourceLiveness` (they are a property of the source graph, not the variant) and share them from `SourceLiveness`; replace the two `std.mem.sort` calls with counting sorts keyed by leader/loop index (dense u32 keys); allocate the rest from `worker.scratch`.
- Status: open

### src/lir/arc.zig:7614: `prepareSourceLiveness` computes predecessor CSR with `usize` arrays and keeps `predecessor_starts: []const usize`, `predecessors: []const usize`, `loop_edges: []const usize` in the durable source arena
- category C; runs per source procedure, retained for the whole ARC run.; impact low-medium; risk low
- Finding: node indices are bounded by `no_stmt_node_index = maxInt(u32)` (line 7244 check) yet the CSR arrays are `usize` (8 bytes per entry): `pred_counts`, `pred_starts`, `pred_writes`, `predecessors`, `loop_edges`, `loop_edge_nodes: ArrayList(usize)`; `ReadBeforeRebindNode.successor_start: usize`. The companion `succ_starts`/`succs` built later (line 6335, 7891) are u32.
- Fix: switch to u32 (program-size counts must be >= 32 bits, which u32 satisfies); halves the retained predecessor storage and the per-node field.
- Status: open

### src/lir/arc.zig:7252: `edge_kills` merge allocates a new `merged` slice in the graph arena per kill append
- category A/F; runs per string-match capture per source statement (rare).; impact low; risk low
- Finding: `const merged = try graph.allocator.alloc(ReadBeforeRebindEdgeKill, existing.len + kills.items.len)` appends by reallocating the whole per-node slice in the durable arena; the old slice leaks in the arena. Pool `edge_kills` in one `ArrayList` and store a span per node.
- Fix: shared pool + span.
- Status: open

### src/lir/arc.zig:8089: loop-liveness search allocates stamps, `carried`, `seeds`, and three sorted lists per loop-liveness solve; `std.mem.sort` on `seeds`, `loop_bits`, `node_bits`, and members
- category A/E; runs per emission that reaches a loop edge (once per loop-keyed liveness cache), `std.mem.sort(u32, members, …)` per loop.; impact low-medium; risk low
- Finding: lines 8089-8200: `node_stamp`/`loop_stamp` arrays per solve (`@memset` to maxInt each time, O(nodes)), `seeds` list rebuilt and sorted per iteration (`std.mem.sort(Seed, …)` at 8138), `loop_bits`/`node_bits` sorted per unit at 8179-8180 (sort on `BitAt{index}`: the bits are pushed in traversal order; a per-unit counting bucket by index would avoid the comparison sort), members sorted at 8200.
- Fix: hoist stamps into the lane-local scratch with an epoch counter (no memset per solve); bucket-sort by dense index.
- Status: open

### src/lir/arc.zig:8298: `rows = alloc(?ExactBitSet, node_count)` per loop cache, plus `in_work` bitsets and `node_work` lists per solve
- category A/C; runs per loop identity per emission: O(nodes) x 48 bytes each.; impact low-medium; risk low
- Finding: `LoopLivenessCache.rows: []?ExactBitSet` allocates a 48-byte optional per statement node for every loop (nested loops multiply); most rows are null ("outside the forward closure"). Combined with the `ExactBitSet` slimming above (-> 16 bytes) and a presence bitset instead of `?`, this is 3x smaller.
- Fix: `rows: []ExactBitSetRoot` (8 bytes, null root = absent) after the snapshot slimming.
- Status: open

### src/lir/arc.zig:2193: materialization creates one heap object per control statement (`emission_allocator.create(MaterializeStopTask/JoinTask/ControlTask)`) plus a `child_results` slice each
- category F; runs per control-flow statement per emission.; impact low; risk low
- Finding: lines 2193-2270: `create` per switch/join/str_match/stop, with `child_results` allocated separately. All from the atomic std arena. The task list `tasks: ArrayList(MaterializeTask)` grows from `.empty` per emission.
- Fix: store the task records in a per-emission `ArrayList(MaterializeState)` indexed by u32 (one allocation, amortized) with `child_results` as spans into a shared `ArrayList(CFStmtId)`; grow `tasks` from the lane scratch with retained capacity.
- Status: open

### src/lir/arc.zig:4307: the join-summary solver allocates `SolveSegment`, `SolveStop`, `SolveBodyScope`, `JoinSummary`, `SwitchSummary` with `create` per node, and `branch_resources: []ArrayList(RestoredResource)` per restitution switch
- category F; runs per segment/stop/scope per fixed-point visit, per emission.; impact low-medium; risk low
- Finding: `self.solve_allocator.create(SolveSegment)` (4307), `create(SolveStop)` (3457, 4348), `create(SolveBodyScope)` (4695), `create(JoinSummary)` (4744) + `alloc(?OwnedSet, jump_count)`, `create(SwitchSummary)` (3430); `alloc(std.ArrayList(RestoredResource), branches)` (6000): an ArrayList per switch branch. `chain: ArrayList(*const SolveStop)` (4335) per call.
- Fix: arena-backed is already leak-free; the win is moving `solve_allocator` to a non-atomic arena (SingleThreadArena / scratch) and using index-linked records (u32 parent indices into per-emission `ArrayList`s) instead of pointer-linked heap nodes, which also shrinks `SolveContext` (4 pointers -> 4 u32).
- Status: open

### src/lir/arc.zig:721: debug-only certifier input: `all_sigs` copy of every signature (debug builds only; noted, not scored)
- category D; runs once per compile in debug builds only (`if (builtin.mode == .debug)`).; impact n/a (debug only); risk n/a
- Finding: copies all signatures into a fresh slice so the certifier sees one table. Debug-only, excluded from ranking.
- Status: open

### src/lir/arc_solve.zig:4696: `UniquenessStructure.build` re-walks every procedure body that `liftReachableStatements` already inventoried, and `arc_dismantle.compute` walks it a third time
- category E/D; runs once per `insert` (per program), but each walk is O(all statements) with a 144-byte `getCFStmt` copy per statement plus `forEachStmtRead/Def` dispatch; the …; impact medium-high (3-5 full-store walks collapse to 1 in the …; risk low
- Finding: `solveWithOptions` builds `solver.proc_stmts: []ArrayList(CFStmtId)` in `liftReachableStatements` (line 2069) and hands it to `computeOutcomeRestitution`, but `settleUniquenessWithOptions` (called from line ~1142 while `solver.proc_stmts` is alive) builds a fresh `UniquenessStructure` whose `build` calls `collectProcStatements` per proc (line 4740) to produce the same per-proc statement lists. …
- Fix: thread the per-proc statement inventory (as one flat `stmts: []CFStmtId` + `proc_starts: []u32` CSR built once) from the solver into `UniquenessStructure.build`, `arc_dismantle.compute`, `computeEmissionContainsRefcounted` and `firstFreshJoinPoint` (the max join id is also derivable from `LirStore.join_points` / a store-maintained `max_join_point` counter). Verify: the ordering of statements inside each list matters …
- Status: open

### src/lir/arc_solve.zig:2089: `liftReachableStatements` copies each statement by value three to four times
- category D; runs per statement (once per program, but inside the hottest single loop of the solver).; impact low-medium; risk low
- Finding: `const stmt = solver.store.getCFStmt(current)` (line 2097), then `liftProcStmtFacts` does `switch (solver.store.getCFStmt(current))` (line 2117) and for `assign_call` also `getCFStmt(assign.next)` (line 2125), then `liftSharedStmtFacts(solver, current)` (line 2671) reads it again, then `appendStructuralSuccessors(.., stmt)`. Each is a 144-byte copy through the three-way store lookup.
- Fix: read once (`*const CFStmt`) and pass the pointer/value to the three helpers.
- Status: open

### src/lir/arc_solve.zig:731: `Solver.proc_stmts / proc_returns / proc_join_bodies`, `UniquenessStructure.proc_stmts / returns / callers` and `UniquenessComponent` are arrays of `ArrayList`s (one heap block per procedure per table)
- category F; runs per procedure, 3 + 3 lists plus 4 per component.; impact medium; risk low
- Finding: `solver.proc_stmts = alloc(ArrayList(CFStmtId), proc_count)` + `@memset(.empty)`; each proc's list grows independently (amortized reallocations per proc). `UniquenessStructure.build` repeats it (`proc_stmts`, `returns`, `callers`) in an arena, and `UniquenessComponent{procs, locals, stmts, unresolved: ArrayList}` x components. `Solution.join_bodies/tail_calls` are already flattened with offset/len arrays: the …
- Fix: flat CSR: one `ArrayList(CFStmtId)` plus `proc_starts: []u32` (statements are discovered proc by proc, so a flat append with a start index per proc is natural). Same for returns, callers (count then place), and components.
- Status: open

### src/lir/arc_solve.zig:1291: `computeOutcomeRestitution` creates three hash maps and a work list per (procedure, parameter) and two `DenseMap`s per procedure, all keyed by dense statement indices
- category A/B; runs per procedure with a tag-union return and owned refcounted params x per such param: `AutoHashMap(u32, OutcomeBitAccum)` (line 1394), …; impact low-medium; risk low
- Finding: `OutcomeWalkState{stmt, present, discriminant}` is a (dense stmt index, bool, u32) triple; the walk is over the proc's own statements, so a stamped column indexed by `statement_indices` (which the function already maintains as a `DenseMap`) with a small per-statement list of seen discriminants would avoid hashing. All of these are created and destroyed per iteration instead of hoisted out of the loops with …
- Fix: hoist the maps/lists to function scope and clear per iteration (or use epoch-stamped arrays sized to `proc_stmts.len`); replace `seen: AutoHashMap(OutcomeWalkState)` with a per-statement stamp + discriminant pair (a statement is re-walked only when `(present, discriminant)` differs; a tiny inline array of 2 entries per statement covers the common case).
- Status: open

### src/lir/arc_solve.zig:4874: `UniquenessComponentTask.execute` creates two `std.heap.ArenaAllocator`s per component from `worker.allocator` and never uses `worker.scratch`
- category A; runs per ownership component per settlement wave (the structure's `result` is retained across waves of one settlement but destroyed between settlements, i.e. at …; impact medium; risk low-medium
- Finding: `owner.arena = std.heap.ArenaAllocator.init(allocator)` (persistent per component) and `.scratch = std.heap.ArenaAllocator.init(allocator)` (reset per wave). `computeUniquenessDetailed` then allocates ~30 bitsets/arrays/lists per call from the scratch arena (lines 5505-5600). The task callback only receives `worker.allocator` (line 4861).
- Fix: the per-wave scratch should be `worker.scratch` (dies with the callback; `result.uniqueness` is copied into `updates` in the owner arena: verify `Uniqueness` is deep-copied before the scratch dies, currently `result.uniqueness` points into `arena` = the workspace scratch, so the arena must stay until `settleUniquenessWithOptions` reads the verdict; keep that one as `SingleThreadArena` and move the truly transient …
- Status: open

### src/lir/arc_solve.zig:5236: every settlement wave rebuilds and `dupe`s the whole `ret_conditions` row table
- category D; runs per wave (typically 2-5 per settlement).; impact low; risk low
- Finding: `flat_rows` is rebuilt from `proc_rows` for every proc, then `allocator.dupe` into `solution.ret_conditions` and the old table freed, even when no row changed.
- Fix: only rebuild when some `changed` fired; or keep `flat_rows` as the live table and swap.
- Status: open

### src/lir/arc_solve.zig:2213: `resolveJumpIndices` hashes `(proc, JoinPointId)` pairs to find a jump's join
- category B; runs per join and per jump, once per program.; impact low; risk low
- Finding: `AutoHashMapUnmanaged(JoinKey{proc: u32, id: JoinPointId}, JoinSlot)`; `JoinPointId` is a dense per-store id (the store allocates them from `firstFreshJoinPoint`), so a `DenseMap(JoinPointId, JoinSlot)` cleared per proc (joins are enumerated proc by proc in `proc_join_bodies`) suffices.
- Fix: `DenseMap(JoinPointId, u32)` reused across procs with `clearRetainingCapacity`, resolving each proc's `pending_jumps` right after its joins are recorded (requires grouping pending jumps by proc, which the lift already does in order).
- Status: open

### src/lir/arc_solve.zig:6675: `computeSccs` copies `direct_calls` into an `edges: ArrayList([2]u32)` only to count and place them
- category D; runs once per program, O(call sites).; impact low; risk low
- Finding: `edges` duplicates `(caller, callee)` from `solver.direct_calls` before building the CSR; the two counting passes can read `direct_calls` directly.
- Fix: iterate `solver.direct_calls.items` in both passes.
- Status: open

### src/lir/arc_solve.zig:4587: `UniquenessWorkspace.use_answers: AutoHashMap(UseQuery{stmt, local}, bool)` memo keyed by a dense id pair
- category B; runs per ordered-use query (per consume of a refcounted local), persistent per component.; impact low; risk low
- Finding: the memo key is `(CFStmtId, LocalId)`. Queries are issued per statement for a handful of locals, so a `DenseMap(CFStmtId, small list)` or a stamped per-statement slot would avoid hashing; given the lint only bans `HashMap(...Id, ...)` as the first argument, this escapes it by using a struct key.
- Fix: index by statement (dense within the component via `stmt_to_dense`) into a per-statement `ArrayList`-span of `(local, answer)` pairs.
- Status: open

### src/lir/use_order.zig:342: `initTopology` reads every statement twice by value, duplicates `member_list`, and (for owned tables) allocates and memsets ten store-sized arrays
- category A/D/E; runs once per uniqueness structure (whole store, owned tables without marks), plus per procedure wherever `UseOrder.initFromStore`/`init` is used with `.owned` …; impact low (per program) / medium where per-proc owned topologies …; risk low
- Finding: `Scratch.initTablesInDomain` allocates `jump_join`, two `RowIndex` (4 arrays), `seen`, `unresolved`, `mark_gen`, `block_first`, `block_pos`, `block_last` each sized to `store.cfStmtCount()`/`localCount()` and memsets them (10 x 4 bytes x statements). `member_list` is built then `dupe`d into `members` (line 379). The joins/jumps resolution loop calls `store.getCFStmt` twice per statement (lines 395-402).
- Fix: for the per-procedure `.owned` case use `.compact` (already exists) or the caller's `Scratch`; `members = try member_list.toOwnedSlice(allocator)` instead of dupe; resolve jumps from a single pass that records joins and jumps together.
- Status: open

### src/lir/use_order.zig:673: `initFromStore` builds one `ArrayList` per procedure then `toOwnedSlice`s it, with a `DenseMap(CFStmtId, void)` visited set
- category F/A; runs per call (whole-store or per proc).; impact low; risk low
- Finding: `lists: ArrayList([]const CFStmtId)` where each entry is an owned slice (one allocation per proc) that is freed after the topology is built; a flat list + starts would be one allocation.
- Fix: flat CSR as above.
- Status: open

### src/lir/arc_dismantle.zig:242: `Dismantles` keeps seven hash maps keyed by dense `CFStmtId`/`LocalId`, probed per statement during every emission
- category B; runs `takeAt(stmt)` / `ownedOnlyTake(stmt)` / `completeTakeAt(stmt)` are consulted per `assign_ref` statement per emission (procedures x ownership variants); …; impact medium; risk low
- Finding: `takes`, `owned_only_takes`, `complete_takes: AutoHashMapUnmanaged(CFStmtId, …)`, `containers`, `owned_only_containers`, `projection_units`, `projection_aliases: AutoHashMapUnmanaged(LocalId, …)`. The `Unmanaged` spelling escapes the `HashMap(...Id` lint. The sets are sparse (few takes per program), but they are read on hot per-statement paths. A `DenseMap(CFStmtId, Take)` or a `take_bindings`-style dense column …
- Fix: `collections.DenseMap` for the statement- and local-keyed tables; for `takes`, additionally a `DynamicBitSet` over statements so the common miss is one bit test.
- Status: fixed (fix 17)

### src/lir/arc_dismantle.zig:1651: `compute` allocates ~25 store-sized arrays with memsets and several per-proc/per-statement hash maps (`param_bodies`, `read_kinds`, `body_states`, `proc_joins: []AutoHashMapUnmanaged`)
- category A/B/F; runs once per program; `proc_joins` is one hash map per procedure (2196-2201), `body_states: AutoHashMapUnmanaged(CFStmtId, FlowState)` (2241), `read_kinds: …; impact low-medium; risk low
- Finding: all keyed by dense ids. The ~25 `[]u32`/`[]bool` columns are fine (dense), but the `[]bool` ones (`is_param`, `explicit_init_join`, `owned_demand`, `take_bindings`, `joins_ready`) could be bitsets and `[]?CFStmtId` columns elsewhere are 8 bytes where a u32 sentinel suffices.
- Fix: `DenseMap` (or epoch-stamped columns, since `join_scan_epochs` already exists for exactly this) for the statement/local-keyed maps; flatten candidate lists into shared pools with spans; bitsets for the bool columns.
- Status: open

### src/lir/range_prove.zig:1024: `Pass.init` builds ~45 maps/lists and four heap scratch objects per procedure, then `deinit`s them all
- category A; runs per procedure (`runProcWithScratch` at line 121 is the per-task entry used by `proc_passes`): every `.range`-admitted procedure.; impact medium; risk low
- Finding: `Pass.init` creates 22 `collections.DenseMap`s, 9 `AutoHashMap`s, ~15 `ArrayList`s, `LoopScan.init`, and `create`s three `LoopFacts` + one `MergeEnvBounds` with `ensureCapacity` to fixed caps, every time. They come from the task's scratch arena so freeing is cheap, but every map starts empty and regrows its backing storage from zero for each procedure; the `DenseMap`s grow their sparse pages per procedure as well.
- Fix: keep one `Pass` per worker lane in `LaneState` (as `BodyClone.AnalysisScratch` already is) and add a `resetForProc()` that `clearRetainingCapacity`s every container; the four scratch objects become plain fields.
- Status: open

### src/lir/range_prove.zig:849: six `AutoHashMap(u64, …)` keyed by packed pairs of dense ids
- category B; runs per fact/field read/sum/loop within a procedure's proof rounds.; impact low-medium; risk low
- Finding: `loop_bounds` (join x param), `field_values` (struct root x field), `int_fields`, `seeded_param_roots` (join x param), `sum_roots` (root pair), `join_assigned` (join x local). Packed `u64` keys hide dense structure; `field_values`/`sum_roots` could be per-root lists (`NodeId` is dense and already has `sum_heads`/`fwd_heads` per-root linked lists), `loop_bounds`/`seeded_param_roots`/`join_assigned` per-join lists …
- Fix: per-root / per-join small lists (linked through parallel `next` arrays like `sum_heads`) instead of hashing.
- Status: open

### src/lir/known_tag_jump.zig:80: `runProc` creates a `std.heap.ArenaAllocator` over the worker scratch (itself an arena) on every rewrite iteration and rebuilds its `Analysis` (4 DenseMaps + read counts) from scratch
- category A; runs per procedure per rewrite iteration (the `while (true)` loop runs once per deleted join-parameter write).; impact low-medium; risk low
- Finding: `var arena = std.heap.ArenaAllocator.init(allocator)` wraps the scratch allocator that `proc_passes` already provides as a per-task arena (serial path) or `worker.scratch`; `Analysis.init` then re-runs `countReachableReadsWithAllocator` and re-walks the body each iteration.
- Fix: drop the inner arena (allocate directly from `allocator`), and reuse `Analysis` containers across iterations with `clearRetainingCapacity`; use `countReachableReadsWithScratch` with the lane's `AnalysisScratch` (the `.tag_fusion` lane already has one: proc_passes.zig:114 passes `analysis` only to `TagCaseFusion`, not to `KnownTagJump`).
- Status: open

### src/lir/scalarize_joins.zig:283: `Pass.init` per procedure creates 20 `DenseMap`s, six of which map a local to an `ArrayList(CFStmtId)`
- category A/F; runs per `.scalarize`-admitted procedure (`runProc` line 86 builds a fresh `Pass`).; impact medium; risk low
- Finding: `field_reads`, `init_writes`, `tag_reads`, `tag_forward_writes`, `struct_forward_writes`, `alias_init_writes`, `join_params: DenseMap(LocalId, ArrayList(CFStmtId))`: one heap block per local that has any entry, per procedure; `StructBuild.builds`/`TagBuild.builds` lists likewise. `clearRetainingCapacity` at 377-398 exists for rounds within a procedure but the `Pass` itself is rebuilt per procedure. …
- Fix: lane-resident `Pass` reused across procedures; replace map-of-lists with a shared `ArrayList(CFStmtId)` pool + per-local `{start,len}` (the reads are collected in one walk, so grouping by local can be done with a counting pass, or with per-local linked lists through a parallel `next` array like `arc_dismantle.mention_edges`).
- Status: open

### src/lir/loop_append_promote.zig:350: per-procedure/per-candidate `DenseMap`s created and destroyed in inner functions, plus map-of-lists
- category A/F; runs per procedure and per candidate loop/family: lines 410 (`edge_values: DenseMap(CFStmtId, ArrayList(LocalId))` per join), 518/522 (`env`, `provenance` per …; impact medium; risk low
- Finding: each `DenseMap.init` + first `put` allocates its sparse page table and dense column; done tens of times per procedure.
- Fix: hoist into the `Pass` (lane-resident) and clear; pool the lists.
- Status: open

### src/lir/tag_case_fusion.zig:44: `JoinParamIndex.indices: AutoHashMap(u64, usize)` (in body_clone.zig:146) keyed by `JoinPointId`, and per-proc `DenseMap`s created in each analysis helper
- category B/A; runs `JoinParamIndex` is built per task in `proc_passes` (`.forwarding_join`, `.tag_fusion` lanes, line 125) and consulted per jump; the per-proc helper maps at …; impact low-medium; risk low
- Finding: `JoinParamIndex.record` does `indices.put(@backingInt(join.id), …)` and `get` hashes a dense id; `targets: AutoHashMap(u64, CFStmtId)` at line 70 likewise.
- Fix: `DenseMap(JoinPointId, LocalSpan)`; hoist helper maps into a per-lane pass state.
- Status: open

### src/lir/body_clone.zig:1304: `CloneFrame` is ~400 bytes (two inline `CFStmt`s + origin + four slices) and switch/str-match frames heap-allocate four slices each
- category C/F; runs per cloned statement (inlining, variant cloning, loop versioning, tag-case fusion arm copies).; impact low-medium (shrinks with the `CFStmt` repack); risk low
- Finding: `CloneFrame{source: CFStmt, built: CFStmt, origin: StmtOrigin, old_branches/branches/old_arms/arms: []…}` is pushed per statement onto `frames`; the 144-byte unions dominate. `old_branches`, `branches`, `old_arms`, `arms` are allocated per switch frame and freed in `CloneFrame.deinit`.
- Fix: keep `source` as `*const CFStmt` (the store is not mutated for prefix rows during a clone) and build `built` in place in the destination; pool branch/arm scratch in two `ArrayList`s with spans.
- Status: open

### src/lir/body_clone.zig:1172: `BodyCloner.initInternal` creates five `DenseMap`s per clone and `initWithInlineScopeOuterAndAllocator` builds a throwaway `JoinParamIndex` by walking the destination body
- category A/E; runs per inlined call (`single_use_inline`, `forwarding_join_inline`, `comptime_value_guards`), per variant clone (`cloneCallVariant`), per fusion arm copy.; impact low-medium; risk low
- Finding: `local_map`, `stmt_map`, `inline_scope_map`, `declared_joins`, `join_map`: five `DenseMap.init` per clone; `index.indexReachable(store, destination_body)` walks the entire destination procedure per inline just to find `next_join_point`.
- Fix: keep a cloner per lane/pass and `clearRetainingCapacity`; maintain the next fresh join id once per procedure (callers already hold a `JoinParamIndex` in most pass lanes).
- Status: open

### src/lir/body_clone.zig:123: `firstFreshJoinPoint` scans every statement of the store (by value) to find the maximum join id
- category E; runs once per `.forwarding_join` and `.tag_fusion` phase (2 x all statements), plus per `firstFreshJoinPoint` call elsewhere.; impact low; risk low
- Finding: iterates `0..store.cfStmtCount()` with `getCFStmt` to find `.join` statements. The store appends joins through `addCFStmt`, so it can track `max_join_point_id` in `noteStmtShapes` for free.
- Fix: store-maintained counter (updated on `addCFStmt`/`replaceCFStmt`/`appendBodyShard`/relocation); keep the scan as a debug assertion.
- Status: open

### src/lir/single_use_inline.zig:221: `inlineAt` dupes three local spans per inlined call
- category D; runs per single-use inline site.; impact low; risk low
- Finding: `source_args`, `call_args`, `source_frame` are `GuardedList.dupe`d out of the store even though the spans are immutable during the clone (the clone appends locals, which in release does not move `local_ids` while a span borrow is held only if no append happens: `addLocalSpan` is called later, so the dupe guards a real reallocation of `local_ids`).
- Fix: copy the three spans into one reused scratch `ArrayList(LocalId)` (lane-resident) instead of three allocations per site; or read `call_args` after the clone through indices.
- Status: open

### src/lir/trmc.zig:430: `Detection.joins: DenseMap(JoinPointId, JoinInfo)` created per procedure; `runProc` builds a `Scratch` per task
- category A; runs per `.trmc`-admitted procedure.; impact low; risk low
- Finding: `Scratch.init(scratch_allocator)` per `runProc` (line 102) and `DenseMap.init` per `Detection` (430); the serial path in `proc_passes` resets the arena per task so capacity is not retained.
- Fix: lane-resident `Scratch`.
- Status: open

### src/lir/tag_reachability.zig:36: one `AutoArrayHashMapUnmanaged(u32, void)` per local (`ValueInfo`/`TagSet` columns sized to `localCount()`)
- category F/B; runs once per program, but `local_info = alloc(ValueInfo, localCount)` embeds a hash map header per local (~40 bytes each) and each local with a known tag set gets …; impact medium (memory: ~40 bytes x locals; time: per-round rescans); risk low
- Finding: `TagSet.values` holds the known discriminants of a local's tag-union value. Discriminant sets are small and bounded by the union's variant count; a `u64` bitmask (with `all` for > 64 variants, which the layout's variant count bounds) or a span into a shared pool would make `ValueInfo` a few words with no per-local allocation. `collectUseCounts` (line 383) re-scans all statements per fixpoint round …
- Fix: bitmask-based `TagSet` (fall back to `all` beyond 64 variants); count uses once outside the round loop unless rewrites change them.
- Status: open

### src/lir/reachable_procs.zig:109: ten store-sized `[]bool` / `[]?Id` columns, `?CFStmtId` = 8 bytes where a u32 sentinel suffices
- category C; runs once per program.; impact low; risk low
- Finding: `visited_stmts`, `reachable_stmts: []bool` (bitsets would be 8x smaller), `old_stmt_to_new: []?LIR.CFStmtId` (8 bytes per statement), `old_to_new: []?LirProcSpecId`, `old_static_data_to_new`, `old_export_to_new`, `static_data_exports: []?…`. `remapReachableProcBodies` (489) memsets `visited_stmts` (whole store) once more.
- Fix: `DynamicBitSetUnmanaged` for the bool columns, `u32` with `maxInt` sentinel for the optional id columns.
- Status: open

### src/lir/immortal_locals.zig:231: `successor: []CFStmtId` + `dropped: []bool` + `visited` bitset per program; fine. (no finding)
- 
- Status: open

### src/lir/forwarding_join_inline.zig:196: `forwardedJoins` returns an `AutoHashMap(ForwardTarget, void)` and the pass builds `DenseMap`s + `ReadCounts` per procedure
- category A/B; runs per `.forwarding_join`-admitted procedure.; impact low; risk low
- Finding: `targets: AutoHashMap(ForwardTarget, void)` where `ForwardTarget` wraps a `JoinPointId`; `shared_stmts: DenseMap`, `fresh_definitions: ReadCounts` with its own `DenseMap` (297), `nodes`/`successors` lists: all per call.
- Fix: `DenseMap(JoinPointId, void)`; lane-resident scratch.
- Status: open

### src/lir/checked_pipeline.zig:1212: root entrypoint names are `dupe`d per root and `test_plan_metadata` is duplicated twice (lines 1252, 1315)
- category D; runs per compile (per root).; impact low; risk low
- Finding: `names[i] = try allocator.dupe(u8, symbol)` for each root metadata entry; the symbols are already owned by the checked data for the pipeline's lifetime. `RootTestPlanMetadata` is `dupe`d in two places for the same data.
- Fix: borrow with an explicit lifetime note, or dupe once.
- Status: open

### src/lir/program.zig:689: `boxy_frozen_method_origins: AutoHashMapUnmanaged(LirProcSpecId, …)` and `expect_site_ids: AutoHashMapUnmanaged(ExpectSiteKey, …)`
- category B; runs per Boxy frozen method / per expect site (once per program).; impact low; risk low
- Finding: the first is keyed by a dense proc id (lint evaded via `Unmanaged`); sparse, so a `DenseMap` is the stated replacement. `ExpectSiteKey` is a structural key (fine).
- Fix: `collections.DenseMap(LirProcSpecId, BoxyFrozenMethodOrigin)`.
- Status: no change: both maps hold a handful of entries per program, and as unmanaged maps with empty defaults they keep every `Program` literal allocator-free, which `DenseMap` cannot

### src/lir/lir_image.zig:1230: `copyProgramIntoBuffer` copies every column through `allocator.dupe`/`alloc` into the image buffer (expected for serialization; no finding beyond noting that `cf_stmts` at 144 bytes/stmt dominates image size, so the `CFStmt` repack shrinks LIR images 2-3x as well).
- 
- Status: open

### src/backend/dev/LirCodeGen.zig:955: `local_locations: AutoHashMap(u32, ValueLocation)` is a hash map keyed by dense `LocalId`, probed on every local read and written (with an undo journal entry) on every bind
- category B/C; runs per local operand read (`generateLookup`, line 9259: one probe per operand of every statement), per local definition (`setLocalLocation`, line 1830: `get` + …; impact high (the dev backend is the `roc run`/`roc …; risk medium (wide mechanical change inside a …
- Finding: design.md ("LLVM's store-indexed local-slot ... columns" and "Wasm's local bindings use a store-indexed column") describes exactly the structure the other two backends use; the dev backend instead hashes every access. The value type `ValueLocation` is a 32-byte union with 16-byte alignment because of `immediate_i128: i128` (every other variant is <= 12 bytes), so each map entry is 4 + 32 bytes plus metadata, and …
- Fix: a store-indexed `ArrayList(ValueLocationRow)` with a per-procedure scope stamp (copy of `src/backend/wasm/Storage.zig`'s design: rows live only for the stamp that wrote them, writes journal the replaced row, scope exit unwinds the journal); `immediate_i128` moved to a side list (`i128_immediates: ArrayList(i128)` with a u32 index) so `ValueLocation` becomes 16 bytes / align 8. Verify: `restoreStmtEnv` semantics …
- Status: fixed (fix 20)

### src/backend/dev/LirCodeGen.zig:20498: `compileProcSpec` deep-clones six maps/lists per procedure to save the caller's state
- category D/A; runs per compiled procedure (`compileAllProcSpecs` calls it for every proc; nested compilation from RC helpers/thunks re-enters).; impact high; risk low (top-level fast path), medium …
- Finding: `saved_local_locations = self.local_locations.clone()`, `saved_join_points = join_points.clone()`, `saved_stmt_locations = stmt_locations.clone()`, `saved_join_point_jumps = cloneJoinPointJumpsMap(...)` (clones every inner `ArrayList(JumpRecord)`), `saved_join_point_params.clone()`, three `ArrayList.clone`s, plus `dupe` of `local_location_undo`. `std.HashMap.clone` allocates at the source's *capacity* and copies all …
- Fix: distinguish top-level from nested compilation: at top level, assert the maps are empty and skip the clones; for nested compilation (helpers), use the scope-stamp column so entering a nested scope is O(1) (as wasm `Storage.beginScope`) and `stmt_locations`/`join_points` get a scope stamp as well. Verify: nested proc compilation during `compileAllProcSpecs` (RC helper drain is deferred via worklist, so nesting is …
- Status: partly fixed (fix 20: the location map clone is allocation-free when empty)

### src/backend/dev/LirCodeGen.zig:980: `stmt_locations: AutoHashMap(u32, usize)`, `join_points: AutoHashMap(u32, usize)`, `join_point_params: AutoHashMap(u32, LocalSpan)`, `precomputed_overflow_results: AutoHashMap(u32, void)`, `proc_registry: AutoHashMap(u32, CompiledProc)`, `boxy_dict_thunks: AutoHashMap(u32, usize)` keyed by dense `CFStmtId` / `JoinPointId` / `LirProcSpecId`
- category B; runs `stmt_locations` get+put per generated statement (`generateStmt`, line 22463-22470); `proc_registry.get` per call site; `join_points`/`join_point_params` per …; impact medium; risk low
- Finding: all keyed by dense ids over domains the store already numbers. `CompiledProc` is 56 bytes and `proc_registry` is also iterated to find procs by id in `compileAllProcSpecs` (`contains` + `get` per proc).
- Fix: `stmt_locations` -> store-indexed `ArrayList(u32)` of code offsets with a per-proc stamp (statements of one procedure are visited once; a `maxInt` sentinel + stamp avoids clearing); `proc_registry` -> `[]CompiledProc` indexed by proc id (procs are registered up front for all ids at 20267-20279, so it is already dense); `join_points`/`join_point_params` -> `DenseMap(JoinPointId, …)` or stamped columns; …
- Status: fixed: the per-procedure tables are `DenseMap`s and the write-only join table is gone

### src/backend/dev/LirCodeGen.zig:1039: `join_point_jumps: AutoHashMap(u32, std.ArrayList(JumpRecord))` is a map of lists, cleared by iterating all values and deep-cloned per procedure
- category F; runs per jump (append), per join (patch loop), per procedure (`clearFunctionControlFlowState` walks every list; `reset` likewise; `compileProcSpec` clones every …; impact medium; risk low
- Finding: `JumpRecord{location: usize}` is one word; a flat `ArrayList(struct{join: u32, location: u32})` sorted/bucketed at join emission, or a per-join linked list through a parallel `next` array, removes the per-join heap block and the clone.
- Fix: flat list of `(join, location)` pairs; patch by scanning the proc's pending jumps for the join (jumps per proc are small) or bucket by join at `join_patch` time.
- Status: open

### src/backend/dev/LirCodeGen.zig:1437: `ValueLocation` is 32 bytes / align 16 because of `immediate_i128`; stored per local in the map, per arg in `ArgInfo`/`FrozenCallArg`/scratch buffers, and as `?ValueLocation` (48 bytes) in the undo journal
- category C; runs per local binding, per call argument, per undo record.; impact medium; risk low
- Finding: variants: `general_reg` 1, `float_reg` 2, `vector_reg` 2, `stack{i32, ValueSize, Idx}` 12, `stack_i128` 4, `stack_str` 4, `list_stack{i32,i32,u32}` 12, `immediate_i64` 8, `immediate_f32/f64` 4/8, `immediate_i128` 16 (align 16), `noreturn` 0. Without `immediate_i128` the union is 12 + tag -> 16 bytes, align 4.
- Fix: side table for i128 immediates (`immediate_i128: u32` index into `ArrayList(i128)`), or split into `immediate_i128_lo/hi: [2]u64` with align 8 (24 bytes). `LocalLocationUndo.prev: ?ValueLocation` -> use the `noreturn`-like sentinel variant `absent` instead of optional.
- Status: open

### src/backend/dev/LirCodeGen.zig:1983: procedure bodies are `dupe`d out of the code buffer and re-appended to prepend the prologue (five sites: 1983, 15056, 20732, 24204-24487)
- category D; runs per compiled procedure / helper / entrypoint: O(body bytes) copy out + `shrinkRetainingCapacity` + copy back, then every pending list is shifted …; impact medium (code buffer is copied 2x per procedure today; with …; risk low
- Finding: `const body_bytes = self.allocator.dupe(u8, buf[body_start..body_end])` then `appendSlice(body_bytes)`. The copy is needed because the prologue size is only known after the body; an alternative is to reserve the maximum prologue size up front and emit the prologue in place (padding with a jump or nops) or to emit the prologue into a small scratch and `insertSlice` once (`ArrayList.insertSlice` does one memmove …
- Fix: `self.codegen.emit.buf.insertSlice(body_start, prologue_bytes)` with the prologue emitted into a stack buffer (prologues are tens of bytes), dropping the body dupe; the relocation/patch shifting stays.
- Status: open

### src/backend/dev/LirCodeGen.zig:16782: per hosted call / per entrypoint `std.heap.ArenaAllocator` for ABI lowering, plus per-call `alloc`s (`arg_offsets`, `frozen_args`, `arg_plans`, `hidden_offs`, `param_slots`, `ordered`)
- category A; runs per hosted call site (16782), per entrypoint (24969), per direct call (`frozen_args` 15947/21394), per dict call (`arg_plans` 18373, `hidden_offs` 18384), per …; impact low-medium; risk low
- Finding: small per-call heap allocations from the GPA; `generateCall` already uses a `BufferFirstAllocator` stack buffer for `arg_locs`/`arg_layouts` (15832), so the pattern exists.
- Fix: use `base.Scratch`-style reusable buffers (`scratch_arg_infos` etc. already exist on the generator) for `frozen_args`, `arg_plans`, `hidden_offs`, `arg_offsets`; replace the ABI arena with the worker scratch or a `SingleThreadArena` reset per call.
- Status: open

### src/backend/dev/LirCodeGen.zig:22645: switch/str-match continuation states are `create`d and `destroy`ed per control statement
- category F; runs per switch / boolean switch / str_match / str_match_set statement.; impact low-medium; risk low
- Finding: `SwitchState`, `SwitchState1`, `StrMatchState`, `StrMatchSetState` are heap objects referenced by several `StmtWork` items (so they need stable addresses while the work stack is live); `SwitchState.end_patches: ArrayList(usize)` and `StrMatchSetState.miss_patches/end_patches` are further per-switch lists.
- Fix: store the states in a per-generator `ArrayList(SwitchState)` and reference by u32 index from `StmtWork` (indices stay valid across growth); keep patch lists as spans in one shared `ArrayList(usize)` since switches nest LIFO.
- Status: open

### src/backend/dev/LirCodeGen.zig:17401: `layoutsInterchangeable` builds an `AutoHashMap(u64, void)` seen-set per query
- category A/B; runs per `list_map_can_reuse` lowering (per such statement).; impact low; risk low
- Finding: a visited set keyed by a pair of dense `layout.Idx` is created and freed per call; the answer depends only on the two layouts and could be memoized per generator (`DenseMap(layout.Idx, …)` per root, or a small cache keyed by the pair).
- Fix: memoize per module in a `DenseMap` keyed by the (a,b) pair packed as a dense-id pair into a per-`a` list; reuse the seen set across calls.
- Status: open

### src/backend/dev/StackPlan.zig:687: `assignSlots` creates an `AutoHashMap(u32, SizeClass)` (each with a `PriorityQueue` + `ArrayList`), an `order` list + sort, a `reservations` list and a `std.heap.ArenaAllocator` for tree nodes, per procedure
- category A/F; runs per procedure stack planning (every compiled proc).; impact low-medium; risk low
- Finding: all of these are rebuilt per call; the size-class map is keyed by slot size (a structural key, acceptable) but there are only a handful of sizes; `nodes = std.heap.ArenaAllocator.init(self.allocator)` allocates one `Reservations.Node` per live range (`create` per range, line 727).
- Fix: keep `classes`, `order`, `reservations` and the node pool in the worker's `StackPlan.Indexes` (already lane-shared) and `clearRetainingCapacity`; use `SingleThreadArena` or a pooled node list indexed by u32.
- Status: open

### src/backend/dev/StackPlan.zig:71: `Value` carries an inline `Ranges` with an embedded `ArrayList(Range)` (24 bytes) plus a one-range inline buffer
- category C; runs per planned local per procedure.; impact low; risk low
- Finding: `Value{local 4, representative 4, definitions 4, mutable 1, size 4, slot ?u32 8, ranges: {single [1]Range 8, single_len usize 8, multiple ArrayList 24}}` ≈ 72 bytes; `single_len: usize` could be a bool/u8 and `slot: ?u32` a sentinel, and `multiple` a `{start,len}` span into a shared `ArrayList(Range)`.
- Fix: span-based ranges; `Value` -> ~32 bytes.
- Status: open

### src/backend/dev/ProcArtifact.zig:276: artifact sets `dupe` every code buffer, reference list, relocation and symbol name string into an arena (`copy`), and `ContentNames` keeps four `StringHashMap`s keyed by symbol names
- category D/B; runs per artifact (per compiled procedure/helper) at pack creation and at `collect`; per data item name lookups.; impact low-medium (object-cache/pack path, once per build); risk low
- Finding: `ProcArtifact.Set.copy`/`collect` deep-copy code bytes, refs, symbolic refs, lines, named relocations (each `relocation.name` dupe'd separately: 261-262, 299-300), data items (name + bytes). `ContentNames.items/names/digests/active: StringHashMapUnmanaged` keyed by data names where the content digest (`[32]u8`, a `...Key`) would be the natural key and the generated names are derived from it (line 363 `allocPrint` …
- Fix: intern relocation/symbol names once per set (string interner with u32 ids) instead of per-relocation `dupe`; key `ContentNames` by digest; sort regions in place on a borrowed slice.
- Status: open

### src/backend/dev/ObjectFileCompiler.zig:742: `procs_by_identity: AutoHashMap(ProcIdentity, LirProcSpecId)` and `packs: AutoHashMap(*const Set, PackState{placed: AutoHashMap(u32, usize)})` (map keyed by pointer whose values are maps)
- category F/B; runs once per object-file emission; `placed` keyed by artifact index (dense within a set).; impact low; risk low
- Finding: nested hash maps allocated lazily per pack; `placed` could be a `[]u32` sized to the set's artifact count. `ProcIdentity` is a legitimate structural key.
- Fix: `placed: []u32` (sentinel for unplaced) per set, in an `ArrayList(PackState)` indexed by set ordinal.
- Status: open

### src/backend/dev/HostSplice.zig:53: `data_names`, `data_offsets`, `hosted_stubs`, `code_symbols: StringHashMap` keyed by symbol strings, with `dupe` per name (94, 184, 429)
- category B/D; runs per spliced symbol (object-cache hit path).; impact low; risk low
- Finding: names are hashed and duplicated on each registration; a `SymbolTable.Id`-style interner (the dev backend already has `SymbolTable` with `indices: StringHashMapUnmanaged(Id)`) would intern once and index by id afterwards.
- Fix: intern names through the generator's `SymbolTable` and key the side tables by `SymbolTable.Id` in `DenseMap`s.
- Status: open

### src/backend/dev/NativeProcCompiler.zig:246: per-fragment `std.heap.ArenaAllocator` and `AutoHashMap(u64, void)` helper-seen set; `cached_procs/cached_helpers` keyed by identity/u64
- category A; runs per native fragment compilation (per procedure in the in-process compile path).; impact low; risk low
- Finding: `var scratch = std.heap.ArenaAllocator.init(allocator)` per fragment; `requireHelpers` with `seen: AutoHashMap(u64, void)` per call; `indexes = worker.allocator.create(StackPlan.Indexes)` per worker (fine).
- Fix: `worker.scratch` / `SingleThreadArena`; keep `helper_seen` per worker and clear.
- Status: open

### src/backend/dev/RunImage.zig:324: image building copies code symbols, relocation records, data symbols and names from temporary lists into the image allocator (expected one-time serialization; the temporaries could be sized up front from the inputs: `relocation_records`/`code_symbols` grow by `append` without `ensureTotalCapacity` although the counts are known from the codegen's lists)
- category G; runs once per run image.; impact low; risk low
- Fix: `ensureTotalCapacity(relocations.len)` etc.
- Status: open

### src/backend/dev/object/coff.zig:215: `sortFunctions` dupes and sorts the function table, then builds a second list and sorts again
- category D/E; runs once per object (Windows unwind data).; impact low; risk low
- Fix: sort once in place on an owned copy; merge ranges in the same pass.
- Status: open

### src/backend/dev/ExecutableMemory.zig:110: same double copy+sort for Windows runtime function tables (once per executable image). Low.
- 
- Status: open

### src/backend/llvm/MonoLlvmCodeGen.zig:4700: a fresh `std.heap.ArenaAllocator` per direct fast call, per frame-replacing call, per fast proc declaration, per proc body, per entrypoint/shim, and per cold shim
- category A; runs per `assign_call` to a fast-ABI callee (`emitFastDirectCall` 4700, `emitFrameReplacingCall` 4770), per procedure (`declareFastProcSpec` 2273, 2380, …; impact medium-high (one arena + several allocations per call site …; risk low
- Finding: each creates `std.heap.ArenaAllocator.init(self.allocator)` (atomic bump allocations) only to compute `fastSignature`/`layout.abi.lower` results that are discarded immediately. Per-call `param_types: ArrayList(Type)` and `arg_layouts`/`arg_ptrs` allocations (4645-4666, 4863, 3275) accompany them.
- Fix: one `SingleThreadArena` field on the generator reset per call (`reset(.retain_capacity)`), and memoize `fastSignature` per procedure (it depends only on the proc spec) in a `[]?FastSignature` column indexed by proc id, computed once in `declareFastProcSpec`.
- Status: open

### src/backend/llvm/MonoLlvmCodeGen.zig:292: `proc_registry`, `fast_registry: AutoHashMap(u32, Function.Index)`, `boxy_dict_thunks`, `static_refcounted_backings`, `static_data_globals`, `static_data_export_rows`, `debug_types: AutoHashMap(u32, …)` keyed by dense ids
- category B; runs `fast_registry.get`/`proc_registry.get` per call site; `static_data_globals` per static-data read; `debug_types` per debug type lookup.; impact medium; risk low
- Finding: procedure ids, static-data ids and layout ids are dense store domains; design.md names columns for these. `proc_registry`/`fast_registry` are populated for every proc up front, so a `[]Function.Index` (sentinel `.none`) is exact.
- Fix: `ArrayList(Function.Index)` indexed by proc id (two columns), `DenseMap` for the static-data and layout-keyed caches.
- Status: fixed (fix 51)

### src/backend/llvm/MonoLlvmCodeGen.zig:367: `join_points: AutoHashMap(u32, JoinInfo)`, `compiled_joins: AutoHashMap(u32, void)`, `stmt_incoming_counts: AutoHashMap(u32, u32)`, `stmt_entry_blocks: AutoHashMap(u32, Block.Index)` keyed by `JoinPointId`/`CFStmtId`, and `collectStmtIncomingCounts`/`inlineShapeOf` build an `AutoHashMap(u32, void)` visited set per procedure
- category B/A; runs `stmt_incoming_counts`/`stmt_entry_blocks` are probed per statement during emission; the visited maps are created per procedure (3457) and per inline-shape …; impact medium; risk low
- Finding: same pattern as the dev backend: per-statement hash probes in the emission loop where the store's statement ids are a dense domain; a stamped u32 column (`incoming_count`, `entry_block`) sized once per module (as `local_slot_storage` already is for locals, lines 3079) would mirror the design the file already follows for locals.
- Fix: store-indexed stamped columns, same as `local_slot_storage`/`deferred_str_capture_storage`.
- Status: open

### src/backend/llvm/MonoLlvmCodeGen.zig:7943: per-switch/join/eq-helper state objects via `create` (`SwitchState`, `InitializedPayloadSwitchState`, `JoinState`, `StructEqState`, `ListEqState`, `TagEqState`) and per-switch `alloc` of `branch_blocks`/`case_blocks`, `capture_slots`, `toOwnedSlice`d `match_bodies`/`captures`
- category F; runs per control statement / per generated equality helper.; impact low-medium; risk low
- Finding: one heap object per statement on an explicit work stack; same fix as the dev backend (index into a per-generator `ArrayList`, spans into a shared block list).
- Fix: indexed state records + shared `ArrayList(Block.Index)` with spans.
- Status: open

### src/backend/llvm/MonoLlvmCodeGen.zig:9321: static-data relocations `dupe`d and sorted per export; symbol-name `dupe` per `static_data_symbols`/`static_bytes` key (9363, 9404) and `storage = alloc(data_offset + bytes.len)` + memset per datum
- category D; runs per static-data export (once per program).; impact low; risk low
- Finding: `sorted = dupe(relocations)` + `std.mem.sort` per export; the relocation producer could emit them sorted (they are produced by walking the datum's bytes in order). Key strings are `dupe`d into the GPA for `StringHashMap` keys.
- Fix: sort in the producer; intern names once.
- Status: open

### src/backend/llvm/MonoLlvmCodeGen.zig:2263: `allocPrint` of every procedure's symbol name twice (fast and packed variants)
- category D; runs per procedure.; impact low; risk low
- Finding: `ProcIdentity.writeSymbolName` exists precisely to avoid formatting (LIR.zig:72); the LLVM backend formats `"{s}{s}{s}.fast"` with `allocPrint` per proc (2263, 2466) instead of writing into a fixed buffer (the length is constant: prefix + 32 hex + ".fast").
- Fix: stack buffer of `symbol_name_len + prefix.len + 5` bytes via `writeSymbolName`.
- Status: open

### src/backend/wasm/WasmCodeGen.zig:10169: `assign_struct` lowering allocates two arrays per struct literal statement
- category A; runs per `assign_struct` statement.; impact medium (per-statement heap traffic in the wasm backend); risk low
- Finding: `field_val_locals = alloc(u32, fields.len)` and `field_val_types = alloc(ValType, fields.len)` from the GPA on every struct build, freed at the end of the statement. Likewise per join `param_locals = alloc(u32, jp_params.len)` (8678, stored in `join_point_param_locals: AutoHashMap(u32, []u32)` and never pooled), per hosted call `arg_offsets`/`hosted_arg_layouts` (7734, 7811, 7900), per erased call …
- Fix: a reusable `base.Scratch`-style buffer pair on the generator (`scratch_field_locals`, `scratch_field_types`) with `top()`/`clearFrom`; store join param locals as spans into one `ArrayList(u32)` per procedure.
- Status: open

### src/backend/wasm/WasmCodeGen.zig:234: `pending_bodies: AutoHashMap(LocalFunctionIndex, CodeBuilder)`, `registered_procs`, `proc_table_indices`, `boxy_dict_thunk_table_indices: AutoHashMap(u32,u32)`, `function_symbols_by_index`, `static_str_offsets`, `static_data_symbols`, `join_point_depths/param_locals/state_locals`, `precomputed_overflow_results`, `active_stmt_generations`, `stmt_generation_counts: AutoHashMap(u32, …)` keyed by dense ids
- category B; runs `registered_procs.get` per call site, `join_point_*` per join/jump, `pending_bodies` per function (and `std.sort.heap` over its keys at 1452 to get …; impact medium; risk low
- Finding: the wasm backend keeps its local bindings in a store-indexed column (`Storage.zig`) but every other per-id table is a hash map. The per-statement generation-count guard (`stmt_generation_counts`) is a hash probe + insert per statement in release builds.
- Fix: columns indexed by proc id / function index / join id / statement id (stamped where per-proc); gate the generation-count bookkeeping behind `builtin.mode == .debug` (it is documented as debug-only at lines 317-320).
- Status: fixed (fix 51)

### src/backend/wasm/WasmCodeGen.zig:6432: `func_type_cache: StringHashMap(u32)` keyed by a formatted signature string rebuilt into `func_type_key_scratch` on every lookup
- category D/E; runs per call-signature lookup (per indirect/direct call needing a type index, per function declaration).; impact low; risk low
- Finding: the key is built by formatting param/result types into a byte buffer, hashed, and `dupe`d on insert. A signature is a small vector of `ValType` (u8); hashing the raw byte vector without formatting (`std.hash.Wyhash` over `[]ValType`) and keying on that, or interning signatures by `(params, results)` spans in a `StringHashMapUnmanaged` whose key is the raw type bytes, avoids the formatting.
- Fix: key on the raw `[]const ValType` bytes (params ++ 0xFF ++ results) built by `appendSlice`, not `fmt`.
- Status: open

### src/backend/wasm/WasmCodeGen.zig:1520: per hosted call / per hosted wrapper `std.heap.ArenaAllocator` for ABI lowering (1520, 7741, 7826)
- category A; runs per hosted call site and wrapper.; impact low; risk low
- Fix: shared `SingleThreadArena` reset per call, as for the LLVM backend.
- Status: open

### src/backend/wasm/WasmModule.zig:1005: `setFunctionBody` `dupe`s each function body; `extractFunctionBodies` (2284-2305) re-`dupe`s every body out of `code_bytes` into one allocation per function
- category D/F; runs per function (twice: once when set, once when the module is re-parsed/relinked).; impact low-medium (object-size proportional copies per link step); risk low
- Finding: `func_bodies: ArrayList(struct{body: []u8})` holds one heap block per function; the bodies already live contiguously in `code_bytes`, so `{offset, len}` spans into it (or into a single owned byte buffer) would avoid per-function allocations and the `gpa.free` loop.
- Fix: store bodies as spans into one `ArrayList(u8)`; `setFunctionBody` appends.
- Status: open

### src/backend/wasm/WasmModule.zig:934: `UndefinedDataSymbols: StringHashMap(ArrayList(u32))` and `SymbolNameIndex`/`public_counts: StringHashMap` keyed by symbol-name strings
- category F/B; runs per data symbol during linking (per module merge).; impact low; risk low
- Finding: map of lists keyed by strings; symbols could be interned to u32 ids once per module (`WasmLinking.symbol_table` is already a dense table), then grouped with a CSR by name id.
- Fix: intern names (one `StringHashMap(u32)`), then dense per-name-id lists.
- Status: open

### src/backend/wasm/WasmModule.zig:2796: function-liveness passes allocate three `[]bool` arrays per `fn_count` and `@memset(next_pass)` per iteration
- category E/C; runs per link, per liveness iteration (`@memset(next_pass, false)` at 2907 inside the loop).; impact low; risk low
- Finding: `live_flags`, `current_pass`, `next_pass: []bool` could be bitsets (8x smaller memsets) or the iteration could use a worklist (no per-iteration clear at all).
- Fix: worklist-based reachability (push callees when first marked live) replaces the fixed-point passes.
- Status: open

### src/backend/wasm/WasmModule.zig:3843: relocation entries `dupe`d then sorted with a context per section emission (once per link)
- category D; impact low; risk low
- Status: open

### src/backend/wasm/WasmModule.zig:762: `addFuncType` dupes the param slice per function type (fine, interned once). (no finding)
- 
- Status: open

### src/backend/wasm/WasmCodeGen.zig:1877: `called_fns = alloc(bool, liveFunctionCount)` (and 18459) as `[]bool` where a bitset suffices
- category C; impact low; risk low
- Status: open

### src/lir/arc_certify.zig:2094: certifier state owns four `std.heap.ArenaAllocator`s plus `memo: AutoHashMap(MemoEntry, void)` and `seen_outcomes: AutoHashMap(u32, void)`; `graph_arena` per procedure (4445)
- category A/B (debug builds only: `certifyStoreOrPanic` is invoked under `builtin.mode == .debug` at src/lir/arc.zig:719); runs per procedure in debug builds.; impact n/a for production; risk n/a
- Finding: noted for completeness; it does not run in release compiles. The same slimming of `ExactBitSet`/`OwnedSet` benefits it.
- Status: open

### src/lir/proc_passes.zig:224: the serial path uses `std.heap.ArenaAllocator` (atomic) for scratch; the parallel path's `worker.scratch` is whatever `post_check_task_executor` provides
- category A; runs per phase (one arena) in serial mode; per task in parallel mode.; impact low-medium; risk low
- Finding: `var scratch = std.heap.ArenaAllocator.init(allocator)` reset per task; `base.SingleThreadArena` avoids the atomics on every scratch allocation of every pass. The parallel executor's worker scratch (src/base/post_check_task_executor.zig:69) is passed through to the passes correctly; ARC and uniqueness tasks ignore it (see arc.zig:836 and arc_solve.zig:4874 findings).
- Fix: `base.SingleThreadArena` here; thread `worker.scratch` into ARC/uniqueness.
- Status: fixed (fix 15)

### src/lir/LirStore.zig:1300: `spanRange` + `add*Span` helpers append element-by-element after `ensureUnusedCapacity` (pattern across `addLocalSpan`, `addU64Span`, `addU32Span`, `addCFSwitchBranches`, `addStrMatchArms`, `addJoinPointSpan`)
- category G; runs per span append (per statement with operands).; impact low; risk low
- Finding: each `add*Span` reserves then loops `append` (re-checking capacity each time) or `appendSlice`; `appendSliceAssumeCapacity` after the reservation is the cheaper form. (Read at lines 1300-1360; the loop form is used for the id-typed columns.)
- Fix: `appendSliceAssumeCapacity`.
- Status: open

### src/backend/dev/aarch64/CodeGen.zig:86: `branch_site_index: AutoHashMapUnmanaged(usize, u32)` maps every branch patch location to its `branch_sites` index; `retired_call_veneers: AutoHashMapUnmanaged(usize, usize)`
- category B; runs per emitted branch (`registerBranchSite` puts; `branchSiteIndex`/`callVeneer`/`registerAssembledCallSite` get/contains per patch), plus `remove` + `put` per …; impact low-medium (aarch64 only; per branch); risk low
- Finding: branch sites are appended in strictly increasing `loc` order (emission order), so `branch_sites` is sorted by `loc` and a binary search (as `firstEntryAtOrAfter` does for pending calls in LirCodeGen.zig:14112) answers `loc -> index` without a hash map; shifting a body keeps the order.
- Fix: binary search over `branch_sites` (or a `(loc -> index)` lookup that exploits that locs are 4-byte aligned instruction offsets: `loc / 4` indexes a stamped `ArrayList(u32)` grown with the code buffer).
- Status: open

### src/backend/dev/CallingConvention.zig:149: `CallBuilder.stack_args: ArrayListUnmanaged(StackArg)` is allocated per call with stack arguments and freed in `deinit`
- category A; runs per emitted C-ABI/builtin call that overflows registers (182 `Builder.init` sites in LirCodeGen.zig).; impact low; risk low
- Finding: the builder is a stack value created per call; its `stack_args` list grows from empty each time. A `base.Scratch(StackArg)` on the generator (like `scratch_arg_infos`) would retain capacity.
- Fix: pass a reusable scratch list into `Builder.init` / keep the builder's list on the generator.
- Status: open

### src/lir/arc_liveness.zig, src/lir/arc_claims.zig: used only by the debug certifier (`arc_certify.zig`); `arc_liveness.solve` already uses `collections.SingleThreadArena`. Not scored.
- 
- Status: open

### src/lir/lir_image.zig:445: `interned_layouts: StringHashMap(layout.Idx)` keyed by a serialized layout digest string when a mapped image rebuilds a layout store
- category B/D; runs per interned layout on image view (once per loaded program).; impact low; risk n/a
- Finding: the key is a string built into `scratch_intern_key`; the layout store's own interning (`layout_mod.Store`) is reused here, so this mirrors the layout store's design and is out of scope for this directory; noted only.
- Status: open


## Interpreter, compile-time evaluation, coordinator, CLI, LSP, reporting
### src/eval/interpreter.zig:600: Frame local lookup is a binary search on every local read/write
- category E; runs per local access (several times per statement; `getLocalChecked`, `Frame.setLocal`, `setLocalDesc`, `localDesc`, `isAssigned` all go through `slotIndex` -> …; impact high (every operand of every interpreted statement during …; risk low
- Finding: `Frame.locals` is the proc's sorted `frame_locals` span and `slots` is parallel to it; `findSlotIndex` (lines 600-614) binary-searches `locals` for the `LocalId` on every access (`low/high` loop over `GuardedList.at(self.locals, mid)`), and `slotIndex` is called by every local read and write. A proc with 200 locals pays ~8 probes (each a bounds-checked store read) per operand, so a two-operand statement costs ~25 …
- Fix: Give each LIR proc a dense, contiguous local range so that `slot = local_id - proc.first_local` (LIR lowering already assigns locals per proc; the `frame_locals` span is sorted by id, so if lowering guarantees the proc's locals are contiguous the span becomes a base+len), or add a `frame_slot: u32` column to the LIR local table filled when `frame_locals` is built so the interpreter indexes …
- Status: fixed (fix 18)

### src/eval/interpreter.zig:3243: Three arena arrays plus descriptor resolution per low-level statement
- category A; runs per statement (every `assign_low_level`; arithmetic other than +,-,* , comparisons, all string/list builtins); impact high; risk low
- Finding: `collectLocalValues` (line 4052), `localLayouts` (4062) and `localBoxyDescs` (4072) each `self.arena.allocator().alloc(...)` an array sized by the arg count and `localBoxyDescs` additionally calls `resolveOptionalBoxyDescRef` per argument even when the op is a plain scalar op that never reads `arg_descs`. The arena is a bump allocator that is only reset per host entry (`resetRetainedExecution`, line 1224; `eval` at …
- Fix: Low-level arg counts are bounded (the LIR op table maxes out at a handful); use fixed `[max_low_level_args]Value/Idx/?*Desc` stack buffers in `execStmtChain` (or interpreter-owned reusable scratch slices) and pass slices of them into `evalLowLevel`. Resolve `arg_descs` lazily only for ops whose implementation consults `ll.arg_descs` (grep shows a small set: boxy list/str/inspect/hash/eq ops). Verify: `evalLowLevel` …
- Status: fixed (fix 26)

### src/eval/interpreter.zig:2629: Five diagnostic fields written per statement from three store lookups
- category E; runs per statement; impact medium (removes ~30 bytes of stores and 3 column loads per …; risk low
- Finding: At the top of the `execStmtChain` loop (lines 2629-2633) the interpreter does `self.active_stmt_loc = self.store.stmtLoc(current); self.active_stmt_region = self.store.stmtRegion(current); self.active_proc_id = frame.proc_id; self.active_stmt_id = current; self.active_stmt_inline_scope = self.store.stmtInlineScope(current);`. `stmtLoc`/`stmtRegion`/`stmtInlineScope` are three separate column reads (SourceLoc is 12+ …
- Fix: Record only `active_stmt_id` (and `active_proc_id`) per statement and derive loc/region/inline scope from the id at the failure site and at the call sites that need them. The failure paths already have the stmt id (`getLocalChecked` does exactly this at line 1711). Verify that `active_stmt_loc` consumers in `compile_time_host`/`runtime_host` are updated to call a `self.store.stmtLoc(self.active_stmt_id)` accessor …
- Status: fixed (fix 52)

### src/eval/interpreter.zig:2580: initFrame scans every local's layout to pre-assign ZST slots
- category E; runs per call; impact medium (recursive compile-time functions with many locals …; risk low
- Finding: `initFrame` memsets all slots and then loops `for (0..locals.len)` doing `self.store.getLocal(local_id).layout_idx` and `self.layout_store.getLayout(layout_idx).tag == .zst` for every frame local (lines 2583-2590). Each call therefore does O(frame locals) store+layout lookups even though a proc's ZST set is a static property of the proc.
- Fix: Precompute per proc, once (lazily in a column indexed by `LirProcSpecId` or at LIR build time), the list/bitset of ZST frame locals, and in `initFrame` only flip those bits; or store a per-proc template `[]LocalSlot` that is memcpy'd into the acquired slot array. Verify nothing mutates a proc's local layouts after lowering.
- Status: fixed (fix 28)

### src/eval/interpreter.zig:4195: Every local-to-local assignment allocates and copies a fresh value
- category A/D; runs per statement (`assign_ref .local`, call argument binding at 2470, call result materialisation); impact high for (1)+(2) (every value construction), medium for (3); risk low for (1)+(2), medium for (3)
- Finding: `materializeLocalValue` -> `boxy_runtime.materializeLocalValue` (boxy_runtime.zig:6691) always does `hooks.allocValue(target_layout)` (arena alloc + `@memset(slice, 0)` in `allocAlignedByteSlice` line 1420-1431) and then `storage.copyFrom(value, size)` where `copyFrom` (value.zig:52) is a byte-by-byte loop, not `@memcpy`. So a simple `x = y` of a 24-byte list header costs an arena bump, a 24-byte memset, and a …
- Fix: (1) In `copyFrom`, use `@memcpy` when the ranges do not overlap (they never do for a fresh allocation) and keep the overlap loop only for the overlapping case. (2) Skip the zero memset in `allocAlignedByteSlice` for callers that immediately overwrite the whole value (`materializeLocalValue`, `rocStrToValue`, `evalI64Literal`, `allocPointerIntValue`, scalar low-level results) by adding an `allocUninit` variant; keep …
- Status: partly fixed (fix 27: memcpy for non-overlapping copies and no zeroing of fully copied values; aliasing not done)

### src/eval/interpreter.zig:1420: Evaluation arena is never released within an evaluation
- category A/H; runs per eval (memory growth proportional to total statements executed, not live values); impact high (memory) / medium (CPU: better cache locality, fewer …; risk medium
- Finding: All interpreter values (`alloc`), argument arrays, descriptor bindings, hidden pointer cells (`allocPointerIntValue`, 8 bytes each) and formatted strings are bump-allocated from `self.arena` which is reset only in `resetRetainedExecution` (line 1224) between host entries; `eval` (line 1555) never resets it and `evalProcSpec` has no frame-scoped release. A compile-time loop of N iterations retains N copies of every …
- Fix: Mark the arena at `evalProcSpec` entry and release back to the mark when the call returns, after copying the returned value (and its `desc`) out to the caller's frame storage: results are already copied by `materializeLocalValue`/`materializeDrivenCallResult` at the call site, so the only values that must survive a frame are the return value (copy it before release) and anything that escaped into Roc heap …
- Status: open

### src/eval/interpreter.zig:4707: erasedDescriptorBindingsFromCapture does an O(params x offsets) key scan per erased call
- category E; runs per erased callable call; impact low-medium (closures passed to boxy/erased APIs in CTFE); risk low
- Finding: For every descriptor param (`params`) the loop `for (offsets) |entry| if (!std.meta.eql(entry.key, param.key)) continue;` (lines 4712-4722) linearly scans all `erased_arg_desc_offsets` comparing composite keys, and the result array is arena-allocated per call. The pairing of params to offsets is static per proc.
- Fix: Precompute, when the LIR tables are built (or lazily per `LirProcSpecId` in a column), the offset index for each param (`param_offset_index: u32` or `none`), so the per-call loop is a direct index. Keep `bindings` on a reusable interpreter scratch list instead of the arena.
- Status: open

### src/eval/interpreter.zig:4637: Hidden capture/reuse pointer args each get an 8-byte arena value per erased call
- category A; runs per erased callable call, per dictionary call (`assign_dict_call` hidden args), per boxy desc local write (`allocPointerIntValue` at 2697, 2820, 3025, 3100 …; impact medium; risk medium
- Finding: `allocPointerIntValue` (line 9621) does `self.alloc(.opaque_ptr)` -> arena alloc + memset + write for each pointer-sized hidden argument and each descriptor local. Interpreter `Value` is just a pointer, so a pointer-valued local needs a cell to point at, but the cells are never reclaimed (see arena finding) and cost an allocator call each.
- Fix: Store pointer-valued locals inline: add a small inline buffer to `LocalSlot` (e.g. `inline_word: usize`) and have `allocPointerIntValue`'s callers write into the slot's inline word and point `val.ptr` at it; or keep a per-frame array of `usize` cells sized by the proc's count of `opaque_ptr` locals (known from layouts). Verify that pointer cells are never read after the frame is released (results are copied out by …
- Status: open

### src/eval/interpreter.zig:5373: callHostedProc allocates with the general allocator and a std ArenaAllocator per hosted call
- category A; runs per hosted function call (runtime interpretation; `roc run`/`roc test` through the interpreter); impact medium for effectful interpreted programs; risk low
- Finding: `arg_offsets = try self.allocator.alloc(u32, arg_layouts.len)` / `defer free` (lines 5373-5374), then `var arena_state = std.heap.ArenaAllocator.init(self.allocator)` (line 5421) created and destroyed per call for `host_trampoline.call`. The std arena pays atomics on each allocation and a fresh arena allocates its first chunk from the GPA every time.
- Fix: Allocate `arg_offsets` from `self.arena` (the hosted call's args buffer already comes from there) and pass `self.arena.allocator()` (or a retained `base.SingleThreadArena` reset per hosted call) to `host_trampoline.call`. Verify `host_trampoline.call` does not retain the memory past return (it builds a register image; the image is consumed synchronously).
- Status: fixed (fix 52)

### src/eval/interpreter.zig:5629: Str.inspect builds into a temporary ArrayList then copies into a RocStr
- category D; runs per `inspect` evaluation; impact low-medium; risk low
- Finding: `inspectBoxyValue` creates `std.ArrayList(u8).empty` on the eval arena, appends the whole rendering, then `makeRocStr(out.items)` copies the bytes into a fresh Roc heap string, and `out.deinit` on an arena is a no-op so the ArrayList's growth doublings all stay allocated in the arena.
- Fix: Keep one reusable `inspect_scratch: std.ArrayListUnmanaged(u8)` on the interpreter (general allocator, `clearRetainingCapacity` per use) so growth capacity is retained across inspects; or render directly into a `RocStr` grown with `RocStr.reserve`/`appendSlice` to avoid the final copy. Verify nested inspects (to_inspect methods calling inspect) by using a depth-indexed scratch or save/restore of the scratch length.
- Status: no change: an inspect can run a user `to_inspect` that inspects again, so one reused buffer would be appended to by the nested render while the outer one is in progress

### src/eval/boxy_runtime.zig:5990: appendScalarInspect dupes a stack-formatted number into the arena before appending
- category D; runs per scalar rendered by inspect; impact low; risk low
- Finding: Every branch formats into a stack buffer (`float_buf`, `int_buf`, `dec_buf`) and then `try self.eval_arena.dupe(u8, ...)` the result (lines 6018-6047), `defer self.eval_arena.free(text)` (no-op on an arena), and finally `out.appendSlice(self.eval_arena, text)`. The dupe is pure waste; the stack buffer outlives the append.
- Fix: Compute `text` as a slice of the local stack buffers and `appendSlice` directly; delete the dupe/free.
- Status: open

### src/eval/boxy_runtime.zig:6058: appendQuotedInspectBytes appends byte by byte
- category G; runs per string byte rendered by inspect; impact low; risk low
- Finding: `for (bytes) |byte| { if (...) try out.append('\\'); try out.append(byte); }` performs an `ArrayList.append` (capacity check each) per byte.
- Fix: `try out.ensureUnusedCapacity(self.eval_arena, bytes.len + 2)` then append runs between escape characters with `appendSliceAssumeCapacity`, growing only when an escape is found.
- Status: open

### src/eval/interpreter.zig:9086: numToStr formats via allocPrint into the arena then copies into a RocStr
- category D; runs per `Num.to_str` on integers; impact low; risk low
- Finding: `std.fmt.allocPrint(arena, "{d}", ...)` allocates up to ~40 bytes in the arena, then `makeRocStr(formatted)` copies the bytes into a Roc string; the arena copy is never reclaimed.
- Fix: `std.fmt.bufPrint` into a `[48]u8` stack buffer (i128 max is 40 chars) and `makeRocStr` from that. Same pattern as `appendScalarInspect` which already uses stack buffers.
- Status: fixed (fix 52)

### src/eval/interpreter.zig:1460: marshalAbiArgs sorts argument indices with an O(n^2) selection sort and three arena arrays
- category E/A; runs per entrypoint evaluation (`runEntrypoint`/`eval` with `arg_ptr`); impact low; risk low
- Finding: Lines 1472-1484 sort `sorted_indices` with a nested `for (0..n) for (i+1..n)` loop comparing alignments, after allocating `args_buf`, `sorted_indices`, `arg_offsets` from the arena.
- Fix: Argument counts are tiny so the sort is cheap, but the offsets are a static property of the entrypoint's layouts: compute them once per entrypoint (the native ABI already has `ErasedCallArgsPlan`/`getErasedCallArgOffsets` for the same purpose, line 4645) and reuse. Low priority.
- Status: open

### src/eval/interpreter.zig:1534: runEntrypoint allocates arg_layouts with the GPA per entrypoint call
- category A; runs per entrypoint call from a host; impact low; risk low
- Finding: `self.allocator.alloc(layout_mod.Idx, arg_ids.len)` + `defer free` to build the layout list that `localLayoutsFromSpan` (line 4083) already knows how to build from the arena.
- Fix: Use `localLayoutsFromSpan(proc.args)` (arena) or a stack buffer; delete the GPA round trip.
- Status: fixed (fix 52)

### src/eval/interpreter.zig:582: LocalSlot wastes 7 bytes of padding per slot
- category C; runs per frame slot (allocated per call via the pool; memset per call); impact low-medium (33% less frame memory, smaller memset per call); risk low
- Finding: `LocalSlot { assigned: bool, val: Value (one pointer), desc: ?*const BoxyTypeDesc }` is 24 bytes with 7 bytes padding after the bool; `initFrame` memsets every slot on every call.
- Fix: Fold `assigned` into the pointer (use a null/sentinel `val.ptr` to mean unassigned, since `Value.zst` already uses a sentinel) giving a 16-byte slot, or move `assigned` to a separate `[]bool`/bitset parallel array that is cleared with one memset of n/8 bytes. Verify every `slot.assigned` reader (`getLocalChecked`, `localDesc`, `isAssigned`, `setLocalDesc` debug check).
- Status: fixed (fix 52)

### src/eval/interpreter.zig:374: free_frame_slots is an ArrayList of ArrayLists indexed by slot count
- category F; runs per call (acquire/release); impact medium; risk low
- Finding: `free_frame_slots: ArrayListUnmanaged(ArrayListUnmanaged([]LocalSlot))` keeps a separate pool per distinct frame size; `releaseFrameSlots` grows the outer list with `appendNTimes` to `slots.len + 1` entries on first sight of a large frame and each pool holds individually GPA-allocated slot arrays.
- Fix: Since calls are strictly LIFO, replace the pool with a single contiguous slot stack (`ArrayListUnmanaged(LocalSlot)` + a `usize` top): `acquire` bumps the top by `len` (growing once), `release` pops. No per-size pools and no per-frame heap objects. Verify erased callables re-entering the interpreter from a host callback still nest LIFO (they do; `execution_depth` tracks nesting).
- Status: open

### src/eval/interpreter.zig:552: setjmp per builtin invocation
- category H; runs per builtin call that can crash (~80 `enterCrashBoundary` sites, e.g. every list/str op); impact medium; risk medium
- Finding: Each builtin op that can call `roc_crashed` wraps itself in `enterCrashBoundary()` which installs a jump buffer and `setjmp`s; the `eval` entry (line 1577) already installs an outer jump buffer. The per-op boundary exists so that a longjmp does not skip the Zig `defer`s (`call_depth -= 1`, `releaseFrameSlots`, `call_stack.pop`) between the op and `eval`.
- Fix: If the frame-slot stack (previous finding) and `call_stack` are made restorable from a saved depth, the outer boundary at `eval` can restore them after a longjmp (`self.call_depth = 0; slot_top = saved; call_stack.len = saved`) and the per-op boundaries can be dropped. Each `setjmp` is ~a few dozen instructions plus a 200-byte jmp_buf write, paid per list/str op. Verify that no other per-frame `defer` performs …
- Status: open

### src/eval/compiler_host.zig:80: Every Roc heap allocation during interpreter CTFE is inserted into and removed from a hash map
- category B/H; runs per `roc_alloc`/`roc_dealloc`/`roc_realloc` during compile-time evaluation (every list/str/box allocation); impact high (two hash operations per Roc allocation across all …; risk low
- Finding: `CompilerHost.rocAlloc` does `allocateBytes` then `self.allocations.put(@intFromPtr(ptr), {size, alignment})`; `rocDealloc` does `fetchRemove`; `rocRealloc` does alloc + fetchRemove + memcpy + free + put (lines 80-115). The map exists for two reasons: `rocRealloc` needs the old size (the RocOps realloc callback does not pass it), and `freeRemainingAllocations` frees leftovers at deinit. …
- Fix: Store the size and alignment in a header word immediately before the returned pointer (allocate `header + alignForward(size)`), read it back in realloc/dealloc, and drop the map. For leftover freeing in `CompilerHost`, back allocations with a `base.SingleThreadArena` (as `CompileTimeHost` already does) so deinit is a single arena release; or keep an intrusive doubly linked list through the headers if individual …
- Status: fixed (fix 14)

### src/eval/runtime_host.zig:153: Runtime host tracks every allocation in a hash map too
- category B/H; runs per Roc allocation in interpreted `roc run`/`roc test` runs (`allocation_tracker: AutoHashMap(usize, AllocationInfo)`, `allocation_call_count`); impact medium; risk low
- Finding: Same pattern as the compiler host (lines 470-510 `allocateTrackedBytes`/`freeTrackedBytes`, `freeRemainingAllocations` iterating the map); the tracker powers `checkForLeaks`, which is a test-only concern but the code runs in production interpretation.
- Fix: Same header-word scheme; keep a live-count counter for `checkForLeaks` (count > 0 means leak) instead of a map, and an intrusive list only when `freeRemainingAllocations` must free leftovers.
- Status: open

### src/eval/compile_time_finalization.zig:2952: A full Interpreter and CompilerHost are constructed per literal root
- category A/F; runs per literal root (`evalInterpreterLiteralRoot`, line 2380, is called once per literal root plan); impact medium; risk low
- Finding: `InterpreterProgram.fork` does `allocator.create(InterpreterProgram)`, `CompilerHost.init` (a fresh `AutoHashMap`), and `Interpreter.initWithBoxyTables` which `allocator.create(InterpreterRocEnv)` and creates two `SingleThreadArena`s; the child is `deinit`ed after a single `eval`. Each literal root therefore pays interpreter construction and tear-down (arena first-chunk allocation, hash map, boxy table binding) for …
- Fix: Keep one child `InterpreterProgram` per lowering batch (or per thread) and reuse it across literal roots with `resetRetainedExecution` between roots (the comment says forks exist so a suspended evaluator keeps its own frames while another root publishes; a reusable pool of forks keyed by nesting depth preserves that). Verify `static_data_demand` nesting: a root that demands another root forks again, so the pool must …
- Status: open

### src/eval/compile_time_finalization.zig:1411: ComptimeCoverage.entryFor scans all entries linearly per branch hit
- category E; runs per compile-time branch hit recorded (every `if`/`match` branch taken during CTFE of a module with coverage); impact low-medium; risk low
- Finding: `entryFor` loops `for (self.entries.items) |*entry| if (entry.kind == site.kind and regionsEqual(entry.region, site.region)) return entry;` and the caller `record` is invoked for every `ComptimeBranchHit` collected from each root. With S distinct sites and H hits this is O(S*H).
- Fix: Index entries by `ComptimeSiteId` (a dense id the hit already carries, `interpreter.comptime_branch_hits[i].site`) with a `DenseMap(ComptimeSiteId, u32)` or a column sized by the site count; the region/kind equality collapses to site identity. Verify that two different sites never share a region (if they can, keep the region key as a secondary check at insert time only).
- Status: open

### src/eval/compile_time_finalization.zig:3048: PublicationSlots.checked_heads is an array of ArrayLists grown with appendNTimes per slot
- category F/G; runs once per StaticSlotEnvironment init (per lowering batch / per module finalisation); impact low; risk low
- Finding: `checked_heads: []std.ArrayList(?StaticDataId)` has one heap-owned list per lowering module and `init` grows each list with `appendNTimes(null, raw + 1 - len)` while iterating `static_data_values` (line 3077), so a module whose root ids are visited in decreasing order reallocates repeatedly.
- Fix: The per-module root counts are known (`module.compile_time_roots.roots.len`); allocate one flat `[]?StaticDataId` with per-module offsets computed up front (prefix sums), eliminating the ArrayList-of-ArrayList and the incremental growth.
- Status: open

### src/eval/compile_time_finalization.zig:3213: failure_origins is sized by every CF statement in the program per environment
- category E; runs once per StaticSlotEnvironment init (per module finalisation batch); impact low-medium (memory proportional to program size per batch); risk low
- Finding: `failure_origins = try allocator.alloc(?ComptimeFailureOrigin, lowered.lir_result.store.getCFStmts().len)` + `@memset(null)`; `?ComptimeFailureOrigin` is a `{loc, region}` optional (~28 bytes), so this is ~28 bytes x every LIR statement in the lowered program just to record origins for the handful of statements that read a failed slot (`publishFailureOrigin`).
- Fix: Make it a `DenseMap(CFStmtId, ComptimeFailureOrigin)` (sparse pages, only touched statements materialised) since lookups are by dense `CFStmtId` and most statements never get an origin; the lookup sites (`compile_time_host.rocComptimeFailureRegion` line 298, interpreter `failure_origins[stmt]`) become a `get`.
- Status: open

### src/eval/const_store_writer.zig:333: One ArrayList plus three heap arrays per stored value node
- category F/A; runs per constant value node written to the const store (every element of a compile-time list/record/tag); impact medium (large compile-time constants, e.g. tables); risk low
- Finding: `startStore` creates `children = std.ArrayList(ValueRequest).empty` per node (line 300), and `pushStoreFrame` allocates `nodes`, `capture_types` and `requests = children.toOwnedSlice()` per frame (lines 349-355), freed in `freeStoreFrame` (236-238). A constant list of 10k records creates 10k ArrayLists and 30k heap arrays.
- Fix: Use one shared `ValueRequest` stack and one `ConstNodeId` stack on the Writer with frames recording `(requests_start, requests_len, nodes_start)` spans into them (frames are LIFO, so the spans are popped in order); `capture_types` likewise as a span into a shared buffer. Removes all per-node heap objects.
- Status: open

### src/eval/const_store_writer.zig:859: Packed scalar lists are converted through a temporary allocation
- category D; runs per packed scalar list constant; impact low; risk low
- Finding: `storePackedScalarList` allocates `bytes` of `len * width`, fills it element by element via `storeScalar` + `writePackedScalar`, then `addBlobData(bytes)` copies into the const store, then frees. `storeProductList` (line 772) has the same shape for non-identity plans.
- Fix: Reserve the blob region in the const store first (an `addBlobDataUninit(len)` returning a writable slice) and encode directly into it; for identity plans the little-endian fast path already avoids the copy.
- Status: open

### src/eval/native_root_export.zig:151: frozen_descriptors hashes a dense BoxyTypeDescId
- category B; runs per boxy descriptor referenced by a published root (per root export); impact low; risk low
- Finding: `frozen_descriptors: std.AutoHashMapUnmanaged(Program.BoxyTypeDescId, Destination)` hashes a dense `...Id`; the owning table size is `boxy_tables.type_descs.len`.
- Fix: `collections.DenseMap(BoxyTypeDescId, Destination)` (the builder is per-root and short-lived, so a sparse-page dense map is the design.md-prescribed shape).
- Status: fixed (fix 57)

### src/eval/native_root_export.zig:510: finish() dupes every node's name, bytes, relocations and capacities out of the arena
- category D; runs per node of every published compile-time root (per root); impact low-medium (static data size x2 per root); risk low
- Finding: The Builder allocates nodes in an `std.heap.ArenaAllocator` (line 69) and `finish` copies each node's `name`, `bytes`, `relocations.items` and `empty_list_capacities.items` into GPA allocations (lines 524-530), then a second pass dupes `.named` relocation symbol names again. Every exported byte is thus written twice.
- Fix: Build the nodes directly with the GPA (owning `ArrayList`s that become the export slices via `toOwnedSlice`), or hand the arena to the caller as the owner of the exports (the exports' lifetime is the finalisation, which already owns arenas). Also the `std.heap.ArenaAllocator` should be `base.SingleThreadArena` (atomics on every node alloc).
- Status: open

### src/eval/boxy_abi.zig:128: GlobalBoxyRuntime.procs hashes dense proc ids and uses std arenas for per-call scratch
- category B/A; runs per boxy ABI call (`procs.get(@backingInt(call.proc))` at lines 723 and 2625) and per allocation in `value_scratch`; impact medium (dev-backend CTFE and native boxy programs call …; risk low
- Finding: `procs: AutoHashMapUnmanaged(u32, RegisteredProc)` is keyed by the backing int of `LirProcSpecId` (a dense id, renamed to u32 to pass the lint); `desc_arena`/`value_scratch` are `std.heap.ArenaAllocator` (atomic ops per alloc). Each dictionary call does 4-6 scratch allocations (`call_args`, `hidden_values`, one 8-byte `slot` per hidden arg, `arg_ptrs`) at lines 2598-2627.
- Fix: `procs` -> a column `[]?RegisteredProc` (or `DenseMap`) sized by the proc table, indexed by `LirProcSpecId`; replace the two std arenas with `base.SingleThreadArena` (the ABI is single-threaded per runtime); the per-hidden-arg `scratch.alloc(usize, 1)` can be one `alloc(usize, raw_hidden.len)`.
- Status: open

### src/eval/boxy_runtime.zig:1765: adapterSourceTagVariants builds a hash map keyed by BoxyNameId per adapter merge
- category B; runs per adapter descriptor specialization (cached in `adapter_desc_specializations`, so per distinct source/target pair); impact low; risk low
- Finding: `std.AutoHashMapUnmanaged(LIR.BoxyNameId, LirProgram.BoxyTagVariant)` keyed by the dense interned name id, filled from the extension chain and consumed once.
- Fix: A `DenseMap(BoxyNameId, BoxyTagVariant)` or, since variant rows are sorted by name rank, a merge over sorted variant lists. Low frequency, low priority.
- Status: fixed (fix 57)

### src/eval/boxy_runtime.zig:1614: Descriptor specialization dupes nested-desc and variant slices into scratch to survive table growth
- category D; runs per runtime descriptor specialization (per boxy coercion that creates a new descriptor); impact low; risk low
- Finding: `scratch.dupe(LIR.BoxyDescRef, self.requireBoxyDescRefs(source.nested_descs))`, same for `target`, then per variant `scratch.dupe(BoxyTagPayloadDesc, ...)` (lines 1614-1700), each followed by `defer free`, because the loop appends to `runtime_boxy_desc_refs` which may reallocate the borrowed slice.
- Fix: Iterate by index into the owning table (`runtime_boxy_desc_refs.items[start + i]` re-read after each append) instead of copying, or `ensureUnusedCapacity` for the known total up front so the borrowed slices stay valid.
- Status: open

### src/eval/boxy_runtime.zig:1281: Descriptor template materialization allocates two scratch arrays per lookup even on cache hit
- category A; runs per closure-descriptor materialization (every evaluation of a `desc_ref .static` with captures); impact low-medium; risk low
- Finding: `capture_ids`/`capture_descs` are `scratch.alloc`'d, filled by `hooks.captureDescs`, used as the `DescMaterializationKey` for `desc_materializations.get(key)`, and freed, on every call including hits.
- Fix: Keep two reusable `ArrayListUnmanaged` scratch buffers on the runtime (cleared per call); only on a miss dupe into `descriptor_arena` as today.
- Status: open

### src/eval/compile_time_finalization.zig:3695: DevRootJob embeds a whole CompileTimeHost per root
- category C/F; runs per compile-time root (dev backend path); impact low-medium (memory per module with many roots); risk low
- Finding: `jobs = allocator.alloc(DevRootJob, const_roots.len)` where each job embeds `host: CompileTimeHost` (two arenas, a hash map, four lists, a jmp_buf of ~200 bytes, several atomics) plus `ret_buf` and a label snippet; all jobs are built before any runs, so N roots materialise N hosts up front.
- Fix: Keep hosts per worker lane (the executor has a fixed lane count) and store only `root`, `ret_buf`, `result` and the label per job; a job takes the lane's host when it runs. Verify the progress reporter reads only job-level atomics.
- Status: open

### src/eval/interpreter.zig:5906: rc_conformance snapshot plumbing in the statement loop
- category H (debug-only, noted for completeness); runs per low-level statement in debug builds only (`rc_conformance.enabled` is comptime false in release, so `beginStatement` returns null); impact none; risk none
- Finding: No release cost; included so the reviewer knows it was checked.
- Fix: none
- Status: no change: no release cost

### src/compile/coordinator.zig:1240: Packages are addressed by name string everywhere instead of a dense package id
- category B; runs per module event (every `handleResult`, `tryUnblock`, `externalImportReadiness`, `getExternalEnv/Artifact`, `checkedArtifactByKey`, `wakeDependents`) and per …; impact medium (coordinator thread is the serial bottleneck of a …; risk low
- Finding: `packages: std.StringHashMap(*PackageState)` is the only package index; `ModuleRef { pkg_name: []const u8, module_id }` (line 1228), every task/result carries `package_name: []const u8`, and the hot helpers do `self.packages.get(source_pkg)` + `source.shorthands.get(qualifier)` + `self.packages.get(target_pkg_name)` + `target_pkg.getPublicModuleId(module)` (four string hashes, lines 5367-5373) for every external …
- Fix: Assign each package a dense `PackageId` at registration (`packages` becomes `ArrayList(*PackageState)` + a name->id map used only at registration/parsing time); resolve each module's external import names once, when the import is discovered, into `{ target_pkg: PackageId, module_id }` stored on `ModuleState` (a `resolved_external_imports` column parallel to `external_imports`), so readiness is …
- Status: open

### src/compile/coordinator.zig:1319: cross_package_dependents keyed by a formatted "pkg:id" string with ArrayList values
- category B/D/F; runs per cross-package import edge (registration, line 5326) and per module completion (lookup, line 3907); impact low-medium; risk low
- Finding: `cross_package_dependents: StringHashMap(std.ArrayList(ModuleRef))`; `registerCrossPackageDependency` does `std.fmt.allocPrint(self.gpa, "{s}:{d}", ...)` for the key (freed again when the key already exists) and `wakeDependents`/`completeModule` does another `allocPrint` + free per completed module just to probe the map. Each value is a separately heap-allocated ArrayList.
- Fix: Key by `struct { pkg: PackageId, module: ModuleId }` (`AutoHashMap` on a packed u64, or better a per-module `dependents` list on `ModuleState` that already exists for local dependents: cross-package dependents can live in the same `dependents: ArrayList(ModuleRef)` once `ModuleRef` is id-based). No formatting, no allocation on the completion path.
- Status: open

### src/compile/coordinator.zig:3646: Cached checked-module install copies the env body, the artifact body, and the source
- category D; runs per checked-module cache hit (per module on warm `roc check`); impact high for warm builds (eliminates the dominant per-module …; risk medium (ownership plumbing)
- Finding: `installCachedCheckedArtifact` mmaps the entry (`loadRawBytesMapped`) and then `alignedAlloc` + `@memcpy` the env body (line 3637-3641), `module_alloc.dupe(u8, current_env.common.source)` (3646), and `alignedAlloc` + `@memcpy` the artifact body (3690-3694). The comment explains the two bodies need independent owners with 16-byte alignment and the artifact body's file offset is not 16-aligned. A warm check therefore …
- Fix: Write the cache entry with the artifact body padded to a 16-byte-aligned file offset (the header already records lengths, so add padding) and make the mapping refcounted (one `CacheData` owner with a count of two) so both the env and the artifact alias the copy-on-write mapping directly; the relocation writes already target a COW mapping. The source dupe can be avoided by moving `current_env.common.source` ownership …
- Status: open

### src/compile/coordinator.zig:3517: Checked-module cache store gathers into one heap buffer, then writes it
- category D; runs per module published (cold `roc check`, every module); impact medium (halves memory traffic of cache writes per module); risk low
- Finding: `storeCheckedModuleInCache` serializes env and artifact into two `CompactWriter`s (iovec lists), then `manager.allocator.alloc(u8, header + env_len + artifact_len)`, `writeToBuffer` both into it, and `storeRawBytes` writes the buffer to a temp file. The intermediate buffer is the full size of the module's serialized bytes, allocated and freed per module. `storeCanonicalizedModuleInCache` (3380-3415) does the same on …
- Fix: Have `CacheManager.storeRawBytes` accept the writers (or an iovec list) and `pwritev` the header + iovecs directly to the temp file; `CompactWriter` already models scatter-gather output. Verify the checksum/header fields do not depend on the gathered bytes (they only need lengths).
- Status: open

### src/compile/coordinator.zig:3450: Platform pairing cache uses std.heap.ArenaAllocator and copies the loaded body
- category A/D; runs once per build (store) and once per build (load); impact low; risk low
- Finding: `storePlatformPairingInCache` creates `std.heap.ArenaAllocator` (atomics per alloc) and `tryLoadCachedPlatformPairing` (3487-3497) `gpa.create`s a heap `std.heap.ArenaAllocator`, then `alignedAlloc` + `@memcpy` the artifact body out of the mapping.
- Fix: Use `base.SingleThreadArena`; alias the mapping as in the previous finding. Low frequency.
- Status: open

### src/compile/coordinator.zig:3305: checkedModuleCacheKey builds a full CheckedModules view per module just to hash a key
- category A/E; runs per module reaching type check (cache probe) and again inside `typeCheckModule`; impact medium (per module on every build); risk low
- Finding: `checkedModuleCacheKey` calls `CheckedModules.initForRootModule(self.gpa, env, imported_envs)` and `typed.modules.deinit()` solely to compute `checkedModuleKeyFromTypedModule`; the type-check task then builds the same `CheckedModules` again. The key input is the module source, identity, context and import keys (`cache_key.zig:55-70` shows `CheckedModuleArtifactKey.compute` needs only those).
- Fix: Compute the key from the explicit inputs (`source`, `ModuleIdentity`, `CheckingContextIdentity`, `direct_import_artifact_keys`) without instantiating `CheckedModules`; if `checkedModuleKeyFromTypedModule` needs the typed view for the identity digest, retain the view across the probe and the subsequent type-check task instead of building it twice.
- Status: open

### src/compile/coordinator.zig:5451: resolveModulePathWithAllocator builds the path through two temporaries
- category D; runs per local import discovered (per import edge, per parse task) and per module path resolution; impact low-medium; risk low
- Finding: Appends segments into an `ArrayList(u8)`, `toOwnedSlice`, then `std.fs.path.join(alloc, &.{ root_dir, rel })`, freeing the intermediate: three allocations and two copies for one path. `module_discovery.resolveLocalImportLogicalPath` (module_discovery.zig:190-226) feeding it also builds a `segments` ArrayList and `std.mem.join`s, so a single import costs five allocations before the `dupe`s at coordinator 5509/5530.
- Fix: Compute the final length (`root_dir.len + 1 + mod_name.len + ".roc".len`) and write directly into one `alloc`, replacing `/` with the path separator in place; make `resolveLocalImportLogicalPath` write into a caller-provided scratch buffer (the parse task has a `scratch` arena) instead of allocating segments.
- Status: open

### src/compile/coordinator.zig:5074: added_module_indices hashes dense resolved module indices
- category B; runs per module type-check preparation; impact low; risk low
- Finding: `buildTypecheckImportedArtifacts` dedupes `resolved_module_idx` (a dense u32 index into `imported_envs`) through `std.AutoHashMap(u32, void)`.
- Fix: A `std.DynamicBitSetUnmanaged` sized by `imported_envs.len` (known from `buildTypecheckImportedEnvsForEnv`), or a `[]bool` on the scratch arena.
- Status: fixed (fix 53)

### src/compile/coordinator.zig:2466: checkedArtifactByKey falls back to a linear scan of retired artifacts
- category E; runs per dependency key lookup when a key is not in the live index (after cache installs replace artifacts); impact low; risk low
- Finding: On a miss in `checked_artifact_index`, the code scans `retired_checked_artifacts.items` comparing 32-byte keys; the retired list grows for the build's lifetime.
- Fix: Insert retired artifacts into the same `checked_artifact_index` (value variant `.retired(index)`) or a second map; or drop retired entries once the frontend barrier passes (no worker can hold a view after that).
- Status: no change: only artifacts replaced by platform pairing or a cache install retire, a handful per build; an index would have to be kept exact through four retire paths, one of which recovers from allocation failure

### src/compile/coordinator.zig:2780: Checked-program preparation builds two [32]u8-keyed maps and sorts by package/module name
- category E/A; runs once per program finalization (per `finishCheckedProgram`) and per LSP re-check; impact low (medium for LSP responsiveness); risk low
- Finding: `prepareCheckedProgramModules` builds `dependencies: AutoHashMap([32]u8, void)`, collects every module with a pending evaluation into `entries`, sorts them by string names, builds `indices: AutoHashMap([32]u8, usize)` and runs a recursive DFS. Fine once per build, but the LSP calls this per document change.
- Fix: Keep a persistent topological order on the coordinator maintained incrementally as modules complete (each module completes after its imports, so completion order is already a valid order); then this is a filter over that order, no maps or sorts.
- Status: open

### src/compile/coordinator.zig:1411: (see eval section) ComptimeCoverage linear scan: listed there
- 
- Status: no change: listed under the eval section

### src/compile/coordinator.zig:928: PackageState keeps four StringHashMaps on the thread-safe allocator
- category B/C; runs per package (lookups per import); impact low; risk low
- Finding: `module_names`, `public_module_targets`, `physical_module_paths`, `shorthands` are `StringHashMap`s initialized with `thread_safe_allocator` (smp allocator) even though the coordinator thread is their only mutator; `getPublicModuleId` is called per external import probe (see package id finding).
- Fix: Once external imports are resolved to ids at discovery time these maps are only hit at registration; keep them but use the coordinator's gpa. Fold into the package-id finding.
- Status: open

### src/compile/coordinator.zig:662: ModuleState carries five ArrayLists and owned dupes of every import name
- category C/F/D; runs per module; impact low-medium (memory per module; fewer allocations per import); risk low
- Finding: `imports`, `external_imports` (each entry an owned `dupe`d string, lines 4157/4201/4242), `rejected_imports`, `dependents`, `reports` are five 24-byte ArrayList headers plus a bitset per module; `DiscoveredLocalImport` carries `import_name`, `module_name`, `path` as three separately owned strings per import (coordinator 5509-5540), later copied again into the module state.
- Fix: Intern import names in the module's own `ModuleEnv` string table (the names come from the AST which is already in the env) and store `Ident.Idx`/`StringLiteral.Idx` plus the resolved id; `path` is derivable from `module_name` + package root on demand (only used to open the file). Collapse the per-module lists into coordinator-wide flat spans (imports of all modules in one `ArrayList(LocalImportEdge)` with `(start, …
- Status: open

### src/compile/messages.zig:398: WorkerResult union sized by its largest variant fills the channel ring buffer
- category C; runs once per build (channel buffer of `DEFAULT_CAPACITY = 1024` entries) and per message copy; impact low (capacity 1024 x 250 B = 256 KB per channel, two …; risk low
- Finding: `WorkerResult` embeds `ParsedResult`/`CanonicalizedCachedResult` which hold 3-4 `std.ArrayList` headers (24 bytes each), several slices, a `[32]u8` key and `?watch_inputs.State`; the union is ~250 bytes and `Channel(T)` allocates `cap * @sizeOf(T)` up front (channel.zig:33) and copies the whole union on `send`/`recv` under the mutex.
- Fix: Box the large result payloads (`*ParsedResult` allocated by the worker from its result allocator, freed by the coordinator) so the union is pointer-sized; or shrink by replacing ArrayLists with slices (`toOwnedSlice` at the worker boundary). The channel copy under the mutex then shrinks from ~250 bytes to 16-24.
- Status: open

### src/compile/cache_manager.zig:114: Cache file path built with two joins (and a third allocPrint for the temp path)
- category D; runs per cache probe and per cache store (per module, twice per module on a cold build: canonicalized + checked); impact low-medium (removes ~7 heap allocations per module from the …; risk low
- Finding: `computeCacheFilePathIn` joins `entries_dir + subdir` into a temporary, then joins that with the filename; `ensureCacheSubdirWith` joins again; `storeRawBytesIn` then `allocPrint`s the `.tmp` path. Five allocations per store, two per probe, and `getCheckedArtifactCacheDir`/`getCanonicalizedModuleCacheDir` (`cache_config.zig:390/410`) re-join the version dir with "mod"/"can" on every call (coordinator 3380, 3519, …
- Fix: Cache the entries directories once on the `CacheManager` at init (they depend only on config); build the entry path with one `alloc` of exactly `dir.len + 1 + 2 + 1 + 62 + 4(.tmp)` bytes using `bufPrint` into a stack buffer (`[std.fs.max_path_bytes]u8`) since the path length is bounded.
- Status: open

### src/compile/compile_build.zig:1597: logicalModuleToPath allocates four times per module path
- category D; runs per module path lookup (`moduleToPath` is called per drained module report and per module during package registration); impact low; risk low
- Finding: `segs` ArrayList, `std.fs.path.join`, `allocPrint("{s}.roc")`, then `std.fs.path.resolve` each allocate; three of the four are freed immediately.
- Fix: Single `resolve` call with the pieces (`std.fs.path.resolve(gpa, &.{ root_dir, logical_with_slashes, ".roc" })` after writing the suffix into a stack buffer), or compute into a stack buffer and `resolve` once.
- Status: open

### src/compile/module_discovery.zig:70: Public surface extraction builds a StringHashMap of imports per header
- category B/A; runs per package header parsed (once per package) and per module in `extractImportsFromDeclIndex`; impact low; risk low
- Finding: `module_imports: StringHashMapUnmanaged(usize)` keyed by the module binding's ident text, built lazily per `exposes` list; keys come from `ast.env.getIdent(binding)` which is already an interned `Ident.Idx`.
- Fix: Key by `Ident.Idx` via a small `DenseMap`/array, or compare `Ident.Idx` directly since the exposed item's qualifier is also an ident in the same env (no string hashing at all).
- Status: open

### src/compile/package_resolution.zig:644: Version-solving state keys are serialized strings built with allocPrint per choice
- category D/B; runs per resolution round (once per build when packages are involved; rounds are few); impact low; risk low
- Finding: `serializeChoices` sorts keys, `allocPrint`s one line per chosen group into the arena, appends to an `out` list, and the result is used as a `states_seen` string key; `recordSeenUrlEdge` (904) `allocPrint`s a `"group\0major.minor.patch"` key per edge per round.
- Fix: Hash the sorted (group, version, hash) tuples into a 32-byte digest for `states_seen` instead of building strings; key `seen_version_hashes` by `struct { group_index: u32, version: Version }` after interning group names. Low frequency; arena-backed so leak-free, but allocates proportional to graph size per round.
- Status: open

### src/compile/coordinator.zig:2106: Transitive import view collection re-walks the DAG per module with a hash set
- category E; runs per type-check task (`collectTransitiveImportedViews` is called when building the task's available artifacts); impact medium for wide module graphs; risk low
- Finding: For every module about to be checked, the coordinator BFS-walks its whole transitive import closure, hashing each 32-byte artifact key into `seen` and looking each dependency up through `checkedArtifactByKey` (another 32-byte hash). For a module graph with depth d and fan-out f this is O(closure) per module, O(N * closure) per build; the closure of a module is a superset of its imports' closures, which were just …
- Fix: Memoize each artifact's transitive view list (artifacts are immutable once published): store `transitive_views: []ImportedModuleView` on the module state when it completes (union of imports' lists, deduped by a per-build epoch mark on `ModuleState` instead of a hash set), so a dependent only concatenates.
- Status: open

### src/cli/main.zig:8828: SpliceClosure.seen hashes dense artifact indices
- category B; runs per offered specialization in `packFileBytes` (`closure.of(artifact)` per spec proc, per pack written); impact low-medium (object-cache writes run once per build per …; risk low
- Finding: `seen: std.AutoHashMap(u32, void)` keyed by the artifact index into `set.artifacts` (dense); `of` clears it and `getOrPut`s per reached artifact, so the closure walk hashes every edge.
- Fix: `std.DynamicBitSetUnmanaged` sized `set.artifacts.len`, cleared with `setRangeValue`/`unsetAll` (proportional to the set, or track touched indices in `order` and unset only those).
- Status: open

### src/cli/main.zig:8746: packFileBytes uses std.heap.ArenaAllocator and two ProcIdentity hash maps per pack
- category A/B; runs per pack written (per module per build when the object cache is cold); impact low; risk low
- Finding: `conditions_arena = std.heap.ArenaAllocator.init(allocator)` (atomics); `artifact_by_identity: AutoHashMap(ProcIdentity, u32)` and `literal_converters: AutoHashMap(ProcIdentity, void)` hash 32-byte identities, and `SpliceClosure.init` builds `procs`/`thunks`/`helpers` maps over the same artifacts again (three more maps per pack).
- Fix: `base.SingleThreadArena`; `literal_converters` can be a `[]bool`/bitset indexed by proc spec index (`converting` already is one, line 8750: index the spec's proc index instead of its identity); share one `identity -> artifact index` map between `packFileBytes` and `SpliceClosure`.
- Status: open

### src/cli/main.zig:2210: LayoutHashContext.seen hashes dense layout.Idx
- category B; runs per layout node hashed when computing host-callable identity (per build, per entrypoint layout); impact low; risk low
- Finding: `seen: std.AutoHashMap(layout.Idx, u32)` maps a dense layout index to a visit ordinal; `updateLayoutFingerprint` creates and destroys the map per root layout.
- Fix: `collections.DenseMap(layout.Idx, u32)` or a `[]u32` column sized by the layout store (`layouts.count()`), reused across the roots of one fingerprint.
- Status: open

### src/cli/pack_store.zig:432: Every object-cache pack is read fully into memory then decoded into a second copy
- category D; runs per `.rpk` file loaded (per module in view, per build with a warm object cache); impact medium for warm `roc build` with many module packs; risk low-medium (PackFile.read is in …
- Finding: `loadDirIntoUnrecorded` does `readFileAlloc(..., .limited(1 GiB))` into a heap buffer, `PackFile.read(self.allocator, bytes)` decodes into an arena-owned copy, then frees the file bytes. Each pack's bytes therefore live twice during load and the machine code is copied once more at splice time.
- Fix: mmap the pack file (read-only) and have `PackFile.read` alias code/data slices into the mapping (the splice copies code into the executable image anyway), keeping the mapping alive in `LoadedPacks`; or at least decode straight from the mapping without the intermediate heap read. Also `names` dupes each directory entry name and `std.fs.path.join`s per file; use a stack path buffer.
- Status: open

### src/cli/pack_store.zig:49: Pack path lookups allocate three strings per `has`/`write`
- category D; runs per visible artifact per build (`store.has` in `writePacksToStore` loops over all visible artifacts, main.zig 8700-8720); impact low; risk low
- Finding: `packPath` = `identityDir` (join) + `allocPrint(".rpk")` + join; `has` then frees it; `write` repeats `identityDir` and `packPath`.
- Fix: Format into a `[std.fs.max_path_bytes]u8` stack buffer with `bufPrint` (root + "/" + origin + "/" + 64 hex + "/" + 64 hex + ".rpk" is bounded).
- Status: open

### src/cli/main.zig:12269: One sentinel-terminated symbol name allocated per test root
- category A/D; runs per test root in `roc test` (hundreds to thousands per run); impact low; risk low
- Finding: `std.fmt.allocPrintSentinel(ctx.gpa, "roc_test_expect_{d}", .{result_index}, 0)` per entry, retained in `entries` for the whole test run and freed individually.
- Fix: Generate the names into one shared `ArrayList(u8)` with recorded offsets (or on `ctx.arena` which is freed once), since they are only read by the backend symbol table; the index is enough to regenerate the name on demand with `bufPrint`.
- Status: open

### src/cli/main.zig:13034: Test transcript events are deep-copied per event twice
- category D; runs per dbg/expect/crash event per test; impact low; risk low
- Finding: `copyCliTestTranscriptEvents`/the `events` conversion at 13025-13063 `dupe` every payload into a fresh allocation; the source events are themselves owned copies made by `runtime_host`/`inspected_run` (runtime_host.zig:101-120 `RecordedRun.dupe` dupes each message again).
- Fix: Move ownership instead of copying (take the slices from the recorded run, mark the source as moved), or record transcript payloads into one per-module `ArrayList(u8)` with offsets.
- Status: open

### src/cli/main.zig:18077: Drained report paths dupe per module per render
- category D; runs per module with reports, per check/build (four copies of this pattern at 18077, 18127, 18219, 18256); impact low; risk low
- Finding: `reports[i].file_path = try ctx.gpa.dupe(u8, mod.abs_path)` then `build_env.freeDrainedReportsPathsOnly(drained)` frees the originals: an owned path is copied and the original freed for no reason.
- Fix: Transfer ownership of `mod.abs_path` into `DrainedReport` (skip the free), or make `DrainedReport.file_path` borrow since `drained` outlives the render.
- Status: open

### src/cli/main.zig:2191: checkedHostedTable dupes every external symbol name after building the table
- category D; runs per build (per hosted function); impact low; risk low
- Finding: `symbols[i] = try allocator.dupe(u8, entry.external_symbol_name)` for every hosted entry so the table can own its strings; the names already live in the checked artifact which outlives the table.
- Fix: Borrow from the artifact (`CheckedHostedTable` holds a pointer to the root artifact already) and drop the per-entry dupe; `appendHostedCacheEntriesFromView` also keeps `seen_keys` as a linear `ArrayList([32]u8)` scanned per view (`checkedModuleKeySeen`, line 2046): fine for small counts, note only.
- Status: open

### src/cli/ReplSession.zig: interactive REPL; per-line allocations are user-paced. No findings recorded (allocations at 274-308, 426-462, 493-537 are per submitted line).
- 
- Status: open

### src/build/builtin_compiler/main.zig:373: module_envs keyed by Ident.Idx in a hash map
- category B; runs once per compiler build (build-time only); impact low (compile_package instance: per module, small map); risk low
- Finding: `std.AutoHashMap(base.Ident.Idx, Can.AutoImportedType)` keyed by the dense interned identifier index; the same shape exists in `compile_package.zig:422` (`module_envs_map`) which runs per module type check in the production pipeline, and in `eval/inspected.zig:1745/1781`.
- Fix: In `compile_package.typeCheckModule` the set of auto-imported types is tiny and fixed per build (Builtin's nested types); build it once per coordinator and share a const pointer, or key a `DenseMap(Ident.Idx, ...)`. The build-time instance can stay.
- Status: open

### src/lsp/semantic_tokens.zig:763: ImportContext is a StringHashMap of StringHashMaps rebuilt per semantic-tokens request
- category F/B; runs per `textDocument/semanticTokens` request; impact medium for editor latency on large modules; risk low
- Finding: `module_functions: StringHashMap(StringHashMap(void))` is populated by walking every exported def of every imported module (`addModuleExports`) on each request, allocating one inner map per module, keyed by name strings copied from the envs.
- Fix: Cache per `ModuleEnv` (keyed by env pointer + env version/generation) a sorted `[]Ident.Idx` of exported function names, computed once when the module is checked; membership becomes a binary search or an `Ident.Idx` bitset. No nested maps.
- Status: open

### src/lsp/completion/builder.zig:180: Every completion item dupes its label and keeps a StringHashMap for dedupe
- category D/B; runs per completion item per request (hundreds per keystroke); impact medium (editor latency); risk low
- Finding: `addItem` checks `seen_labels.contains(item.label)`, then `allocator.dupe(u8, item.label)` and inserts into the map; `detail` strings are also `dupe`d per item (lines 370-1253) after being formatted into temporaries.
- Fix: Allocate labels/details from a per-request arena (`base.SingleThreadArena`) freed after the response is serialized, removing per-item frees; dedupe by `Ident.Idx` where the label is an identifier (most cases) with a bitset/`DenseMap`, falling back to a string set only for synthesized labels.
- Status: open

### src/lsp/cir_queries.zig:1141: PatternSet hashes dense CIR.Pattern.Idx on every inlay-hint request
- category B; runs per `textDocument/inlayHint` request (every viewport change), per binding in the module; impact medium; risk low
- Finding: `PatternSet = std.AutoHashMapUnmanaged(CIR.Pattern.Idx, void)` for `annotated` and `declared`, filled over the whole module per request (the comment at 1148-1152 says all three questions are answered per viewport change).
- Fix: Two `DynamicBitSetUnmanaged`s sized by the pattern store's node count, or cache the answers per checked module (the module does not change between viewport scrolls; key the cache on the document version).
- Status: open

### src/lsp/scope_map.zig:71: Scope map rebuilt and sorted per request
- category E; runs per completion/hover request that needs scope (`getOrBuildScope` caches only within one builder, i.e. one request); impact medium; risk low
- Finding: `ScopeMap.build` traverses every def and statement of the module, appends all bindings and sorts them, per request.
- Fix: Cache the built `ScopeMap` per (`ModuleEnv` pointer, document version) in the server session; invalidate on didChange.
- Status: open

### src/lsp/document_store.zig:90: Incremental edits copy the whole document text per change
- category D; runs per `didChange` notification (per keystroke), per change in the batch; impact medium (typing latency on large files); risk low
- Finding: `applyContentChanges` dupes the entire current text, then for each change `replaceRange` allocates a new full-size buffer and frees the previous one: a k-change notification copies the document k+1 times.
- Fix: Apply changes in place on an `ArrayList(u8)` with `replaceRange` (std's `ArrayList.replaceRange` moves the tail once per change and only grows when needed); drop the initial dupe (edit the stored text directly, with a rollback copy only on error, or validate all ranges first).
- Status: open

### src/lsp/position.zig:24: LineOffsets built from a temporary ArrayList then toOwnedSlice
- category D; runs per request that converts positions (most handlers build one per request); impact low-medium; risk low
- Finding: `findLineStarts` allocates a growing list then `toOwnedSlice` copies on shrink; the line table is recomputed per request for the same document version.
- Fix: Store the line-start table on the `Document` entry and update it on didChange (or lazily once per version); build with `ensureTotalCapacity(count of '\n' + 1)` from a single counting pass.
- Status: open

### src/lsp/syntax.zig:545: Diagnostics publication dupes every URI and message per publish
- category D; runs per diagnostics publish (per didChange/didSave); impact low; risk low
- Finding: Each publish builds `PublishDiagnostics` with `dupe`d URIs (588, 598, 632, 651, 659) and formatted messages; `published_diagnostic_uris` is a `StringHashMapUnmanaged(void)` of owned dupes cleared and refilled per publish.
- Fix: Per-publish arena for the outgoing payload (serialized immediately), and keep `published_diagnostic_uris` keyed by the document store's already-owned URI slices (stable for the document's lifetime).
- Status: open

### src/lsp/handlers/selection_range.zig:215: One heap node per selection-range level
- category F; runs per selection range request, per nesting level; impact low; risk low
- Finding: `allocator.create(SelectionRange)` for each parent link, after collecting and sorting `containing_regions` into a second deduplicated list.
- Fix: Build the chain in one `[]SelectionRange` slice with parent indices (the LSP encoder can follow indices); dedupe during the sorted walk without the second list.
- Status: open

### src/fmt/fmt.zig:1466: formatOrderedCollection allocates an owned name string per entry
- category A/D; runs per entry of every ordered header collection (package/platform header fields, requires, symbol maps) per format; impact low (header collections are small; per-file); risk low
- Finding: `orderedEntryName` writes the entry's identifier into an allocating writer and `toOwnedSlice`s it; `entries` holds one owned string per item freed after sorting. The names are already contiguous token text in `ast.env.source` (`fmt.ast.resolve(token)`).
- Fix: Store `(token_start, token_len)` or the `[]const u8` slice into the source for simple identifiers (the only non-trivial case is the qualified-name concatenation, which can be compared piecewise) and sort by slice comparison.
- Status: open

### src/fmt/fmt.zig:543: Three node/token-sized arrays allocated per formatted file
- category A/G; runs per `formatAst` call (per file; per keystroke in LSP formatting); impact low; risk low
- Finding: `node_layouts` and `grouped_layouts` (`TypeLayout` per AST node, two arrays) and `comment_prefix` (`u32` per token) are allocated and memset per format. `TypeLayout` appears to be an enum (1 byte) so these are small, but `comment_prefix` does a `findScalar` over every inter-token gap per file.
- Fix: Reuse the arrays across files in a multi-file `roc format` run via a retained `Formatter.Scratch` (`clearRetainingCapacity`-style resize); compute `comment_prefix` from the tokenizer's comment records if it already tracks comments instead of rescanning the source.
- Status: open

### src/fmt/open_rows.zig:91: Redundant-`..` analysis keys several maps by type-name strings
- category B/F; runs per annotation / per type declaration per formatted file; impact low-medium (per file with many annotations); risk low
- Finding: `VarOccurrences = StringHashMapUnmanaged(bool)` per annotation, `type_decls: StringHashMapUnmanaged(ArrayList(Statement.Idx))` (map of ArrayLists), `external_names`, `provided_names` (string sets), `position_cache: AutoHashMapUnmanaged(AST.Statement.Idx, ?[]Positions)` keyed by a dense `Idx` with heap-allocated `[]Positions` values that `declarationPositions` dupes per query (644, 663, 711, 726).
- Fix: Key everything by the tokenizer's interned ident index (`ast.env` idents) instead of text; `position_cache` becomes a `DenseMap(Statement.Idx, ...)` or a column over statements with positions stored as spans into one shared `ArrayList(Positions)` so no per-query dupe is needed (return a borrowed slice).
- Status: open

### src/docs/extract.zig:362: `gpa.alloc(DocEntry, 0)` per leaf entry and per-entry name dupes
- category A; runs per documented entry (per def/type/field) during `roc docs`; impact low; risk low
- Finding: Empty children are materialised with `try gpa.alloc(DocModel.DocEntry, 0)` at 362, 401, 456, 842, 926, 1058, 1207, 1187 (an allocation call per leaf), and every name is `dupe`d from the env's ident table (248, 854, 924, 942, 1025, 1076, 1204).
- Fix: Use `&.{}` for empty children (a zero-length allocation is never freed differently from a slice literal if `deinit` checks `len == 0`), and borrow names from the module env which outlives the doc model.
- Status: open

### src/docs/render_html.zig:82: Sidebar tree is a heap node per module with ArrayList children and path dupes
- category F/D; runs per module in the documented package set; impact low; risk low
- Finding: `gpa.create(SidebarNode)` per node, `ArrayList(*SidebarNode)` per node, `gpa.dupe` of each path prefix (953, 1012, 1066); `current_module_anchors`/`all_anchors` are string sets refilled per module with arena dupes.
- Fix: Flat `ArrayList(SidebarNode)` with child index spans; borrow path slices from the module names.
- Status: open

### src/glue/glue.zig:1606: Type table uses several std hash maps keyed by TypeTableKey/u64 and dupes arg key lists per open context
- category B/D; runs per glue-visible type (per `roc glue` run); impact low; risk low
- Finding: `var_map`, `template_bindings`, `open_memo` are `AutoHashMap`s; `open_ctxs` is scanned linearly per `openCtxFor` call (1755-1762) comparing artifact keys and arg key lists, and each new context `dupe`s its `arg_keys`.
- Fix: Keys are structural (checked type ids across artifacts) so hashing is legitimate; index `open_ctxs` by a hash of (decl key, statement, arg keys) instead of the linear scan. Low frequency.
- Status: open

### src/unbundle/unbundle.zig:260: Extracted files held as StringHashMap of managed ArrayLists
- category F; runs per file in a bundle (per `roc unbundle`/package fetch); impact low; risk low
- Finding: `files: StringHashMap(std.array_list.Managed(u8))` with the path `dupe`d per entry; each file's bytes are a separate growing list.
- Fix: Append all file bytes into one arena/buffer and keep `(path, offset, len)` entries in an ArrayList; the consumer writes them to disk in order.
- Status: open

### src/watch/Inputs.zig:12: Watch input nodes keep two ArrayLists per node and dupe every path component during resolve
- category F/D; runs per watched path (per watch setup / per rescan); impact low; risk low
- Finding: `Node { inputs: ArrayList(usize), children: ArrayList(usize) }` per filesystem node; `resolve` (160-180) `dupe`s `current` and each `parent_path` per component.
- Fix: Flat edge spans into a shared `ArrayList(usize)`; resolve with a single growing path buffer instead of per-component dupes.
- Status: open

### src/ipc/SharedMemoryAllocator.zig:529: Hot-reload image snapshot dupes the used region
- category D; runs per hot-reload publish; impact low; risk medium (hot-reload correctness)
- Finding: `gpa.dupe(u8, self.base_ptr[0..used])` copies the whole used shared-memory image to produce the snapshot.
- Fix: If the consumer only needs read access while the mapping is alive, borrow the mapping; otherwise copy only the dirty descriptor range (the tracker in main.zig already knows the regions touched per generation).
- Status: open

### src/reporting/document.zig:299: Every document element dupes its text
- category D; runs per diagnostic element (per report, dozens of elements); impact medium for diagnostics-heavy runs (LSP publishes reports …; risk low
- Finding: `addText`/`addAnnotated`/`addSourceCodeRegion` etc. `dupe` each string into a separate GPA allocation (299, 306, 345, 352, 407, 411, 434, 441, 540, 562, 566, 592, 614); `Report.addOwnedString` (report.zig:270) adds another copy for strings that are then passed to `addText`, so a formatted message is often copied twice. Each `Report` thus owns tens of tiny allocations freed one by one.
- Fix: Give `Report` a `base.SingleThreadArena` (or an `ArrayList(u8)` string pool with `(offset, len)` references) and allocate all element text from it; `owned_strings` becomes unnecessary. Reports are built and rendered once, so arena lifetime equals report lifetime. Verify `Report` move semantics (reports are moved through worker results; an arena moves with it as long as it is not self-referential).
- Status: open

### src/reporting/source_region.zig:102: One allocation per rendered source line
- category A; runs per line of every rendered source snippet; impact low; risk low
- Finding: `expandTabs` allocates a `row` per line and appends it to `rows`; the rows are only read by the renderer immediately after.
- Fix: Expand into one `ArrayList(u8)` with recorded line spans, or expand tabs while writing to the output writer directly (the renderer already streams).
- Status: open

### src/reporting/renderer.zig:257: Renderer buffers prose in an allocating writer per section, cleared per flush
- category A (acceptable; noted); runs per report section; impact low; risk low
- Finding: `self.buffer.clearRetainingCapacity()` after each region: capacity is retained within a report but the `Allocating` writer is created per report render.
- Fix: Keep the renderer's buffer on the long-lived renderer context (`ReportingConfig`-scoped) so capacity persists across reports.
- Status: open

### src/eval/boxy_runtime.zig:5628: collectBoxyRowTagNames dedupes with an O(n^2) scan into a fresh ArrayList per tag hash
- category E/A; runs per hash of a boxy tag-union value (every Dict/Set key hash of a tag value in Boxy-routed code; called from `layoutHash` at line 5509); impact medium for Boxy programs hashing tag keys in loops; risk low
- Finding: `layoutHash` for `.tag_union` creates `row_names = std.ArrayList(BoxyNameId).empty`, and `collectBoxyRowTagNames` walks the extension chain appending each variant name after a linear `for (names.items)` membership scan, then `tagUnionHashInRow` searches the list again. The complete row for a given descriptor is a static property of that descriptor.
- Fix: Cache the complete sorted row name list per descriptor pointer (a `DenseMap`/column on the runtime descriptor table, computed once per descriptor) and hash the variant's index by binary search; the per-call ArrayList and quadratic dedupe disappear.
- Status: open

### src/eval/inspected.zig:729: available_imports rebuilt and reallocated for every extra module
- category A/G; runs per imported virtual module per inspected compile (REPL evaluation, `roc test` inspect path); impact low; risk low
- Finding: Inside `for (imports)` the code allocates `available_imports` sized by the modules checked so far and fills it, so N imports cost N allocations and O(N^2) entry writes; `main_imports` repeats the fill afterwards.
- Fix: Keep one `ArrayList(AvailableImport)` that grows as modules are appended and pass `items` (slices stay valid if capacity is reserved for `imports.len` up front).
- Status: open

### src/eval/inspected_run.zig:618: Function relocation resolution builds a StringHashMap and allocPrints a name per relocation
- category D; runs per static-data function relocation per inspected LLVM run; impact low; risk low
- Finding: `allocPrintSentinel("roc_inspect_{s}")` per relocation, freed immediately, and a `StringHashMap(usize)` keyed by the target symbol name consulted through a `Resolver` callback.
- Fix: Format into a stack buffer (`[256]u8` with `bufPrintZ`); since relocations repeat target names rarely, resolve directly in the callback without the map, or key the map by the relocation's symbol id.
- Status: open

### src/eval/interpreter.zig:5314: List literal elements are coerced then memcpy'd one at a time
- category H (acceptable; noted); runs per list literal evaluation; impact none; risk none
- Finding: One `getLocalChecked` (binary search) + `coerceExplicitRefValueToLayout` + `@memcpy` per element; no extra allocations beyond the single Roc allocation. The binary search per element is the only waste, covered by the frame slot finding.
- Fix: none beyond the slot-index finding
- Status: no change: the per-element lookup is the direct slot index of fix 18

