# Allocation and Lifetime Audit for the Zig Compiler in `src/`

This audit follows the compiler from the CLI/build entry points through parsing,
canonicalization, checking, checked artifacts, post-check lowering, ARC insertion,
and the active backends. The goal is to describe what owns memory today, what
data is scratch versus durable compiler fact, and where arenas or phase-level
bulk teardown would simplify the implementation without violating the compiler
rules in `AGENTS.md`.

## Executive Summary

- The compiler already has arena-shaped APIs, but most real allocations still
  use `gpa` or `std.heap.page_allocator`. `base.Allocators.arena` is threaded
  through parse/canonicalize APIs but is mostly unused. The coordinator also has
  a per-worker arena type, but worker task execution currently allocates through
  `getWorkerAllocator()` instead of that arena.

- Manual `deinit` is doing a lot of work for objects whose natural lifetime is
  one phase, one module, one checked artifact, or one lowered program. The best
  candidates for bulk teardown are checked artifact stores, worker/task scratch,
  canonicalizer/checker scratch, and post-check lowering stores.

- Directly replacing `gpa` with an arena in existing `ArrayList` and `HashMap`
  code would usually be the wrong move. Zig arena allocators do not reclaim old
  buffers on grow/realloc, so a growing `ArrayList` on an arena can waste more
  memory than the current `gpa` path. Arena use should be paired with either
  chunked append-only stores, explicit pre-sized builders, or a two-step pattern:
  build with a realloc-capable allocator, then seal/copy final data into a
  lifetime arena.

- The most valuable simplification is not "use one arena everywhere." It is to
  make ownership scopes explicit:
  - CLI/build lifetime for build graph state, package metadata, and final reports.
  - Per-module semantic lifetime for source, `ModuleEnv`, CIR, type store, and
    eventually the checked artifact.
  - Per-worker-task scratch lifetime for parse/import extraction/report building.
  - Per-checker session scratch lifetime for unification, occurrence checking,
    and exhaustiveness internals.
  - Per-checked-artifact lifetime for published immutable stores.
  - Per-post-check-stage lifetime for mono/row/lifted/lambda-solved/executable/IR/LIR intermediates.
  - Backend/output lifetime for code buffers, object writers, wasm modules, and
    executable memory.

- The code has several allocator-related policy smells that are worth fixing
  before introducing more arenas: silent allocation failure catches, `page_allocator`
  bypasses, parse-time allocator objects that are not deinitialized, and one
  legacy import string leak.

## High-Level Lifetime Map

| Scope | Current owner | What lives there | Current allocator pattern | Arena suitability |
| --- | --- | --- | --- | --- |
| CLI invocation | `src/cli/main.zig`, `BuildEnv` | Build environment, builtin modules, package discovery, caches, final build results | Mostly `gpa`, some owned strings and maps | Good for immutable invocation-wide facts, not for growable maps unless sealed |
| Header discovery | `BuildEnv.parseHeaderDeps` | Temporary source, header `ModuleEnv`, parse AST; promoted package/header strings | `gpa` with manual frees | Excellent for temporary header parse state; promoted strings must move to build lifetime |
| Coordinator/build graph | `compile/coordinator.zig` and legacy `compile_package.zig` | Package/module states, source/module envs, cached ASTs, reports, dependency edges | `gpa` in single-thread mode, `page_allocator` in threaded worker paths | Good if split into durable module arenas plus task scratch arenas |
| Parse | `parse/mod.zig`, `parse/AST.zig`, `parse/NodeStore.zig` | Tokens, parse AST node store, parse diagnostics, parse scratch | `allocators.gpa`; `allocators.arena` unused | AST/node store is module-phase durable until canonicalize; parse scratch can bulk reset |
| Canonicalize | `canonicalize/ModuleEnv.zig`, `canonicalize/Can.zig` | CIR stores and module environment; canonicalizer maps/scopes/scratch | Mostly `env.gpa`; `allocators.arena` stored but unused | CIR can be module arena or append-only store; canonicalizer scratch should be phase scratch |
| Check | `check/Check.zig`, `types/store.zig` | Checker state, snapshots, problems, type store, solver scratch | `gpa` with many owned substructures | Checker scratch is phase arena friendly; type store is module/artifact durable |
| Exhaustiveness | `check/exhaustive.zig` | Match-check intermediate pattern matrices and reified patterns | Local `ArenaAllocator` already used, result copied out | Good existing example of correct arena usage |
| Checked artifact | `check/checked_artifact.zig` | Published checked facts consumed after checking | Many slices and nested allocations, explicit `deinit` | Strongest candidate for artifact arena or sealed builders |
| Post-check MIR/IR/LIR | `mir/*`, `ir/*`, `lir/*` | Stage programs, append-only stores, temporary maps, root lists | One allocator passed through stages, manual ownership moves | Good candidate for per-stage arenas after store builders are made arena-safe |
| ARC insertion | `lir/arc.zig` | Temporary owned-local sets and new explicit RC statements | Temporary sets and maps on LIR store allocator; RC statements appended to LIR store | Temporary sets can use scratch; inserted statements must live in LIR |
| Backends | `backend/dev`, `backend/wasm`, `backend/llvm` | Code buffers, wasm/object sections, static data, executable memory | Builder-owned `ArrayList`s and maps, some arena for static data | Output builders can use arena/chunked stores; executable memory is OS-owned |
| Runtime image | `lir/runtime_image.zig` | Shared-memory view over already-published LIR/layout arrays | Child view uses slices into mapped memory, allocator set to `page_allocator` but should not deinit | Already bulk lifetime via mapping; avoid treating views as owning data |

## Current Allocator Model

### `base.Allocators`

`src/base/mod.zig` defines:

- `gpa`: realloc-capable allocator. Comments say anything allocated here must be freed.
- `arena`: an `ArenaAllocator` intended to live for an entire Roc compilation.
- `arena_impl`: stored inside the struct, with a warning that `Allocators` must not be moved after initialization.

The intent is sensible, but the implementation is not yet using it:

- `src/parse/mod.zig` uses `allocators.gpa` for tokenizer, parser, AST, tokens,
  diagnostics, and node store.
- `src/canonicalize/Can.zig` stores `allocators` but explicitly comments that
  arena use is a future optimization and that current allocations use `env.gpa`.
- Several call sites initialize `base.Allocators` just to pass it through these
  APIs.

The practical result is that `base.Allocators.arena` mostly creates a false
ownership signal today. Before relying on it for performance, parse and
canonicalize should either actually use it in well-scoped ways or remove it from
paths where it is not relevant.

### `base.Scratch`

`src/base/Scratch.zig` is a reusable stack-like `Managed` array list with
`clearFrom(start)` retaining capacity. This is the right shape for recursive
scratch, but there are two issues:

- Scratch objects are initialized with a normal allocator and individually
  deinitialized. That is fine, but a per-task scratch allocator would let the
  compiler bulk-reset all scratch capacity between phase tasks.
- `Scratch.SetView` uses `std.heap.page_allocator` directly for larger ranges
  and silently returns to linear scan if allocation fails. That is both an
  allocator bypass and a fallback outside parsing/error reporting. Callers
  outside parsing/error reporting should not rely on this behavior. A caller
  should pass explicit scratch storage or use one deterministic structure.

### `page_allocator`

`std.heap.page_allocator` appears in several important places:

- The coordinator uses it for module/worker allocations in multi-threaded mode.
- `PackageState` maps in `compile/coordinator.zig` are initialized with
  `page_allocator`.
- Coordinator channels use `page_allocator` when threads are available.
- `Scratch.SetView` uses `page_allocator`.
- `lir/runtime_image.zig` uses `page_allocator` in child-side non-owning views.
- Some debug-only maps in `base/Ident.zig` use `page_allocator`.
- Backend relocation/object helper code has small direct `page_allocator` use.

The coordinator use is understandable as a thread-safety shortcut, but it makes
bulk phase teardown impossible and pushes many small allocations to mmap/munmap
style behavior. A better shape is a thread-safe backing allocator plus per-worker
or per-module arenas, with explicit promotion of result data to the coordinator
owner when a task completes.

## CLI and Build Entry

The CLI starts in `src/cli/main.zig` and eventually enters the build/check/run
paths in `src/compile`. The important ownership root is `BuildEnv` in
`src/compile/compile_build.zig`.

`BuildEnv` owns:

- `gpa`
- workspace roots
- discovered packages
- legacy `PackageEnv` schedulers
- ordered report sink
- optional coordinator
- builtin modules
- resolver/sink/schedule contexts
- pending known modules
- package/cache metadata

`BuildEnv.deinit` manually frees or deinitializes each of these fields. This is
a reasonable process-lifetime owner, but the build path duplicates ownership
between newer coordinator state and legacy `PackageEnv` state:

- `compileDiscovered` builds through the coordinator.
- `transferCoordinatorResults` then moves semantic results from coordinator
  module states into legacy scheduler module states.
- It sets coordinator semantic fields to `null` to avoid double frees.

That transfer step is a sign that the lifetime model is overcomplicated. The
same module result should not need two long-lived owners. A simpler design would
make one build graph the durable owner and expose borrowed result views for
later APIs. If compatibility with `PackageEnv` is still needed, use explicit
move-only wrappers so that ownership transfer is mechanical and visible.

## Header Discovery

`BuildEnv.parseHeaderDeps` reads a module source file, creates a temporary
`ModuleEnv`, computes line starts, parses the header, extracts imports/package
metadata, and then frees the temporary parse state.

Current lifetime:

- Source bytes are allocated with `gpa`, normalized, and freed at the end.
- Header `ModuleEnv` is stack-owned but owns stores allocated with `gpa`.
- A temporary `base.Allocators` is initialized and deinitialized.
- Parse AST is created and deinitialized.
- Header facts that must survive discovery are duplicated into `HeaderInfo` and
  then into `BuildEnv` or `Package`.

This is a good candidate for a local header-discovery arena:

- The temporary source, parse AST, parse scratch, line starts, and header
  `ModuleEnv` can all die at the end of header parsing.
- Durable header outputs should be explicitly promoted into the build allocator.
- The promotion boundary is clear because `HeaderInfo.deinit` already lists the
  fields that outlive the parse.

This is also a good place to keep parsing/error-reporting fallbacks isolated.
Header parsing is part of discovery and error reporting, so tolerant parsing can
belong here. The package/build graph produced by this step should still be
explicit facts, not reconstructed later.

## Build Graph and Coordinator

There are two build schedulers in play:

- `src/compile/coordinator.zig`: newer actor/coordinator model.
- `src/compile/compile_package.zig`: legacy per-package scheduler and result owner.

### Coordinator State

The coordinator owns package and module state:

- Package names, root dirs, shorthand maps.
- Module states with name/path/source directory, imports, external imports,
  dependents, reports, cached AST, and semantic state.
- Cross-package dependent edges.
- Checked artifact index.
- Worker task/result channels.

The header comment says workers should be pure and that the coordinator owns
mutable state. This is the right model. The allocator details currently weaken
it:

- `WorkerAllocators` has a backing allocator and an arena reset function, but
  worker execution calls `executeTaskInline(t)` and the `execute*` functions use
  `self.getWorkerAllocator()` instead of `worker_allocs.arena`.
- In multi-threaded mode, `getWorkerAllocator()` and `getModuleAllocator()`
  return `page_allocator`.
- `executeParse` creates a `base.Allocators` value and does not deinitialize it.
  Today the parse arena is not used, so this is usually harmless. If parsing
  starts using `allocators.arena`, this becomes a real leak.
- `executeCanonicalize` creates and deinitializes `base.Allocators` properly.
- `executeParse` and `executeCanonicalize` contain several `catch {}` or
  `catch &[_]...{}` paths that silently continue after allocation or compiler
  failures. Some are in parsing/error-reporting territory, but the general
  pattern should not leak into later stages.

The coordinator should get an explicit task allocator model:

- Durable module data: source bytes, `ModuleEnv`, cached AST, checked artifact.
- Task scratch: import extraction arrays, diagnostic report construction,
  temporary module dependency arrays, canonicalizer scratch, typecheck report
  arrays.
- Result transfer: anything sent back to the coordinator either lives in the
  durable module owner or is copied/promoted into coordinator-owned storage.

### Legacy `PackageEnv`

`compile_package.zig` still owns module states after coordinator results are
transferred. It also has its own parse/canonicalize/typecheck code paths.

Important details:

- `PackageEnv.init` preallocates module capacity with `ensureTotalCapacity(...,
  256) catch {}` in multi-thread mode. This silently ignores allocation failure
  and should be removed or made explicit.
- `doParse` heap-allocates `ModuleEnv` and source bytes, caches the AST, and
  extracts imports. It initializes `base.Allocators` but does not deinitialize
  it. As with the coordinator parse path, this must be fixed before parse uses
  `allocators.arena`.
- `doCanonicalize` creates a fresh `base.Allocators` and deinitializes it.
- `ModuleState.deinit` frees external import strings in the coordinator version,
  but the legacy `PackageEnv.ModuleState.deinit` deinitializes the
  `external_imports` list without freeing the duplicated strings appended by
  `doParse`. That is a likely leak in the legacy path.
- `tryEmitReady` moves reports into the ordered sink and clears the module report
  list retaining capacity to avoid double frees.

This path should either disappear behind the coordinator as sole owner or be
made a true result-view layer. The current split causes avoidable ownership
transfer complexity.

## Reports

Reports are constructed in parse/canonicalize/typecheck tasks, moved into module
state, then drained into `OrderedSink` and rendered.

Current pattern:

- Report values are copied between array lists.
- The copied `Report` payloads own nested allocations through the allocator
  embedded in each `Report`.
- Lists are often cleared retaining capacity after append-to-sink so the same
  `Report` payload is not deinitialized twice.

This works but is fragile. A cleaner model would use one of:

- A report arena with the same lifetime as the final diagnostic emission batch.
- A move-only report list wrapper that makes transfer and invalidation explicit.
- A module-owned report store whose reports are borrowed by the sink until the
  sink is drained.

Reports are error-reporting data, so fallbacks are allowed there by policy, but
allocator ownership should still be explicit.

## Parse and AST

`src/parse/mod.zig` allocates tokenizer output, parser state, AST node store,
and diagnostics using `allocators.gpa`. `AST.deinit` frees tokens, the node
store, diagnostics, and the AST object.

`parse/NodeStore.zig` contains:

- Durable parse nodes and extra data.
- Many `base.Scratch` buffers used while building spans.
- `emptyScratch()` to clear scratch after parsing.

Current lifetime:

- The parse AST must survive parsing because canonicalization consumes it.
- Parse scratch only needs to survive until the AST is fully built, or at most
  until parser teardown.
- Parse diagnostics must survive until they are converted into reports.

Recommended strategy:

- Keep the AST/node store on a durable module or parse-result allocator until
  canonicalization is done.
- Allocate parser scratch from a per-parse scratch arena or a scratch allocator
  that is reset after parse.
- Do not put growing AST `ArrayList`s directly on an arena unless the store is
  changed to chunked append-only storage or built with known capacity.

## Canonicalization

`src/canonicalize/ModuleEnv.zig` owns `CommonEnv`, type store, CIR stores,
imports, import mapping, method idents, and evaluation order. The source bytes
are owned outside `ModuleEnv` and freed by the owner.

`src/canonicalize/Can.zig` owns canonicalizer-local state:

- scopes and imports
- local name tracking
- aliases and symbol maps
- pending/diagnostic scratch
- many `base.Scratch` buffers

Current pattern:

- `Can.initModule` accepts `*base.Allocators` but uses `env.gpa`.
- Durable CIR facts are stored in `ModuleEnv`.
- Canonicalizer-local maps and scratch are deinitialized manually at the end.
- Some code has scope-exit cleanup guarded with `catch {}` in defers. That is
  not an allocator strategy, but it is another example of silent recovery in a
  non-parse stage.

Recommended strategy:

- Separate `ModuleEnv` durable storage from `Can` scratch storage in the type
  signatures.
- Use a canonicalization scratch allocator for maps/lists whose contents do not
  escape canonicalization.
- Keep `ModuleEnv` storage on a module semantic allocator. To use an arena here,
  the CIR stores should become append-only chunked stores or be sealed from
  realloc-capable builders.

## Common Environment and Module Environment

`src/base/CommonEnv.zig` owns identifier store, string literal store, exposed
items, and line starts. It deliberately does not own source bytes.

`CommonEnv.calcLineStarts` currently deinitializes and reinitializes
`line_starts`. For normal compilation, line starts are computed once per source,
so the overhead is small. If it is ever recomputed repeatedly, it should clear
retaining capacity instead of freeing and reallocating.

`ModuleEnvStorage` in `checked_artifact.zig` is the real owner boundary after
checking:

- `checked_source`: owns a live `ModuleEnv` plus source bytes.
- `compiled_buffer`: owns a serialized module buffer and a lightweight env view.
- `cached_buffer`: owns cached buffer, cached source, and env.

This is a clear ownership boundary. It would become even clearer if the live
source `ModuleEnv` used a module arena and `ModuleEnvStorage.deinit` reset that
arena instead of walking every store.

## Type Checking

`src/check/Check.zig` is a module type-checking session. It does not own the CIR
or the main type store, but it owns many supporting structures:

- copied regions
- snapshots and problems
- import mapping
- unifier and occurs-check scratch
- env pool and generalizer
- constraints
- maps for instantiation, static dispatch, top-level patterns, and error tracking
- `base.Scratch` buffers for vars, tags, record fields, and static dispatch
- deferred unification/cycle arrays
- type writer

`Check.deinit` manually tears all of this down. Most of it is check-session
scratch and can die together after the module has been checked and the checked
artifact has been published.

There are two important exceptions:

- The module type store is durable semantic data until artifact publication.
- Problems/reports must survive long enough to be converted into user-facing
  reports.

Recommended strategy:

- Use a check-session scratch arena for solver-local transient data.
- Keep the module type store in the module semantic owner.
- Publish checked facts into an artifact owner, then reset the checker scratch.
- Keep `EnvPool` as a reusable data structure if it is hot; backing it with a
  check-session allocator is still compatible with bulk teardown.

`src/check/exhaustive.zig` is a good model: `checkMatch` creates a local
`ArenaAllocator` for intermediate pattern matrices and reification, then copies
the final `CheckResult` to the caller allocator before the arena is freed.

## Checked Artifacts

`src/check/checked_artifact.zig` is the largest and most important allocation
surface in the compiler. It publishes immutable post-check facts consumed by
later stages.

`CheckedModuleArtifact` owns:

- canonical names
- module identity and checking context identity
- direct import artifact keys
- `ModuleEnvStorage`
- exports and provides/requires metadata
- checked type store
- checked body store and checked const bodies
- procedure/const/template tables
- static dispatch and method registries
- resolved value refs and nested proc sites
- compile-time roots/plans/values/dependencies
- executable type payloads and transforms
- callable-set descriptors and erased function ABIs
- platform requirement tables
- promoted procedure/wrapper tables

`CheckedModuleArtifact.deinitInternal` manually deinitializes all of these in a
long fixed order. This is exactly the kind of owner that would benefit from an
artifact arena.

### Checked Type Store

`CheckedTypeStore.fromModule` builds `roots`, `payloads`, `schemes`, and
`nominal_declarations` with `ArrayList`s, then converts them to owned slices.
Many payloads also contain nested allocated slices:

- function args
- tuple elements
- record fields
- tag payload args
- static dispatch constraints
- type variable constraint arrays
- optional copied variable names

`CheckedTypeStore.deinit` walks all payloads and frees nested allocations.

Even more importantly, several mutation helpers grow immutable slices by
allocating `len + 1`, copying the old contents, freeing the old slice, and
installing the new one:

- `appendSyntheticFunctionRoot`
- `appendSyntheticPayloadRoot`
- `reserveSyntheticTypeRoot`
- `ensureSyntheticSchemeForRoot`
- instantiation/projection helpers that append synthetic roots

This is a prime example of over-micromanaged memory. The store behaves like a
mutable builder for a while, but its representation is already sealed slices.

Recommended strategy:

- Introduce a `CheckedArtifactBuilder` or `CheckedTypeStoreBuilder` with
  `ArrayList` fields.
- Perform all synthetic root/payload/scheme appends on the builder.
- Seal once into immutable artifact data.
- If post-publication mutation is still required, model that as an explicit
  mutable side table or require a builder phase before publication.
- Once sealed, an artifact arena can own the nested payload slices and eliminate
  most deinit walking.

### Checked Body Store

`CheckedBodyStore.fromModule` builds expressions, patterns, statements, bodies,
string literals, and source-node indexes. It has a lot of nested allocation in
payload copying:

- expression argument arrays
- branch arrays
- match pattern remaps
- record fields
- closure captures
- lambda args
- pattern arrays
- string literal copies

`CheckedBodyStore.appendBody` also grows the `bodies` slice with allocate-copy-free.

This is another strong builder/seal candidate. The durable artifact wants a
stable indexed store; the construction process wants appendable arrays.

### Other Artifact Tables

Many artifact tables use the same allocate-copy-free append pattern:

- intrinsic wrappers
- entry wrappers
- executable value transform plans
- executable type payloads
- promoted callable body plans
- promoted callable wrappers
- top-level procedure bindings
- callable eval templates
- resolved value refs
- checked procedure templates
- hosted procs and nested proc site paths

This is not inherently wrong for tiny tables, but it is a pervasive pattern. It
also hides the actual lifetime: these tables usually live exactly as long as the
artifact. They should be appendable during artifact construction and bulk-owned
after publication.

## MIR and Post-Check Lowering

The post-check pipeline in `src/lir/checked_pipeline.zig` is:

1. `mir.Mono.Specialize.run`
2. `mir.MonoRow.run`
3. `mir.Lifted.Lift.run`
4. `mir.LambdaSolved.Solve.run`
5. executable MIR build
6. IR lowering
7. LIR lowering
8. ARC insertion

Each stage consumes explicit facts from the previous stage and produces the next
stage. That matches the design constraints. The memory model is mostly manual
ownership transfer:

- A stage takes the previous program by value.
- On success it moves selected stores into the new program and resets the old
  fields to empty owners.
- On error it deinitializes the input.
- The caller later deinitializes the final stage result.

This is correct but verbose. It is also a natural place for stage arenas, as long
as no later stage borrows from an arena that has been reset.

### Mono Specialization

`src/mir/mono/specialize.zig` builds a mono `Program` with canonical names,
concrete source types, literal pool, symbol store, type store, AST store, proc
lists, root lists, and specialization caches.

There are also many type instantiator maps and temporary arrays. Some of these
temporaries are short-lived and can use a stage scratch arena. The program
stores themselves are durable until row finalization consumes them.

### Row Finalization and Lifted MIR

`src/mir/mono_row/mod.zig` and `src/mir/lifted/lift.zig` both have append-only
program stores plus temporary arrays/maps used while transforming bodies.

Examples of scratch:

- field/tag sorting arrays
- temporary `bool` consumed arrays
- capture sets
- bound/restoration maps
- direct-call collection lists

These are good stage scratch arena candidates. The output AST/type/shape stores
should be either realloc-capable builders or chunked arena-backed stores.

### Lambda-Solved MIR

`src/mir/lambda_solved/solve.zig` is a large solver. The program owns canonical
names, type stores, AST stores, procedure instances, value info stores, solve
sessions, capture sources, and root mappings.

The solver also allocates many temporary maps and arrays:

- SCC adjacency and Tarjan arrays
- reservation queues
- representation/group maps
- active/visited sets
- transform boundary arrays
- callable group sets
- box-erasure provenance arrays
- type lowering maps

There is a clear split between durable representation facts and solver scratch.
The durable facts must live until executable MIR consumes them. The scratch can
be reset at the end of lambda solving or at smaller solver sub-phase boundaries.

### Executable MIR

`src/mir/executable/build.zig`, `ast.zig`, and `type.zig` build executable
facts. The type store has nested slices for tuples, records, tag unions, callable
sets, and erased functions, all freed manually in `Store.deinit`.

This stage would benefit from the same builder/seal pattern:

- Use appendable builders while constructing executable MIR.
- Store final indexed facts in an executable-program owner.
- Use scratch arenas for bridge construction arrays and temporary reorderings.

## IR and LIR

`src/ir/ast.zig` and `src/ir/lower.zig` use append-only stores and local lowering
state. Temporary arrays for args, fields, branch plans, and seen sets are
allocated and freed around each lowering helper.

`src/lir/LirStore.zig` is especially arena-friendly at the conceptual level:

- control-flow statements
- switch branches
- locals
- local id spans
- procedure specs
- string literals

All live for the lowered LIR result. There is no per-item deletion. However, the
current `ArrayList` representation wants realloc. Direct arena allocation would
waste abandoned buffers during growth. A chunked LIR store or a builder that
seals into final arrays would unlock bulk ownership.

`src/lir/lower_ir.zig` has a good ownership pattern:

- `Lowerer.finish` transfers long-lived output fields to the result.
- Temporary maps and local lowering state are deinitialized.
- The lowerer is reset to empty to avoid double free.

That pattern could be preserved while changing the backing storage.

## ARC Insertion and Reference Counting Boundary

`src/lir/arc.zig` is the ARC insertion pass. It is intentionally before the
backends. It uses temporary `OwnedSet` bit arrays, stacks, and maps to compute
where to append explicit LIR `incref` and `decref` statements.

Allocator status:

- Temporary ownership sets use `store.allocator`.
- New RC statements are appended into the LIR store and must live with the LIR.
- Temporary maps/stacks are deinitialized after each procedure/path analysis.

This can be optimized with ARC-pass scratch storage for the temporary sets and
visited maps, but the inserted statements must remain in the LIR store.

The important design boundary is already documented in the code and should stay:
backends lower explicit LIR RC statements. They must not infer or synthesize
reference-counting policy.

## Runtime Image

`src/lir/runtime_image.zig` publishes an ARC-inserted LIR image into shared
memory for interpreter-shim execution.

The child-side `ProgramView` owns no compiler storage. It builds `ArrayList` and
safe-list views from offsets in the mapped image and warns not to call `deinit`.
This is already a bulk-lifetime model: unmapping shared memory releases the
storage. The `allocator = page_allocator` fields in the view are placeholders for
types that require an allocator field, not real ownership.

## Backends

### Dev Backend

The dev backend uses large builder-owned buffers:

- `LirCodeGen` owns code generation state, pending calls, proc registry, local
  locations, join-point maps, loop patch stacks, and static data interning.
- Architecture-specific emitters own machine-code buffers and relocation lists.
- Object writers for ELF/Mach-O/COFF own text/data/rodata buffers, symbol lists,
  relocation lists, string tables, and unwind/function metadata.
- `ExecutableMemory` allocates OS executable memory with mmap/VirtualAlloc and
  frees it with munmap/VirtualFree.

`StaticDataInterner.MemoryBackend` already uses an arena for static data that
lives as long as compiled code execution. That is a good use of an arena because
individual static data blobs do not need to be freed.

Backend builder buffers are output-lifetime data. They can be bulk-owned, but
they are also heavily growable. As elsewhere, use chunked builders or seal final
object/code buffers rather than putting every `ArrayList` directly on an arena.

### Wasm Backend

`src/backend/wasm/WasmCodeGen.zig` owns a `WasmModule`, current function body,
storage, proc/function caches, join-point maps, loop stacks, and scratch keys.

`WasmModule` owns function types, function bodies, exports, imports, data
segments, and table entries. Encoding builds temporary section buffers, appends
them to the output buffer, then returns an owned wasm byte slice.

Good arena candidates:

- Per-function instruction body scratch.
- Per-encode section buffers.
- Whole-module final data if the module is not mutated after encode.

Keep the returned wasm bytes as a normal owned output allocation or as part of a
backend output arena with an explicit result lifetime.

### LLVM Backend

The current `MonoLlvmCodeGen` is a stub for statement-only LIR. The LLVM
`Builder` still exists and is a large builder with many arrays, maps, constants,
metadata, function bodies, and bitcode writer buffers.

`Builder.clearAndFree` and `Builder.deinit` manually clear/deinit a long list of
fields. This is another builder/seal candidate, but it is not currently the main
active code generation path.

The bitcode writer itself owns a managed `u32` buffer and returns an owned slice.
That is a clean output lifetime.

## Scratch vs Durable Facts

Scratch data observed:

- parse scratch nodes and span builders
- import extraction arrays
- diagnostic/report construction temporaries
- canonicalizer scope/name/import maps
- canonicalizer `base.Scratch` lists
- checker unifier and occurs-check scratch
- checker env pool internals after module checking
- exhaustiveness pattern matrices
- MIR transformation temporary arrays and visited sets
- lambda-solved SCC and representation solver maps
- IR/LIR lowering temp arrays
- ARC `OwnedSet`s and visited stacks
- backend per-procedure/per-section temporary buffers

Durable compiler facts observed:

- source bytes while a module is being processed or retained in artifact storage
- `ModuleEnv` and CIR stores until checked artifact publication
- type store until checked artifact publication
- checked artifact stores until the build/lowering/cache no longer needs them
- post-check stage programs until consumed by the next stage
- final LIR/layout stores until backend/runtime image generation completes
- backend output bytes or executable memory until caller consumes them
- reports until they are rendered/drained

The boundary between those two categories should be explicit in APIs. Functions
that produce durable facts should not return slices allocated from scratch
allocators. Functions that only need scratch should not allocate from durable
module or artifact owners.

## Recommended Allocation Strategy

### 1. Fix allocator correctness issues first

Before adding more arenas:

- Deinitialize `base.Allocators` in parse paths that currently skip it:
  `compile_package.doParse` and `coordinator.executeParse`.
- Free legacy `PackageEnv.ModuleState.external_imports` strings.
- Remove `ensureTotalCapacity(..., 256) catch {}` in `PackageEnv.init`.
- Replace `Scratch.SetView` direct `page_allocator` use and allocation-failure
  fallback with an explicit caller-provided scratch map or deterministic path.
- Audit coordinator `catch {}` and `catch &[_]...{}` sites and keep tolerant
  behavior only in parsing/error-reporting code.
- Either wire `WorkerAllocators.arena` into worker task execution or remove the
  unused arena/comment until it is real.

### 2. Introduce explicit task scratch

Add a task scratch allocator that is reset after parse/canonicalize/typecheck
tasks. In coordinator mode, this should be per worker. In single-thread mode, it
can be a reusable phase scratch arena.

Use it for:

- temporary import name arrays
- report-building intermediate strings before promotion
- canonicalizer local maps/scopes that do not escape
- typecheck imported env/artifact slice construction
- MIR/lowering helper arrays
- ARC temporary sets

Do not use it for:

- `ModuleEnv`
- cached AST
- checked artifact stores
- report payloads that outlive the task
- worker results returned to the coordinator unless they are promoted first

### 3. Add artifact builders and seal them

The checked artifact is the clearest long-lived unit. The current artifact
tables often mutate sealed slices by allocate-copy-free. Replace that pattern
with builders:

- `CheckedTypeStoreBuilder`
- `CheckedBodyStoreBuilder`
- builders for wrapper/template/transform tables

Then seal once into `CheckedModuleArtifact`. After sealing, either:

- store final slices in an artifact arena and make `deinit` reset the arena, or
- keep owned slices but eliminate repeated append reallocations.

The arena version gives the biggest simplification, but only after mutation is
confined to builders.

### 4. Split module semantic ownership from task scratch

A module semantic owner should contain:

- source bytes
- `ModuleEnv`
- `CommonEnv`
- CIR node/type/import stores
- type store
- cached AST until canonicalization consumes it

This could be a per-module arena once stores are made arena-safe. Until then, a
module owner with `gpa`-backed growable stores is still useful because it makes
promotion and deinit boundaries explicit.

### 5. Use stage arenas only at consumption boundaries

For post-check lowering, a stage arena is safe only if the next stage owns its
facts independently before the previous stage arena is reset.

Good model:

1. Build stage output in a stage-owned program.
2. Pass that program by value to the next stage.
3. The next stage either consumes/moves the stores or builds its own stores.
4. Reset the previous stage arena only after no pointers into it remain.

This matches the existing consume-by-value shape. The main implementation work
is making the stores arena-safe or builder/sealed.

### 6. Keep backend RC policy unchanged

Allocator improvements should not move ownership analysis into backends. ARC
insertion should continue to produce explicit LIR `incref`, `decref`, and
`free` statements. Backends should keep dumbly lowering those statements and
builtin/runtime helper calls.

## Priority List

1. Fix parse `base.Allocators` deinit omissions and the legacy external import
   string leak.
2. Remove allocator failure fallbacks and direct `page_allocator` bypasses from
   non-parse/non-error-reporting paths.
3. Wire real per-worker task scratch into the coordinator.
4. Replace checked artifact allocate-copy-free append tables with builders.
5. Make `CheckedModuleArtifact` optionally arena-owned after builder sealing.
6. Split `ModuleEnv` durable storage from canonicalizer/checker scratch in APIs.
7. Convert hot append-only stores (`CIR`, MIR AST stores, IR store, LIR store)
   to chunked or sealed builders before using arenas underneath them.
8. Add per-stage post-check arena ownership once stage outputs do not borrow
   from soon-to-be-reset memory.

## Bottom Line

The suspicion is correct: the compiler is manually freeing many structures whose
actual lifetime is much coarser than the code suggests. The largest wins are not
from putting the current `ArrayList` and `HashMap` allocations directly on
arenas, but from making the lifetime scopes explicit and using arenas at those
scope boundaries.

The most important target is `checked_artifact.zig`: it contains many immutable
or append-then-seal stores, and its deinit logic is both large and easy to get
wrong. The next most important target is worker/task scratch in the coordinator:
the intended arena exists but is not wired through task execution, so the build
currently pays for many individual allocations in places where bulk reset is the
natural lifetime.
