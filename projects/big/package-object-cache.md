# Package Object Cache

## Problem

A dev build re-lowers every dependency from checked artifacts on every run.
Parsing, canonicalization, and type checking of an unchanged package are
already cache hits (`Coordinator.storeCheckedModuleInCache`,
`src/compile/coordinator.zig:3160`), but everything after checking is one flat
whole-program unit: Monotype specialization, closure lifting, SpecConstr,
lambda-set solving, LIR lowering, the LIR passes, ARC, and codegen all run over
package and builtin code as if it were app code, and the result is exactly one
relocatable object per build (`src/backend/dev/ObjectFileCompiler.zig:164`,
`src/cli/main.zig:10336`). Measured on real apps with a debug compiler, the
post-check "Specializing" phase is the dominant cost of a warm dev build
(roc-signals task-board: 17.5s of 21.1s; roc-signals conduit: 26.7s of 61s;
roc-ray top_down: 1.5s of 2.3s), while type checking is a small share and is
already cached.

The second cost is quality, not time. Dependencies such as roc-deflate rely on
optimization to be usable at all, but a dev build compiles them with the dev
backend. Nothing lets a package be optimized once and reused.

This project caches compiled objects per specialization so that a dev build
links already-compiled package code instead of lowering it, and so that
package code can be LLVM-optimized once and reused by every app on the machine.
It keeps monomorphization and the existing lambda-set representation. It does
not introduce a polymorphism-stable runtime ABI, descriptor passing, or boxed
closures.

## Why specialization-level caching, and what the census says

A polymorphism-stable ABI (opaque values plus a runtime layout descriptor per
type variable, unboxed) would let a package be compiled without knowing any
app's instantiations. It is a large new lowering mode with a permanent
performance cliff for polymorphic code and a new correctness surface. The
census over fifteen apps from roc-deflate, roc-ray, roc-wasm4, and roc-signals
shows the cheaper design captures most of the value:

- Package and platform callables are overwhelmingly monomorphic by signature:
  1,175 of 1,328 callables seen. roc-deflate is 100% monomorphic (every one of
  its 71 annotated functions is first-order with closed error unions); the
  roc-wasm4 platform is 100%; roc-ray's platform is 148 of 168 in its largest
  app.
- Polymorphic callables are bimodal. Of 293 polymorphic package and builtin
  callables, 103 had exactly one specialization across all apps, and every
  callable with more than 100 specializations carries a function type in its
  requests (`Capability.handle` 500, `Box.box` 466, `Signal.map` 234). Among
  polymorphic callables never requested with a function type, 85 of 166 had
  exactly one specialization.
- Sharing across apps is real: 278 monomorphic package callables and 89
  monomorphic builtin callables appeared in at least two apps.

The closure-heavy tail is roc-signals' reactive engine, which by design
instantiates its combinators at app types with app lambdas. That code cannot
be shared across apps under any monomorphizing design; it can only be kept
warm across rebuilds of one app.

## Cache unit and identity

The unit is one specialization: the object code of one LIR procedure plus the
static data, refcount helpers, and dependency list it needs. Each entry is
content-addressed by the specialization's identity, which is the identity
Monotype already writes once at reservation
(`src/postcheck/monotype/specialize.zig`): callable identity, checked source
function type digest, evidence digest, and requested monomorphic function type
digest, extended past Monotype with the solved function type including its
lambda-set members and capture shapes (the `(symbol, solved_fn_ty,
capture_shape)` key of `src/postcheck/lambda_mono/specialize.zig`).

### What the key is made of

Module identity survives into Monotype in usable form. A callable identity
names its module by the checked artifact key (`CheckedModuleArtifactKey`,
`src/check/checked_artifact.zig`), whose module identity component is a deep
content identity of the transitive closure's module names and source bytes
with no coordinator-assigned indices, paths, or display strings, and whose
other components are the source hash, the compiler artifact hash, the checking
context, and the direct import keys. For a URL package module that key is
identical in every app that uses the same package version and compiler. The
procedure base and template ordinals inside it are module-local and
deterministic from the module's own source. Request type digests are content
identities (the census saw one `List.len` instantiation under a single digest
in 11 unrelated apps), and structural types such as records and tag unions
digest by structure alone.

The specialization identity is therefore: callee module artifact key,
procedure base ordinal, template ordinal, evidence digest, the demand-adjusted
request type digest, and past Monotype the lambda-set member identities and
capture shapes. The object key is that identity hashed with the compiler
artifact hash, target triple, and backend option set. The manifest lists the
identities of everything the object references. What is program-local today
and must be replaced is only the derived naming: `ModuleIdentity.module_idx`,
LIR symbols (`roc__proc_{hex}` from `lir.Symbol.raw()`,
`src/static_data.zig:132`; `roc_proc_{ordinal}` in
`src/backend/llvm/MonoLlvmCodeGen.zig`), layout indices, and refcount helper
names (`roc__rc_helper_{op<<32|layout_idx}`,
`src/backend/dev/ObjectFileCompiler.zig:432`). The first milestone derives
each from the identity above.

### Identity granularity for app-side names

A tier 2 entry's identity mentions app-side things: a nominal type the app
declared, a method the app implemented that reaches the callee as dispatch
evidence, or an app lambda in a lambda set. Today a nominal type is identified
by its declaring module's content identity plus its name (`NamedType` in
`src/postcheck/monotype/type.zig` carries the module digest). Under that rule
any edit anywhere in the module that declares `Model` changes the identity of
every package specialization instantiated at `Model`, `List(Model)`,
`Dict(Str, Model)`, and so on, and the whole tier 2 set for that module misses
on every keystroke.

The rule for app-side names in cache identities is therefore: identify a
declaration by what compiled code can depend on, never by its module's source
hash. A nominal type is its module name, its type name, and the structural
digest of its declaration taken transitively through the nominal types it
mentions. A method is the identity of its owning type, its name, and its
checked type. A lambda is its module name, definition path, site ordinal,
capture shape, and solved type. An edit that changes none of those leaves
every dependent entry valid, and an edit that changes any of them changes
exactly the entries whose code would differ. Structural types already behave
this way. The checked-module cache keeps its content-hash key; that cache
answers a different question and is not affected.

Two tiers share this one store.

**Tier 1, closed specializations.** A specialization is closed when its
callable is not in the app and its request type contains no lambda set at any
depth, looking through nominal backings. A request never contains a type
variable: Monotype requests are closed monomorphic types by construction, so a
polymorphic callable such as `List.set` has closed specializations whenever a
package's own monomorphic code instantiates it, and those are as cacheable as
a monomorphic function. Closedness is decidable at Monotype reservation time
from the request type alone, before the body is lowered. A closed
specialization's object is determined by its package alone, so it is shared by
every app and can be produced at package download time, including with LLVM.

Boxed functions are deliberately exempt from the lambda-set test. An erased
callable (`Box(fn)`, layout `erased_callable`) has one pointer-sized layout and
one calling convention regardless of which closure it holds, which is the same
property that lets platforms pass them across the host boundary today. A
request that mentions a function type only behind a `Box` is therefore closed.
This matters for roc-signals, whose `Html` and `Gui` modules take and return
`Elem` values that carry boxed closures but are themselves closed by this
rule.

**Tier 2, app-instantiated specializations.** Everything else: callables of
one module instantiated at types, methods, or lambdas declared in another.
These are keyed identically and stored in the same place; they are simply
determined by two modules rather than one, so they are shared only among
programs that request the same identity. "App" and "package" are not
categories the cache knows about; the tiers describe how many modules a
specialization's identity depends on.

### Specialization identity by demand, not by full request type

Today two requests with different types always produce two specializations,
even when the callee's body cannot tell them apart. `List.len : List(a) -> U64`
never inspects an element: it reads a length field at a fixed offset whatever
`a` is. The census found 139 distinct `List.len` specializations across 14
apps, one per element type, every one of them the same machine code. The same
holds for any type variable a body never touches, and in weaker form for
variables a body touches only through their layout: `List.get` at `List(U8)`
and `List(I8)` are identical code, and so are `List.set` at any two record
types with the same size, alignment, and refcount plan, because `List`
operations move elements as opaque bytes and refcount them by plan.

The identity should therefore be computed over the *demand* the callee makes
on each type variable of its scheme, decided once per checked body and not
per request:

- **none**: the variable is never inspected, moved, refcounted, or dispatched
  on (a phantom, or `List.len`'s `a`). It contributes nothing to the identity;
  every instantiation shares one specialization, which can be built eagerly
  the moment the callable is seen.
- **layout**: the variable's values are moved, stored, or refcounted but never
  dispatched on or operated on numerically (`List.get`, `List.set`,
  `List.with_capacity`, `Box.box`). It contributes its layout digest, so all
  element types with one layout and refcount plan share one specialization.
- **full**: the body dispatches a method on the variable, performs numeric
  operations whose semantics depend on it, or matches on its structure. It
  contributes the full type digest as today.

The demand class is a property of the checked body and its static dispatch
plans, and layout is a pure function of type, so this is a refinement of the
existing Monotype identity computable at reservation time: replace each
`none` variable in the request type by a canonical placeholder and each
`layout` variable by its layout digest before digesting. The rule must be
derived from explicit checked data (dispatch plans, low-level operation
signatures, the layout store), never from scanning bodies for names. The
zero-sized element case stays distinct where the layout store gives it a
distinct layout (`list_of_zst`).

This is a compile-time win on its own, since every build emits fewer
specializations, and it multiplies the reach of the shared tiers: a `none` or
`layout` specialization requested by an app is far more likely to already
exist in the prebuilt builtin set or in another app's entries than a
full-type one.

### Lambda identities are site-and-shape, never body content

A tier 2 specialization that receives an app closure calls it through a direct
symbol after the lambda-set tag switch. If that symbol's name derives from the
closure's module name, definition path, nested site ordinal, capture shape, and
solved type, then editing the closure's body changes the closure's own object
and nothing else: every package specialization that received it keeps hitting.
Identities must therefore never fold in a body hash. This is what keeps the
roc-signals dev loop warm.

## Compilation under boundary-conservative assumptions

A cached object was compiled in a different program than the one linking it.
Every pass that uses whole-program facts must, for a cached specialization,
use only facts closed under the specialization's own dependency closure:

- ARC ownership signatures are solved to an interprocedural fixpoint
  (`src/lir/arc_solve.zig`). A cached procedure's signature is solved within
  its closure, recorded in its manifest, and becomes part of its ABI; the
  linking program's ARC treats it as a fixed external signature.
- `TagReachability` prunes based on what the program constructs. A cached
  procedure is compiled with no pruning derived from callers.
- SpecConstr call-pattern clones (`src/postcheck/monotype_lifted/spec_constr.zig`)
  and the single-use inliner clone bodies across procedures. A caller-driven
  clone of a cached callee is an app-side specialization with its own
  identity; it never rewrites the cached entry.
- Static dispatch evidence is already part of the identity.

The correctness gate is differential: build an app cold, build it warm with
every hit taken, and compare program output and refcount event logs. The
existing differential harnesses are the model.

## ABI of a cached procedure

The dev backend calls Roc procedures with the native C calling convention
(`src/backend/dev/CallingConvention.zig`); the LLVM backend uses a private
`roc_proc_N(ret_ptr, args_ptr)` packed-buffer convention
(`src/backend/llvm/MonoLlvmCodeGen.zig:3-7`). Objects from either backend must
link into programs from either, so a cached procedure's entry uses a C-ABI
signature classified from its layouts by `src/layout/abi`, which the dev
backend already produces and which LLVM gains as an emission mode for boundary
procedures. Calls internal to one cached unit may keep whatever the backend
prefers. The recorded ownership signature and return-slot convention complete
the ABI.

## Constants are static data

A cached object stores every compile-time constant it references as read-only
data, never as a procedure that constructs it. The design already requires
this for the whole program ("Compile-Time Evaluation And Static Storage" in
`design.md`), and the LIR lowerer has the mechanism
(`lowerStaticDataCandidateInto` and `layoutNeedsStaticData`,
`src/postcheck/solved_lir_lower.zig`). roc-lang/roc#11376 documents a case
where a folded `List.repeat(record, 50001)` is instead emitted as a 200,007
line procedure; fixing that is a prerequisite, and the object cache adds the
rule that a static datum is a first-class cache entry: content-addressed by
its ConstStore node and concrete type, emitted once as a read-only symbol with
a content-derived name, and referenced from procedure entries through the
manifest. Two entries that reference the same datum share one symbol at link
time.

## Refcount helpers and other per-layout code

Refcount helpers, structural equality and hashing, and any other code generated
per layout are emitted as weak or linkonce symbols named by layout content
digest, so that objects compiled separately deduplicate at link time. This
needs weak-symbol support in the ELF, Mach-O, and COFF writers under
`src/backend/dev/object/`.

## Manifest and dependency closure

Each entry carries a manifest: its identity, its ABI record, the identities of
every specialization it calls, every static datum and per-layout helper it
references, and the compiler build, target, and option set it was compiled
under. A lookup for a specialization is a hit only if the entry and its
transitive closure are all present; a missing dependency makes the root a
miss, and the program lowers it normally. Entries are never rewritten.

## Determinism

The contents added to the store by a build of program P are exactly the
specializations P's finished program emits after reachability, each under its
content identity. This is a function of the program, not of scheduling, so the
same codebase produces the same entries on any machine with the same compiler
build and target. Eviction is by last use through the existing sweep; it
only ever deletes files and never changes what a build writes, so it cannot
make two builds of the same tree write different bytes.

Two properties make this true and both are gated:

1. The emitted specialization set is order-independent. Monotype's index
   registers a finished record under two lookup entries, its request type
   digest and its solved type digest (`matchInBucket` in
   `src/postcheck/monotype/specialize.zig` scans request-view matches first,
   then solved-view aliases). A request is "less specific" than its own
   solution when it was reserved before its requester's graph was sealed and
   refined afterwards (`refineRequest`). If request X arrives first and
   solves to S, a later request for S reuses X's record through the solved
   alias: one record. If S arrives first, a later request X finds no entry
   under X's digest and creates a second record whose body is identical: two
   records. The emitted program and the set of identities both depend on
   scheduling. Two changes remove this: every finished record is registered
   under every identity that resolved to it, so the identity set is a function
   of the request set; and `markReady` merges a record whose solved type equals
   an existing record's request or solved type into that record, so the
   emitted program has one body per solved type in either order. A gate builds
   with one thread and with many and diffs the emitted identity manifests.
2. Objects are byte-deterministic: canonical symbol and relocation order, no
   timestamps, and for LLVM a pinned version and flag set. A gate builds twice
   and compares bytes.

Concurrent writers produce identical bytes, so the write protocol is stage
into a private directory, rename into place, and never overwrite an existing
entry, as `downloadAndPublish` in `src/compile/package_resolution.zig` already
does for package directories. A debug gate compares bytes on collision.

## Entry format and on-disk layout

The unit of identity is the specialization. An entry is not an object file. It
is the per-procedure artifact the dev backend already produces and `RunImage`
already streams for hot reload: code bytes, relocation records against
symbolic targets, and read-only data contributions, plus the manifest.
LLVM-built entries take the same shape by compiling with one section per
function and slicing the object into per-function records with
`src/backend/dev/object_reader.zig`. At build time the object writer
concatenates procedure artifacts and resolves relocations exactly as it does
today; some artifacts come from the cache instead of from codegen. The linker
sees no more inputs than it does now.

Every file in the store is a pure function of source bytes, the compiler
artifact hash, the target, and the option set, so two machines building the
same tree write byte-identical files at identical paths, the rename race is
harmless, and no file is ever rewritten in place. There is one file per
module per target and option set, exactly as the checked-module cache has one
file per module.

**A module's pack contains every specialization its code reaches that is not
already in the pack of a module it imports.** For a package module such as
roc-deflate's `HuffmanEncode.roc` that is its own functions at their concrete
types plus the builtin instantiations it uses that `Builtin.roc`'s own pack
does not carry, such as `List.get` at `List(U32)`. A module that imports
`HuffmanEncode` excludes what that pack already holds and adds only what is
new to it. An app's root module gets a pack holding its own functions plus
the cross-module specializations it is the first to need, such as
`Signal.map` at the app's `Model` with the app's lambda. Because the set is
computed against the imports' packs, which are themselves functions of the
imports' content, the pack is a function of the module and its transitive
imports and nothing else. `Builtin.roc`'s pack is produced by the compiler
build and extended with the curated seed list, which is part of the compiler.
A pack is one file: header, sorted index from identity to offset and length,
entry blobs.

The pack's content is canonical, not "whatever this build reached". It is the
complete set of closed specializations reachable from all of the module's
exports, minus what its imports' packs hold. A build that needs a module's
pack and does not find it lowers that complete set, including exports the
current program never calls, and writes it; otherwise a program that reaches
only part of a module would write a smaller pack to the same path that a
fuller program writes, and the rename race would no longer be between
identical bytes. Lowering unreached exports is a one-time cost per package
version and a per-edit cost bounded by the edited module's size. The object
writer still links only the entries the program reaches.

App-instantiated specializations of a module's functions are reached from the
requesting module's code, not from the callee's, so they live in the
requester's pack, and a package module's pack never depends on any program
that uses it. Platform modules checked against an app's `requires` already
carry the platform-app relation in their checked artifact key, so a
`Model`-parameterized platform's packs are per-app, which matches the fact
that their code is instantiated at `Model`. With these rules two writers can
meet at one path only with identical bytes, and no locking is needed.

Lookup is by identity across every pack present, through a per-machine index
that is rebuildable from the packs and not part of the store's contract. When
a module is edited its artifact key changes and its pack is written fresh,
but the specializations inside it whose identities did not change still hit
against the previous version of the pack, and the build copies their bytes
into the new pack instead of recompiling them. Warmness therefore survives
edits; the cost of an edit is writing one pack the size of that module's
compiled dependencies, a few megabytes for the roc-signals apps, and the
previous version of the pack persisting until collection.

Duplication of entry bytes occurs in two bounded forms, both consisting of
identical bytes: between successive versions of an edited module's pack until
collection, and between modules that reach the same specialization without
importing one another. Storing cross-module specializations as one file per
identity would avoid both, and tools such as git, ccache, and Bazel's disk
cache do store one file per object, but it creates thousands of files per
program (the census counts roughly 2,000 for roc-signals task-board and 3,500
for conduit) and file creation is the expensive operation on every platform.
The per-edit pack rewrite is a few megabytes of sequential writing and is
cheaper than that, so the store uses module packs only. If a very large
module's per-edit write ever measures as significant, the remedy is to split
that module's pack, not to introduce per-entry files.

Collection reuses the existing background sweep (`src/compile/cache_cleanup.zig`)
with a policy specific to this cache; `mod/`, `exe/`, `test/`, and
`wasm-host/` keep their current 30-day mtime rule untouched. The pack
directory is split by module origin, which the build already knows: `pkg/`
holds packs for modules whose package or platform arrived as a URL bundle,
which are written once per package version and only ever read; `local/` holds
packs for modules reached by path, including the app and locally checked-out
packages and platforms, where every edit writes a new pack and the previous
one is never read again. The sweep gives `pkg/` a 30-day window and `local/`
a window on the order of one day, so rewritten packs cannot pile up while
optimized dependency packs survive. The split is by origin, not by backend
mode: a dev-backend pack for a downloaded package is as stable as the LLVM
one, and only rewrites of local modules accumulate. The builtin seed pack
ships inside the compiler's version directory and is not swept.

For these two directories the sweep ages a file by `now - max(atime, mtime)`
rather than mtime alone, so that an in-use pack is not deleted and rebuilt
once per window merely because it was written long ago. No build writes to
the cache directory to record a read. Linux `relatime` refreshes atime on
read whenever it is more than a day old, which is sufficient for both
windows, and macOS refreshes it on read including through mmap. Where atime
is not maintained (`noatime` mounts, NTFS with last-access updates disabled)
the age is time since write, which is the sweep's existing behavior and costs
one rebuild of an in-use pack per window. Deletion never affects what a build
writes, so determinism is unaffected. Enforcing the size cap that
`CacheConfig.max_size_mb` already declares, evicting oldest first, remains a
worthwhile backstop for every cache directory.

Files live under the compiler cache root beside the checked-module cache, in
a directory keyed by compiler build, target triple, PIC, debug, and backend
option set. The checked-module cache stays the source of interface truth: the
app is always type-checked against the package's checked artifact, and the
object cache is consulted only when lowering a request.

## Where hits happen

A closed request is recognized at Monotype reservation before the body is
lowered; on a hit the program records an external procedure reference with the
manifest's ABI and skips Monotype body lowering, lifting, lambda solving, LIR
lowering, the passes, ARC, and codegen for that procedure and its closure. A
request containing a function type is not closed until lambda solving
finishes, so its earliest hit is at the Lambda Mono queue; Monotype and lifting
have already run for that body. Tier 1 gets the full skip; tier 2 with lambdas
gets a partial one. Measuring how much of tier 2's cost is in the stages before
the hit point decides whether a lambda-set-aware earlier hit is worth building.

## Linking and hot reload

`roc build --opt=dev` passes the hit objects to the embedded LLD alongside the
app object (`src/cli/linker.zig`). For `roc run`, the host executable is
already linked once and cached under `exe/` keyed by host identity
(`src/cli/main.zig:2494`); package objects are linked into that executable and
added to its key, and the app image streamed into shared memory resolves
package symbols by name at relocation time. Only the app image reloads on an
edit.

## Prebuilt builtin specializations

`Builtin.roc` is the one package every program depends on, and its
instantiations recur across unrelated apps: request digests are content
identities, and in the census one `List.len` instantiation appeared under the
same identity in 11 of 15 apps, 57 of roc-deflate's 146 builtin
specializations recurred in other apps, and about half of each roc-signals
app's builtin specializations were shared with its siblings. The compiler
build therefore produces a seed set of cache entries, LLVM-optimized, and
ships them the way `roc_builtins.o` is shipped today (`src/cli/main.zig:459`).
The seed set is every closed specialization reachable from `Builtin.roc`'s own
monomorphic code (its internal uses of `List(U8)`, `List(Str)`, `Str`, and
the numeric types) plus a curated list of request types added as measurements
justify them. Seed entries are ordinary content-addressed entries under the
same identities, so an app that requests one gets a hit with no special path,
and an app that requests something outside the set falls through to ordinary
lowering. Whether the set is embedded in the binary or materialized into the
cache directory on first use is an implementation choice; either way the set
is regenerated per compiler build because the identity folds in the builtin
artifact.

## Optimized objects

A closed tier 1 entry can be compiled with LLVM at package download time with
no app involvement. Tier 2 entries can be LLVM-compiled only after an app
requests them; the first build emits them with the dev backend and a detached
background job produces the optimized object for the next build, both under
the same identity but distinct option sets in the key. `roc check` never
triggers either. The interpreter backend and `--opt=speed` app builds do not
consult the object cache: the former has no objects, and the latter wants
cross-package inlining.

## The Monotype specialization cache is removed

`src/postcheck/monotype/serialize.zig` and the loaded-shard read path in
`src/postcheck/monotype/lower.zig` implement an on-disk cache of Monotype
output that is populated only by a unit test and whose validity id is
whole-program. It caches the wrong stage for this project and is deleted in
the first milestone; its import-table idea (a loaded unit names another unit's
function by a stable slot) survives as the manifest's dependency list.

## Non-goals

No descriptor-passing or otherwise polymorphism-stable ABI. No change to boxy;
`--specialize=no` programs do not use the object cache. No caching for
`--opt=speed`, `--opt=size`, or the interpreter.

App modules are not a non-goal. The mechanism does not distinguish app modules
from package modules: a closed specialization is keyed by its callee's module
artifact key, and every module has one. An app module that is not being edited
therefore caches exactly like a package, and in a multi-module app editing one
module leaves the other modules' entries hitting. The module under edit
misses by construction, since its artifact key changes with every edit, and
its entries are written anyway so that the store's contents remain a function
of the program rather than of edit history; the last-used sweep reclaims
them.

## Milestones and gates

1. **Identities.** Content-derived module identity, procedure symbol names,
   layout digests, and lambda-site names in both backends. Demand classes per
   scheme variable and the demand-based request digest. Delete the Monotype
   specialization cache (roc-lang/roc#11379). Gate: two builds of the same program produce
   byte-identical objects; one-thread and many-thread builds produce the same
   emitted identity manifest.
2. **Boundary compilation.** ARC signatures, tag reachability, SpecConstr, and
   inlining restricted to a specialization's closure when it is compiled as a
   cache entry; ABI record; static data and per-layout helpers as weak,
   content-named symbols. Gate: the cold-versus-warm differential over the
   full test corpus.
3. **Tier 1 with dev objects.** Reservation-time hit, manifests, on-disk
   store, LLD linking, `Builtin.roc` and roc-deflate as the first packages.
   Gate: differential plus a CI measurement of warm dev build time on the
   census apps.
4. **LLVM objects.** C-ABI boundary emission in the LLVM backend, download-time
   optimization of tier 1, cross-backend linking, and the prebuilt builtin
   seed set produced at compiler build time. Gate: differential across
   backend mixes, and a check that every seed entry is byte-identical to what
   an app build would produce for the same identity.
5. **Tier 2 and hot reload.** Lambda Mono hit point, site-and-shape lambda
   identities, package objects in the cached host executable, background
   optimization of tier 2 entries.

## Measurement notes

The census hook that produced the numbers above is an environment-gated
instrumentation of `src/lir/checked_pipeline.zig` (`ROC_SPEC_CENSUS=1`) that
prints one line per Monotype specialization and per final LIR procedure. It is
worth keeping as a permanent diagnostic once it prints proper names for
procedures declared inside type blocks. The roc-deflate example is a poor
timing benchmark because CTFE folds its compression of a string literal; with
the folded buffers made runtime-dependent, its final LIR drops from 492,853
lines to 60,039.
