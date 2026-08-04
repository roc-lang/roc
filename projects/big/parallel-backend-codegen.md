# Parallel Backend Code Generation

## Problem

The frontend already scales across cores. The compile coordinator is an
actor model — "the single owner of all mutable state in the compilation
pipeline" with "Workers are pure: they receive tasks, return results"
(`src/compile/coordinator.zig:1-32`) — and it parses, canonicalizes, and
type-checks modules concurrently on `std.Thread.getCpuCount()` workers
(`coordinator.zig:2503`). Once that parallel frontend finishes, the
dev backend lowers the whole program to machine code on a single thread:
`ObjectFileCompiler` walks every procedure in one sequential loop
(`compileAllProcSpecs`, `src/backend/dev/LirCodeGen.zig:14217-14236`),
appending each proc's bytes into one shared code buffer. On a program with
thousands of monomorphized procs this codegen pass is a serial tail on an
otherwise-parallel build, and on a many-core machine it dominates
dev-build wall-clock after the frontend has drained.

Nothing about generating one proc's code depends on generating another
proc's code concurrently: the inputs are read-only and the cross-proc
references are already expressed as relocation records. The per-proc
output the backend produces — final code bytes plus relocations plus its
readonly-data contributions — is exactly the artifact a worker could
produce independently and hand to a single writer. This project makes
codegen produce those per-proc artifacts on worker threads and consume
them through one serialized, deterministic writer. **It introduces no new
IR layer between LIR and machine code; the unit of parallelism is the
per-procedure artifact the backend already emits.**

## Current architecture (the sequential reality)

The dev backend is a comptime-target-parameterized driver,
`LirCodeGen(comptime target: RocTarget)` (`LirCodeGen.zig:422`). One
instance is constructed from three inputs, all read-only or private:
`init(allocator, store: *const LirStore, layout_store: *const LayoutStore,
static_strings)` (`LirCodeGen.zig:919`). It compiles procs in two
sequential loops (`compileAllProcSpecs`, `:14217`): first every proc is
registered, then `compileProcSpec` (`:14303`) is called for each in
spec-id order, then `patchPendingCalls`/`patchPendingProcAddrs` resolve
cross-proc references.

Everything a proc emits is per-proc data today; it is only the
*accumulation* that is shared and sequential:

- **Code bytes.** `compileProcSpec` appends into one buffer;
  `getGeneratedCode` returns `self.codegen.getCode()`, the single blob
  (`:18154`). Each proc's extent within that blob is recorded as
  `CompiledProc.code_start`/`code_end` (`:695-718`) and surfaced via
  `compiledProcSymbol` (`:14284`).
- **Relocations.** `Relocation` (`src/backend/dev/Relocation.zig:17-79`)
  is a per-offset record: `local_data` (inline bytes to place in the data
  section), `linked_function` (a call/address patched by **symbol name**),
  `linked_data` (a data address patched by name), and `jmp_to_return`
  (proc-internal). `getRelocations` returns the one shared list
  (`:18164`).
- **Rodata.** String and byte-list literal backings are materialized
  up-front from the whole store by `StaticStringData.build`
  (`src/backend/dev/StaticStringData.zig:57`) into `StaticDataExport`
  records (name + bytes + relocations), independent of per-proc codegen.

The single writer already exists and is a pure serializer:
`ObjectWriter.generateObjectFile(code, rodata, symbols, relocations,
rodata_relocations, output)` (`src/backend/dev/ObjectWriter.zig:17-27`)
"takes generated machine code and relocations and produces a relocatable
object file" (`src/backend/dev/object/mod.zig:1-9`), dispatching to the
elf/macho/coff writers. `compileWithCodeGen`
(`src/backend/dev/ObjectFileCompiler.zig:179-451`) is the glue: it
publishes one symbol per proc from `compiledProcSymbol` (`:260-295`),
resolves each `linked_function`/`linked_data` relocation against the
symbol table by name (`:367-416`), and calls the writer once. The build
driver invokes this sequentially from `src/cli/main.zig:9198-9224`.

The just-in-time path has the identical shape. `RunImage`
(`src/backend/dev/RunImage.zig:1-6`) serializes "machine code plus
explicit relocation and readonly data records" into shared memory; the
`machine_code_shim` maps it, patches the records, and calls the
entrypoint. Its `RelocationRecord` "names the target symbol explicitly"
(`RunImage.zig:90`) and each proc appears as a named `CodeSymbol`
(`:119-123`). So both consumers — object writer and run image — already
resolve inter-proc references by name at layout time.

The one place a proc→proc reference is **not** a relocation is direct
calls: `emitPendingCallToProc` (`LirCodeGen.zig:12377`) records a
`PendingCall{call_site, target_proc}` with an absolute offset into the
shared buffer, and `patchPendingCalls` (`:14138`) rewrites it to the
callee's absolute `code_start`. That in-buffer patch is an optimization of
exactly the operation a `linked_function` relocation performs
declaratively. This is the coupling the design converts.

The wasm backend already has the same per-artifact structure: each
function body is accumulated into its own self-contained `CodeBuilder`
("Accumulates one WASM function body with deferred relocation
resolution", `src/backend/wasm/CodeBuilder.zig:1`) held in
`pending_bodies: AutoHashMap(LocalFunctionIndex, CodeBuilder)`
(`WasmCodeGen.zig:233`), with per-body relocations keyed by
symbol-table index (`WasmLinking.zig:57-80`), assembled into the module
in function-index order by `flushPendingBodies`
(`WasmCodeGen.zig:1536-1564`), which sorts the bodies by function index
(`:1545`) and appends each through `CodeBuilder`'s `insertIntoModule`
(`src/backend/wasm/CodeBuilder.zig:129`, invoked at `:1564`). The only difference from the dev backend
is the relocation identifier — a symbol-table index rather than a symbol
name string — not the artifact shape.

## The enabling invariants

Three facts make per-proc artifacts independently generatable; the design
depends on them and states them as invariants:

1. **The LIR store and layout store are read-only during codegen.** The
   driver holds `store: *const LirStore` and `layout_store: *const
   LayoutStore` (`LirCodeGen.zig:513-517, 921-922`) — the immutability is
   already in the types. `LirProgram.Result` (`src/lir/program.zig:146`)
   is fully built before codegen begins and is only read thereafter. N
   worker driver instances can share these `*const` borrows with no
   synchronization, exactly as the frontend shares `builtin_modules:
   *const BuiltinModules` (`coordinator.zig:948`).

2. **Cross-proc references are already nameable relocations.** Every
   external and inter-symbol reference the writers resolve is keyed by a
   deterministic symbol name (`static_data_export.procSymbolName`,
   `ObjectFileCompiler.zig:269`; the `linked_function`/`linked_data`
   name fields). A proc that emits its call targets and address-of-proc
   references as `linked_function` relocations instead of in-buffer
   `PendingCall` patches is complete on its own — it needs no other
   proc's final address at generation time.

3. **The per-proc output is a value, not a position in shared state.** A
   proc's artifact is `(spec id, code bytes starting at offset 0,
   relocations relative to offset 0, rodata contributions)`. Nothing in
   that tuple references another proc's buffer offset. This is the
   handoff unit; it requires no IR between LIR and machine code.

## Solution design

**Reuse the coordinator's actor pattern.** A codegen worker pool mirrors
the frontend: one owner thread holds all mutable output state; N pure
workers each own a private arena (`WorkerAllocators`,
`coordinator.zig:382`) and a private `LirCodeGen(target)` instance;
tasks and results travel over bounded channels (`src/compile/channel.zig`,
"Bounded capacity provides backpressure", `channel.zig:8`). Whether this
is a second phase of the existing coordinator or a sibling pool
instantiated after lowering is an implementation choice; the messages and
ownership discipline are the same. Worker count comes from the existing
`--jobs`/`max_threads` surface (`src/cli/main.zig:146-149`), where a value
of 1 selects single-threaded execution.

**Work queue.** The tasks are the proc specs
(`lir_result.store.getProcSpecs()`, `cli/main.zig:9216`), each carrying
its `LirProcSpecId`, with one exclusion the queue must preserve: procs
with `is_static_initializer` set are skipped by `compileAllProcSpecs`
today (`LirCodeGen.zig:14230`), so their spec ids must never enter the
worker pool either. A worker pulls a spec, generates it into a private
offset-0 buffer with all cross-proc references emitted as name-keyed
relocations, and returns `{spec_id, code, relocations, rodata, dwarf line
entries, unwind info}`.

**One serialized, deterministic writer.** The owner collects results and
assembles the final object exactly as `compileWithCodeGen` does today,
with one addition that is the crux of determinism: **procs are appended in
ascending spec-id order, not completion order.** The frontend coordinator
collects results in arrival order and relies on keying each result into a
fixed per-module slot by `module_id` for determinism
(`coordinator.zig:2617-2620, 3373-3386`) — but codegen concatenates bytes,
so arrival order would leak into the output layout. The writer therefore
buffers out-of-order completions keyed by spec id and emits each proc only
when it is the next in sequence. On emission it assigns the proc its base
offset in the growing code blob, shifts that proc's relocation offsets by
the base (`Relocation.adjustOffset`, `Relocation.zig:68-78`, already
exists), appends its rodata with the same alignment logic
(`appendStaticDataExport`, `ObjectFileCompiler.zig:493`), records its
symbol, and resolves inter-proc `linked_function` references against the
accumulated symbol table. The output byte layout is then a pure function
of spec-id order and each proc's bytes — identical run-to-run and
identical to single-threaded mode.

**Backpressure.** The result channel bounds in-flight artifacts. Codegen
artifacts are larger than frontend results (whole proc code buffers), so
an unbounded queue would let fast workers race ahead and grow memory
without limit. Using the blocking `send` variant (`channel.zig:84-104`)
rather than the growable one makes a worker block once the writer is
`capacity` results behind, capping peak memory at roughly `capacity`
artifacts.

**Shared-state resolutions.** Each structure that is shared-mutable in the
single-buffer driver today is resolved to either per-worker-private or
owned-by-the-writer:

- *Code buffer, relocation list, `proc_registry`, `pending_calls`/
  `pending_proc_addrs`* — become per-worker-private and offset-0-local.
  Cross-proc calls become `linked_function` relocations; the writer owns
  the global symbol table and does resolution. `patchPendingCalls`'
  absolute-offset patching is removed for the object/JIT paths in favor of
  named relocations.
- *RC helpers* (`compiled_rc_helpers: AutoHashMap(u64, usize)`,
  `LirCodeGen.zig:554`; `rc_helper_worklist`, `:563`) — the one genuinely
  cross-proc-shared structure: a drop/copy helper for a layout is compiled
  once and referenced by every proc that needs it, keyed by helper
  identity. Resolution: hoist RC-helper generation into a pre-pass that
  emits each unique helper as its own named artifact (a synthetic proc
  spec), so procs reference helpers through ordinary `linked_function`
  relocations and the helper set is generated once, uniformly, before or
  alongside the proc pool. (Alternatively each worker emits its own
  helpers and the writer dedups by identity before layout; the pre-pass is
  preferred because it keeps output size and layout stable.)
- *Rodata / static strings* — already built up-front by
  `StaticStringData.build` from the whole store; the writer owns the one
  rodata section and each worker only references entries by name, so no
  change to interning is needed. Worker-generated static-data symbol
  names (`static_data_symbol_names`, `:527`) are returned in the result
  and merged by the writer.
- *DWARF line entries and unwind info* (`line_entries`, `:547`;
  `unwind_functions`, `:557`) — emitted per proc relative to offset 0 and
  rebased by the writer at emission, in the same spec-id order, so debug
  sections are deterministic too.

**Degraded single-threaded path.** On freestanding/wasm builds of the
compiler, `threading.is_freestanding` is true
(`src/compile/threading.zig:11`) and no threads exist; the object-file
compiler is itself compiled out to `void` there
(`src/backend/dev/mod.zig:52`). The pool must run inline on the owner
thread in that configuration, draining its own queue and invoking the same
per-proc generation function — the pattern the coordinator already uses
(`coordinator.zig:2606-2613`). `--jobs=1` selects the same inline path on
native targets and is the byte-identity oracle (below).

**JIT and hot-reload.** The run-image writer (`RunImage.zig`) consumes the
same per-proc-artifact-plus-named-relocation inputs, so it is fed from the
same collected results; only its serializer differs. The hot-reload code
reference protocol (`enable_hot_reload`, `LirCodeGen.zig:654`) and the
`src/machine_code_shim`/`src/ipc` boundary see byte-identical images
because emission order is deterministic — a reload produces the same image
for the same program.

## Implementation slices

Each slice lands and is testable on its own.

1. **Make each proc independently generatable, still single-threaded.**
   Generate each proc into a private offset-0 buffer; express direct
   calls and address-of-proc as `linked_function` relocations instead of
   `PendingCall`/`PendingProcAddr` in-buffer patches; move layout,
   rebasing (`adjustOffset`), and symbol resolution into a writer step
   that concatenates procs in spec-id order. Byte-level requirement: for
   locally-defined targets the serialized writer must resolve each
   `linked_function` reference in place, emitting the same rel32/BL
   displacement `patchCallTarget` (`LirCodeGen.zig:14154`) produces today,
   and must emit no linker relocation entry and no undef symbol for them.
   Today `patchPendingCalls` bakes proc→proc displacements into the buffer
   before the object writer runs, so `getRelocations()` carries no
   proc→proc entries; the emitted object must preserve that. No pool yet.
   Acceptance: object bytes and run-image bytes are byte-identical to
   before for the whole test corpus. This is the load-bearing refactor;
   parallelism is mechanical afterward.
2. **Hoist RC helpers to named artifacts.** Generate the unique RC-helper
   set as synthetic proc specs referenced by relocation, deleting the
   shared `compiled_rc_helpers` offset map from the per-proc path.
   Acceptance is behavioral, not byte-level, because hoisting changes
   helper layout: today helper bodies are drained on demand
   (`maybeDrainRcHelpers` runs during proc compilation, ~`:14657`) and so
   land interleaved after the first proc that references them, whereas
   hoisting lays every helper out up front in helper-identity order.
   Acceptance is therefore behavioral equivalence (all differential suites
   green), relocation-set equivalence, and helper-identity-set equality
   versus slice 1's output. Raw byte-identity stays the *within-slice*
   oracle — output identical across `--jobs` counts and across repeated
   runs at a fixed slice — but it is not an *across-slice* oracle between
   slices 1 and 2.
3. **Introduce the codegen worker pool.** Reuse `channel.zig` and the
   `WorkerAllocators` arena discipline; N workers, one writer with the
   spec-id reorder buffer; wire `--jobs`. Acceptance: `--jobs=1` is
   byte-identical to the slice-1 output; `--jobs=N` is byte-identical to
   `--jobs=1`.
4. **Backpressure and worker-failure handling.** Bound the result channel;
   propagate a worker OOM or codegen error to the owner and fail the build
   cleanly (mirroring `worker_oom`, `coordinator.zig:934`); ensure a
   worker panic aborts deterministically rather than hanging the writer.
5. **Wasm follow-up.** Apply the same pool to `WasmCodeGen`'s
   per-function `pending_bodies`, collected by one module assembler.
   Interpreter is unaffected (it walks LIR directly).

## Risks

- **Nondeterminism leaking into output layout.** Concatenating in
  completion order would make object bytes depend on scheduling. Mitigated
  by the spec-id reorder buffer; guarded by the byte-identity oracle,
  which turns any leak into a test failure.
- **Shared-state races.** A missed shared-mutable accumulator (RC-helper
  interning is the subtle one) would corrupt output under threads.
  Mitigated by making the only shared state `*const` and enumerating every
  mutable structure to a private or writer-owned home; the two-pass
  register-then-generate structure means symbol assignment stays a serial
  pre-pass, and only body generation runs on workers.
- **Memory growth without backpressure.** Whole-proc code buffers held
  in-flight can outgrow available memory on large programs. Mitigated by a
  bounded result channel with a blocking send, capping peak at ~`capacity`
  artifacts.
- **Worker panic or OOM.** A worker that dies must fail the build
  deterministically, not hang the writer waiting for a spec that never
  arrives. Mitigated by propagating a typed failure to the owner (as the
  frontend does via `worker_oom`, `coordinator.zig:934`) and aborting the
  reorder buffer.
- **Debug-section ordering.** DWARF line entries and unwind records must
  be rebased in the same spec-id order as code, or debug info desyncs from
  the bytes. Mitigated by emitting them per proc at offset 0 and rebasing
  at the same emission step.

## What success looks like

Every criterion below must hold; the project is not done until all do:

- **Byte-identical objects across worker counts.** For the full test
  corpus, the object file produced with `--jobs=N` is byte-for-byte equal
  to the one produced with `--jobs=1`, and both equal the pre-project
  sequential output where semantics are unchanged. A build flag forcing a
  single codegen worker exists and is the reference.
- **Run-to-run determinism.** Repeated builds of the same program at the
  same `--jobs` produce identical object bytes and identical run-image
  bytes; there is no ordering dependence on which worker finished first.
- **All differential suites green.** The four-backend eval oracle
  (`src/eval/test/parallel_runner.zig`, `NUM_BACKENDS = 4` at `:172-173`
  — interpreter, dev, wasm, and optionally llvm, all compared by
  `Str.inspect`) and the CLI build-and-run integration tests
  (`src/cli/test/fx_platform_test.zig`, which shells out to `roc build
  --opt=dev` and executes the produced binary) pass unchanged, at the
  default `--jobs` and at `--jobs=1`.
- **No new IR.** There is no representation between `LirProgram.Result`
  and machine code; a diff of the pipeline shows only that per-proc
  generation moved onto workers and a serialized writer performs layout.
- **Cross-proc references are relocations.** The object and JIT paths no
  longer patch proc→proc calls by absolute in-buffer offset;
  `grep -n 'PendingCall\|patchPendingCalls' src/backend/dev/LirCodeGen.zig`
  shows those removed from the object/run-image path (any remaining use is
  documented and proc-internal).
- **Degraded path intact.** The compiler still builds and runs for
  freestanding targets, executing codegen inline with no thread
  dependency.
- **Measured speedup, methodology stated.** Dev-backend codegen wall-clock
  scales with worker count on a multi-proc program. Because this machine
  is too noisy for reliable timing, the speedup is measured on CI
  benchmarks, not locally; the comparison is `--jobs=1` vs `--jobs=N` on a
  large fixture, reported as codegen-phase time.

## How to evaluate the result

### Correctness ideal

Determinism is structural, not incidental: output layout is a function of
spec-id order and per-proc bytes, and inter-proc references resolve
through a name table the writer owns, so worker scheduling cannot affect
the bytes. Data races are impossible by construction because the only
shared state during generation is `*const` (the LIR and layout stores) and
every mutable accumulator is either worker-private or owned by the single
writer — the same property that makes the frontend actor model race-free.
The byte-identity oracle turns any accidental order- or thread-dependence
into a hard test failure rather than a latent nondeterminism.

### Performance ideal

Codegen wall-clock approaches (serial time / worker count) minus the
serialized writer's layout cost, which is linear in total code size and
relocation count and small relative to instruction selection. Peak memory
is bounded by the result channel capacity times artifact size, not by the
proc count. Verify on CI: codegen-phase time at increasing `--jobs` on a
large program, and peak RSS at a fixed `--jobs` with and without the
channel bound to confirm backpressure caps growth.

## Tests to add

- **Byte-identity pin.** A test that builds a representative multi-proc
  program to an object file at `--jobs=1` and `--jobs=8` and asserts the
  two object byte streams are equal; extend to the run-image bytes. This
  is a new guard — there is no golden-object comparison in the suite
  today.
- **Reorder-buffer unit test.** Feed the writer artifacts in a shuffled
  arrival order and assert the emitted layout matches spec-id order.
- **Determinism sweep.** Build the same program 20 times at the default
  `--jobs` and assert identical output each time.
- **Differential suites at both job counts.** Run
  `src/eval/test/parallel_runner.zig` and the `src/cli/test/` build-and-run
  suite with codegen forced to 1 worker and to many, asserting identical
  program results.
- **Degraded-path build.** A compile of the freestanding/wasm compiler
  target to confirm the inline codegen path compiles and runs.

## Scope

The dev backend is in scope first: it is the per-proc-artifact backend and
the dev-build hot path. The **LLVM backend is out of scope** for this
project — it builds one in-memory module with a single module builder and
its bottleneck is different; parallelizing it is a separate effort. The
**wasm backend is a follow-up** with the same shape, since it already
produces per-function bodies (`WasmCodeGen.zig:233, 1564`). The
**interpreter is unaffected** — it walks LIR directly and generates no
code artifacts.

## Related projects

- [runtime-representation-single-sourcing.md](runtime-representation-single-sourcing.md)
  — hardens the per-backend value-representation constants this codegen
  emits; independent, but both touch the dev backend's emission path.
- [host-boundary-single-sourcing.md](host-boundary-single-sourcing.md) —
  the symbol-name and shim-boundary contracts the object writer and run
  image resolve relocations against.
