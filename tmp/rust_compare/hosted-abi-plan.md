# Plan: C-ABI hosted calls and allocation

## One-paragraph summary

Replace Roc's uniform memory ABI for hosted (platform) calls and `RocOps` callbacks —
`fn(roc_ops, ret_ptr, args_ptr) -> void`, everything marshaled through memory — with the
real per-target C ABI, so small arguments and returns travel in registers exactly as a
hand-written C/Rust call would. We vendor and adapt Zig's per-architecture C-ABI
classifiers (MIT, with attribution), thread `roc_ops` only when a hosted function's layouts
actually require Roc memory management, keep `roc_ops` in a callee-saved register on our side
(imposing nothing on the host), and give the memory callbacks malloc-style register
signatures. The end state is verified by building with `ReleaseFast` and diffing the
disassembly of the relevant instruction sequences against the C and Rust gold standards on
every target. Refcount elimination, SpecConstr, and inlining are explicitly out of scope —
they are separate projects layered on top of this one.

---

## Motivation

For a pipeline like `0..14 |> map random_plant |> collect : List Plant`, after the loop is
formed, each `random_plant : I32 => Plant` call costs ~6 instructions today (marshal the
seed into an args buffer, set up `ret_ptr`/`args_ptr`, re-establish `roc_ops`, indirect
call) where the natural C ABI does it in 3 (`mov w0,#seed ; blr ; str x0,[dst]`). The
allocation is similarly inflated by passing a `RocAlloc` struct by pointer and reading an
`answer` field back. Across a realistic function this is the difference between ~105 and ~66
instructions, and — more importantly — it is the difference between matching the C/Rust
machine code per-element and not. Everything that makes the loop itself (unrolling, single
pre-sized allocation, in-place stores) only pays off if the per-call cost is also at parity.

## Philosophy

1. **The C ABI is the contract, and agreement is by construction.** We do not invent a Roc
   calling convention and ask hosts to conform. We emit calls that obey the published
   platform C ABI for each function's real signature; glue emits host prototypes for the
   same signature; the host's own compiler (clang/rustc/zig) independently produces the
   matching decomposition. Two sides implementing the same spec from the same signature
   agree — that is what a stable ABI is for.

2. **Impose nothing on the host.** Hosts are arbitrary C/Rust/Zig. They see only ordinary
   C signatures. We never demand a reserved register, a TLS slot, or a calling-convention
   trick. Any efficiency we want on our side is achieved with mechanisms the ABI already
   guarantees (callee-saved registers), not with new obligations on the host.

3. **`roc_ops` is the memory-management vtable, nothing more.** A hosted function receives
   `roc_ops` only when it must allocate or free Roc-owned memory — a property of its
   argument and return *layouts*. A hosted function's own runtime state (RNG seed, handles,
   arenas) is the host's business and is host-managed, not threaded by Roc.

4. **One source of ABI truth.** Per target there is exactly one classifier. The backend
   call lowering and glue prototype generation both consume it. No ABI rule is written
   twice.

5. **Long-term-correct, no workarounds.** We implement the full per-target C ABI (including
   the hard cases — HFAs, the small-struct register boundary, the SysV eightbyte
   classification, Win64 by-reference rules), not a scalar-only shortcut, because the
   shortcut would not cover real hosted signatures like `Plant`.

## Goals

- Hosted calls and `RocOps` callbacks pass/return small values in registers per the C ABI.
- Per-element cost of a hosted call reaches parity with C/Rust on every supported target.
- No source-level or ABI obligation on the host beyond a standard C signature.
- A single vendored, attributed, per-target classifier shared by backends and glue.

## Non-goals (separate projects, layered on top)

- ARC / refcount elimination (borrow inference). Disassembly will still show `rc`
  increments/decrements around the list; that is expected and compared separately.
- SpecConstr / stream deforestation and Roc-side inlining (what *forms* the loop).
- Link-time symbol resolution for hosted functions (we keep the runtime vtable; calls stay
  indirect `blr`, which is the same instruction count as `bl` once the pointer is hoisted).

---

## Background: the current lowering

- Every Roc proc is `void roc_proc_N(*RocOps, ret_ptr, args_ptr)`
  (`src/backend/llvm/MonoLlvmCodeGen.zig:5`). Arguments are packed into `args_ptr`; results
  are written through `ret_ptr`.
- Hosted calls go through `callHostedFunction`
  (`MonoLlvmCodeGen.zig:601`, `:849`): load the `hosted_fns` table pointer out of `RocOps`,
  index by `dispatch_index`, load the function pointer, and
  `call .ccc void (ptr, ptr, ptr)` with `(roc_ops, ret_ptr, args_ptr)`.
- `RocOps` (`src/builtins/host_abi.zig:100`) holds `env`, the allocator/dbg/expect/crashed
  callbacks, and `hosted_fns`. `roc_alloc` takes `*RocAlloc {alignment, length, answer}` and
  writes the result into `answer`; on OOM it must not return (it crashes), so generated code
  contains no null check.
- The same uniform shape exists in the dev backend (`src/backend/dev/LirCodeGen.zig`) and
  wasm backend (`src/backend/wasm/WasmCodeGen.zig`). Glue generates the host-side
  counterparts (`src/glue/`, per-target under `src/glue/platform/targets/`), and already
  carries each hosted function's full type structure in `HostedFunctionInfo`
  (`arg_fields`, `arg_type_ids`, `ret_fields`, `ret_type_id`, `type_str`).

---

## Components

### A. Vendored per-target C-ABI classifiers

Adapt Zig's classifiers (MIT, "Copyright (c) Zig contributors"), source pinned at
`24fdd5b7a4` (Release 0.16.0):

| our module | from | covers |
|---|---|---|
| `abi/aarch64.zig` | `~/code/zig/src/codegen/aarch64/abi.zig` | arm64 macOS, Linux/musl, **and Windows** (pure AAPCS; correct for fixed prototypes — Windows ARM64 follows AAPCS64 for non-varargs) |
| `abi/x86_64.zig` | `~/code/zig/src/codegen/x86_64/abi.zig` | x64 **SysV** (`classifySystemV`) and **Win64** (`classifyWindows`) — both conventions in one file |
| `abi/wasm.zig` | `~/code/zig/src/codegen/wasm/abi.zig` | wasm C ABI |

Each yields a small `Class` describing how a type is passed/returned:
- aarch64: `{ memory, byval, integer, double_integer, float_array: u8 }` — `memory` ⇒
  sret/by-reference; `integer`/`double_integer` ⇒ 1 or 2 GP registers; `float_array` ⇒ HFA
  in up to 4 SIMD registers; `byval` ⇒ scalar as-is.
- x86_64: the SysV eightbyte `[8]Class` (INTEGER/SSE/MEMORY) plus the Win64
  by-reference-if->8-bytes rule.
- wasm: `{ direct, indirect }`.

The decision trees are the published ABI, not Zig-specific, so they port directly. Each file
gets a header comment: "Adapted from the Zig compiler (MIT), `src/codegen/<arch>/abi.zig` @
24fdd5b7a4," matching how `Builder.zig` is attributed.

### B. Layout-store adapter

Zig's classifiers query `Type`/`Zcu` (`zigTypeTag`, `bitSize`, `structFieldCount`,
`fieldType`, `floatBits`, `containerLayout`, `isPtrLikeOptional`, `isSlice`). We provide a
thin adapter exposing the same questions over **our layout store**:

- tag of a layout (scalar int/float/bool/ptr, struct, union, …),
- bit size and alignment,
- field count + i-th field layout,
- float element width (for HFA counting),
- packed-ness.

This is the only place the classifiers touch Roc internals; the classification logic itself
is unchanged from the adapted source. The adapter is what makes the classifiers a single
source of truth usable from any backend and from glue.

### C. Call/return lowering per backend

For each native code-generating backend (LLVM first, then dev, then wasm), replace the
uniform `(roc_ops, ret_ptr, args_ptr)` emission for hosted calls (and, separately, the
`RocOps` callbacks) with classification-driven lowering:

- Build the lowered signature from the `Class` of each arg and the return:
  `memory`/MEMORY ⇒ `sret` pointer (return) or by-reference (arg); register classes ⇒
  coerced integer/float register types; HFA ⇒ array of floats.
- Marshal each argument from its slot into the chosen registers (or pass a pointer for
  by-reference); after the call, reconstruct the result from the return registers or read
  the `sret` buffer.
- The vtable *storage* stays type-erased (`HostedFn` keeps its opaque-pointer signature);
  cast the loaded pointer to the concrete lowered signature at the call site (free) and emit
  the call.

Zig already implements the full C ABI for every target we need, in both consumption forms,
so we are adapting a complete reference, not inventing one:
- **LLVM backend** consumption mirrors Zig's `src/codegen/llvm.zig` param iterator (sret
  attr, `byval` attr, coerce types) — and that code is written against the same `std.zig`
  LLVM `Builder` we already vendor as `Builder.zig`, so the attribute/coerce calls already
  exist in-tree.
- **Dev backend** does its own codegen-side lowering (no LLVM), so it mirrors Zig's
  *self-hosted* backend: `src/codegen/x86_64/CodeGen.zig:resolveCallingConventionValues`
  (which calls `classifySystemV`/`classifyWindows`, assigns registers/stack, handles sret)
  and the aarch64 equivalent in `src/codegen/aarch64/Select.zig`. Our existing
  `src/backend/dev/CallingConvention.zig` is brought up to full coverage against that
  reference (it currently handles scalars, return-by-pointer, Windows shadow space, and
  large-struct-by-pointer, but not HFAs or the full by-value aggregate classification).
- **wasm backend** mirrors `wasm/abi.zig` plus its wasm codegen consumption.

This is the full C ABI — HFAs, the SysV eightbyte classification, and all the Win64
by-value-struct/position-shared-register rules included. The uniform pointer ABI that exists
today was a deliberate Roc choice to avoid writing this (`MonoLlvmCodeGen.zig:3` and the
decomposed builtin wrappers in `LirCodeGen.zig:330` say so explicitly); it is not a Zig
limitation, and we are replacing it with the real thing.

### D. `roc_ops` as the memory vtable only (decoupling)

- A hosted function is passed `roc_ops` **iff** any of its argument or return layouts
  involves a heap-allocated Roc type (`List`, `Str`, `Box`, recursive union) — i.e. iff it
  could call `roc_alloc`/`roc_dealloc`/`roc_realloc`.
- `RocOps.env` remains, but solely as the allocator's own context (consumed by the memory
  callbacks). It is no longer handed to hosted functions for their own logic.
- Platforms that today stash a hosted function's own state in `env` move it to host-managed
  storage (global/TLS). One-time platform migration.

### E. The `needsRocOps` predicate (shared)

A pure function `needsRocOps(signature_layouts, target) -> bool`, returning true iff any
arg/return layout is heap-allocated. Consumed identically by (1) backend call lowering, to
decide whether to thread `roc_ops`, and (2) glue, to decide whether to emit the parameter.
Because Roc generates both, they cannot disagree.

### F. Callee-saved `roc_ops` threading (no host imposition)

When `roc_ops` is threaded:
- Hold it in a **callee-saved** register across the call sequence. The C ABI guarantees
  callees preserve it, so it survives every hosted call with no spill/reload — we know
  statically it is still there.
- Pass it as the **leading** C argument; emit one register-to-register `mov` per call to
  place it in the argument register. (Move elimination makes this ~zero execution cost; it
  is +1 code byte.)

**Convention: `roc_ops` is always the first parameter, for every Roc→host call** (hosted
functions and all `RocOps` callbacks alike). Leading is chosen over trailing because:
it matches the existing Roc proc ABI (`roc_ops` is already arg 0 everywhere); a leading
pointer is *guaranteed* to occupy the first GP argument register and is never spilled to the
stack, so the per-call setup is always exactly one `mov` regardless of the function's arity
(a trailing `roc_ops` on a high-arity function could be stack-passed); it is varargs-safe;
and its index stays stable (0) as signatures gain arguments. The only cost is departing from
the C "userdata-last" callback idiom, which is purely aesthetic.
- For hosted functions that don't need `roc_ops`, none of this exists — the call is the bare
  C call.

### G. Register-based memory callbacks

Convert **all** `RocOps` callbacks to leading-`roc_ops`, register signatures together, so
there is one uniform Roc→host convention:
- `roc_alloc(roc_ops, size, align) -> ptr`, `roc_realloc(roc_ops, ptr, new_size, align) ->
  ptr`, `roc_dealloc(roc_ops, ptr, align)` — malloc-style; these use only pointer/`usize`
  types, so their classification is the trivial register case on every target. OOM still
  crashes inside `roc_alloc` (no generated null check).
- `roc_dbg`/`roc_expect_failed`/`roc_crashed` — moved to the same leading-`roc_ops` shape,
  passing their small message struct by pointer (or by register class where it fits). Done in
  the same phase rather than left on the old convention, to avoid two conventions coexisting.

### H. Glue generation

Glue emits, per target and per hosted function:
- the natural C prototype derived from the function's layouts via the same classifier, and
- a leading `roc_ops` parameter iff `needsRocOps` is true.

The host implements the body of the generated declaration and compiles against the generated
header, so any signature divergence is a host-compiler error. Glue is already per-target and
already has the type info; this is a change in the shape it renders, not new information.

---

## The coordination contract

1. **One decider.** The call site and the host prototype are both produced by Roc, from the
   same `needsRocOps` predicate and the same classifier. They cannot disagree.
2. **Agreement by construction.** Both sides implement the same platform C ABI from the same
   signature; independent compilers produce matching decompositions.
3. **Compile-time enforcement.** The host builds against the generated header; a mismatch
   fails to compile on the host side.
4. **Conservative default.** Default to threading `roc_ops`; omit only on proof that all
   layouts are heap-free. Wrong-conservative is harmless (an ignored argument + one
   zero-latency `mov`); omission is never speculative, so never UB.

---

## Invariants we want to hold

1. **Backend agreement.** Interpreter, dev, and LLVM (and wasm) produce identical observable
   results before and after this change. (Existing project invariant.)
2. **Single ABI source.** For a given target, exactly one classifier module decides passing;
   backend lowering and glue both call it. No duplicated ABI logic anywhere.
3. **Classifier conformance.** For every supported target, Roc's classification of a type
   matches what the platform C compiler does for the equivalent C type (differential test
   against clang).
4. **Signature symmetry.** The lowered signature emitted at a call site is identical to the
   prototype glue emits for the same hosted function on the same target.
5. **`roc_ops` discipline.** `roc_ops` is threaded to a hosted function iff `needsRocOps` is
   true; when threaded across calls it lives in a callee-saved register with no spill/reload;
   the host sees it only as a normal pointer parameter.
6. **No host imposition.** Generated host signatures are standard C; no reserved register,
   TLS, or custom convention is required of the host.
7. **OOM locality.** Allocation-failure handling lives inside `roc_alloc`; generated code
   contains no allocator null check or cold path.
8. **No marshaling for register-class values.** For an all-register hosted signature, the
   generated call contains no args-buffer `alloca` and no memory store/load of arguments —
   they go straight to/from registers.

---

## Verification

The acceptance bar is: on every supported target, the *relevant instruction sequences* of a
`ReleaseFast` Roc build match the C and Rust gold standards (`tmp/rust_compare/plants.c`,
`plants_roc.rs`), modulo the separately-tracked ARC ops.

1. **Differential classifier test (invariant 3).** For a corpus of types (scalars; small
   all-integer structs like `Plant`; 16-byte structs; >16-byte structs; HFAs of 1–4 floats;
   mixed int/float; nested) emit the equivalent C type, compile with clang for each target,
   and assert Roc's `Class` decomposition matches clang's (register vs memory, which
   registers, sret vs not). This is the conformance gate and it runs in CI.

2. **Call-shape regression test (invariant 8).** Like the existing
   `list builtins inline` custom CLI test: build a small app with an all-scalar hosted
   function under `--opt=speed`, read the generated object/IR, and assert the hosted call
   site uses register passing — no args-buffer `alloca`, no per-argument memory store before
   the call, signature matches glue.

3. **End-to-end disassembly comparison (the goal).** Per target
   (arm64-macos, arm64-linux-musl, arm64-windows, x86_64-linux-musl, x86_64-windows, wasm):
   - regenerate the C/Rust gold for that target (`clang -O3` / `rustc -C opt-level=3`,
     matching triple),
   - build the Roc `starting_plants` equivalent with `ReleaseFast` / `--opt=speed`,
   - disassemble each and diff the **allocation site** and the **hosted-call site**
     instruction-for-instruction.
   - Expected outcome: the hosted-call site matches C/Rust per element (one `mov` for
     `roc_ops` only when `needsRocOps`; bare call otherwise); the allocation matches a
     register `malloc`-style call; the remaining differences are exactly the ARC ops and the
     bit-shifted-capacity header `mov`, both documented and out of scope here.
   - Capture the per-target diffs in `tmp/rust_compare/` (extend `example.md`) as the record
     of done.

4. **Result-equivalence test (invariant 1).** Run the existing eval suite (interp/dev/llvm
   agreement) plus a hosted-call execution test against a real host on each target, asserting
   identical results pre/post change.

---

## Phasing / sequencing

Each phase is independently landable and verifiable.

1. **Classifiers + adapter (A, B).** Vendor and adapt `aarch64.zig`, `x86_64.zig`,
   `wasm.zig` onto the layout store. Land with the differential classifier test (verification
   #1) — no codegen wired up yet. This is the foundation and the riskiest correctness piece,
   so it gets its conformance gate first.
2. **Register memory callbacks (G).** Convert `roc_alloc`/`dealloc`/`realloc` to register
   signatures in the LLVM backend + glue + the default platform host. Fully testable in
   isolation (known signatures), and delivers the allocation win on its own.
3. **`needsRocOps` + decoupling (D, E).** Introduce the predicate; stop threading host state
   through hosted calls; keep `env` for the allocator only. Migrate the in-repo platform(s).
4. **Hosted-call C-ABI lowering — LLVM (C, F).** The main event for the loop: classification
   -driven hosted call lowering + callee-saved `roc_ops` + per-call `mov`. Land with the
   call-shape regression test (verification #2) and the arm64-macos disassembly diff
   (verification #3).
5. **Glue prototypes (H).** Emit natural per-target prototypes using the predicate +
   classifier; wire the generated-header agreement check.
6. **Dev and wasm backends (C).** Bring the dev backend and wasm backend to the same
   lowering; extend the disassembly diffs to all targets.
7. **Full verification sweep.** Run verification #1–#4 across every target; record per-target
   diffs.

---

## Risks and mitigations

- **C ABI corner cases** (HFA boundaries, small-struct register/memory line, SysV eightbyte
  merges, Win64 by-reference). Mitigation: we are adapting a battle-tested implementation,
  and the differential test against clang (verification #1) is the gate — if clang and Roc
  decompose the same signature identically on every target, we are conformant by observation.
- **Roc types with no clean C mapping** (closures, recursive unions, seamless slices).
  Represent as their `repr(C)` layout and pass per the normal aggregate rules (registers if
  small, by reference if large). Backend and glue read the same layout, so they agree.
- **arm64-windows divergences.** Limited to varargs (not used by hosted callbacks); AAPCS
  covers our fixed prototypes. Mitigation: include arm64-windows in the differential test to
  confirm, and only add a Windows-on-arm64 branch if a case actually diverges.
- **Platform migration of `env` usage.** A breaking change for platforms that stored hosted
  function state in `env`. Mitigation: document the move to host-managed storage; the
  allocator's use of `env` is unaffected.
- **Backend skew.** Three native backends must adopt the same lowering. Mitigation: the
  classifier + `needsRocOps` are shared modules; only the marshaling differs per backend, and
  result-equivalence (verification #4) catches divergence.
