# Design: Register-based hosted-call ABI

## Goal

Make Roc's machine code for hosted (platform) calls and allocation match what a
hand-written C/Rust version compiles to, instruction-for-instruction in the hot path.

Today a hosted call uses a *uniform memory ABI* — `fn(roc_ops, ret_ptr, args_ptr) -> void`,
everything passed through memory, regardless of the actual types. For a function like
`random_plant : I32 => Plant` (where `Plant = { x: I32, type: U32 }`, a flat 8-byte value),
that's ~6 instructions per call: marshal the seed into an args buffer, set up `ret_ptr` and
`args_ptr`, re-establish `roc_ops`, and `blr`. The natural C ABI does it in 3:
`mov w0,#seed ; blr ; str x0,[dst]`.

The target: after inlining + SpecConstr + borrow inference turn a pipeline like
`0..14 |> map random_plant |> collect` into a counted loop over a pre-sized list, this ABI
work makes each iteration cost the same 3 instructions a C `for` loop would. For the
`starting_plants` benchmark that's ~66 total instructions vs ~105 today, and the per-element
cost reaches exact parity with C/Rust.

**Non-goals:** This does *not* change the platform/host contract from a runtime vtable to
link-time symbols (calls stay indirect `blr` — same instruction count as `bl` once the
pointer is hoisted). It does not depend on inlining/SpecConstr/borrow-inference; it is
orthogonal and complementary — those form the loop, this determines the per-call cost.

---

## Background: why the uniform ABI exists

`RocOps` (`src/builtins/host_abi.zig`) bundles the allocator/effect callbacks plus an
opaque `env` pointer, and a `hosted_fns` vtable of type-erased function pointers. Every
hosted function has the same shape `(roc_ops, ret_ptr, args_ptr) -> void`, so:

- the host side is trivial to generate — one signature shape for everything;
- Roc never has to compute per-type register/stack decomposition;
- the vtable is homogeneous (all entries the same pointer type).

The cost is that *everything* goes through memory and `roc_ops` is threaded on every call,
even when the types would fit in registers and the callee never touches Roc's heap.

---

## The design

Five components. (1) and (4) are the real backend work; (2) and (5) are mostly a change in
*what shape we render*; (3) is a small codegen rule.

### 1. Per-signature C ABI for hosted calls

Roc's backend computes the platform C ABI decomposition for each hosted function's *actual*
signature (args + return), per target. This is the same job every C compiler has — clang
calls it `ABIInfo`. LLVM does **not** do it for you: LLVM IR is ABI-agnostic, so the
frontend must classify each type and emit the right `byval` / `sret` / coerce-to-registers.

- Small scalars and small structs go in registers; large aggregates pass by reference /
  `sret`, exactly per the platform spec (e.g. aarch64: ≤16 bytes → up to two registers,
  else caller-allocated `sret`).
- Per-target: aarch64 AAPCS, x86-64 SysV, wasm — each has its own classification rules,
  selected by the active target.
- Roc already has a slice of this for `builtins.bc` interop (sret pointers, splitting
  `i128` into `i64` pairs). Generalize it into a real per-target `ABIInfo`.

The vtable *storage* stays type-erased — `HostedFn` keeps its opaque-pointer signature to
avoid the type-dependency loop. At each call site Roc casts the erased pointer to the
concrete signature (a free, compile-time cast) and emits the C-ABI `blr`. So only the *call
lowering* changes; the vtable representation does not.

### 2. The host's own state is host-managed (not carried by `roc_ops`)

A hosted function needs `roc_ops` for exactly one reason: to manage *Roc-owned memory* —
allocate to build a heap value it returns (`Str`, `List`, `Box`, recursive union), or
dealloc/realloc an owned heap argument it consumes. That is fully determined by the
function's argument and return **layouts**, which the compiler already knows.

A hosted function's *own* runtime state — an RNG seed, file handles, an arena — is the
host's business. The host keeps it in its own global/thread-local, not on a pointer Roc
threads through. `random_plant` reads its RNG state from host-managed storage; it does not
receive a context parameter for that.

Consequence: **a hosted function is passed `roc_ops` iff its arg/return layouts require Roc
memory management.** Flat-in/flat-out functions like `random_plant : I32 => Plant` receive
nothing extra and compile to the bare C call. (`RocOps.env` still exists — it is the
allocator's own context, used by `roc_alloc`/`roc_dealloc` — it just stops being a
general-purpose context handed to hosted functions for their own logic.)

### 3. Threading `roc_ops` when it *is* needed

When a hosted function does manage Roc memory, Roc passes `roc_ops` as an ordinary C
argument (first parameter, by convention). Codegen rule:

- Keep `roc_ops` in a **callee-saved** register across the call sequence. The C ABI
  guarantees every conforming callee preserves callee-saved registers, so `roc_ops` stays
  live across all hosted calls with **zero** spills/reloads — Roc knows statically it is
  still there.
- Emit one `mov` per call to copy it from its callee-saved home into the argument register
  the ABI expects. This `mov` is unavoidable within a strict C ABI (argument registers are
  caller-saved, clobbered by every call) — but it is a register-to-register move, which
  modern cores eliminate at rename (zero execution latency; +1 code byte only).

No imposition on the host: `roc_ops` is just a normal pointer parameter. If the host wants
to stash it somewhere for its own use, that is the host's choice.

### 4. Register-based `roc_alloc`

Give the allocator callback a malloc-like register signature — `fn(roc_ops, size, align) ->
ptr` — instead of passing a `RocAlloc { alignment, length, answer }` struct by pointer.

- `roc_alloc` is called once per allocation (here, once total — hoisted out of the loop), so
  its setup cost does not scale with the data.
- It still receives `roc_ops` (its arena context lives in `RocOps.env`).
- It crashes internally on OOM (per the existing contract: "must not return null"), so the
  generated code carries **no** null check and **no** cold path — that handling lives once
  inside `roc_alloc`, shared across every allocation site, instead of being inlined at each.

### 5. Glue generation

`roc glue` already emits per-target host code (`src/glue/platform/targets/{arm64mac,
arm64musl,x64musl}`) and already carries each hosted function's full type structure in
`HostedFunctionInfo` (`arg_fields`, `arg_type_ids`, `ret_fields`, `ret_type_id`, `type_str`).

Glue derives the host prototype from the **same** inputs and the **same** rule the backend
uses for the call site:

- the natural C signature for the function's types (Component 1),
- a leading `roc_ops` parameter iff the layout rule says it manages Roc memory (Component 2).

The host author implements the body of whatever glue generated, and the host translation
unit `#include`s / imports the generated declaration — so any divergence between "what Roc
passes" and "what the host expects" is a **compile-time error in the host's own compiler**,
not a silent ABI mismatch.

---

## The coordination contract

This is the crux of why it is safe without any runtime negotiation:

1. **One decider.** Roc generates *both* the call sites and the glue (it is the same tool).
   The predicate "does this hosted function take `roc_ops`?" is a pure function of the
   hosted signature's layouts, run in one place. The two sides cannot disagree because they
   run the same function on the same input.

2. **Agreement by construction.** Both sides implement the same published platform C ABI
   from the same signature. Independent compilers (Roc's backend, and clang/rustc/zig on the
   host) compute the same register/stack decomposition — that is exactly what a stable ABI
   guarantees.

3. **Compile-time enforcement.** The host builds against the glue-generated prototype, so a
   mismatch fails to compile on the host side.

4. **Conservative default.** Default to passing `roc_ops`; omit it only when the layout rule
   *proves* it is unneeded (all arg/return layouts are flat). Being wrong in the
   conservative direction is harmless — an ignored argument plus one zero-latency `mov`.
   Omitting is only ever done on proof, so it can never produce UB.

---

## Worked example: `starting_plants`

`0..14 |> map random_plant |> collect : List Plant`, after inlining + SpecConstr + borrow
inference, with this ABI. `random_plant : I32 => Plant` is flat-in/flat-out, so by the rule
it takes **no** `roc_ops` — the loop is the bare C call.

```
; prologue (5)
sub  sp, sp, #0x20
stp  x20, x19, [sp, #0x10]      ; x19 = sret ptr, x20 = data ptr
stp  x23, x22, [sp, #...]       ; x23 = random_plant fn-ptr (held across the loop)
stp  x29, x30, [sp, #...]
add  x29, sp, #...
; setup (2)
mov  x19, x8                    ; sret ptr  (x0 = roc_ops on entry)
ldr  x23, [x0, #rp_off]         ; load random_plant fn-ptr from the vtable
; allocation (5) — no null check; roc_alloc crashes internally on OOM
ldr  x9,  [x0, #alloc_off]
mov  w1,  #0x78                 ; size = 120
mov  w2,  #0x8                  ; align
blr  x9
mov  x20, x0                    ; data ptr
; loop (45) = 15 × 3, fully unrolled, seeds constant-folded
mov  w0, #seed ; blr x23 ; str x0, [x20, #off]
; header (4) — RocList { ptr, len, cap }, cap = len << 1
str  x20, [x19]                 ; ptr@0
mov  w8,  #0xf                  ; len = 15
mov  w9,  #0x1e                 ; cap = 30
stp  x8,  x9, [x19, #0x8]       ; len@8, cap@16
; epilogue (5)
ldp  x29, x30, [sp, #...]
ldp  x23, x22, [sp, #...]
ldp  x20, x19, [sp, #0x10]
add  sp,  sp, #...
ret
```

**66 instructions.** The 15-iteration loop is identical to the C/Rust gold's per-element
cost. The residual vs a fair C baseline (allocator-handled OOM, ~60) is ~+6 of fixed,
non-scaling overhead — two vtable fn-ptr loads, the `align` argument, one extra
callee-saved pair, and the `+1` for the bit-shifted capacity in the header. Only that last
one is intrinsic to Roc's representation; the rest is the runtime-vtable indirection we are
deliberately keeping. None of it scales with list length.

---

## Implementation phases

1. **Per-target `ABIInfo` in the backend** — the C ABI classification for emitting hosted
   calls/returns in registers. The largest and highest-value piece; generalizes the existing
   `builtins.bc` interop machinery.
2. **Layout-driven `needsRocOps` predicate** — shared by the backend (call-site lowering)
   and glue (prototype generation). True iff any arg/return layout involves a heap Roc type.
3. **Stop threading the host's own state through hosted calls** — pass `roc_ops` only per
   the predicate; keep `RocOps.env` solely as the allocator's context for the memory
   callbacks.
4. **Register-based `roc_alloc`/`roc_dealloc`/`roc_realloc`** — malloc-style signatures.
5. **Glue emits natural prototypes** — using the predicate and the per-target ABI, with the
   generated header as the agreement check.

`RocOps` stays (storage type-erased); platform host code is regenerated by glue.

---

## Risks and edge cases

- **C ABI corner cases.** Homogeneous float aggregates, the small-struct register/memory
  boundary, alignment, per-target differences. Mitigation: implement the published spec and
  differentially test the emitted calls against clang's output for the same signatures.
- **Roc types with no clean C mapping** (tag unions, closures, recursive types). Represent
  as `repr(C)` structs (discriminant + union) passed by the C ABI's normal aggregate rules
  (in registers if small, by reference if large). Backend and glue must agree on the layout
  — which they already must for any interop today.
- **Migration of `env` usage.** Platforms that currently stash per-call state in
  `RocOps.env` for a hosted function's own logic must move it to host-managed storage
  (global/TLS). One-time platform change; the allocator's use of `env` is unaffected.
- **Thread-safety of host-managed state.** The host owns this. Hosted calls are synchronous
  on Roc's calling thread, so per-thread storage is sound; cross-thread sharing is the
  host's responsibility as it already is.
- **wasm.** Different ABI rules, handled by the per-target `ABIInfo`; host-managed state via
  module globals works there too.
