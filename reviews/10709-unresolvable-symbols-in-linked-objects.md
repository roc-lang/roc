# PR #10709 — Stop leaving unresolvable symbols in the objects roc links

- **Author:** rtfeldman · **Draft:** yes · **Base:** `main` · **Head:** `fix-i128-div-and-getauxval-links`
- **Size:** +156 / −38 across 14 files

Two unrelated link failures fixed in one PR:

1. **`__udivti3` / `__divti3`** — the LLVM backend emitted bare `sdiv`/`udiv`/
   `srem`/`urem` on `i128`, which instruction selection turns into compiler-rt
   libcalls that a Roc object doesn't define. Windows LLVM lane failed.
2. **`getauxval`** — `std.heap.pageSize()` on targets without a comptime-known
   page size reaches the Linux auxiliary vector; without libc that's an extern
   whose weak definition Zig only emits for executables, so static archives were
   left referencing it. aarch64 Linux dev-backend lanes failed.

## Verdict

**Both diagnoses are correct and I verified each independently. Fix (1) is the
right shape and I'd merge it as-is. Fix (2) is sound but has a design question
worth answering before merge** (finding #1). Two smaller items below.

Bundling two unrelated root causes in one PR is defensible here — same CI run,
same one-sentence theme — but they have very different blast radii. Fix (1)
touches only LLVM codegen; fix (2) changes the page-size answer for *every*
static archive roc links, on every target. If (2) needs iteration, (1) is stuck
behind it. Consider splitting.

---

## Fix 1: i128 division → decomposed builtins

### Verification

I chased this all the way down and it holds up:

- `compiler_rt_128.zig:317-380` implements `divTrunc_i128`, `divTrunc_u128`,
  `rem_i128`, `rem_u128`, `mod_i128` entirely on top of `udivmod` — a software
  long-division over 64-bit halves. So the builtin does **not** merely relocate
  the compiler-rt reference into the builtins archive; the reference genuinely
  disappears. This is the load-bearing fact and the PR body doesn't state it.
  Worth one sentence.
- `mod_i128` (`compiler_rt_128.zig:375-382`) is `r != 0 and ((r ^ b) < 0) → r + b`,
  which is **exactly** the inline sign-correction sequence the LLVM backend
  emits for the non-128 widths (`MonoLlvmCodeGen.zig:3883-3891`: `rem_is_zero`
  → 0, `sign_differs` → `raw + rhs`). Semantics agree bit for bit, so `i128 %`
  and `i64 %` won't diverge.
- Placement is right. The new block sits *after* the `checked_op` handling
  (`3856-3861`), so `emitCheckedIntegerDenominator` still rewrites `rhs` and
  still emits the div-by-zero and `MIN / -1` crashes before the builtin is
  reached. Putting it earlier would have silently dropped both checks.
- `target_layout == .i128 or .u128` cannot collide with Dec — Dec has its own
  `.dec` layout and its own `emitDecBinary` path — so the two i128 routes stay
  disjoint.
- New test's expected values check out by hand with stdin `3`:
  `3 × 10^20 = 300000000000000000000`; `// 7 = 42857142857142857142`
  (`7 × that = 299999999999999999994`, remainder `6`); negated, `// 7` truncates
  toward zero to `-42857142857142857142`, `% 7 = -6`, `mod_by(7) = -6 + 7 = 1`.
  Matches the `io_spec` string exactly. `to_i128_wrap` is a no-op here since
  `3e20 < i128 max`, so the signed and unsigned lines are expected to agree —
  which is itself a useful cross-check that the signed path isn't sign-confused.
- The test defeats constant folding by deriving the dividend from
  `Stdin.line!()`, and both operands exceed `U64` range so the low half alone
  can't produce the answers. Well constructed.

### This is the established discipline, not a new one

`LowLevelBuiltins` already routes 128-bit shifts (`num_shl_u128`, `num_shr_i128`),
checked 128-bit multiply (`checkedMul128`), and both directions of 128↔float
(`int128ToFloat`, `f64_to_i128_trunc`) through decomposed builtins for the same
reason. Division was the remaining hole. That materially raises my confidence
that this is the intended architecture rather than a patch.

### Finding: `emitDecToIntTruncConversion` loses a documented optimization

`MonoLlvmCodeGen.zig:5209-5228`. The old comment said, verbatim, that dividing
by a constant "keeps the sequence foldable instead of calling into the
builtins." That property is now gone — every runtime Dec→int conversion becomes
an opaque call with two pointer out-params, which is a full optimization barrier
for the surrounding code even at `--opt=speed`.

The divisor here is the compile-time constant `10^18`, and the dividend is a
signed i128. The exact-division-by-constant trick (multiply by a magic
reciprocal, shift) is expressible with the 64-bit primitives already in
`compiler_rt_128.zig` (`mulX` gives a 128×128→128 low product; a `mulhi` would
complete it). Given the repo's stance on not leaving free performance on the
table, it's worth at least *recording* in the comment that the fold was traded
away for linkability and that a magic-reciprocal path could restore it —
otherwise the next reader sees only "we call a builtin" and never learns a
faster shape was possible.

Not a merge blocker: correctness beats a fold, and a broken Windows link beats
both.

### Nit: two spellings of the same lookup

Existing call sites use `builtinSymbol(LowLevelBuiltins.foo(...))`; the new
`emitI128DivRem` uses `builtin_fn.symbolName()`. They're the same thing —
`builtinSymbol` is a one-line wrapper (`MonoLlvmCodeGen.zig:98-100`) — and the
new code *has* to use `symbolName()` because `builtinSymbol` takes a `comptime`
parameter while `unsigned` is runtime. That's a legitimate reason, but a reader
will wonder. One clause in the comment ("`unsigned` is runtime, so the symbol is
looked up dynamically") would settle it. The registry is still the single source
of the spelling either way, so no rule is broken.

### Nit: alloca naming and count

`callI128BinaryBuiltin` still names its out-slots `"dec_low"` / `"dec_high"`
(`MonoLlvmCodeGen.zig:4067-4068`) after being generalized past Dec. Rename to
`"i128_low"`/`"i128_high"`. Also, `allocEntryBlockSlot` is not memoized, so a
function with N 128-bit divisions gets 2N entry-block allocas. SROA will clean
that up under LLVM's pipeline, so this is cosmetic — but it's pre-existing
behavior now applied to a much larger set of programs.

### Improvement worth calling out in the PR body

In the *unchecked* path, `sdiv i128 MIN, -1` was UB (LLVM poison). It's now a
call to `divTrunc_i128`, which wraps to `MIN` — defined behavior. That's a
genuine correctness improvement riding along, and the body doesn't mention it.

### Missing: nothing prevents this class of regression from returning

The fix is one more entry on a list ("don't leave a native 128-bit op in the
module") that is enforced only by a Windows CI lane noticing a link error. A
post-link or post-object check that scans the emitted object's undefined
symbols and fails on anything matching `__[a-z]*ti[0-9]` / `__float*ti*` /
`__fix*ti*` would turn the next occurrence from "a nightly gate goes red on one
platform" into "the build fails everywhere, immediately." Given that four
separate op families (shift, mul, convert, and now divide) have each had to be
routed by hand, the list is clearly still growing. Strongly worth adding.

---

## Fix 2: `queryPageSize` override

### Verification

- `std.Options.queryPageSize: fn () usize = heap.defaultQueryPageSize`
  (`std.zig:144`) — the field exists and the signature matches.
- `defaultQueryPageSize` on Linux without libc is
  `std.os.linux.getauxval(std.elf.AT_PAGESZ)` (`heap.zig:83-85`). Confirmed.
- `pageSize()` short-circuits when `page_size_min == page_size_max`
  (`heap.zig:64-67`), which is why x86-64 Linux (4 KiB both) never queried and
  aarch64 Linux (4 KiB min, **64 KiB max** — `heap.zig:983`) did. The PR's
  explanation of the arch split is exactly right.
- The PR's claim that `page_size_max` "is also what std itself settles on when
  the auxiliary vector is unavailable" is literally true:
  `heap.zig:120` — `if (size == 0) size = page_size_max;`. Nice grounding.

### Finding 1 (the one I'd want answered before merge): the `link_libc` branch makes the answer differ *between archives in the same program*

```zig
fn queryPageSize() usize {
    if (comptime builtin.link_libc) return std.heap.defaultQueryPageSize();
    return std.heap.page_size_max;
}
```

`builtin.link_libc` is a property of **each archive's own compilation**, not of
the final link. A roc-produced program links several of these archives together
— the machine-code shim, the builtins static lib, and the platform host — and
they are not guaranteed to have been compiled with the same `link_libc`. On
aarch64 Linux a libc-linked host would answer **4096** while a freestanding shim
in the same process answers **65536**.

That is fine as long as no `PageAllocator`-derived memory crosses between them,
because `PageAllocator.free`/`remap` round the length with `pageSize()`
(`PageAllocator.zig:193,200`) and would `munmap` a different length than was
mapped. Under the symbol ABI the shim routes allocation through the host's
`roc_alloc`, so I *believe* nothing crosses — but that's an invariant the code
now silently depends on and nothing states.

Two ways out, either fine:

- **Drop the branch.** Always return `page_size_max`. Every archive then agrees
  by construction, and the result is still correct-and-conservative. You lose
  exactness under libc, which only costs address-space rounding (see below).
- **Keep the branch and document the invariant** in the doc comment: that no
  page-granular allocation may cross an archive boundary, and why.

The current comment explains *why the value is safe* but not *why it's safe for
it to differ per archive*, which is the sharper question.

Related: the comment says these archives "are not the program's entry point, so
nothing populates `std.os.linux.elf_aux_maybe`." That's true for the shim, but
the platform test hosts in `test/*/platform/host.zig` — which this PR also
switches over — *are* built as executables in some configurations, where Zig's
start code does populate auxv and `defaultQueryPageSize` would have answered
exactly. So those hosts now over-round where they previously didn't need to.
Harmless, but the comment overstates its scope by covering them with the same
sentence.

### Finding 2 (informational): the cost is address space, not RSS

Worth stating plainly in the PR body so nobody reads "64 KiB minimum
allocation" and panics: `PageAllocator` `mmap`s the rounded-up length, but
untouched pages are never faulted in, so resident memory is essentially
unchanged. The real costs are (a) VA fragmentation and (b) `SmpAllocator` slab
granularity. On a 64-bit target neither is likely to matter. I checked this
specifically because of the "never waste perf" standard — I don't think this
one clears the bar for concern, but the PR body should say so rather than leave
a reader to work it out.

### Nit: the rename is good, and the doc comment is the best part of this PR

`std_options_no_stack_tracing` → `std_options_static_archive` is the right call:
the old name described one of two settings, and would have been wrong again on
the next addition. The expanded doc comment on `shim_io.zig:69-95` explains both
settings' *why* at exactly the altitude `.rules` asks for. The per-host comments
that point at it rather than restating it are also right.

### Nit: `queryPageSize` doesn't need `defaultQueryPageSize`'s cache

Returning a comptime constant in the no-libc branch means no atomic load, which
is strictly better than std's cached path. Not a problem — just noting it's
deliberate-looking and correct.

---

## Testing

- `test/fx/runtime_i128_div_rem_mod.roc` + its `io_spec` entry covers fix (1)
  across whichever backends the fx suite runs. Good.
- **Fix (2) has no test.** It's only covered by "the aarch64 Linux nightly lane
  stops failing," which means a future std upgrade or option change can silently
  reintroduce the `getauxval` reference and nobody learns until the next
  nightly. The undefined-symbol scan suggested above would cover both halves of
  this PR with one mechanism — which is a decent argument for adding it here
  rather than later.
