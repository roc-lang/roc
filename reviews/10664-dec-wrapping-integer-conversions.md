# PR #10664 — Fix Dec's wrapping integer conversions in the LLVM and dev backends

- **Author:** ESRogs (Eric S. Rogstad) · **Draft:** no · **Base:** `main`
- **Size:** +343 / −79 across 9 files
- **Head:** `f57540f`

Three real bugs: the LLVM backend never implemented any `dec_to_*_trunc` (they
fell through a name-matching fallback and were lowered as a plain coercion of
the raw scaled payload — **silently wrong values**); the dev backend divided
into an `i64` and panicked outside `I64` range; and the dev backend's 128-bit
results were rebuilt from that `i64`, so `Dec.to_u128_wrap(-42.5)` returned
2^64 − 42 instead of 2^128 − 42.

## Verdict

**This is the best-diagnosed PR in the open set, and I'd merge it.** The
root-cause analysis is exact, the fix removes the asymmetry that caused the bug
(one generic builtin, as the already-correct checked path had) rather than
patching ten call sites, and replacing the name-matching fallback with a
layout-matching one plus a hard compile error is squarely on the right side of
AGENTS.md's ban on fallbacks and heuristics.

I hand-verified every expected value in the new tests. All 40+ are correct.
Findings below are one testing gap and a set of nits.

---

## Verification I did

**Every arithmetic constant in the new tests checks out.** I computed these
independently rather than trusting them, because a wrong expectation in a
wrapping-semantics test is indistinguishable from a passing test:

| assertion | check |
|---|---|
| `to_i8_wrap(200.0) == -56` | 200 − 256 ✓ |
| `to_u128_wrap(-42.5) == 340282366920938463463374607431768211414` | 2^128 − 42 ✓ |
| `big = 12345678901234567890` → `0xAB54A98CEB1F0AD2` | low byte `0xD2` = 210 (u8) / −46 (i8) ✓; low 16 `0x0AD2` = 2770 both ✓; low 32 `0xEB1F0AD2` = 3944680146 (u32) / −350287150 (i32) ✓; low 64 = the value itself (u64) / −6101065172474983726 (i64) ✓ |
| `-big` at every width | 46/46, −2770/62766, 350287150 both, 6101065172474983726, `to_u128 = 2^128 − big` ✓ |
| `Dec.lowest` | i128min/10^18 truncates to −170141183460469231731; mod 2^64 gives u64 14326257276626284429 / i64 −4120486797083267187 ✓; `to_u128 = 2^128 − 170141183460469231731` ✓ |

That last row is the strongest test in the set — it's the exact value the old
dev-backend code panicked on and the old LLVM code got wrong, at all four wide
widths at once.

**`decToIntWrapBits` is correct.** `magnitude & (shl(1, target_bits) - 1)` with
`target_bits == 128` short-circuited first, so `@intCast(target_bits)` to the
`u7` shift operand can't overflow, and `shl(1, 127) - 1` is the right 127-bit
mask. Boundary `target_bits == 1` gives mask `1`. ✓

**The dev backend's partial-write handling is right.** The builtin writes only
`dst_bits/8` bytes into a 16-byte slot, and the code then loads a full `.w64`
and masks with `shl`/`lsr` — so the uninitialized high bytes of the slot are
provably discarded. The comment says exactly this. ✓ For `dst_bits == 128` it
returns `.stack_i128` and all 16 bytes were written. ✓

**No int→Dec regression from the tightened fallback.** This is the thing that
worried me: `isIntegerLayout` (`MonoLlvmCodeGen.zig:11293`) and `isFloatLayout`
(`:11289`) both exclude `.dec`, so every one of `u8_to_dec` … `i64_to_dec`
would hit the new `return error.UnsupportedLowLevel` — except they're caught by
the `endsWith(name, "_to_dec")` check at `:5112`, which runs first. Fine, but
see nit #3.

**`error.UnsupportedLowLevel` is a real compiler error, not a panic** — it's an
enumerated `CliProblem` (`CliProblem.zig:112`), so a missing lowering fails the
build cleanly. ✓ See nit #4 about what it says.

---

## Findings

### 1. (Should fix) The pure-function unit tests were removed and not replaced

`dec.toIntWrap` is deleted, and at the PR head `src/builtins/dec.zig` contains
no reference to it — which means the `toIntWrap` unit tests that lived beside it
went too. Its replacement, `numeric_conversions.decToIntWrapBits`, has **no unit
test**, and that's out of step with its own file:

- `floatToIntWrapBits` → tested by
  `test "wrapping float to int conversions wrap modulo 2^bits and zero non-finite inputs"`
- `decToIntTryBits` → tested by
  `test "raw float and Dec conversion bits preserve signed integer representation"`
- `decToIntWrapBits` → nothing

The eval low-level tests do cover it end-to-end across all four backends, so
this isn't uncovered behavior — but the two things a direct test would catch
cheaply are exactly the two non-obvious lines: the `target_bits == 128`
short-circuit and the mask arithmetic. Three assertions appended to the existing
`decToIntTryBits` test would close it:

```zig
try std.testing.expectEqual(@as(u128, 42), decToIntWrapBits(42_900_000_000_000_000_000, 8));
try std.testing.expectEqual(@as(u128, 214), decToIntWrapBits(-42_900_000_000_000_000_000, 8));
try std.testing.expectEqual(
    @as(u128, @bitCast(@as(i128, -42))),
    decToIntWrapBits(-42_900_000_000_000_000_000, 128),
);
```

Relatedly: the PR body says "These conversions had no tests anywhere." That's
true of the *backends*, which is the point, but the builtin function itself did
have unit tests — and they're being removed here. Worth stating accurately, so
the removal is a visible decision rather than an incidental one.

### 2. (Nit) `magnitude` is the wrong name

```zig
const magnitude: u128 = @bitCast(whole_part);
```

`whole_part` is a signed `i128` and this is its two's-complement bit pattern —
for a negative Dec it is emphatically not a magnitude. `bits` or `whole_bits`
would match the surrounding naming (`decToIntTryBits` uses the `*Bits` suffix
throughout) and wouldn't invite a reader to assume the sign was already handled.

### 3. (Nit) Two name matches survive right above the new layout match

The PR's stated principle is "the fallback matches on operand layouts, not the
op's name," and the new block delivers that with a genuinely good comment
explaining why `int_to_int`, `int_to_float`, and `float_to_float` are the only
three cases where `coerceScalar`'s single instruction *is* Roc's semantics. But
two name matches remain immediately above it:

- `endsWith(name, "_to_dec")` (`:5112`) — this one is **already** guarded by
  both layouts (`isIntegerLayout(src)` and `target == .dec`), so the name test
  is redundant and could just be deleted, making it pure-layout.
- `endsWith(name, "_try")` (`:5116`) — this one **cannot** be layout-based:
  `i128_to_i8_wrap` and `i128_to_i8_try` have identical layouts and different
  semantics. That's fine, but it's the one remaining place where a
  name-substring decides codegen semantics, and the PR is otherwise about
  eliminating exactly that. A one-line comment saying why the name is
  unavoidable here would keep the next person from "cleaning it up" — or, better
  long-term, a `LowLevel` method (the way #10702 adds `producedTypeFlow()`)
  would make it explicit data rather than a string test.

### 4. (Nit) `error.UnsupportedLowLevel` drops the op name

`emitCrashBytes(name)` at least embedded the operation name in the runtime
crash. The replacement returns a payload-free error, so a developer who adds a
new conversion op and forgets to lower it gets "unsupported low level" with no
indication of *which*. Since this path is unreachable-by-construction for
correct compilers, `Common.invariant` with the op name bufPrinted in (the
codebase's established pattern — note `std.debug.print` is off-limits because it
breaks the wasm playground build) would be strictly more useful than a bare
error, and equally loud.

### 5. (Nit) The builtin takes both `target_bits` and `val_size`

`roc_builtins_dec_to_int_wrap(out, dec_low, dec_high, target_bits, val_size)`
carries the width twice; `val_size` is always `target_bits / 8`. Both call sites
derive them consistently (dev passes `dst_bits` and `dst_bits / 8`; LLVM passes
`bytes * 8` and `bytes`), so nothing is wrong today — but the signature admits a
disagreement that would silently write the wrong number of bytes. This mirrors
the existing `roc_builtins_f64_to_int_wrap` shape, so it's consistency rather
than a new wart; just noting the two-source-of-truth smell now that a second
builtin has adopted it.

### 6. The endianness assertion is a good addition, and it's in the right place

```zig
comptime {
    std.debug.assert(@import("builtin").cpu.arch.endian() == .little);
}
```

The `@bitCast`-to-bytes-then-`@memcpy`-the-prefix pattern in these wrappers was
already little-endian-only; the pre-existing `f32_to_int_wrap` /
`f64_to_int_wrap` just never said so. Making it a compile error rather than a
silent miscompile is the right call, and `dev_wrappers.zig` is the correct home
since that's where the byte-prefix copy happens. Worth one sentence in the PR
body — it's a real (if currently unreachable) portability constraint being
written down for the first time.

### 7. `dec_to_i64_trunc` is now narrower than its name suggests

The dev backend no longer calls it; the wasm backend and one LLVM site
(`MonoLlvmCodeGen.zig:8512`) still do. The PR correctly updates its doc comment
to warn that it panics when the quotient doesn't fit an `i64` — which is now
the *only* remaining place that hazard lives. Given that `dec_to_int_wrap`
supersedes it for every wrapping use, it's worth asking whether the surviving
callers should move over too, so the panicking variant can go away entirely.
Not required for this PR; just don't leave it as a trap for the next person who
reaches for the obvious-sounding name.

## On the CI situation

The PR body is refreshingly honest that "PR CI runs only a narrow LLVM check …
so it cannot exercise most of this change," and that the full suite under LLVM
runs only in the nightly gate. That's the right disclosure, and it's also the
strongest argument for the eval-test coverage being as broad as it is here —
those tests run on every backend on every PR, which is what makes this
reviewable at all. Worth noting for whoever merges: the LLVM half of this fix
will not actually be validated by PR CI, so the nightly after merge is the real
signal.
