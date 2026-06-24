# String Pattern Disassembly Notes

Generated on arm64 macOS from the fixtures in this directory.

Commands:

```sh
for name in discard_only one_capture two_capture branchy long_literal width_literals; do
  zig-out/bin/roc build --opt=speed --no-cache --output=.tmp/pattern-disasm/out/$name.a .tmp/pattern-disasm/roc/$name.roc
  llvm-nm .tmp/pattern-disasm/out/$name.a > .tmp/pattern-disasm/out/$name.roc.nm
  llvm-objdump -d --macho --no-show-raw-insn .tmp/pattern-disasm/out/$name.a > .tmp/pattern-disasm/out/$name.roc.s
done

clang++ -O3 -std=c++20 -fno-exceptions -fno-rtti -c .tmp/pattern-disasm/cpp/patterns.cpp -o .tmp/pattern-disasm/out/patterns.o
llvm-nm .tmp/pattern-disasm/out/patterns.o > .tmp/pattern-disasm/out/patterns.cpp.nm
llvm-objdump -d --macho --no-show-raw-insn .tmp/pattern-disasm/out/patterns.o > .tmp/pattern-disasm/out/patterns.cpp.s
```

The C++ fixture uses guarded unaligned loads through `__builtin_memcpy`, so the source has no out-of-bounds or alignment UB. Clang lowers those guarded loads to ordinary AArch64 loads.

## Function Sizes

Instruction counts are approximate object-dump instruction lines for the main matching body. Roc also emits wrappers and ARC cleanup around the app ABI.

| Fixture | Roc core proc | C++ function |
| --- | ---: | ---: |
| `discard_only` | 185 | 208 |
| `one_capture` | 124 | 107 |
| `two_capture` | 183 | 206 |
| `branchy` | 334 | 423 |
| `long_literal` | 124 | 115 |
| `width_literals` | 266 | 313 |

## Width Checks

Roc emits guarded scalar wide loads for literal checks:

- 16-bit literals: `ldrh`, e.g. `"ab"` in `width_literals`.
- 32-bit literals: `ldr w...`, e.g. `"GET "` and small suffix chunks.
- 64-bit literals: `ldr x...`, e.g. `"abcdefgh"`.
- 128-bit literals: `ldp x..., x...`, e.g. `"abcdefghijklmnop"`.

The loads are preceded by length checks (`b.lo`, `b.hs`, `cmp`) so the generated code does not rely on padding past the logical string end.

The handwritten C++ emits the same classes of loads. One difference is that Roc often uses the delimiter byte already proven by the scan and compares the remaining suffix with shifted or overlapping loads. For example, the 16-byte `"qrstuvwxyz012345"` suffix compares after the found `q` with loads from offsets `+1` and `+8`, while the C++ reference reloads the full 16-byte suffix.

## Delimiter Search

Roc now emits a 16-byte-at-a-time SWAR delimiter scan in GPRs:

```asm
ldp   x4, x3, [x3]
eor   ...
adds  ...
bic   ...
adc   ...
and   ..., #0x8080808080808080
orr   ...
cbnz  ...
rbit
clz
```

The updated C++ reference emits the same broad strategy, using two guarded 64-bit loads per 16-byte chunk and the usual zero-byte bit trick. Roc's loop is generally tighter because LLVM formed an `ldp` pair and carries the subtract/borrow path through `adds`/`adc`.

Neither side emits NEON compare instructions for these fixtures on arm64. The current generated code is scalar SWAR, not `ld1`/`cmeq` SIMD.

## Remaining Differences

Roc still has ABI overhead that the bare C++ functions do not:

- Roc `Str` must handle heap and small-string representations before reaching the bytes.
- The app-facing functions include input-string lifetime cleanup. Captures themselves are view slices and do not allocate, but the owning input string still participates in ARC at the function boundary.
- Some fixtures emit duplicate wrapper/specialized procs around the core matcher.

The core matching path is now close to the explicit C++ reference: direct branches, no materialized matcher object, no capture allocation, guarded wide literal checks, and 16-byte delimiter scans. The biggest remaining pure-codegen opportunity visible here is target-specific SIMD delimiter search, but that needs separate per-target cost modeling because the current SWAR loop is already compact on AArch64.
