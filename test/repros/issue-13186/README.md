# Nested expects in x86_64 test units

Issue: https://github.com/roc-lang/roc/issues/13186

A test unit loads each expect-site id as a 32-bit field relocated against
`roc__expect_site_base`, which each loaded image binds to its first
program-wide site id. On x86_64 the id is loaded with `mov r32, imm32`, which
zero-extends it, and the field is an unsigned `abs32_field` relocation that
ELF (`R_X86_64_32`), Mach-O (`X86_64_RELOC_UNSIGNED`, four bytes) and COFF
(`IMAGE_REL_AMD64_ADDR32`) all encode. A module with an `expect` inside a
function emits such a site even when nothing calls the function, because a
test unit compiles every procedure.

## Regression coverage

The backend tests in `src/backend/dev/LirCodeGen.zig` exercise the real
x86_64 emitter without needing an x86_64 host:

```sh
zig build run-test-zig-module-backend -- --test-filter 'issue 13186'
zig build run-test-zig-module-backend -- --test-filter 'expect sites are zero-extended'
```

- `issue 13186: x64mac inline expect site survives unit image construction`
  builds an x64mac unit-image object from an emitted site and checks its
  relocation's addend and symbol.
- `x86_64 expect sites are zero-extended 32-bit fields in every object format`
  checks the exact instruction bytes (including a register that needs a REX
  prefix and the largest u32 id), the relocations, and that the encoded
  object reads back into the same unit image, for x64mac, x64linux and x64win.

The CLI suite runs `test/cli/Issue13186NestedExpect.roc` under `roc test` as
`issue 13186: nested expects run from test unit images`; on an x86_64 macOS
host it covers the issue's original failure end to end.
