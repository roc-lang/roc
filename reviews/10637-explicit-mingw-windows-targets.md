# PR #10637 — Add explicit MinGW Windows targets

- **Author:** lukewilliamboswell · **Draft:** no · **Base:** `main`
- **Size:** +322 / −54 across 19 files
- **Closes:** #8779

Splits Windows into two target families: `x64win`/`arm64win` (+`v1`) stay MSVC,
and new `x64mingw`/`arm64mingw` (+`v1`) select the GNU Windows ABI. The choice
is carried through the target query, LLVM triple, embedded builtin objects,
platform-input directory, and the final link.

## Verdict

**Architecturally this is exactly right**, and the design.md paragraph states the
invariant that matters:

> It never classifies a COFF input as MSVC or MinGW from its container format,
> symbols, or linker failures.

That's the AGENTS.md rule ("consume explicit data produced by earlier stages
rather than trying to recover, guess, reconstruct") applied to a place where
guessing is genuinely tempting, and the implementation follows through — the
MSVC and MinGW link paths are fully disjoint with `InvalidArguments` on every
crossed wire.

**Three things to resolve:** an undisclosed behavior change to the *existing*
`x64win` target (#1), an optional-unwrap that panics on a reachable input (#2),
and a native-ABI fallback that infers the app's runtime from the compiler's own
build (#3).

---

## Findings

### 1. (Should disclose) `x64win`/`arm64win` change behavior — this is the actual bug fix, and the PR body hides it

The PR body says it will "keep `x64win`, `x64v1win`, `arm64win`, and
`arm64v1win` as MSVC targets," which reads as *no change to existing targets*.
The diff says otherwise:

```diff
-        .{ .name = "x64win",   .query = .{ .cpu_arch = .x86_64,  .os_tag = .windows, .abi = .gnu } },
-        .{ .name = "arm64win", .query = .{ .cpu_arch = .aarch64, .os_tag = .windows, .abi = .gnu } },
+        .{ .name = "x64win",   .query = .{ .cpu_arch = .x86_64,  .os_tag = .windows, .abi = .msvc } },
+        .{ .name = "arm64win", .query = .{ .cpu_arch = .aarch64, .os_tag = .windows, .abi = .msvc } },
```

That's the `addMainExe` embedded-builtins list. Before this PR, `x64win`
embedded **GNU-built** builtin objects while `linker.zig` unconditionally linked
them against the **MSVC** runtime (`/defaultlib:msvcrt`, Windows SDK libpaths) —
and `windows_cross_targets` at the top of the same file already said `.msvc`, so
the two lists disagreed with each other.

That mismatch is almost certainly the real content of #8779, and fixing it is
the most valuable part of this PR. But it means **every existing `x64win` /
`arm64win` build now gets different builtin objects than it did before.** Anyone
whose Windows platform links today is depending on the accidental
GNU-builtins-into-MSVC-link combination, and this will change under them —
hopefully to "correct," possibly to "differently broken."

Put it in the PR body, and ideally in the root-cause section: "the two target
lists disagreed; `x64win` embedded GNU objects into an MSVC link" is a much
better one-line summary of the bug than "Roc represented Windows as one target
family."

### 2. (Should fix) `windowsAbiFromStd(abi).?` panics on a reachable input

`windowsAbiFromStd` deliberately returns `?WindowsAbi` so an unsupported ABI is
representable — and then three call sites in `fromOsAndArch` immediately throw
that away:

```zig
.windows => return switch (windowsAbiFromStd(abi).?) {
    .msvc => .x64win,
    .mingw => .x64mingw,
},
```

`fromOsAndArch(.windows, .x86_64, .none)` panics. `.none` is not exotic — it's
what an unspecified `std.Target.Query.abi` and several of Roc's own non-Windows
target queries use, and the third call site is the *unknown-arch fallback*
branch, which is exactly where a partially-specified query lands.

Either:

- return an error from `fromOsAndArch` for an unsupported Windows ABI, or
- keep the unwrap but replace `.?` with an explicit invariant carrying the ABI
  name, so the failure says what happened rather than "index out of bounds" /
  "unreachable."

The optional is doing real work at the `linker.zig` call site
(`orelse return LinkError.InvalidArguments`); the `fromOsAndArch` sites should
match that discipline rather than assert it away.

**Credit where due:** the switch in `windowsAbiFromStd` enumerates every
`std.Target.Abi` member explicitly rather than using `else => null`. That means a
new Zig ABI is a compile error here instead of silently becoming "unsupported."
That's the right call and worth keeping as Zig evolves.

### 3. (Question) The native fallback derives the app's runtime ABI from the compiler's own build ABI

```zig
const target_abi = config.target_abi orelse blk: {
    if (builtin.target.os.tag != .windows) return LinkError.InvalidArguments;
    break :blk switch (roc_target.windowsAbiFromStd(builtin.target.abi) orelse ...) {
        .msvc => TargetAbi.msvc,
        .mingw => TargetAbi.mingw,
    };
};
```

`builtin.target.abi` is the ABI **`roc` itself was compiled with**. So a
MinGW-built `roc` running on Windows defaults to MinGW-linking the user's app,
and an MSVC-built `roc` defaults to MSVC — for the same source, same machine,
same command. That's the compiler's build configuration leaking into the
artifact it produces.

If every caller sets `config.target_abi` from the `RocTarget` (which is what the
whole PR is about), this branch is unreachable and should be an invariant rather
than a silent inference. If it *is* reachable, it's the one place the ABI is
guessed rather than declared, which sits awkwardly next to the design.md
sentence about never classifying ABI implicitly. Worth stating which.

### 4. (Nit) The generated objects are provably identical across the ABI split

The snapshot updates show it plainly:

```
 x64win=d286ad7c93561a310b64656d24d94ef6947b69f04b0fa9d5d9f9821561861022
+x64mingw=d286ad7c93561a310b64656d24d94ef6947b69f04b0fa9d5d9f9821561861022
 arm64win=00b03a5f9c21f616abad611ed8755c45f7338556759adbfdb38c8ede7b0c61cb
+arm64mingw=00b03a5f9c21f616abad611ed8755c45f7338556759adbfdb38c8ede7b0c61cb
```

Byte-identical, for all ten `dev_object_*` snapshots. That's the *right* answer —
Roc's own emitted COFF doesn't depend on the C runtime — but it means the PR body's
"target-specific embedded objects" is true only of the Zig-compiled builtin
archives (`src/cli/targets/*/roc_builtins.obj`), not of anything Roc generates.

Two consequences worth acknowledging in the body:

- **Binary size.** `roc` `@embedFile`s a full set of builtin + extern +
  default-platform objects per target. Four new targets means four more sets
  compiled into every `roc` binary, for a feature most users won't use. If the
  MinGW builtins turn out to be identical to the MSVC ones too, that's pure
  duplication and worth deduplicating; if they differ, say so, because it's the
  justification for the four new directories.
- **Build time.** `windows_cross_targets` grows from 2 to 4, so every full build
  cross-compiles two more Windows builtin sets.

### 5. (Nit) `/alternatename:__image_base__=__ImageBase` deserves its "why"

```zig
try args.append("-lldmingw");
try args.append("/nodefaultlib");
try args.append("/alternatename:__image_base__=__ImageBase");
```

The comment above covers `-lldmingw` and `/nodefaultlib` well. The
`alternatename` is the one that will baffle the next reader: it exists because
GNU-toolchain CRT startup code references `__image_base__` while the COFF linker
defines `__ImageBase`. One clause saying that turns a magic string into a fact.

### 6. Things I checked and found correct

- **No silent crossover between link paths.** The ELF branch rejects
  `.msvc`/`.mingw` and the COFF branch rejects `.musl`/`.gnu`/`.freestanding`,
  both with `InvalidArguments`. Adding a `TargetAbi` member will be a compile
  error in both switches. ✓
- **`TargetAbi.fromRocTarget` checks `windowsAbi()` first**, before the
  `isStatic()` musl/gnu split, so a Windows target can never fall through to
  `.gnu`. ✓
- **`/defaultlib:` suppression is complete** for MinGW — including the
  Tracy-conditional `/defaultlib:msvcprt`, which is now gated on
  `target_abi == .msvc`. Easy one to miss; it wasn't. ✓
- **The negative test is the right test.** `"MinGW linking uses explicit platform
  runtime inputs without MSVC defaults"` loops over every emitted argument and
  asserts no `/libpath:` and no `/defaultlib:` appears at all. That pins the
  invariant the design paragraph states, rather than just checking the flags
  that *were* added. ✓
- **The `v1` variants are fully wired**: `defaultTarget`/`v1Target` mappings both
  directions, `family`, `toOsTag`, `toCpuArch`, `toTriple`, `dynamicLinker`'s
  `error.WindowsTarget`, both `BuiltinsObjects` switches, and
  `DefaultPlatformObjects`. The existing
  `test "every v1 target shares its default target's platform"` gains a
  `windowsAbi()` assertion, so a future target that forgets the mapping fails
  there. ✓
- **Triples are correct**: `x86_64-w64-windows-gnu` / `aarch64-w64-windows-gnu`
  is the conventional MinGW-w64 spelling, and the new
  `test "Windows targets preserve their C runtime ABI in their query and triple"`
  pins all eight targets against ABI, Zig ABI, and triple simultaneously. ✓
- **User-facing target list updated** — `targets_validator.zig` now
  distinguishes "Windows (MSVC)" from "Windows (MinGW)" in the error message, so
  a user who types a bad `--target` learns both exist. Easy to forget; it wasn't. ✓

## Validation note

The PR body's validation is unusually concrete for a cross-compilation change —
actually generating Go `c-archive` hosts for `windows/amd64` and `windows/arm64`
and linking PE32+ executables through Roc at both `--opt` levels is the
end-to-end evidence this needs, and it's the thing CI cannot do. Worth keeping
that paragraph in the merge commit.

What's *not* covered by any automated test is the MSVC path's behavior change
from finding #1 — the linker unit tests exercise argument construction, not the
resulting link. If there's any Windows CI lane that links an MSVC-target
executable, confirming it's green after the embedded-objects ABI flip is the
check that matters most here.
