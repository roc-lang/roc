# Machine-code run shim

This archive loads the compiler's explicit run-image contract inside a platform
process. Its build-time verifier seals the host boundary: only declared Roc and
C/OS ABI imports and the three shim entrypoints may participate in platform
symbol resolution.

Compiler-generated integer division, wide shifts and ARM EABI helpers use
unmodified copies from the active Zig toolchain. Linux x86/ARM retain 64-bit
shifts; LLVM Linux x86_64/AArch64 and macOS x86_64 retain 128-bit shifts.
The self-hosted x86_64 backend does not produce these shift libcalls and does
not use the LLVM assembler retention mechanism. ARM shift adapters pin the
AAPCS core-register signature while calling the upstream integer C entries.
Symbol and relocation inspection of Zig 0.17.0 checked archives in all four
optimization modes shows no recursive shift calls: Debug ELF cores call only
`memset`, ARM adapters call local C entries, Debug Mach-O bodies call `memset`
and stack-canary support, and optimized helpers have no calls. Half-width
operations therefore close at native instructions in those inspected artifacts.
Host arithmetic tests exercise the upstream C entries; ARM adapters have ABI
assertions and cross-link coverage, not behavioral execution coverage. An
adapter selecting the wrong same-signature core would still need an ARM
execution test to detect it.
Integer/float conversion cores are an
attributed, checked-in copy of Zig's pure implementations: importing the upstream
conversion files also references unrelated wrapper families and emits unwanted
runtime dependencies in Debug. All helpers are compiled into the shim with local
binding. They must neither depend on the host's compiler-rt nor interpose its
definitions.
Source references retain the implementations, while assembler-local aliases
preserve libcall names through LLVM's late libcall generation. Whole public
compiler-rt archives are not shim inputs.

Linux ARM instruction publication uses the kernel cache-flush ABI. The Linux
ARM shim emits no exception-unwind tables: its host crash contract aborts and
its std options disable stack tracing, so it must not depend on EHABI
personalities or pretend to implement them with empty routines. This policy
does not change Windows unwind metadata or other architectures' stack walking.

`zig build check-selected-machine-code-shim -Dtarget=x86-linux-musl` (or
`arm-linux-musleabihf`) checks the actual archive and links it with deliberate
strong host/compiler-runtime collisions. `run-check-machine-code-shim-archive`
covers all supported object formats. Run optimized checks too: libcall creation
and dead-code elimination differ from Debug.
