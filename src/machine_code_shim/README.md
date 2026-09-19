# Machine-code run shim

This archive loads the compiler's explicit run-image contract inside a platform
process. Its build-time verifier seals the host boundary: only declared Roc and
C/OS ABI imports and the three shim entrypoints may participate in platform
symbol resolution.

Compiler-generated 32-bit arithmetic and ARM AAPCS helpers use the active Zig
toolchain's implementations, compiled into the shim with local binding. They
must neither depend on the host's compiler-rt nor interpose its definitions.
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
