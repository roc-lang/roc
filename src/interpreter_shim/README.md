# Interpreter Shim

The interpreter shim runs already-lowered Roc programs. The compiler parent
process owns checking and post-check compilation and publishes an ARC-inserted
LIR image into shared memory. The child process maps that same memory, creates
zero-copy views over the LIR/runtime-layout arrays, and evaluates them with the
LIR interpreter.

## Runtime Boundary

```
┌─────────────────┐   LIR image    ┌──────────────────┐
│    roc CLI      │ ──────────────────────> │  Interpreter     │
│  (parent proc)  │                         │  Host (child)    │
└─────────────────┘                         └──────────────────┘
```

Parent responsibilities:

1. Parse and canonicalize source.
2. Type check and publish checked modules.
3. Resolve roots, platform entrypoints, hosted procedures, static dispatch, and
   compile-time constants before post-check lowering.
4. Lower through the post-check IRs to LIR and insert ARC statements.
5. Publish the target-specific LIR image into shared memory.

Child responsibilities:

1. Map shared memory.
2. Validate the LIR image header and bounds.
3. Create zero-copy views of the LIR store, committed layouts, literal pool,
   root procedures, and hosted-function dispatch metadata.
3. Initialize `LirInterpreter`.
4. Invoke the explicit LIR root procedure requested by the host.

The child must never receive `ModuleEnv`, CIR, checked modules, or post-check
IRs. It must not perform compiler lowering, root discovery, static dispatch
resolution, platform lookup, compile-time evaluation, or recovery of missing
compiler data.

## Operating Modes

### IPC Mode (`roc path/to/app.roc`)

The parent publishes the LIR image into shared memory using the existing
shared-memory coordination path. The child maps that memory and views the image
in place.

### Embedded Mode (`roc build --opt=interpreter path/to/app.roc`)

The parent embeds a viewable LIR image into the output binary. At runtime,
the shim views that image without running semantic compiler stages.

## Linking Into a Platform

`roc` embeds one shim archive, built for the target `roc` itself runs on, and
links it into any platform target with the same OS and architecture. The shim
calls the OS directly and brings its own compiler-rt, so the platform's C ABI
does not matter: the same archive serves `x64win` and `x64mingw`.

On Windows the shim's compiler-rt is private. A platform host carries the same
helpers (`compiler_builtins` in a Rust host, compiler-rt in a Zig or C host),
and a COFF link rejects the duplicate weak definitions that ELF and Mach-O let
coexist. The build gives every global in the shim's `compiler_rt.obj` a
shim-only name (see `src/build/coff_private_compiler_rt.zig`) and then links the
result beside a platform object that defines the same helpers.

## Key Symbols

- `roc__main`: Entry point called by the platform host.
- `roc_alloc`, `roc_dealloc`, `roc_realloc`: Memory allocation functions.
