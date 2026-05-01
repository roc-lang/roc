# Interpreter Shim

The interpreter shim runs already-lowered Roc programs. The compiler parent
process owns all semantic compilation and writes a serialized ARC-inserted LIR
runtime image. The child process only deserializes that image and evaluates it
with the LIR interpreter.

## Runtime Boundary

```
┌─────────────────┐   LIR runtime image    ┌──────────────────┐
│    roc CLI      │ ──────────────────────> │  Interpreter     │
│  (parent proc)  │                         │  Host (child)    │
└─────────────────┘                         └──────────────────┘
```

Parent responsibilities:

1. Parse and canonicalize source.
2. Type check and publish checked artifacts.
3. Resolve roots, platform entrypoints, hosted procedures, static dispatch, and
   compile-time constants before post-check lowering.
4. Lower through MIR, IR, LIR, and ARC insertion.
5. Serialize the target-specific LIR runtime image.

Child responsibilities:

1. Map or read the serialized LIR runtime image.
2. Deserialize the LIR store, committed layouts, literal pool, root procedures,
   and hosted-function dispatch metadata.
3. Initialize `LirInterpreter`.
4. Invoke the explicit LIR root procedure requested by the host.

The child must never receive `ModuleEnv`, CIR, checked artifacts, MIR, or IR. It
must not perform semantic lowering, root discovery, static dispatch resolution,
platform lookup, compile-time evaluation, or recovery of missing compiler data.

## Operating Modes

### IPC Mode (`roc path/to/app.roc`)

The parent writes the serialized runtime image into shared memory. The child
maps that memory and deserializes the image.

### Embedded Mode (`roc build --opt=interpreter path/to/app.roc`)

The parent embeds the serialized runtime image into the output binary. At
runtime, the shim reads the embedded bytes and deserializes the same image shape
used by IPC mode.

## Key Symbols

- `roc__serialized_base_ptr`: Points to embedded serialized runtime-image data.
- `roc__main`: Entry point called by the platform host.
- `roc_alloc`, `roc_dealloc`, `roc_realloc`: Memory allocation functions.
