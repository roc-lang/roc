# Interpreter Shim

The interpreter shim is a key component for running Roc programs. It provides two operating modes depending on how the Roc application is built and executed.

## Operating Modes

### 1. IPC Mode (`roc path/to/app.roc`)

When running Roc programs during development with `roc`, the shim operates in **IPC (Inter-Process Communication) mode**:

```
┌─────────────────┐     Shared Memory      ┌──────────────────┐
│    roc CLI      │ ──────────────────────>│  Interpreter     │
│  (parent proc)  │   ModuleEnv + CIR      │  Host (child)    │
└─────────────────┘                        └──────────────────┘
```

**How it works:**
1. The `roc` CLI compiles the Roc source code and creates a `ModuleEnv` containing the Canonical IR (CIR)
2. This data is placed in shared memory (POSIX `shm_open` or Windows `CreateFileMapping`)
3. The interpreter host is spawned as a child process
4. The child maps the shared memory and directly accesses the `ModuleEnv` (pointer relocation is applied)
5. The interpreter evaluates the CIR and executes the program

**Characteristics:**
- Fast startup (no serialization/deserialization)
- Same-architecture only (pointers must match between parent and child)
- Memory efficient (data is shared, not copied)
- Used for development workflow

### 2. Embedded Mode (`roc build path/to/app.roc`)

When building standalone executables with `roc build`, the shim operates in **Embedded mode**:

```
┌─────────────────┐                     ┌──────────────────────────────┐
│    roc CLI      │   serialize         │      Output Binary           │
│   (compiler)    │ ─────────────────>  │  ┌──────────────────────┐   │
└─────────────────┘                     │  │ Interpreter Shim     │   │
                                        │  │ + Embedded CIR Data  │   │
                                        │  └──────────────────────┘   │
                                        └──────────────────────────────┘
```

**How it works:**
1. The `roc` CLI compiles the Roc source and serializes the `ModuleEnv` to a portable format
2. The serialized data is embedded directly into the output binary (via `@embedFile`)
3. At runtime, the shim reads from `roc__serialized_base_ptr` (a symbol pointing to embedded data)
4. The data is deserialized into a `ModuleEnv` and executed

**Characteristics:**
- Cross-architecture support (serialization is portable)
- Standalone binaries (no external dependencies)
- Slightly slower startup (deserialization required)
- Used for distribution

## Key Symbols

- `roc__serialized_base_ptr`: Points to embedded serialized data (embedded mode)
- `roc__main`: Entry point called by the platform host
- `roc_alloc`, `roc_dealloc`, `roc_realloc`: Memory allocation functions

## Platform Support

| Platform | IPC Mode | Embedded Mode |
|----------|----------|---------------|
| Linux    | Yes      | Yes           |
| macOS    | Yes      | Yes           |
| Windows  | Yes      | Yes           |
| WASM32   | No       | Yes           |

WASM targets only support embedded mode.
