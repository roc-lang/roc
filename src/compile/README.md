# Compile

This directory holds the core logic for compiling Roc modules. It takes Roc source code and processes it through various stages to produce an executable or library.

Key responsibilities include:

- **Compilation Pipeline**: Managing the flow of a module through the compiler stages:
  - Tokenizing
  - Parsing
  - Canonicalizing
  - Type Checking
  - Code Generation (Not yet implemented)
- **Module Caching**: Managing the cache of compiled modules to speed up subsequent builds
- **Package Building**: Building complete packages
- **Build Environment**: Managing build contexts and environments

The compile module serves as the orchestrator that coordinates between the different compiler stages and manages the build process efficiently.

## Embedding

Roc can be embedded in another Zig program to compile and run `.roc` files in-process. The driver is [`Coordinator`](coordinator.zig); the in-process execution path uses [`lir.LirImage`](../lir/lir_image.zig) and [`eval.LirInterpreter`](../eval/interpreter.zig).

The embedding API is stable in shape; implementation details may change between releases.

### Canonical sequence

```zig
const compile = @import("compile");
const lir = @import("lir");
const eval = @import("eval");
const check = @import("check");
const base = @import("base");

// 1. Builtins + Coordinator
var builtins = try eval.BuiltinModules.init(gpa);
defer builtins.deinit();

var coord = try compile.coordinator.Coordinator.init(
    gpa, .single_threaded, 1,
    target, &builtins, version, null,
);
defer coord.deinit();
coord.enable_hosted_transform = true;  // if you have host functions
coord.setIo(my_io);                     // optional: virtualise the filesystem

// 2. Discover + compile
try coord.start();
try coord.discoverAppFromPath(arena, .{ .entry_path = app_path });
try coord.coordinatorLoop();

// 3. Render or collect diagnostics, then check for errors before finalizing.
var it = coord.iterReports();
while (it.next()) |entry| {
    // entry.package_name, entry.module_name, entry.report
}
if (coord.hasUserErrors()) return error.CompilationFailed;

// 4. Finalize — links the platform-app relation. Must come after the
//    error check; calling this with outstanding user errors returns
//    error.HasUserErrors.
try coord.finalizeExecutableArtifacts();
// finalize can surface new errors of its own — check once more.
if (coord.hasUserErrors()) return error.CompilationFailed;

// 5. Lower to LIR. The allocator must own a single contiguous buffer —
//    see "Runtime arena" below.
const root = coord.executableRootCheckedArtifact();
const imports = try coord.collectImportedArtifactViews(arena, root);
const relations = try coord.collectRelationArtifactViews(arena, root);
const lir_roots = try lir.CheckedPipeline.selectPlatformEntrypointRoots(arena, root.root_requests.runtime_requests);

const lowered = try lir.CheckedPipeline.lowerCheckedModulesToLir(
    runtime_alloc,
    .{
        .root = check.CheckedArtifact.loweringViewWithRelations(root, relations),
        .imports = imports,
    },
    .{ .requests = lir_roots },
    .{ .target_usize = base.target.TargetUsize.native },
);

// 6. Build the LIR image in the same contiguous buffer.
const entrypoints = try lowered.platformEntrypoints(runtime_alloc);
const entrypoint_names = try lowered.platformEntrypointNames(arena, root);

const image_header = try runtime_alloc.create(lir.LirImage.Header);
try lir.LirImage.fillHeaderInBuffer(
    image_header,
    runtime_buffer.ptr,    // start of the contiguous backing buffer
    runtime_fba.end_index, // bytes used so far
    &lowered.lir_result,
    lowered.target_usize,
    entrypoints,
);
const view = try lir.LirImage.viewMappedImage(
    image_header,
    runtime_buffer.ptr,
    runtime_fba.end_index,
);

// 7. Execute via the interpreter.
var interp = try eval.LirInterpreter.init(gpa, &view.store, &view.layouts, &my_roc_ops);
defer interp.deinit();
_ = try interp.runEntrypoint(&view, 0, &args, &result_buf);
```

### Type-check-only flow

For LSP, fuzzing, or any consumer that only needs diagnostics, stop after step 3:

```zig
try coord.start();
try coord.discoverAppFromPath(arena, .{ .entry_path = app_path });
try coord.coordinatorLoop();

var it = coord.iterReports();
while (it.next()) |entry| {
    // render entry.report
}
const had_errors = coord.hasUserErrors();
```

Skip `finalizeExecutableArtifacts`, LIR lowering, and execution entirely.

### Runtime arena

`fillHeaderInBuffer` and `viewMappedImage` compute offsets via `ptr - base_ptr` arithmetic, so **the image allocator must own a single contiguous virtual region**. `std.heap.ArenaAllocator` grows by allocating new pages and **will not work** — the offsets it computes will be wrong, either silently producing a broken image or tripping `error.InvalidLirImage`.

Use a `std.heap.FixedBufferAllocator` over a heap-allocated contiguous buffer:

```zig
const RUNTIME_ARENA_SIZE = 128 * 1024 * 1024; // 128 MiB

const runtime_buffer = try gpa.alignedAlloc(u8, 16, RUNTIME_ARENA_SIZE);
defer gpa.free(runtime_buffer);

var runtime_fba = std.heap.FixedBufferAllocator.init(runtime_buffer);
const runtime_alloc = runtime_fba.allocator();
```

**Sizing:** budget at least 128 MiB for a typical app — even a "hello world" needs more than 16 MiB after lowering all reachable modules. Anonymous virtual memory overcommits on Linux/macOS, so a generously-sized buffer doesn't cost real RAM until actually touched. Pick a number that comfortably exceeds your largest expected program.

### Memory ownership

Refcounted values that Roc returns to the host (a `RocStr`, `RocList`, or `RocBox` written through `ret_ptr`; or any of those passed as arguments to your hosted functions) are **heap-owned by the host**. The interpreter does not auto-tear-down on `eval` return — that's intentional, since embedders frequently want to inspect the return value before freeing it.

The host must explicitly `decref` returned refcounted values when done. Without this, host allocator leak-checking will report leaks tracing back to `rocAllocFn` in the interpreter:

```zig
// After interp.runEntrypoint(...) returns, if the entrypoint's return type
// is a refcounted value (e.g. RocStr returned via ret_ptr), decref before
// tearing down. `RocStr.decref` takes the value by copy and a *RocOps; it's
// a no-op for small (stack) strings.
const result_str: RocStr = @as(*const RocStr, @ptrCast(@alignCast(&result_buf))).*;
defer result_str.decref(&my_roc_ops);
```

The same rule applies to hosted-fn arguments: the host owns each `*RocStr` (or other refcounted pointer) it receives for the call's duration. Read freely, but if you stow the value to use later, call the appropriate `incref` helper first.

### Custom filesystem

Implement the [`Io`](../io/Io.zig) vtable and pass it via `coord.setIo(...)`. The default `Io` reads from `std.fs.cwd()`.

For embedders that don't have a filesystem at all (e.g. wasm), `Coordinator` doesn't require one as long as every path passed in is served through the configured `Io`. The cache manager (`cache_manager` parameter to `Coordinator.init`) can be left null.

### Host functions

Set `coord.enable_hosted_transform = true` before `coord.start()`. Hosted lambda bodies in platform modules are auto-converted during canonicalization.

At execution time, `RocOps.hosted_fns.fns` is a **positional array** — the interpreter calls `fns[dispatch_index]`. The dispatch index is computed deterministically by:

1. Within each checked artifact, hosted functions are sorted alphabetically by their fully-qualified name `Module.fn_name` (with trailing `!` stripped — so `Echo.line!` sorts as `Echo.line`), tiebroken by definition order.
2. The global catalog concatenates artifacts in this order: the root (executable) artifact's hosted functions, then each imported artifact in import order, then each relation artifact in relation order.

For a platform whose hosted functions all live in one module — the typical case — this collapses to **plain alphabetical order by `Module.fn_name`**. Build your `HostedFn` array to match.

Wrong order is silent (wrong function called), not loud — be deliberate, especially when adding hosted functions across multiple modules.

### URL-resolved packages

`Coordinator.discoverAppFromPath` does not download URL-specified packages — it only supports relative paths. For URL support:

1. Call `compile.app_header.parseAppHeader(io, gpa, arena, path)` to get the raw header.
2. Resolve URLs to local paths using your own caching policy.
3. Register packages with `coord.ensurePackage` / `coord.registerInlinePackage` instead of `discoverAppFromPath`.

The Roc CLI follows exactly this pattern — see `buildLirImageWithCoordinator` in `src/cli/main.zig` for a worked example.
