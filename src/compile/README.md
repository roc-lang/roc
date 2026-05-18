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

Roc can be embedded in another Zig program to compile and run `.roc` files in-process. The driver is [`Coordinator`](coordinator.zig); the in-process execution path uses [`lir.RuntimeImage`](../lir/runtime_image.zig) and [`eval.LirInterpreter`](../eval/interpreter.zig).

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

// Render or collect diagnostics
var it = coord.iterReports();
while (it.next()) |entry| {
    // entry.package_name, entry.module_name, entry.report
}

try coord.finalizeExecutableArtifacts();
if (coord.hasUserErrors()) return error.CompilationFailed;

// 3. Lower to LIR
const root = coord.executableRootCheckedArtifact();
const imports = try coord.collectImportedArtifactViews(arena, root);
const relations = try coord.collectRelationArtifactViews(arena, root);

const lowered = try lir.CheckedPipeline.lowerArtifactsToLir(
    arena,
    .{
        .root = check.CheckedArtifact.loweringViewWithRelations(root, relations),
        .imports = imports,
    },
    .{ .requests = root.root_requests.requests },
    .{ .target_usize = base.target.TargetUsize.native },
);

// 4. Build the runtime image in an arena buffer (no shared memory required)
const entrypoints = try lowered.platformEntrypoints(arena);
const entrypoint_names = try lowered.platformEntrypointNames(arena, root);

const runtime_header = try arena.create(lir.RuntimeImage.Header);
try lir.RuntimeImage.fillHeaderInBuffer(
    runtime_header,
    base_ptr,            // start of the arena's backing buffer
    used_size,           // bytes used so far
    &lowered.lir_result,
    lowered.target_usize,
    entrypoints,
);
const view = try lir.RuntimeImage.viewMappedImage(
    runtime_header,
    @constCast(base_ptr),
    used_size,
);

// 5. Execute via the interpreter
var interp = try eval.LirInterpreter.init(gpa, &view.store, &view.layouts, &my_roc_ops);
defer interp.deinit();
_ = try interp.runEntrypoint(&view, 0, &args, &result_buf);
```

For type-check-only flows (LSP, fuzzing), skip steps 3-5.

### Custom filesystem

Implement the [`Io`](../io/Io.zig) vtable and pass it via `coord.setIo(...)`. The default `Io` reads from `std.fs.cwd()`.

For embedders that don't have a filesystem at all (e.g. wasm), `Coordinator` doesn't require one as long as every path passed in is served through the configured `Io`. The cache manager (`cache_manager` parameter to `Coordinator.init`) can be left null.

### Host functions

Set `coord.enable_hosted_transform = true` before `coord.start()`. Hosted lambda bodies in platform modules are auto-converted during canonicalization.

At execution time, populate `RocOps.hosted_fns` with function pointers. The interpreter matches against the platform's hosted function names; ordinals are determined by the order they appear in `lowered.lir_result.root_procs`.

### URL-resolved packages

`Coordinator.discoverAppFromPath` does not download URL-specified packages — it only supports relative paths. For URL support:

1. Call `compile.app_header.parseAppHeader(io, gpa, arena, path)` to get the raw header.
2. Resolve URLs to local paths using your own caching policy.
3. Register packages with `coord.ensurePackage` / `coord.registerInlinePackage` instead of `discoverAppFromPath`.

The Roc CLI follows exactly this pattern — see `buildLirRuntimeImageWithCoordinator` in `src/cli/main.zig` for a worked example.
