//! Glue code generation for Roc platforms.
//!
//! This module handles the `roc glue` command, which generates host-language
//! binding code (e.g., Zig structs) from a platform's type information.
//!
//! The glue contract is targetless: the glue script input carries no concrete
//! `RocTarget`, OS, or architecture. Every ABI fact is emitted for both pointer
//! widths (`size32`/`size64`, `offset32`/`offset64`, ...) via explicit
//! dual-width layout store queries, or is width-independent (field order,
//! names, discriminants, refcount plans). ABI field lists are emitted in
//! committed layout order, which is identical at both widths with
//! non-decreasing offsets (asserted here). Generated bindings branch on host
//! pointer width only; concrete targets exist solely when compiling/running
//! things on this machine (the platform type collection and the glue script
//! itself).
//!
//! The pipeline:
//! 1. Parse platform header to extract requires entries and type aliases
//! 2. Compile the platform via BuildEnv with a synthetic app, publishing checked artifacts
//! 3. Collect hosted functions and module type info from checked artifacts
//! 4. Build the glue input type table from artifact-owned checked type data
//! 5. Materialize the glue input as Roc C-ABI values
//! 6. Compile the glue spec through checked artifacts, lower to LIR, and run it with the requested backend

const std = @import("std");
const builtin = @import("builtin");
const Allocator = std.mem.Allocator;
const base = @import("base");
const parse = @import("parse");
const compile = @import("compile");
const check = @import("check");
const can = @import("can");
const backend = @import("backend");
const roc_target = @import("roc_target");
const layout = @import("layout");
const lir = @import("lir");
const GuardedList = lir.LirStore.GuardedList;

const ModuleEnv = can.ModuleEnv;
const BuildEnv = compile.BuildEnv;
const RocTarget = roc_target.RocTarget;
const CheckedArtifact = check.CheckedArtifact;
const CanonicalNameStore = check.CanonicalNames.CanonicalNameStore;
const CIR = can.CIR;
const checked_artifact_layout_resolver = @import("checked_artifact_layout_resolver.zig");
const CheckedArtifactLayoutResolver = checked_artifact_layout_resolver.Resolver;

const builtins = @import("builtins");
const RocStr = builtins.str.RocStr;
const RocList = builtins.list.RocList;

const eval_mod = @import("eval");

/// Backend used to execute the glue spec.
pub const GlueOpt = enum {
    dev,
    interpreter,
};

/// Arguments for glue code generation.
pub const GlueArgs = struct {
    glue_spec: []const u8,
    output_dir: []const u8,
    platform_path: []const u8,
    opt: GlueOpt = .dev,
    no_cache: bool = false,
};

/// Error types for glue generation operations.
pub const GlueError = error{
    GlueSpecNotFound,
    NotPlatformFile,
    FileNotFound,
    ParseFailed,
    PlatformPathResolution,
    TempDirCreation,
    SyntheticAppWrite,
    BuildEnvInit,
    CompilationFailed,
    DevBackendUnavailable,
    ModuleRetrieval,
    OutOfMemory,
    WriteFailed,
};

/// Print platform glue information for a platform's main.roc file using the checked-artifact pipeline.
/// Hosted function ordering comes from published `HostedProcTable` records.
pub fn rocGlue(gpa: Allocator, stderr: *std.Io.Writer, stdout: *std.Io.Writer, args: GlueArgs, temp_dir: []const u8, std_io: std.Io) GlueError!void {
    rocGlueInner(gpa, stderr, stdout, args, temp_dir, std_io) catch |err| {
        (switch (err) {
            error.GlueSpecNotFound => stderr.print("Error: Glue spec file not found: '{s}'\n", .{args.glue_spec}),
            error.NotPlatformFile => blk: {
                stderr.print("Error: '{s}' is not a platform file.\n", .{args.platform_path}) catch {};
                break :blk stderr.print("The glue command only works with platform files.\n", .{});
            },
            error.FileNotFound => stderr.print("Error: File not found: '{s}'\n", .{args.platform_path}),
            error.ParseFailed => stderr.print("Error: Failed to parse '{s}'\n", .{args.platform_path}),
            error.PlatformPathResolution => stderr.print("Error: Could not resolve platform path\n", .{}),
            error.TempDirCreation => stderr.print("Error: Could not create temp directory\n", .{}),
            error.SyntheticAppWrite => stderr.print("Error: Could not write synthetic app\n", .{}),
            error.BuildEnvInit => stderr.print("Error: Failed to initialize build environment\n", .{}),
            error.CompilationFailed => stderr.print("Error: Compilation failed\n", .{}),
            error.DevBackendUnavailable => stderr.print("Error: The dev backend is not available for this host. Use `roc glue --opt=interpreter ...`.\n", .{}),
            error.ModuleRetrieval => stderr.print("Error: Failed to get compiled modules\n", .{}),
            error.OutOfMemory => stderr.print("Error: Out of memory\n", .{}),
            error.WriteFailed => stderr.print("Error: Write failed\n", .{}),
        }) catch {};
        return err;
    };
}

fn rocGlueInner(gpa: Allocator, stderr: *std.Io.Writer, stdout: *std.Io.Writer, args: GlueArgs, temp_dir: []const u8, std_io: std.Io) GlueError!void {

    // 0. Validate glue spec file exists
    std.Io.Dir.cwd().access(std_io, args.glue_spec, .{}) catch {
        return error.GlueSpecNotFound;
    };

    // 1. Parse platform header to get requires entries and verify it's a platform file.
    // Header parsing is still allowed here because it is parser-stage syntax handling,
    // not post-check semantic recovery.
    const platform_info = parsePlatformHeader(gpa, args.platform_path, std_io) catch |err| {
        return switch (err) {
            error.NotPlatformFile => error.NotPlatformFile,
            error.FileNotFound => error.FileNotFound,
            error.ParseFailed => error.ParseFailed,
            else => error.ParseFailed,
        };
    };
    defer platform_info.deinit(gpa);

    // 2. Compile platform using BuildEnv by creating a synthetic app.
    // BuildEnv publishes checked artifacts for both the synthetic app and the platform.
    const platform_abs_path = std.Io.Dir.cwd().realPathFileAlloc(std_io, args.platform_path, gpa) catch {
        return error.PlatformPathResolution;
    };
    defer gpa.free(platform_abs_path);

    var app_source = std.ArrayList(u8).empty;
    defer app_source.deinit(gpa);
    var aw: std.Io.Writer.Allocating = .fromArrayList(gpa, &app_source);
    const w = &aw.writer;

    try w.print("app [", .{});

    for (platform_info.type_aliases, 0..) |alias_name, i| {
        if (i > 0) try w.print(", ", .{});
        try w.print("{s}", .{alias_name});
    }

    for (platform_info.requires_entries, 0..) |entry, i| {
        if (platform_info.type_aliases.len > 0 or i > 0) {
            try w.print(", ", .{});
        }
        try w.print("{s}", .{entry.name});
    }

    try w.print("] {{ pf: platform \"", .{});
    for (platform_abs_path) |ch| {
        if (ch == '\\') {
            try w.print("\\\\", .{});
        } else {
            try w.print("{c}", .{ch});
        }
    }
    try w.print("\" }}\n\n", .{});

    for (platform_info.type_aliases) |alias_name| {
        try w.print("{s} : {{}}\n", .{alias_name});
    }
    if (platform_info.type_aliases.len > 0) {
        try w.print("\n", .{});
    }

    for (platform_info.requires_entries) |entry| {
        try w.print("{s} = {s}\n", .{ entry.name, entry.stub_expr });
    }

    // Sync the writer back to app_source
    app_source = aw.toArrayList();
    const synthetic_app_path = std.fs.path.join(gpa, &.{ temp_dir, "synthetic_app.roc" }) catch {
        return error.OutOfMemory;
    };
    defer gpa.free(synthetic_app_path);

    std.Io.Dir.cwd().writeFile(std_io, .{
        .sub_path = synthetic_app_path,
        .data = app_source.items,
    }) catch {
        return error.SyntheticAppWrite;
    };

    const cwd = std.Io.Dir.cwd().realPathFileAlloc(std_io, ".", gpa) catch {
        return error.BuildEnvInit;
    };
    defer gpa.free(cwd);
    var build_env = BuildEnv.init(gpa, .single_threaded, 1, RocTarget.detectNative(), cwd, std_io) catch {
        return error.BuildEnvInit;
    };
    defer build_env.deinit();
    build_env.setSyntheticRootPackageIdentity();
    // Glue caches by default like every other pipeline; --no-cache opts out.
    if (!args.no_cache) build_env.enableDefaultCacheManager(false) catch {
        return error.BuildEnvInit;
    };

    build_env.build(synthetic_app_path) catch {
        _ = try build_env.renderDiagnostics(stderr);
        return error.CompilationFailed;
    };
    _ = try build_env.renderDiagnostics(stderr);

    const modules = build_env.getModulesInSerializationOrder(gpa) catch {
        return error.ModuleRetrieval;
    };
    defer gpa.free(modules);

    const hosted_indices = collectHostedProcGlobalIndices(gpa, modules) catch {
        return error.OutOfMemory;
    };
    defer {
        for (hosted_indices) |index| gpa.free(index.sort_key);
        gpa.free(hosted_indices);
    }
    var hosted_symbols = collectHostedSymbols(gpa, &platform_info) catch {
        return error.OutOfMemory;
    };
    defer deinitHostedSymbols(gpa, &hosted_symbols);

    // 3. Collect platform module type information from checked artifacts.
    var collected_modules = std.ArrayList(CollectedModuleTypeInfo).empty;
    defer {
        for (collected_modules.items) |*mod_info| {
            mod_info.deinit(gpa);
        }
        collected_modules.deinit(gpa);
    }

    // Index every checked artifact by key so nominal representations can resolve
    // their declaration owners without reconstructing ownership from names.
    var artifacts_by_key = ArtifactKeyMap.init(gpa);
    defer artifacts_by_key.deinit();
    for (modules) |mod| {
        const artifact = mod.semantic.checked_artifact orelse continue;
        try artifacts_by_key.put(artifact.key, artifact);
    }

    var glue_layouts = layout.Store.init(gpa, .u64) catch {
        return error.OutOfMemory;
    };
    defer glue_layouts.deinit();

    var layout_resolver = CheckedArtifactLayoutResolver.init(&glue_layouts, &artifacts_by_key);
    defer layout_resolver.deinit();

    var type_table = TypeTable.init(gpa, &artifacts_by_key, &glue_layouts, &layout_resolver);
    defer type_table.deinit();

    for (modules) |mod| {
        if (mod.is_platform_sibling or mod.is_platform_main) {
            const artifact = mod.semantic.checked_artifact orelse continue;
            type_table.clearVarMap();
            const collected = collectModuleTypeInfo(gpa, artifact, mod.name, hosted_indices, &hosted_symbols, &type_table) catch |err| switch (err) {
                error.OutOfMemory => return error.OutOfMemory,
                error.UnresolvedByValue => return reportUnresolvedTypeVariable(stderr, &type_table),
            };
            if (collected) |mod_info| {
                var owned_mod_info = mod_info;
                errdefer owned_mod_info.deinit(gpa);
                try collected_modules.append(gpa, owned_mod_info);
            }
        }
    }

    // 4. Register platform entrypoint and provided-function type ids from the
    // platform main artifact's published requires/provides metadata.
    var entrypoint_type_ids = std.StringHashMap(u64).init(gpa);
    defer entrypoint_type_ids.deinit();
    var provides_type_ids = std.StringHashMap(u64).init(gpa);
    defer provides_type_ids.deinit();

    var provides_entries = std.ArrayList(PlatformHeaderInfo.ProvidesEntry).empty;
    defer {
        for (provides_entries.items) |entry| {
            gpa.free(entry.name);
            gpa.free(entry.ffi_symbol);
        }
        provides_entries.deinit(gpa);
    }

    for (modules) |mod| {
        if (!mod.is_platform_main) continue;
        const artifact = mod.semantic.checked_artifact orelse return error.ModuleRetrieval;
        type_table.clearVarMap();

        for (artifact.provides_requires.provides) |provides_entry| {
            try provides_entries.append(gpa, .{
                .name = try gpa.dupe(u8, artifact.canonical_names.exportNameText(provides_entry.source_name)),
                .ffi_symbol = try gpa.dupe(u8, artifact.canonical_names.externalSymbolNameText(provides_entry.ffi_symbol)),
            });
        }

        for (artifact.platform_required_declarations.declarations) |declaration| {
            const name = artifact.canonical_names.exportNameText(declaration.platform_name);
            const checked_type = platformRequiredEntrypointCheckedType(artifact, declaration);
            const type_id = type_table.getOrInsert(artifact, checked_type) catch |err| switch (err) {
                error.OutOfMemory => return error.OutOfMemory,
                error.UnresolvedByValue => return reportUnresolvedTypeVariable(stderr, &type_table),
            };
            try entrypoint_type_ids.put(name, type_id);
        }

        for (artifact.provides_requires.provides) |provides_entry| {
            const def_idx = provides_entry.def orelse continue;
            const top_level = artifact.top_level_values.lookupByDef(def_idx) orelse
                glueInvariant("provided entry has no top-level value", .{});
            const scheme = artifact.checked_types.schemeForKey(top_level.source_scheme) orelse
                glueInvariant("provided entry has no checked type scheme", .{});
            const type_id = type_table.getOrInsert(artifact, scheme.root) catch |err| switch (err) {
                error.OutOfMemory => return error.OutOfMemory,
                error.UnresolvedByValue => return reportUnresolvedTypeVariable(stderr, &type_table),
            };
            const ffi_symbol = artifact.canonical_names.externalSymbolNameText(provides_entry.ffi_symbol);
            try provides_type_ids.put(ffi_symbol, type_id);
        }
        break;
    }

    type_table.attachAbiLayouts(&build_env) catch {
        return error.OutOfMemory;
    };

    // 5. Compile glue spec through checked artifacts and lower to LIR.
    const glue_spec_abs = std.Io.Dir.cwd().realPathFileAlloc(std_io, args.glue_spec, gpa) catch {
        return error.GlueSpecNotFound;
    };
    defer gpa.free(glue_spec_abs);

    const glue_cwd = std.Io.Dir.cwd().realPathFileAlloc(std_io, ".", gpa) catch {
        return error.BuildEnvInit;
    };
    defer gpa.free(glue_cwd);
    var glue_build_env = BuildEnv.init(gpa, .single_threaded, 1, RocTarget.detectNative(), glue_cwd, std_io) catch {
        return error.BuildEnvInit;
    };
    defer glue_build_env.deinit();
    if (!args.no_cache) glue_build_env.enableDefaultCacheManager(false) catch {
        return error.BuildEnvInit;
    };

    glue_build_env.build(glue_spec_abs) catch {
        _ = try glue_build_env.renderDiagnostics(stderr);
        return error.CompilationFailed;
    };
    _ = try glue_build_env.renderDiagnostics(stderr);

    const root_artifact = glue_build_env.executableRootCheckedArtifact();
    const imported_artifacts = glue_build_env.collectImportedArtifactViews(gpa, root_artifact) catch {
        return error.OutOfMemory;
    };
    defer gpa.free(imported_artifacts);
    const relation_artifacts = glue_build_env.collectRelationArtifactViews(gpa, root_artifact) catch {
        return error.OutOfMemory;
    };
    defer gpa.free(relation_artifacts);

    const lir_roots = lir.CheckedPipeline.selectPlatformEntrypointRoots(gpa, root_artifact.root_requests.runtime_requests) catch {
        return error.OutOfMemory;
    };
    defer gpa.free(lir_roots);

    // The width the glue *script* executes at on this machine. This is a runner
    // implementation detail: the emitted ABI facts inside the Types payload were
    // already fixed by attachAbiLayouts querying both widths explicitly.
    const script_target_usize = base.target.TargetUsize.native;
    var lowered = lir.CheckedPipeline.lowerCheckedModulesToLir(
        gpa,
        .{
            .root = CheckedArtifact.loweringViewWithRelations(root_artifact, relation_artifacts),
            .imports = imported_artifacts,
        },
        .{ .requests = lir_roots },
        .{
            .target_usize = script_target_usize,
        },
    ) catch {
        return error.OutOfMemory;
    };
    defer lowered.deinit();

    const glue_proc = selectGlueSpecRootProc(root_artifact, &lowered, "roc_make_glue") orelse {
        if (builtin.mode == .Debug) {
            std.debug.panic("glue invariant violated: glue spec produced no published make_glue platform root", .{});
        }
        unreachable;
    };

    const arg_layouts = argLayoutsForProc(gpa, &lowered.lir_result.store, glue_proc) catch {
        return error.OutOfMemory;
    };
    defer gpa.free(arg_layouts);
    if (arg_layouts.len != 1) {
        glueInvariant("make_glue expected one List(Types) argument, got {d}", .{arg_layouts.len});
    }

    // 6. Construct List(Types) using the exact committed LIR layout and invoke the requested backend.
    var runtime_env = eval_mod.RuntimeHostEnv.init(gpa);
    defer runtime_env.deinit();
    const glue_writer = GlueRocValueWriter{
        .layouts = &lowered.lir_result.layouts,
        .schemas = &lowered.runtime_value_schemas,
        .roc_ops = runtime_env.get_ops(),
    };
    var types_list = constructTypesRocList(&glue_writer, collected_modules.items, &platform_info, provides_entries.items, &type_table, &entrypoint_type_ids, &provides_type_ids, arg_layouts[0]);

    const proc = lowered.lir_result.store.getProcSpec(glue_proc);
    const ret_size_align = lowered.lir_result.layouts.layoutSizeAlign(lowered.lir_result.layouts.getLayout(proc.ret_layout));
    const ret_alignment = std.mem.Alignment.fromByteUnits(ret_size_align.alignment.toByteUnits());
    const result_ptr = gpa.rawAlloc(ret_size_align.size, ret_alignment, @returnAddress()) orelse {
        return error.OutOfMemory;
    };
    const result_buf = result_ptr[0..ret_size_align.size];
    defer gpa.rawFree(result_buf, ret_alignment, @returnAddress());
    if (result_buf.len > 0) @memset(result_buf, 0);

    switch (args.opt) {
        .dev => try runGlueSpecDev(gpa, stderr, &lowered, glue_proc, arg_layouts, &types_list, result_buf.ptr, &runtime_env),
        .interpreter => try runGlueSpecInterpreter(gpa, stderr, &lowered, glue_proc, arg_layouts, &types_list, result_buf.ptr, runtime_env.get_ops()),
    }
    try writeHostEvents(stderr, &runtime_env);

    const glue_result = try extractGlueResult(gpa, &glue_writer, result_buf.ptr, proc.ret_layout);
    defer glue_result.deinit();
    if (glue_result.err_msg) |err_msg| {
        stderr.print("Glue spec error: {s}\n", .{err_msg}) catch {};
        return error.CompilationFailed;
    }

    const files = glue_result.files;
    if (files.len == 0) {
        stdout.print("Glue spec returned 0 files.\n", .{}) catch {};
        return;
    }

    std.Io.Dir.cwd().createDirPath(std_io, args.output_dir) catch {
        stderr.print("Error: Could not create output directory: {s}\n", .{args.output_dir}) catch {};
        return error.CompilationFailed;
    };

    stdout.print("Glue spec returned {d} file(s):\n", .{files.len}) catch {};
    for (files) |file| {
        const file_name = file.name;
        const file_path = std.fs.path.join(gpa, &.{ args.output_dir, file_name }) catch {
            return error.OutOfMemory;
        };
        defer gpa.free(file_path);

        std.Io.Dir.cwd().writeFile(std_io, .{
            .sub_path = file_path,
            .data = file.content,
        }) catch {
            stderr.print("Error: Could not write file '{s}'\n", .{file_path}) catch {};
            return error.CompilationFailed;
        };

        stdout.print("  Wrote: {s}\n", .{file_path}) catch {};
    }
}

fn runGlueSpecInterpreter(
    gpa: Allocator,
    stderr: *std.Io.Writer,
    lowered: *lir.CheckedPipeline.LoweredProgram,
    glue_proc: lir.LirProcSpecId,
    arg_layouts: []const layout.Idx,
    types_list: *RocList,
    result_ptr: [*]u8,
    roc_ops: *builtins.host_abi.RocOps,
) GlueError!void {
    var interpreter = eval_mod.LirInterpreter.init(
        gpa,
        &lowered.lir_result.store,
        &lowered.lir_result.layouts,
        roc_ops,
    ) catch return error.OutOfMemory;
    defer interpreter.deinit();

    const proc = lowered.lir_result.store.getProcSpec(glue_proc);
    _ = interpreter.eval(.{
        .proc_id = glue_proc,
        .arg_layouts = arg_layouts,
        .ret_layout = proc.ret_layout,
        .arg_ptr = @ptrCast(types_list),
        .ret_ptr = @ptrCast(result_ptr),
    }) catch |err| {
        stderr.print("Error running glue spec: {}\n", .{err}) catch {};
        return error.CompilationFailed;
    };
}

fn runGlueSpecDev(
    gpa: Allocator,
    stderr: *std.Io.Writer,
    lowered: *lir.CheckedPipeline.LoweredProgram,
    glue_proc: lir.LirProcSpecId,
    arg_layouts: []const layout.Idx,
    types_list: *RocList,
    result_ptr: [*]u8,
    runtime_env: *eval_mod.RuntimeHostEnv,
) GlueError!void {
    if (comptime !backend.host_lir_codegen_available) {
        return error.DevBackendUnavailable;
    } else {
        var static_strings = backend.StaticStringData.build(
            gpa,
            &lowered.lir_result.store,
            backend.dev.LirCodeGenMod.host_lir_codegen_target,
        ) catch return error.OutOfMemory;
        defer static_strings.deinit();

        var codegen = backend.HostLirCodeGen.init(
            gpa,
            &lowered.lir_result.store,
            &lowered.lir_result.layouts,
            static_strings.entries,
        ) catch return error.OutOfMemory;
        defer codegen.deinit();

        codegen.compileAllProcSpecs(lowered.lir_result.store.getProcSpecs()) catch return error.OutOfMemory;

        const proc = lowered.lir_result.store.getProcSpec(glue_proc);
        const entrypoint = codegen.generateEntrypointWrapper(
            "roc_make_glue",
            glue_proc,
            arg_layouts,
            proc.ret_layout,
        ) catch return error.OutOfMemory;

        var executable = backend.ExecutableMemory.initWithEntryOffsetAndUnwindInfo(
            codegen.getGeneratedCode(),
            entrypoint.offset,
            codegen.getUnwindFunctions(),
        ) catch |err| switch (err) {
            error.OutOfMemory => return error.OutOfMemory,
            else => return error.CompilationFailed,
        };
        defer executable.deinit();

        runtime_env.resetObservation();
        var crash_boundary = runtime_env.enterCrashBoundary();
        defer crash_boundary.deinit();

        const sj = crash_boundary.set();
        if (sj == 0) {
            executable.callRocABI(
                @ptrCast(runtime_env.get_ops()),
                @ptrCast(result_ptr),
                @ptrCast(types_list),
            );
        }

        switch (runtime_env.crashState()) {
            .did_not_crash => {},
            .crashed => |message| {
                stderr.print("Error running glue spec: crashed with message: {s}\n", .{message}) catch {};
                return error.CompilationFailed;
            },
        }
    }
}

fn writeHostEvents(stderr: *std.Io.Writer, runtime_env: *const eval_mod.RuntimeHostEnv) GlueError!void {
    for (runtime_env.events.items) |event| {
        switch (event) {
            .dbg => |msg| try stderr.print("[dbg] {s}\n", .{msg}),
            .expect_failed => |msg| try stderr.print("Expect failed: {s}\n", .{msg}),
            .crashed => {},
        }
    }
}

const HostedProcGlobalIndex = struct {
    artifact_key: CheckedArtifact.CheckedModuleArtifactKey,
    def_idx: can.CIR.Def.Idx,
    index: usize,
    sort_key: []const u8,
};

fn checkedArtifactKeysEqual(
    a: CheckedArtifact.CheckedModuleArtifactKey,
    b: CheckedArtifact.CheckedModuleArtifactKey,
) bool {
    return std.mem.eql(u8, &a.bytes, &b.bytes);
}

/// Report the type table's recorded unresolved-type-variable error to stderr and
/// map it to a compilation failure, so `roc glue` exits nonzero with a message
/// naming the offending type instead of crashing on a missing committed layout.
fn reportUnresolvedTypeVariable(stderr: *std.Io.Writer, type_table: *const TypeTable) GlueError {
    const message = type_table.unresolved_error orelse
        "a glue-visible type has no committed memory layout because it holds an unresolved type variable by value";
    stderr.print("Error: {s}\n", .{message}) catch {};
    return error.CompilationFailed;
}

fn glueInvariant(comptime message: []const u8, args: anytype) noreturn {
    if (builtin.mode == .Debug) {
        std.debug.panic("glue invariant violated: " ++ message, args);
    }
    unreachable;
}

fn hostedProcSortKey(
    allocator: Allocator,
    artifact: *const CheckedArtifact.CheckedModuleArtifact,
    hosted: CheckedArtifact.HostedProc,
) Allocator.Error![]const u8 {
    return try allocator.dupe(u8, hosted.orderKey(&artifact.hosted_procs));
}

fn hostedProcForDef(
    table: *const CheckedArtifact.HostedProcTable,
    def_idx: CIR.Def.Idx,
) ?CheckedArtifact.HostedProc {
    for (table.procs) |proc| {
        if (proc.def_idx == def_idx) return proc;
    }
    return null;
}

fn collectHostedProcGlobalIndices(
    allocator: Allocator,
    modules: []const BuildEnv.CompiledModuleInfo,
) Allocator.Error![]HostedProcGlobalIndex {
    var indices = std.ArrayList(HostedProcGlobalIndex).empty;
    errdefer {
        for (indices.items) |index| allocator.free(index.sort_key);
        indices.deinit(allocator);
    }

    for (modules) |mod| {
        if (!(mod.is_platform_sibling or mod.is_platform_main)) continue;
        const artifact = mod.semantic.checked_artifact orelse continue;
        for (artifact.hosted_procs.procs) |hosted| {
            try indices.append(allocator, .{
                .artifact_key = artifact.key,
                .def_idx = hosted.def_idx,
                .index = 0,
                .sort_key = try hostedProcSortKey(allocator, artifact, hosted),
            });
        }
    }

    const SortContext = struct {
        pub fn lessThan(_: void, a: HostedProcGlobalIndex, b: HostedProcGlobalIndex) bool {
            return switch (std.mem.order(u8, a.sort_key, b.sort_key)) {
                .lt => true,
                .gt => false,
                .eq => @intFromEnum(a.def_idx) < @intFromEnum(b.def_idx),
            };
        }
    };
    std.mem.sort(HostedProcGlobalIndex, indices.items, {}, SortContext.lessThan);

    for (indices.items, 0..) |*index, i| {
        index.index = i;
    }

    return try indices.toOwnedSlice(allocator);
}

fn hostedGlobalIndexForDef(
    indices: []const HostedProcGlobalIndex,
    artifact_key: CheckedArtifact.CheckedModuleArtifactKey,
    def_idx: can.CIR.Def.Idx,
) usize {
    for (indices) |index| {
        if (index.def_idx == def_idx and checkedArtifactKeysEqual(index.artifact_key, artifact_key)) {
            return index.index;
        }
    }
    if (builtin.mode == .Debug) {
        std.debug.panic("glue invariant violated: hosted proc has no global index", .{});
    }
    unreachable;
}

fn stripTrailingBang(name: []const u8) []const u8 {
    if (std.mem.endsWith(u8, name, "!")) return name[0 .. name.len - 1];
    return name;
}

fn hostedKeyAlloc(allocator: Allocator, module_name: []const u8, local_name: []const u8) Allocator.Error![]const u8 {
    const stripped = stripTrailingBang(local_name);
    if (module_name.len == 0) return try allocator.dupe(u8, stripped);
    return try std.fmt.allocPrint(allocator, "{s}.{s}", .{ module_name, stripped });
}

fn deinitHostedSymbols(allocator: Allocator, hosted_symbols: *std.StringHashMap([]const u8)) void {
    var it = hosted_symbols.iterator();
    while (it.next()) |entry| {
        allocator.free(entry.key_ptr.*);
        allocator.free(entry.value_ptr.*);
    }
    hosted_symbols.deinit();
}

fn collectHostedSymbols(
    allocator: Allocator,
    platform_info: *const PlatformHeaderInfo,
) Allocator.Error!std.StringHashMap([]const u8) {
    var hosted_symbols = std.StringHashMap([]const u8).init(allocator);
    errdefer deinitHostedSymbols(allocator, &hosted_symbols);

    for (platform_info.hosted_entries) |entry| {
        const key = try allocator.dupe(u8, entry.key);
        const symbol = allocator.dupe(u8, entry.ffi_symbol) catch |err| {
            allocator.free(key);
            return err;
        };
        const gop = hosted_symbols.getOrPut(key) catch |err| {
            allocator.free(key);
            allocator.free(symbol);
            return err;
        };
        if (gop.found_existing) {
            allocator.free(key);
            allocator.free(symbol);
        } else {
            gop.value_ptr.* = symbol;
        }
    }

    return hosted_symbols;
}

fn selectGlueSpecRootProc(
    root_artifact: *const CheckedArtifact.CheckedModuleArtifact,
    lowered: *const lir.CheckedPipeline.LoweredProgram,
    expected_ffi_symbol: []const u8,
) ?lir.LirProcSpecId {
    for (lowered.lir_result.root_procs.items, lowered.lir_result.root_metadata.items) |root_proc, metadata| {
        if (metadata.kind != .provided_export) continue;
        const root = rootRequestByOrder(root_artifact, metadata.order);
        const ffi_symbol = providedRootFfiSymbol(root_artifact, root);
        if (std.mem.eql(u8, ffi_symbol, expected_ffi_symbol)) return root_proc;
    }
    return null;
}

fn rootRequestByOrder(
    root_artifact: *const CheckedArtifact.CheckedModuleArtifact,
    order: u32,
) CheckedArtifact.RootRequest {
    for (root_artifact.root_requests.requests) |request| {
        if (request.order == order) return request;
    }
    if (builtin.mode == .Debug) {
        std.debug.panic("glue invariant violated: missing root request order {d}", .{order});
    }
    unreachable;
}

fn providedRootFfiSymbol(
    root_artifact: *const CheckedArtifact.CheckedModuleArtifact,
    root: CheckedArtifact.RootRequest,
) []const u8 {
    const def_idx = switch (root.source) {
        .def => |def| def,
        else => {
            if (builtin.mode == .Debug) {
                std.debug.panic("glue invariant violated: provided export root is not a definition", .{});
            }
            unreachable;
        },
    };
    const top_level = root_artifact.top_level_values.lookupByDef(def_idx) orelse {
        if (builtin.mode == .Debug) {
            std.debug.panic("glue invariant violated: provided export root has no published top-level value", .{});
        }
        unreachable;
    };

    for (root_artifact.provides_requires.provides) |entry| {
        if (entry.source_name == top_level.source_name) {
            return root_artifact.canonical_names.externalSymbolNameText(entry.ffi_symbol);
        }
    }

    if (builtin.mode == .Debug) {
        std.debug.panic("glue invariant violated: provided export root has no published FFI symbol", .{});
    }
    unreachable;
}

fn platformRequiredEntrypointCheckedType(
    artifact: *const CheckedArtifact.CheckedModuleArtifact,
    declaration: CheckedArtifact.PlatformRequiredDeclaration,
) CheckedArtifact.CheckedTypeId {
    if (artifact.platform_required_bindings.lookupByRequiredIndex(declaration.requires_idx)) |binding| {
        if (binding.declaration != declaration.id) {
            glueInvariant("platform-required binding disagreed with declaration id", .{});
        }
        const relation = artifact.platform_requirement_relations.lookupByRelationId(binding.checked_relation) orelse
            glueInvariant("platform-required binding has no checked relation row", .{});
        if (relation.declaration != declaration.id or relation.requires_idx != declaration.requires_idx) {
            glueInvariant("platform-required relation disagreed with declaration", .{});
        }
        return relation.requested_source_ty_payload;
    }

    const scheme = artifact.checked_types.schemeForKey(declaration.declared_source_ty) orelse
        glueInvariant("platform-required declaration has no checked type scheme", .{});
    return scheme.root;
}

fn argLayoutsForProc(
    allocator: Allocator,
    store: *const lir.LirStore,
    proc_id: lir.LirProcSpecId,
) Allocator.Error![]layout.Idx {
    const proc = store.getProcSpec(proc_id);
    const arg_ids = store.getLocalSpan(proc.args);
    const arg_layouts = try allocator.alloc(layout.Idx, arg_ids.len);
    errdefer allocator.free(arg_layouts);

    for (0..arg_ids.len) |i| {
        const local_id = GuardedList.at(arg_ids, i);
        arg_layouts[i] = store.getLocal(local_id).layout_idx;
    }

    return arg_layouts;
}

/// Information extracted from a platform header for glue generation.
pub const PlatformHeaderInfo = struct {
    requires_entries: []RequiresEntry,
    hosted_entries: []HostedEntry,
    type_aliases: [][]const u8,

    pub const RequiresEntry = struct {
        name: []const u8,
        type_str: []const u8,
        stub_expr: []const u8,
    };

    pub const HostedEntry = struct {
        key: []const u8,
        ffi_symbol: []const u8,
    };

    pub const ProvidesEntry = struct {
        name: []const u8,
        ffi_symbol: []const u8,
    };

    pub fn deinit(self: *const PlatformHeaderInfo, gpa: std.mem.Allocator) void {
        deinitPlatformRequiresEntries(gpa, self.requires_entries);
        deinitPlatformHostedEntries(gpa, self.hosted_entries);
        deinitPlatformTypeAliases(gpa, self.type_aliases);
    }
};

fn deinitPlatformRequiresEntries(gpa: std.mem.Allocator, entries: []const PlatformHeaderInfo.RequiresEntry) void {
    for (entries) |entry| {
        gpa.free(entry.name);
        gpa.free(entry.type_str);
        gpa.free(entry.stub_expr);
    }
    gpa.free(entries);
}

fn deinitPlatformHostedEntries(gpa: std.mem.Allocator, entries: []const PlatformHeaderInfo.HostedEntry) void {
    for (entries) |entry| {
        gpa.free(entry.key);
        gpa.free(entry.ffi_symbol);
    }
    gpa.free(entries);
}

fn deinitPlatformTypeAliases(gpa: std.mem.Allocator, aliases: []const []const u8) void {
    for (aliases) |alias_name| {
        gpa.free(alias_name);
    }
    gpa.free(aliases);
}

fn hostedEntryLocalNameAlloc(
    gpa: Allocator,
    env: *ModuleEnv,
    ast: *const parse.AST,
    entry: parse.AST.SymbolMapEntry,
) Allocator.Error!?[]const u8 {
    const direct = ast.tokens.resolveIdentifier(entry.func) orelse return null;
    const module_tok = entry.module orelse return try gpa.dupe(u8, env.common.getIdent(direct));
    if (entry.func == module_tok + 1) return try gpa.dupe(u8, env.common.getIdent(direct));

    var text = std.ArrayList(u8).empty;
    defer text.deinit(gpa);

    var tok = module_tok + 1;
    while (tok <= entry.func) : (tok += 1) {
        const segment = ast.tokens.resolveIdentifier(tok) orelse return null;
        if (text.items.len != 0) try text.append(gpa, '.');
        try text.appendSlice(gpa, env.common.getIdent(segment));
    }

    return try text.toOwnedSlice(gpa);
}

fn hostedEntryKeyAllocFromAst(
    gpa: Allocator,
    env: *ModuleEnv,
    ast: *const parse.AST,
    entry: parse.AST.SymbolMapEntry,
) Allocator.Error!?[]const u8 {
    const local_name = (try hostedEntryLocalNameAlloc(gpa, env, ast, entry)) orelse return null;
    defer gpa.free(local_name);

    const module_name = if (entry.module) |module_tok| blk: {
        const module_ident = ast.tokens.resolveIdentifier(module_tok) orelse return null;
        break :blk env.common.getIdent(module_ident);
    } else "";

    return try hostedKeyAlloc(gpa, module_name, local_name);
}

/// Parse a platform header to extract requires entries and validate it's a platform file.
fn parsePlatformHeader(gpa: Allocator, platform_path: []const u8, std_io: std.Io) (Allocator.Error || error{ FileNotFound, ParseFailed, NotPlatformFile })!PlatformHeaderInfo {
    // Read source file
    var source = std.Io.Dir.cwd().readFileAlloc(std_io, platform_path, gpa, .unlimited) catch |err| switch (err) {
        error.FileNotFound => return error.FileNotFound,
        else => return error.ParseFailed,
    };
    source = base.source_utils.normalizeLineEndingsRealloc(gpa, source) catch {
        gpa.free(source);
        return error.OutOfMemory;
    };
    defer gpa.free(source);

    // Get module name from path
    const module_name = std.fs.path.stem(platform_path);

    // Create ModuleEnv
    var env = ModuleEnv.init(gpa, source) catch return error.OutOfMemory;
    defer env.deinit();

    env.common.source = source;
    env.module_name = module_name;
    env.common.calcLineStarts(gpa) catch return error.OutOfMemory;

    // Parse the source code
    var parse_ast = parse.file(gpa, &env.common) catch return error.ParseFailed;
    defer parse_ast.deinit();

    // Get the file header
    const file_node = parse_ast.store.getFile();
    const header = parse_ast.store.getHeader(file_node.header);

    // Check if this is a platform file
    switch (header) {
        .platform => |platform_header| {
            // Extract requires entries
            const requires_entries_ast = parse_ast.store.requiresEntrySlice(platform_header.requires_entries);
            var requires_entries = std.ArrayList(PlatformHeaderInfo.RequiresEntry).empty;
            errdefer {
                for (requires_entries.items) |entry| {
                    gpa.free(entry.name);
                    gpa.free(entry.type_str);
                    gpa.free(entry.stub_expr);
                }
                requires_entries.deinit(gpa);
            }

            var hosted_entries = std.ArrayList(PlatformHeaderInfo.HostedEntry).empty;
            errdefer {
                for (hosted_entries.items) |entry| {
                    gpa.free(entry.key);
                    gpa.free(entry.ffi_symbol);
                }
                hosted_entries.deinit(gpa);
            }

            const hosted_entries_ast = parse_ast.store.symbolMapEntrySlice(platform_header.hosted);
            for (hosted_entries_ast) |entry_idx| {
                const entry = parse_ast.store.getSymbolMapEntry(entry_idx);
                const hosted_key = (try hostedEntryKeyAllocFromAst(gpa, &env, parse_ast, entry)) orelse continue;
                const ffi_symbol = gpa.dupe(u8, parse_ast.resolve(entry.symbol)) catch |err| {
                    gpa.free(hosted_key);
                    return err;
                };
                hosted_entries.append(gpa, .{
                    .key = hosted_key,
                    .ffi_symbol = ffi_symbol,
                }) catch |err| {
                    gpa.free(hosted_key);
                    gpa.free(ffi_symbol);
                    return err;
                };
            }

            // Use a hash set to deduplicate type aliases across requires entries
            var type_alias_set = std.StringHashMap(void).init(gpa);
            defer type_alias_set.deinit();

            for (requires_entries_ast) |entry_idx| {
                const entry = parse_ast.store.getRequiresEntry(entry_idx);

                // Extract type aliases from for-clause
                const type_aliases_ast = parse_ast.store.forClauseTypeAliasSlice(entry.type_aliases);
                for (type_aliases_ast) |alias_idx| {
                    const alias = parse_ast.store.getForClauseTypeAlias(alias_idx);
                    if (parse_ast.tokens.resolveIdentifier(alias.alias_name)) |ident_idx| {
                        const alias_name = env.common.getIdent(ident_idx);
                        if (!type_alias_set.contains(alias_name)) {
                            try type_alias_set.put(try gpa.dupe(u8, alias_name), {});
                        }
                    }
                }

                if (parse_ast.tokens.resolveIdentifier(entry.entrypoint_name)) |ident_idx| {
                    const name = env.common.getIdent(ident_idx);

                    // Format type annotation to string
                    var type_buf = std.ArrayList(u8).empty;
                    defer type_buf.deinit(gpa);

                    try printTypeAnnoToBuf(gpa, &env, parse_ast, entry.type_anno, &type_buf);

                    // Generate stub expression from type annotation
                    var stub_buf = std.ArrayList(u8).empty;
                    defer stub_buf.deinit(gpa);

                    try generateStubExprFromTypeAnno(gpa, &env, parse_ast, entry.type_anno, &stub_buf);

                    try requires_entries.append(gpa, .{
                        .name = try gpa.dupe(u8, name),
                        .type_str = try type_buf.toOwnedSlice(gpa),
                        .stub_expr = try stub_buf.toOwnedSlice(gpa),
                    });
                }
            }

            // Convert type alias set to owned slice
            var type_aliases = std.ArrayList([]const u8).empty;
            errdefer {
                for (type_aliases.items) |alias_name| {
                    gpa.free(alias_name);
                }
                type_aliases.deinit(gpa);
            }
            var alias_iter = type_alias_set.keyIterator();
            while (alias_iter.next()) |key| {
                try type_aliases.append(gpa, key.*);
            }

            const requires_entries_owned = try requires_entries.toOwnedSlice(gpa);
            errdefer deinitPlatformRequiresEntries(gpa, requires_entries_owned);
            const hosted_entries_owned = try hosted_entries.toOwnedSlice(gpa);
            errdefer deinitPlatformHostedEntries(gpa, hosted_entries_owned);
            const type_aliases_owned = try type_aliases.toOwnedSlice(gpa);
            errdefer deinitPlatformTypeAliases(gpa, type_aliases_owned);

            return PlatformHeaderInfo{
                .requires_entries = requires_entries_owned,
                .hosted_entries = hosted_entries_owned,
                .type_aliases = type_aliases_owned,
            };
        },
        else => return error.NotPlatformFile,
    }
}

/// Collected module type information for glue generation
const CollectedModuleTypeInfo = struct {
    name: []const u8,
    main_type: []const u8,
    functions: std.ArrayList(CollectedFunctionInfo),
    hosted_functions: std.ArrayList(CollectedHostedFunctionInfo),

    const CollectedFunctionInfo = struct {
        name: []const u8,
        type_str: []const u8,
    };

    const CollectedRecordFieldInfo = struct {
        name: []const u8,
        type_str: []const u8,
    };

    const CollectedHostedFunctionInfo = struct {
        index: usize,
        ffi_symbol: []const u8,
        name: []const u8,
        type_str: []const u8,
        arg_fields: []const CollectedRecordFieldInfo,
        ret_fields: []const CollectedRecordFieldInfo,
        arg_type_ids: []const u64,
        ret_type_id: u64,
    };

    fn deinit(self: *CollectedModuleTypeInfo, gpa: std.mem.Allocator) void {
        gpa.free(self.name);
        gpa.free(self.main_type);
        for (self.functions.items) |f| {
            gpa.free(f.name);
            gpa.free(f.type_str);
        }
        self.functions.deinit(gpa);
        for (self.hosted_functions.items) |h| {
            gpa.free(h.ffi_symbol);
            gpa.free(h.name);
            gpa.free(h.type_str);
            for (h.arg_fields) |field| {
                gpa.free(field.name);
                gpa.free(field.type_str);
            }
            gpa.free(h.arg_fields);
            for (h.ret_fields) |field| {
                gpa.free(field.name);
                gpa.free(field.type_str);
            }
            gpa.free(h.ret_fields);
            if (h.arg_type_ids.len > 0) gpa.free(h.arg_type_ids);
        }
        self.hosted_functions.deinit(gpa);
    }
};

/// Internal representation of a collected type for the type table.
const CollectedLayoutFacts = struct {
    layout_idx: layout.Idx,
    size_32: u64,
    alignment_32: u64,
    size_64: u64,
    alignment_64: u64,
};

const CollectedTypeRepr = union(enum) {
    bool_: CollectedLayoutFacts,
    box: struct { inner_id: u64, layout: CollectedLayoutFacts },
    dec: CollectedLayoutFacts,
    f32_: CollectedLayoutFacts,
    f64_: CollectedLayoutFacts,
    i8_: CollectedLayoutFacts,
    i16_: CollectedLayoutFacts,
    i32_: CollectedLayoutFacts,
    i64_: CollectedLayoutFacts,
    i128_: CollectedLayoutFacts,
    u8_: CollectedLayoutFacts,
    u16_: CollectedLayoutFacts,
    u32_: CollectedLayoutFacts,
    u64_: CollectedLayoutFacts,
    u128_: CollectedLayoutFacts,
    str_: CollectedLayoutFacts,
    unit: CollectedLayoutFacts,
    list: struct { elem_id: u64, layout: CollectedLayoutFacts },
    function: struct { arg_ids: []const u64, ret_id: u64, layout: CollectedLayoutFacts },
    record: struct { name: []const u8, anonymous: bool, fields: []const CollectedRecordField, layout: CollectedLayoutFacts },
    tag_union: struct { name: []const u8, tags: []const CollectedTagInfo, layout: CollectedLayoutFacts },
    unknown: struct { name: []const u8, layout: CollectedLayoutFacts },
};

const CollectedRecordField = struct {
    name: []const u8,
    type_id: u64,
    original_index: u64,
    /// True for an unnamed nominal-record padding field (`_` / `_name`). The
    /// emitters render it as a fixed-size byte array (`[n]u8` in Zig,
    /// `uint8_t name[n]` in C) whose per-width byte counts come from the
    /// committed `AbiFieldLayout.size32`/`size64`, and skip it for refcount
    /// helpers. Zero-sized unnamed fields are layout markers only and are not
    /// collected. `type_id` is unused for padding fields.
    is_padding: bool = false,
};

const CollectedTagInfo = struct {
    name: []const u8,
    payload_ids: []const u64,
};

/// Source checked type for one public glue type id.
///
/// Authoritative sources:
/// - `CheckedModuleArtifact` owns checked type ids.
/// - `lir.Program.RequestedLayout` in `src/lir/program.zig` is the post-check
///   boundary that maps those checked ids to committed LIR layout ids.
const CollectedTypeSource = struct {
    artifact: *const CheckedArtifact.CheckedModuleArtifact,
    checked_type: CheckedArtifact.CheckedTypeId,
};

const CollectedAbiSizeAlign = struct {
    size32: u64,
    alignment32: u64,
    size64: u64,
    alignment64: u64,
};

const CollectedAbiFieldLayout = struct {
    name: []const u8,
    type_id: u64,
    original_index: u64,
    is_padding: bool,
    offset32: u64,
    offset64: u64,
    size32: u64,
    alignment32: u64,
    size64: u64,
    alignment64: u64,
};

const CollectedAbiTagLayout = struct {
    name: []const u8,
    payload_ids: []const u64,
    payload_fields: []const CollectedAbiFieldLayout,
    discriminant: u64,
    payload_size32: u64,
    payload_alignment32: u64,
    payload_size64: u64,
    payload_alignment64: u64,
};

const CollectedAbiLayoutDetails = union(enum) {
    builtin,
    record: []const CollectedAbiFieldLayout,
    tag_union: struct {
        discriminant_size: u64,
        discriminant_offset32: u64,
        discriminant_offset64: u64,
        tags: []const CollectedAbiTagLayout,
    },
};

/// Exact ABI layout metadata emitted to Roc glue.
///
/// Authoritative sources:
/// - `src/layout/store.zig` owns committed sizes, alignments, offsets, tag
///   payload layouts, discriminant offsets, and refcountedness.
/// - `design.md` "Layout Selection" forbids glue from rediscovering those facts.
const CollectedAbiLayout = struct {
    size_align: CollectedAbiSizeAlign,
    contains_refcounted: bool,
    details: CollectedAbiLayoutDetails,
};

const CollectedTypeInfo = struct {
    repr: CollectedTypeRepr,
    source: ?CollectedTypeSource,
    abi: ?CollectedAbiLayout = null,
};

/// Maps checked artifact keys to artifacts. Populated once from the compiled
/// module list before collection so nominal representation refs can resolve
/// their declaration owners directly.
const ArtifactKeyMap = checked_artifact_layout_resolver.ArtifactMap;

const TypeTableKey = struct {
    artifact_key: CheckedArtifact.CheckedModuleArtifactKey,
    checked_type: CheckedArtifact.CheckedTypeId,
};

/// Builds a type table from artifact-owned checked type payloads.
/// Error set for building the glue type table: allocation failures plus a
/// glue-visible type whose committed memory layout cannot be determined because
/// it still holds an unresolved (flex/rigid) type variable by value. The latter
/// is a user-facing glue error, not an internal invariant.
const TypeTableError = Allocator.Error || error{UnresolvedByValue};

const TypeTable = struct {
    entries: std.ArrayList(CollectedTypeInfo),
    var_map: std.AutoHashMap(TypeTableKey, u64),
    /// Declaration-formal -> application-argument substitutions for the nominal
    /// backing opening(s) in flight (issue #9983 / Option A): a nominal
    /// application carries no backing, so its ABI structure is the declaration's
    /// backing TEMPLATE with formals replaced by the application's args. Empty
    /// unless a backing is being opened. Bindings accumulate across nested opens.
    template_bindings: std.AutoHashMap(TypeTableKey, BoundSource),
    /// Non-zero while opening a backing; identifies the current open so its
    /// entries memoize separately from the global `var_map` and from sibling
    /// opens at different arguments (which convert to different structures).
    open_ctx: u32 = 0,
    next_open_ctx: u32 = 0,
    /// Stable open-context ids keyed by open identity (declaration + resolved
    /// args), so the same concrete backing opened at multiple use sites shares
    /// one context and its per-open entries deduplicate (a monotonic counter
    /// would mint duplicate anonymous structs that the ABI emitter can't
    /// reconcile).
    open_ctxs: std.ArrayList(OpenCtxEntry) = .empty,
    /// Per-open memo (keyed by open ctx + checked type) providing dedup for
    /// non-nominal declaration-space types while `var_map` is bypassed.
    open_memo: std.AutoHashMap(OpenMemoKey, u64),
    /// Nominal backings currently being opened, so a recursive backing
    /// reference to the same (declaration, args) resolves to the in-progress
    /// type-table entry instead of opening forever.
    active_opens: std.ArrayList(ActiveOpen) = .empty,
    gpa: std.mem.Allocator,
    layouts: *const layout.Store,
    layout_resolver: *CheckedArtifactLayoutResolver,
    /// Lookup from checked artifact key to artifact. Borrowed; not owned by the
    /// type table.
    artifacts_by_key: *const ArtifactKeyMap,
    /// User-facing message for the first glue-visible type found to have no
    /// committed memory layout because it holds an unresolved (flex/rigid) type
    /// variable by value. Set alongside `error.UnresolvedByValue`; owned here
    /// and freed in `deinit`.
    unresolved_error: ?[]const u8 = null,

    /// A checked type resolved through the active formal bindings: the artifact
    /// and checked type to actually convert (an application argument for a
    /// declaration formal, or the input unchanged).
    const BoundSource = struct {
        artifact: *const CheckedArtifact.CheckedModuleArtifact,
        checked_type: CheckedArtifact.CheckedTypeId,
    };

    const BoundRecordField = struct {
        artifact: *const CheckedArtifact.CheckedModuleArtifact,
        field: CheckedArtifact.CheckedRecordField,
    };

    const HostedFunctionTypeMetadata = struct {
        arg_fields: []const CollectedModuleTypeInfo.CollectedRecordFieldInfo,
        ret_fields: []const CollectedModuleTypeInfo.CollectedRecordFieldInfo,
        arg_type_ids: []const u64,
        ret_type_id: u64,
    };

    const OpenMemoKey = struct {
        ctx: u32,
        artifact_key: CheckedArtifact.CheckedModuleArtifactKey,
        checked_type: CheckedArtifact.CheckedTypeId,
    };

    const ActiveOpen = struct {
        decl_artifact_key: CheckedArtifact.CheckedModuleArtifactKey,
        source_statement: u32,
        arg_keys: []const TypeTableKey,
        entry_idx: u64,
    };

    const OpenCtxEntry = struct {
        decl_artifact_key: CheckedArtifact.CheckedModuleArtifactKey,
        source_statement: u32,
        arg_keys: []const TypeTableKey,
        ctx: u32,
    };

    fn init(
        gpa: std.mem.Allocator,
        artifacts_by_key: *const ArtifactKeyMap,
        layouts: *const layout.Store,
        layout_resolver: *CheckedArtifactLayoutResolver,
    ) TypeTable {
        return .{
            .entries = std.ArrayList(CollectedTypeInfo).empty,
            .var_map = std.AutoHashMap(TypeTableKey, u64).init(gpa),
            .template_bindings = std.AutoHashMap(TypeTableKey, BoundSource).init(gpa),
            .open_memo = std.AutoHashMap(OpenMemoKey, u64).init(gpa),
            .active_opens = .empty,
            .open_ctxs = .empty,
            .gpa = gpa,
            .layouts = layouts,
            .layout_resolver = layout_resolver,
            .artifacts_by_key = artifacts_by_key,
        };
    }

    fn deinit(self: *TypeTable) void {
        if (self.unresolved_error) |msg| self.gpa.free(msg);
        for (self.entries.items) |entry| {
            self.freeEntry(entry);
        }
        self.entries.deinit(self.gpa);
        self.var_map.deinit();
        self.template_bindings.deinit();
        self.open_memo.deinit();
        for (self.active_opens.items) |open| self.gpa.free(open.arg_keys);
        self.active_opens.deinit(self.gpa);
        for (self.open_ctxs.items) |entry| self.gpa.free(entry.arg_keys);
        self.open_ctxs.deinit(self.gpa);
    }

    /// Find or assign a stable open-context id for an open identity.
    fn ctxForOpen(
        self: *TypeTable,
        decl_artifact_key: CheckedArtifact.CheckedModuleArtifactKey,
        source_statement: u32,
        arg_keys: []const TypeTableKey,
    ) Allocator.Error!u32 {
        for (self.open_ctxs.items) |entry| {
            if (!checkedArtifactKeysEqual(entry.decl_artifact_key, decl_artifact_key)) continue;
            if (entry.source_statement != source_statement) continue;
            if (!typeTableKeysEqual(entry.arg_keys, arg_keys)) continue;
            return entry.ctx;
        }
        self.next_open_ctx += 1;
        const ctx = self.next_open_ctx;
        const owned = try self.gpa.dupe(TypeTableKey, arg_keys);
        errdefer self.gpa.free(owned);
        try self.open_ctxs.append(self.gpa, .{
            .decl_artifact_key = decl_artifact_key,
            .source_statement = source_statement,
            .arg_keys = owned,
            .ctx = ctx,
        });
        return ctx;
    }

    /// Resolve a checked type through the active formal bindings.
    fn substituteFormal(
        self: *const TypeTable,
        artifact: *const CheckedArtifact.CheckedModuleArtifact,
        checked_type: CheckedArtifact.CheckedTypeId,
    ) BoundSource {
        var cur = BoundSource{ .artifact = artifact, .checked_type = checked_type };
        // Follow the binding chain: a nested opening can bind a formal to an
        // outer formal, which is bound in turn to a concrete argument.
        while (self.template_bindings.count() != 0) {
            const key = TypeTableKey{ .artifact_key = cur.artifact.key, .checked_type = cur.checked_type };
            const bound = self.template_bindings.get(key) orelse break;
            cur = bound;
        }
        return cur;
    }

    fn bindNominalFormals(
        self: *TypeTable,
        artifact: *const CheckedArtifact.CheckedModuleArtifact,
        nominal: CheckedArtifact.CheckedNominalType,
        lookup: NominalDeclarationLookup,
        saved: []?BoundSource,
        bound_formals: []bool,
    ) Allocator.Error!void {
        const formals = lookup.declaration.formalArgs(&lookup.artifact.checked_types);
        if (formals.len != nominal.args.len) glueInvariant("nominal application arity disagreed with its declaration in glue metadata opening", .{});

        for (formals, nominal.args, saved, bound_formals) |formal, arg, *slot, *is_bound| {
            const fkey = TypeTableKey{ .artifact_key = lookup.artifact.key, .checked_type = formal };
            const resolved = self.substituteFormal(artifact, arg);
            if (checkedArtifactKeysEqual(resolved.artifact.key, fkey.artifact_key) and resolved.checked_type == fkey.checked_type) {
                is_bound.* = false;
                continue;
            }
            is_bound.* = true;
            slot.* = self.template_bindings.get(fkey);
            try self.template_bindings.put(fkey, .{ .artifact = resolved.artifact, .checked_type = resolved.checked_type });
        }
    }

    fn restoreNominalFormals(
        self: *TypeTable,
        lookup: NominalDeclarationLookup,
        saved: []const ?BoundSource,
        bound_formals: []const bool,
    ) void {
        const formals = lookup.declaration.formalArgs(&lookup.artifact.checked_types);
        for (formals, saved, bound_formals) |formal, slot, is_bound| {
            if (!is_bound) continue;
            const fkey = TypeTableKey{ .artifact_key = lookup.artifact.key, .checked_type = formal };
            if (slot) |prev| {
                self.template_bindings.put(fkey, prev) catch unreachable;
            } else {
                _ = self.template_bindings.remove(fkey);
            }
        }
    }

    fn collectHostedFunctionMetadata(
        self: *TypeTable,
        artifact: *const CheckedArtifact.CheckedModuleArtifact,
        checked_type: CheckedArtifact.CheckedTypeId,
    ) TypeTableError!?HostedFunctionTypeMetadata {
        const src = self.substituteFormal(artifact, checked_type);
        return switch (checkedTypePayload(src.artifact, src.checked_type)) {
            .function => |func| try self.metadataForFunctionPayload(src.artifact, func),
            .alias => |alias| try self.collectHostedFunctionMetadata(src.artifact, alias.backing),
            .nominal => |nominal| blk: {
                const lookup = self.nominalDeclarationFor(src.artifact, nominal) orelse break :blk null;
                const saved = try self.gpa.alloc(?BoundSource, nominal.args.len);
                defer self.gpa.free(saved);
                const bound_formals = try self.gpa.alloc(bool, nominal.args.len);
                defer self.gpa.free(bound_formals);
                try self.bindNominalFormals(src.artifact, nominal, lookup, saved, bound_formals);
                defer self.restoreNominalFormals(lookup, saved, bound_formals);
                break :blk try self.collectHostedFunctionMetadata(lookup.artifact, lookup.declaration.backing);
            },
            else => null,
        };
    }

    fn metadataForFunctionPayload(
        self: *TypeTable,
        artifact: *const CheckedArtifact.CheckedModuleArtifact,
        func: CheckedArtifact.CheckedFunctionType,
    ) TypeTableError!HostedFunctionTypeMetadata {
        const ret_fields = try self.extractRecordFieldsBound(artifact, func.ret);
        errdefer self.freeRecordFieldInfo(ret_fields);

        var arg_fields: []const CollectedModuleTypeInfo.CollectedRecordFieldInfo = &.{};
        errdefer self.freeRecordFieldInfo(arg_fields);
        if (func.args.len == 1) {
            arg_fields = try self.extractRecordFieldsBound(artifact, func.args[0]);
        }

        const ret_type_id = try self.getOrInsert(artifact, func.ret);
        var arg_type_ids: []const u64 = &.{};
        errdefer if (arg_type_ids.len > 0) self.gpa.free(arg_type_ids);
        if (func.args.len > 0) {
            const ids = try self.gpa.alloc(u64, func.args.len);
            for (func.args, 0..) |arg, i| {
                ids[i] = try self.getOrInsert(artifact, arg);
            }
            arg_type_ids = ids;
        }

        return .{
            .arg_fields = arg_fields,
            .ret_fields = ret_fields,
            .arg_type_ids = arg_type_ids,
            .ret_type_id = ret_type_id,
        };
    }

    fn freeRecordFieldInfo(self: *TypeTable, fields: []const CollectedModuleTypeInfo.CollectedRecordFieldInfo) void {
        for (fields) |field| {
            self.gpa.free(field.name);
            self.gpa.free(field.type_str);
        }
        if (fields.len > 0) self.gpa.free(fields);
    }

    fn extractRecordFieldsBound(
        self: *TypeTable,
        artifact: *const CheckedArtifact.CheckedModuleArtifact,
        checked_type: CheckedArtifact.CheckedTypeId,
    ) Allocator.Error![]const CollectedModuleTypeInfo.CollectedRecordFieldInfo {
        var fields = std.ArrayList(BoundRecordField).empty;
        defer fields.deinit(self.gpa);
        if (!(try self.collectRecordFieldsForRootBound(artifact, checked_type, &fields))) {
            return &[_]CollectedModuleTypeInfo.CollectedRecordFieldInfo{};
        }

        var indices = try self.gpa.alloc(usize, fields.items.len);
        defer self.gpa.free(indices);
        for (0..fields.items.len) |i| indices[i] = i;

        const SortCtx = struct {
            fields: []const BoundRecordField,

            pub fn lessThan(ctx: @This(), a: usize, b: usize) bool {
                return std.mem.lessThan(
                    u8,
                    ctx.fields[a].artifact.canonical_names.recordFieldLabelText(ctx.fields[a].field.name),
                    ctx.fields[b].artifact.canonical_names.recordFieldLabelText(ctx.fields[b].field.name),
                );
            }
        };
        std.mem.sort(usize, indices, SortCtx{ .fields = fields.items }, SortCtx.lessThan);

        var result_list = std.ArrayList(CollectedModuleTypeInfo.CollectedRecordFieldInfo).empty;
        errdefer {
            for (result_list.items) |item| {
                self.gpa.free(item.name);
                self.gpa.free(item.type_str);
            }
            result_list.deinit(self.gpa);
        }
        for (indices) |idx| {
            const entry = fields.items[idx];
            const field = entry.field;
            const name = try self.gpa.dupe(u8, entry.artifact.canonical_names.recordFieldLabelText(field.name));
            errdefer self.gpa.free(name);
            const type_str = try self.typeStringAllocBound(entry.artifact, field.ty);
            errdefer self.gpa.free(type_str);
            try result_list.append(self.gpa, .{
                .name = name,
                .type_str = type_str,
            });
        }
        return result_list.toOwnedSlice(self.gpa);
    }

    fn collectRecordFieldsForRootBound(
        self: *TypeTable,
        artifact: *const CheckedArtifact.CheckedModuleArtifact,
        checked_type: CheckedArtifact.CheckedTypeId,
        fields: *std.ArrayList(BoundRecordField),
    ) Allocator.Error!bool {
        const src = self.substituteFormal(artifact, checked_type);
        switch (checkedTypePayload(src.artifact, src.checked_type)) {
            .alias => |alias| return try self.collectRecordFieldsForRootBound(src.artifact, alias.backing, fields),
            .nominal => |nominal| {
                const lookup = self.nominalDeclarationFor(src.artifact, nominal) orelse return false;
                const saved = try self.gpa.alloc(?BoundSource, nominal.args.len);
                defer self.gpa.free(saved);
                const bound_formals = try self.gpa.alloc(bool, nominal.args.len);
                defer self.gpa.free(bound_formals);
                try self.bindNominalFormals(src.artifact, nominal, lookup, saved, bound_formals);
                defer self.restoreNominalFormals(lookup, saved, bound_formals);
                return try self.collectRecordFieldsForRootBound(lookup.artifact, lookup.declaration.backing, fields);
            },
            .record => |record| {
                for (record.fields) |field| try fields.append(self.gpa, .{ .artifact = src.artifact, .field = field });
                return try self.collectRecordFieldsForRootBound(src.artifact, record.ext, fields);
            },
            .record_unbound => |unbound| {
                for (unbound) |field| try fields.append(self.gpa, .{ .artifact = src.artifact, .field = field });
                return true;
            },
            .empty_record => return true,
            else => return false,
        }
    }

    fn typeStringAllocBound(
        self: *TypeTable,
        artifact: *const CheckedArtifact.CheckedModuleArtifact,
        checked_type: CheckedArtifact.CheckedTypeId,
    ) Allocator.Error![]const u8 {
        var buf = std.ArrayList(u8).empty;
        errdefer buf.deinit(self.gpa);
        var active = std.AutoHashMap(TypeTableKey, void).init(self.gpa);
        defer active.deinit();
        try self.writeTypeStringBound(artifact, checked_type, &buf, &active);
        return buf.toOwnedSlice(self.gpa);
    }

    fn writeTypeStringBound(
        self: *TypeTable,
        artifact: *const CheckedArtifact.CheckedModuleArtifact,
        checked_type: CheckedArtifact.CheckedTypeId,
        buf: *std.ArrayList(u8),
        active: *std.AutoHashMap(TypeTableKey, void),
    ) Allocator.Error!void {
        const src = self.substituteFormal(artifact, checked_type);
        const key = TypeTableKey{ .artifact_key = src.artifact.key, .checked_type = src.checked_type };
        if (active.contains(key)) {
            try buf.appendSlice(self.gpa, "<cycle>");
            return;
        }
        try active.put(key, {});
        defer _ = active.remove(key);

        switch (checkedTypePayload(src.artifact, src.checked_type)) {
            .pending => glueInvariant("pending checked type reached glue bound type string", .{}),
            .err => glueInvariant("erroneous checked type reached glue bound type string", .{}),
            .flex => try buf.appendSlice(self.gpa, "flex"),
            .rigid => try buf.appendSlice(self.gpa, "rigid"),
            .alias => |alias| try self.writeTypeStringBound(src.artifact, alias.backing, buf, active),
            .record => |record| try self.writeRecordTypeStringBound(src.artifact, record.fields, record.ext, buf, active),
            .record_unbound => |fields| try self.writeRecordTypeStringBound(src.artifact, fields, null, buf, active),
            .tuple => |items| try self.writeTupleTypeStringBound(src.artifact, items, buf, active),
            .nominal => |nominal| try self.writeNominalTypeStringBound(src.artifact, nominal, buf, active),
            .function => |func| try self.writeFunctionTypeStringBound(src.artifact, func, buf, active),
            .empty_record => try buf.appendSlice(self.gpa, "{}"),
            .tag_union => |tag_union| try self.writeTagUnionTypeStringBound(src.artifact, tag_union.tags, tag_union.ext, buf, active),
            .empty_tag_union => try buf.appendSlice(self.gpa, "[]"),
        }
    }

    fn writeNominalTypeStringBound(
        self: *TypeTable,
        artifact: *const CheckedArtifact.CheckedModuleArtifact,
        nominal: CheckedArtifact.CheckedNominalType,
        buf: *std.ArrayList(u8),
        active: *std.AutoHashMap(TypeTableKey, void),
    ) Allocator.Error!void {
        const name = TypeTable.getTypeDisplayName(artifact.canonical_names.typeNameText(nominal.name));
        try buf.appendSlice(self.gpa, name);
        if (nominal.args.len == 0) return;
        try buf.append(self.gpa, '(');
        for (nominal.args, 0..) |arg, i| {
            if (i > 0) try buf.appendSlice(self.gpa, ", ");
            try self.writeTypeStringBound(artifact, arg, buf, active);
        }
        try buf.append(self.gpa, ')');
    }

    fn writeFunctionTypeStringBound(
        self: *TypeTable,
        artifact: *const CheckedArtifact.CheckedModuleArtifact,
        func: CheckedArtifact.CheckedFunctionType,
        buf: *std.ArrayList(u8),
        active: *std.AutoHashMap(TypeTableKey, void),
    ) Allocator.Error!void {
        if (func.args.len == 0) {
            try buf.appendSlice(self.gpa, "{}");
        } else {
            for (func.args, 0..) |arg, i| {
                if (i > 0) try buf.appendSlice(self.gpa, ", ");
                try self.writeTypeStringBound(artifact, arg, buf, active);
            }
        }
        try buf.appendSlice(self.gpa, if (func.kind == .effectful) " => " else " -> ");
        try self.writeTypeStringBound(artifact, func.ret, buf, active);
    }

    fn writeRecordTypeStringBound(
        self: *TypeTable,
        artifact: *const CheckedArtifact.CheckedModuleArtifact,
        fields: []const CheckedArtifact.CheckedRecordField,
        ext: ?CheckedArtifact.CheckedTypeId,
        buf: *std.ArrayList(u8),
        active: *std.AutoHashMap(TypeTableKey, void),
    ) Allocator.Error!void {
        var all_fields = std.ArrayList(BoundRecordField).empty;
        defer all_fields.deinit(self.gpa);
        for (fields) |field| try all_fields.append(self.gpa, .{ .artifact = artifact, .field = field });
        if (ext) |ext_id| _ = try self.collectRecordFieldsForRootBound(artifact, ext_id, &all_fields);

        if (all_fields.items.len == 0) {
            try buf.appendSlice(self.gpa, "{}");
            return;
        }

        var indices = try self.gpa.alloc(usize, all_fields.items.len);
        defer self.gpa.free(indices);
        for (0..all_fields.items.len) |i| indices[i] = i;
        const SortCtx = struct {
            fields: []const BoundRecordField,

            pub fn lessThan(ctx: @This(), a: usize, b: usize) bool {
                return std.mem.lessThan(
                    u8,
                    ctx.fields[a].artifact.canonical_names.recordFieldLabelText(ctx.fields[a].field.name),
                    ctx.fields[b].artifact.canonical_names.recordFieldLabelText(ctx.fields[b].field.name),
                );
            }
        };
        std.mem.sort(usize, indices, SortCtx{ .fields = all_fields.items }, SortCtx.lessThan);

        try buf.appendSlice(self.gpa, "{ ");
        for (indices, 0..) |src_idx, i| {
            if (i > 0) try buf.appendSlice(self.gpa, ", ");
            const field = all_fields.items[src_idx];
            try buf.appendSlice(self.gpa, field.artifact.canonical_names.recordFieldLabelText(field.field.name));
            try buf.appendSlice(self.gpa, " : ");
            try self.writeTypeStringBound(field.artifact, field.field.ty, buf, active);
        }
        try buf.appendSlice(self.gpa, " }");
    }

    fn writeTupleTypeStringBound(
        self: *TypeTable,
        artifact: *const CheckedArtifact.CheckedModuleArtifact,
        items: []const CheckedArtifact.CheckedTypeId,
        buf: *std.ArrayList(u8),
        active: *std.AutoHashMap(TypeTableKey, void),
    ) Allocator.Error!void {
        try buf.append(self.gpa, '(');
        for (items, 0..) |item, i| {
            if (i > 0) try buf.appendSlice(self.gpa, ", ");
            try self.writeTypeStringBound(artifact, item, buf, active);
        }
        try buf.append(self.gpa, ')');
    }

    fn writeTagUnionTypeStringBound(
        self: *TypeTable,
        artifact: *const CheckedArtifact.CheckedModuleArtifact,
        tags: []const CheckedArtifact.CheckedTag,
        ext: CheckedArtifact.CheckedTypeId,
        buf: *std.ArrayList(u8),
        active: *std.AutoHashMap(TypeTableKey, void),
    ) Allocator.Error!void {
        var all_tags = std.ArrayList(CheckedArtifact.CheckedTag).empty;
        defer all_tags.deinit(self.gpa);
        try appendTagRowTags(self.gpa, artifact, tags, ext, &all_tags);

        try buf.append(self.gpa, '[');
        for (all_tags.items, 0..) |tag, i| {
            if (i > 0) try buf.appendSlice(self.gpa, ", ");
            try buf.appendSlice(self.gpa, artifact.canonical_names.tagLabelText(tag.name));
            const tag_args = tag.argsSlice(&artifact.checked_types);
            if (tag_args.len > 0) {
                try buf.append(self.gpa, '(');
                for (tag_args, 0..) |arg, arg_i| {
                    if (arg_i > 0) try buf.appendSlice(self.gpa, ", ");
                    try self.writeTypeStringBound(artifact, arg, buf, active);
                }
                try buf.append(self.gpa, ')');
            }
        }
        try buf.append(self.gpa, ']');
    }

    /// If a checked type is a non-builtin nominal application with a resolvable
    /// declaration, return the identity (declaration + resolved args) used to
    /// detect recursive backing references during opening. Caller owns arg_keys.
    fn nominalOpenIdentity(
        self: *TypeTable,
        artifact: *const CheckedArtifact.CheckedModuleArtifact,
        checked_type: CheckedArtifact.CheckedTypeId,
    ) Allocator.Error!?struct { decl_artifact_key: CheckedArtifact.CheckedModuleArtifactKey, source_statement: u32, arg_keys: []TypeTableKey } {
        const nominal = switch (checkedTypePayload(artifact, checked_type)) {
            .nominal => |nominal| nominal,
            else => return null,
        };
        if (nominal.builtin != null) return null;
        const lookup = self.nominalDeclarationFor(artifact, nominal) orelse return null;
        const arg_keys = try self.gpa.alloc(TypeTableKey, nominal.args.len);
        errdefer self.gpa.free(arg_keys);
        for (nominal.args, arg_keys) |arg, *arg_key| {
            const resolved = self.substituteFormal(artifact, arg);
            arg_key.* = .{ .artifact_key = resolved.artifact.key, .checked_type = resolved.checked_type };
        }
        return .{
            .decl_artifact_key = lookup.artifact.key,
            .source_statement = lookup.declaration.source_statement,
            .arg_keys = arg_keys,
        };
    }

    fn freeEntry(self: *TypeTable, entry: CollectedTypeInfo) void {
        switch (entry.repr) {
            .record => |rec| {
                for (rec.fields) |field| {
                    self.freeDuped(field.name);
                }
                self.gpa.free(rec.fields);
                self.freeDuped(rec.name);
            },
            .tag_union => |tu| {
                for (tu.tags) |tag| {
                    self.freeDuped(tag.name);
                    self.gpa.free(tag.payload_ids);
                }
                self.gpa.free(tu.tags);
                self.freeDuped(tu.name);
            },
            .function => |func| {
                self.gpa.free(func.arg_ids);
            },
            .unknown => |unknown| {
                self.freeDuped(unknown.name);
            },
            .box,
            .list,
            .bool_,
            .dec,
            .f32_,
            .f64_,
            .i8_,
            .i16_,
            .i32_,
            .i64_,
            .i128_,
            .u8_,
            .u16_,
            .u32_,
            .u64_,
            .u128_,
            .str_,
            .unit,
            => {},
        }
        if (entry.abi) |abi| {
            switch (abi.details) {
                .record => |fields| {
                    self.freeAbiFieldLayouts(fields);
                },
                .tag_union => |tu| {
                    for (tu.tags) |tag| {
                        self.freeDuped(tag.name);
                        self.gpa.free(tag.payload_ids);
                        self.freeAbiFieldLayouts(tag.payload_fields);
                    }
                    if (tu.tags.len > 0) self.gpa.free(tu.tags);
                },
                .builtin => {},
            }
        }
    }

    /// Free a slice that was created with gpa.dupe. Skips empty slices, which
    /// may point into static memory (e.g. the `.unknown = ""` placeholder).
    fn freeDuped(self: *TypeTable, slice: []const u8) void {
        if (slice.len == 0) return;
        self.gpa.free(slice);
    }

    fn freeAbiFieldLayouts(self: *TypeTable, fields: []const CollectedAbiFieldLayout) void {
        for (fields) |field| self.freeDuped(field.name);
        if (fields.len > 0) self.gpa.free(fields);
    }

    fn freeCollectedRecordFieldNames(self: *TypeTable, fields: []const CollectedRecordField) void {
        for (fields) |field| self.freeDuped(field.name);
    }

    fn freeCollectedRecordFields(self: *TypeTable, fields: []const CollectedRecordField, populated: usize) void {
        self.freeCollectedRecordFieldNames(fields[0..populated]);
        self.gpa.free(fields);
    }

    /// Clear the checked-type map when switching modules (checked ids are artifact-local).
    fn clearVarMap(self: *TypeTable) void {
        self.var_map.clearRetainingCapacity();
    }

    fn layoutFactsForIdx(self: *const TypeTable, layout_idx: layout.Idx) CollectedLayoutFacts {
        const layout_value = self.layouts.getLayout(layout_idx);
        const sa32 = self.layouts.layoutSizeAlignAt(layout_value, .u32);
        const sa64 = self.layouts.layoutSizeAlignAt(layout_value, .u64);
        return .{
            .layout_idx = layout_idx,
            .size_32 = sa32.size,
            .alignment_32 = sa32.alignment.toByteUnits(),
            .size_64 = sa64.size,
            .alignment_64 = sa64.alignment.toByteUnits(),
        };
    }

    /// Record the user-facing glue error for a glue-visible type that holds an
    /// unresolved (flex/rigid) type variable by value, naming the type and its
    /// declaring module. Keeps the first message; the whole glue run aborts on
    /// the first such type, so later ones are never inspected.
    fn recordUnresolvedByValue(
        self: *TypeTable,
        artifact: *const CheckedArtifact.CheckedModuleArtifact,
        checked_type: CheckedArtifact.CheckedTypeId,
    ) Allocator.Error!void {
        if (self.unresolved_error != null) return;
        const type_name = try self.typeStringAllocBound(artifact, checked_type);
        defer self.gpa.free(type_name);
        const module_name = artifact.moduleEnvConst().module_name;
        self.unresolved_error = try std.fmt.allocPrint(
            self.gpa,
            "The type `{s}` from module `{s}` still has an unresolved type variable, so it has no committed memory layout and glue cannot generate bindings for it. Give the type a concrete layout (for example, box the value across the host boundary) so its size is known.",
            .{ type_name, module_name },
        );
    }

    fn layoutFactsForCheckedType(
        self: *TypeTable,
        artifact: *const CheckedArtifact.CheckedModuleArtifact,
        checked_type: CheckedArtifact.CheckedTypeId,
    ) TypeTableError!CollectedLayoutFacts {
        const layout_idx = if (self.template_bindings.count() == 0)
            self.layout_resolver.resolve(artifact, checked_type) catch |err| switch (err) {
                error.OutOfMemory => return error.OutOfMemory,
                error.UnresolvedByValue => {
                    try self.recordUnresolvedByValue(artifact, checked_type);
                    return error.UnresolvedByValue;
                },
            }
        else layout_with_bindings: {
            // Under a backing opening, layout facts for backing subtypes must
            // resolve the declaration's formals to the application's args.
            var bindings = std.ArrayList(CheckedArtifactLayoutResolver.FormalArgBinding).empty;
            defer bindings.deinit(self.gpa);
            var it = self.template_bindings.iterator();
            while (it.next()) |entry| {
                const formal_artifact = self.artifacts_by_key.get(entry.key_ptr.artifact_key) orelse continue;
                try bindings.append(self.gpa, .{
                    .formal_artifact = formal_artifact,
                    .formal = entry.key_ptr.checked_type,
                    .arg_artifact = entry.value_ptr.artifact,
                    .arg = entry.value_ptr.checked_type,
                });
            }
            break :layout_with_bindings self.layout_resolver.resolveWithFormalBindings(artifact, checked_type, bindings.items) catch |err| switch (err) {
                error.OutOfMemory => return error.OutOfMemory,
                error.UnresolvedByValue => {
                    try self.recordUnresolvedByValue(artifact, checked_type);
                    return error.UnresolvedByValue;
                },
            };
        };
        return self.layoutFactsForIdx(layout_idx);
    }

    /// Get an existing type table index for a checked type, or insert a new entry.
    /// Pre-registers a placeholder before conversion to prevent infinite recursion
    /// on cyclic types (the placeholder is updated in-place after conversion).
    fn getOrInsert(
        self: *TypeTable,
        artifact_in: *const CheckedArtifact.CheckedModuleArtifact,
        checked_type_in: CheckedArtifact.CheckedTypeId,
    ) TypeTableError!u64 {
        const src = self.substituteFormal(artifact_in, checked_type_in);
        const artifact = src.artifact;
        const checked_type = src.checked_type;

        // A recursive backing reference to a nominal currently being opened (same
        // declaration + resolved args) resolves to that open's in-progress entry.
        const open_identity = try self.nominalOpenIdentity(artifact, checked_type);
        var open_arg_keys: ?[]TypeTableKey = if (open_identity) |ident| ident.arg_keys else null;
        defer if (open_arg_keys) |keys| self.gpa.free(keys);
        if (open_identity) |ident| {
            for (self.active_opens.items) |open| {
                if (!checkedArtifactKeysEqual(open.decl_artifact_key, ident.decl_artifact_key)) continue;
                if (open.source_statement != ident.source_statement) continue;
                if (!typeTableKeysEqual(open.arg_keys, ident.arg_keys)) continue;
                return open.entry_idx;
            }
        }

        // While opening a backing, non-nominal declaration-space entries memoize
        // in the per-open table (keyed by open ctx) rather than the global
        // `var_map`, so the same structure opened at different arguments does
        // not alias.
        const idx: u64 = idx: {
            if (self.open_ctx != 0) {
                const mkey = OpenMemoKey{ .ctx = self.open_ctx, .artifact_key = artifact.key, .checked_type = checked_type };
                if (self.open_memo.get(mkey)) |existing| return existing;
                const reserved: u64 = @intCast(self.entries.items.len);
                try self.entries.append(self.gpa, .{
                    .repr = .{ .unknown = .{ .name = "", .layout = self.layoutFactsForIdx(.opaque_ptr) } },
                    .source = .{ .artifact = artifact, .checked_type = checked_type },
                });
                try self.open_memo.put(mkey, reserved);
                break :idx reserved;
            }
            const key = TypeTableKey{ .artifact_key = artifact.key, .checked_type = checked_type };
            if (self.var_map.get(key)) |existing| return existing;
            const reserved: u64 = @intCast(self.entries.items.len);
            try self.entries.append(self.gpa, .{
                .repr = .{ .unknown = .{ .name = "", .layout = self.layoutFactsForIdx(.opaque_ptr) } },
                .source = .{ .artifact = artifact, .checked_type = checked_type },
            });
            try self.var_map.put(key, reserved);
            break :idx reserved;
        };

        // Register this nominal as an active open (transferring ownership of
        // arg_keys) so its backing's recursive references resolve to `idx`.
        var pushed_open = false;
        if (open_identity) |ident| {
            try self.active_opens.append(self.gpa, .{
                .decl_artifact_key = ident.decl_artifact_key,
                .source_statement = ident.source_statement,
                .arg_keys = ident.arg_keys,
                .entry_idx = idx,
            });
            open_arg_keys = null; // ownership moved to active_opens
            pushed_open = true;
        }
        defer if (pushed_open) {
            const popped = self.active_opens.pop().?;
            self.gpa.free(popped.arg_keys);
        };

        const repr = try self.convertCheckedType(artifact, checked_type);

        self.entries.items[@intCast(idx)].repr = repr;

        switch (repr) {
            .record => |rec| {
                if (rec.name.len == 0) {
                    // Name anonymous structs by a STRUCTURAL content hash (field
                    // names + each field's structural identity), not the volatile
                    // type-table index. This keeps host-facing names stable across
                    // any change that reorders the type table (issue #9983's
                    // backing opening inserts entries), and deduplicates
                    // structurally identical anonymous structs.
                    var hasher = std.hash.Wyhash.init(0);
                    for (rec.fields) |field| {
                        hasher.update(field.name);
                        hasher.update(&[_]u8{0});
                        self.hashStructuralId(&hasher, field.type_id);
                        hasher.update(&[_]u8{0});
                    }
                    self.entries.items[@intCast(idx)].repr = .{ .record = .{
                        .name = try std.fmt.allocPrint(self.gpa, "__AnonStruct_{x}", .{hasher.final()}),
                        .anonymous = true,
                        .fields = rec.fields,
                        .layout = rec.layout,
                    } };
                }
            },
            else => {},
        }

        return idx;
    }

    /// Mix a checked type's STABLE structural identity into `hasher` for naming
    /// anonymous structs independently of type-table entry order. Named types
    /// (record/tag/unknown) contribute their name; list/box recurse into their
    /// element; everything else contributes its variant tag. Recursion
    /// terminates at named types (a recursive structure routes through a named
    /// nominal), so this never loops.
    fn hashStructuralId(self: *const TypeTable, hasher: *std.hash.Wyhash, type_id: u64) void {
        if (type_id >= self.entries.items.len) {
            hasher.update("?");
            return;
        }
        switch (self.entries.items[@intCast(type_id)].repr) {
            .record => |r| hasher.update(r.name),
            .tag_union => |t| hasher.update(t.name),
            .unknown => |u| hasher.update(u.name),
            .list => |l| {
                hasher.update("list:");
                self.hashStructuralId(hasher, l.elem_id);
            },
            .box => |b| {
                hasher.update("box:");
                self.hashStructuralId(hasher, b.inner_id);
            },
            else => |r| hasher.update(@tagName(r)),
        }
    }

    /// Insert a Unit type and return its index.
    fn insertUnit(self: *TypeTable) Allocator.Error!u64 {
        const idx: u64 = @intCast(self.entries.items.len);
        try self.entries.append(self.gpa, .{ .repr = .{ .unit = self.layoutFactsForIdx(.zst) }, .source = null });
        return idx;
    }

    fn attachAbiLayouts(self: *TypeTable, build_env: *BuildEnv) Allocator.Error!void {
        var artifacts = std.ArrayList(*const CheckedArtifact.CheckedModuleArtifact).empty;
        defer artifacts.deinit(self.gpa);

        for (self.entries.items) |*entry| {
            if (entry.source) |source| {
                if (!artifactListed(artifacts.items, source.artifact)) {
                    try artifacts.append(self.gpa, source.artifact);
                }
            } else {
                entry.abi = zeroSizedBuiltinAbi();
            }
        }

        for (artifacts.items) |artifact| {
            var requests = std.ArrayList(CheckedArtifact.CheckedTypeId).empty;
            defer requests.deinit(self.gpa);

            for (self.entries.items) |entry| {
                const source = entry.source orelse continue;
                if (source.artifact == artifact) {
                    if (!checkedTypeListed(requests.items, source.checked_type)) {
                        try requests.append(self.gpa, source.checked_type);
                    }
                }
            }
            if (requests.items.len == 0) continue;

            const imported_artifacts = try build_env.collectImportedArtifactViews(self.gpa, artifact);
            defer self.gpa.free(imported_artifacts);
            const relation_artifacts = try build_env.collectRelationArtifactViews(self.gpa, artifact);
            defer self.gpa.free(relation_artifacts);

            var lowered = try lir.CheckedPipeline.lowerCheckedModulesToLir(
                self.gpa,
                .{
                    .root = CheckedArtifact.loweringViewWithRelations(artifact, relation_artifacts),
                    .imports = imported_artifacts,
                },
                .{ .layout_requests = requests.items },
                // Lowering needs a default width for the layout store, but every
                // ABI fact glue emits is an explicit dual-width query
                // (`sizeAt(.u32/.u64)`, `getStructFieldOffsetByOriginalIndexAt(..., .u32/.u64)`,
                // ...), so this fixed choice cannot affect glue output.
                .{ .target_usize = .u64, .layout_request_const_plans = false },
            );
            defer lowered.deinit();

            for (lowered.lir_result.requested_layouts.items) |request| {
                for (self.entries.items) |*entry| {
                    const source = entry.source orelse continue;
                    if (source.artifact != artifact or source.checked_type != request.checked_type) continue;
                    if (entry.abi != null) continue;
                    const layout_idx = request.layout_idx;
                    entry.abi = try self.abiForLayout(&lowered.lir_result.layouts, layout_idx, entry.repr);
                }
            }
        }

        for (self.entries.items, 0..) |entry, idx| {
            if (entry.abi == null) {
                glueInvariant("missing compiler-emitted ABI layout for glue type id {d}", .{idx});
            }
        }
    }

    fn artifactListed(
        artifacts: []const *const CheckedArtifact.CheckedModuleArtifact,
        artifact: *const CheckedArtifact.CheckedModuleArtifact,
    ) bool {
        for (artifacts) |item| {
            if (item == artifact) return true;
        }
        return false;
    }

    fn checkedTypeListed(
        checked_types: []const CheckedArtifact.CheckedTypeId,
        checked_type: CheckedArtifact.CheckedTypeId,
    ) bool {
        for (checked_types) |item| {
            if (item == checked_type) return true;
        }
        return false;
    }

    fn zeroSizedBuiltinAbi() CollectedAbiLayout {
        return .{
            .size_align = .{
                .size32 = 0,
                .alignment32 = 1,
                .size64 = 0,
                .alignment64 = 1,
            },
            .contains_refcounted = false,
            .details = .builtin,
        };
    }

    fn abiForLayout(
        self: *TypeTable,
        store: *const layout.Store,
        layout_idx: layout.Idx,
        repr: CollectedTypeRepr,
    ) Allocator.Error!CollectedAbiLayout {
        const layout_val = store.getLayout(layout_idx);
        const details = switch (repr) {
            .record => |rec| try self.abiRecordDetails(store, layout_val, rec),
            .tag_union => |tu| try self.abiTagUnionDetails(store, layout_val, tu),
            else => CollectedAbiLayoutDetails.builtin,
        };
        return .{
            .size_align = abiSizeAlign(store, layout_val),
            .contains_refcounted = store.layoutContainsRefcounted(layout_val),
            .details = details,
        };
    }

    fn abiSizeAlign(store: *const layout.Store, layout_val: layout.Layout) CollectedAbiSizeAlign {
        return .{
            .size32 = store.sizeAt(layout_val, .u32),
            .alignment32 = layout_val.alignment(.u32).toByteUnits(),
            .size64 = store.sizeAt(layout_val, .u64),
            .alignment64 = layout_val.alignment(.u64).toByteUnits(),
        };
    }

    fn abiRecordDetails(
        self: *TypeTable,
        store: *const layout.Store,
        layout_val: layout.Layout,
        rec: anytype,
    ) Allocator.Error!CollectedAbiLayoutDetails {
        if (layout_val.tag == .zst) {
            return .{ .record = &.{} };
        }
        if (layout_val.tag != .struct_) {
            glueInvariant("record glue type reached ABI attachment with {s} layout", .{@tagName(layout_val.tag)});
        }

        const struct_idx = layout_val.getStruct().idx;
        const info = store.getStructInfo(layout_val);
        const fields = try self.gpa.alloc(CollectedAbiFieldLayout, info.fields.len);
        var populated: usize = 0;
        errdefer {
            for (fields[0..populated]) |field| self.freeDuped(field.name);
            self.gpa.free(fields);
        }

        var prev_offset32: u64 = 0;
        var prev_offset64: u64 = 0;
        for (0..info.fields.len) |field_index| {
            const committed = info.fields.get(@intCast(field_index));
            const size32 = store.getStructFieldSizeByOriginalIndexAt(struct_idx, committed.index, .u32);
            const size64 = store.getStructFieldSizeByOriginalIndexAt(struct_idx, committed.index, .u64);
            // The layout store commits declared-order padding fields verbatim,
            // including zero-sized markers like `_ : {}`. Glue's reflected field
            // list drops those (they are layout markers only), so skip them here
            // too: emitters render padding as nonzero byte arrays per width.
            if (committed.is_padding and size32 == 0 and size64 == 0) continue;
            const semantic = recordFieldByOriginalIndex(rec.fields, committed.index) orelse
                glueInvariant("record ABI field original index {d} missing from reflected fields", .{committed.index});
            const field_layout = store.getLayout(committed.layout);
            const field_alignment32: u64 = if (committed.is_padding) 1 else field_layout.alignment(.u32).toByteUnits();
            const field_alignment64: u64 = if (committed.is_padding) 1 else field_layout.alignment(.u64).toByteUnits();
            fields[populated] = .{
                .name = try self.gpa.dupe(u8, semantic.name),
                .type_id = semantic.type_id,
                .original_index = committed.index,
                .is_padding = committed.is_padding,
                .offset32 = store.getStructFieldOffsetByOriginalIndexAt(struct_idx, committed.index, .u32),
                .offset64 = store.getStructFieldOffsetByOriginalIndexAt(struct_idx, committed.index, .u64),
                .size32 = size32,
                .alignment32 = field_alignment32,
                .size64 = size64,
                .alignment64 = field_alignment64,
            };
            assertCommittedFieldOrder(fields[populated], &prev_offset32, &prev_offset64);
            populated += 1;
        }

        const emitted = if (populated == fields.len)
            fields
        else
            try self.gpa.realloc(fields, populated);
        return .{ .record = emitted };
    }

    /// Emitters iterate ABI field lists in the given (committed) order without
    /// re-sorting, so that order must be valid at both pointer widths, and
    /// zero-sized padding must never be emitted (emitters render padding as
    /// nonzero byte arrays per width).
    fn assertCommittedFieldOrder(
        field: CollectedAbiFieldLayout,
        prev_offset32: *u64,
        prev_offset64: *u64,
    ) void {
        if (field.offset32 < prev_offset32.* or field.offset64 < prev_offset64.*) {
            glueInvariant(
                "committed struct field offsets must be non-decreasing at both pointer widths (field '{s}' at offset32={d} offset64={d})",
                .{ field.name, field.offset32, field.offset64 },
            );
        }
        prev_offset32.* = field.offset32;
        prev_offset64.* = field.offset64;
        if (field.is_padding and (field.size32 == 0 or field.size64 == 0)) {
            glueInvariant("zero-sized padding field '{s}' committed to glue ABI layout", .{field.name});
        }
    }

    fn recordFieldByOriginalIndex(
        fields: []const CollectedRecordField,
        original_index: u64,
    ) ?CollectedRecordField {
        for (fields) |field| {
            if (field.original_index == original_index) return field;
        }
        return null;
    }

    fn abiTagUnionDetails(
        self: *TypeTable,
        store: *const layout.Store,
        layout_val: layout.Layout,
        tu: anytype,
    ) Allocator.Error!CollectedAbiLayoutDetails {
        if (layout_val.tag == .zst) {
            return .{ .tag_union = .{
                .discriminant_size = 0,
                .discriminant_offset32 = 0,
                .discriminant_offset64 = 0,
                .tags = try self.abiZeroSizedTagLayouts(tu),
            } };
        }
        if (layout_val.tag != .tag_union) {
            glueInvariant("tag-union glue type reached ABI attachment with {s} layout", .{@tagName(layout_val.tag)});
        }

        const info = store.getTagUnionInfo(layout_val);
        if (info.variants.len != tu.tags.len) {
            glueInvariant("tag-union ABI variant count {d} differed from reflected tag count {d}", .{ info.variants.len, tu.tags.len });
        }

        const tags = try self.gpa.alloc(CollectedAbiTagLayout, tu.tags.len);
        var populated: usize = 0;
        errdefer {
            for (tags[0..populated]) |tag| {
                self.freeDuped(tag.name);
                self.gpa.free(tag.payload_ids);
                self.freeAbiFieldLayouts(tag.payload_fields);
            }
            self.gpa.free(tags);
        }

        for (tu.tags, 0..) |tag, i| {
            const variant_layout_idx = info.variants.get(@intCast(i)).payload_layout;
            const variant_layout = store.getLayout(variant_layout_idx);
            tags[i] = .{
                .name = try self.gpa.dupe(u8, tag.name),
                .payload_ids = try self.gpa.dupe(u64, tag.payload_ids),
                .payload_fields = try self.abiPayloadFields(store, variant_layout, tag.payload_ids),
                .discriminant = @intCast(i),
                .payload_size32 = store.sizeAt(variant_layout, .u32),
                .payload_alignment32 = variant_layout.alignment(.u32).toByteUnits(),
                .payload_size64 = store.sizeAt(variant_layout, .u64),
                .payload_alignment64 = variant_layout.alignment(.u64).toByteUnits(),
            };
            populated += 1;
        }

        return .{ .tag_union = .{
            .discriminant_size = info.data.discriminant_size,
            .discriminant_offset32 = info.data.discriminant_offset.get(.u32),
            .discriminant_offset64 = info.data.discriminant_offset.get(.u64),
            .tags = tags,
        } };
    }

    fn abiPayloadFields(
        self: *TypeTable,
        store: *const layout.Store,
        payload_layout: layout.Layout,
        payload_ids: []const u64,
    ) Allocator.Error![]const CollectedAbiFieldLayout {
        if (payload_ids.len <= 1 or payload_layout.tag == .zst) {
            return &.{};
        }
        if (payload_layout.tag != .struct_) {
            glueInvariant("multi-payload tag variant reached ABI attachment with {s} payload layout", .{@tagName(payload_layout.tag)});
        }

        const struct_idx = payload_layout.getStruct().idx;
        const info = store.getStructInfo(payload_layout);
        const fields = try self.gpa.alloc(CollectedAbiFieldLayout, info.fields.len);
        var populated: usize = 0;
        errdefer {
            for (fields[0..populated]) |field| self.freeDuped(field.name);
            self.gpa.free(fields);
        }

        var prev_offset32: u64 = 0;
        var prev_offset64: u64 = 0;
        for (0..info.fields.len) |field_index| {
            const committed = info.fields.get(@intCast(field_index));
            const original_index: usize = @intCast(committed.index);
            if (original_index >= payload_ids.len) {
                glueInvariant("tag payload field original index {d} out of bounds for {d} payloads", .{ committed.index, payload_ids.len });
            }
            const size32 = store.getStructFieldSizeByOriginalIndexAt(struct_idx, committed.index, .u32);
            const size64 = store.getStructFieldSizeByOriginalIndexAt(struct_idx, committed.index, .u64);
            // Mirror abiRecordDetails: zero-sized committed padding is a layout
            // marker only and must not reach emitters.
            if (committed.is_padding and size32 == 0 and size64 == 0) continue;
            const field_layout = store.getLayout(committed.layout);
            fields[populated] = .{
                .name = try std.fmt.allocPrint(self.gpa, "_{d}", .{committed.index}),
                .type_id = payload_ids[original_index],
                .original_index = committed.index,
                .is_padding = committed.is_padding,
                .offset32 = store.getStructFieldOffsetByOriginalIndexAt(struct_idx, committed.index, .u32),
                .offset64 = store.getStructFieldOffsetByOriginalIndexAt(struct_idx, committed.index, .u64),
                .size32 = size32,
                .alignment32 = if (committed.is_padding) 1 else field_layout.alignment(.u32).toByteUnits(),
                .size64 = size64,
                .alignment64 = if (committed.is_padding) 1 else field_layout.alignment(.u64).toByteUnits(),
            };
            assertCommittedFieldOrder(fields[populated], &prev_offset32, &prev_offset64);
            populated += 1;
        }

        const emitted = if (populated == fields.len)
            fields
        else
            try self.gpa.realloc(fields, populated);
        return emitted;
    }

    fn abiZeroSizedTagLayouts(self: *TypeTable, tu: anytype) Allocator.Error![]const CollectedAbiTagLayout {
        const tags = try self.gpa.alloc(CollectedAbiTagLayout, tu.tags.len);
        var populated: usize = 0;
        errdefer {
            for (tags[0..populated]) |tag| {
                self.freeDuped(tag.name);
                self.gpa.free(tag.payload_ids);
                self.freeAbiFieldLayouts(tag.payload_fields);
            }
            self.gpa.free(tags);
        }
        for (tu.tags, 0..) |tag, i| {
            tags[i] = .{
                .name = try self.gpa.dupe(u8, tag.name),
                .payload_ids = try self.gpa.dupe(u64, tag.payload_ids),
                .payload_fields = &.{},
                .discriminant = @intCast(i),
                .payload_size32 = 0,
                .payload_alignment32 = 1,
                .payload_size64 = 0,
                .payload_alignment64 = 1,
            };
            populated += 1;
        }
        return tags;
    }

    fn convertCheckedType(
        self: *TypeTable,
        artifact: *const CheckedArtifact.CheckedModuleArtifact,
        checked_type: CheckedArtifact.CheckedTypeId,
    ) TypeTableError!CollectedTypeRepr {
        const payload = checkedTypePayload(artifact, checked_type);
        return switch (payload) {
            .pending => glueInvariant("pending checked type reached glue type table", .{}),
            .err => glueInvariant("erroneous checked type reached glue type table", .{}),
            .flex => .{ .unknown = .{ .name = try self.gpa.dupe(u8, "flex"), .layout = self.layoutFactsForIdx(.opaque_ptr) } },
            .rigid => .{ .unknown = .{ .name = try self.gpa.dupe(u8, "rigid"), .layout = self.layoutFactsForIdx(.opaque_ptr) } },
            .alias => |alias| try self.getAliasBackingRepr(artifact, alias.backing),
            .record => |record| try self.convertRecord(artifact, checked_type, record.fields, record.ext),
            .record_unbound => |fields| try self.convertRecord(artifact, checked_type, fields, null),
            .tuple => |items| try self.convertTuple(artifact, checked_type, items),
            .nominal => |nominal| try self.convertNominal(artifact, checked_type, nominal),
            .function => |func| try self.convertFunc(artifact, checked_type, func),
            .empty_record, .empty_tag_union => .{ .unit = self.layoutFactsForIdx(.zst) },
            .tag_union => |tag_union| try self.convertTagUnion(artifact, checked_type, tag_union.tags, tag_union.ext),
        };
    }

    fn getAliasBackingRepr(
        self: *TypeTable,
        artifact: *const CheckedArtifact.CheckedModuleArtifact,
        backing: CheckedArtifact.CheckedTypeId,
    ) TypeTableError!CollectedTypeRepr {
        return self.convertCheckedType(artifact, backing);
    }

    fn convertNominal(
        self: *TypeTable,
        artifact: *const CheckedArtifact.CheckedModuleArtifact,
        checked_type: CheckedArtifact.CheckedTypeId,
        nominal: CheckedArtifact.CheckedNominalType,
    ) TypeTableError!CollectedTypeRepr {
        const display_name = TypeTable.getTypeDisplayName(artifact.canonical_names.typeNameText(nominal.name));
        const nominal_layout = try self.layoutFactsForCheckedType(artifact, checked_type);

        if (nominal.builtin) |builtin_nominal| {
            switch (builtin_nominal) {
                .list => {
                    if (nominal.args.len >= 1) return .{ .list = .{
                        .elem_id = try self.getOrInsert(artifact, nominal.args[0]),
                        .layout = nominal_layout,
                    } };
                    return .{ .unknown = .{ .name = try self.gpa.dupe(u8, "List"), .layout = self.layoutFactsForIdx(.opaque_ptr) } };
                },
                .box => {
                    if (nominal.args.len >= 1) return .{ .box = .{
                        .inner_id = try self.getOrInsert(artifact, nominal.args[0]),
                        .layout = nominal_layout,
                    } };
                    return .{ .unknown = .{ .name = try self.gpa.dupe(u8, "Box"), .layout = self.layoutFactsForIdx(.opaque_ptr) } };
                },
                .parse_tag_union_spec,
                .fields,
                .field,
                => return .{ .unit = self.layoutFactsForIdx(.zst) },
                .dict,
                .set,
                .crypto_sha256_digest,
                .crypto_sha256_hasher,
                .crypto_blake3_digest,
                .crypto_blake3_hasher,
                => {},
                .str => return .{ .str_ = self.layoutFactsForIdx(.str) },
                .bool => return .{ .bool_ = self.layoutFactsForIdx(.bool) },
                .dec => return .{ .dec = self.layoutFactsForIdx(.dec) },
                .u8 => return .{ .u8_ = self.layoutFactsForIdx(.u8) },
                .u16 => return .{ .u16_ = self.layoutFactsForIdx(.u16) },
                .u32 => return .{ .u32_ = self.layoutFactsForIdx(.u32) },
                .u64 => return .{ .u64_ = self.layoutFactsForIdx(.u64) },
                .u128 => return .{ .u128_ = self.layoutFactsForIdx(.u128) },
                .i8 => return .{ .i8_ = self.layoutFactsForIdx(.i8) },
                .i16 => return .{ .i16_ = self.layoutFactsForIdx(.i16) },
                .i32 => return .{ .i32_ = self.layoutFactsForIdx(.i32) },
                .i64 => return .{ .i64_ = self.layoutFactsForIdx(.i64) },
                .i128 => return .{ .i128_ = self.layoutFactsForIdx(.i128) },
                .f32 => return .{ .f32_ = self.layoutFactsForIdx(.f32) },
                .f64 => return .{ .f64_ = self.layoutFactsForIdx(.f64) },
            }
        }

        const backing_repr = if (self.nominalDeclarationFor(artifact, nominal)) |lookup| open_blk: {
            // Open the declaration's backing TEMPLATE with the application's
            // args substituted for the declaration's formals (issue #9983).
            const decl = lookup.declaration;
            const formals = decl.formalArgs(&lookup.artifact.checked_types);
            if (formals.len != nominal.args.len) glueInvariant("nominal application arity disagreed with its declaration in glue backing opening", .{});

            // Resolve the application's args through the CURRENT (outer)
            // bindings before this open's bindings shadow anything, giving the
            // open its concrete identity.
            const resolved_arg_keys = try self.gpa.alloc(TypeTableKey, nominal.args.len);
            defer self.gpa.free(resolved_arg_keys);
            for (nominal.args, resolved_arg_keys) |arg, *arg_key| {
                const resolved = self.substituteFormal(artifact, arg);
                arg_key.* = .{ .artifact_key = resolved.artifact.key, .checked_type = resolved.checked_type };
            }

            const saved = try self.gpa.alloc(?BoundSource, formals.len);
            defer self.gpa.free(saved);
            const bound_formals = try self.gpa.alloc(bool, formals.len);
            defer self.gpa.free(bound_formals);
            for (formals, nominal.args, saved, bound_formals) |formal, arg, *slot, *is_bound| {
                const fkey = TypeTableKey{ .artifact_key = lookup.artifact.key, .checked_type = formal };
                // A polymorphic application binds a formal to itself (arg == formal,
                // a bare rigid). Skip it: leaving the formal unbound resolves it as
                // a rigid in-context and avoids a self-referential binding cycle.
                const resolved = self.substituteFormal(artifact, arg);
                if (std.meta.eql(resolved.artifact.key, fkey.artifact_key) and resolved.checked_type == fkey.checked_type) {
                    is_bound.* = false;
                    continue;
                }
                is_bound.* = true;
                slot.* = self.template_bindings.get(fkey);
                try self.template_bindings.put(fkey, .{ .artifact = artifact, .checked_type = arg });
            }
            defer {
                for (formals, saved, bound_formals) |formal, slot, is_bound| {
                    if (!is_bound) continue;
                    const fkey = TypeTableKey{ .artifact_key = lookup.artifact.key, .checked_type = formal };
                    if (slot) |prev| {
                        self.template_bindings.put(fkey, prev) catch unreachable;
                    } else {
                        _ = self.template_bindings.remove(fkey);
                    }
                }
            }

            const prev_ctx = self.open_ctx;
            self.open_ctx = try self.ctxForOpen(lookup.artifact.key, decl.source_statement, resolved_arg_keys);
            defer self.open_ctx = prev_ctx;

            break :open_blk try self.convertCheckedType(lookup.artifact, decl.backing);
        } else glueInvariant("nominal glue conversion could not find declaration backing", .{});

        return switch (backing_repr) {
            .record => |rec| blk: {
                // The backing record `rec.fields` is in the structural (sorted)
                // order. A nominal record keeps DECLARED source order only when
                // it opts in with an unnamed `_` padding field.
                const declared_fields = try self.nominalRecordInDeclaredOrder(artifact, nominal, rec, nominal_layout) orelse
                    break :blk .{ .record = .{
                        .name = try self.gpa.dupe(u8, display_name),
                        .anonymous = false,
                        .fields = rec.fields,
                        .layout = nominal_layout,
                    } };
                // `declared_fields` replaces `rec.fields`, which we now own and free.
                for (rec.fields) |field| self.freeDuped(field.name);
                self.gpa.free(rec.fields);
                break :blk .{ .record = .{
                    .name = try self.gpa.dupe(u8, display_name),
                    .anonymous = false,
                    .fields = declared_fields,
                    .layout = nominal_layout,
                } };
            },
            .tag_union => |tu| blk: {
                self.freeDuped(tu.name);
                break :blk .{ .tag_union = .{
                    .name = try self.gpa.dupe(u8, display_name),
                    .tags = tu.tags,
                    .layout = nominal_layout,
                } };
            },
            else => backing_repr,
        };
    }

    const NominalDeclarationLookup = struct {
        artifact: *const CheckedArtifact.CheckedModuleArtifact,
        declaration: CheckedArtifact.CheckedNominalDeclaration,
        padding_field_types: []const CheckedArtifact.CheckedTypeId,
    };

    fn nominalDeclarationFor(
        self: *TypeTable,
        artifact: *const CheckedArtifact.CheckedModuleArtifact,
        nominal: CheckedArtifact.CheckedNominalType,
    ) ?NominalDeclarationLookup {
        // Self-contained artifacts embed a copy of every imported/builtin
        // nominal declaration they use, keyed by content identity. Resolve
        // locally first so no owner artifact must be loaded.
        const local_key = check.CanonicalNames.NominalTypeKey{
            .module = nominal.origin_module,
            .type_name = nominal.name,
            .source_decl = nominal.source_decl,
        };
        if (artifact.checked_types.nominalDeclaration(local_key)) |declaration| {
            return .{
                .artifact = artifact,
                .declaration = declaration,
                .padding_field_types = declaration.paddingFieldTypes(&artifact.checked_types),
            };
        }
        return switch (nominal.representation) {
            .local_declaration => |declaration_id| .{
                .artifact = artifact,
                .declaration = artifact.checked_types.nominalDeclarationById(declaration_id),
                .padding_field_types = artifact.checked_types.nominalDeclarationById(declaration_id).paddingFieldTypes(&artifact.checked_types),
            },
            .imported_declaration => |imported| blk: {
                const owner = self.artifacts_by_key.get(CheckedArtifact.importedNominalDeclarationModuleId(imported)) orelse
                    glueInvariant("imported nominal declaration referenced an artifact that glue did not load", .{});
                const declaration = owner.checked_types.nominalDeclarationById(imported.declaration);
                break :blk .{
                    .artifact = owner,
                    .declaration = declaration,
                    .padding_field_types = declaration.paddingFieldTypes(&owner.checked_types),
                };
            },
            .local_box_payload_capability => |capability_ref| blk: {
                const capability = artifact.interface_capabilities.boxPayloadCapability(capability_ref.capability);
                const declaration = artifact.checked_types.nominalDeclaration(capability.nominal) orelse
                    glueInvariant("boxed payload capability referenced a nominal declaration that is not in the owner artifact", .{});
                break :blk .{
                    .artifact = artifact,
                    .declaration = declaration,
                    .padding_field_types = capability.paddingFieldTys(&artifact.interface_capabilities),
                };
            },
            .imported_box_payload_capability => |capability_ref| blk: {
                const owner = self.artifacts_by_key.get(CheckedArtifact.importedBoxPayloadCapabilityModuleId(capability_ref)) orelse
                    glueInvariant("imported boxed payload capability referenced an artifact that glue did not load", .{});
                const capability = owner.interface_capabilities.boxPayloadCapability(capability_ref.capability);
                const declaration = owner.checked_types.nominalDeclaration(capability.nominal) orelse
                    glueInvariant("imported boxed payload capability referenced a nominal declaration that is not in the owner artifact", .{});
                break :blk .{
                    .artifact = owner,
                    .declaration = declaration,
                    .padding_field_types = capability.paddingFieldTys(&owner.interface_capabilities),
                };
            },
            .builtin,
            .opaque_without_backing,
            => null,
        };
    }

    /// Builds a nominal record's field list in DECLARED source order from
    /// checked artifact metadata, with nonzero unnamed `_` / `_name` fields
    /// reinstated as padding spacers, matching the layout store's
    /// `putNominalStructFields`. Byte offsets/sizes are not computed here; the
    /// committed ABI facts attach later via `attachAbiLayouts`.
    /// Returns `null` only when the declaration has no `_` field; such records
    /// intentionally use structural backing order. `backing` provides each named
    /// field's already converted `type_id`, matched by name text.
    fn nominalRecordInDeclaredOrder(
        self: *TypeTable,
        artifact: *const CheckedArtifact.CheckedModuleArtifact,
        nominal: CheckedArtifact.CheckedNominalType,
        backing: anytype,
        nominal_layout: CollectedLayoutFacts,
    ) Allocator.Error!?[]const CollectedRecordField {
        const lookup = self.nominalDeclarationFor(artifact, nominal) orelse return null;
        const declared_fields = lookup.declaration.declaredRecordFields(&lookup.artifact.checked_types);
        if (declared_fields.len == 0) return null;
        const padding_types = lookup.padding_field_types;
        const layout_value = self.layouts.getLayout(nominal_layout.layout_idx);
        if (layout_value.tag != .struct_) return null;
        const struct_idx = layout_value.getStruct().idx;

        const committed_fields = self.layouts.getStructInfo(layout_value).fields;

        // Each named declared field reads its converted shape from the backing
        // record (matched by name); each nonzero unnamed field becomes a padding
        // spacer whose per-width byte counts come from the committed ABI layout.
        const collected = try self.gpa.alloc(CollectedRecordField, declared_fields.len);
        var populated: usize = 0;
        errdefer self.freeCollectedRecordFields(collected, populated);

        var padding_cursor: usize = 0;
        var pad_index: usize = 0;
        var committed_pos: usize = 0;
        var saw_unnamed_field = false;
        for (declared_fields) |field| {
            switch (field) {
                .padding => {
                    saw_unnamed_field = true;
                    if (padding_cursor >= padding_types.len) {
                        glueInvariant("nominal declaration had more padding fields than padding types", .{});
                    }
                    _ = padding_types[padding_cursor];
                    padding_cursor += 1;
                    const padding_ordinal = pad_index;
                    pad_index += 1;
                    if (committed_pos >= committed_fields.len) {
                        glueInvariant("nominal declaration had more padding fields than committed layout fields", .{});
                    }
                    const committed_field = committed_fields.get(@intCast(committed_pos));
                    if (!committed_field.is_padding) {
                        glueInvariant("nominal padding field did not line up with committed padding field", .{});
                    }
                    const size_32 = self.layouts.getStructFieldSizeAt(struct_idx, @intCast(committed_pos), .u32);
                    const size_64 = self.layouts.getStructFieldSizeAt(struct_idx, @intCast(committed_pos), .u64);
                    committed_pos += 1;
                    if (size_32 == 0 and size_64 == 0) continue;

                    const name = try std.fmt.allocPrint(self.gpa, "_pad{d}", .{padding_ordinal});
                    collected[populated] = .{
                        .name = name,
                        .type_id = 0,
                        .original_index = @intCast(committed_field.index),
                        .is_padding = true,
                    };
                    populated += 1;
                },
                .named => |field_name_id| {
                    const field_name = lookup.artifact.canonical_names.recordFieldLabelText(field_name_id);
                    const match = backingFieldByName(backing, field_name) orelse
                        glueInvariant("nominal declaration field '{s}' missing from backing record", .{field_name});
                    if (committed_pos >= committed_fields.len) {
                        glueInvariant("nominal declaration had more named fields than committed layout fields", .{});
                    }
                    const committed_field = committed_fields.get(@intCast(committed_pos));
                    if (committed_field.is_padding) {
                        glueInvariant("nominal named field lined up with committed padding field", .{});
                    }
                    committed_pos += 1;
                    const name = try self.gpa.dupe(u8, field_name);
                    collected[populated] = .{
                        .name = name,
                        .type_id = match.type_id,
                        .original_index = @intCast(committed_field.index),
                        .is_padding = false,
                    };
                    populated += 1;
                },
            }
        }

        // A nominal record keeps its declared order only when it opts in with an
        // unnamed `_` field. Without one it lays out like a structural record.
        if (!saw_unnamed_field) {
            self.freeCollectedRecordFields(collected, populated);
            return null;
        }
        if (padding_cursor != padding_types.len) {
            glueInvariant("nominal declaration had more padding types than padding fields", .{});
        }
        if (committed_pos != committed_fields.len) {
            glueInvariant("nominal declaration field count {d} did not match committed field count {d}", .{ committed_pos, committed_fields.len });
        }
        const collected_fields = if (populated == collected.len)
            collected
        else
            try self.gpa.realloc(collected, populated);

        return collected_fields;
    }

    /// Finds a backing record field by its name text, returning its converted
    /// `type_id` and original index. The backing is a structurally-ordered
    /// `CollectedTypeRepr.record` payload.
    fn backingFieldByName(backing: anytype, name: []const u8) ?struct { type_id: u64, original_index: u64 } {
        for (backing.fields) |field| {
            if (std.mem.eql(u8, field.name, name)) {
                return .{ .type_id = field.type_id, .original_index = field.original_index };
            }
        }
        return null;
    }

    fn convertRecord(
        self: *TypeTable,
        artifact: *const CheckedArtifact.CheckedModuleArtifact,
        checked_type: CheckedArtifact.CheckedTypeId,
        fields: []const CheckedArtifact.CheckedRecordField,
        ext: ?CheckedArtifact.CheckedTypeId,
    ) TypeTableError!CollectedTypeRepr {
        var all_fields = std.ArrayList(CheckedArtifact.CheckedRecordField).empty;
        defer all_fields.deinit(self.gpa);
        try appendRecordRowFields(self.gpa, artifact, fields, ext, &all_fields);
        sortRecordFieldsByName(artifact, all_fields.items);
        return self.convertRecordFields(artifact, checked_type, all_fields.items);
    }

    fn convertRecordFields(
        self: *TypeTable,
        artifact: *const CheckedArtifact.CheckedModuleArtifact,
        checked_type: CheckedArtifact.CheckedTypeId,
        fields: []const CheckedArtifact.CheckedRecordField,
    ) TypeTableError!CollectedTypeRepr {
        if (fields.len == 0) return .{ .unit = self.layoutFactsForIdx(.zst) };
        const record_layout = try self.layoutFactsForCheckedType(artifact, checked_type);

        const field_type_ids = try self.gpa.alloc(u64, fields.len);
        defer self.gpa.free(field_type_ids);
        for (fields, 0..) |field, i| {
            field_type_ids[i] = try self.getOrInsert(artifact, field.ty);
        }

        const record_layout_value = self.layouts.getLayout(record_layout.layout_idx);
        if (record_layout_value.tag != .struct_) glueInvariant("record type committed to non-struct layout", .{});
        const record_info = self.layouts.getStructInfo(record_layout_value);
        if (record_info.fields.len != fields.len) {
            glueInvariant("record committed field count mismatch: expected {d}, found {d}", .{ fields.len, record_info.fields.len });
        }

        const collected_fields = try self.gpa.alloc(CollectedRecordField, record_info.fields.len);
        var populated: usize = 0;
        errdefer self.freeCollectedRecordFields(collected_fields, populated);
        for (0..record_info.fields.len) |dst_idx| {
            const committed_field = record_info.fields.get(@intCast(dst_idx));
            if (committed_field.is_padding) {
                glueInvariant("structural record committed an unexpected padding field", .{});
            }
            const src_idx: usize = committed_field.index;
            if (src_idx >= fields.len) {
                glueInvariant("record committed field index {d} out of bounds for {d} fields", .{ src_idx, fields.len });
            }
            collected_fields[dst_idx] = .{
                .name = try self.gpa.dupe(u8, artifact.canonical_names.recordFieldLabelText(fields[src_idx].name)),
                .type_id = field_type_ids[src_idx],
                .original_index = @intCast(committed_field.index),
            };
            populated += 1;
        }

        return .{ .record = .{
            .name = "",
            .anonymous = true,
            .fields = collected_fields,
            .layout = record_layout,
        } };
    }

    fn convertTuple(
        self: *TypeTable,
        artifact: *const CheckedArtifact.CheckedModuleArtifact,
        checked_type: CheckedArtifact.CheckedTypeId,
        elems: []const CheckedArtifact.CheckedTypeId,
    ) TypeTableError!CollectedTypeRepr {
        if (elems.len == 0) return .{ .unit = self.layoutFactsForIdx(.zst) };
        const tuple_layout = try self.layoutFactsForCheckedType(artifact, checked_type);

        // Convert tuple elements as record fields with positional names (_0, _1, ...)
        const field_type_ids = try self.gpa.alloc(u64, elems.len);
        defer self.gpa.free(field_type_ids);
        for (elems, 0..) |elem, i| {
            field_type_ids[i] = try self.getOrInsert(artifact, elem);
        }

        const tuple_layout_value = self.layouts.getLayout(tuple_layout.layout_idx);
        if (tuple_layout_value.tag != .struct_) glueInvariant("tuple type committed to non-struct layout", .{});
        const tuple_info = self.layouts.getStructInfo(tuple_layout_value);
        if (tuple_info.fields.len != elems.len) {
            glueInvariant("tuple committed field count mismatch: expected {d}, found {d}", .{ elems.len, tuple_info.fields.len });
        }

        const collected_fields = try self.gpa.alloc(CollectedRecordField, tuple_info.fields.len);
        var populated: usize = 0;
        errdefer self.freeCollectedRecordFields(collected_fields, populated);
        for (0..tuple_info.fields.len) |dst_idx| {
            const committed_field = tuple_info.fields.get(@intCast(dst_idx));
            if (committed_field.is_padding) {
                glueInvariant("tuple committed an unexpected padding field", .{});
            }
            const src_idx: usize = committed_field.index;
            if (src_idx >= elems.len) {
                glueInvariant("tuple committed field index {d} out of bounds for {d} fields", .{ src_idx, elems.len });
            }
            collected_fields[dst_idx] = .{
                .name = try std.fmt.allocPrint(self.gpa, "_{d}", .{src_idx}),
                .type_id = field_type_ids[src_idx],
                .original_index = @intCast(committed_field.index),
            };
            populated += 1;
        }

        return .{ .record = .{
            .name = "",
            .anonymous = true,
            .fields = collected_fields,
            .layout = tuple_layout,
        } };
    }

    fn convertTagUnion(
        self: *TypeTable,
        artifact: *const CheckedArtifact.CheckedModuleArtifact,
        checked_type: CheckedArtifact.CheckedTypeId,
        tags: []const CheckedArtifact.CheckedTag,
        ext: CheckedArtifact.CheckedTypeId,
    ) TypeTableError!CollectedTypeRepr {
        var all_tags = std.ArrayList(CheckedArtifact.CheckedTag).empty;
        defer all_tags.deinit(self.gpa);
        try appendTagRowTags(self.gpa, artifact, tags, ext, &all_tags);

        if (all_tags.items.len == 0) return .{ .unit = self.layoutFactsForIdx(.zst) };
        const union_layout = try self.layoutFactsForCheckedType(artifact, checked_type);

        // Build sortable array of tag indices
        var tag_indices = try self.gpa.alloc(usize, all_tags.items.len);
        defer self.gpa.free(tag_indices);
        for (0..all_tags.items.len) |i| {
            tag_indices[i] = i;
        }

        // Sort by name (alphabetical = discriminant order)
        const SortCtx = struct {
            tags: []const CheckedArtifact.CheckedTag,
            names: *const CanonicalNameStore,

            pub fn lessThan(ctx: @This(), a: usize, b: usize) bool {
                const a_text = ctx.names.tagLabelText(ctx.tags[a].name);
                const b_text = ctx.names.tagLabelText(ctx.tags[b].name);
                return std.mem.order(u8, a_text, b_text) == .lt;
            }
        };
        std.mem.sort(usize, tag_indices, SortCtx{ .tags = all_tags.items, .names = &artifact.canonical_names }, SortCtx.lessThan);

        // Collect tags in discriminant order.
        const collected_tags = try self.gpa.alloc(CollectedTagInfo, all_tags.items.len);

        // Also build auto-generated name from variant names joined with "Or"
        var name_len: usize = 0;
        for (tag_indices) |src_idx| {
            const nt = artifact.canonical_names.tagLabelText(all_tags.items[src_idx].name);
            name_len += nt.len;
        }
        // Add "Or" separators between names
        if (all_tags.items.len > 1) name_len += (all_tags.items.len - 1) * 2;
        const auto_name_buf: []u8 = try self.gpa.alloc(u8, name_len);
        var name_pos: usize = 0;

        for (tag_indices, 0..) |src_idx, dst_idx| {
            const tag = all_tags.items[src_idx];
            const name_text = artifact.canonical_names.tagLabelText(tag.name);

            const tag_args = tag.argsSlice(&artifact.checked_types);
            const payload_ids = try self.gpa.alloc(u64, tag_args.len);
            for (tag_args, 0..) |arg, i| {
                payload_ids[i] = try self.getOrInsert(artifact, arg);
            }

            collected_tags[dst_idx] = .{
                .name = try self.gpa.dupe(u8, name_text),
                .payload_ids = payload_ids,
            };

            // Build auto-name
            if (auto_name_buf.len > 0) {
                if (dst_idx > 0) {
                    if (name_pos + 2 <= auto_name_buf.len) {
                        auto_name_buf[name_pos] = 'O';
                        auto_name_buf[name_pos + 1] = 'r';
                        name_pos += 2;
                    }
                }
                if (name_pos + name_text.len <= auto_name_buf.len) {
                    @memcpy(auto_name_buf[name_pos .. name_pos + name_text.len], name_text);
                    name_pos += name_text.len;
                }
            }
        }

        const auto_name: []const u8 = auto_name_buf[0..name_pos];

        return .{ .tag_union = .{
            .name = auto_name,
            .tags = collected_tags,
            .layout = union_layout,
        } };
    }

    fn convertFunc(
        self: *TypeTable,
        artifact: *const CheckedArtifact.CheckedModuleArtifact,
        checked_type: CheckedArtifact.CheckedTypeId,
        func: CheckedArtifact.CheckedFunctionType,
    ) TypeTableError!CollectedTypeRepr {
        const arg_ids = try self.gpa.alloc(u64, func.args.len);
        for (func.args, 0..) |arg, i| {
            arg_ids[i] = try self.getOrInsert(artifact, arg);
        }
        const ret_id = try self.getOrInsert(artifact, func.ret);

        return .{ .function = .{
            .arg_ids = arg_ids,
            .ret_id = ret_id,
            .layout = try self.layoutFactsForCheckedType(artifact, checked_type),
        } };
    }

    /// Strip "Builtin." and "Num." prefixes from type names (mirrors TypeWriter.getDisplayName).
    pub fn getTypeDisplayName(raw_name: []const u8) []const u8 {
        if (std.mem.startsWith(u8, raw_name, "Builtin.")) {
            const without_builtin = raw_name[8..];
            if (std.mem.startsWith(u8, without_builtin, "Num.")) {
                return without_builtin[4..];
            }
            return without_builtin;
        }
        if (std.mem.startsWith(u8, raw_name, "Num.")) {
            return raw_name[4..];
        }
        return raw_name;
    }
};

const GlueFieldSlot = struct {
    ptr: [*]u8,
    layout_idx: layout.Idx,
};

const GlueAllocatedList = struct {
    list: RocList,
    bytes: ?[*]u8,
    elem_layout: layout.Idx,
    elem_size: usize,
};

const GlueRocValueWriter = struct {
    layouts: *const layout.Store,
    schemas: *const lir.CheckedPipeline.RuntimeValueSchemaStore,
    roc_ops: *builtins.host_abi.RocOps,

    fn recordField(
        self: *const GlueRocValueWriter,
        record_base: [*]u8,
        record_layout_idx: layout.Idx,
        record_type_name: []const u8,
        field_name: []const u8,
    ) GlueFieldSlot {
        const schema = self.schemas.record(record_type_name);
        const field_index = schema.fieldLogicalIndex(field_name) orelse
            glueInvariant("glue schema record '{s}' missing field '{s}'", .{ record_type_name, field_name });
        const record_layout = self.layouts.getLayout(record_layout_idx);
        if (record_layout.tag != .struct_) {
            glueInvariant("glue record '{s}' used non-struct layout {d}", .{ record_type_name, @intFromEnum(record_layout_idx) });
        }
        const offset = self.layouts.getStructFieldOffsetByOriginalIndex(record_layout.getStruct().idx, field_index);
        const field_layout = self.layouts.getStructFieldLayoutByOriginalIndex(record_layout.getStruct().idx, field_index);
        return .{
            .ptr = record_base + offset,
            .layout_idx = field_layout,
        };
    }

    fn tagIndex(self: *const GlueRocValueWriter, tag_union_type_name: []const u8, tag_name: []const u8) u16 {
        return self.schemas.tagUnion(tag_union_type_name).tagDiscriminant(tag_name) orelse
            glueInvariant("glue schema tag union '{s}' missing tag '{s}'", .{ tag_union_type_name, tag_name });
    }

    fn listElementLayout(self: *const GlueRocValueWriter, list_layout_idx: layout.Idx) layout.Idx {
        const list_layout = self.layouts.getLayout(list_layout_idx);
        return switch (list_layout.tag) {
            .list => list_layout.getIdx(),
            .list_of_zst => .zst,
            else => glueInvariant("glue expected list layout, got {s}", .{@tagName(list_layout.tag)}),
        };
    }

    fn sizeOf(self: *const GlueRocValueWriter, layout_idx: layout.Idx) usize {
        return self.layouts.layoutSize(self.layouts.getLayout(layout_idx));
    }

    fn alignmentOf(self: *const GlueRocValueWriter, layout_idx: layout.Idx) usize {
        return self.layouts.layoutSizeAlign(self.layouts.getLayout(layout_idx)).alignment.toByteUnits();
    }

    fn allocateList(
        self: *const GlueRocValueWriter,
        list_layout_idx: layout.Idx,
        len: usize,
        elements_refcounted: bool,
    ) GlueAllocatedList {
        const elem_layout = self.listElementLayout(list_layout_idx);
        const elem_size = self.sizeOf(elem_layout);
        if (len == 0) {
            return .{
                .list = RocList.empty(),
                .bytes = null,
                .elem_layout = elem_layout,
                .elem_size = elem_size,
            };
        }
        if (elem_size == 0) {
            return .{
                .list = .{ .bytes = null, .length = len, .capacity_or_alloc_ptr = builtins.list.RocList.encodeCapacity(len) },
                .bytes = null,
                .elem_layout = elem_layout,
                .elem_size = elem_size,
            };
        }

        const elem_alignment = self.alignmentOf(elem_layout);
        if (elem_alignment > std.math.maxInt(u32)) {
            glueInvariant("glue list element alignment {d} exceeds Roc allocation ABI", .{elem_alignment});
        }
        const bytes = builtins.utils.allocateWithRefcount(
            len * elem_size,
            @intCast(elem_alignment),
            elements_refcounted,
            self.roc_ops,
        );
        return .{
            .list = .{
                .bytes = bytes,
                .length = len,
                .capacity_or_alloc_ptr = builtins.list.RocList.encodeCapacity(len),
            },
            .bytes = bytes,
            .elem_layout = elem_layout,
            .elem_size = elem_size,
        };
    }

    fn zeroValue(self: *const GlueRocValueWriter, ptr: [*]u8, layout_idx: layout.Idx) void {
        const size = self.sizeOf(layout_idx);
        if (size > 0) @memset(ptr[0..size], 0);
    }

    fn writeValue(_: *const GlueRocValueWriter, ptr: [*]u8, comptime T: type, value: T) void {
        const bytes = std.mem.asBytes(&value);
        @memcpy(ptr[0..bytes.len], bytes);
    }

    fn readValue(_: *const GlueRocValueWriter, ptr: [*]const u8, comptime T: type) T {
        const typed: *const T = @ptrCast(@alignCast(ptr));
        return typed.*;
    }

    fn writeField(
        self: *const GlueRocValueWriter,
        record_base: [*]u8,
        record_layout_idx: layout.Idx,
        record_type_name: []const u8,
        field_name: []const u8,
        comptime T: type,
        value: T,
    ) void {
        const slot = self.recordField(record_base, record_layout_idx, record_type_name, field_name);
        self.writeValue(slot.ptr, T, value);
    }

    fn variantPayloadLayout(self: *const GlueRocValueWriter, tag_union_layout_idx: layout.Idx, tag_index: u16) layout.Idx {
        const tag_union_layout = self.layouts.getLayout(tag_union_layout_idx);
        if (tag_union_layout.tag != .tag_union) {
            glueInvariant("glue expected tag-union layout, got {s}", .{@tagName(tag_union_layout.tag)});
        }
        const info = self.layouts.getTagUnionInfo(tag_union_layout);
        if (tag_index >= info.variants.len) {
            glueInvariant("glue tag index {d} out of bounds for layout {d}", .{ tag_index, @intFromEnum(tag_union_layout_idx) });
        }
        return info.variants.get(tag_index).payload_layout;
    }

    fn writeTagDiscriminant(self: *const GlueRocValueWriter, tag_union_base: [*]u8, tag_union_layout_idx: layout.Idx, tag_index: u16) void {
        const tag_union_layout = self.layouts.getLayout(tag_union_layout_idx);
        if (tag_union_layout.tag != .tag_union) {
            glueInvariant("glue expected tag-union layout, got {s}", .{@tagName(tag_union_layout.tag)});
        }
        self.layouts.getTagUnionInfo(tag_union_layout).data.writeDiscriminant(tag_union_base, tag_index, self.layouts.targetUsize());
    }

    fn readTagDiscriminant(self: *const GlueRocValueWriter, tag_union_base: [*]const u8, tag_union_layout_idx: layout.Idx) u64 {
        const tag_union_layout = self.layouts.getLayout(tag_union_layout_idx);
        if (tag_union_layout.tag != .tag_union) {
            glueInvariant("glue expected tag-union layout, got {s}", .{@tagName(tag_union_layout.tag)});
        }
        return self.layouts.getTagUnionInfo(tag_union_layout).data.readDiscriminant(@constCast(tag_union_base), self.layouts.targetUsize());
    }
};

const SMALL_STRING_SIZE = @sizeOf(RocStr);

/// Create a big RocStr from a slice (avoids small string encoding issues).
fn createBigRocStr(str: []const u8, roc_ops: *builtins.host_abi.RocOps) RocStr {
    if (str.len < SMALL_STRING_SIZE) {
        const first_element = builtins.utils.allocateWithRefcount(
            SMALL_STRING_SIZE,
            @sizeOf(usize),
            false,
            roc_ops,
        );
        @memcpy(first_element[0..str.len], str);
        @memset(first_element[str.len..SMALL_STRING_SIZE], 0);

        return RocStr{
            .bytes = first_element,
            .capacity_or_alloc_ptr = RocStr.encodeCapacity(SMALL_STRING_SIZE),
            .length = str.len,
        };
    } else {
        return RocStr.fromSlice(str, roc_ops);
    }
}

/// Build a RocList of RecordFieldInfo from collected field info.
fn buildRecordFieldsRocList(
    writer: *const GlueRocValueWriter,
    fields: []const CollectedModuleTypeInfo.CollectedRecordFieldInfo,
    list_layout: layout.Idx,
) RocList {
    const allocated = writer.allocateList(list_layout, fields.len, true);
    if (allocated.bytes == null) return allocated.list;

    for (fields, 0..) |field, i| {
        const elem_base = allocated.bytes.? + i * allocated.elem_size;
        writer.zeroValue(elem_base, allocated.elem_layout);
        writer.writeField(elem_base, allocated.elem_layout, "RecordFieldInfo", "name", RocStr, createBigRocStr(field.name, writer.roc_ops));
        writer.writeField(elem_base, allocated.elem_layout, "RecordFieldInfo", "type_str", RocStr, createBigRocStr(field.type_str, writer.roc_ops));
    }

    return allocated.list;
}

/// Build a RocList of u64 from a slice of u64.
fn buildU64RocList(
    writer: *const GlueRocValueWriter,
    ids: []const u64,
    list_layout: layout.Idx,
) RocList {
    const allocated = writer.allocateList(list_layout, ids.len, false);
    if (allocated.bytes == null) return allocated.list;
    if (allocated.elem_size != @sizeOf(u64)) {
        glueInvariant("glue U64 list element layout had size {d}", .{allocated.elem_size});
    }
    const bytes = allocated.bytes.?;
    const ptr: [*]u64 = @ptrCast(@alignCast(bytes));
    for (ids, 0..) |id, i| {
        ptr[i] = id;
    }
    return allocated.list;
}

fn writeRecordFieldTypeRepr(
    writer: *const GlueRocValueWriter,
    value_base: [*]u8,
    record_field_layout: layout.Idx,
    field: CollectedRecordField,
) void {
    writer.zeroValue(value_base, record_field_layout);
    writer.writeField(value_base, record_field_layout, "RecordField", "is_padding", bool, field.is_padding);
    writer.writeField(value_base, record_field_layout, "RecordField", "name", RocStr, createBigRocStr(field.name, writer.roc_ops));
    writer.writeField(value_base, record_field_layout, "RecordField", "type_id", u64, field.type_id);
}

fn buildRecordFieldTypeReprList(
    writer: *const GlueRocValueWriter,
    fields: []const CollectedRecordField,
    list_layout: layout.Idx,
) RocList {
    const allocated = writer.allocateList(list_layout, fields.len, true);
    if (allocated.bytes == null) return allocated.list;
    for (fields, 0..) |field, i| {
        writeRecordFieldTypeRepr(writer, allocated.bytes.? + i * allocated.elem_size, allocated.elem_layout, field);
    }
    return allocated.list;
}

fn writeTagVariant(
    writer: *const GlueRocValueWriter,
    value_base: [*]u8,
    tag_variant_layout: layout.Idx,
    tag: CollectedTagInfo,
) void {
    writer.zeroValue(value_base, tag_variant_layout);
    const payload_slot = writer.recordField(value_base, tag_variant_layout, "TagVariant", "payload");
    writer.writeField(value_base, tag_variant_layout, "TagVariant", "name", RocStr, createBigRocStr(tag.name, writer.roc_ops));
    writer.writeField(value_base, tag_variant_layout, "TagVariant", "payload", RocList, buildU64RocList(writer, tag.payload_ids, payload_slot.layout_idx));
}

fn buildTagVariantList(
    writer: *const GlueRocValueWriter,
    tags: []const CollectedTagInfo,
    list_layout: layout.Idx,
) RocList {
    const allocated = writer.allocateList(list_layout, tags.len, true);
    if (allocated.bytes == null) return allocated.list;
    for (tags, 0..) |tag, i| {
        writeTagVariant(writer, allocated.bytes.? + i * allocated.elem_size, allocated.elem_layout, tag);
    }
    return allocated.list;
}

fn writeAbiFieldLayout(
    writer: *const GlueRocValueWriter,
    value_base: [*]u8,
    abi_field_layout: layout.Idx,
    field: CollectedAbiFieldLayout,
) void {
    writer.zeroValue(value_base, abi_field_layout);
    writer.writeField(value_base, abi_field_layout, "AbiFieldLayout", "alignment32", u64, field.alignment32);
    writer.writeField(value_base, abi_field_layout, "AbiFieldLayout", "alignment64", u64, field.alignment64);
    writer.writeField(value_base, abi_field_layout, "AbiFieldLayout", "is_padding", bool, field.is_padding);
    writer.writeField(value_base, abi_field_layout, "AbiFieldLayout", "name", RocStr, createBigRocStr(field.name, writer.roc_ops));
    writer.writeField(value_base, abi_field_layout, "AbiFieldLayout", "offset32", u64, field.offset32);
    writer.writeField(value_base, abi_field_layout, "AbiFieldLayout", "offset64", u64, field.offset64);
    writer.writeField(value_base, abi_field_layout, "AbiFieldLayout", "original_index", u64, field.original_index);
    writer.writeField(value_base, abi_field_layout, "AbiFieldLayout", "size32", u64, field.size32);
    writer.writeField(value_base, abi_field_layout, "AbiFieldLayout", "size64", u64, field.size64);
    writer.writeField(value_base, abi_field_layout, "AbiFieldLayout", "type_id", u64, field.type_id);
}

fn buildAbiFieldLayoutList(
    writer: *const GlueRocValueWriter,
    fields: []const CollectedAbiFieldLayout,
    list_layout: layout.Idx,
) RocList {
    const allocated = writer.allocateList(list_layout, fields.len, true);
    if (allocated.bytes == null) return allocated.list;
    for (fields, 0..) |field, i| {
        writeAbiFieldLayout(writer, allocated.bytes.? + i * allocated.elem_size, allocated.elem_layout, field);
    }
    return allocated.list;
}

fn writeAbiTagLayout(
    writer: *const GlueRocValueWriter,
    value_base: [*]u8,
    abi_tag_layout: layout.Idx,
    tag: CollectedAbiTagLayout,
) void {
    writer.zeroValue(value_base, abi_tag_layout);
    const payload_slot = writer.recordField(value_base, abi_tag_layout, "AbiTagLayout", "payload");
    const payload_fields_slot = writer.recordField(value_base, abi_tag_layout, "AbiTagLayout", "payload_fields");
    writer.writeField(value_base, abi_tag_layout, "AbiTagLayout", "discriminant", u64, tag.discriminant);
    writer.writeField(value_base, abi_tag_layout, "AbiTagLayout", "name", RocStr, createBigRocStr(tag.name, writer.roc_ops));
    writer.writeField(value_base, abi_tag_layout, "AbiTagLayout", "payload", RocList, buildU64RocList(writer, tag.payload_ids, payload_slot.layout_idx));
    writer.writeField(value_base, abi_tag_layout, "AbiTagLayout", "payload_fields", RocList, buildAbiFieldLayoutList(writer, tag.payload_fields, payload_fields_slot.layout_idx));
    writer.writeField(value_base, abi_tag_layout, "AbiTagLayout", "payload_alignment32", u64, tag.payload_alignment32);
    writer.writeField(value_base, abi_tag_layout, "AbiTagLayout", "payload_alignment64", u64, tag.payload_alignment64);
    writer.writeField(value_base, abi_tag_layout, "AbiTagLayout", "payload_size32", u64, tag.payload_size32);
    writer.writeField(value_base, abi_tag_layout, "AbiTagLayout", "payload_size64", u64, tag.payload_size64);
}

fn buildAbiTagLayoutList(
    writer: *const GlueRocValueWriter,
    tags: []const CollectedAbiTagLayout,
    list_layout: layout.Idx,
) RocList {
    const allocated = writer.allocateList(list_layout, tags.len, true);
    if (allocated.bytes == null) return allocated.list;
    for (tags, 0..) |tag, i| {
        writeAbiTagLayout(writer, allocated.bytes.? + i * allocated.elem_size, allocated.elem_layout, tag);
    }
    return allocated.list;
}

fn writeAbiLayoutDetails(
    writer: *const GlueRocValueWriter,
    value_base: [*]u8,
    details_layout: layout.Idx,
    details: CollectedAbiLayoutDetails,
) void {
    writer.zeroValue(value_base, details_layout);
    switch (details) {
        .builtin => {
            writer.writeTagDiscriminant(value_base, details_layout, writer.tagIndex("AbiLayoutDetails", "AbiBuiltin"));
        },
        .record => |fields| {
            const tag_index = writer.tagIndex("AbiLayoutDetails", "AbiRecord");
            const payload_layout = writer.variantPayloadLayout(details_layout, tag_index);
            writer.zeroValue(value_base, payload_layout);
            const fields_slot = writer.recordField(value_base, payload_layout, "AbiRecordLayout", "fields");
            writer.writeField(value_base, payload_layout, "AbiRecordLayout", "fields", RocList, buildAbiFieldLayoutList(writer, fields, fields_slot.layout_idx));
            writer.writeTagDiscriminant(value_base, details_layout, tag_index);
        },
        .tag_union => |tu| {
            const tag_index = writer.tagIndex("AbiLayoutDetails", "AbiTagUnion");
            const payload_layout = writer.variantPayloadLayout(details_layout, tag_index);
            writer.zeroValue(value_base, payload_layout);
            const tags_slot = writer.recordField(value_base, payload_layout, "AbiTagUnionLayout", "tags");
            writer.writeField(value_base, payload_layout, "AbiTagUnionLayout", "discriminant_offset32", u64, tu.discriminant_offset32);
            writer.writeField(value_base, payload_layout, "AbiTagUnionLayout", "discriminant_offset64", u64, tu.discriminant_offset64);
            writer.writeField(value_base, payload_layout, "AbiTagUnionLayout", "discriminant_size", u64, tu.discriminant_size);
            writer.writeField(value_base, payload_layout, "AbiTagUnionLayout", "tags", RocList, buildAbiTagLayoutList(writer, tu.tags, tags_slot.layout_idx));
            writer.writeTagDiscriminant(value_base, details_layout, tag_index);
        },
    }
}

fn writeAbiLayout(
    writer: *const GlueRocValueWriter,
    value_base: [*]u8,
    abi_layout_idx: layout.Idx,
    abi: CollectedAbiLayout,
) void {
    writer.zeroValue(value_base, abi_layout_idx);
    const details_slot = writer.recordField(value_base, abi_layout_idx, "AbiLayout", "details");
    writer.writeField(value_base, abi_layout_idx, "AbiLayout", "alignment32", u64, abi.size_align.alignment32);
    writer.writeField(value_base, abi_layout_idx, "AbiLayout", "alignment64", u64, abi.size_align.alignment64);
    writer.writeField(value_base, abi_layout_idx, "AbiLayout", "contains_refcounted", bool, abi.contains_refcounted);
    writeAbiLayoutDetails(writer, details_slot.ptr, details_slot.layout_idx, abi.details);
    writer.writeField(value_base, abi_layout_idx, "AbiLayout", "size32", u64, abi.size_align.size32);
    writer.writeField(value_base, abi_layout_idx, "AbiLayout", "size64", u64, abi.size_align.size64);
}

fn writeHostRcPlan(
    writer: *const GlueRocValueWriter,
    value_base: [*]u8,
    rc_layout_idx: layout.Idx,
    contains_refcounted: bool,
) void {
    writer.zeroValue(value_base, rc_layout_idx);
    const tag_name: []const u8 = if (contains_refcounted) "RcRefcounted" else "RcNoop";
    writer.writeTagDiscriminant(value_base, rc_layout_idx, writer.tagIndex("HostRcPlan", tag_name));
}

/// Serialize a CollectedTypeRepr into the exact committed TypeRepr layout.
fn writeTypeRepr(
    writer: *const GlueRocValueWriter,
    value_base: [*]u8,
    type_repr_layout: layout.Idx,
    entry: CollectedTypeRepr,
) void {
    writer.zeroValue(value_base, type_repr_layout);

    const tag_name: []const u8 = switch (entry) {
        .bool_ => "RocBool",
        .box => |box| {
            const tag_index = writer.tagIndex("TypeRepr", "RocBox");
            writer.writeValue(value_base, u64, box.inner_id);
            writer.writeTagDiscriminant(value_base, type_repr_layout, tag_index);
            return;
        },
        .dec => "RocDec",
        .f32_ => "RocF32",
        .f64_ => "RocF64",
        .i8_ => "RocI8",
        .i16_ => "RocI16",
        .i32_ => "RocI32",
        .i64_ => "RocI64",
        .i128_ => "RocI128",
        .u8_ => "RocU8",
        .u16_ => "RocU16",
        .u32_ => "RocU32",
        .u64_ => "RocU64",
        .u128_ => "RocU128",
        .str_ => "RocStr",
        .unit => "RocUnit",
        .list => |list| {
            const tag_index = writer.tagIndex("TypeRepr", "RocList");
            _ = writer.variantPayloadLayout(type_repr_layout, tag_index);
            writer.writeValue(value_base, u64, list.elem_id);
            writer.writeTagDiscriminant(value_base, type_repr_layout, tag_index);
            return;
        },
        .function => |func| {
            const tag_index = writer.tagIndex("TypeRepr", "RocFunction");
            const payload_layout = writer.variantPayloadLayout(type_repr_layout, tag_index);
            writer.zeroValue(value_base, payload_layout);
            const args_slot = writer.recordField(value_base, payload_layout, "FunctionRepr", "args");
            writer.writeField(value_base, payload_layout, "FunctionRepr", "args", RocList, buildU64RocList(writer, func.arg_ids, args_slot.layout_idx));
            writer.writeField(value_base, payload_layout, "FunctionRepr", "ret", u64, func.ret_id);
            writer.writeTagDiscriminant(value_base, type_repr_layout, tag_index);
            return;
        },
        .record => |rec| {
            const tag_index = writer.tagIndex("TypeRepr", "RocRecord");
            const payload_layout = writer.variantPayloadLayout(type_repr_layout, tag_index);
            writer.zeroValue(value_base, payload_layout);
            const fields_slot = writer.recordField(value_base, payload_layout, "RecordRepr", "fields");
            writer.writeField(value_base, payload_layout, "RecordRepr", "anonymous", bool, rec.anonymous);
            writer.writeField(value_base, payload_layout, "RecordRepr", "fields", RocList, buildRecordFieldTypeReprList(writer, rec.fields, fields_slot.layout_idx));
            writer.writeField(value_base, payload_layout, "RecordRepr", "name", RocStr, createBigRocStr(rec.name, writer.roc_ops));
            writer.writeTagDiscriminant(value_base, type_repr_layout, tag_index);
            return;
        },
        .tag_union => |tu| {
            const tag_index = writer.tagIndex("TypeRepr", "RocTagUnion");
            const payload_layout = writer.variantPayloadLayout(type_repr_layout, tag_index);
            writer.zeroValue(value_base, payload_layout);
            const tags_slot = writer.recordField(value_base, payload_layout, "TagUnionRepr", "tags");
            writer.writeField(value_base, payload_layout, "TagUnionRepr", "name", RocStr, createBigRocStr(tu.name, writer.roc_ops));
            writer.writeField(value_base, payload_layout, "TagUnionRepr", "tags", RocList, buildTagVariantList(writer, tu.tags, tags_slot.layout_idx));
            writer.writeTagDiscriminant(value_base, type_repr_layout, tag_index);
            return;
        },
        .unknown => |unknown| {
            const tag_index = writer.tagIndex("TypeRepr", "RocUnknown");
            _ = writer.variantPayloadLayout(type_repr_layout, tag_index);
            writer.writeValue(value_base, RocStr, createBigRocStr(unknown.name, writer.roc_ops));
            writer.writeTagDiscriminant(value_base, type_repr_layout, tag_index);
            return;
        },
    };
    writer.writeTagDiscriminant(value_base, type_repr_layout, writer.tagIndex("TypeRepr", tag_name));
}

fn writeTypeInfo(
    writer: *const GlueRocValueWriter,
    value_base: [*]u8,
    type_info_layout: layout.Idx,
    entry: CollectedTypeInfo,
) void {
    const abi = entry.abi orelse glueInvariant("TypeInfo row reached writer without ABI metadata", .{});
    writer.zeroValue(value_base, type_info_layout);
    const layout_slot = writer.recordField(value_base, type_info_layout, "TypeInfo", "layout");
    const rc_slot = writer.recordField(value_base, type_info_layout, "TypeInfo", "rc");
    const repr_slot = writer.recordField(value_base, type_info_layout, "TypeInfo", "repr");
    writeAbiLayout(writer, layout_slot.ptr, layout_slot.layout_idx, abi);
    writeHostRcPlan(writer, rc_slot.ptr, rc_slot.layout_idx, abi.contains_refcounted);
    writeTypeRepr(writer, repr_slot.ptr, repr_slot.layout_idx, entry.repr);
}

/// Build a RocList of TypeInfo from the type table.
fn buildTypeInfoRocList(
    writer: *const GlueRocValueWriter,
    type_table: *const TypeTable,
    list_layout: layout.Idx,
) RocList {
    const allocated = writer.allocateList(list_layout, type_table.entries.items.len, true);
    if (allocated.bytes == null) return allocated.list;

    for (type_table.entries.items, 0..) |entry, i| {
        writeTypeInfo(writer, allocated.bytes.? + i * allocated.elem_size, allocated.elem_layout, entry);
    }

    return allocated.list;
}

fn buildFunctionInfoList(
    writer: *const GlueRocValueWriter,
    functions: []const CollectedModuleTypeInfo.CollectedFunctionInfo,
    list_layout: layout.Idx,
) RocList {
    const allocated = writer.allocateList(list_layout, functions.len, true);
    if (allocated.bytes == null) return allocated.list;
    for (functions, 0..) |func, index| {
        const elem_base = allocated.bytes.? + index * allocated.elem_size;
        writer.zeroValue(elem_base, allocated.elem_layout);
        writer.writeField(elem_base, allocated.elem_layout, "FunctionInfo", "name", RocStr, createBigRocStr(func.name, writer.roc_ops));
        writer.writeField(elem_base, allocated.elem_layout, "FunctionInfo", "type_str", RocStr, createBigRocStr(func.type_str, writer.roc_ops));
    }
    return allocated.list;
}

fn buildHostedFunctionInfoList(
    writer: *const GlueRocValueWriter,
    hosted_functions: []const CollectedModuleTypeInfo.CollectedHostedFunctionInfo,
    list_layout: layout.Idx,
) RocList {
    const allocated = writer.allocateList(list_layout, hosted_functions.len, true);
    if (allocated.bytes == null) return allocated.list;
    for (hosted_functions, 0..) |hosted, index| {
        const elem_base = allocated.bytes.? + index * allocated.elem_size;
        writer.zeroValue(elem_base, allocated.elem_layout);

        const arg_fields_slot = writer.recordField(elem_base, allocated.elem_layout, "HostedFunctionInfo", "arg_fields");
        const arg_type_ids_slot = writer.recordField(elem_base, allocated.elem_layout, "HostedFunctionInfo", "arg_type_ids");
        const ret_fields_slot = writer.recordField(elem_base, allocated.elem_layout, "HostedFunctionInfo", "ret_fields");

        writer.writeField(elem_base, allocated.elem_layout, "HostedFunctionInfo", "arg_fields", RocList, buildRecordFieldsRocList(writer, hosted.arg_fields, arg_fields_slot.layout_idx));
        writer.writeField(elem_base, allocated.elem_layout, "HostedFunctionInfo", "arg_type_ids", RocList, buildU64RocList(writer, hosted.arg_type_ids, arg_type_ids_slot.layout_idx));
        writer.writeField(elem_base, allocated.elem_layout, "HostedFunctionInfo", "ffi_symbol", RocStr, createBigRocStr(hosted.ffi_symbol, writer.roc_ops));
        writer.writeField(elem_base, allocated.elem_layout, "HostedFunctionInfo", "index", u64, hosted.index);
        writer.writeField(elem_base, allocated.elem_layout, "HostedFunctionInfo", "name", RocStr, createBigRocStr(hosted.name, writer.roc_ops));
        writer.writeField(elem_base, allocated.elem_layout, "HostedFunctionInfo", "ret_fields", RocList, buildRecordFieldsRocList(writer, hosted.ret_fields, ret_fields_slot.layout_idx));
        writer.writeField(elem_base, allocated.elem_layout, "HostedFunctionInfo", "ret_type_id", u64, hosted.ret_type_id);
        writer.writeField(elem_base, allocated.elem_layout, "HostedFunctionInfo", "type_str", RocStr, createBigRocStr(hosted.type_str, writer.roc_ops));
    }
    return allocated.list;
}

fn buildModuleTypeInfoList(
    writer: *const GlueRocValueWriter,
    modules: []const CollectedModuleTypeInfo,
    list_layout: layout.Idx,
) RocList {
    const allocated = writer.allocateList(list_layout, modules.len, true);
    if (allocated.bytes == null) return allocated.list;
    for (modules, 0..) |module, index| {
        const elem_base = allocated.bytes.? + index * allocated.elem_size;
        writer.zeroValue(elem_base, allocated.elem_layout);

        const functions_slot = writer.recordField(elem_base, allocated.elem_layout, "ModuleTypeInfo", "functions");
        const hosted_functions_slot = writer.recordField(elem_base, allocated.elem_layout, "ModuleTypeInfo", "hosted_functions");

        writer.writeField(elem_base, allocated.elem_layout, "ModuleTypeInfo", "functions", RocList, buildFunctionInfoList(writer, module.functions.items, functions_slot.layout_idx));
        writer.writeField(elem_base, allocated.elem_layout, "ModuleTypeInfo", "hosted_functions", RocList, buildHostedFunctionInfoList(writer, module.hosted_functions.items, hosted_functions_slot.layout_idx));
        writer.writeField(elem_base, allocated.elem_layout, "ModuleTypeInfo", "main_type", RocStr, createBigRocStr(module.main_type, writer.roc_ops));
        writer.writeField(elem_base, allocated.elem_layout, "ModuleTypeInfo", "name", RocStr, createBigRocStr(module.name, writer.roc_ops));
    }
    return allocated.list;
}

fn buildEntryPointList(
    writer: *const GlueRocValueWriter,
    platform_info: *const PlatformHeaderInfo,
    entrypoint_type_ids: *const std.StringHashMap(u64),
    list_layout: layout.Idx,
) RocList {
    const allocated = writer.allocateList(list_layout, platform_info.requires_entries.len, true);
    if (allocated.bytes == null) return allocated.list;
    for (platform_info.requires_entries, 0..) |entry, index| {
        const elem_base = allocated.bytes.? + index * allocated.elem_size;
        writer.zeroValue(elem_base, allocated.elem_layout);
        writer.writeField(elem_base, allocated.elem_layout, "EntryPoint", "name", RocStr, createBigRocStr(entry.name, writer.roc_ops));
        const type_id = entrypoint_type_ids.get(entry.name) orelse
            glueInvariant("entrypoint '{s}' missing reflected type id", .{entry.name});
        writer.writeField(elem_base, allocated.elem_layout, "EntryPoint", "type_id", u64, type_id);
    }
    return allocated.list;
}

fn buildProvidesEntryList(
    writer: *const GlueRocValueWriter,
    provides_entries: []const PlatformHeaderInfo.ProvidesEntry,
    provides_type_ids: *const std.StringHashMap(u64),
    list_layout: layout.Idx,
) RocList {
    const allocated = writer.allocateList(list_layout, provides_entries.len, true);
    if (allocated.bytes == null) return allocated.list;
    for (provides_entries, 0..) |entry, index| {
        const elem_base = allocated.bytes.? + index * allocated.elem_size;
        writer.zeroValue(elem_base, allocated.elem_layout);
        writer.writeField(elem_base, allocated.elem_layout, "ProvidesEntry", "ffi_symbol", RocStr, createBigRocStr(entry.ffi_symbol, writer.roc_ops));
        writer.writeField(elem_base, allocated.elem_layout, "ProvidesEntry", "name", RocStr, createBigRocStr(entry.name, writer.roc_ops));
        const type_id = provides_type_ids.get(entry.ffi_symbol) orelse
            glueInvariant("provided symbol '{s}' missing reflected type id", .{entry.ffi_symbol});
        writer.writeField(elem_base, allocated.elem_layout, "ProvidesEntry", "type_id", u64, type_id);
    }
    return allocated.list;
}

/// Construct the List(Types) Roc value from collected module type info.
fn constructTypesRocList(
    writer: *const GlueRocValueWriter,
    collected_modules: []const CollectedModuleTypeInfo,
    platform_info: *const PlatformHeaderInfo,
    provides_entries: []const PlatformHeaderInfo.ProvidesEntry,
    type_table: *const TypeTable,
    entrypoint_type_ids: *const std.StringHashMap(u64),
    provides_type_ids: *const std.StringHashMap(u64),
    list_layout: layout.Idx,
) RocList {
    const allocated = writer.allocateList(list_layout, 1, true);
    const bytes = allocated.bytes orelse glueInvariant("List(Types) layout unexpectedly had no element bytes", .{});
    const types_base = bytes;
    writer.zeroValue(types_base, allocated.elem_layout);

    const entrypoints_slot = writer.recordField(types_base, allocated.elem_layout, "Types", "entrypoints");
    const modules_slot = writer.recordField(types_base, allocated.elem_layout, "Types", "modules");
    const provides_slot = writer.recordField(types_base, allocated.elem_layout, "Types", "provides_entries");
    const types_slot = writer.recordField(types_base, allocated.elem_layout, "Types", "types");

    writer.writeField(types_base, allocated.elem_layout, "Types", "entrypoints", RocList, buildEntryPointList(writer, platform_info, entrypoint_type_ids, entrypoints_slot.layout_idx));
    writer.writeField(types_base, allocated.elem_layout, "Types", "modules", RocList, buildModuleTypeInfoList(writer, collected_modules, modules_slot.layout_idx));
    writer.writeField(types_base, allocated.elem_layout, "Types", "provides_entries", RocList, buildProvidesEntryList(writer, provides_entries, provides_type_ids, provides_slot.layout_idx));
    writer.writeField(types_base, allocated.elem_layout, "Types", "types", RocList, buildTypeInfoRocList(writer, type_table, types_slot.layout_idx));

    return allocated.list;
}

/// Extract files from a Try(List(File), Str) result buffer.
/// Returns the file list on Ok, or an error message on Err.
const GlueResultFile = struct {
    name: []const u8,
    content: []const u8,
};

const GlueResultFiles = struct {
    allocator: Allocator,
    files: []const GlueResultFile,
    err_msg: ?[]const u8,

    fn deinit(self: GlueResultFiles) void {
        for (self.files) |file| {
            self.allocator.free(file.name);
            self.allocator.free(file.content);
        }
        if (self.files.len > 0) self.allocator.free(self.files);
        if (self.err_msg) |err_msg| self.allocator.free(err_msg);
    }
};

fn copyRocStrSlice(allocator: Allocator, str: RocStr) Allocator.Error![]const u8 {
    return try allocator.dupe(u8, str.asSlice());
}

fn extractGlueResult(
    allocator: Allocator,
    writer: *const GlueRocValueWriter,
    result_base: [*]const u8,
    result_layout: layout.Idx,
) Allocator.Error!GlueResultFiles {
    const ok_index = writer.tagIndex("Builtin.Try", "Ok");
    const err_index = writer.tagIndex("Builtin.Try", "Err");
    const discriminant = writer.readTagDiscriminant(result_base, result_layout);

    if (discriminant == ok_index) {
        const files_list_layout = writer.variantPayloadLayout(result_layout, ok_index);
        const files = writer.readValue(result_base, RocList);
        if (files.len() == 0 or files.bytes == null) {
            return .{ .allocator = allocator, .files = &.{}, .err_msg = null };
        }

        const file_layout = writer.listElementLayout(files_list_layout);
        const file_size = writer.sizeOf(file_layout);
        const out = try allocator.alloc(GlueResultFile, files.len());
        const file_bytes = files.bytes.?;
        for (out, 0..) |*file, index| {
            const file_base = file_bytes + index * file_size;
            const name_slot = writer.recordField(file_base, file_layout, "File", "name");
            const content_slot = writer.recordField(file_base, file_layout, "File", "content");
            const name = writer.readValue(name_slot.ptr, RocStr);
            const content = writer.readValue(content_slot.ptr, RocStr);
            file.* = .{
                .name = try copyRocStrSlice(allocator, name),
                .content = try copyRocStrSlice(allocator, content),
            };
        }
        return .{ .allocator = allocator, .files = out, .err_msg = null };
    }

    if (discriminant == err_index) {
        _ = writer.variantPayloadLayout(result_layout, err_index);
        const err = writer.readValue(result_base, RocStr);
        return .{ .allocator = allocator, .files = &.{}, .err_msg = try copyRocStrSlice(allocator, err) };
    }

    glueInvariant("glue result Try discriminant {d} was neither Ok nor Err", .{discriminant});
}

fn typeTableKeysEqual(a: []const TypeTableKey, b: []const TypeTableKey) bool {
    if (a.len != b.len) return false;
    for (a, b) |x, y| {
        if (!checkedArtifactKeysEqual(x.artifact_key, y.artifact_key)) return false;
        if (x.checked_type != y.checked_type) return false;
    }
    return true;
}

fn checkedTypePayload(
    artifact: *const CheckedArtifact.CheckedModuleArtifact,
    checked_type: CheckedArtifact.CheckedTypeId,
) CheckedArtifact.CheckedTypePayload {
    const idx = @intFromEnum(checked_type);
    if (idx >= artifact.checked_types.payloadCount()) {
        glueInvariant("checked type id {d} out of bounds", .{idx});
    }
    return artifact.checked_types.payload(checked_type);
}

fn checkedTypeRootForScheme(
    artifact: *const CheckedArtifact.CheckedModuleArtifact,
    scheme_key: check.CanonicalNames.CanonicalTypeSchemeKey,
) CheckedArtifact.CheckedTypeId {
    return (artifact.checked_types.schemeForKey(scheme_key) orelse
        glueInvariant("checked type scheme missing from artifact", .{})).root;
}

fn appendRecordRowFields(
    gpa: std.mem.Allocator,
    artifact: *const CheckedArtifact.CheckedModuleArtifact,
    head: []const CheckedArtifact.CheckedRecordField,
    ext: ?CheckedArtifact.CheckedTypeId,
    fields: *std.ArrayList(CheckedArtifact.CheckedRecordField),
) Allocator.Error!void {
    try fields.appendSlice(gpa, head);

    var current = ext;
    var seen = std.AutoHashMap(CheckedArtifact.CheckedTypeId, void).init(gpa);
    defer seen.deinit();

    while (current) |current_id| {
        if (seen.contains(current_id)) break;
        try seen.put(current_id, {});

        switch (checkedTypePayload(artifact, current_id)) {
            .alias => |alias| current = alias.backing,
            .empty_record => break,
            .flex, .rigid => |variable| {
                if (variable.row_default == .empty_record) break;
                glueInvariant("open non-record checked row reached glue record conversion", .{});
            },
            .record => |record| {
                try fields.appendSlice(gpa, record.fields);
                current = record.ext;
            },
            .record_unbound => |tail_fields| {
                try fields.appendSlice(gpa, tail_fields);
                break;
            },
            else => glueInvariant("non-record checked row reached glue record conversion", .{}),
        }
    }
}

fn sortRecordFieldsByName(
    artifact: *const CheckedArtifact.CheckedModuleArtifact,
    fields: []CheckedArtifact.CheckedRecordField,
) void {
    std.mem.sort(CheckedArtifact.CheckedRecordField, fields, &artifact.canonical_names, checkedRecordFieldLessThan);
}

fn checkedRecordFieldLessThan(
    names: *const CanonicalNameStore,
    lhs: CheckedArtifact.CheckedRecordField,
    rhs: CheckedArtifact.CheckedRecordField,
) bool {
    return names.recordFieldLabelTextLessThan(lhs.name, rhs.name);
}

fn appendTagRowTags(
    gpa: std.mem.Allocator,
    artifact: *const CheckedArtifact.CheckedModuleArtifact,
    head: []const CheckedArtifact.CheckedTag,
    ext: ?CheckedArtifact.CheckedTypeId,
    tags: *std.ArrayList(CheckedArtifact.CheckedTag),
) Allocator.Error!void {
    try tags.appendSlice(gpa, head);

    var current = ext;
    var seen = std.AutoHashMap(CheckedArtifact.CheckedTypeId, void).init(gpa);
    defer seen.deinit();

    while (current) |current_id| {
        if (seen.contains(current_id)) break;
        try seen.put(current_id, {});

        switch (checkedTypePayload(artifact, current_id)) {
            .alias => |alias| current = alias.backing,
            .empty_tag_union => break,
            .flex, .rigid => |variable| {
                if (variable.row_default == .empty_tag_union) break;
                glueInvariant("open non-tag checked row reached glue tag-union conversion", .{});
            },
            .tag_union => |tag_union| {
                try tags.appendSlice(gpa, tag_union.tags);
                current = tag_union.ext;
            },
            else => glueInvariant("non-tag checked row reached glue tag-union conversion", .{}),
        }
    }
}

fn typeStringAlloc(
    gpa: std.mem.Allocator,
    artifact: *const CheckedArtifact.CheckedModuleArtifact,
    checked_type: CheckedArtifact.CheckedTypeId,
) Allocator.Error![]const u8 {
    var buf = std.ArrayList(u8).empty;
    errdefer buf.deinit(gpa);
    var active = std.AutoHashMap(CheckedArtifact.CheckedTypeId, void).init(gpa);
    defer active.deinit();
    try writeTypeString(gpa, artifact, checked_type, &buf, &active);
    return buf.toOwnedSlice(gpa);
}

fn writeTypeString(
    gpa: std.mem.Allocator,
    artifact: *const CheckedArtifact.CheckedModuleArtifact,
    checked_type: CheckedArtifact.CheckedTypeId,
    buf: *std.ArrayList(u8),
    active: *std.AutoHashMap(CheckedArtifact.CheckedTypeId, void),
) Allocator.Error!void {
    if (active.contains(checked_type)) {
        try buf.appendSlice(gpa, "<cycle>");
        return;
    }
    try active.put(checked_type, {});
    defer _ = active.remove(checked_type);

    switch (checkedTypePayload(artifact, checked_type)) {
        .pending => glueInvariant("pending checked type reached glue type string", .{}),
        .err => glueInvariant("erroneous checked type reached glue type string", .{}),
        .flex => try buf.appendSlice(gpa, "flex"),
        .rigid => try buf.appendSlice(gpa, "rigid"),
        .alias => |alias| try writeTypeString(gpa, artifact, alias.backing, buf, active),
        .record => |record| try writeRecordTypeString(gpa, artifact, record.fields, record.ext, buf, active),
        .record_unbound => |fields| try writeRecordTypeString(gpa, artifact, fields, null, buf, active),
        .tuple => |items| try writeTupleTypeString(gpa, artifact, items, buf, active),
        .nominal => |nominal| try writeNominalTypeString(gpa, artifact, nominal, buf, active),
        .function => |func| try writeFunctionTypeString(gpa, artifact, func, buf, active),
        .empty_record => try buf.appendSlice(gpa, "{}"),
        .tag_union => |tag_union| try writeTagUnionTypeString(gpa, artifact, tag_union.tags, tag_union.ext, buf, active),
        .empty_tag_union => try buf.appendSlice(gpa, "[]"),
    }
}

fn writeNominalTypeString(
    gpa: std.mem.Allocator,
    artifact: *const CheckedArtifact.CheckedModuleArtifact,
    nominal: CheckedArtifact.CheckedNominalType,
    buf: *std.ArrayList(u8),
    active: *std.AutoHashMap(CheckedArtifact.CheckedTypeId, void),
) Allocator.Error!void {
    const name = TypeTable.getTypeDisplayName(artifact.canonical_names.typeNameText(nominal.name));
    try buf.appendSlice(gpa, name);
    if (nominal.args.len == 0) return;
    try buf.append(gpa, '(');
    for (nominal.args, 0..) |arg, i| {
        if (i > 0) try buf.appendSlice(gpa, ", ");
        try writeTypeString(gpa, artifact, arg, buf, active);
    }
    try buf.append(gpa, ')');
}

fn writeFunctionTypeString(
    gpa: std.mem.Allocator,
    artifact: *const CheckedArtifact.CheckedModuleArtifact,
    func: CheckedArtifact.CheckedFunctionType,
    buf: *std.ArrayList(u8),
    active: *std.AutoHashMap(CheckedArtifact.CheckedTypeId, void),
) Allocator.Error!void {
    if (func.args.len == 0) {
        try buf.appendSlice(gpa, "{}");
    } else {
        for (func.args, 0..) |arg, i| {
            if (i > 0) try buf.appendSlice(gpa, ", ");
            try writeTypeString(gpa, artifact, arg, buf, active);
        }
    }
    try buf.appendSlice(gpa, if (func.kind == .effectful) " => " else " -> ");
    try writeTypeString(gpa, artifact, func.ret, buf, active);
}

fn writeRecordTypeString(
    gpa: std.mem.Allocator,
    artifact: *const CheckedArtifact.CheckedModuleArtifact,
    fields: []const CheckedArtifact.CheckedRecordField,
    ext: ?CheckedArtifact.CheckedTypeId,
    buf: *std.ArrayList(u8),
    active: *std.AutoHashMap(CheckedArtifact.CheckedTypeId, void),
) Allocator.Error!void {
    var all_fields = std.ArrayList(CheckedArtifact.CheckedRecordField).empty;
    defer all_fields.deinit(gpa);
    try appendRecordRowFields(gpa, artifact, fields, ext, &all_fields);

    if (all_fields.items.len == 0) {
        try buf.appendSlice(gpa, "{}");
        return;
    }

    var indices = try gpa.alloc(usize, all_fields.items.len);
    defer gpa.free(indices);
    for (0..all_fields.items.len) |i| indices[i] = i;
    const SortCtx = struct {
        fields: []const CheckedArtifact.CheckedRecordField,
        names: *const CanonicalNameStore,

        pub fn lessThan(ctx: @This(), a: usize, b: usize) bool {
            return std.mem.lessThan(
                u8,
                ctx.names.recordFieldLabelText(ctx.fields[a].name),
                ctx.names.recordFieldLabelText(ctx.fields[b].name),
            );
        }
    };
    std.mem.sort(usize, indices, SortCtx{ .fields = all_fields.items, .names = &artifact.canonical_names }, SortCtx.lessThan);

    try buf.appendSlice(gpa, "{ ");
    for (indices, 0..) |src_idx, i| {
        if (i > 0) try buf.appendSlice(gpa, ", ");
        const field = all_fields.items[src_idx];
        try buf.appendSlice(gpa, artifact.canonical_names.recordFieldLabelText(field.name));
        try buf.appendSlice(gpa, " : ");
        try writeTypeString(gpa, artifact, field.ty, buf, active);
    }
    try buf.appendSlice(gpa, " }");
}

fn writeTupleTypeString(
    gpa: std.mem.Allocator,
    artifact: *const CheckedArtifact.CheckedModuleArtifact,
    items: []const CheckedArtifact.CheckedTypeId,
    buf: *std.ArrayList(u8),
    active: *std.AutoHashMap(CheckedArtifact.CheckedTypeId, void),
) Allocator.Error!void {
    try buf.append(gpa, '(');
    for (items, 0..) |item, i| {
        if (i > 0) try buf.appendSlice(gpa, ", ");
        try writeTypeString(gpa, artifact, item, buf, active);
    }
    try buf.append(gpa, ')');
}

fn writeTagUnionTypeString(
    gpa: std.mem.Allocator,
    artifact: *const CheckedArtifact.CheckedModuleArtifact,
    tags: []const CheckedArtifact.CheckedTag,
    ext: CheckedArtifact.CheckedTypeId,
    buf: *std.ArrayList(u8),
    active: *std.AutoHashMap(CheckedArtifact.CheckedTypeId, void),
) Allocator.Error!void {
    var all_tags = std.ArrayList(CheckedArtifact.CheckedTag).empty;
    defer all_tags.deinit(gpa);
    try appendTagRowTags(gpa, artifact, tags, ext, &all_tags);

    try buf.append(gpa, '[');
    for (all_tags.items, 0..) |tag, i| {
        if (i > 0) try buf.appendSlice(gpa, ", ");
        try buf.appendSlice(gpa, artifact.canonical_names.tagLabelText(tag.name));
        const tag_args = tag.argsSlice(&artifact.checked_types);
        if (tag_args.len > 0) {
            try buf.append(gpa, '(');
            for (tag_args, 0..) |arg, arg_i| {
                if (arg_i > 0) try buf.appendSlice(gpa, ", ");
                try writeTypeString(gpa, artifact, arg, buf, active);
            }
            try buf.append(gpa, ')');
        }
    }
    try buf.append(gpa, ']');
}

/// Collect type information from a published checked artifact.
fn collectModuleTypeInfo(
    gpa: Allocator,
    artifact: *const CheckedArtifact.CheckedModuleArtifact,
    module_name: []const u8,
    hosted_indices: []const HostedProcGlobalIndex,
    hosted_symbols: *const std.StringHashMap([]const u8),
    type_table: *TypeTable,
) TypeTableError!?CollectedModuleTypeInfo {
    var main_type_str: []const u8 = try gpa.dupe(u8, "");
    errdefer gpa.free(main_type_str);
    for (artifact.checked_types.nominal_declarations.items) |declaration| {
        const type_name = TypeTable.getTypeDisplayName(artifact.canonical_names.typeNameText(declaration.nominal.type_name));
        if (std.mem.eql(u8, type_name, module_name)) {
            gpa.free(main_type_str);
            main_type_str = try typeStringAlloc(gpa, artifact, declaration.declaration_root);
            break;
        }
    }

    // Collect functions
    var functions = std.ArrayList(CollectedModuleTypeInfo.CollectedFunctionInfo).empty;
    errdefer {
        for (functions.items) |f| {
            gpa.free(f.name);
            gpa.free(f.type_str);
        }
        functions.deinit(gpa);
    }
    var hosted_functions = std.ArrayList(CollectedModuleTypeInfo.CollectedHostedFunctionInfo).empty;
    errdefer {
        for (hosted_functions.items) |h| {
            gpa.free(h.ffi_symbol);
            gpa.free(h.name);
            gpa.free(h.type_str);
            for (h.arg_fields) |field| {
                gpa.free(field.name);
                gpa.free(field.type_str);
            }
            gpa.free(h.arg_fields);
            for (h.ret_fields) |field| {
                gpa.free(field.name);
                gpa.free(field.type_str);
            }
            gpa.free(h.ret_fields);
            if (h.arg_type_ids.len > 0) gpa.free(h.arg_type_ids);
        }
        hosted_functions.deinit(gpa);
    }

    // Membership and local names come from the module's structured method
    // tables (owner declaration + bare member ident), not from re-parsing
    // qualified export-name strings.
    const module_env = artifact.moduleEnvConst();
    var member_by_def = std.AutoHashMap(can.CIR.Def.Idx, can.ModuleEnv.MethodKey).init(gpa);
    defer member_by_def.deinit();
    for (module_env.method_defs.entries.items) |member_entry| {
        try member_by_def.put(member_entry.value.def_idx, member_entry.key);
    }

    for (artifact.top_level_values.entries) |entry| {
        const def_idx = entry.def;

        _ = member_by_def.get(def_idx) orelse continue;
        const source_name = artifact.canonical_names.exportNameText(entry.source_name);
        const local_name = try moduleLocalMemberName(gpa, module_name, source_name);
        defer gpa.free(local_name);

        const checked_type = checkedTypeRootForScheme(artifact, entry.source_scheme);
        const type_str = try typeStringAlloc(gpa, artifact, checked_type);
        errdefer gpa.free(type_str);

        if (hostedProcForDef(&artifact.hosted_procs, def_idx)) |hosted_proc| {
            // Extract record fields from function arg and return types.
            var arg_fields: []const CollectedModuleTypeInfo.CollectedRecordFieldInfo = &.{};
            errdefer {
                for (arg_fields) |field| {
                    gpa.free(field.name);
                    gpa.free(field.type_str);
                }
                gpa.free(arg_fields);
            }
            var ret_fields: []const CollectedModuleTypeInfo.CollectedRecordFieldInfo = &.{};
            errdefer {
                for (ret_fields) |field| {
                    gpa.free(field.name);
                    gpa.free(field.type_str);
                }
                gpa.free(ret_fields);
            }
            var arg_type_ids: []const u64 = &.{};
            errdefer if (arg_type_ids.len > 0) gpa.free(arg_type_ids);
            var ret_type_id: u64 = 0;

            if (try type_table.collectHostedFunctionMetadata(artifact, checked_type)) |metadata| {
                ret_fields = metadata.ret_fields;
                arg_fields = metadata.arg_fields;
                ret_type_id = metadata.ret_type_id;
                arg_type_ids = metadata.arg_type_ids;
            } else {
                ret_type_id = try type_table.insertUnit();
            }

            const hosted_key = hosted_proc.orderKey(&artifact.hosted_procs);
            const hosted_symbol = hosted_symbols.get(hosted_key) orelse
                glueInvariant("hosted function '{s}' has no platform hosted symbol", .{hosted_key});
            const ffi_symbol = try gpa.dupe(u8, hosted_symbol);
            errdefer gpa.free(ffi_symbol);
            const name = try gpa.dupe(u8, local_name);
            errdefer gpa.free(name);
            try hosted_functions.append(gpa, .{
                .index = hostedGlobalIndexForDef(hosted_indices, artifact.key, def_idx),
                .ffi_symbol = ffi_symbol,
                .name = name,
                .type_str = type_str,
                .arg_fields = arg_fields,
                .ret_fields = ret_fields,
                .arg_type_ids = arg_type_ids,
                .ret_type_id = ret_type_id,
            });
        } else switch (entry.value) {
            .procedure_binding => {
                const name = try gpa.dupe(u8, local_name);
                errdefer gpa.free(name);
                try functions.append(gpa, .{
                    .name = name,
                    .type_str = type_str,
                });
            },
            .const_ref => gpa.free(type_str),
        }
    }

    return CollectedModuleTypeInfo{
        .name = try gpa.dupe(u8, module_name),
        .main_type = main_type_str,
        .functions = functions,
        .hosted_functions = hosted_functions,
    };
}

fn moduleLocalMemberName(
    allocator: Allocator,
    module_name: []const u8,
    source_name: []const u8,
) Allocator.Error![]const u8 {
    if (module_name.len == 0) return try allocator.dupe(u8, source_name);
    if (!std.mem.startsWith(u8, source_name, module_name)) return try allocator.dupe(u8, source_name);
    if (source_name.len == module_name.len) return try allocator.dupe(u8, source_name);
    if (source_name[module_name.len] != '.') return try allocator.dupe(u8, source_name);
    return try allocator.dupe(u8, source_name[module_name.len + 1 ..]);
}

/// Print a type annotation to a buffer (for requires entries which use AST types)
fn printTypeAnnoToBuf(gpa: std.mem.Allocator, env: *ModuleEnv, ast: *const parse.AST, type_anno_idx: parse.AST.TypeAnno.Idx, buf: *std.ArrayList(u8)) Allocator.Error!void {
    const type_anno = ast.store.getTypeAnno(type_anno_idx);

    switch (type_anno) {
        .@"fn" => |f| {
            const arrow = if (f.effectful) "=>" else "->";
            const args = ast.store.typeAnnoSlice(f.args);
            if (args.len == 0) {
                try buf.appendSlice(gpa, "()");
            } else {
                for (args, 0..) |arg_idx, i| {
                    if (i > 0) try buf.appendSlice(gpa, ", ");
                    try printTypeAnnoToBuf(gpa, env, ast, arg_idx, buf);
                }
            }
            try buf.appendSlice(gpa, " ");
            try buf.appendSlice(gpa, arrow);
            try buf.appendSlice(gpa, " ");
            try printTypeAnnoToBuf(gpa, env, ast, f.ret, buf);
        },
        .ty => |t| {
            // Print qualified type name
            const qualifiers = ast.store.tokenSlice(t.qualifiers);
            for (qualifiers) |qual_tok_idx| {
                const qual_tok: parse.tokenize.Token.Idx = @intCast(qual_tok_idx);
                if (ast.tokens.resolveIdentifier(qual_tok)) |ident_idx| {
                    try buf.appendSlice(gpa, env.common.getIdent(ident_idx));
                    try buf.append(gpa, '.');
                }
            }
            if (ast.tokens.resolveIdentifier(t.token)) |ident_idx| {
                try buf.appendSlice(gpa, env.common.getIdent(ident_idx));
            }
        },
        .ty_var => |tv| {
            if (ast.tokens.resolveIdentifier(tv.tok)) |ident_idx| {
                try buf.appendSlice(gpa, env.common.getIdent(ident_idx));
            }
        },
        .record => |r| {
            try buf.appendSlice(gpa, "{ ");
            const fields = ast.store.annoRecordFieldSlice(r.fields);
            for (fields, 0..) |field_idx, i| {
                if (i > 0) try buf.appendSlice(gpa, ", ");
                const field = ast.store.getAnnoRecordField(field_idx) catch continue;
                if (ast.tokens.resolveIdentifier(field.name)) |ident_idx| {
                    try buf.appendSlice(gpa, env.common.getIdent(ident_idx));
                    try buf.appendSlice(gpa, " : ");
                }
                try printTypeAnnoToBuf(gpa, env, ast, field.ty, buf);
            }
            switch (r.ext) {
                .closed => {},
                .open => try buf.appendSlice(gpa, ", .."),
                .named => |named| {
                    try buf.appendSlice(gpa, ", ..");
                    try printTypeAnnoToBuf(gpa, env, ast, named.anno, buf);
                },
            }
            try buf.appendSlice(gpa, " }");
        },
        .tag_union => |tu| {
            try buf.append(gpa, '[');
            const tags = ast.store.typeAnnoSlice(tu.tags);
            for (tags, 0..) |tag_idx, i| {
                if (i > 0) try buf.appendSlice(gpa, ", ");
                try printTypeAnnoToBuf(gpa, env, ast, tag_idx, buf);
            }
            switch (tu.ext) {
                .closed => {},
                .open => try buf.appendSlice(gpa, ", .."),
                .named => |named| {
                    try buf.appendSlice(gpa, ", ..");
                    try printTypeAnnoToBuf(gpa, env, ast, named.anno, buf);
                },
            }
            try buf.append(gpa, ']');
        },
        .tuple => |t| {
            try buf.append(gpa, '(');
            const annos = ast.store.typeAnnoSlice(t.annos);
            for (annos, 0..) |anno_idx, i| {
                if (i > 0) try buf.appendSlice(gpa, ", ");
                try printTypeAnnoToBuf(gpa, env, ast, anno_idx, buf);
            }
            try buf.append(gpa, ')');
        },
        .apply => |a| {
            const args = ast.store.typeAnnoSlice(a.args);
            if (args.len > 0) {
                try printTypeAnnoToBuf(gpa, env, ast, args[0], buf);
                if (args.len > 1) {
                    try buf.append(gpa, ' ');
                    for (args[1..], 0..) |arg_idx, i| {
                        if (i > 0) try buf.append(gpa, ' ');
                        try printTypeAnnoToBuf(gpa, env, ast, arg_idx, buf);
                    }
                }
            }
        },
        .parens => |p| {
            try buf.append(gpa, '(');
            try printTypeAnnoToBuf(gpa, env, ast, p.anno, buf);
            try buf.append(gpa, ')');
        },
        .underscore => {
            try buf.append(gpa, '_');
        },
        .underscore_type_var => {
            try buf.append(gpa, '_');
        },
        .malformed => {
            try buf.appendSlice(gpa, "<malformed>");
        },
    }
}

/// Generate a stub expression from a type annotation.
/// This produces valid Roc expressions that will crash at runtime rather than compile-time.
/// Uses `...` inside lambdas to defer the crash to runtime.
fn generateStubExprFromTypeAnno(gpa: std.mem.Allocator, env: *ModuleEnv, ast: *const parse.AST, type_anno_idx: parse.AST.TypeAnno.Idx, buf: *std.ArrayList(u8)) Allocator.Error!void {
    const type_anno = ast.store.getTypeAnno(type_anno_idx);

    switch (type_anno) {
        .@"fn" => |f| {
            // Generate lambda stub
            const args = ast.store.typeAnnoSlice(f.args);
            if (args.len == 0) {
                // No args: || body
                try buf.appendSlice(gpa, "|| ");
            } else {
                // Has args: |_, _, ...| body
                try buf.append(gpa, '|');
                for (0..args.len) |i| {
                    if (i > 0) try buf.appendSlice(gpa, ", ");
                    try buf.append(gpa, '_');
                }
                try buf.appendSlice(gpa, "| ");
            }

            // Check if return type is unit {}
            const ret_anno = ast.store.getTypeAnno(f.ret);
            if (ret_anno == .record) {
                const record = ret_anno.record;
                const fields = ast.store.annoRecordFieldSlice(record.fields);
                if (fields.len == 0 and record.ext == .closed) {
                    // Return type is {} (unit) - return empty record
                    try buf.appendSlice(gpa, "{}");
                    return;
                }
            }

            // Non-unit return type - use { ... } to crash at runtime (not compile-time)
            // The block syntax is required for single-line lambdas
            try buf.appendSlice(gpa, "{ ... }");
        },
        .record => |r| {
            try buf.appendSlice(gpa, "{ ");
            const fields = ast.store.annoRecordFieldSlice(r.fields);
            for (fields, 0..) |field_idx, i| {
                if (i > 0) try buf.appendSlice(gpa, ", ");
                const field = ast.store.getAnnoRecordField(field_idx) catch continue;
                if (ast.tokens.resolveIdentifier(field.name)) |ident_idx| {
                    try buf.appendSlice(gpa, env.common.getIdent(ident_idx));
                    try buf.appendSlice(gpa, ": ");
                }
                try generateStubExprFromTypeAnno(gpa, env, ast, field.ty, buf);
            }
            try buf.appendSlice(gpa, " }");
        },
        else => {
            // For all other types, use { ... } to crash at runtime
            try buf.appendSlice(gpa, "{ ... }");
        },
    }
}
