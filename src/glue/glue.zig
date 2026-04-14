//! Glue code generation for Roc platforms.
//!
//! This module handles the `roc glue` command, which generates platform-specific
//! binding code (e.g., Zig structs) from a platform's type information.
//!
//! The pipeline:
//! 1. Parse platform header to extract requires entries and type aliases
//! 2. Compile the platform via BuildEnv with a synthetic app, publishing checked artifacts
//! 3. Collect hosted functions and module type info from checked artifacts
//! 4. Build the glue input type table from artifact-owned checked type data
//! 5. Materialize the glue input as Roc C-ABI values
//! 6. Compile the glue spec through checked artifacts, lower to LIR, and run the LIR interpreter

const std = @import("std");
const builtin = @import("builtin");
const Allocator = std.mem.Allocator;
const base = @import("base");
const parse = @import("parse");
const compile = @import("compile");
const check = @import("check");
const can = @import("can");
const echo_platform = @import("echo_platform");
const roc_target = @import("roc_target");
const layout = @import("layout");
const lir = @import("lir");

const ModuleEnv = can.ModuleEnv;
const BuildEnv = compile.BuildEnv;
const RocTarget = roc_target.RocTarget;
const CheckedArtifact = check.CheckedArtifact;
const CanonicalNameStore = check.CanonicalNames.CanonicalNameStore;
const CIR = can.CIR;

const builtins = @import("builtins");
const RocStr = builtins.str.RocStr;
const RocList = builtins.list.RocList;

const eval_mod = @import("eval");
const EvalBackend = eval_mod.EvalBackend;

/// Arguments for glue code generation.
pub const GlueArgs = struct {
    glue_spec: []const u8,
    output_dir: []const u8,
    platform_path: []const u8,
    backend: EvalBackend = .dev,
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
    ModuleRetrieval,
    OutOfMemory,
};

/// Print platform glue information for a platform's main.roc file using the checked-artifact pipeline.
/// Hosted function ordering comes from published `HostedProcTable` records.
pub fn rocGlue(gpa: Allocator, stderr: *std.Io.Writer, stdout: *std.Io.Writer, args: GlueArgs, temp_dir: []const u8) GlueError!void {
    rocGlueInner(gpa, stderr, stdout, args, temp_dir) catch |err| {
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
            error.ModuleRetrieval => stderr.print("Error: Failed to get compiled modules\n", .{}),
            error.OutOfMemory => stderr.print("Error: Out of memory\n", .{}),
        }) catch {};
        return err;
    };
}

fn rocGlueInner(gpa: Allocator, stderr: *std.Io.Writer, stdout: *std.Io.Writer, args: GlueArgs, temp_dir: []const u8) GlueError!void {
    // 0. Validate glue spec file exists
    std.Io.Dir.cwd().access(args.glue_spec, .{}) catch {
        return error.GlueSpecNotFound;
    };

    // 1. Parse platform header to get requires entries and verify it's a platform file.
    // Header parsing is still allowed here because it is parser-stage syntax handling,
    // not post-check semantic recovery.
    const platform_info = parsePlatformHeader(gpa, args.platform_path) catch |err| {
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
    const platform_abs_path = std.Io.Dir.cwd().realpathAlloc(gpa, args.platform_path) catch {
        return error.PlatformPathResolution;
    };
    defer gpa.free(platform_abs_path);

    var app_source = std.ArrayList(u8).empty;
    defer app_source.deinit(gpa);
    const w = app_source.writer(gpa);

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

    const synthetic_app_path = std.fs.path.join(gpa, &.{ temp_dir, "synthetic_app.roc" }) catch {
        return error.OutOfMemory;
    };
    defer gpa.free(synthetic_app_path);

    std.Io.Dir.cwd().writeFile(.{
        .sub_path = synthetic_app_path,
        .data = app_source.items,
    }) catch {
        return error.SyntheticAppWrite;
    };

    const cwd = std.process.getCwdAlloc(gpa) catch {
        return error.BuildEnvInit;
    };
    defer gpa.free(cwd);
    var build_env = BuildEnv.init(gpa, .single_threaded, 1, RocTarget.detectNative(), cwd) catch {
        return error.BuildEnvInit;
    };
    defer build_env.deinit();

    build_env.build(synthetic_app_path) catch {
        _ = build_env.renderDiagnostics(stderr);
        return error.CompilationFailed;
    };
    _ = build_env.renderDiagnostics(stderr);

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

    // 3. Collect platform module type information from checked artifacts.
    var collected_modules = std.ArrayList(CollectedModuleTypeInfo).empty;
    defer {
        for (collected_modules.items) |*mod_info| {
            mod_info.deinit(gpa);
        }
        collected_modules.deinit(gpa);
    }

    var type_table = TypeTable.init(gpa);
    defer type_table.deinit();

    for (modules) |mod| {
        if (mod.is_platform_sibling or mod.is_platform_main) {
            const artifact = mod.semantic.checked_artifact orelse continue;
            type_table.clearVarMap();
            if (collectModuleTypeInfo(gpa, artifact, mod.name, hosted_indices, &type_table)) |mod_info| {
                collected_modules.append(gpa, mod_info) catch {};
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
            const scheme = artifact.checked_types.schemeForKey(declaration.declared_source_ty) orelse
                glueInvariant("platform-required declaration has no checked type scheme", .{});
            const type_id = type_table.getOrInsert(artifact, scheme.root);
            try entrypoint_type_ids.put(name, type_id);
        }

        for (provides_entries.items) |provides_entry| {
            const def_idx = findTopLevelDefByName(artifact, provides_entry.name) orelse continue;
            const top_level = artifact.top_level_values.lookupByDef(def_idx) orelse
                glueInvariant("provided entry has no top-level value", .{});
            const scheme = artifact.checked_types.schemeForKey(top_level.source_scheme) orelse
                glueInvariant("provided entry has no checked type scheme", .{});
            const type_id = type_table.getOrInsert(artifact, scheme.root);
            try provides_type_ids.put(provides_entry.ffi_symbol, type_id);
        }
        break;
    }

    // 5. Compile glue spec through checked artifacts and lower to LIR.
    const glue_spec_abs = std.Io.Dir.cwd().realpathAlloc(gpa, args.glue_spec) catch {
        return error.GlueSpecNotFound;
    };
    defer gpa.free(glue_spec_abs);

    const glue_cwd = std.process.getCwdAlloc(gpa) catch {
        return error.BuildEnvInit;
    };
    defer gpa.free(glue_cwd);
    var glue_build_env = BuildEnv.init(gpa, .single_threaded, 1, RocTarget.detectNative(), glue_cwd) catch {
        return error.BuildEnvInit;
    };
    defer glue_build_env.deinit();

    glue_build_env.build(glue_spec_abs) catch {
        _ = glue_build_env.renderDiagnostics(stderr);
        return error.CompilationFailed;
    };
    _ = glue_build_env.renderDiagnostics(stderr);

    const root_artifact = glue_build_env.executableRootCheckedArtifact();
    const imported_artifacts = glue_build_env.collectImportedArtifactViews(gpa, root_artifact) catch {
        return error.OutOfMemory;
    };
    defer gpa.free(imported_artifacts);
    const relation_artifacts = glue_build_env.collectRelationArtifactViews(gpa, root_artifact) catch {
        return error.OutOfMemory;
    };
    defer gpa.free(relation_artifacts);

    var lowered = lir.CheckedPipeline.lowerArtifactsToLir(
        gpa,
        .{
            .root = CheckedArtifact.loweringViewWithRelations(root_artifact, relation_artifacts),
            .imports = imported_artifacts,
        },
        .{ .requests = root_artifact.root_requests.requests },
        .{
            .target_usize = base.target.TargetUsize.native,
        },
    ) catch {
        return error.OutOfMemory;
    };
    defer lowered.deinit();

    const glue_proc = selectGlueSpecRootProc(root_artifact, &lowered, "make_glue") orelse {
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

    // 6. Construct List(Types) using the exact committed LIR layout and invoke the LIR interpreter.
    const hosted_function_ptrs = [_]builtins.host_abi.HostedFn{};
    var default_roc_ops_env: echo_platform.DefaultRocOpsEnv = .{};
    var roc_ops = echo_platform.makeDefaultRocOps(&default_roc_ops_env, @constCast(&hosted_function_ptrs));
    const glue_writer = GlueRocValueWriter{
        .layouts = &lowered.lir_result.layouts,
        .schemas = &lowered.runtime_value_schemas,
        .roc_ops = &roc_ops,
    };
    var types_list = constructTypesRocList(&glue_writer, collected_modules.items, &platform_info, provides_entries.items, &type_table, &entrypoint_type_ids, &provides_type_ids, arg_layouts[0]);

    var interpreter = eval_mod.LirInterpreter.init(
        gpa,
        &lowered.lir_result.store,
        &lowered.lir_result.layouts,
        &roc_ops,
    ) catch {
        return error.OutOfMemory;
    };
    defer interpreter.deinit();

    const proc = lowered.lir_result.store.getProcSpec(glue_proc);
    const ret_size_align = lowered.lir_result.layouts.layoutSizeAlign(lowered.lir_result.layouts.getLayout(proc.ret_layout));
    const ret_alignment = std.mem.Alignment.fromByteUnits(ret_size_align.alignment.toByteUnits());
    const result_ptr = gpa.rawAlloc(ret_size_align.size, ret_alignment, @returnAddress()) orelse {
        return error.OutOfMemory;
    };
    const result_buf = result_ptr[0..ret_size_align.size];
    defer gpa.rawFree(result_buf, ret_alignment, @returnAddress());
    if (result_buf.len > 0) @memset(result_buf, 0);

    _ = interpreter.eval(.{
        .proc_id = glue_proc,
        .arg_layouts = arg_layouts,
        .ret_layout = proc.ret_layout,
        .arg_ptr = @ptrCast(&types_list),
        .ret_ptr = @ptrCast(result_buf.ptr),
    }) catch |err| {
        stderr.print("Error running glue spec: {}\n", .{err}) catch {};
        return error.CompilationFailed;
    };

    const glue_result = extractGlueResult(gpa, &glue_writer, result_buf.ptr, proc.ret_layout);
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

    std.Io.Dir.cwd().makePath(args.output_dir) catch {
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

        std.Io.Dir.cwd().writeFile(.{
            .sub_path = file_path,
            .data = file.content,
        }) catch {
            stderr.print("Error: Could not write file '{s}'\n", .{file_path}) catch {};
            return error.CompilationFailed;
        };

        stdout.print("  Wrote: {s}\n", .{file_path}) catch {};
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
    const module_name = artifact.canonical_names.moduleNameText(artifact.module_identity.module_name);
    const local_name = artifact.canonical_names.externalSymbolNameText(hosted.external_symbol_name);
    const qualified = try std.fmt.allocPrint(allocator, "{s}.{s}", .{ module_name, local_name });
    if (!std.mem.endsWith(u8, qualified, "!")) return qualified;

    const stripped = try allocator.dupe(u8, qualified[0 .. qualified.len - 1]);
    allocator.free(qualified);
    return stripped;
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

fn findTopLevelDefByName(
    artifact: *const CheckedArtifact.CheckedModuleArtifact,
    local_name: []const u8,
) ?can.CIR.Def.Idx {
    const module_name = artifact.canonical_names.moduleNameText(artifact.module_identity.module_name);

    for (artifact.top_level_values.entries) |entry| {
        const def_name = artifact.canonical_names.exportNameText(entry.source_name);
        const candidate = if (std.mem.startsWith(u8, def_name, module_name) and
            def_name.len > module_name.len and
            def_name[module_name.len] == '.')
            def_name[module_name.len + 1 ..]
        else
            def_name;
        if (std.mem.eql(u8, candidate, local_name)) return entry.def;
    }

    return null;
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

fn argLayoutsForProc(
    allocator: Allocator,
    store: *const lir.LirStore,
    proc_id: lir.LirProcSpecId,
) Allocator.Error![]layout.Idx {
    const proc = store.getProcSpec(proc_id);
    const arg_ids = store.getLocalSpan(proc.args);
    const arg_layouts = try allocator.alloc(layout.Idx, arg_ids.len);
    errdefer allocator.free(arg_layouts);

    for (arg_ids, 0..) |local_id, i| {
        arg_layouts[i] = store.locals.items[@intFromEnum(local_id)].layout_idx;
    }

    return arg_layouts;
}

/// Information extracted from a platform header for glue generation.
pub const PlatformHeaderInfo = struct {
    requires_entries: []RequiresEntry,
    type_aliases: [][]const u8,

    pub const RequiresEntry = struct {
        name: []const u8,
        type_str: []const u8,
        stub_expr: []const u8,
    };

    pub const ProvidesEntry = struct {
        name: []const u8,
        ffi_symbol: []const u8,
    };

    pub fn deinit(self: *const PlatformHeaderInfo, gpa: std.mem.Allocator) void {
        for (self.requires_entries) |entry| {
            gpa.free(entry.name);
            gpa.free(entry.type_str);
            gpa.free(entry.stub_expr);
        }
        gpa.free(self.requires_entries);
        for (self.type_aliases) |alias_name| {
            gpa.free(alias_name);
        }
        gpa.free(self.type_aliases);
    }
};

/// Parse a platform header to extract requires entries and validate it's a platform file.
fn parsePlatformHeader(gpa: Allocator, platform_path: []const u8) !PlatformHeaderInfo {
    // Read source file
    var source = std.Io.Dir.cwd().readFileAlloc(gpa, platform_path, std.math.maxInt(usize)) catch |err| switch (err) {
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

    var allocators: base.Allocators = undefined;
    allocators.initInPlace(gpa);
    defer allocators.deinit();

    // Parse the source code
    var parse_ast = parse.parse(&allocators, &env.common) catch return error.ParseFailed;
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

                    printTypeAnnoToBuf(gpa, &env, parse_ast, entry.type_anno, &type_buf);

                    // Generate stub expression from type annotation
                    var stub_buf = std.ArrayList(u8).empty;
                    defer stub_buf.deinit(gpa);

                    generateStubExprFromTypeAnno(gpa, &env, parse_ast, entry.type_anno, &stub_buf);

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

            return PlatformHeaderInfo{
                .requires_entries = try requires_entries.toOwnedSlice(gpa),
                .type_aliases = try type_aliases.toOwnedSlice(gpa),
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
const CollectedTypeRepr = union(enum) {
    bool_,
    box: u64,
    dec,
    f32_,
    f64_,
    i8_,
    i16_,
    i32_,
    i64_,
    i128_,
    u8_,
    u16_,
    u32_,
    u64_,
    u128_,
    str_,
    unit,
    list: u64,
    function: struct { arg_ids: []const u64, ret_id: u64 },
    record: struct { name: []const u8, fields: []const CollectedRecordField, size: u64, alignment: u64 },
    tag_union: struct { name: []const u8, tags: []const CollectedTagInfo, size: u64, alignment: u64 },
    unknown: []const u8,
};

const CollectedRecordField = struct {
    name: []const u8,
    type_id: u64,
    size: u64,
    alignment: u64,
};

const CollectedTagInfo = struct {
    name: []const u8,
    payload_ids: []const u64,
    payload_size: u64,
    payload_alignment: u64,
};

/// Builds a type table from artifact-owned checked type payloads.
const TypeTable = struct {
    entries: std.ArrayList(CollectedTypeRepr),
    var_map: std.AutoHashMap(CheckedArtifact.CheckedTypeId, u64),
    gpa: std.mem.Allocator,

    fn init(gpa: std.mem.Allocator) TypeTable {
        return .{
            .entries = std.ArrayList(CollectedTypeRepr).empty,
            .var_map = std.AutoHashMap(CheckedArtifact.CheckedTypeId, u64).init(gpa),
            .gpa = gpa,
        };
    }

    fn deinit(self: *TypeTable) void {
        for (self.entries.items) |entry| {
            self.freeEntry(entry);
        }
        self.entries.deinit(self.gpa);
        self.var_map.deinit();
    }

    fn freeEntry(self: *TypeTable, entry: CollectedTypeRepr) void {
        switch (entry) {
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
            .unknown => |text| {
                self.freeDuped(text);
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
    }

    /// Free a slice that was created with gpa.dupe. Skips empty slices and
    /// slices that point into static memory (from catch fallbacks).
    fn freeDuped(self: *TypeTable, slice: []const u8) void {
        if (slice.len == 0) return;
        self.gpa.free(slice);
    }

    /// Clear the checked-type map when switching modules (checked ids are artifact-local).
    fn clearVarMap(self: *TypeTable) void {
        self.var_map.clearRetainingCapacity();
    }

    /// Get an existing type table index for a checked type, or insert a new entry.
    /// Pre-registers a placeholder before conversion to prevent infinite recursion
    /// on cyclic types (the placeholder is updated in-place after conversion).
    fn getOrInsert(
        self: *TypeTable,
        artifact: *const CheckedArtifact.CheckedModuleArtifact,
        checked_type: CheckedArtifact.CheckedTypeId,
    ) u64 {
        if (self.var_map.get(checked_type)) |idx| {
            return idx;
        }

        const idx: u64 = @intCast(self.entries.items.len);
        self.entries.append(self.gpa, .{ .unknown = "" }) catch glueInvariant("could not allocate glue type-table placeholder", .{});
        self.var_map.put(checked_type, idx) catch glueInvariant("could not allocate glue type-table index", .{});

        const repr = self.convertCheckedType(artifact, checked_type);

        self.entries.items[@intCast(idx)] = repr;

        switch (repr) {
            .record => |rec| {
                if (rec.name.len == 0) {
                    self.entries.items[@intCast(idx)] = .{ .record = .{
                        .name = std.fmt.allocPrint(self.gpa, "__AnonStruct{d}", .{idx}) catch "",
                        .fields = rec.fields,
                        .size = rec.size,
                        .alignment = rec.alignment,
                    } };
                }
            },
            else => {},
        }

        return idx;
    }

    /// Insert a Unit type and return its index.
    fn insertUnit(self: *TypeTable) u64 {
        const idx: u64 = @intCast(self.entries.items.len);
        self.entries.append(self.gpa, .unit) catch return 0;
        return idx;
    }

    const SizeAlign = struct { size: u64, alignment: u64 };

    /// Get the size and alignment for a type table entry by index.
    fn getSizeAlign(self: *const TypeTable, type_id: u64) SizeAlign {
        if (type_id >= self.entries.items.len) return .{ .size = 0, .alignment = 1 };
        return getSizeAlignForRepr(self.entries.items[@intCast(type_id)]);
    }

    /// Get the size and alignment for a CollectedTypeRepr.
    fn getSizeAlignForRepr(repr: CollectedTypeRepr) SizeAlign {
        return switch (repr) {
            .bool_ => .{ .size = 1, .alignment = 1 },
            .box => .{ .size = 8, .alignment = 8 },
            .u8_, .i8_ => .{ .size = 1, .alignment = 1 },
            .u16_, .i16_ => .{ .size = 2, .alignment = 2 },
            .u32_, .i32_, .f32_ => .{ .size = 4, .alignment = 4 },
            .u64_, .i64_, .f64_, .dec => .{ .size = 8, .alignment = 8 },
            .u128_, .i128_ => .{ .size = 16, .alignment = 16 },
            .str_ => .{ .size = 24, .alignment = 8 },
            .list => .{ .size = 24, .alignment = 8 },
            .unit => .{ .size = 0, .alignment = 0 },
            .record => |rec| .{ .size = rec.size, .alignment = rec.alignment },
            .function => .{ .size = 0, .alignment = 1 },
            .tag_union => |tu| .{ .size = tu.size, .alignment = tu.alignment },
            .unknown => .{ .size = 0, .alignment = 1 },
        };
    }

    fn convertCheckedType(
        self: *TypeTable,
        artifact: *const CheckedArtifact.CheckedModuleArtifact,
        checked_type: CheckedArtifact.CheckedTypeId,
    ) CollectedTypeRepr {
        const payload = checkedTypePayload(artifact, checked_type);
        return switch (payload) {
            .pending => glueInvariant("pending checked type reached glue type table", .{}),
            .flex => .{ .unknown = self.gpa.dupe(u8, "flex") catch "" },
            .rigid => .{ .unknown = self.gpa.dupe(u8, "rigid") catch "" },
            .alias => |alias| self.getAliasBackingRepr(artifact, alias.backing),
            .record => |record| self.convertRecord(artifact, record.fields, record.ext),
            .record_unbound => |fields| self.convertRecord(artifact, fields, null),
            .tuple => |items| self.convertTuple(artifact, items),
            .nominal => |nominal| self.convertNominal(artifact, nominal),
            .function => |func| self.convertFunc(artifact, func),
            .empty_record, .empty_tag_union => .unit,
            .tag_union => |tag_union| self.convertTagUnion(artifact, tag_union.tags, tag_union.ext),
        };
    }

    fn getAliasBackingRepr(
        self: *TypeTable,
        artifact: *const CheckedArtifact.CheckedModuleArtifact,
        backing: CheckedArtifact.CheckedTypeId,
    ) CollectedTypeRepr {
        return self.convertCheckedType(artifact, backing);
    }

    fn convertNominal(
        self: *TypeTable,
        artifact: *const CheckedArtifact.CheckedModuleArtifact,
        nominal: CheckedArtifact.CheckedNominalType,
    ) CollectedTypeRepr {
        const display_name = TypeTable.getTypeDisplayName(artifact.canonical_names.typeNameText(nominal.name));

        if (nominal.builtin) |builtin_nominal| {
            switch (builtin_nominal) {
                .list => {
                    if (nominal.args.len >= 1) return .{ .list = self.getOrInsert(artifact, nominal.args[0]) };
                    return .{ .unknown = self.gpa.dupe(u8, "List") catch "" };
                },
                .box => {
                    if (nominal.args.len >= 1) return .{ .box = self.getOrInsert(artifact, nominal.args[0]) };
                    return .{ .unknown = self.gpa.dupe(u8, "Box") catch "" };
                },
                .str => return .str_,
                .bool => return .bool_,
                .dec => return .dec,
                .u8 => return .u8_,
                .u16 => return .u16_,
                .u32 => return .u32_,
                .u64 => return .u64_,
                .u128 => return .u128_,
                .i8 => return .i8_,
                .i16 => return .i16_,
                .i32 => return .i32_,
                .i64 => return .i64_,
                .i128 => return .i128_,
                .f32 => return .f32_,
                .f64 => return .f64_,
            }
        }

        const backing_repr = self.convertCheckedType(artifact, nominal.backing);
        return switch (backing_repr) {
            .record => |rec| .{ .record = .{
                .name = self.gpa.dupe(u8, display_name) catch "",
                .fields = rec.fields,
                .size = rec.size,
                .alignment = rec.alignment,
            } },
            .tag_union => |tu| blk: {
                self.freeDuped(tu.name);
                break :blk .{ .tag_union = .{
                    .name = self.gpa.dupe(u8, display_name) catch "",
                    .tags = tu.tags,
                    .size = tu.size,
                    .alignment = tu.alignment,
                } };
            },
            else => backing_repr,
        };
    }

    fn convertRecord(
        self: *TypeTable,
        artifact: *const CheckedArtifact.CheckedModuleArtifact,
        fields: []const CheckedArtifact.CheckedRecordField,
        ext: ?CheckedArtifact.CheckedTypeId,
    ) CollectedTypeRepr {
        var all_fields = std.ArrayList(CheckedArtifact.CheckedRecordField).empty;
        defer all_fields.deinit(self.gpa);
        all_fields.appendSlice(self.gpa, fields) catch return self.oomUnknown("record");
        if (ext) |ext_id| self.appendRecordExtFields(artifact, ext_id, &all_fields);
        return self.convertRecordFields(artifact, all_fields.items);
    }

    fn appendRecordExtFields(
        self: *TypeTable,
        artifact: *const CheckedArtifact.CheckedModuleArtifact,
        ext: CheckedArtifact.CheckedTypeId,
        fields: *std.ArrayList(CheckedArtifact.CheckedRecordField),
    ) void {
        switch (checkedTypePayload(artifact, ext)) {
            .empty_record => {},
            .record => |record| {
                fields.appendSlice(self.gpa, record.fields) catch glueInvariant("could not allocate extended record fields", .{});
                self.appendRecordExtFields(artifact, record.ext, fields);
            },
            .record_unbound => |unbound| fields.appendSlice(self.gpa, unbound) catch glueInvariant("could not allocate unbound record fields", .{}),
            else => glueInvariant("non-record extension reached glue record conversion", .{}),
        }
    }

    fn convertRecordFields(
        self: *TypeTable,
        artifact: *const CheckedArtifact.CheckedModuleArtifact,
        fields: []const CheckedArtifact.CheckedRecordField,
    ) CollectedTypeRepr {
        if (fields.len == 0) return .unit;

        const field_type_ids = self.gpa.alloc(u64, fields.len) catch return self.oomUnknown("record");
        defer self.gpa.free(field_type_ids);
        for (fields, 0..) |field, i| {
            field_type_ids[i] = self.getOrInsert(artifact, field.ty);
        }

        const field_sizes = self.gpa.alloc(SizeAlign, fields.len) catch return self.oomUnknown("record");
        defer self.gpa.free(field_sizes);
        for (0..fields.len) |i| {
            field_sizes[i] = self.getSizeAlign(field_type_ids[i]);
        }

        var field_indices = self.gpa.alloc(usize, fields.len) catch return self.oomUnknown("record");
        defer self.gpa.free(field_indices);
        for (0..fields.len) |i| field_indices[i] = i;

        const SortCtx = struct {
            fields: []const CheckedArtifact.CheckedRecordField,
            names: *const CanonicalNameStore,
            sizes: []const SizeAlign,

            pub fn lessThan(ctx: @This(), a: usize, b: usize) bool {
                const a_align = ctx.sizes[a].alignment;
                const b_align = ctx.sizes[b].alignment;
                if (a_align != b_align) return a_align > b_align;
                const a_text = ctx.names.recordFieldLabelText(ctx.fields[a].name);
                const b_text = ctx.names.recordFieldLabelText(ctx.fields[b].name);
                return std.mem.order(u8, a_text, b_text) == .lt;
            }
        };
        std.mem.sort(usize, field_indices, SortCtx{ .fields = fields, .names = &artifact.canonical_names, .sizes = field_sizes }, SortCtx.lessThan);

        const collected_fields = self.gpa.alloc(CollectedRecordField, fields.len) catch return self.oomUnknown("record");
        var max_alignment: u64 = 0;
        var current_offset: u64 = 0;
        for (field_indices, 0..) |src_idx, dst_idx| {
            const f_size = field_sizes[src_idx].size;
            const f_align = field_sizes[src_idx].alignment;
            if (f_align > max_alignment) max_alignment = f_align;
            if (f_align > 0) {
                const rem = current_offset % f_align;
                if (rem != 0) current_offset += f_align - rem;
            }
            current_offset += f_size;

            collected_fields[dst_idx] = .{
                .name = self.gpa.dupe(u8, artifact.canonical_names.recordFieldLabelText(fields[src_idx].name)) catch "",
                .type_id = field_type_ids[src_idx],
                .size = f_size,
                .alignment = f_align,
            };
        }

        var record_size = current_offset;
        if (max_alignment > 0) {
            const rem = record_size % max_alignment;
            if (rem != 0) record_size += max_alignment - rem;
        }

        return .{ .record = .{
            .name = "",
            .fields = collected_fields,
            .size = record_size,
            .alignment = max_alignment,
        } };
    }

    fn oomUnknown(self: *TypeTable, name: []const u8) CollectedTypeRepr {
        return .{ .unknown = self.gpa.dupe(u8, name) catch "" };
    }

    fn convertTuple(
        self: *TypeTable,
        artifact: *const CheckedArtifact.CheckedModuleArtifact,
        elems: []const CheckedArtifact.CheckedTypeId,
    ) CollectedTypeRepr {
        if (elems.len == 0) return .unit;

        // Convert tuple elements as record fields with positional names (_0, _1, ...)
        const field_type_ids = self.gpa.alloc(u64, elems.len) catch return self.oomUnknown("tuple");
        defer self.gpa.free(field_type_ids);
        for (elems, 0..) |elem, i| {
            field_type_ids[i] = self.getOrInsert(artifact, elem);
        }

        const field_sizes = self.gpa.alloc(SizeAlign, elems.len) catch return self.oomUnknown("tuple");
        defer self.gpa.free(field_sizes);
        for (0..elems.len) |i| {
            field_sizes[i] = self.getSizeAlign(field_type_ids[i]);
        }

        // Generate positional field names (_0, _1, ...) before sorting
        const field_names = self.gpa.alloc([]const u8, elems.len) catch return self.oomUnknown("tuple");
        defer self.gpa.free(field_names);
        for (0..elems.len) |i| {
            field_names[i] = std.fmt.allocPrint(self.gpa, "_{d}", .{i}) catch "";
        }

        // Sort by alignment descending, then name ascending (matching Roc ABI)
        var field_indices = self.gpa.alloc(usize, elems.len) catch return self.oomUnknown("tuple");
        defer self.gpa.free(field_indices);
        for (0..elems.len) |i| {
            field_indices[i] = i;
        }

        const SortCtx = struct {
            sizes: []const SizeAlign,
            names: []const []const u8,

            pub fn lessThan(ctx: @This(), a: usize, b: usize) bool {
                const a_align = ctx.sizes[a].alignment;
                const b_align = ctx.sizes[b].alignment;
                if (a_align != b_align) {
                    return a_align > b_align; // descending alignment
                }
                return std.mem.order(u8, ctx.names[a], ctx.names[b]) == .lt;
            }
        };
        std.mem.sort(usize, field_indices, SortCtx{ .sizes = field_sizes, .names = field_names }, SortCtx.lessThan);

        const collected_fields = self.gpa.alloc(CollectedRecordField, elems.len) catch return self.oomUnknown("tuple");
        var max_alignment: u64 = 0;
        var current_offset: u64 = 0;
        for (field_indices, 0..) |src_idx, dst_idx| {
            const f_size = field_sizes[src_idx].size;
            const f_align = field_sizes[src_idx].alignment;

            if (f_align > max_alignment) max_alignment = f_align;

            if (f_align > 0) {
                const rem = current_offset % f_align;
                if (rem != 0) current_offset += f_align - rem;
            }
            current_offset += f_size;

            collected_fields[dst_idx] = .{
                .name = field_names[src_idx],
                .type_id = field_type_ids[src_idx],
                .size = f_size,
                .alignment = f_align,
            };
        }

        var record_size = current_offset;
        if (max_alignment > 0) {
            const rem = record_size % max_alignment;
            if (rem != 0) record_size += max_alignment - rem;
        }

        return .{ .record = .{
            .name = "",
            .fields = collected_fields,
            .size = record_size,
            .alignment = max_alignment,
        } };
    }

    fn convertTagUnion(
        self: *TypeTable,
        artifact: *const CheckedArtifact.CheckedModuleArtifact,
        tags: []const CheckedArtifact.CheckedTag,
        ext: CheckedArtifact.CheckedTypeId,
    ) CollectedTypeRepr {
        var all_tags = std.ArrayList(CheckedArtifact.CheckedTag).empty;
        defer all_tags.deinit(self.gpa);
        all_tags.appendSlice(self.gpa, tags) catch return self.oomUnknown("tag_union");
        self.appendTagUnionExtTags(artifact, ext, &all_tags);

        if (all_tags.items.len == 0) return .unit;

        // Build sortable array of tag indices
        var tag_indices = self.gpa.alloc(usize, all_tags.items.len) catch return self.oomUnknown("tag_union");
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

        // Collect tags and compute per-variant payload layout
        const collected_tags = self.gpa.alloc(CollectedTagInfo, all_tags.items.len) catch return self.oomUnknown("tag_union");
        var max_payload_size: u64 = 0;
        var max_payload_alignment: u64 = 0;

        // Also build auto-generated name from variant names joined with "Or"
        var name_len: usize = 0;
        for (tag_indices) |src_idx| {
            const nt = artifact.canonical_names.tagLabelText(all_tags.items[src_idx].name);
            name_len += nt.len;
        }
        // Add "Or" separators between names
        if (all_tags.items.len > 1) name_len += (all_tags.items.len - 1) * 2;
        const auto_name_buf: []u8 = self.gpa.alloc(u8, name_len) catch return self.oomUnknown("tag_union");
        var name_pos: usize = 0;

        for (tag_indices, 0..) |src_idx, dst_idx| {
            const tag = all_tags.items[src_idx];
            const name_text = artifact.canonical_names.tagLabelText(tag.name);

            const payload_ids = self.gpa.alloc(u64, tag.args.len) catch return self.oomUnknown("tag_union");
            for (tag.args, 0..) |arg, i| {
                payload_ids[i] = self.getOrInsert(artifact, arg);
            }

            // Compute payload as a tuple: sequential fields with alignment padding
            var payload_size: u64 = 0;
            var payload_alignment: u64 = 0;
            for (payload_ids) |pid| {
                const sa = self.getSizeAlign(pid);
                if (sa.alignment > payload_alignment) payload_alignment = sa.alignment;
                // Align current offset
                if (sa.alignment > 0) {
                    const rem = payload_size % sa.alignment;
                    if (rem != 0) payload_size += sa.alignment - rem;
                }
                payload_size += sa.size;
            }
            // Round up to payload alignment
            if (payload_alignment > 0) {
                const rem = payload_size % payload_alignment;
                if (rem != 0) payload_size += payload_alignment - rem;
            }

            if (payload_size > max_payload_size) max_payload_size = payload_size;
            if (payload_alignment > max_payload_alignment) max_payload_alignment = payload_alignment;

            collected_tags[dst_idx] = .{
                .name = self.gpa.dupe(u8, name_text) catch "",
                .payload_ids = payload_ids,
                .payload_size = payload_size,
                .payload_alignment = payload_alignment,
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

        // Compute discriminant size/alignment from tag count.
        // Single-variant tag unions have no discriminant (ZigGlue unwraps them to payload).
        const disc_size: u64 = if (all_tags.items.len <= 1) 0 else layout.TagUnionData.discriminantSize(all_tags.items.len);
        const disc_align: u64 = disc_size;

        // Compute overall tag union layout: payload at offset 0, discriminant at end
        // disc_offset = alignForward(max_payload_size, disc_align)
        var disc_offset = max_payload_size;
        if (disc_align > 0) {
            const rem = disc_offset % disc_align;
            if (rem != 0) disc_offset += disc_align - rem;
        }

        const total_align = @max(max_payload_alignment, disc_align);
        // total_size = alignForward(disc_offset + disc_size, total_align)
        var total_size = disc_offset + disc_size;
        if (total_align > 0) {
            const rem = total_size % total_align;
            if (rem != 0) total_size += total_align - rem;
        }

        const auto_name: []const u8 = auto_name_buf[0..name_pos];

        return .{ .tag_union = .{
            .name = auto_name,
            .tags = collected_tags,
            .size = total_size,
            .alignment = total_align,
        } };
    }

    fn appendTagUnionExtTags(
        self: *TypeTable,
        artifact: *const CheckedArtifact.CheckedModuleArtifact,
        ext: CheckedArtifact.CheckedTypeId,
        tags: *std.ArrayList(CheckedArtifact.CheckedTag),
    ) void {
        switch (checkedTypePayload(artifact, ext)) {
            .empty_tag_union => {},
            .tag_union => |tag_union| {
                tags.appendSlice(self.gpa, tag_union.tags) catch glueInvariant("could not allocate extended tag-union tags", .{});
                self.appendTagUnionExtTags(artifact, tag_union.ext, tags);
            },
            else => glueInvariant("non-tag-union extension reached glue tag-union conversion", .{}),
        }
    }

    fn convertFunc(
        self: *TypeTable,
        artifact: *const CheckedArtifact.CheckedModuleArtifact,
        func: CheckedArtifact.CheckedFunctionType,
    ) CollectedTypeRepr {
        const arg_ids = self.gpa.alloc(u64, func.args.len) catch return self.oomUnknown("function");
        for (func.args, 0..) |arg, i| {
            arg_ids[i] = self.getOrInsert(artifact, arg);
        }
        const ret_id = self.getOrInsert(artifact, func.ret);

        return .{ .function = .{
            .arg_ids = arg_ids,
            .ret_id = ret_id,
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
        const offset = self.layouts.getStructFieldOffsetByOriginalIndex(record_layout.data.struct_.idx, field_index);
        const field_layout = self.layouts.getStructFieldLayoutByOriginalIndex(record_layout.data.struct_.idx, field_index);
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
            .list => list_layout.data.list,
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
                .list = .{ .bytes = null, .length = len, .capacity_or_alloc_ptr = len },
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
                .capacity_or_alloc_ptr = len,
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
        self.layouts.getTagUnionInfo(tag_union_layout).data.writeDiscriminant(tag_union_base, tag_index);
    }

    fn readTagDiscriminant(self: *const GlueRocValueWriter, tag_union_base: [*]const u8, tag_union_layout_idx: layout.Idx) u64 {
        const tag_union_layout = self.layouts.getLayout(tag_union_layout_idx);
        if (tag_union_layout.tag != .tag_union) {
            glueInvariant("glue expected tag-union layout, got {s}", .{@tagName(tag_union_layout.tag)});
        }
        return self.layouts.getTagUnionInfo(tag_union_layout).data.readDiscriminant(@constCast(tag_union_base));
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
            .length = str.len,
            .capacity_or_alloc_ptr = SMALL_STRING_SIZE,
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
    writer.writeField(value_base, record_field_layout, "RecordField", "alignment", u64, field.alignment);
    writer.writeField(value_base, record_field_layout, "RecordField", "name", RocStr, createBigRocStr(field.name, writer.roc_ops));
    writer.writeField(value_base, record_field_layout, "RecordField", "size", u64, field.size);
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
    writer.writeField(value_base, tag_variant_layout, "TagVariant", "payload_alignment", u64, tag.payload_alignment);
    writer.writeField(value_base, tag_variant_layout, "TagVariant", "payload_size", u64, tag.payload_size);
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
        .box => |inner_id| {
            const tag_index = writer.tagIndex("TypeRepr", "RocBox");
            writer.writeValue(value_base, u64, inner_id);
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
        .list => |elem_id| {
            const tag_index = writer.tagIndex("TypeRepr", "RocList");
            _ = writer.variantPayloadLayout(type_repr_layout, tag_index);
            writer.writeValue(value_base, u64, elem_id);
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
            writer.writeField(value_base, payload_layout, "RecordRepr", "alignment", u64, rec.alignment);
            writer.writeField(value_base, payload_layout, "RecordRepr", "fields", RocList, buildRecordFieldTypeReprList(writer, rec.fields, fields_slot.layout_idx));
            writer.writeField(value_base, payload_layout, "RecordRepr", "name", RocStr, createBigRocStr(rec.name, writer.roc_ops));
            writer.writeField(value_base, payload_layout, "RecordRepr", "size", u64, rec.size);
            writer.writeTagDiscriminant(value_base, type_repr_layout, tag_index);
            return;
        },
        .tag_union => |tu| {
            const tag_index = writer.tagIndex("TypeRepr", "RocTagUnion");
            const payload_layout = writer.variantPayloadLayout(type_repr_layout, tag_index);
            writer.zeroValue(value_base, payload_layout);
            const tags_slot = writer.recordField(value_base, payload_layout, "TagUnionRepr", "tags");
            writer.writeField(value_base, payload_layout, "TagUnionRepr", "alignment", u64, tu.alignment);
            writer.writeField(value_base, payload_layout, "TagUnionRepr", "name", RocStr, createBigRocStr(tu.name, writer.roc_ops));
            writer.writeField(value_base, payload_layout, "TagUnionRepr", "size", u64, tu.size);
            writer.writeField(value_base, payload_layout, "TagUnionRepr", "tags", RocList, buildTagVariantList(writer, tu.tags, tags_slot.layout_idx));
            writer.writeTagDiscriminant(value_base, type_repr_layout, tag_index);
            return;
        },
        .unknown => |text| {
            const tag_index = writer.tagIndex("TypeRepr", "RocUnknown");
            _ = writer.variantPayloadLayout(type_repr_layout, tag_index);
            writer.writeValue(value_base, RocStr, createBigRocStr(text, writer.roc_ops));
            writer.writeTagDiscriminant(value_base, type_repr_layout, tag_index);
            return;
        },
    };
    writer.writeTagDiscriminant(value_base, type_repr_layout, writer.tagIndex("TypeRepr", tag_name));
}

/// Build a RocList of TypeReprRoc from the type table.
fn buildTypeTableRocList(
    writer: *const GlueRocValueWriter,
    type_table: *const TypeTable,
    list_layout: layout.Idx,
) RocList {
    const allocated = writer.allocateList(list_layout, type_table.entries.items.len, true);
    if (allocated.bytes == null) return allocated.list;

    for (type_table.entries.items, 0..) |entry, i| {
        writeTypeRepr(writer, allocated.bytes.? + i * allocated.elem_size, allocated.elem_layout, entry);
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
        writer.writeField(elem_base, allocated.elem_layout, "EntryPoint", "type_id", u64, entrypoint_type_ids.get(entry.name) orelse 0);
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
        writer.writeField(elem_base, allocated.elem_layout, "ProvidesEntry", "type_id", u64, provides_type_ids.get(entry.ffi_symbol) orelse 0);
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
    const type_table_slot = writer.recordField(types_base, allocated.elem_layout, "Types", "type_table");

    writer.writeField(types_base, allocated.elem_layout, "Types", "entrypoints", RocList, buildEntryPointList(writer, platform_info, entrypoint_type_ids, entrypoints_slot.layout_idx));
    writer.writeField(types_base, allocated.elem_layout, "Types", "modules", RocList, buildModuleTypeInfoList(writer, collected_modules, modules_slot.layout_idx));
    writer.writeField(types_base, allocated.elem_layout, "Types", "provides_entries", RocList, buildProvidesEntryList(writer, provides_entries, provides_type_ids, provides_slot.layout_idx));
    writer.writeField(types_base, allocated.elem_layout, "Types", "type_table", RocList, buildTypeTableRocList(writer, type_table, type_table_slot.layout_idx));

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

fn copyRocStrSlice(allocator: Allocator, str: RocStr) []const u8 {
    return allocator.dupe(u8, str.asSlice()) catch glueInvariant("could not copy glue result string", .{});
}

fn extractGlueResult(
    allocator: Allocator,
    writer: *const GlueRocValueWriter,
    result_base: [*]const u8,
    result_layout: layout.Idx,
) GlueResultFiles {
    const ok_index = writer.tagIndex("Try", "Ok");
    const err_index = writer.tagIndex("Try", "Err");
    const discriminant = writer.readTagDiscriminant(result_base, result_layout);

    if (discriminant == ok_index) {
        const files_list_layout = writer.variantPayloadLayout(result_layout, ok_index);
        const files = writer.readValue(result_base, RocList);
        if (files.len() == 0 or files.bytes == null) {
            return .{ .allocator = allocator, .files = &.{}, .err_msg = null };
        }

        const file_layout = writer.listElementLayout(files_list_layout);
        const file_size = writer.sizeOf(file_layout);
        const out = allocator.alloc(GlueResultFile, files.len()) catch {
            glueInvariant("could not allocate glue result file slice", .{});
        };
        const file_bytes = files.bytes.?;
        for (out, 0..) |*file, index| {
            const file_base = file_bytes + index * file_size;
            const name_slot = writer.recordField(file_base, file_layout, "File", "name");
            const content_slot = writer.recordField(file_base, file_layout, "File", "content");
            const name = writer.readValue(name_slot.ptr, RocStr);
            const content = writer.readValue(content_slot.ptr, RocStr);
            file.* = .{
                .name = copyRocStrSlice(allocator, name),
                .content = copyRocStrSlice(allocator, content),
            };
        }
        return .{ .allocator = allocator, .files = out, .err_msg = null };
    }

    if (discriminant == err_index) {
        _ = writer.variantPayloadLayout(result_layout, err_index);
        const err = writer.readValue(result_base, RocStr);
        return .{ .allocator = allocator, .files = &.{}, .err_msg = copyRocStrSlice(allocator, err) };
    }

    glueInvariant("glue result Try discriminant {d} was neither Ok nor Err", .{discriminant});
}

fn checkedTypePayload(
    artifact: *const CheckedArtifact.CheckedModuleArtifact,
    checked_type: CheckedArtifact.CheckedTypeId,
) CheckedArtifact.CheckedTypePayload {
    const idx = @intFromEnum(checked_type);
    if (idx >= artifact.checked_types.payloads.len) {
        glueInvariant("checked type id {d} out of bounds", .{idx});
    }
    return artifact.checked_types.payloads[idx];
}

fn checkedTypeRootForScheme(
    artifact: *const CheckedArtifact.CheckedModuleArtifact,
    scheme_key: check.CanonicalNames.CanonicalTypeSchemeKey,
) CheckedArtifact.CheckedTypeId {
    return (artifact.checked_types.schemeForKey(scheme_key) orelse
        glueInvariant("checked type scheme missing from artifact", .{})).root;
}

fn typeStringAlloc(
    gpa: std.mem.Allocator,
    artifact: *const CheckedArtifact.CheckedModuleArtifact,
    checked_type: CheckedArtifact.CheckedTypeId,
) []const u8 {
    var buf = std.ArrayList(u8).empty;
    var active = std.AutoHashMap(CheckedArtifact.CheckedTypeId, void).init(gpa);
    defer active.deinit();
    writeTypeString(gpa, artifact, checked_type, &buf, &active) catch {
        buf.deinit(gpa);
        return gpa.dupe(u8, "") catch "";
    };
    return buf.toOwnedSlice(gpa) catch "";
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
    try all_fields.appendSlice(gpa, fields);
    if (ext) |ext_id| appendRecordStringExtFields(gpa, artifact, ext_id, &all_fields);

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

fn appendRecordStringExtFields(
    gpa: std.mem.Allocator,
    artifact: *const CheckedArtifact.CheckedModuleArtifact,
    ext: CheckedArtifact.CheckedTypeId,
    fields: *std.ArrayList(CheckedArtifact.CheckedRecordField),
) void {
    switch (checkedTypePayload(artifact, ext)) {
        .empty_record => {},
        .record => |record| {
            fields.appendSlice(gpa, record.fields) catch glueInvariant("could not allocate record type-string extension", .{});
            appendRecordStringExtFields(gpa, artifact, record.ext, fields);
        },
        .record_unbound => |unbound| fields.appendSlice(gpa, unbound) catch glueInvariant("could not allocate record type-string unbound fields", .{}),
        else => glueInvariant("non-record extension reached glue type string", .{}),
    }
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
    try all_tags.appendSlice(gpa, tags);
    appendTagStringExtTags(gpa, artifact, ext, &all_tags);

    try buf.append(gpa, '[');
    for (all_tags.items, 0..) |tag, i| {
        if (i > 0) try buf.appendSlice(gpa, ", ");
        try buf.appendSlice(gpa, artifact.canonical_names.tagLabelText(tag.name));
        if (tag.args.len > 0) {
            try buf.append(gpa, '(');
            for (tag.args, 0..) |arg, arg_i| {
                if (arg_i > 0) try buf.appendSlice(gpa, ", ");
                try writeTypeString(gpa, artifact, arg, buf, active);
            }
            try buf.append(gpa, ')');
        }
    }
    try buf.append(gpa, ']');
}

fn appendTagStringExtTags(
    gpa: std.mem.Allocator,
    artifact: *const CheckedArtifact.CheckedModuleArtifact,
    ext: CheckedArtifact.CheckedTypeId,
    tags: *std.ArrayList(CheckedArtifact.CheckedTag),
) void {
    switch (checkedTypePayload(artifact, ext)) {
        .empty_tag_union => {},
        .tag_union => |tag_union| {
            tags.appendSlice(gpa, tag_union.tags) catch glueInvariant("could not allocate tag-union type-string extension", .{});
            appendTagStringExtTags(gpa, artifact, tag_union.ext, tags);
        },
        else => glueInvariant("non-tag-union extension reached glue type string", .{}),
    }
}

fn functionPayloadForRoot(
    artifact: *const CheckedArtifact.CheckedModuleArtifact,
    checked_type: CheckedArtifact.CheckedTypeId,
) ?CheckedArtifact.CheckedFunctionType {
    return switch (checkedTypePayload(artifact, checked_type)) {
        .function => |func| func,
        .alias => |alias| functionPayloadForRoot(artifact, alias.backing),
        .nominal => |nominal| functionPayloadForRoot(artifact, nominal.backing),
        else => null,
    };
}

/// Extract record fields from artifact-owned checked type payloads.
fn extractRecordFields(
    gpa: std.mem.Allocator,
    artifact: *const CheckedArtifact.CheckedModuleArtifact,
    checked_type: CheckedArtifact.CheckedTypeId,
) []const CollectedModuleTypeInfo.CollectedRecordFieldInfo {
    var fields = std.ArrayList(CheckedArtifact.CheckedRecordField).empty;
    defer fields.deinit(gpa);
    if (!collectRecordFieldsForRoot(gpa, artifact, checked_type, &fields)) {
        return &[_]CollectedModuleTypeInfo.CollectedRecordFieldInfo{};
    }

    var indices = gpa.alloc(usize, fields.items.len) catch return &[_]CollectedModuleTypeInfo.CollectedRecordFieldInfo{};
    defer gpa.free(indices);
    for (0..fields.items.len) |i| indices[i] = i;

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
    std.mem.sort(usize, indices, SortCtx{ .fields = fields.items, .names = &artifact.canonical_names }, SortCtx.lessThan);

    var result_list = std.ArrayList(CollectedModuleTypeInfo.CollectedRecordFieldInfo).empty;
    for (indices) |idx| {
        const field = fields.items[idx];
        result_list.append(gpa, .{
            .name = gpa.dupe(u8, artifact.canonical_names.recordFieldLabelText(field.name)) catch continue,
            .type_str = typeStringAlloc(gpa, artifact, field.ty),
        }) catch continue;
    }
    return result_list.toOwnedSlice(gpa) catch &[_]CollectedModuleTypeInfo.CollectedRecordFieldInfo{};
}

fn collectRecordFieldsForRoot(
    gpa: std.mem.Allocator,
    artifact: *const CheckedArtifact.CheckedModuleArtifact,
    checked_type: CheckedArtifact.CheckedTypeId,
    fields: *std.ArrayList(CheckedArtifact.CheckedRecordField),
) bool {
    switch (checkedTypePayload(artifact, checked_type)) {
        .alias => |alias| return collectRecordFieldsForRoot(gpa, artifact, alias.backing, fields),
        .nominal => |nominal| return collectRecordFieldsForRoot(gpa, artifact, nominal.backing, fields),
        .record => |record| {
            fields.appendSlice(gpa, record.fields) catch glueInvariant("could not allocate record field extraction", .{});
            collectRecordExtFields(gpa, artifact, record.ext, fields);
            return true;
        },
        .record_unbound => |unbound| {
            fields.appendSlice(gpa, unbound) catch glueInvariant("could not allocate unbound record field extraction", .{});
            return true;
        },
        .empty_record => return true,
        else => return false,
    }
}

fn collectRecordExtFields(
    gpa: std.mem.Allocator,
    artifact: *const CheckedArtifact.CheckedModuleArtifact,
    ext: CheckedArtifact.CheckedTypeId,
    fields: *std.ArrayList(CheckedArtifact.CheckedRecordField),
) void {
    switch (checkedTypePayload(artifact, ext)) {
        .empty_record => {},
        .record => |record| {
            fields.appendSlice(gpa, record.fields) catch glueInvariant("could not allocate record extension extraction", .{});
            collectRecordExtFields(gpa, artifact, record.ext, fields);
        },
        .record_unbound => |unbound| fields.appendSlice(gpa, unbound) catch glueInvariant("could not allocate unbound record extension extraction", .{}),
        else => glueInvariant("non-record extension reached record field extraction", .{}),
    }
}

/// Collect type information from a published checked artifact.
fn collectModuleTypeInfo(
    gpa: Allocator,
    artifact: *const CheckedArtifact.CheckedModuleArtifact,
    module_name: []const u8,
    hosted_indices: []const HostedProcGlobalIndex,
    type_table: *TypeTable,
) ?CollectedModuleTypeInfo {
    var main_type_str: []const u8 = gpa.dupe(u8, "") catch "";
    for (artifact.checked_types.nominal_declarations) |declaration| {
        const type_name = TypeTable.getTypeDisplayName(artifact.canonical_names.typeNameText(declaration.nominal.type_name));
        if (std.mem.eql(u8, type_name, module_name)) {
            if (main_type_str.len > 0) gpa.free(main_type_str);
            main_type_str = typeStringAlloc(gpa, artifact, declaration.declaration_root);
            break;
        }
    }

    // Collect functions
    var functions = std.ArrayList(CollectedModuleTypeInfo.CollectedFunctionInfo).empty;
    var hosted_functions = std.ArrayList(CollectedModuleTypeInfo.CollectedHostedFunctionInfo).empty;

    const module_prefix = std.fmt.allocPrint(gpa, "{s}.", .{module_name}) catch return null;
    defer gpa.free(module_prefix);

    for (artifact.top_level_values.entries) |entry| {
        const def_idx = entry.def;

        const def_name = artifact.canonical_names.exportNameText(entry.source_name);

        if (std.mem.eql(u8, def_name, module_name)) continue;

        const local_name = if (std.mem.startsWith(u8, def_name, module_prefix))
            def_name[module_prefix.len..]
        else
            continue;

        const checked_type = checkedTypeRootForScheme(artifact, entry.source_scheme);
        const type_str = typeStringAlloc(gpa, artifact, checked_type);

        if (hostedProcForDef(&artifact.hosted_procs, def_idx)) |_| {
            // Extract record fields from function arg and return types.
            var arg_fields: []const CollectedModuleTypeInfo.CollectedRecordFieldInfo = &.{};
            var ret_fields: []const CollectedModuleTypeInfo.CollectedRecordFieldInfo = &.{};
            var arg_type_ids: []const u64 = &.{};
            var ret_type_id: u64 = 0;

            if (functionPayloadForRoot(artifact, checked_type)) |func| {
                ret_fields = extractRecordFields(gpa, artifact, func.ret);
                if (func.args.len == 1) {
                    arg_fields = extractRecordFields(gpa, artifact, func.args[0]);
                }
                ret_type_id = type_table.getOrInsert(artifact, func.ret);
                if (func.args.len > 0) {
                    const ids = gpa.alloc(u64, func.args.len) catch continue;
                    for (func.args, 0..) |arg, i| {
                        ids[i] = type_table.getOrInsert(artifact, arg);
                    }
                    arg_type_ids = ids;
                }
            } else {
                ret_type_id = type_table.insertUnit();
            }

            hosted_functions.append(gpa, .{
                .index = hostedGlobalIndexForDef(hosted_indices, artifact.key, def_idx),
                .name = gpa.dupe(u8, local_name) catch continue,
                .type_str = type_str,
                .arg_fields = arg_fields,
                .ret_fields = ret_fields,
                .arg_type_ids = arg_type_ids,
                .ret_type_id = ret_type_id,
            }) catch {
                gpa.free(type_str);
                continue;
            };
        } else switch (entry.value) {
            .procedure_binding => {
                functions.append(gpa, .{
                    .name = gpa.dupe(u8, local_name) catch continue,
                    .type_str = type_str,
                }) catch {
                    gpa.free(type_str);
                    continue;
                };
            },
            .const_ref => gpa.free(type_str),
        }
    }

    return CollectedModuleTypeInfo{
        .name = gpa.dupe(u8, module_name) catch return null,
        .main_type = main_type_str,
        .functions = functions,
        .hosted_functions = hosted_functions,
    };
}

/// Print a type annotation to a buffer (for requires entries which use AST types)
fn printTypeAnnoToBuf(gpa: std.mem.Allocator, env: *ModuleEnv, ast: *const parse.AST, type_anno_idx: parse.AST.TypeAnno.Idx, buf: *std.ArrayList(u8)) void {
    const type_anno = ast.store.getTypeAnno(type_anno_idx);

    switch (type_anno) {
        .@"fn" => |f| {
            const arrow = if (f.effectful) "=>" else "->";
            const args = ast.store.typeAnnoSlice(f.args);
            if (args.len == 0) {
                buf.appendSlice(gpa, "()") catch {};
            } else {
                for (args, 0..) |arg_idx, i| {
                    if (i > 0) buf.appendSlice(gpa, ", ") catch {};
                    printTypeAnnoToBuf(gpa, env, ast, arg_idx, buf);
                }
            }
            buf.appendSlice(gpa, " ") catch {};
            buf.appendSlice(gpa, arrow) catch {};
            buf.appendSlice(gpa, " ") catch {};
            printTypeAnnoToBuf(gpa, env, ast, f.ret, buf);
        },
        .ty => |t| {
            // Print qualified type name
            const qualifiers = ast.store.tokenSlice(t.qualifiers);
            for (qualifiers) |qual_tok_idx| {
                const qual_tok: parse.tokenize.Token.Idx = @intCast(qual_tok_idx);
                if (ast.tokens.resolveIdentifier(qual_tok)) |ident_idx| {
                    buf.appendSlice(gpa, env.common.getIdent(ident_idx)) catch {};
                    buf.append(gpa, '.') catch {};
                }
            }
            if (ast.tokens.resolveIdentifier(t.token)) |ident_idx| {
                buf.appendSlice(gpa, env.common.getIdent(ident_idx)) catch {};
            }
        },
        .ty_var => |tv| {
            if (ast.tokens.resolveIdentifier(tv.tok)) |ident_idx| {
                buf.appendSlice(gpa, env.common.getIdent(ident_idx)) catch {};
            }
        },
        .record => |r| {
            buf.appendSlice(gpa, "{ ") catch {};
            const fields = ast.store.annoRecordFieldSlice(r.fields);
            for (fields, 0..) |field_idx, i| {
                if (i > 0) buf.appendSlice(gpa, ", ") catch {};
                const field = ast.store.getAnnoRecordField(field_idx) catch continue;
                if (ast.tokens.resolveIdentifier(field.name)) |ident_idx| {
                    buf.appendSlice(gpa, env.common.getIdent(ident_idx)) catch {};
                    buf.appendSlice(gpa, " : ") catch {};
                }
                printTypeAnnoToBuf(gpa, env, ast, field.ty, buf);
            }
            switch (r.ext) {
                .closed => {},
                .open => buf.appendSlice(gpa, ", ..") catch {},
                .named => |named| {
                    buf.appendSlice(gpa, ", ..") catch {};
                    printTypeAnnoToBuf(gpa, env, ast, named.anno, buf);
                },
            }
            buf.appendSlice(gpa, " }") catch {};
        },
        .tag_union => |tu| {
            buf.append(gpa, '[') catch {};
            const tags = ast.store.typeAnnoSlice(tu.tags);
            for (tags, 0..) |tag_idx, i| {
                if (i > 0) buf.appendSlice(gpa, ", ") catch {};
                printTypeAnnoToBuf(gpa, env, ast, tag_idx, buf);
            }
            switch (tu.ext) {
                .closed => {},
                .open => buf.appendSlice(gpa, ", ..") catch {},
                .named => |named| {
                    buf.appendSlice(gpa, ", ..") catch {};
                    printTypeAnnoToBuf(gpa, env, ast, named.anno, buf);
                },
            }
            buf.append(gpa, ']') catch {};
        },
        .tuple => |t| {
            buf.append(gpa, '(') catch {};
            const annos = ast.store.typeAnnoSlice(t.annos);
            for (annos, 0..) |anno_idx, i| {
                if (i > 0) buf.appendSlice(gpa, ", ") catch {};
                printTypeAnnoToBuf(gpa, env, ast, anno_idx, buf);
            }
            buf.append(gpa, ')') catch {};
        },
        .apply => |a| {
            const args = ast.store.typeAnnoSlice(a.args);
            if (args.len > 0) {
                printTypeAnnoToBuf(gpa, env, ast, args[0], buf);
                if (args.len > 1) {
                    buf.append(gpa, ' ') catch {};
                    for (args[1..], 0..) |arg_idx, i| {
                        if (i > 0) buf.append(gpa, ' ') catch {};
                        printTypeAnnoToBuf(gpa, env, ast, arg_idx, buf);
                    }
                }
            }
        },
        .parens => |p| {
            buf.append(gpa, '(') catch {};
            printTypeAnnoToBuf(gpa, env, ast, p.anno, buf);
            buf.append(gpa, ')') catch {};
        },
        .underscore => {
            buf.append(gpa, '_') catch {};
        },
        .underscore_type_var => {
            buf.append(gpa, '_') catch {};
        },
        .malformed => {
            buf.appendSlice(gpa, "<malformed>") catch {};
        },
    }
}

/// Generate a stub expression from a type annotation.
/// This produces valid Roc expressions that will crash at runtime rather than compile-time.
/// Uses `...` inside lambdas to defer the crash to runtime.
fn generateStubExprFromTypeAnno(gpa: std.mem.Allocator, env: *ModuleEnv, ast: *const parse.AST, type_anno_idx: parse.AST.TypeAnno.Idx, buf: *std.ArrayList(u8)) void {
    const type_anno = ast.store.getTypeAnno(type_anno_idx);

    switch (type_anno) {
        .@"fn" => |f| {
            // Generate lambda stub
            const args = ast.store.typeAnnoSlice(f.args);
            if (args.len == 0) {
                // No args: || body
                buf.appendSlice(gpa, "|| ") catch {};
            } else {
                // Has args: |_, _, ...| body
                buf.append(gpa, '|') catch {};
                for (0..args.len) |i| {
                    if (i > 0) buf.appendSlice(gpa, ", ") catch {};
                    buf.append(gpa, '_') catch {};
                }
                buf.appendSlice(gpa, "| ") catch {};
            }

            // Check if return type is unit {}
            const ret_anno = ast.store.getTypeAnno(f.ret);
            if (ret_anno == .record) {
                const record = ret_anno.record;
                const fields = ast.store.annoRecordFieldSlice(record.fields);
                if (fields.len == 0 and record.ext == .closed) {
                    // Return type is {} (unit) - return empty record
                    buf.appendSlice(gpa, "{}") catch {};
                    return;
                }
            }

            // Non-unit return type - use { ... } to crash at runtime (not compile-time)
            // The block syntax is required for single-line lambdas
            buf.appendSlice(gpa, "{ ... }") catch {};
        },
        .record => |r| {
            buf.appendSlice(gpa, "{ ") catch {};
            const fields = ast.store.annoRecordFieldSlice(r.fields);
            for (fields, 0..) |field_idx, i| {
                if (i > 0) buf.appendSlice(gpa, ", ") catch {};
                const field = ast.store.getAnnoRecordField(field_idx) catch continue;
                if (ast.tokens.resolveIdentifier(field.name)) |ident_idx| {
                    buf.appendSlice(gpa, env.common.getIdent(ident_idx)) catch {};
                    buf.appendSlice(gpa, ": ") catch {};
                }
                generateStubExprFromTypeAnno(gpa, env, ast, field.ty, buf);
            }
            buf.appendSlice(gpa, " }") catch {};
        },
        else => {
            // For all other types, use { ... } to crash at runtime
            buf.appendSlice(gpa, "{ ... }") catch {};
        },
    }
}
