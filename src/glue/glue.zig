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
    std.fs.cwd().access(args.glue_spec, .{}) catch {
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
    const platform_abs_path = std.fs.cwd().realpathAlloc(gpa, args.platform_path) catch {
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

    std.fs.cwd().writeFile(.{
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
        const env = @constCast(artifact.moduleEnvConst());
        type_table.clearVarMap();

        for (artifact.provides_requires.provides) |provides_entry| {
            try provides_entries.append(gpa, .{
                .name = try gpa.dupe(u8, artifact.canonical_names.exportNameText(provides_entry.source_name)),
                .ffi_symbol = try gpa.dupe(u8, artifact.canonical_names.externalSymbolNameText(provides_entry.ffi_symbol)),
            });
        }

        for (artifact.platform_required_declarations.declarations) |declaration| {
            const name = artifact.canonical_names.exportNameText(declaration.platform_name);
            const type_id = type_table.getOrInsert(env, ModuleEnv.varFrom(declaration.type_anno));
            try entrypoint_type_ids.put(name, type_id);
        }

        for (provides_entries.items) |provides_entry| {
            const def_idx = findTopLevelDefByName(artifact, provides_entry.name) orelse continue;
            const type_id = type_table.getOrInsert(env, ModuleEnv.varFrom(def_idx));
            try provides_type_ids.put(provides_entry.ffi_symbol, type_id);
        }
        break;
    }

    // 5. Compile glue spec through checked artifacts and lower to LIR.
    const glue_spec_abs = std.fs.cwd().realpathAlloc(gpa, args.glue_spec) catch {
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

    // 6. Construct List(Types) as Roc C-ABI structs and invoke the LIR interpreter.
    const hosted_function_ptrs = [_]builtins.host_abi.HostedFn{};
    var roc_ops = echo_platform.makeDefaultRocOps(@constCast(&hosted_function_ptrs));
    var types_list = constructTypesRocList(collected_modules.items, &platform_info, provides_entries.items, &type_table, &entrypoint_type_ids, &provides_type_ids, &roc_ops);

    var interpreter = eval_mod.LirInterpreter.init(
        gpa,
        &lowered.lir_result.store,
        &lowered.lir_result.layouts,
        &roc_ops,
    ) catch {
        return error.OutOfMemory;
    };
    defer interpreter.deinit();

    const arg_layouts = argLayoutsForProc(gpa, &lowered.lir_result.store, glue_proc) catch {
        return error.OutOfMemory;
    };
    defer gpa.free(arg_layouts);

    var result_buf: ResultListFileStr = undefined;
    const proc = lowered.lir_result.store.getProcSpec(glue_proc);
    _ = interpreter.eval(.{
        .proc_id = glue_proc,
        .arg_layouts = arg_layouts,
        .ret_layout = proc.ret_layout,
        .arg_ptr = @ptrCast(&types_list),
        .ret_ptr = @ptrCast(&result_buf),
    }) catch |err| {
        stderr.print("Error running glue spec: {}\n", .{err}) catch {};
        return error.CompilationFailed;
    };

    const glue_result = extractGlueResult(&result_buf);
    if (glue_result.err_msg) |err_msg| {
        stderr.print("Glue spec error: {s}\n", .{err_msg}) catch {};
        return error.CompilationFailed;
    }

    const files = glue_result.files;
    if (files.len == 0) {
        stdout.print("Glue spec returned 0 files.\n", .{}) catch {};
        return;
    }

    std.fs.cwd().makePath(args.output_dir) catch {
        stderr.print("Error: Could not create output directory: {s}\n", .{args.output_dir}) catch {};
        return error.CompilationFailed;
    };

    stdout.print("Glue spec returned {d} file(s):\n", .{files.len}) catch {};
    for (files) |file| {
        const file_name = file.name.asSlice();
        const file_path = std.fs.path.join(gpa, &.{ args.output_dir, file_name }) catch {
            return error.OutOfMemory;
        };
        defer gpa.free(file_path);

        std.fs.cwd().writeFile(.{
            .sub_path = file_path,
            .data = file.content.asSlice(),
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
    var source = std.fs.cwd().readFileAlloc(gpa, platform_path, std.math.maxInt(usize)) catch |err| switch (err) {
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

/// Builds a type table from compiler type vars, deduplicating entries.
const TypeTable = struct {
    entries: std.ArrayList(CollectedTypeRepr),
    var_map: std.AutoHashMap(@import("types").Var, u64),
    gpa: std.mem.Allocator,

    const types = @import("types");

    fn init(gpa: std.mem.Allocator) TypeTable {
        return .{
            .entries = std.ArrayList(CollectedTypeRepr).empty,
            .var_map = std.AutoHashMap(types.Var, u64).init(gpa),
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

    /// Clear the var map when switching modules (vars are module-local).
    fn clearVarMap(self: *TypeTable) void {
        self.var_map.clearRetainingCapacity();
    }

    /// Get an existing type table index for a var, or insert a new entry.
    /// Pre-registers a placeholder before conversion to prevent infinite recursion
    /// on cyclic types (the placeholder is updated in-place after conversion).
    fn getOrInsert(self: *TypeTable, env: *const ModuleEnv, type_var: types.Var) u64 {
        const resolved = env.types.resolveVar(type_var);
        const root_var = resolved.var_;

        if (self.var_map.get(root_var)) |idx| {
            return idx;
        }

        // Pre-register placeholder to break cycles
        const idx: u64 = @intCast(self.entries.items.len);
        self.entries.append(self.gpa, .{ .unknown = "" }) catch return 0;
        self.var_map.put(root_var, idx) catch {};

        const repr = self.convertContent(env, resolved.desc.content);

        // Update placeholder with actual representation
        self.entries.items[@intCast(idx)] = repr;

        // Assign synthetic names to anonymous records so glue generates struct defs
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

    fn convertContent(self: *TypeTable, env: *const ModuleEnv, content: types.Content) CollectedTypeRepr {
        switch (content) {
            .structure => |flat_type| return self.convertFlatType(env, flat_type),
            .alias => |alias| {
                const backing_var = env.types.getAliasBackingVar(alias);
                return self.convertContent(env, env.types.resolveVar(backing_var).desc.content);
            },
            .flex => return .{ .unknown = self.gpa.dupe(u8, "flex") catch "" },
            .rigid => return .{ .unknown = self.gpa.dupe(u8, "rigid") catch "" },
            .err => return .{ .unknown = self.gpa.dupe(u8, "error") catch "" },
        }
    }

    fn convertFlatType(self: *TypeTable, env: *const ModuleEnv, flat_type: types.FlatType) CollectedTypeRepr {
        switch (flat_type) {
            .nominal_type => |nominal| return self.convertNominal(env, nominal),
            .record => |record| return self.convertRecord(env, record),
            .tag_union => |tag_union| return self.convertTagUnion(env, tag_union),
            .fn_pure, .fn_effectful, .fn_unbound => |func| return self.convertFunc(env, func),
            .empty_record => return .unit,
            .empty_tag_union => return .unit,
            .tuple => |tuple| return self.convertTuple(env, tuple),
            .record_unbound => return .{ .unknown = self.gpa.dupe(u8, "record_unbound") catch "" },
        }
    }

    fn convertNominal(self: *TypeTable, env: *const ModuleEnv, nominal: types.NominalType) CollectedTypeRepr {
        const ident_store = env.getIdentStoreConst();
        const raw_name = ident_store.getText(nominal.ident.ident_idx);
        const display_name = getTypeDisplayName(raw_name);

        // Check for known builtin types
        if (std.mem.eql(u8, display_name, "List")) {
            const args = env.types.sliceNominalArgs(nominal);
            if (args.len >= 1) {
                const elem_id = self.getOrInsert(env, args[0]);
                return .{ .list = elem_id };
            }
            return .{ .unknown = self.gpa.dupe(u8, "List") catch "" };
        }
        if (std.mem.eql(u8, display_name, "Box")) {
            const args = env.types.sliceNominalArgs(nominal);
            if (args.len >= 1) {
                const inner_id = self.getOrInsert(env, args[0]);
                return .{ .box = inner_id };
            }
            return .{ .unknown = self.gpa.dupe(u8, "Box") catch "" };
        }
        if (std.mem.eql(u8, display_name, "Str")) return .str_;
        if (std.mem.eql(u8, display_name, "Bool")) return .bool_;
        if (std.mem.eql(u8, display_name, "Dec")) return .dec;
        if (std.mem.eql(u8, display_name, "U8")) return .u8_;
        if (std.mem.eql(u8, display_name, "U16")) return .u16_;
        if (std.mem.eql(u8, display_name, "U32")) return .u32_;
        if (std.mem.eql(u8, display_name, "U64")) return .u64_;
        if (std.mem.eql(u8, display_name, "U128")) return .u128_;
        if (std.mem.eql(u8, display_name, "I8")) return .i8_;
        if (std.mem.eql(u8, display_name, "I16")) return .i16_;
        if (std.mem.eql(u8, display_name, "I32")) return .i32_;
        if (std.mem.eql(u8, display_name, "I64")) return .i64_;
        if (std.mem.eql(u8, display_name, "I128")) return .i128_;
        if (std.mem.eql(u8, display_name, "F32")) return .f32_;
        if (std.mem.eql(u8, display_name, "F64")) return .f64_;

        // Not a known builtin — check if it's an opaque wrapping something
        if (nominal.vars.nonempty.count > 0) {
            const backing_var = env.types.getNominalBackingVar(nominal);
            const backing_resolved = env.types.resolveVar(backing_var);

            // If it wraps a record, convert it but preserve the qualified name
            if (backing_resolved.desc.content.unwrapRecord()) |record| {
                const record_repr = self.convertRecord(env, record);
                switch (record_repr) {
                    .record => |rec| {
                        return .{ .record = .{
                            .name = self.gpa.dupe(u8, display_name) catch "",
                            .fields = rec.fields,
                            .size = rec.size,
                            .alignment = rec.alignment,
                        } };
                    },
                    else => return record_repr,
                }
            }

            // If it wraps a tag union, convert it but preserve the qualified name
            if (backing_resolved.desc.content.unwrapTagUnion()) |tu| {
                const tu_repr = self.convertTagUnion(env, tu);
                switch (tu_repr) {
                    .tag_union => |collected_tu| {
                        // Free the auto-generated name from convertTagUnion
                        // before replacing it with the nominal's display name.
                        self.freeDuped(collected_tu.name);
                        return .{ .tag_union = .{
                            .name = self.gpa.dupe(u8, display_name) catch "",
                            .tags = collected_tu.tags,
                            .size = collected_tu.size,
                            .alignment = collected_tu.alignment,
                        } };
                    },
                    else => return tu_repr,
                }
            }

            // Otherwise, follow the backing var
            return self.convertContent(env, backing_resolved.desc.content);
        }

        return .{ .unknown = self.gpa.dupe(u8, display_name) catch "" };
    }

    fn convertRecord(self: *TypeTable, env: *const ModuleEnv, record: types.Record) CollectedTypeRepr {
        const ident_store = env.getIdentStoreConst();
        const fields_slice = env.types.getRecordFieldsSlice(record.fields);
        const field_names = fields_slice.items(.name);
        const field_vars = fields_slice.items(.var_);

        if (field_names.len == 0) return .unit;

        // First pass: getOrInsert all field type_ids so nested types are in the table
        const field_type_ids = self.gpa.alloc(u64, field_names.len) catch return self.oomUnknown("record");
        defer self.gpa.free(field_type_ids);
        for (0..field_names.len) |i| {
            field_type_ids[i] = self.getOrInsert(env, field_vars[i]);
        }

        // Get size/alignment for each field
        const field_sizes = self.gpa.alloc(SizeAlign, field_names.len) catch return self.oomUnknown("record");
        defer self.gpa.free(field_sizes);
        for (0..field_names.len) |i| {
            field_sizes[i] = self.getSizeAlign(field_type_ids[i]);
        }

        // Build sortable array of field indices
        var field_indices = self.gpa.alloc(usize, field_names.len) catch return self.oomUnknown("record");
        defer self.gpa.free(field_indices);
        for (0..field_names.len) |i| {
            field_indices[i] = i;
        }

        // Sort by alignment descending, then name ascending (matching store.zig ABI)
        const SortCtx = struct {
            names: []const base.Ident.Idx,
            idents: *const base.Ident.Store,
            sizes: []const SizeAlign,

            pub fn lessThan(ctx: @This(), a: usize, b: usize) bool {
                const a_align = ctx.sizes[a].alignment;
                const b_align = ctx.sizes[b].alignment;
                if (a_align != b_align) {
                    return a_align > b_align; // descending alignment
                }
                const a_text = ctx.idents.getText(ctx.names[a]);
                const b_text = ctx.idents.getText(ctx.names[b]);
                return std.mem.order(u8, a_text, b_text) == .lt;
            }
        };
        std.mem.sort(usize, field_indices, SortCtx{ .names = field_names, .idents = ident_store, .sizes = field_sizes }, SortCtx.lessThan);

        // Build collected fields in sorted order and compute record size
        const collected_fields = self.gpa.alloc(CollectedRecordField, field_names.len) catch return self.oomUnknown("record");
        var max_alignment: u64 = 0;
        var current_offset: u64 = 0;
        for (field_indices, 0..) |src_idx, dst_idx| {
            const name_text = ident_store.getText(field_names[src_idx]);
            const f_size = field_sizes[src_idx].size;
            const f_align = field_sizes[src_idx].alignment;

            // Track max alignment for the record
            if (f_align > max_alignment) max_alignment = f_align;

            // Align current offset
            if (f_align > 0) {
                const rem = current_offset % f_align;
                if (rem != 0) current_offset += f_align - rem;
            }
            current_offset += f_size;

            collected_fields[dst_idx] = .{
                .name = self.gpa.dupe(u8, name_text) catch "",
                .type_id = field_type_ids[src_idx],
                .size = f_size,
                .alignment = f_align,
            };
        }

        // Round total size up to max alignment
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

    fn convertTuple(self: *TypeTable, env: *const ModuleEnv, tuple: types.Tuple) CollectedTypeRepr {
        const elem_vars = env.types.sliceVars(tuple.elems);
        if (elem_vars.len == 0) return .unit;

        // Convert tuple elements as record fields with positional names (_0, _1, ...)
        const field_type_ids = self.gpa.alloc(u64, elem_vars.len) catch return self.oomUnknown("tuple");
        defer self.gpa.free(field_type_ids);
        for (elem_vars, 0..) |ev, i| {
            field_type_ids[i] = self.getOrInsert(env, ev);
        }

        const field_sizes = self.gpa.alloc(SizeAlign, elem_vars.len) catch return self.oomUnknown("tuple");
        defer self.gpa.free(field_sizes);
        for (0..elem_vars.len) |i| {
            field_sizes[i] = self.getSizeAlign(field_type_ids[i]);
        }

        // Generate positional field names (_0, _1, ...) before sorting
        const field_names = self.gpa.alloc([]const u8, elem_vars.len) catch return self.oomUnknown("tuple");
        defer self.gpa.free(field_names);
        for (0..elem_vars.len) |i| {
            field_names[i] = std.fmt.allocPrint(self.gpa, "_{d}", .{i}) catch "";
        }

        // Sort by alignment descending, then name ascending (matching Roc ABI)
        var field_indices = self.gpa.alloc(usize, elem_vars.len) catch return self.oomUnknown("tuple");
        defer self.gpa.free(field_indices);
        for (0..elem_vars.len) |i| {
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

        const collected_fields = self.gpa.alloc(CollectedRecordField, elem_vars.len) catch return self.oomUnknown("tuple");
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

    fn convertTagUnion(self: *TypeTable, env: *const ModuleEnv, tag_union: types.TagUnion) CollectedTypeRepr {
        const ident_store = env.getIdentStoreConst();
        const tags_slice = env.types.getTagsSlice(tag_union.tags);
        const tag_names = tags_slice.items(.name);
        const tag_args = tags_slice.items(.args);

        if (tag_names.len == 0) return .unit;

        // Build sortable array of tag indices
        var tag_indices = self.gpa.alloc(usize, tag_names.len) catch return self.oomUnknown("tag_union");
        defer self.gpa.free(tag_indices);
        for (0..tag_names.len) |i| {
            tag_indices[i] = i;
        }

        // Sort by name (alphabetical = discriminant order)
        const SortCtx = struct {
            names: []const base.Ident.Idx,
            idents: *const base.Ident.Store,

            pub fn lessThan(ctx: @This(), a: usize, b: usize) bool {
                const a_text = ctx.idents.getText(ctx.names[a]);
                const b_text = ctx.idents.getText(ctx.names[b]);
                return std.mem.order(u8, a_text, b_text) == .lt;
            }
        };
        std.mem.sort(usize, tag_indices, SortCtx{ .names = tag_names, .idents = ident_store }, SortCtx.lessThan);

        // Collect tags and compute per-variant payload layout
        const collected_tags = self.gpa.alloc(CollectedTagInfo, tag_names.len) catch return self.oomUnknown("tag_union");
        var max_payload_size: u64 = 0;
        var max_payload_alignment: u64 = 0;

        // Also build auto-generated name from variant names joined with "Or"
        var name_len: usize = 0;
        for (tag_indices) |src_idx| {
            const nt = ident_store.getText(tag_names[src_idx]);
            name_len += nt.len;
        }
        // Add "Or" separators between names
        if (tag_names.len > 1) name_len += (tag_names.len - 1) * 2;
        const auto_name_buf: []u8 = self.gpa.alloc(u8, name_len) catch return self.oomUnknown("tag_union");
        var name_pos: usize = 0;

        for (tag_indices, 0..) |src_idx, dst_idx| {
            const name_text = ident_store.getText(tag_names[src_idx]);
            const args_range = tag_args[src_idx];
            const arg_vars = env.types.sliceVars(args_range);

            const payload_ids = self.gpa.alloc(u64, arg_vars.len) catch return self.oomUnknown("tag_union");
            for (arg_vars, 0..) |av, i| {
                payload_ids[i] = self.getOrInsert(env, av);
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
        const disc_size: u64 = if (tag_names.len <= 1) 0 else layout.TagUnionData.discriminantSize(tag_names.len);
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

    fn convertFunc(self: *TypeTable, env: *const ModuleEnv, func: types.Func) CollectedTypeRepr {
        const arg_vars = env.types.sliceVars(func.args);
        const arg_ids = self.gpa.alloc(u64, arg_vars.len) catch return self.oomUnknown("function");
        for (arg_vars, 0..) |av, i| {
            arg_ids[i] = self.getOrInsert(env, av);
        }
        const ret_id = self.getOrInsert(env, func.ret);

        return .{ .function = .{
            .arg_ids = arg_ids,
            .ret_id = ret_id,
        } };
    }

    /// Strip "Builtin." and "Num." prefixes from type names (mirrors TypeWriter.getDisplayName).
    fn getTypeDisplayName(raw_name: []const u8) []const u8 {
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

// Roc C-ABI struct definitions for glue platform types.
// Fields are ordered alphabetically to match Roc's C ABI layout.

/// RecordFieldInfo := { name : Str, type_str : Str }
const RecordFieldInfoRoc = extern struct {
    name: RocStr, // offset 0
    type_str: RocStr, // offset 24
};

/// HostedFunctionInfo := { arg_fields : List(RecordFieldInfo), arg_type_ids : List(U64), index : U64, name : Str, ret_fields : List(RecordFieldInfo), ret_type_id : U64, type_str : Str }
const HostedFunctionInfoRoc = extern struct {
    arg_fields: RocList,
    arg_type_ids: RocList,
    index: u64,
    name: RocStr,
    ret_fields: RocList,
    ret_type_id: u64,
    type_str: RocStr,
};

/// FunctionInfo := { name : Str, type_str : Str }
const FunctionInfoRoc = extern struct {
    name: RocStr,
    type_str: RocStr,
};

/// ModuleTypeInfo := { functions : List(FunctionInfo), hosted_functions : List(HostedFunctionInfo), main_type : Str, name : Str }
const ModuleTypeInfoRoc = extern struct {
    functions: RocList,
    hosted_functions: RocList,
    main_type: RocStr,
    name: RocStr,
};

/// ProvidesEntry := { ffi_symbol : Str, name : Str, type_id : U64 }
/// Fields ordered by alignment descending, then alphabetically
const ProvidesEntryRoc = extern struct {
    ffi_symbol: RocStr,
    name: RocStr,
    type_id: u64,
};

/// Types := { entrypoints : List(EntryPoint), modules : List(ModuleTypeInfo), provides_entries : List(ProvidesEntry), type_table : List(TypeRepr) }
/// Fields ordered by alignment descending, then alphabetically
const TypesInnerRoc = extern struct {
    entrypoints: RocList,
    modules: RocList,
    provides_entries: RocList,
    type_table: RocList,
};

/// File := { name : Str, content : Str }
const FileRoc = extern struct {
    content: RocStr,
    name: RocStr,
};

/// Result tag: Err=0, Ok=1 (alphabetical)
const ResultTag = enum(u8) {
    Err = 0,
    Ok = 1,
};

/// Try(List(File), Str) result layout
const ResultListFileStr = extern struct {
    payload: extern union {
        ok: RocList,
        err: RocStr,
    },
    tag: ResultTag,
};

// TypeRepr ABI structs for the type table

/// Tag discriminant for TypeRepr tagged union (21 variants, alphabetical with Roc prefix)
const TypeReprTag = enum(u8) {
    RocBool = 0,
    RocBox = 1,
    RocDec = 2,
    RocF32 = 3,
    RocF64 = 4,
    RocFunction = 5,
    RocI128 = 6,
    RocI16 = 7,
    RocI32 = 8,
    RocI64 = 9,
    RocI8 = 10,
    RocList = 11,
    RocRecord = 12,
    RocStr = 13,
    RocTagUnion = 14,
    RocU128 = 15,
    RocU16 = 16,
    RocU32 = 17,
    RocU64 = 18,
    RocU8 = 19,
    RocUnit = 20,
    RocUnknown = 21,
};

/// FunctionRepr := { args : List(U64), ret : U64 } — fields alphabetical
const FunctionPayload = extern struct {
    args: RocList,
    ret: u64,
};

/// RecordRepr := { alignment : U64, fields : List(RecordField), name : Str, size : U64 } — fields alphabetical
const RecordPayload = extern struct {
    alignment: u64,
    fields: RocList,
    name: RocStr,
    size: u64,
};

/// TagUnionRepr := { alignment : U64, name : Str, size : U64, tags : List(TagVariant) } — fields alphabetical
const TagUnionPayload = extern struct {
    alignment: u64,
    name: RocStr,
    size: u64,
    tags: RocList,
};

/// Payload union for TypeRepr — max payload is 64 bytes (RecordPayload)
const TypeReprPayload = extern union {
    box_elem: u64,
    function: FunctionPayload,
    list_elem: u64,
    record: RecordPayload,
    tag_union: TagUnionPayload,
    unknown: RocStr,
};

/// TypeRepr Roc ABI layout: payload then discriminant
const TypeReprRoc = extern struct {
    payload: TypeReprPayload,
    tag: TypeReprTag,
};

/// RecordField := { alignment : U64, name : Str, size : U64, type_id : U64 }
const RecordFieldTypeReprRoc = extern struct {
    alignment: u64,
    name: RocStr,
    size: u64,
    type_id: u64,
};

/// TagVariant := { name : Str, payload : List(U64), payload_alignment : U64, payload_size : U64 }
const TagVariantRoc = extern struct {
    name: RocStr,
    payload: RocList,
    payload_alignment: u64,
    payload_size: u64,
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

/// Build a RocList of RecordFieldInfoRoc from collected field info.
fn buildRecordFieldsRocList(
    fields: []const CollectedModuleTypeInfo.CollectedRecordFieldInfo,
    roc_ops: *builtins.host_abi.RocOps,
) RocList {
    if (fields.len == 0) return RocList.empty();

    const data_size = fields.len * @sizeOf(RecordFieldInfoRoc);
    const bytes = builtins.utils.allocateWithRefcount(
        data_size,
        @alignOf(RecordFieldInfoRoc),
        true,
        roc_ops,
    );
    const ptr: [*]RecordFieldInfoRoc = @ptrCast(@alignCast(bytes));

    for (fields, 0..) |field, i| {
        ptr[i] = RecordFieldInfoRoc{
            .name = createBigRocStr(field.name, roc_ops),
            .type_str = createBigRocStr(field.type_str, roc_ops),
        };
    }

    return RocList{
        .bytes = bytes,
        .length = fields.len,
        .capacity_or_alloc_ptr = fields.len,
    };
}

/// Build a RocList of u64 from a slice of u64.
fn buildU64RocList(
    ids: []const u64,
    roc_ops: *builtins.host_abi.RocOps,
) RocList {
    if (ids.len == 0) return RocList.empty();

    const data_size = ids.len * @sizeOf(u64);
    const bytes = builtins.utils.allocateWithRefcount(
        data_size,
        @alignOf(u64),
        false, // u64 elements are not refcounted
        roc_ops,
    );
    const ptr: [*]u64 = @ptrCast(@alignCast(bytes));
    for (ids, 0..) |id, i| {
        ptr[i] = id;
    }
    return RocList{
        .bytes = bytes,
        .length = ids.len,
        .capacity_or_alloc_ptr = ids.len,
    };
}

/// Serialize a CollectedTypeRepr into a TypeReprRoc for the Roc ABI.
fn serializeTypeRepr(
    entry: CollectedTypeRepr,
    roc_ops: *builtins.host_abi.RocOps,
) TypeReprRoc {
    var result: TypeReprRoc = undefined;
    // Zero-initialize the payload to avoid undefined bytes
    result.payload = std.mem.zeroes(TypeReprPayload);

    switch (entry) {
        .bool_ => result.tag = .RocBool,
        .box => |inner_id| {
            result.tag = .RocBox;
            result.payload.box_elem = inner_id;
        },
        .dec => result.tag = .RocDec,
        .f32_ => result.tag = .RocF32,
        .f64_ => result.tag = .RocF64,
        .i8_ => result.tag = .RocI8,
        .i16_ => result.tag = .RocI16,
        .i32_ => result.tag = .RocI32,
        .i64_ => result.tag = .RocI64,
        .i128_ => result.tag = .RocI128,
        .u8_ => result.tag = .RocU8,
        .u16_ => result.tag = .RocU16,
        .u32_ => result.tag = .RocU32,
        .u64_ => result.tag = .RocU64,
        .u128_ => result.tag = .RocU128,
        .str_ => result.tag = .RocStr,
        .unit => result.tag = .RocUnit,
        .list => |elem_id| {
            result.tag = .RocList;
            result.payload.list_elem = elem_id;
        },
        .function => |func| {
            result.tag = .RocFunction;
            result.payload.function = .{
                .args = buildU64RocList(func.arg_ids, roc_ops),
                .ret = func.ret_id,
            };
        },
        .record => |rec| {
            result.tag = .RocRecord;
            // Build RocList of RecordFieldTypeReprRoc
            const fields_list = if (rec.fields.len > 0) fblk: {
                const data_size = rec.fields.len * @sizeOf(RecordFieldTypeReprRoc);
                const fb = builtins.utils.allocateWithRefcount(
                    data_size,
                    @alignOf(RecordFieldTypeReprRoc),
                    true,
                    roc_ops,
                );
                const fptr: [*]RecordFieldTypeReprRoc = @ptrCast(@alignCast(fb));
                for (rec.fields, 0..) |field, i| {
                    fptr[i] = .{
                        .alignment = field.alignment,
                        .name = createBigRocStr(field.name, roc_ops),
                        .size = field.size,
                        .type_id = field.type_id,
                    };
                }
                break :fblk RocList{
                    .bytes = fb,
                    .length = rec.fields.len,
                    .capacity_or_alloc_ptr = rec.fields.len,
                };
            } else RocList.empty();

            result.payload.record = .{
                .alignment = rec.alignment,
                .fields = fields_list,
                .name = createBigRocStr(rec.name, roc_ops),
                .size = rec.size,
            };
        },
        .tag_union => |tu| {
            result.tag = .RocTagUnion;
            // Build RocList of TagVariantRoc
            const tags_list = if (tu.tags.len > 0) tblk: {
                const data_size = tu.tags.len * @sizeOf(TagVariantRoc);
                const tb = builtins.utils.allocateWithRefcount(
                    data_size,
                    @alignOf(TagVariantRoc),
                    true,
                    roc_ops,
                );
                const tptr: [*]TagVariantRoc = @ptrCast(@alignCast(tb));
                for (tu.tags, 0..) |tag, i| {
                    tptr[i] = .{
                        .name = createBigRocStr(tag.name, roc_ops),
                        .payload = buildU64RocList(tag.payload_ids, roc_ops),
                        .payload_alignment = tag.payload_alignment,
                        .payload_size = tag.payload_size,
                    };
                }
                break :tblk RocList{
                    .bytes = tb,
                    .length = tu.tags.len,
                    .capacity_or_alloc_ptr = tu.tags.len,
                };
            } else RocList.empty();

            result.payload.tag_union = .{
                .alignment = tu.alignment,
                .name = createBigRocStr(tu.name, roc_ops),
                .size = tu.size,
                .tags = tags_list,
            };
        },
        .unknown => |text| {
            result.tag = .RocUnknown;
            result.payload.unknown = createBigRocStr(text, roc_ops);
        },
    }
    return result;
}

/// Build a RocList of TypeReprRoc from the type table.
fn buildTypeTableRocList(
    type_table: *const TypeTable,
    roc_ops: *builtins.host_abi.RocOps,
) RocList {
    if (type_table.entries.items.len == 0) return RocList.empty();

    const data_size = type_table.entries.items.len * @sizeOf(TypeReprRoc);
    const bytes = builtins.utils.allocateWithRefcount(
        data_size,
        @alignOf(TypeReprRoc),
        true,
        roc_ops,
    );
    const ptr: [*]TypeReprRoc = @ptrCast(@alignCast(bytes));

    for (type_table.entries.items, 0..) |entry, i| {
        ptr[i] = serializeTypeRepr(entry, roc_ops);
    }

    return RocList{
        .bytes = bytes,
        .length = type_table.entries.items.len,
        .capacity_or_alloc_ptr = type_table.entries.items.len,
    };
}

/// Construct the List(Types) Roc value from collected module type info.
fn constructTypesRocList(
    collected_modules: []const CollectedModuleTypeInfo,
    platform_info: *const PlatformHeaderInfo,
    provides_entries: []const PlatformHeaderInfo.ProvidesEntry,
    type_table: *const TypeTable,
    entrypoint_type_ids: *const std.StringHashMap(u64),
    provides_type_ids: *const std.StringHashMap(u64),
    roc_ops: *builtins.host_abi.RocOps,
) RocList {
    // Build modules list
    const modules_list = if (collected_modules.len > 0) blk: {
        const modules_data_size = collected_modules.len * @sizeOf(ModuleTypeInfoRoc);
        const modules_bytes = builtins.utils.allocateWithRefcount(
            modules_data_size,
            @alignOf(ModuleTypeInfoRoc),
            true,
            roc_ops,
        );
        const modules_ptr: [*]ModuleTypeInfoRoc = @ptrCast(@alignCast(modules_bytes));

        for (collected_modules, 0..) |mod, mod_idx| {
            // Build functions list
            const functions_list = if (mod.functions.items.len > 0) fblk: {
                const funcs_data_size = mod.functions.items.len * @sizeOf(FunctionInfoRoc);
                const funcs_bytes = builtins.utils.allocateWithRefcount(
                    funcs_data_size,
                    @alignOf(FunctionInfoRoc),
                    true,
                    roc_ops,
                );
                const funcs_ptr: [*]FunctionInfoRoc = @ptrCast(@alignCast(funcs_bytes));

                for (mod.functions.items, 0..) |func, func_idx| {
                    funcs_ptr[func_idx] = FunctionInfoRoc{
                        .name = createBigRocStr(func.name, roc_ops),
                        .type_str = createBigRocStr(func.type_str, roc_ops),
                    };
                }

                break :fblk RocList{
                    .bytes = funcs_bytes,
                    .length = mod.functions.items.len,
                    .capacity_or_alloc_ptr = mod.functions.items.len,
                };
            } else RocList.empty();

            // Build hosted_functions list
            const hosted_functions_list = if (mod.hosted_functions.items.len > 0) hblk: {
                const hosted_data_size = mod.hosted_functions.items.len * @sizeOf(HostedFunctionInfoRoc);
                const hosted_bytes = builtins.utils.allocateWithRefcount(
                    hosted_data_size,
                    @alignOf(HostedFunctionInfoRoc),
                    true,
                    roc_ops,
                );
                const hosted_ptr: [*]HostedFunctionInfoRoc = @ptrCast(@alignCast(hosted_bytes));

                for (mod.hosted_functions.items, 0..) |hosted, hosted_idx| {
                    hosted_ptr[hosted_idx] = HostedFunctionInfoRoc{
                        .arg_fields = buildRecordFieldsRocList(hosted.arg_fields, roc_ops),
                        .arg_type_ids = buildU64RocList(hosted.arg_type_ids, roc_ops),
                        .index = hosted.index,
                        .name = createBigRocStr(hosted.name, roc_ops),
                        .ret_fields = buildRecordFieldsRocList(hosted.ret_fields, roc_ops),
                        .ret_type_id = hosted.ret_type_id,
                        .type_str = createBigRocStr(hosted.type_str, roc_ops),
                    };
                }

                break :hblk RocList{
                    .bytes = hosted_bytes,
                    .length = mod.hosted_functions.items.len,
                    .capacity_or_alloc_ptr = mod.hosted_functions.items.len,
                };
            } else RocList.empty();

            modules_ptr[mod_idx] = ModuleTypeInfoRoc{
                .functions = functions_list,
                .hosted_functions = hosted_functions_list,
                .main_type = createBigRocStr(mod.main_type, roc_ops),
                .name = createBigRocStr(mod.name, roc_ops),
            };
        }

        break :blk RocList{
            .bytes = modules_bytes,
            .length = collected_modules.len,
            .capacity_or_alloc_ptr = collected_modules.len,
        };
    } else RocList.empty();

    // Build entrypoints list
    const EntryPointRoc = extern struct {
        name: RocStr,
        type_id: u64,
    };

    const entrypoints_list = if (platform_info.requires_entries.len > 0) eblk: {
        const ep_data_size = platform_info.requires_entries.len * @sizeOf(EntryPointRoc);
        const ep_bytes = builtins.utils.allocateWithRefcount(
            ep_data_size,
            @alignOf(EntryPointRoc),
            true,
            roc_ops,
        );
        const ep_ptr: [*]EntryPointRoc = @ptrCast(@alignCast(ep_bytes));

        for (platform_info.requires_entries, 0..) |entry, idx| {
            const tid = entrypoint_type_ids.get(entry.name) orelse 0;
            ep_ptr[idx] = EntryPointRoc{
                .name = createBigRocStr(entry.name, roc_ops),
                .type_id = tid,
            };
        }

        break :eblk RocList{
            .bytes = ep_bytes,
            .length = platform_info.requires_entries.len,
            .capacity_or_alloc_ptr = platform_info.requires_entries.len,
        };
    } else RocList.empty();

    // Build provides list
    const provides_list = if (provides_entries.len > 0) pblk: {
        const prov_data_size = provides_entries.len * @sizeOf(ProvidesEntryRoc);
        const prov_bytes = builtins.utils.allocateWithRefcount(
            prov_data_size,
            @alignOf(ProvidesEntryRoc),
            true,
            roc_ops,
        );
        const prov_ptr: [*]ProvidesEntryRoc = @ptrCast(@alignCast(prov_bytes));

        for (provides_entries, 0..) |entry, idx| {
            prov_ptr[idx] = ProvidesEntryRoc{
                .ffi_symbol = createBigRocStr(entry.ffi_symbol, roc_ops),
                .name = createBigRocStr(entry.name, roc_ops),
                .type_id = provides_type_ids.get(entry.ffi_symbol) orelse 0,
            };
        }

        break :pblk RocList{
            .bytes = prov_bytes,
            .length = provides_entries.len,
            .capacity_or_alloc_ptr = provides_entries.len,
        };
    } else RocList.empty();

    // Build TypesInner and wrap in a List(Types) with one element
    const types_inner_bytes = builtins.utils.allocateWithRefcount(
        @sizeOf(TypesInnerRoc),
        @alignOf(TypesInnerRoc),
        true,
        roc_ops,
    );
    const types_inner_ptr: *TypesInnerRoc = @ptrCast(@alignCast(types_inner_bytes));
    types_inner_ptr.* = TypesInnerRoc{
        .entrypoints = entrypoints_list,
        .modules = modules_list,
        .provides_entries = provides_list,
        .type_table = buildTypeTableRocList(type_table, roc_ops),
    };

    return RocList{
        .bytes = types_inner_bytes,
        .length = 1,
        .capacity_or_alloc_ptr = 1,
    };
}

/// Extract files from a Try(List(File), Str) result buffer.
/// Returns the file list on Ok, or an error message on Err.
const GlueResultFiles = struct {
    files: []const FileRoc,
    err_msg: ?[]const u8,
};

fn extractGlueResult(result: *const ResultListFileStr) GlueResultFiles {
    switch (result.tag) {
        .Ok => {
            const files = result.payload.ok;
            if (files.bytes) |file_bytes| {
                const file_slice: [*]const FileRoc = @ptrCast(@alignCast(file_bytes));
                return .{ .files = file_slice[0..files.length], .err_msg = null };
            }
            return .{ .files = &[_]FileRoc{}, .err_msg = null };
        },
        .Err => {
            return .{ .files = &[_]FileRoc{}, .err_msg = result.payload.err.asSlice() };
        },
    }
}

/// Extract record fields from a type variable, returning field names and type strings.
/// If the type is a nominal wrapping a record, unwraps the nominal first.
/// Returns an empty slice for non-record types.
fn extractRecordFields(
    gpa: std.mem.Allocator,
    env: *ModuleEnv,
    type_var: @import("types").Var,
) []const CollectedModuleTypeInfo.CollectedRecordFieldInfo {
    const resolved = env.types.resolveVar(type_var);

    // Check for nominal type wrapping a record
    const content = switch (resolved.desc.content) {
        .structure => |flat_type| switch (flat_type) {
            .nominal_type => |nominal| blk: {
                if (nominal.vars.nonempty.count > 0) {
                    const backing_var = env.types.getNominalBackingVar(nominal);
                    const backing_resolved = env.types.resolveVar(backing_var);
                    break :blk backing_resolved.desc.content;
                }
                break :blk resolved.desc.content;
            },
            else => resolved.desc.content,
        },
        else => resolved.desc.content,
    };

    // Check if the (possibly unwrapped) content is a record
    const record = content.unwrapRecord() orelse return &[_]CollectedModuleTypeInfo.CollectedRecordFieldInfo{};

    const fields_slice = env.types.getRecordFieldsSlice(record.fields);
    const field_names = fields_slice.items(.name);
    const field_vars = fields_slice.items(.var_);

    var result_list = std.ArrayList(CollectedModuleTypeInfo.CollectedRecordFieldInfo).empty;

    // Collect fields sorted by name (Roc's C ABI uses alphabetical order)
    const ident_store = env.getIdentStoreConst();

    // Build sortable array of field indices
    var field_indices = gpa.alloc(usize, field_names.len) catch return &[_]CollectedModuleTypeInfo.CollectedRecordFieldInfo{};
    defer gpa.free(field_indices);
    for (0..field_names.len) |i| {
        field_indices[i] = i;
    }

    // Sort by name text
    const SortCtx = struct {
        names: []const base.Ident.Idx,
        idents: *const base.Ident.Store,

        pub fn lessThan(ctx: @This(), a: usize, b: usize) bool {
            const a_text = ctx.idents.getText(ctx.names[a]);
            const b_text = ctx.idents.getText(ctx.names[b]);
            return std.mem.order(u8, a_text, b_text) == .lt;
        }
    };
    std.mem.sort(usize, field_indices, SortCtx{ .names = field_names, .idents = ident_store }, SortCtx.lessThan);

    for (field_indices) |idx| {
        const name_text = ident_store.getText(field_names[idx]);
        const field_var = field_vars[idx];

        // Write field type to string
        var type_writer = env.initTypeWriter() catch continue;
        defer type_writer.deinit();

        type_writer.write(field_var, .one_line) catch continue;
        const field_type_str = type_writer.get();

        result_list.append(gpa, .{
            .name = gpa.dupe(u8, name_text) catch continue,
            .type_str = gpa.dupe(u8, field_type_str) catch continue,
        }) catch continue;
    }

    return result_list.toOwnedSlice(gpa) catch &[_]CollectedModuleTypeInfo.CollectedRecordFieldInfo{};
}

/// Collect type information from a published checked artifact.
fn collectModuleTypeInfo(
    gpa: Allocator,
    artifact: *const CheckedArtifact.CheckedModuleArtifact,
    module_name: []const u8,
    hosted_indices: []const HostedProcGlobalIndex,
    type_table: *TypeTable,
) ?CollectedModuleTypeInfo {
    const env = @constCast(artifact.moduleEnvConst());

    // Find main type
    var main_type_str: []const u8 = "";
    const all_stmts = env.store.sliceStatements(env.all_statements);

    for (all_stmts) |stmt_idx| {
        const stmt = env.store.getStatement(stmt_idx);
        if (stmt == .s_nominal_decl) {
            const nominal = stmt.s_nominal_decl;
            const type_header = env.store.getTypeHeader(nominal.header);
            const type_name = env.getIdent(type_header.relative_name);

            if (std.mem.eql(u8, type_name, module_name)) {
                var type_writer = env.initTypeWriter() catch continue;
                defer type_writer.deinit();

                const anno_node_idx: @TypeOf(env.store.nodes).Idx = @enumFromInt(@intFromEnum(nominal.anno));
                const type_var = ModuleEnv.varFrom(anno_node_idx);

                type_writer.write(type_var, .one_line) catch continue;
                const type_str = type_writer.get();

                main_type_str = gpa.dupe(u8, type_str) catch "";
                break;
            }
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

        if (hostedProcForDef(&artifact.hosted_procs, def_idx)) |_| {
            var type_writer = env.initTypeWriter() catch continue;
            defer type_writer.deinit();

            const def_node_idx: @TypeOf(env.store.nodes).Idx = @enumFromInt(@intFromEnum(def_idx));
            const type_var = ModuleEnv.varFrom(def_node_idx);

            type_writer.write(type_var, .one_line) catch continue;
            const type_str = type_writer.get();

            // Extract record fields from function arg and return types.
            const resolved = env.types.resolveVar(type_var);
            var arg_fields: []const CollectedModuleTypeInfo.CollectedRecordFieldInfo = &.{};
            var ret_fields: []const CollectedModuleTypeInfo.CollectedRecordFieldInfo = &.{};

            if (resolved.desc.content.unwrapFunc()) |func| {
                ret_fields = extractRecordFields(gpa, env, func.ret);
                const arg_vars = env.types.sliceVars(func.args);
                if (arg_vars.len == 1) {
                    arg_fields = extractRecordFields(gpa, env, arg_vars[0]);
                }
            } else {
                switch (resolved.desc.content) {
                    .structure => |flat_type| {
                        switch (flat_type) {
                            .nominal_type => |nom| {
                                if (nom.vars.nonempty.count > 0) {
                                    const backing_var = env.types.getNominalBackingVar(nom);
                                    const backing_resolved = env.types.resolveVar(backing_var);
                                    if (backing_resolved.desc.content.unwrapFunc()) |func| {
                                        ret_fields = extractRecordFields(gpa, env, func.ret);
                                        const arg_vars = env.types.sliceVars(func.args);
                                        if (arg_vars.len == 1) {
                                            arg_fields = extractRecordFields(gpa, env, arg_vars[0]);
                                        }
                                    }
                                }
                            },
                            else => {},
                        }
                    },
                    else => {},
                }
            }

            var arg_type_ids: []const u64 = &.{};
            var ret_type_id: u64 = 0;

            const func_content = blk: {
                if (resolved.desc.content.unwrapFunc()) |func| break :blk func;
                if (resolved.desc.content.unwrapNominalType()) |nom| {
                    if (nom.vars.nonempty.count > 0) {
                        const bv = env.types.getNominalBackingVar(nom);
                        const br = env.types.resolveVar(bv);
                        if (br.desc.content.unwrapFunc()) |func| break :blk func;
                    }
                }
                break :blk null;
            };

            if (func_content) |func| {
                ret_type_id = type_table.getOrInsert(env, func.ret);
                const arg_vars_for_ids = env.types.sliceVars(func.args);
                if (arg_vars_for_ids.len > 0) {
                    const ids = gpa.alloc(u64, arg_vars_for_ids.len) catch continue;
                    for (arg_vars_for_ids, 0..) |av, i| {
                        ids[i] = type_table.getOrInsert(env, av);
                    }
                    arg_type_ids = ids;
                }
            } else {
                ret_type_id = type_table.insertUnit();
            }

            hosted_functions.append(gpa, .{
                .index = hostedGlobalIndexForDef(hosted_indices, artifact.key, def_idx),
                .name = gpa.dupe(u8, local_name) catch continue,
                .type_str = gpa.dupe(u8, type_str) catch continue,
                .arg_fields = arg_fields,
                .ret_fields = ret_fields,
                .arg_type_ids = arg_type_ids,
                .ret_type_id = ret_type_id,
            }) catch continue;
        } else switch (entry.value) {
            .procedure_binding => {
                var type_writer = env.initTypeWriter() catch continue;
                defer type_writer.deinit();

                const def_node_idx: @TypeOf(env.store.nodes).Idx = @enumFromInt(@intFromEnum(def_idx));
                const type_var = ModuleEnv.varFrom(def_node_idx);

                type_writer.write(type_var, .one_line) catch continue;
                const type_str = type_writer.get();

                functions.append(gpa, .{
                    .name = gpa.dupe(u8, local_name) catch continue,
                    .type_str = gpa.dupe(u8, type_str) catch continue,
                }) catch continue;
            },
            .const_ref => {},
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
