//! Build-time compiler for Roc builtin module (Builtin.roc).
//!
//! This executable runs during `zig build` on the host machine to:
//! 1. Parse and type-check the Builtin.roc module (which contains nested Bool, Try, Str, Dict, Set types)
//! 2. Serialize the resulting ModuleEnv to a binary file
//! 3. Publish the type-checked module to a CheckedModuleArtifact and serialize it
//! 4. Output Builtin.bin, builtin_indices.zig, and Builtin.artifact.bin to paths provided by the build system

const std = @import("std");
const base = @import("base");
const parse = @import("parse");
const can = @import("can");
const check = @import("check");
const collections = @import("collections");
const types = @import("types");
const reporting = @import("reporting");
const builtin_static = can.BuiltinStatic;
const comptime_finalizer = @import("comptime_finalizer");

const ModuleEnv = can.ModuleEnv;
const Can = can.Can;
const Check = check.Check;
const Allocator = std.mem.Allocator;
const CoreCtx = can.CoreCtx;
const CIR = can.CIR;
const CompactWriter = collections.CompactWriter;
const CheckedModuleArtifact = check.CheckedArtifact.CheckedModuleArtifact;

const max_builtin_bytes = 1024 * 1024;

// Stderr writer for diagnostic reporting
var stderr_buffer: [4096]u8 = undefined;
var stderr_writer: std.Io.File.Writer = undefined;
var stderr_initialized = false;

fn flushStderr() void {
    if (stderr_initialized) {
        stderr_writer.interface.flush() catch {};
    }
}

fn stderrWriter(io: std.Io) *std.Io.Writer {
    if (!stderr_initialized) {
        stderr_writer = std.Io.File.stderr().writer(io, &stderr_buffer);
        stderr_initialized = true;
    }
    return &stderr_writer.interface;
}

// Use the canonical BuiltinIndices from CIR
const BuiltinIndices = CIR.BuiltinIndices;

fn readFileAllocPath(gpa: Allocator, io: std.Io, path: []const u8, limit: usize) ![]u8 {
    if (std.fs.path.isAbsolute(path)) {
        const root_dir = try std.Io.Dir.openDirAbsolute(io, "/", .{});
        return try root_dir.readFileAlloc(io, path, gpa, .limited(limit));
    }
    return try std.Io.Dir.cwd().readFileAlloc(io, path, gpa, .limited(limit));
}

/// Build-time compiler that compiles builtin .roc sources into serialized ModuleEnvs.
/// This runs during `zig build` on the host machine to generate .bin files
/// that get embedded into the final roc executable.
///
/// The build system passes:
/// 1. the absolute path to Builtin.roc for cache tracking
/// 2. the output path for Builtin.bin
/// 3. the output path for builtin_indices.zig
/// 4. the output path for Builtin.artifact.bin
///
/// We also keep project-relative defaults so manual runs still succeed.
pub fn main(process_init: std.process.Init) !void {
    const gpa = process_init.gpa;
    const io = process_init.io;

    var args_list = std.ArrayList([:0]const u8).empty;
    defer args_list.deinit(gpa);
    var args_iter = try process_init.minimal.args.iterateAllocator(gpa);
    defer args_iter.deinit();
    while (args_iter.next()) |arg| {
        try args_list.append(gpa, arg);
    }
    const args = args_list.items;

    // Prefer the explicit paths provided by the build system, but fall back to the
    // project-relative defaults so manual runs still succeed.
    const builtin_src_path = if (args.len >= 2) args[1] else "src/build/roc/Builtin.roc";
    const builtin_bin_path = if (args.len >= 3) args[2] else "zig-out/builtins/Builtin.bin";
    const builtin_indices_zig_path = if (args.len >= 4) args[3] else "zig-out/builtins/builtin_indices.zig";
    const builtin_artifact_path = if (args.len >= 5) args[4] else "zig-out/builtins/Builtin.artifact.bin";

    // Read the Builtin.roc source file at runtime
    // NOTE: We must free this source manually; CommonEnv.deinit() does not free the source.
    const builtin_roc_source = try readFileAllocPath(gpa, io, builtin_src_path, max_builtin_bytes);

    // Compile Builtin.roc (it's completely self-contained)
    const builtin_env = try compileModule(
        gpa,
        io,
        "Builtin",
        builtin_roc_source,
        builtin_src_path,
        &.{}, // No module dependencies
        null, // bool_stmt not available yet (will be found within Builtin)
        null, // try_stmt not available yet (will be found within Builtin)
        null, // str_stmt not available yet (will be found within Builtin)
    );
    defer {
        builtin_env.deinit();
        gpa.destroy(builtin_env);
        gpa.free(builtin_roc_source);
    }

    const builtin_indices = try buildBuiltinIndices(gpa, builtin_env);
    try installBuiltinNodeIndices(gpa, builtin_env, builtin_indices);

    // Create output directories when needed.
    if (std.fs.path.dirname(builtin_bin_path)) |dir| {
        try std.Io.Dir.cwd().createDirPath(io, dir);
    }
    if (std.fs.path.dirname(builtin_indices_zig_path)) |dir| {
        try std.Io.Dir.cwd().createDirPath(io, dir);
    }
    if (std.fs.path.dirname(builtin_artifact_path)) |dir| {
        try std.Io.Dir.cwd().createDirPath(io, dir);
    }

    // Prepare once before baking so the compiler executable can view the static
    // env directly without enabling runtime inserts or finalizing method tables.
    try check.TypedCIR.prepareRuntimeEnv(gpa, builtin_env);

    // Serialize the single Builtin module
    try serializeModuleEnv(gpa, io, builtin_env, builtin_bin_path);

    // Publish the type-checked Builtin to a CheckedModuleArtifact and serialize
    // it from the same prepared env that is embedded as Builtin.bin.
    try serializeBuiltinArtifact(gpa, io, builtin_env, builtin_artifact_path);

    // Validate that BuiltinIndices contains all type declarations under Builtin
    // before emitting the typed static data consumed by the compiler executable.
    try builtin_static.validateBuiltinIndices(builtin_env, builtin_indices);

    try writeBuiltinIndicesZig(io, builtin_indices, builtin_indices_zig_path);
}

/// Publish the prepared Builtin env to a CheckedModuleArtifact, then serialize
/// the artifact to the output path.
fn serializeBuiltinArtifact(
    gpa: Allocator,
    io: std.Io,
    builtin_env: *ModuleEnv,
    output_path: []const u8,
) !void {
    var typed_modules = try check.TypedCIR.Modules.init(gpa, &.{
        .{ .precompiled = builtin_env },
    });
    defer typed_modules.deinit();

    var artifact = try check.CheckedArtifact.publishFromTypedModule(
        gpa,
        &typed_modules,
        0,
        .{
            .module_env_storage = .{ .checked_source = builtin_env },
            .compile_time_finalizer = comptime_finalizer.finalizer(),
        },
    );
    defer artifact.deinitRetainingModuleEnv(gpa);

    var arena = collections.SingleThreadArena.init(gpa);
    defer arena.deinit();
    const arena_alloc = arena.allocator();

    const file = try std.Io.Dir.cwd().createFile(io, output_path, .{ .read = true });
    defer file.close(io);

    var writer = CompactWriter.init();
    defer writer.deinit(arena_alloc);

    const hdr = try writer.appendAlloc(arena_alloc, CheckedModuleArtifact.Serialized);
    try hdr.serialize(&artifact, arena_alloc, &writer);

    try writer.writeGather(file, io);

    // Append the layout-version hash as a trailer after the serialized region.
    // `CheckedModuleArtifact.splitVersionTrailer` validates and strips it before
    // the runtime creates a static view, rejecting a blob whose layout differs
    // from the running compiler's instead of reading into a mismatched struct.
    try file.writePositionalAll(io, &CheckedModuleArtifact.SERIALIZED_VERSION_HASH, writer.total_bytes);
}

fn buildBuiltinIndices(gpa: Allocator, env: *const ModuleEnv) !BuiltinIndices {
    var result: BuiltinIndices = undefined;
    inline for (CIR.builtin_type_specs) |spec| {
        @field(result, spec.type_field) = try findBuiltinTypeDeclaration(gpa, env, spec);
        @field(result, spec.ident_field) = expectBuiltinIdent(env, spec.qualified_name);
    }
    result.ok_ident = expectBuiltinIdent(env, "Ok");
    result.err_ident = expectBuiltinIdent(env, "Err");
    return result;
}

fn findBuiltinTypeDeclaration(gpa: Allocator, env: *const ModuleEnv, spec: CIR.BuiltinTypeSpec) !CIR.Statement.Idx {
    return switch (spec.lookup) {
        .top_level => |name| findTypeDeclaration(gpa, env, name),
        .nested => |nested| findNestedTypeDeclaration(gpa, env, nested.parent, nested.name),
        .qualified => |name| findTypeDeclarationByQualifiedName(env, name),
    };
}

fn expectBuiltinIdent(env: *const ModuleEnv, text: []const u8) base.Ident.Idx {
    return env.common.findIdent(text) orelse unreachable;
}

fn installBuiltinNodeIndices(gpa: Allocator, env: *ModuleEnv, indices: BuiltinIndices) !void {
    inline for (CIR.builtin_type_specs) |spec| {
        const ident = @field(indices, spec.ident_field);
        const stmt = @field(indices, spec.type_field);
        try env.common.setTypeNodeIndexById(gpa, ident, @intCast(@intFromEnum(stmt)));
    }
}

const ModuleDep = struct {
    name: []const u8,
    env: *const ModuleEnv,
};

fn compileModule(
    gpa: Allocator,
    io: std.Io,
    module_name: []const u8,
    source: []const u8,
    source_path: []const u8,
    deps: []const ModuleDep,
    bool_stmt_opt: ?CIR.Statement.Idx,
    try_stmt_opt: ?CIR.Statement.Idx,
    str_stmt_opt: ?CIR.Statement.Idx,
) !*ModuleEnv {
    // This follows the pattern from TestEnv.init() in src/check/test/TestEnv.zig

    // 1. Create ModuleEnv
    var module_env = try gpa.create(ModuleEnv);
    errdefer gpa.destroy(module_env);

    module_env.* = try ModuleEnv.init(gpa, source);
    errdefer module_env.deinit();

    module_env.module_name = module_name;
    try module_env.common.calcLineStarts(gpa);

    // 2. Create common idents (needed for type checking)
    const module_ident = try module_env.insertIdent(base.Ident.for_text(module_name));

    // Use provided bool_stmt, try_stmt, and str_stmt if available, otherwise use undefined
    // For Builtin module, these will be found after canonicalization and updated before type checking
    var builtin_ctx: Check.BuiltinContext = .{
        .module_name = module_ident,
        .bool_stmt = bool_stmt_opt orelse undefined,
        .try_stmt = try_stmt_opt orelse undefined,
        .str_stmt = str_stmt_opt orelse undefined,
        .builtin_module = null,
        .builtin_indices = null,
    };

    // 3. Parse
    const parse_ast = try parse.file(gpa, &module_env.common);
    defer parse_ast.deinit();
    parse_ast.store.emptyScratch();

    // Check for parse errors
    if (parse_ast.hasErrors()) {
        const stderr = stderrWriter(io);
        const palette = reporting.ColorUtils.getPaletteForConfig(reporting.ReportingConfig.initColorTerminal());
        const config = reporting.ReportingConfig.initColorTerminal();

        // Render tokenize diagnostics
        for (parse_ast.tokenize_diagnostics.items) |diag| {
            var report = parse_ast.tokenizeDiagnosticToReport(diag, gpa, source_path) catch |err| {
                std.debug.print("Error creating tokenize diagnostic report: {}\n", .{err});
                continue;
            };
            defer report.deinit();
            reporting.renderReportToTerminal(&report, stderr, palette, config) catch |err| {
                std.debug.print("Error rendering tokenize diagnostic: {}\n", .{err});
            };
        }

        // Render parse diagnostics
        for (parse_ast.parse_diagnostics.items) |diag| {
            var report = parse_ast.parseDiagnosticToReport(&module_env.common, diag, gpa, source_path) catch |err| {
                std.debug.print("Error creating parse diagnostic report: {}\n", .{err});
                continue;
            };
            defer report.deinit();
            reporting.renderReportToTerminal(&report, stderr, palette, config) catch |err| {
                std.debug.print("Error rendering parse diagnostic: {}\n", .{err});
            };
        }

        flushStderr();
        return error.ParseError;
    }

    // 4. Canonicalize
    try module_env.initCIRFields(module_name);
    module_env.module_role = .builtin;

    var can_result = try gpa.create(Can);
    defer {
        can_result.deinit();
        gpa.destroy(can_result);
    }

    const roc_ctx = CoreCtx.os(gpa, gpa, io);
    can_result.* = try Can.initBuiltin(roc_ctx, module_env, parse_ast);

    try can_result.canonicalizeFile();
    try can_result.validateForChecking();

    // Check for canonicalization errors
    const can_diagnostics = try module_env.getDiagnostics();
    defer gpa.free(can_diagnostics);
    if (can_diagnostics.len > 0) {
        const stderr = stderrWriter(io);
        const palette = reporting.ColorUtils.getPaletteForConfig(reporting.ReportingConfig.initColorTerminal());
        const config = reporting.ReportingConfig.initColorTerminal();

        for (can_diagnostics) |diag| {
            var report = module_env.diagnosticToReport(diag, gpa, source_path) catch |err| {
                std.debug.print("Error creating canonicalization diagnostic report: {}\n", .{err});
                continue;
            };
            defer report.deinit();
            reporting.renderReportToTerminal(&report, stderr, palette, config) catch |err| {
                std.debug.print("Error rendering canonicalization diagnostic: {}\n", .{err});
            };
        }

        flushStderr();
        return error.CanonicalizeError;
    }

    // 5.5. Transform low-level operations (must happen before type checking)
    // For the Builtin module, transform annotation-only defs into low-level operations
    if (std.mem.eql(u8, module_name, "Builtin")) {
        try can.BuiltinLowLevel.apply(module_env);

        const builtin_indices = buildBuiltinIndices(gpa, module_env) catch |err| {
            std.debug.print("\n" ++ "=" ** 80 ++ "\n", .{});
            std.debug.print("ERROR: Could not build Builtin type index before type checking\n", .{});
            std.debug.print("=" ** 80 ++ "\n", .{});
            std.debug.print("Builtin type declarations are required for type checking.\n", .{});
            std.debug.print("=" ** 80 ++ "\n", .{});
            return err;
        };

        builtin_ctx.bool_stmt = builtin_indices.bool_type;
        builtin_ctx.try_stmt = builtin_indices.try_type;
        builtin_ctx.str_stmt = builtin_indices.str_type;
        builtin_ctx.builtin_indices = builtin_indices;
    }

    // 6. Type check
    // Build the list of other modules for type checking
    var imported_envs = std.ArrayList(*const ModuleEnv).empty;
    defer imported_envs.deinit(gpa);

    // Add dependencies
    for (deps) |dep| {
        try imported_envs.append(gpa, dep.env);
    }
    module_env.imports.clearResolvedModules();
    try module_env.imports.resolveImportsByExactModuleName(module_env, imported_envs.items);

    var module_envs = std.AutoHashMap(base.Ident.Idx, Can.AutoImportedType).init(gpa);
    defer module_envs.deinit();

    var checker = try Check.init(
        gpa,
        &module_env.types,
        module_env,
        imported_envs.items,
        &module_envs,
        &module_env.store.regions,
        builtin_ctx,
    );
    checker.fixupTypeWriter();
    defer checker.deinit();

    try checker.checkFile();

    // Check for type errors
    if (checker.problems.problems.items.len > 0) {
        const stderr = stderrWriter(io);
        const palette = reporting.ColorUtils.getPaletteForConfig(reporting.ReportingConfig.initColorTerminal());
        const config = reporting.ReportingConfig.initColorTerminal();

        const problem = check.problem;
        var report_builder = try check.report.ReportBuilder.init(
            gpa,
            module_env,
            module_env,
            &checker.snapshots,
            &checker.problems,
            source_path,
            imported_envs.items,
            &checker.import_mapping,
            &checker.regions,
            null,
        );
        defer report_builder.deinit();

        for (0..checker.problems.len()) |i| {
            const problem_idx: problem.Problem.Idx = @enumFromInt(i);
            const prob = checker.problems.get(problem_idx);
            var report = report_builder.build(prob) catch |err| {
                std.debug.print("Error creating type problem report: {}\n", .{err});
                continue;
            };
            defer report.deinit();
            reporting.renderReportToTerminal(&report, stderr, palette, config) catch |err| {
                std.debug.print("Error rendering type problem: {}\n", .{err});
            };
        }

        flushStderr();
        return error.TypeCheckError;
    }

    return module_env;
}

fn serializeModuleEnv(
    gpa: Allocator,
    io: std.Io,
    env: *const ModuleEnv,
    output_path: []const u8,
) !void {
    var arena = collections.SingleThreadArena.init(gpa);
    defer arena.deinit();
    const arena_alloc = arena.allocator();

    // Create output file
    const file = try std.Io.Dir.cwd().createFile(io, output_path, .{ .read = true });
    defer file.close(io);

    // Serialize using CompactWriter
    var writer = collections.CompactWriter.init();
    defer writer.deinit(arena_alloc);

    const serialized = try writer.appendAlloc(arena_alloc, ModuleEnv.Serialized);
    try serialized.serialize(env, arena_alloc, &writer);

    // Write to file
    try writer.writeGather(file, io);
}

/// Find a type declaration by name in a compiled module
/// Returns the statement index of the type declaration
/// For builtin_compiler, types are always in all_statements (not builtin_statements)
/// because we're compiling Builtin.roc itself, not importing from it.
fn findTypeDeclaration(gpa: Allocator, env: *const ModuleEnv, type_name: []const u8) !CIR.Statement.Idx {
    const qualified_name = try std.fmt.allocPrint(gpa, "{s}.{s}", .{ env.module_name, type_name });
    defer gpa.free(qualified_name);

    // Search in all_statements (where Builtin.roc's own types are stored)
    const all_stmts = env.store.sliceStatements(env.all_statements);
    for (all_stmts) |stmt_idx| {
        const stmt = env.store.getStatement(stmt_idx);
        const header_idx = switch (stmt) {
            .s_nominal_decl => |decl| decl.header,
            .s_alias_decl => |alias| alias.header,
            else => continue,
        };
        const header = env.store.getTypeHeader(header_idx);
        const ident_idx = header.name;
        const ident_text = env.getIdentText(ident_idx);
        if (std.mem.eql(u8, ident_text, qualified_name)) {
            return stmt_idx;
        }
    }

    return error.TypeDeclarationNotFound;
}

/// Find a nested type declaration by parent and type name in a compiled module
/// For example, findNestedTypeDeclaration(env, "Num", "U8") finds "Builtin.Num.U8"
/// Returns the statement index of the type declaration
fn findNestedTypeDeclaration(
    gpa: Allocator,
    env: *const ModuleEnv,
    parent_name: []const u8,
    type_name: []const u8,
) !CIR.Statement.Idx {
    const qualified_name = try std.fmt.allocPrint(gpa, "{s}.{s}.{s}", .{ env.module_name, parent_name, type_name });
    defer gpa.free(qualified_name);
    return findTypeDeclarationByQualifiedName(env, qualified_name);
}

fn findTypeDeclarationByQualifiedName(env: *const ModuleEnv, qualified_name: []const u8) !CIR.Statement.Idx {
    // Search in all_statements (where Builtin.roc's own types are stored)
    const all_stmts = env.store.sliceStatements(env.all_statements);
    for (all_stmts) |stmt_idx| {
        const stmt = env.store.getStatement(stmt_idx);
        switch (stmt) {
            .s_nominal_decl => |decl| {
                const header = env.store.getTypeHeader(decl.header);
                const ident_idx = header.name;
                const ident_text = env.getIdentText(ident_idx);
                if (std.mem.eql(u8, ident_text, qualified_name)) {
                    return stmt_idx;
                }
            },
            else => continue,
        }
    }

    return error.TypeDeclarationNotFound;
}

/// Write BuiltinIndices as typed Zig static data for embedding in the compiler.
fn writeBuiltinIndicesZig(
    io: std.Io,
    indices: BuiltinIndices,
    output_path: []const u8,
) !void {
    const file = try std.Io.Dir.cwd().createFile(io, output_path, .{});
    defer file.close(io);

    var write_buffer: [4096]u8 = undefined;
    var writer = file.writer(io, &write_buffer);
    const out = &writer.interface;

    try out.print("pub const builtin_type_registry_hash: u64 = 0x{x};\n", .{CIR.BUILTIN_TYPE_REGISTRY_HASH});
    try out.print("pub const builtin_indices_layout_hash: u64 = 0x{x};\n\n", .{CIR.BUILTIN_INDICES_LAYOUT_HASH});
    try out.writeAll("pub const builtin_indices_raw = .{\n");

    inline for (@typeInfo(BuiltinIndices).@"struct".fields) |field| {
        const value = @field(indices, field.name);
        if (field.type == CIR.Statement.Idx) {
            try out.print("    .{s} = {d},\n", .{ field.name, @intFromEnum(value) });
        } else if (field.type == base.Ident.Idx) {
            try out.print("    .{s} = {d},\n", .{ field.name, @as(u32, @bitCast(value)) });
        } else {
            @compileError("unsupported BuiltinIndices field type: " ++ @typeName(field.type));
        }
    }

    try out.writeAll(
        \\};
        \\
        \\pub fn builtinIndices(comptime CIR: type) CIR.BuiltinIndices {
        \\    return .{
        \\
    );
    inline for (@typeInfo(BuiltinIndices).@"struct".fields) |field| {
        if (field.type == CIR.Statement.Idx) {
            try out.print("        .{s} = @enumFromInt(builtin_indices_raw.{s}),\n", .{ field.name, field.name });
        } else if (field.type == base.Ident.Idx) {
            try out.print("        .{s} = @bitCast(@as(u32, builtin_indices_raw.{s})),\n", .{ field.name, field.name });
        } else {
            @compileError("unsupported BuiltinIndices field type: " ++ @typeName(field.type));
        }
    }
    try out.writeAll(
        \\    };
        \\}
        \\
    );
    try out.flush();
}
