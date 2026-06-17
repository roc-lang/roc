//! Build-time compiler for Roc builtin module (Builtin.roc).
//!
//! This executable runs during `zig build` on the host machine to:
//! 1. Parse and type-check the Builtin.roc module (which contains nested Bool, Try, Str, Dict, Set types)
//! 2. Serialize the resulting ModuleEnv to a binary file
//! 3. Output Builtin.bin and builtin_indices.bin to paths provided by the build system

const std = @import("std");
const base = @import("base");
const parse = @import("parse");
const can = @import("can");
const check = @import("check");
const collections = @import("collections");
const types = @import("types");
const reporting = @import("reporting");

const ModuleEnv = can.ModuleEnv;
const Can = can.Can;
const Check = check.Check;
const Allocator = std.mem.Allocator;
const CoreCtx = can.CoreCtx;
const CIR = can.CIR;

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

fn readFileAllocPath(gpa: Allocator, io: std.Io, path: []const u8) ![]u8 {
    if (std.fs.path.isAbsolute(path)) {
        const root_dir = try std.Io.Dir.openDirAbsolute(io, "/", .{});
        return try root_dir.readFileAlloc(io, path, gpa, .limited(max_builtin_bytes));
    }
    return try std.Io.Dir.cwd().readFileAlloc(io, path, gpa, .limited(max_builtin_bytes));
}

/// Build-time compiler that compiles builtin .roc sources into serialized ModuleEnvs.
/// This runs during `zig build` on the host machine to generate .bin files
/// that get embedded into the final roc executable.
///
/// The build system passes:
/// 1. the absolute path to Builtin.roc for cache tracking
/// 2. the output path for Builtin.bin
/// 3. the output path for builtin_indices.bin
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
    const builtin_indices_path = if (args.len >= 4) args[3] else "zig-out/builtins/builtin_indices.bin";
    const builtin_checked_bin_path = if (args.len >= 5) args[4] else "zig-out/builtins/Builtin.checked.bin";

    // Read the Builtin.roc source file at runtime
    // NOTE: We must free this source manually; CommonEnv.deinit() does not free the source.
    const builtin_roc_source = try readFileAllocPath(gpa, io, builtin_src_path);

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
    if (std.fs.path.dirname(builtin_indices_path)) |dir| {
        try std.Io.Dir.cwd().createDirPath(io, dir);
    }
    if (std.fs.path.dirname(builtin_checked_bin_path)) |dir| {
        try std.Io.Dir.cwd().createDirPath(io, dir);
    }

    // Serialize the single Builtin module
    try serializeModuleEnv(gpa, io, builtin_env, builtin_bin_path);

    // Validate that BuiltinIndices contains all type declarations under Builtin
    // This ensures BuiltinIndices stays in sync with the actual Builtin module content
    try validateBuiltinIndicesCompleteness(gpa, builtin_env, builtin_indices);

    try serializeBuiltinIndices(io, builtin_indices, builtin_indices_path);

    // Assemble and cache the pre-finalize CheckedArtifact. The expensive,
    // deterministic publish work runs here at build time; `roc` deserializes the
    // result and runs only compile-time finalization at startup.
    try serializeCheckedArtifact(gpa, io, builtin_env, builtin_checked_bin_path);
}

/// Build the pre-finalize checked artifact for the Builtin module and write it
/// to `output_path` using the generic artifact serializer.
fn serializeCheckedArtifact(
    gpa: Allocator,
    io: std.Io,
    env: *ModuleEnv,
    output_path: []const u8,
) !void {
    var typed_modules = try check.TypedCIR.Modules.init(gpa, &.{
        .{ .precompiled = env },
    });
    defer typed_modules.deinit();

    var artifact = try check.CheckedArtifact.assembleUnfinalizedArtifact(
        gpa,
        &typed_modules,
        0,
        .{
            .module_env_storage = .{ .checked_source = env },
            // Never invoked: assembly does not run finalization.
            .compile_time_finalizer = .{ .finalize = noopFinalize },
        },
    );
    // The module env is owned by `main`; do not free it through the artifact.
    defer artifact.deinitRetainingModuleEnv(gpa);

    const bytes = try artifact.serialize(gpa);
    defer gpa.free(bytes);

    const file = try std.Io.Dir.cwd().createFile(io, output_path, .{});
    defer file.close(io);
    try file.writePositionalAll(io, bytes, 0);
}

fn noopFinalize(
    _: ?*anyopaque,
    _: Allocator,
    _: *check.CheckedArtifact.CheckedModuleArtifact,
    _: []const check.CheckedArtifact.PublishImportArtifact,
    _: []const check.CheckedArtifact.ImportedModuleView,
    _: []const check.CheckedArtifact.ImportedModuleView,
    _: ?*check.problem.Store,
) anyerror!void {}

fn buildBuiltinIndices(gpa: Allocator, env: *const ModuleEnv) !BuiltinIndices {
    const bool_type_idx = try findTypeDeclaration(gpa, env, "Bool");
    const try_type_idx = try findTypeDeclaration(gpa, env, "Try");
    const dict_type_idx = try findTypeDeclaration(gpa, env, "Dict");
    const set_type_idx = try findTypeDeclaration(gpa, env, "Set");
    const str_type_idx = try findTypeDeclaration(gpa, env, "Str");
    const hasher_type_idx = try findTypeDeclaration(gpa, env, "Hasher");
    const iter_type_idx = try findTypeDeclaration(gpa, env, "Iter");
    const stream_type_idx = try findTypeDeclaration(gpa, env, "Stream");
    const list_type_idx = try findTypeDeclaration(gpa, env, "List");
    const box_type_idx = try findTypeDeclaration(gpa, env, "Box");

    const utf8_problem_type_idx = try findNestedTypeDeclaration(gpa, env, "Str", "Utf8Problem");

    const u8_type_idx = try findNestedTypeDeclaration(gpa, env, "Num", "U8");
    const i8_type_idx = try findNestedTypeDeclaration(gpa, env, "Num", "I8");
    const u16_type_idx = try findNestedTypeDeclaration(gpa, env, "Num", "U16");
    const i16_type_idx = try findNestedTypeDeclaration(gpa, env, "Num", "I16");
    const u32_type_idx = try findNestedTypeDeclaration(gpa, env, "Num", "U32");
    const i32_type_idx = try findNestedTypeDeclaration(gpa, env, "Num", "I32");
    const u64_type_idx = try findNestedTypeDeclaration(gpa, env, "Num", "U64");
    const i64_type_idx = try findNestedTypeDeclaration(gpa, env, "Num", "I64");
    const u128_type_idx = try findNestedTypeDeclaration(gpa, env, "Num", "U128");
    const i128_type_idx = try findNestedTypeDeclaration(gpa, env, "Num", "I128");
    const dec_type_idx = try findNestedTypeDeclaration(gpa, env, "Num", "Dec");
    const f32_type_idx = try findNestedTypeDeclaration(gpa, env, "Num", "F32");
    const f64_type_idx = try findNestedTypeDeclaration(gpa, env, "Num", "F64");
    const numeral_type_idx = try findNestedTypeDeclaration(gpa, env, "Num", "Numeral");

    return .{
        .bool_type = bool_type_idx,
        .try_type = try_type_idx,
        .dict_type = dict_type_idx,
        .set_type = set_type_idx,
        .str_type = str_type_idx,
        .hasher_type = hasher_type_idx,
        .iter_type = iter_type_idx,
        .stream_type = stream_type_idx,
        .list_type = list_type_idx,
        .box_type = box_type_idx,
        .utf8_problem_type = utf8_problem_type_idx,
        .u8_type = u8_type_idx,
        .i8_type = i8_type_idx,
        .u16_type = u16_type_idx,
        .i16_type = i16_type_idx,
        .u32_type = u32_type_idx,
        .i32_type = i32_type_idx,
        .u64_type = u64_type_idx,
        .i64_type = i64_type_idx,
        .u128_type = u128_type_idx,
        .i128_type = i128_type_idx,
        .dec_type = dec_type_idx,
        .f32_type = f32_type_idx,
        .f64_type = f64_type_idx,
        .numeral_type = numeral_type_idx,
        .bool_ident = expectBuiltinIdent(env, "Builtin.Bool"),
        .try_ident = expectBuiltinIdent(env, "Builtin.Try"),
        .dict_ident = expectBuiltinIdent(env, "Builtin.Dict"),
        .set_ident = expectBuiltinIdent(env, "Builtin.Set"),
        .str_ident = expectBuiltinIdent(env, "Builtin.Str"),
        .hasher_ident = expectBuiltinIdent(env, "Builtin.Hasher"),
        .iter_ident = expectBuiltinIdent(env, "Builtin.Iter"),
        .stream_ident = expectBuiltinIdent(env, "Builtin.Stream"),
        .list_ident = expectBuiltinIdent(env, "Builtin.List"),
        .box_ident = expectBuiltinIdent(env, "Builtin.Box"),
        .utf8_problem_ident = expectBuiltinIdent(env, "Builtin.Str.Utf8Problem"),
        .u8_ident = expectBuiltinIdent(env, "Builtin.Num.U8"),
        .i8_ident = expectBuiltinIdent(env, "Builtin.Num.I8"),
        .u16_ident = expectBuiltinIdent(env, "Builtin.Num.U16"),
        .i16_ident = expectBuiltinIdent(env, "Builtin.Num.I16"),
        .u32_ident = expectBuiltinIdent(env, "Builtin.Num.U32"),
        .i32_ident = expectBuiltinIdent(env, "Builtin.Num.I32"),
        .u64_ident = expectBuiltinIdent(env, "Builtin.Num.U64"),
        .i64_ident = expectBuiltinIdent(env, "Builtin.Num.I64"),
        .u128_ident = expectBuiltinIdent(env, "Builtin.Num.U128"),
        .i128_ident = expectBuiltinIdent(env, "Builtin.Num.I128"),
        .dec_ident = expectBuiltinIdent(env, "Builtin.Num.Dec"),
        .f32_ident = expectBuiltinIdent(env, "Builtin.Num.F32"),
        .f64_ident = expectBuiltinIdent(env, "Builtin.Num.F64"),
        .numeral_ident = expectBuiltinIdent(env, "Builtin.Num.Numeral"),
        .ok_ident = expectBuiltinIdent(env, "Ok"),
        .err_ident = expectBuiltinIdent(env, "Err"),
    };
}

fn expectBuiltinIdent(env: *const ModuleEnv, text: []const u8) base.Ident.Idx {
    return env.common.findIdent(text) orelse unreachable;
}

fn installBuiltinNodeIndices(gpa: Allocator, env: *ModuleEnv, indices: BuiltinIndices) !void {
    try env.common.setTypeNodeIndexById(gpa, indices.bool_ident, @intCast(@intFromEnum(indices.bool_type)));
    try env.common.setTypeNodeIndexById(gpa, indices.try_ident, @intCast(@intFromEnum(indices.try_type)));
    try env.common.setTypeNodeIndexById(gpa, indices.dict_ident, @intCast(@intFromEnum(indices.dict_type)));
    try env.common.setTypeNodeIndexById(gpa, indices.set_ident, @intCast(@intFromEnum(indices.set_type)));
    try env.common.setTypeNodeIndexById(gpa, indices.str_ident, @intCast(@intFromEnum(indices.str_type)));
    try env.common.setTypeNodeIndexById(gpa, indices.hasher_ident, @intCast(@intFromEnum(indices.hasher_type)));
    try env.common.setTypeNodeIndexById(gpa, indices.iter_ident, @intCast(@intFromEnum(indices.iter_type)));
    try env.common.setTypeNodeIndexById(gpa, indices.stream_ident, @intCast(@intFromEnum(indices.stream_type)));
    try env.common.setTypeNodeIndexById(gpa, indices.list_ident, @intCast(@intFromEnum(indices.list_type)));
    try env.common.setTypeNodeIndexById(gpa, indices.box_ident, @intCast(@intFromEnum(indices.box_type)));
    try env.common.setTypeNodeIndexById(gpa, indices.utf8_problem_ident, @intCast(@intFromEnum(indices.utf8_problem_type)));
    try env.common.setTypeNodeIndexById(gpa, indices.u8_ident, @intCast(@intFromEnum(indices.u8_type)));
    try env.common.setTypeNodeIndexById(gpa, indices.i8_ident, @intCast(@intFromEnum(indices.i8_type)));
    try env.common.setTypeNodeIndexById(gpa, indices.u16_ident, @intCast(@intFromEnum(indices.u16_type)));
    try env.common.setTypeNodeIndexById(gpa, indices.i16_ident, @intCast(@intFromEnum(indices.i16_type)));
    try env.common.setTypeNodeIndexById(gpa, indices.u32_ident, @intCast(@intFromEnum(indices.u32_type)));
    try env.common.setTypeNodeIndexById(gpa, indices.i32_ident, @intCast(@intFromEnum(indices.i32_type)));
    try env.common.setTypeNodeIndexById(gpa, indices.u64_ident, @intCast(@intFromEnum(indices.u64_type)));
    try env.common.setTypeNodeIndexById(gpa, indices.i64_ident, @intCast(@intFromEnum(indices.i64_type)));
    try env.common.setTypeNodeIndexById(gpa, indices.u128_ident, @intCast(@intFromEnum(indices.u128_type)));
    try env.common.setTypeNodeIndexById(gpa, indices.i128_ident, @intCast(@intFromEnum(indices.i128_type)));
    try env.common.setTypeNodeIndexById(gpa, indices.dec_ident, @intCast(@intFromEnum(indices.dec_type)));
    try env.common.setTypeNodeIndexById(gpa, indices.f32_ident, @intCast(@intFromEnum(indices.f32_type)));
    try env.common.setTypeNodeIndexById(gpa, indices.f64_ident, @intCast(@intFromEnum(indices.f64_type)));
    try env.common.setTypeNodeIndexById(gpa, indices.numeral_ident, @intCast(@intFromEnum(indices.numeral_type)));
}

/// Validates that BuiltinIndices contains all nominal type declarations in the Builtin module.
/// Iterates through all statements and ensures every s_nominal_decl is present in BuiltinIndices,
/// with the exception of "Num" which is a container type, not an auto-imported type.
fn validateBuiltinIndicesCompleteness(gpa: Allocator, env: *const ModuleEnv, indices: BuiltinIndices) !void {
    // Collect all statement indices from BuiltinIndices using reflection
    // Only check Statement.Idx fields (skip Ident.Idx fields)
    var indexed_stmts = std.AutoHashMap(CIR.Statement.Idx, void).init(gpa);
    defer indexed_stmts.deinit();

    const fields = @typeInfo(BuiltinIndices).@"struct".fields;
    inline for (fields) |field| {
        if (field.type == CIR.Statement.Idx) {
            const stmt_idx = @field(indices, field.name);
            try indexed_stmts.put(stmt_idx, {});
        }
    }

    // Check all nominal type declarations in the Builtin module
    const all_stmts = env.store.sliceStatements(env.all_statements);
    for (all_stmts) |stmt_idx| {
        const stmt = env.store.getStatement(stmt_idx);
        switch (stmt) {
            .s_nominal_decl => |decl| {
                const header = env.store.getTypeHeader(decl.header);
                const ident_text = env.getIdentText(header.name);

                // Skip container types that are not auto-imported types
                if (std.mem.eql(u8, ident_text, "Builtin") or
                    std.mem.eql(u8, ident_text, "Builtin.Num"))
                {
                    continue;
                }

                // Every other nominal type should be in BuiltinIndices
                if (!indexed_stmts.contains(stmt_idx)) {
                    std.debug.print("ERROR: Type '{s}' (stmt_idx={d}) is not in BuiltinIndices!\n", .{
                        ident_text,
                        @intFromEnum(stmt_idx),
                    });
                    std.debug.print("Add this type to BuiltinIndices in CIR.zig and builtin_compiler/main.zig\n", .{});
                    return error.BuiltinIndicesIncomplete;
                }
            },
            else => continue,
        }
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

/// Serialize BuiltinIndices to a binary file
fn serializeBuiltinIndices(
    io: std.Io,
    indices: BuiltinIndices,
    output_path: []const u8,
) !void {
    // Create output file
    const file = try std.Io.Dir.cwd().createFile(io, output_path, .{});
    defer file.close(io);

    // Write the struct directly as binary data
    // BuiltinIndices stores compact enum indices, so we can write it directly.
    try file.writePositionalAll(io, std.mem.asBytes(&indices), 0);
}
