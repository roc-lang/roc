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

    // Find nested type declarations in Builtin module
    // These are nested inside Builtin's record extension (Builtin := [].{...})
    const bool_type_idx = try findTypeDeclaration(builtin_env, "Bool");
    const try_type_idx = try findTypeDeclaration(builtin_env, "Try");
    const dict_type_idx = try findTypeDeclaration(builtin_env, "Dict");
    const set_type_idx = try findTypeDeclaration(builtin_env, "Set");
    const str_type_idx = try findTypeDeclaration(builtin_env, "Str");
    const iter_type_idx = try findTypeDeclaration(builtin_env, "Iter");
    const list_type_idx = try findTypeDeclaration(builtin_env, "List");
    const box_type_idx = try findTypeDeclaration(builtin_env, "Box");

    // Find Utf8Problem nested inside Str (e.g., Builtin.Str.Utf8Problem)
    const utf8_problem_type_idx = try findNestedTypeDeclaration(builtin_env, "Str", "Utf8Problem");

    // Find numeric types nested inside Num (e.g., Builtin.Num.U8)
    const u8_type_idx = try findNestedTypeDeclaration(builtin_env, "Num", "U8");
    const i8_type_idx = try findNestedTypeDeclaration(builtin_env, "Num", "I8");
    const u16_type_idx = try findNestedTypeDeclaration(builtin_env, "Num", "U16");
    const i16_type_idx = try findNestedTypeDeclaration(builtin_env, "Num", "I16");
    const u32_type_idx = try findNestedTypeDeclaration(builtin_env, "Num", "U32");
    const i32_type_idx = try findNestedTypeDeclaration(builtin_env, "Num", "I32");
    const u64_type_idx = try findNestedTypeDeclaration(builtin_env, "Num", "U64");
    const i64_type_idx = try findNestedTypeDeclaration(builtin_env, "Num", "I64");
    const u128_type_idx = try findNestedTypeDeclaration(builtin_env, "Num", "U128");
    const i128_type_idx = try findNestedTypeDeclaration(builtin_env, "Num", "I128");
    const dec_type_idx = try findNestedTypeDeclaration(builtin_env, "Num", "Dec");
    const f32_type_idx = try findNestedTypeDeclaration(builtin_env, "Num", "F32");
    const f64_type_idx = try findNestedTypeDeclaration(builtin_env, "Num", "F64");
    const numeral_type_idx = try findNestedTypeDeclaration(builtin_env, "Num", "Numeral");

    // Look up idents for each type
    // All types use fully-qualified names for consistent member lookup
    // Top-level types: "Builtin.Bool", "Builtin.Str", etc.
    // Nested types under Num: "Builtin.Num.U8", etc.
    const bool_ident = builtin_env.common.findIdent("Builtin.Bool") orelse unreachable;
    const try_ident = builtin_env.common.findIdent("Builtin.Try") orelse unreachable;
    const dict_ident = builtin_env.common.findIdent("Builtin.Dict") orelse unreachable;
    const set_ident = builtin_env.common.findIdent("Builtin.Set") orelse unreachable;
    const str_ident = builtin_env.common.findIdent("Builtin.Str") orelse unreachable;
    const iter_ident = builtin_env.common.findIdent("Builtin.Iter") orelse unreachable;
    const list_ident = builtin_env.common.findIdent("Builtin.List") orelse unreachable;
    const box_ident = builtin_env.common.findIdent("Builtin.Box") orelse unreachable;
    const utf8_problem_ident = builtin_env.common.findIdent("Builtin.Str.Utf8Problem") orelse unreachable;
    const u8_ident = builtin_env.common.findIdent("Builtin.Num.U8") orelse unreachable;
    const i8_ident = builtin_env.common.findIdent("Builtin.Num.I8") orelse unreachable;
    const u16_ident = builtin_env.common.findIdent("Builtin.Num.U16") orelse unreachable;
    const i16_ident = builtin_env.common.findIdent("Builtin.Num.I16") orelse unreachable;
    const u32_ident = builtin_env.common.findIdent("Builtin.Num.U32") orelse unreachable;
    const i32_ident = builtin_env.common.findIdent("Builtin.Num.I32") orelse unreachable;
    const u64_ident = builtin_env.common.findIdent("Builtin.Num.U64") orelse unreachable;
    const i64_ident = builtin_env.common.findIdent("Builtin.Num.I64") orelse unreachable;
    const u128_ident = builtin_env.common.findIdent("Builtin.Num.U128") orelse unreachable;
    const i128_ident = builtin_env.common.findIdent("Builtin.Num.I128") orelse unreachable;
    const dec_ident = builtin_env.common.findIdent("Builtin.Num.Dec") orelse unreachable;
    const f32_ident = builtin_env.common.findIdent("Builtin.Num.F32") orelse unreachable;
    const f64_ident = builtin_env.common.findIdent("Builtin.Num.F64") orelse unreachable;
    const numeral_ident = builtin_env.common.findIdent("Builtin.Num.Numeral") orelse unreachable;
    // Tag idents for Try type (Ok and Err)
    const ok_ident = builtin_env.common.findIdent("Ok") orelse unreachable;
    const err_ident = builtin_env.common.findIdent("Err") orelse unreachable;

    // Expose the types so they can be found by getExposedNodeIndexById (used for auto-imports)
    // Note: These types are already in exposed_items from canonicalization, we just set their node indices
    try builtin_env.common.setNodeIndexById(gpa, bool_ident, @intCast(@intFromEnum(bool_type_idx)));
    try builtin_env.common.setNodeIndexById(gpa, try_ident, @intCast(@intFromEnum(try_type_idx)));
    try builtin_env.common.setNodeIndexById(gpa, dict_ident, @intCast(@intFromEnum(dict_type_idx)));
    try builtin_env.common.setNodeIndexById(gpa, set_ident, @intCast(@intFromEnum(set_type_idx)));
    try builtin_env.common.setNodeIndexById(gpa, str_ident, @intCast(@intFromEnum(str_type_idx)));
    try builtin_env.common.setNodeIndexById(gpa, iter_ident, @intCast(@intFromEnum(iter_type_idx)));
    try builtin_env.common.setNodeIndexById(gpa, list_ident, @intCast(@intFromEnum(list_type_idx)));

    try builtin_env.common.setNodeIndexById(gpa, u8_ident, @intCast(@intFromEnum(u8_type_idx)));
    try builtin_env.common.setNodeIndexById(gpa, i8_ident, @intCast(@intFromEnum(i8_type_idx)));
    try builtin_env.common.setNodeIndexById(gpa, u16_ident, @intCast(@intFromEnum(u16_type_idx)));
    try builtin_env.common.setNodeIndexById(gpa, i16_ident, @intCast(@intFromEnum(i16_type_idx)));
    try builtin_env.common.setNodeIndexById(gpa, u32_ident, @intCast(@intFromEnum(u32_type_idx)));
    try builtin_env.common.setNodeIndexById(gpa, i32_ident, @intCast(@intFromEnum(i32_type_idx)));
    try builtin_env.common.setNodeIndexById(gpa, u64_ident, @intCast(@intFromEnum(u64_type_idx)));
    try builtin_env.common.setNodeIndexById(gpa, i64_ident, @intCast(@intFromEnum(i64_type_idx)));
    try builtin_env.common.setNodeIndexById(gpa, u128_ident, @intCast(@intFromEnum(u128_type_idx)));
    try builtin_env.common.setNodeIndexById(gpa, i128_ident, @intCast(@intFromEnum(i128_type_idx)));
    try builtin_env.common.setNodeIndexById(gpa, dec_ident, @intCast(@intFromEnum(dec_type_idx)));
    try builtin_env.common.setNodeIndexById(gpa, f32_ident, @intCast(@intFromEnum(f32_type_idx)));
    try builtin_env.common.setNodeIndexById(gpa, f64_ident, @intCast(@intFromEnum(f64_type_idx)));
    try builtin_env.common.setNodeIndexById(gpa, numeral_ident, @intCast(@intFromEnum(numeral_type_idx)));

    // Create output directories when needed.
    if (std.fs.path.dirname(builtin_bin_path)) |dir| {
        try std.Io.Dir.cwd().createDirPath(io, dir);
    }
    if (std.fs.path.dirname(builtin_indices_path)) |dir| {
        try std.Io.Dir.cwd().createDirPath(io, dir);
    }

    // Serialize the single Builtin module
    try serializeModuleEnv(gpa, io, builtin_env, builtin_bin_path);

    // Create and serialize builtin indices
    const builtin_indices = BuiltinIndices{
        // Statement indices
        .bool_type = bool_type_idx,
        .try_type = try_type_idx,
        .dict_type = dict_type_idx,
        .set_type = set_type_idx,
        .str_type = str_type_idx,
        .iter_type = iter_type_idx,
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
        .bool_ident = bool_ident,
        .try_ident = try_ident,
        .dict_ident = dict_ident,
        .set_ident = set_ident,
        .str_ident = str_ident,
        .iter_ident = iter_ident,
        .list_ident = list_ident,
        .box_ident = box_ident,
        .utf8_problem_ident = utf8_problem_ident,
        .u8_ident = u8_ident,
        .i8_ident = i8_ident,
        .u16_ident = u16_ident,
        .i16_ident = i16_ident,
        .u32_ident = u32_ident,
        .i32_ident = i32_ident,
        .u64_ident = u64_ident,
        .i64_ident = i64_ident,
        .u128_ident = u128_ident,
        .i128_ident = i128_ident,
        .dec_ident = dec_ident,
        .f32_ident = f32_ident,
        .f64_ident = f64_ident,
        .numeral_ident = numeral_ident,
        .ok_ident = ok_ident,
        .err_ident = err_ident,
    };

    // Validate that BuiltinIndices contains all type declarations under Builtin
    // This ensures BuiltinIndices stays in sync with the actual Builtin module content
    try validateBuiltinIndicesCompleteness(builtin_env, builtin_indices);

    try serializeBuiltinIndices(io, builtin_indices, builtin_indices_path);
}

/// Validates that BuiltinIndices contains all nominal type declarations in the Builtin module.
/// Iterates through all statements and ensures every s_nominal_decl is present in BuiltinIndices,
/// with the exception of "Num" which is a container type, not an auto-imported type.
fn validateBuiltinIndicesCompleteness(env: *const ModuleEnv, indices: BuiltinIndices) !void {
    // Collect all statement indices from BuiltinIndices using reflection
    // Only check Statement.Idx fields (skip Ident.Idx fields)
    var indexed_stmts = std.AutoHashMap(CIR.Statement.Idx, void).init(std.heap.page_allocator);
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

    var arena = std.heap.ArenaAllocator.init(gpa);
    defer arena.deinit();

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
    const parse_ast = try parse.parse(gpa, &module_env.common);
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

        // Find Bool, Try, and Str statements before type checking
        // When compiling Builtin, bool_stmt, try_stmt, and str_stmt are initially undefined,
        // but they must be set before type checking begins
        const found_bool_stmt = findTypeDeclaration(module_env, "Bool") catch {
            std.debug.print("\n" ++ "=" ** 80 ++ "\n", .{});
            std.debug.print("ERROR: Could not find Bool type in Builtin module\n", .{});
            std.debug.print("=" ** 80 ++ "\n", .{});
            std.debug.print("The Bool type declaration is required for type checking.\n", .{});
            std.debug.print("=" ** 80 ++ "\n", .{});
            return error.TypeDeclarationNotFound;
        };
        const found_try_stmt = findTypeDeclaration(module_env, "Try") catch {
            std.debug.print("\n" ++ "=" ** 80 ++ "\n", .{});
            std.debug.print("ERROR: Could not find Try type in Builtin module\n", .{});
            std.debug.print("=" ** 80 ++ "\n", .{});
            std.debug.print("The Try type declaration is required for type checking.\n", .{});
            std.debug.print("=" ** 80 ++ "\n", .{});
            return error.TypeDeclarationNotFound;
        };
        const found_str_stmt = findTypeDeclaration(module_env, "Str") catch {
            std.debug.print("\n" ++ "=" ** 80 ++ "\n", .{});
            std.debug.print("ERROR: Could not find Str type in Builtin module\n", .{});
            std.debug.print("=" ** 80 ++ "\n", .{});
            std.debug.print("The Str type declaration is required for type checking.\n", .{});
            std.debug.print("=" ** 80 ++ "\n", .{});
            return error.TypeDeclarationNotFound;
        };

        // Update builtin_ctx with the found statement indices
        builtin_ctx.bool_stmt = found_bool_stmt;
        builtin_ctx.try_stmt = found_try_stmt;
        builtin_ctx.str_stmt = found_str_stmt;
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
    module_env.imports.resolveImportsByExactModuleName(module_env, imported_envs.items);

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
    var arena = std.heap.ArenaAllocator.init(gpa);
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
fn findTypeDeclaration(env: *const ModuleEnv, type_name: []const u8) !CIR.Statement.Idx {
    // Construct the qualified name (e.g., "Builtin.Bool")
    // Types in nested declarations are stored with their full qualified names
    var qualified_name_buf: [256]u8 = undefined;
    const qualified_name = try std.fmt.bufPrint(&qualified_name_buf, "{s}.{s}", .{ env.module_name, type_name });

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
fn findNestedTypeDeclaration(env: *const ModuleEnv, parent_name: []const u8, type_name: []const u8) !CIR.Statement.Idx {
    // Construct the qualified name (e.g., "Builtin.Num.U8")
    var qualified_name_buf: [256]u8 = undefined;
    const qualified_name = try std.fmt.bufPrint(&qualified_name_buf, "{s}.{s}.{s}", .{ env.module_name, parent_name, type_name });

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
    // This is a simple struct with two u32 fields, so we can write it directly
    try file.writePositionalAll(io, std.mem.asBytes(&indices), 0);
}
