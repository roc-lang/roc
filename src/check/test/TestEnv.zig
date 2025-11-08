//! Test environment for canonicalization testing, providing utilities to parse, canonicalize, and inspect Roc expressions.

const std = @import("std");
const base = @import("base");
const types = @import("types");
const parse = @import("parse");
const CIR = @import("can").CIR;
const Can = @import("can").Can;
const ModuleEnv = @import("can").ModuleEnv;
const collections = @import("collections");

const Check = @import("../Check.zig");
const problem_mod = @import("../problem.zig");

const CommonEnv = base.CommonEnv;
const testing = std.testing;

const compiled_builtins = @import("compiled_builtins");

/// Wrapper for a loaded compiled module that tracks the buffer
const LoadedModule = struct {
    env: *ModuleEnv,
    buffer: []align(collections.CompactWriter.SERIALIZATION_ALIGNMENT.toByteUnits()) u8,
    gpa: std.mem.Allocator,

    fn deinit(self: *LoadedModule) void {
        // Only free the hashmap that was allocated during deserialization
        // Most other data (like the SafeList contents) points into the buffer
        self.env.imports.map.deinit(self.gpa);

        // Free the buffer (the env points into this buffer for most data)
        self.gpa.free(self.buffer);
        // Free the env struct itself
        self.gpa.destroy(self.env);
    }
};

/// Deserialize BuiltinIndices from the binary data generated at build time
fn deserializeBuiltinIndices(gpa: std.mem.Allocator, bin_data: []const u8) !CIR.BuiltinIndices {
    // Copy to properly aligned memory
    const aligned_buffer = try gpa.alignedAlloc(u8, @enumFromInt(@alignOf(CIR.BuiltinIndices)), bin_data.len);
    defer gpa.free(aligned_buffer);
    @memcpy(aligned_buffer, bin_data);

    const indices_ptr = @as(*const CIR.BuiltinIndices, @ptrCast(aligned_buffer.ptr));
    return indices_ptr.*;
}

/// Load a compiled ModuleEnv from embedded binary data
fn loadCompiledModule(gpa: std.mem.Allocator, bin_data: []const u8, module_name: []const u8, source: []const u8) !LoadedModule {
    // Copy the embedded data to properly aligned memory
    // CompactWriter requires specific alignment for serialization
    const CompactWriter = collections.CompactWriter;
    const buffer = try gpa.alignedAlloc(u8, CompactWriter.SERIALIZATION_ALIGNMENT, bin_data.len);
    @memcpy(buffer, bin_data);

    // Cast to the serialized structure
    const serialized_ptr = @as(
        *ModuleEnv.Serialized,
        @ptrCast(@alignCast(buffer.ptr)),
    );

    const env = try gpa.create(ModuleEnv);
    errdefer gpa.destroy(env);

    // Deserialize
    const base_ptr = @intFromPtr(buffer.ptr);

    // Deserialize common env first so we can look up identifiers
    const common = serialized_ptr.common.deserialize(@as(i64, @intCast(base_ptr)), source).*;

    env.* = ModuleEnv{
        .gpa = gpa,
        .common = common,
        .types = serialized_ptr.types.deserialize(@as(i64, @intCast(base_ptr)), gpa).*, // Pass gpa to types deserialize
        .module_kind = serialized_ptr.module_kind,
        .all_defs = serialized_ptr.all_defs,
        .all_statements = serialized_ptr.all_statements,
        .exports = serialized_ptr.exports,
        .builtin_statements = serialized_ptr.builtin_statements,
        .external_decls = serialized_ptr.external_decls.deserialize(@as(i64, @intCast(base_ptr))).*,
        .imports = (try serialized_ptr.imports.deserialize(@as(i64, @intCast(base_ptr)), gpa)).*,
        .module_name = module_name,
        .module_name_idx = undefined, // Not used for deserialized modules (only needed during fresh canonicalization)
        .diagnostics = serialized_ptr.diagnostics,
        .store = serialized_ptr.store.deserialize(@as(i64, @intCast(base_ptr)), gpa).*,
        .evaluation_order = null,
        .from_int_digits_ident = common.findIdent(base.Ident.FROM_INT_DIGITS_METHOD_NAME) orelse unreachable,
        .from_dec_digits_ident = common.findIdent(base.Ident.FROM_DEC_DIGITS_METHOD_NAME) orelse unreachable,
        .try_ident = common.findIdent("Try") orelse unreachable,
        .out_of_range_ident = common.findIdent("OutOfRange") orelse unreachable,
        .builtin_module_ident = common.findIdent("Builtin") orelse unreachable,
    };

    return LoadedModule{
        .env = env,
        .buffer = buffer,
        .gpa = gpa,
    };
}

gpa: std.mem.Allocator,
module_env: *ModuleEnv,
parse_ast: *parse.AST,
can: *Can,
checker: Check,
type_writer: types.TypeWriter,

module_envs: std.AutoHashMap(base.Ident.Idx, Can.AutoImportedType),

// Loaded Builtin module (loaded per test, cleaned up in deinit)
builtin_module: LoadedModule,
// Whether this TestEnv owns the builtin_module and should deinit it
owns_builtin_module: bool,

/// Test environment for canonicalization testing, providing a convenient wrapper around ModuleEnv, AST, and Can.
const TestEnv = @This();

/// Initialize where the provided source is an entire file
///
/// Accepts another module that should already be can'd and type checked, and will
/// add that module as an import to this module.
/// IMPORTANT: This reuses the Builtin module from the imported module to ensure
/// type variables from auto-imported types (Bool, Result, Str) are shared across modules.
pub fn initWithImport(module_name: []const u8, source: []const u8, other_module_name: []const u8, other_test_env: *const TestEnv) !TestEnv {
    const gpa = std.testing.allocator;

    // Allocate our ModuleEnv, AST, and Can on the heap
    // so we can keep them around for testing purposes...
    // this is an unusual setup, but helps us with testing
    const module_env: *ModuleEnv = try gpa.create(ModuleEnv);
    errdefer gpa.destroy(module_env);

    const parse_ast = try gpa.create(parse.AST);
    errdefer gpa.destroy(parse_ast);

    const can = try gpa.create(Can);
    errdefer gpa.destroy(can);

    var module_envs = std.AutoHashMap(base.Ident.Idx, Can.AutoImportedType).init(gpa);

    std.debug.assert(!std.mem.eql(u8, module_name, other_module_name));

    // Reuse the Builtin module from the imported module
    // This ensures type variables for auto-imported types (Bool, Result, Str) are shared
    const builtin_indices = try deserializeBuiltinIndices(gpa, compiled_builtins.builtin_indices_bin);
    const builtin_env = other_test_env.builtin_module.env;

    // Initialize the module_env so we can use its ident store
    module_env.* = try ModuleEnv.init(gpa, source);
    errdefer module_env.deinit();

    module_env.common.source = source;
    module_env.module_name = module_name;
    module_env.module_name_idx = try module_env.insertIdent(base.Ident.for_text(module_name));
    try module_env.common.calcLineStarts(gpa);

    // Put the other module in the env map using module_env's ident store
    const other_module_ident = try module_env.insertIdent(base.Ident.for_text(other_module_name));

    // For type modules, look up the exposed type statement index
    // The type name matches the module name for type modules
    const statement_idx = blk: {
        if (other_test_env.module_env.module_kind == .type_module) {
            // Type modules expose their main type under the module name
            const type_ident = other_test_env.module_env.common.findIdent(other_module_name);
            if (type_ident) |ident| {
                if (other_test_env.module_env.getExposedNodeIndexById(ident)) |node_idx| {
                    // The node index IS the statement index for type declarations
                    break :blk @as(CIR.Statement.Idx, @enumFromInt(node_idx));
                }
            }
        }
        break :blk null;
    };

    try module_envs.put(other_module_ident, .{
        .env = other_test_env.module_env,
        .statement_idx = statement_idx,
    });

    // Populate module_envs with Bool, Result, Dict, Set using shared function
    // This ensures production and tests use identical logic
    try Can.populateModuleEnvs(
        &module_envs,
        module_env,
        builtin_env,
        builtin_indices,
    );

    // Parse the AST
    parse_ast.* = try parse.parse(&module_env.common, gpa);
    errdefer parse_ast.deinit(gpa);
    parse_ast.store.emptyScratch();

    // Canonicalize
    try module_env.initCIRFields(gpa, module_name);

    can.* = try Can.init(module_env, parse_ast, &module_envs);
    errdefer can.deinit();

    try can.canonicalizeFile();
    try can.validateForChecking();

    // Get Bool and Result statement indices from the IMPORTED modules (not copied!)
    const bool_stmt_in_bool_module = builtin_indices.bool_type;
    const try_stmt_in_result_module = builtin_indices.try_type;

    const module_common_idents: Check.CommonIdents = .{
        .module_name = try module_env.insertIdent(base.Ident.for_text(module_name)),
        .list = try module_env.insertIdent(base.Ident.for_text("List")),
        .box = try module_env.insertIdent(base.Ident.for_text("Box")),
        .bool_stmt = bool_stmt_in_bool_module,
        .try_stmt = try_stmt_in_result_module,
        .builtin_module = other_test_env.builtin_module.env,
    };

    // Build imported_envs array
    // Always include the builtin module for auto-imported types (Bool, Str, etc.)
    var imported_envs = try std.ArrayList(*const ModuleEnv).initCapacity(gpa, 2);
    defer imported_envs.deinit(gpa);

    // Add builtin module unconditionally (needed for auto-imported types)
    try imported_envs.append(gpa, other_test_env.builtin_module.env);

    // Process explicit imports
    const import_count = module_env.imports.imports.items.items.len;
    for (module_env.imports.imports.items.items[0..import_count]) |str_idx| {
        const import_name = module_env.getString(str_idx);
        if (std.mem.eql(u8, import_name, other_module_name)) {
            // Cross-module import - append the other test module's env
            try imported_envs.append(gpa, other_test_env.module_env);
        }
    }

    // Type Check - Pass all imported modules
    var checker = try Check.init(
        gpa,
        &module_env.types,
        module_env,
        imported_envs.items,
        &module_envs,
        &module_env.store.regions,
        module_common_idents,
    );
    errdefer checker.deinit();

    try checker.checkFile();

    var type_writer = try module_env.initTypeWriter();
    errdefer type_writer.deinit();

    return TestEnv{
        .gpa = gpa,
        .module_env = module_env,
        .parse_ast = parse_ast,
        .can = can,
        .checker = checker,
        .type_writer = type_writer,
        .module_envs = module_envs,
        .builtin_module = other_test_env.builtin_module,
        .owns_builtin_module = false, // Borrowed from other_test_env
    };
}

/// Initialize where the provided source is an entire file
pub fn init(module_name: []const u8, source: []const u8) !TestEnv {
    const gpa = std.testing.allocator;

    // Allocate our ModuleEnv, AST, and Can on the heap
    // so we can keep them around for testing purposes...
    // this is an unusual setup, but helps us with testing
    const module_env: *ModuleEnv = try gpa.create(ModuleEnv);
    errdefer gpa.destroy(module_env);

    const parse_ast = try gpa.create(parse.AST);
    errdefer gpa.destroy(parse_ast);

    const can = try gpa.create(Can);
    errdefer gpa.destroy(can);

    var module_envs = std.AutoHashMap(base.Ident.Idx, Can.AutoImportedType).init(gpa);

    // Load Builtin module once - Bool, Result, and Str are all types within this module
    const builtin_indices = try deserializeBuiltinIndices(gpa, compiled_builtins.builtin_indices_bin);
    var builtin_module = try loadCompiledModule(gpa, compiled_builtins.builtin_bin, "Builtin", compiled_builtins.builtin_source);
    errdefer builtin_module.deinit();

    // Initialize the ModuleEnv with the CommonEnv
    module_env.* = try ModuleEnv.init(gpa, source);
    errdefer module_env.deinit();

    module_env.common.source = source;
    module_env.module_name = module_name;
    module_env.module_name_idx = try module_env.insertIdent(base.Ident.for_text(module_name));
    try module_env.common.calcLineStarts(gpa);

    // Populate module_envs with Bool, Result, Dict, Set using shared function
    // This ensures production and tests use identical logic
    try Can.populateModuleEnvs(
        &module_envs,
        module_env,
        builtin_module.env,
        builtin_indices,
    );

    // Parse the AST
    parse_ast.* = try parse.parse(&module_env.common, gpa);
    errdefer parse_ast.deinit(gpa);
    parse_ast.store.emptyScratch();

    // Canonicalize
    try module_env.initCIRFields(gpa, module_name);

    can.* = try Can.init(module_env, parse_ast, &module_envs);
    errdefer can.deinit();

    try can.canonicalizeFile();
    try can.validateForChecking();

    // Get Bool and Result statement indices from the IMPORTED modules (not copied!)
    const bool_stmt_in_bool_module = builtin_indices.bool_type;
    const try_stmt_in_result_module = builtin_indices.try_type;

    const module_common_idents: Check.CommonIdents = .{
        .module_name = try module_env.insertIdent(base.Ident.for_text(module_name)),
        .list = try module_env.insertIdent(base.Ident.for_text("List")),
        .box = try module_env.insertIdent(base.Ident.for_text("Box")),
        .bool_stmt = bool_stmt_in_bool_module,
        .try_stmt = try_stmt_in_result_module,
        .builtin_module = builtin_module.env,
    };

    // Build imported_envs array
    // Always include the builtin module for auto-imported types (Bool, Str, etc.)
    var imported_envs = try std.ArrayList(*const ModuleEnv).initCapacity(gpa, 2);
    defer imported_envs.deinit(gpa);

    // Add builtin module unconditionally (needed for auto-imported types)
    try imported_envs.append(gpa, builtin_module.env);


    // Type Check - Pass the imported modules in other_modules parameter
    var checker = try Check.init(
        gpa,
        &module_env.types,
        module_env,
        imported_envs.items,
        &module_envs,
        &module_env.store.regions,
        module_common_idents,
    );
    errdefer checker.deinit();

    try checker.checkFile();

    var type_writer = try module_env.initTypeWriter();
    errdefer type_writer.deinit();

    return TestEnv{
        .gpa = gpa,
        .module_env = module_env,
        .parse_ast = parse_ast,
        .can = can,
        .checker = checker,
        .type_writer = type_writer,
        .module_envs = module_envs,
        .builtin_module = builtin_module,
        .owns_builtin_module = true, // We own this module
    };
}

/// Initialize where the provided source a single expression
pub fn initExpr(module_name: []const u8, comptime source_expr: []const u8) !TestEnv {
    const source_wrapper =
        \\main =
    ;

    var source: [source_wrapper.len + 1 + source_expr.len]u8 = undefined;
    std.mem.copyForwards(u8, source[0..], source_wrapper);
    std.mem.copyForwards(u8, source[source_wrapper.len..], " ");
    std.mem.copyForwards(u8, source[source_wrapper.len + 1 ..], source_expr);

    return TestEnv.init(module_name, &source);
}

pub fn deinit(self: *TestEnv) void {
    self.can.deinit();
    self.gpa.destroy(self.can);
    self.parse_ast.deinit(self.gpa);
    self.gpa.destroy(self.parse_ast);

    self.checker.deinit();
    self.type_writer.deinit();

    // ModuleEnv.deinit calls self.common.deinit() to clean up CommonEnv's internals
    // Since common is now a value field, we don't need to free it separately
    self.module_env.deinit();
    self.gpa.destroy(self.module_env);

    self.module_envs.deinit();

    // Clean up loaded Builtin module (only if we own it)
    if (self.owns_builtin_module) {
        self.builtin_module.deinit();
    }
}

/// Get the inferred type of the last declaration and compare it to the provided
/// expected type string.
///
/// Also assert that there were no problems processing the source code.
pub fn assertDefType(self: *TestEnv, target_def_name: []const u8, expected: []const u8) !void {
    try self.assertNoParseProblems();
    try self.assertNoCanProblems();
    try self.assertNoTypeProblems();

    try testing.expect(self.module_env.all_defs.span.len > 0);

    const idents = self.module_env.getIdentStoreConst();
    const defs_slice = self.module_env.store.sliceDefs(self.module_env.all_defs);
    for (defs_slice) |def_idx| {
        const def = self.module_env.store.getDef(def_idx);
        const ptrn = self.module_env.store.getPattern(def.pattern);

        switch (ptrn) {
            .assign => |assign| {
                const def_name = idents.getText(assign.ident);
                if (std.mem.eql(u8, target_def_name, def_name)) {
                    try testing.expectEqualStrings(
                        expected,
                        try self.type_writer.writeGet(ModuleEnv.varFrom(def_idx)),
                    );
                    return;
                }
            },
            else => {
                return error.TestUnexpectedResult;
            },
        }
    }
    return error.TestUnexpectedResult;
}

/// Get the inferred type of the last declaration and compare it to the provided
/// expected type string.
///
/// Also assert that there were no problems processing the source code.
pub fn assertLastDefType(self: *TestEnv, expected: []const u8) !void {
    try self.assertNoParseProblems();
    try self.assertNoCanProblems();
    try self.assertNoTypeProblems();

    try testing.expect(self.module_env.all_defs.span.len > 0);
    const defs_slice = self.module_env.store.sliceDefs(self.module_env.all_defs);
    const last_def_idx = defs_slice[defs_slice.len - 1];

    try testing.expectEqualStrings(expected, try self.type_writer.writeGet(ModuleEnv.varFrom(last_def_idx)));
}

/// Get the inferred type descriptor of the last declaration
///
/// Also assert that there were no problems processing the source code.
pub fn getLastExprType(self: *TestEnv) !types.Descriptor {
    try self.assertNoParseProblems();
    // try self.assertNoCanProblems();
    try self.assertNoTypeProblems();

    try testing.expect(self.module_env.all_defs.span.len > 0);
    const defs_slice = self.module_env.store.sliceDefs(self.module_env.all_defs);
    const last_def_idx = defs_slice[defs_slice.len - 1];

    return self.module_env.types.resolveVar(ModuleEnv.varFrom(last_def_idx)).desc;
}

/// Assert that there was a single type error when checking the input. Assert
/// that the title of the type error matches the expected title.
pub fn assertOneTypeError(self: *TestEnv, expected: []const u8) !void {
    try self.assertNoParseProblems();
    // try self.assertNoCanProblems();

    // Assert 1 problem
    try testing.expectEqual(1, self.checker.problems.problems.items.len);
    const problem = self.checker.problems.problems.items[0];

    // Assert the rendered problem matches the expected problem
    var report_builder = problem_mod.ReportBuilder.init(
        self.gpa,
        self.module_env,
        self.module_env,
        &self.checker.snapshots,
        "test",
        &.{},
        &self.checker.import_mapping,
    );
    defer report_builder.deinit();

    var report = try report_builder.build(problem);
    defer report.deinit();

    try testing.expectEqualStrings(expected, report.title);
}

fn renderReportToMarkdownBuffer(buf: *std.array_list.Managed(u8), report: anytype) !void {
    buf.clearRetainingCapacity();
    var unmanaged = buf.moveToUnmanaged();
    defer buf.* = unmanaged.toManaged(buf.allocator);

    var writer_alloc = std.Io.Writer.Allocating.fromArrayList(buf.allocator, &unmanaged);
    defer unmanaged = writer_alloc.toArrayList();

    report.render(&writer_alloc.writer, .markdown) catch |err| switch (err) {
        error.WriteFailed => return error.OutOfMemory,
        else => return err,
    };
}

fn assertNoParseProblems(self: *TestEnv) !void {
    if (self.parse_ast.hasErrors()) {
        var report_buf = try std.array_list.Managed(u8).initCapacity(self.gpa, 256);
        defer report_buf.deinit();

        for (self.parse_ast.tokenize_diagnostics.items) |tok_diag| {
            var report = try self.parse_ast.tokenizeDiagnosticToReport(tok_diag, self.gpa, null);
            defer report.deinit();

            try renderReportToMarkdownBuffer(&report_buf, &report);
            try testing.expectEqualStrings("EXPECTED NO ERROR", report_buf.items);
        }

        for (self.parse_ast.parse_diagnostics.items) |diag| {
            var report = try self.parse_ast.parseDiagnosticToReport(&self.module_env.common, diag, self.gpa, self.module_env.module_name);
            defer report.deinit();

            try renderReportToMarkdownBuffer(&report_buf, &report);
            try testing.expectEqualStrings("EXPECTED NO ERROR", report_buf.items);
        }
    }
}

fn assertNoCanProblems(self: *TestEnv) !void {
    var report_buf = try std.array_list.Managed(u8).initCapacity(self.gpa, 256);
    defer report_buf.deinit();

    const diagnostics = try self.module_env.getDiagnostics();
    defer self.gpa.free(diagnostics);

    for (diagnostics) |d| {
        var report = try self.module_env.diagnosticToReport(d, self.gpa, self.module_env.module_name);
        defer report.deinit();

        try renderReportToMarkdownBuffer(&report_buf, &report);

        // Ignore "MISSING MAIN! FUNCTION" error - it's expected in test modules
        if (std.mem.indexOf(u8, report_buf.items, "MISSING MAIN! FUNCTION") != null) {
            continue;
        }

        try testing.expectEqualStrings("EXPECTED NO ERROR", report_buf.items);
    }
}

fn assertNoTypeProblems(self: *TestEnv) !void {
    var report_builder = problem_mod.ReportBuilder.init(self.gpa, self.module_env, self.module_env, &self.checker.snapshots, "test", &.{}, &self.checker.import_mapping);
    defer report_builder.deinit();

    var report_buf = try std.array_list.Managed(u8).initCapacity(self.gpa, 256);
    defer report_buf.deinit();

    for (self.checker.problems.problems.items) |problem| {
        var report = try report_builder.build(problem);
        defer report.deinit();

        try renderReportToMarkdownBuffer(&report_buf, &report);
        try testing.expectEqualStrings("EXPECTED NO ERROR", report_buf.items);
    }

    try testing.expectEqual(0, self.checker.problems.problems.items.len);
}
