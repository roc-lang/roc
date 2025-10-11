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
    buffer: []align(collections.CompactWriter.SERIALIZATION_ALIGNMENT) u8,
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
    const aligned_buffer = try gpa.alignedAlloc(u8, @alignOf(CIR.BuiltinIndices), bin_data.len);
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
    env.* = ModuleEnv{
        .gpa = gpa,
        .common = serialized_ptr.common.deserialize(@as(i64, @intCast(base_ptr)), source).*,
        .types = serialized_ptr.types.deserialize(@as(i64, @intCast(base_ptr))).*,
        .module_kind = serialized_ptr.module_kind,
        .all_defs = serialized_ptr.all_defs,
        .all_statements = serialized_ptr.all_statements,
        .exports = serialized_ptr.exports,
        .builtin_statements = serialized_ptr.builtin_statements,
        .external_decls = serialized_ptr.external_decls.deserialize(@as(i64, @intCast(base_ptr))).*,
        .imports = serialized_ptr.imports.deserialize(@as(i64, @intCast(base_ptr)), gpa).*,
        .module_name = module_name,
        .diagnostics = serialized_ptr.diagnostics,
        .store = serialized_ptr.store.deserialize(@as(i64, @intCast(base_ptr)), gpa).*,
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

module_envs: std.StringHashMap(*const ModuleEnv),
other_envs: std.ArrayList(*const ModuleEnv),

// Loaded builtin modules (loaded per test, cleaned up in deinit)
bool_module: LoadedModule,
result_module: LoadedModule,

/// Test environment for canonicalization testing, providing a convenient wrapper around ModuleEnv, AST, and Can.
const TestEnv = @This();

/// Initialize where the provided source is an entire file
///
/// Accepts another module that should already be can'd and type checked, and will
/// add that module as an import to this module
pub fn initWithImport(source: []const u8, other_module_name: []const u8, other_module_env: *const ModuleEnv) !TestEnv {
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

    var module_envs = std.StringHashMap(*const ModuleEnv).init(gpa);
    var other_envs = std.ArrayList(*const ModuleEnv).init(gpa);

    // Put the other module in the env map
    try module_envs.put(other_module_name, other_module_env);

    const module_name = "Test";
    std.debug.assert(!std.mem.eql(u8, module_name, other_module_name));

    // Load builtin modules (following eval.zig pattern)
    const builtin_indices = try deserializeBuiltinIndices(gpa, compiled_builtins.builtin_indices_bin);
    const bool_source = "Bool := [True, False].{}\n";
    const result_source = "Result(ok, err) := [Ok(ok), Err(err)].{}\n";
    var bool_module = try loadCompiledModule(gpa, compiled_builtins.bool_bin, "Bool", bool_source);
    errdefer bool_module.deinit();
    var result_module = try loadCompiledModule(gpa, compiled_builtins.result_bin, "Result", result_source);
    errdefer result_module.deinit();

    // Initialize the ModuleEnv with the CommonEnv
    module_env.* = try ModuleEnv.init(gpa, source);
    errdefer module_env.deinit();

    module_env.common.source = source;
    module_env.module_name = module_name;
    try module_env.common.calcLineStarts(gpa);

    // Parse the AST
    parse_ast.* = try parse.parse(&module_env.common, gpa);
    errdefer parse_ast.deinit(gpa);
    parse_ast.store.emptyScratch();

    // Canonicalize
    try module_env.initCIRFields(gpa, "test");

    // Inject builtin type declarations (Bool and Result) following eval.zig pattern
    const bool_stmt = bool_module.env.store.getStatement(builtin_indices.bool_type);
    const actual_bool_idx = try module_env.store.addStatement(bool_stmt, base.Region.zero());

    const result_stmt = result_module.env.store.getStatement(builtin_indices.result_type);
    const actual_result_idx = try module_env.store.addStatement(result_stmt, base.Region.zero());

    // Update builtin_statements span
    const start_idx = @intFromEnum(actual_bool_idx);
    const end_idx = @intFromEnum(actual_result_idx);
    module_env.builtin_statements = .{ .span = .{
        .start = start_idx,
        .len = end_idx - start_idx + 1,
    } };

    can.* = try Can.init(module_env, parse_ast, &module_envs);
    errdefer can.deinit();

    try can.canonicalizeFile();
    try can.validateForChecking();

    const module_common_idents: Check.CommonIdents = .{
        .module_name = try module_env.insertIdent(base.Ident.for_text(module_name)),
        .list = try module_env.insertIdent(base.Ident.for_text("List")),
        .box = try module_env.insertIdent(base.Ident.for_text("Box")),
        .bool_stmt = actual_bool_idx,
        .result_stmt = actual_result_idx,
    };

    // Pull out the imported index
    std.debug.assert(can.import_indices.size == 1);
    const import_idx = can.import_indices.get(other_module_name).?;
    std.debug.assert(@intFromEnum(import_idx) == 0);
    try other_envs.append(other_module_env);

    // Type Check
    var checker = try Check.init(gpa, &module_env.types, module_env, other_envs.items, &module_env.store.regions, module_common_idents);
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
        .other_envs = other_envs,
        .bool_module = bool_module,
        .result_module = result_module,
    };
}

/// Initialize where the provided source is an entire file
pub fn init(source: []const u8) !TestEnv {
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

    const module_envs = std.StringHashMap(*const ModuleEnv).init(gpa);
    const other_envs = std.ArrayList(*const ModuleEnv).init(gpa);

    const module_name = "Test";

    // Load builtin modules (following eval.zig pattern)
    const builtin_indices = try deserializeBuiltinIndices(gpa, compiled_builtins.builtin_indices_bin);
    const bool_source = "Bool := [True, False].{}\n";
    const result_source = "Result(ok, err) := [Ok(ok), Err(err)].{}\n";
    var bool_module = try loadCompiledModule(gpa, compiled_builtins.bool_bin, "Bool", bool_source);
    errdefer bool_module.deinit();
    var result_module = try loadCompiledModule(gpa, compiled_builtins.result_bin, "Result", result_source);
    errdefer result_module.deinit();

    // Initialize the ModuleEnv with the CommonEnv
    module_env.* = try ModuleEnv.init(gpa, source);
    errdefer module_env.deinit();

    module_env.common.source = source;
    module_env.module_name = module_name;
    try module_env.common.calcLineStarts(gpa);

    // Parse the AST
    parse_ast.* = try parse.parse(&module_env.common, gpa);
    errdefer parse_ast.deinit(gpa);
    parse_ast.store.emptyScratch();

    // Canonicalize
    try module_env.initCIRFields(gpa, "test");

    // Inject builtin type declarations (Bool and Result) following eval.zig pattern
    // Get the Bool type declaration from the loaded module using the build-time index
    const bool_stmt = bool_module.env.store.getStatement(builtin_indices.bool_type);
    const actual_bool_idx = try module_env.store.addStatement(bool_stmt, base.Region.zero());

    // Get the Result type declaration from the loaded module using the build-time index
    const result_stmt = result_module.env.store.getStatement(builtin_indices.result_type);
    const actual_result_idx = try module_env.store.addStatement(result_stmt, base.Region.zero());

    // Update builtin_statements span to include injected Bool and Result
    // Use the ACTUAL indices where they landed (not hardcoded!)
    const start_idx = @intFromEnum(actual_bool_idx);
    const end_idx = @intFromEnum(actual_result_idx);
    module_env.builtin_statements = .{ .span = .{
        .start = start_idx,
        .len = end_idx - start_idx + 1,
    } };

    can.* = try Can.init(module_env, parse_ast, null);
    errdefer can.deinit();

    try can.canonicalizeFile();
    try can.validateForChecking();

    const module_common_idents: Check.CommonIdents = .{
        .module_name = try module_env.insertIdent(base.Ident.for_text(module_name)),
        .list = try module_env.insertIdent(base.Ident.for_text("List")),
        .box = try module_env.insertIdent(base.Ident.for_text("Box")),
        .bool_stmt = actual_bool_idx,
        .result_stmt = actual_result_idx,
    };

    // Type Check
    var checker = try Check.init(gpa, &module_env.types, module_env, &.{}, &module_env.store.regions, module_common_idents);
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
        .other_envs = other_envs,
        .bool_module = bool_module,
        .result_module = result_module,
    };
}

/// Initialize where the provided source a single expression
pub fn initExpr(comptime source_expr: []const u8) !TestEnv {
    const source_wrapper =
        \\module []
        \\ 
        \\main =
    ;

    var source: [source_wrapper.len + 1 + source_expr.len]u8 = undefined;
    std.mem.copyForwards(u8, source[0..], source_wrapper);
    std.mem.copyForwards(u8, source[source_wrapper.len..], " ");
    std.mem.copyForwards(u8, source[source_wrapper.len + 1 ..], source_expr);

    return TestEnv.init(&source);
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
    self.other_envs.deinit();

    // Clean up loaded builtin modules
    self.bool_module.deinit();
    self.result_module.deinit();
}

/// Get the inferred type of the last declaration and compare it to the provided
/// expected type string.
///
/// Also assert that there were no problems processing the source code.
pub fn assertLastDefType(self: *TestEnv, expected: []const u8) !void {
    try self.assertNoParseProblems();
    // try self.assertNoCanProblems();
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
    );
    defer report_builder.deinit();

    var report = try report_builder.build(problem);
    defer report.deinit();

    try testing.expectEqualStrings(expected, report.title);
}

fn assertNoParseProblems(self: *TestEnv) !void {
    if (self.parse_ast.hasErrors()) {
        var report_buf = try std.ArrayList(u8).initCapacity(self.gpa, 256);
        defer report_buf.deinit();

        for (self.parse_ast.tokenize_diagnostics.items) |tok_diag| {
            var report = try self.parse_ast.tokenizeDiagnosticToReport(tok_diag, self.gpa);
            defer report.deinit();

            report_buf.clearRetainingCapacity();
            try report.render(report_buf.writer(), .markdown);

            try testing.expectEqualStrings("EXPECTED NO ERROR", report_buf.items);
        }

        for (self.parse_ast.parse_diagnostics.items) |diag| {
            var report = try self.parse_ast.parseDiagnosticToReport(&self.module_env.common, diag, self.gpa, self.module_env.module_name);
            defer report.deinit();

            report_buf.clearRetainingCapacity();
            try report.render(report_buf.writer(), .markdown);

            try testing.expectEqualStrings("EXPECTED NO ERROR", report_buf.items);
        }
    }
}

fn assertNoCanProblems(self: *TestEnv) !void {
    var report_buf = try std.ArrayList(u8).initCapacity(self.gpa, 256);
    defer report_buf.deinit();

    const diagnostics = try self.module_env.getDiagnostics();
    for (diagnostics) |d| {
        var report = try self.module_env.diagnosticToReport(d, self.gpa, self.module_env.module_name);
        defer report.deinit();

        report_buf.clearRetainingCapacity();
        try report.render(report_buf.writer(), .markdown);

        try testing.expectEqualStrings("EXPECTED NO ERROR", report_buf.items);
    }
}

fn assertNoTypeProblems(self: *TestEnv) !void {
    var report_builder = problem_mod.ReportBuilder.init(self.gpa, self.module_env, self.module_env, &self.checker.snapshots, "test", &.{});
    defer report_builder.deinit();

    var report_buf = try std.ArrayList(u8).initCapacity(self.gpa, 256);
    defer report_buf.deinit();

    for (self.checker.problems.problems.items) |problem| {
        var report = try report_builder.build(problem);
        defer report.deinit();

        report_buf.clearRetainingCapacity();
        try report.render(report_buf.writer(), .markdown);

        try testing.expectEqualStrings("EXPECTED NO ERROR", report_buf.items);
    }

    try testing.expectEqual(0, self.checker.problems.problems.items.len);
}
