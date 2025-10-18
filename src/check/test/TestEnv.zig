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

    // Deserialize store in-place (returns the same pointer, just cast to NodeStore)
    const deserialized_store_ptr = serialized_ptr.store.deserialize(@as(i64, @intCast(base_ptr)), gpa);
    const deserialized_store = deserialized_store_ptr.*;

    env.* = ModuleEnv{
        .gpa = gpa,
        .common = serialized_ptr.common.deserialize(@as(i64, @intCast(base_ptr)), source).*,
        .types = serialized_ptr.types.deserialize(@as(i64, @intCast(base_ptr)), gpa).*, // Pass gpa to types deserialize
        .module_kind = serialized_ptr.module_kind,
        .all_defs = serialized_ptr.all_defs,
        .all_statements = serialized_ptr.all_statements,
        .exports = serialized_ptr.exports,
        .builtin_statements = serialized_ptr.builtin_statements,
        .external_decls = serialized_ptr.external_decls.deserialize(@as(i64, @intCast(base_ptr))).*,
        .imports = serialized_ptr.imports.deserialize(@as(i64, @intCast(base_ptr)), gpa).*,
        .module_name = module_name,
        .diagnostics = serialized_ptr.diagnostics,
        .store = deserialized_store,
    };

    return LoadedModule{
        .env = env,
        .buffer = buffer,
        .gpa = gpa,
    };
}

/// Helper function to expose all top-level definitions in a module
/// This makes them available for cross-module imports
fn exposeAllDefs(module_env: *ModuleEnv) !void {
    const defs_slice = module_env.store.sliceDefs(module_env.all_defs);
    for (defs_slice, 0..) |def_idx, i| {
        const def = module_env.store.getDef(def_idx);

        // Get the pattern to find the identifier
        const pattern = module_env.store.getPattern(def.pattern);
        if (pattern == .assign) {
            const ident_idx = pattern.assign.ident;
            const def_idx_u16: u16 = @intCast(i);
            try module_env.setExposedNodeIndexById(ident_idx, def_idx_u16);
        }
    }
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

    // Load builtin modules as proper imported modules
    const builtin_indices = try deserializeBuiltinIndices(gpa, compiled_builtins.builtin_indices_bin);
    const bool_source = "Bool := [True, False].{ not }\n";
    const result_source = "Result(ok, err) := [Ok(ok), Err(err)].{}\n";
    var bool_module = try loadCompiledModule(gpa, compiled_builtins.bool_bin, "Bool", bool_source);
    errdefer bool_module.deinit();
    var result_module = try loadCompiledModule(gpa, compiled_builtins.result_bin, "Result", result_source);
    errdefer result_module.deinit();

    // Set node indices for the exposed types so they can be properly referenced
    // The Bool type is at statement index 1, so we set node_idx to 1
    const bool_ident = bool_module.env.common.findIdent("Bool") orelse unreachable;
    try bool_module.env.setExposedNodeIndexById(bool_ident, @intCast(@intFromEnum(builtin_indices.bool_type)));

    // The Result type is at statement index 3, so we set node_idx to 3
    const result_ident = result_module.env.common.findIdent("Result") orelse unreachable;
    try result_module.env.setExposedNodeIndexById(result_ident, @intCast(@intFromEnum(builtin_indices.result_type)));

    // Add Bool and Result to module_envs for auto-importing
    try module_envs.put("Bool", bool_module.env);
    try module_envs.put("Result", result_module.env);

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

    can.* = try Can.init(module_env, parse_ast, &module_envs);
    errdefer can.deinit();

    try can.canonicalizeFile();
    try can.validateForChecking();

    // Expose all top-level definitions so they can be imported by other modules
    try exposeAllDefs(module_env);

    // Get Bool and Result statement indices from the IMPORTED modules (not copied!)
    const bool_stmt_in_bool_module = builtin_indices.bool_type;
    const result_stmt_in_result_module = builtin_indices.result_type;

    const module_common_idents: Check.CommonIdents = .{
        .module_name = try module_env.insertIdent(base.Ident.for_text(module_name)),
        .list = try module_env.insertIdent(base.Ident.for_text("List")),
        .box = try module_env.insertIdent(base.Ident.for_text("Box")),
        .bool_stmt = bool_stmt_in_bool_module,
        .result_stmt = result_stmt_in_result_module,
    };

    // Build other_envs array to match the import indices assigned by canonicalizer
    // The canonicalizer assigns import indices for:
    // 1. Explicit imports (like "import A")
    // 2. Auto-imported modules that are actually used (like Bool, Result)

    // Determine the size needed for other_envs
    var max_import_idx: usize = 0;

    // Check the user module
    if (can.import_indices.get(other_module_name)) |idx| {
        const idx_int = @intFromEnum(idx);
        if (idx_int > max_import_idx) max_import_idx = idx_int;
    }

    // Check Bool
    if (can.import_indices.get("Bool")) |idx| {
        const idx_int = @intFromEnum(idx);
        if (idx_int > max_import_idx) max_import_idx = idx_int;
    }

    // Check Result
    if (can.import_indices.get("Result")) |idx| {
        const idx_int = @intFromEnum(idx);
        if (idx_int > max_import_idx) max_import_idx = idx_int;
    }

    // Allocate array of the right size, initialized to other_module_env as a safe default
    try other_envs.resize(max_import_idx + 1);
    for (other_envs.items) |*item| {
        item.* = other_module_env; // Safe default
    }

    // Fill in the correct modules at their import indices
    if (can.import_indices.get(other_module_name)) |idx| {
        other_envs.items[@intFromEnum(idx)] = other_module_env;
    }
    if (can.import_indices.get("Bool")) |idx| {
        other_envs.items[@intFromEnum(idx)] = bool_module.env;
    }
    if (can.import_indices.get("Result")) |idx| {
        other_envs.items[@intFromEnum(idx)] = result_module.env;
    }

    // Type Check - Pass all imported modules (Bool, Result, and user module)
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

    var module_envs = std.StringHashMap(*const ModuleEnv).init(gpa);

    const module_name = "Test";

    // Load builtin modules as proper imported modules
    const builtin_indices = try deserializeBuiltinIndices(gpa, compiled_builtins.builtin_indices_bin);
    const bool_source = "Bool := [True, False].{ not }\n";
    const result_source = "Result(ok, err) := [Ok(ok), Err(err)].{}\n";
    var bool_module = try loadCompiledModule(gpa, compiled_builtins.bool_bin, "Bool", bool_source);
    errdefer bool_module.deinit();
    var result_module = try loadCompiledModule(gpa, compiled_builtins.result_bin, "Result", result_source);
    errdefer result_module.deinit();

    // Set node indices for the exposed types so they can be properly referenced
    // The Bool type is at statement index 1, so we set node_idx to 1
    const bool_ident = bool_module.env.common.findIdent("Bool") orelse unreachable;
    try bool_module.env.setExposedNodeIndexById(bool_ident, @intCast(@intFromEnum(builtin_indices.bool_type)));

    // The Result type is at statement index 3, so we set node_idx to 3
    const result_ident = result_module.env.common.findIdent("Result") orelse unreachable;
    try result_module.env.setExposedNodeIndexById(result_ident, @intCast(@intFromEnum(builtin_indices.result_type)));

    // Add Bool and Result to module_envs for auto-importing
    try module_envs.put("Bool", bool_module.env);
    try module_envs.put("Result", result_module.env);

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

    can.* = try Can.init(module_env, parse_ast, &module_envs);
    errdefer can.deinit();

    try can.canonicalizeFile();
    try can.validateForChecking();

    // Expose all top-level definitions so they can be imported by other modules
    try exposeAllDefs(module_env);

    // Get Bool and Result statement indices from the IMPORTED modules (not copied!)
    const bool_stmt_in_bool_module = builtin_indices.bool_type;
    const result_stmt_in_result_module = builtin_indices.result_type;

    const module_common_idents: Check.CommonIdents = .{
        .module_name = try module_env.insertIdent(base.Ident.for_text(module_name)),
        .list = try module_env.insertIdent(base.Ident.for_text("List")),
        .box = try module_env.insertIdent(base.Ident.for_text("Box")),
        .bool_stmt = bool_stmt_in_bool_module,
        .result_stmt = result_stmt_in_result_module,
    };

    // Build other_envs array to match the import indices assigned by canonicalizer
    var other_envs = std.ArrayList(*const ModuleEnv).init(gpa);

    // Only build the array if there are actual imports
    if (can.import_indices.size > 0) {
        // Determine the size needed
        var max_import_idx: usize = 0;
        if (can.import_indices.get("Bool")) |idx| {
            const idx_int = @intFromEnum(idx);
            if (idx_int > max_import_idx) max_import_idx = idx_int;
        }
        if (can.import_indices.get("Result")) |idx| {
            const idx_int = @intFromEnum(idx);
            if (idx_int > max_import_idx) max_import_idx = idx_int;
        }

        // Allocate array of the right size, initialized to bool_module.env as a safe default
        try other_envs.resize(max_import_idx + 1);
        for (other_envs.items) |*item| {
            item.* = bool_module.env; // Safe default
        }

        // Fill in the correct modules at their import indices
        if (can.import_indices.get("Bool")) |idx| {
            other_envs.items[@intFromEnum(idx)] = bool_module.env;
        }
        if (can.import_indices.get("Result")) |idx| {
            other_envs.items[@intFromEnum(idx)] = result_module.env;
        }
    }

    // Type Check - Pass the imported modules in other_modules parameter
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

/// Initialize where the provided source a single expression
pub fn initExpr(comptime source_expr: []const u8) !TestEnv {
    const source_wrapper =
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
    defer self.gpa.free(diagnostics);

    for (diagnostics) |d| {
        var report = try self.module_env.diagnosticToReport(d, self.gpa, self.module_env.module_name);
        defer report.deinit();

        report_buf.clearRetainingCapacity();
        try report.render(report_buf.writer(), .markdown);

        // Ignore "MISSING MAIN! FUNCTION" error - it's expected in test modules
        if (std.mem.indexOf(u8, report_buf.items, "MISSING MAIN! FUNCTION") != null) {
            continue;
        }

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
