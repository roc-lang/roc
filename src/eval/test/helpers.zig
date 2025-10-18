//! Tests for the expression evaluator
const std = @import("std");
const parse = @import("parse");
const types = @import("types");
const base = @import("base");
const can = @import("can");
const check = @import("check");
const builtins = @import("builtins");
const collections = @import("collections");
const compiled_builtins = @import("compiled_builtins");

const TestEnv = @import("TestEnv.zig");
const Interpreter = @import("../interpreter.zig").Interpreter;
const StackValue = @import("../StackValue.zig");

const Check = check.Check;
const Can = can.Can;
const CIR = can.CIR;
const ModuleEnv = can.ModuleEnv;
const test_allocator = std.testing.allocator;

const TestParseError = parse.Parser.Error || error{ TokenizeError, SyntaxError };

/// Wrapper for a loaded compiled module that tracks the buffer
pub const LoadedModule = struct {
    env: *ModuleEnv,
    buffer: []align(collections.CompactWriter.SERIALIZATION_ALIGNMENT) u8,
    gpa: std.mem.Allocator,

    pub fn deinit(self: *LoadedModule) void {
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
pub fn deserializeBuiltinIndices(gpa: std.mem.Allocator, bin_data: []const u8) !CIR.BuiltinIndices {
    // Copy to properly aligned memory
    const aligned_buffer = try gpa.alignedAlloc(u8, @alignOf(CIR.BuiltinIndices), bin_data.len);
    defer gpa.free(aligned_buffer);
    @memcpy(aligned_buffer, bin_data);

    const indices_ptr = @as(*const CIR.BuiltinIndices, @ptrCast(aligned_buffer.ptr));
    return indices_ptr.*;
}

/// Load a compiled ModuleEnv from embedded binary data
pub fn loadCompiledModule(gpa: std.mem.Allocator, bin_data: []const u8, module_name: []const u8, source: []const u8) !LoadedModule {
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

/// Helper function to run an expression and expect a specific error.
pub fn runExpectError(src: []const u8, expected_error: anyerror, should_trace: enum { trace, no_trace }) !void {
    const resources = try parseAndCanonicalizeExpr(test_allocator, src);
    defer cleanupParseAndCanonical(test_allocator, resources);

    var test_env_instance = TestEnv.init(test_allocator);
    defer test_env_instance.deinit();

    var interpreter = try Interpreter.init(test_allocator, resources.module_env, resources.bool_stmt, resources.bool_module.env, null);
    defer interpreter.deinit();

    const enable_trace = should_trace == .trace;
    if (enable_trace) {
        interpreter.startTrace(std.io.getStdErr().writer().any());
    }
    defer if (enable_trace) interpreter.endTrace();

    const ops = test_env_instance.get_ops();
    _ = interpreter.evalMinimal(resources.expr_idx, ops) catch |err| {
        try std.testing.expectEqual(expected_error, err);
        return;
    };

    // If we reach here, no error was thrown.
    try std.testing.expect(false);
}

/// Helpers to setup and run an interpreter expecting an integer result.
pub fn runExpectInt(src: []const u8, expected_int: i128, should_trace: enum { trace, no_trace }) !void {
    const resources = try parseAndCanonicalizeExpr(test_allocator, src);
    defer cleanupParseAndCanonical(test_allocator, resources);

    var test_env_instance = TestEnv.init(test_allocator);
    defer test_env_instance.deinit();

    var interpreter = try Interpreter.init(test_allocator, resources.module_env, resources.bool_stmt, resources.bool_module.env, null);
    defer interpreter.deinit();

    const enable_trace = should_trace == .trace;
    if (enable_trace) {
        interpreter.startTrace(std.io.getStdErr().writer().any());
    }
    defer if (enable_trace) interpreter.endTrace();

    const ops = test_env_instance.get_ops();
    const result = try interpreter.evalMinimal(resources.expr_idx, ops);
    const layout_cache = &interpreter.runtime_layout_store;
    defer result.decref(layout_cache, ops);

    try std.testing.expectEqual(expected_int, result.asI128());
}

/// Helper function to run an expression and expect a boolean result.
pub fn runExpectBool(src: []const u8, expected_bool: bool, should_trace: enum { trace, no_trace }) !void {
    const resources = try parseAndCanonicalizeExpr(test_allocator, src);
    defer cleanupParseAndCanonical(test_allocator, resources);

    var test_env_instance = TestEnv.init(test_allocator);
    defer test_env_instance.deinit();

    var interpreter = try Interpreter.init(test_allocator, resources.module_env, resources.bool_stmt, resources.bool_module.env, null);
    defer interpreter.deinit();

    const enable_trace = should_trace == .trace;
    if (enable_trace) {
        interpreter.startTrace(std.io.getStdErr().writer().any());
    }
    defer if (enable_trace) interpreter.endTrace();

    const ops = test_env_instance.get_ops();
    const result = try interpreter.evalMinimal(resources.expr_idx, ops);
    const layout_cache = &interpreter.runtime_layout_store;
    defer result.decref(layout_cache, ops);

    const actual = switch (result.layout.tag) {
        .scalar => switch (result.layout.data.scalar.tag) {
            .bool => result.asBool(),
            .int => result.asI128() != 0,
            else => return error.TestUnexpectedResult,
        },
        else => return error.TestUnexpectedResult,
    };

    try std.testing.expectEqual(expected_bool, actual);
}

/// Helpers to setup and run an interpreter expecting a string result.
pub fn runExpectStr(src: []const u8, expected_str: []const u8, should_trace: enum { trace, no_trace }) !void {
    const resources = try parseAndCanonicalizeExpr(test_allocator, src);
    defer cleanupParseAndCanonical(test_allocator, resources);

    var test_env_instance = TestEnv.init(test_allocator);
    defer test_env_instance.deinit();

    var interpreter = try Interpreter.init(test_allocator, resources.module_env, resources.bool_stmt, resources.bool_module.env, null);
    defer interpreter.deinit();

    const enable_trace = should_trace == .trace;
    if (enable_trace) {
        interpreter.startTrace(std.io.getStdErr().writer().any());
    }
    defer if (enable_trace) interpreter.endTrace();

    const ops = test_env_instance.get_ops();
    const result = try interpreter.evalMinimal(resources.expr_idx, ops);
    const layout_cache = &interpreter.runtime_layout_store;

    try std.testing.expect(result.layout.tag == .scalar);
    try std.testing.expect(result.layout.data.scalar.tag == .str);

    const roc_str: *const builtins.str.RocStr = @ptrCast(@alignCast(result.ptr.?));
    const str_slice = roc_str.asSlice();
    try std.testing.expectEqualStrings(expected_str, str_slice);

    if (!roc_str.isSmallStr()) {
        const mutable_roc_str: *builtins.str.RocStr = @constCast(roc_str);
        mutable_roc_str.decref(ops);
    } else {
        result.decref(layout_cache, ops);
    }
}

/// A record field we expect to see in our unit test results
pub const ExpectedField = struct {
    name: []const u8,
    value: i128,
};

/// A tuple element we expect to see in our unit test results
pub const ExpectedElement = struct {
    index: u32,
    value: i128,
};

/// Helpers to setup and run an interpreter expecting a tuple result.
pub fn runExpectTuple(src: []const u8, expected_elements: []const ExpectedElement, should_trace: enum { trace, no_trace }) !void {
    const resources = try parseAndCanonicalizeExpr(test_allocator, src);
    defer cleanupParseAndCanonical(test_allocator, resources);

    var test_env_instance = TestEnv.init(test_allocator);
    defer test_env_instance.deinit();

    var interpreter = try Interpreter.init(test_allocator, resources.module_env, resources.bool_stmt, resources.bool_module.env, null);
    defer interpreter.deinit();

    const enable_trace = should_trace == .trace;
    if (enable_trace) {
        interpreter.startTrace(std.io.getStdErr().writer().any());
    }
    defer if (enable_trace) interpreter.endTrace();

    const ops = test_env_instance.get_ops();
    const result = try interpreter.evalMinimal(resources.expr_idx, ops);
    const layout_cache = &interpreter.runtime_layout_store;
    defer result.decref(layout_cache, ops);

    // Verify we got a tuple layout
    try std.testing.expect(result.layout.tag == .tuple);

    // Use the TupleAccessor to safely access tuple elements
    const tuple_accessor = try result.asTuple(layout_cache);

    try std.testing.expectEqual(expected_elements.len, tuple_accessor.getElementCount());

    for (expected_elements) |expected_element| {
        // Get the element at the specified index
        const element = try tuple_accessor.getElement(@intCast(expected_element.index));

        // Verify it's an integer
        try std.testing.expect(element.layout.tag == .scalar and element.layout.data.scalar.tag == .int);

        // Get the integer value from the element
        const int_val = element.asI128();
        try std.testing.expectEqual(expected_element.value, int_val);
    }
}

/// Helpers to setup and run an interpreter expecting a record result.
pub fn runExpectRecord(src: []const u8, expected_fields: []const ExpectedField, should_trace: enum { trace, no_trace }) !void {
    const resources = try parseAndCanonicalizeExpr(test_allocator, src);
    defer cleanupParseAndCanonical(test_allocator, resources);

    var test_env_instance = TestEnv.init(test_allocator);
    defer test_env_instance.deinit();

    var interpreter = try Interpreter.init(test_allocator, resources.module_env, resources.bool_stmt, resources.bool_module.env, null);
    defer interpreter.deinit();

    const enable_trace = should_trace == .trace;
    if (enable_trace) {
        interpreter.startTrace(std.io.getStdErr().writer().any());
    }
    defer if (enable_trace) interpreter.endTrace();

    const ops = test_env_instance.get_ops();
    const result = try interpreter.evalMinimal(resources.expr_idx, ops);
    const layout_cache = &interpreter.runtime_layout_store;
    defer result.decref(layout_cache, ops);

    // Verify we got a record layout
    try std.testing.expect(result.layout.tag == .record);

    const record_data = layout_cache.getRecordData(result.layout.data.record.idx);
    const sorted_fields = layout_cache.record_fields.sliceRange(record_data.getFields());

    try std.testing.expectEqual(expected_fields.len, sorted_fields.len);

    for (expected_fields) |expected_field| {
        var found = false;
        var i: u32 = 0;
        while (i < sorted_fields.len) : (i += 1) {
            const sorted_field = sorted_fields.get(i);
            const field_name = resources.module_env.getIdent(sorted_field.name);
            if (std.mem.eql(u8, field_name, expected_field.name)) {
                found = true;
                const field_layout = layout_cache.getLayout(sorted_field.layout);
                try std.testing.expect(field_layout.tag == .scalar and field_layout.data.scalar.tag == .int);

                const offset = layout_cache.getRecordFieldOffset(result.layout.data.record.idx, i);
                const field_ptr = @as([*]u8, @ptrCast(result.ptr.?)) + offset;
                const field_value = StackValue{
                    .layout = field_layout,
                    .ptr = field_ptr,
                    .is_initialized = true,
                };
                const int_val = field_value.asI128();
                try std.testing.expectEqual(expected_field.value, int_val);
                break;
            }
        }
        try std.testing.expect(found);
    }
}

/// Parse and canonicalize an expression.
pub fn parseAndCanonicalizeExpr(allocator: std.mem.Allocator, source: []const u8) TestParseError!struct {
    module_env: *ModuleEnv,
    parse_ast: *parse.AST,
    can: *Can,
    checker: *Check,
    expr_idx: CIR.Expr.Idx,
    bool_stmt: CIR.Statement.Idx,
    bool_module: LoadedModule,
    result_module: LoadedModule,
} {
    // Load builtin modules (following TestEnv.zig pattern)
    const builtin_indices = try deserializeBuiltinIndices(allocator, compiled_builtins.builtin_indices_bin);
    const bool_source = "Bool := [True, False].{}\n";
    const result_source = "Result(ok, err) := [Ok(ok), Err(err)].{}\n";
    var bool_module = try loadCompiledModule(allocator, compiled_builtins.bool_bin, "Bool", bool_source);
    errdefer bool_module.deinit();
    var result_module = try loadCompiledModule(allocator, compiled_builtins.result_bin, "Result", result_source);
    errdefer result_module.deinit();

    // Initialize the ModuleEnv
    const module_env = try allocator.create(ModuleEnv);
    module_env.* = try ModuleEnv.init(allocator, source);

    module_env.common.source = source;
    try module_env.common.calcLineStarts(module_env.gpa);

    // Parse the source code as an expression (following REPL pattern)
    const parse_ast = try allocator.create(parse.AST);
    parse_ast.* = try parse.parseExpr(&module_env.common, module_env.gpa);

    // Check for parse errors in test code
    // NOTE: This is TEST-ONLY behavior! In production, the parser continues and collects
    // diagnostics to provide better error messages. But for tests, we want to fail early
    // on syntax errors to catch issues like semicolons that shouldn't be in Roc code.
    if (parse_ast.tokenize_diagnostics.items.len > 0) {
        // Found tokenization errors in test code
        return error.TokenizeError;
    }

    if (parse_ast.parse_diagnostics.items.len > 0) {
        // Found parse errors in test code
        return error.SyntaxError;
    }

    // Empty scratch space (required before canonicalization)
    parse_ast.store.emptyScratch();

    // Initialize CIR fields in ModuleEnv
    try module_env.initCIRFields(allocator, "test");

    // Register Bool and Result as imports so they're available for qualified name resolution
    _ = try module_env.imports.getOrPut(allocator, &module_env.common.strings, "Bool");
    _ = try module_env.imports.getOrPut(allocator, &module_env.common.strings, "Result");

    // Get Bool and Result statement indices from IMPORTED modules (not copied!)
    const bool_stmt_in_bool_module = builtin_indices.bool_type;
    const result_stmt_in_result_module = builtin_indices.result_type;

    const common_idents: Check.CommonIdents = .{
        .module_name = try module_env.insertIdent(base.Ident.for_text("test")),
        .list = try module_env.insertIdent(base.Ident.for_text("List")),
        .box = try module_env.insertIdent(base.Ident.for_text("Box")),
        .bool_stmt = bool_stmt_in_bool_module,
        .result_stmt = result_stmt_in_result_module,
    };

    // Create module_envs map for canonicalization (enables qualified calls)
    var module_envs_map = std.StringHashMap(*const ModuleEnv).init(allocator);
    defer module_envs_map.deinit();
    try module_envs_map.put("Bool", bool_module.env);
    try module_envs_map.put("Result", result_module.env);

    // Create czer with module_envs_map for qualified name resolution (following REPL pattern)
    const czer = try allocator.create(Can);
    czer.* = try Can.init(module_env, parse_ast, &module_envs_map);

    // NOTE: Qualified tags like Bool.True and Bool.False do not currently work in test expressions
    // because the canonicalizer doesn't support cross-module type references.
    // See QUALIFIED_TAGS.md for details on what needs to be implemented.
    //
    // For now, tests should use unqualified tags (True, False) which work via unqualified_nominal_tags map.

    // Canonicalize the expression (following REPL pattern)
    const expr_idx: parse.AST.Expr.Idx = @enumFromInt(parse_ast.root_node_idx);
    const canonical_expr = try czer.canonicalizeExpr(expr_idx) orelse {
        // If canonicalization fails, create a runtime error
        const diagnostic_idx = try module_env.store.addDiagnostic(.{ .not_implemented = .{
            .feature = try module_env.insertString("canonicalization failed"),
            .region = base.Region.zero(),
        } });
        const checker = try allocator.create(Check);
        // Pass Bool and Result as imported modules
        const other_modules = [_]*const ModuleEnv{ bool_module.env, result_module.env };
        checker.* = try Check.init(allocator, &module_env.types, module_env, &other_modules, &module_env.store.regions, common_idents);
        return .{
            .module_env = module_env,
            .parse_ast = parse_ast,
            .can = czer,
            .checker = checker,
            .expr_idx = try module_env.store.addExpr(.{ .e_runtime_error = .{
                .diagnostic = diagnostic_idx,
            } }, base.Region.zero()),
            .bool_stmt = bool_stmt_in_bool_module,
            .bool_module = bool_module,
            .result_module = result_module,
        };
    };
    const canonical_expr_idx = canonical_expr.get_idx();

    // Create type checker - pass Bool and Result as imported modules
    const other_modules = [_]*const ModuleEnv{ bool_module.env, result_module.env };
    const checker = try allocator.create(Check);
    checker.* = try Check.init(allocator, &module_env.types, module_env, &other_modules, &module_env.store.regions, common_idents);

    // Type check the expression
    _ = try checker.checkExprRepl(canonical_expr_idx);

    return .{
        .module_env = module_env,
        .parse_ast = parse_ast,
        .can = czer,
        .checker = checker,
        .expr_idx = canonical_expr_idx,
        .bool_stmt = bool_stmt_in_bool_module,
        .bool_module = bool_module,
        .result_module = result_module,
    };
}

/// Cleanup resources allocated by parseAndCanonicalizeExpr.
pub fn cleanupParseAndCanonical(allocator: std.mem.Allocator, resources: anytype) void {
    // Cast away const since deinit() needs mutable access
    var bool_module_copy = resources.bool_module;
    var result_module_copy = resources.result_module;
    bool_module_copy.deinit();
    result_module_copy.deinit();
    resources.checker.deinit();
    resources.can.deinit();
    resources.parse_ast.deinit(allocator);
    // module_env.source is not owned by module_env - don't free it
    resources.module_env.deinit();
    allocator.destroy(resources.checker);
    allocator.destroy(resources.can);
    allocator.destroy(resources.parse_ast);
    allocator.destroy(resources.module_env);
}

test "eval runtime error - returns crash error" {
    try runExpectError("{ crash \"test feature\" 0 }", error.Crash, .no_trace);
}

test "eval tag - already primitive" {
    const resources = try parseAndCanonicalizeExpr(test_allocator, "True");
    defer cleanupParseAndCanonical(test_allocator, resources);

    var test_env_instance = TestEnv.init(test_allocator);
    defer test_env_instance.deinit();

    var interpreter = try Interpreter.init(test_allocator, resources.module_env, resources.bool_stmt, resources.bool_module.env, null);
    defer interpreter.deinit();

    const ops = test_env_instance.get_ops();
    const result = try interpreter.evalMinimal(resources.expr_idx, ops);
    const layout_cache = &interpreter.runtime_layout_store;
    defer result.decref(layout_cache, ops);

    try std.testing.expect(result.layout.tag == .scalar);
    try std.testing.expect(result.ptr != null);
}

test "interpreter reuse across multiple evaluations" {
    const cases = [_]struct {
        src: []const u8,
        expected: i128,
    }{
        .{ .src = "42", .expected = 42 },
        .{ .src = "100 + 200", .expected = 300 },
        .{ .src = "if True 1 else 2", .expected = 1 },
    };

    for (cases) |case| {
        const resources = try parseAndCanonicalizeExpr(test_allocator, case.src);
        defer cleanupParseAndCanonical(test_allocator, resources);

        var test_env_instance = TestEnv.init(test_allocator);
        defer test_env_instance.deinit();

        var interpreter = try Interpreter.init(test_allocator, resources.module_env, resources.bool_stmt, resources.bool_module.env, null);
        defer interpreter.deinit();

        const ops = test_env_instance.get_ops();

        var iteration: usize = 0;
        while (iteration < 2) : (iteration += 1) {
            const result = try interpreter.evalMinimal(resources.expr_idx, ops);
            const layout_cache = &interpreter.runtime_layout_store;
            defer result.decref(layout_cache, ops);

            try std.testing.expect(result.layout.tag == .scalar);
            try std.testing.expect(result.layout.data.scalar.tag == .int);
            try std.testing.expectEqual(case.expected, result.asI128());
        }

        try std.testing.expectEqual(@as(usize, 0), interpreter.bindings.items.len);
    }
}

test "nominal type context preservation - boolean" {
    // Test that True and False get correct boolean layout
    // This tests the nominal type context preservation fix

    // Test True
    try runExpectBool("True", true, .no_trace);

    // Test False
    try runExpectBool("False", false, .no_trace);

    // Test boolean negation with nominal types
    try runExpectBool("!True", false, .no_trace);
    try runExpectBool("!False", true, .no_trace);

    // Test boolean operations with nominal types
    try runExpectBool("True and False", false, .no_trace);
    try runExpectBool("True or False", true, .no_trace);
}

test "nominal type context preservation - regression prevention" {
    // Test that the fix prevents the original regression
    // The original issue was that (|x| !x)(True) would return "0" instead of "False"

    // This should work correctly now with nominal type context preservation
    try runExpectBool("(|x| !x)(True)", false, .no_trace);
    try runExpectBool("(|x| !x)(False)", true, .no_trace);
}
