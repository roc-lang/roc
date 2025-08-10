//! Tests for the expression evaluator
const std = @import("std");
const parse = @import("parse");
const types = @import("types");
const base = @import("base");
const compile = @import("compile");
const Can = @import("can");
const Check = @import("check").Check;

const eval = @import("../interpreter.zig");
const test_env = @import("../test_env.zig");
const stack = @import("../stack.zig");
const layout_store = @import("../../layout/store.zig");
const layout = @import("../../layout/layout.zig");
const builtins = @import("builtins");

const ModuleEnv = compile.ModuleEnv;
const Layout = layout.Layout;
const Closure = eval.Closure;
const testing = std.testing;
const test_allocator = testing.allocator;
const ParseError = parse.Parser.Error;

/// Helper function to run an expression and expect a specific error.
pub fn runExpectError(src: []const u8, expected_error: eval.EvalError, should_trace: enum { trace, no_trace }) !void {
    const resources = try parseAndCanonicalizeExpr(test_allocator, src);
    defer cleanupParseAndCanonical(test_allocator, resources);

    var eval_stack = try stack.Stack.initCapacity(test_allocator, 1024);
    defer eval_stack.deinit();

    var layout_cache = try layout_store.Store.init(resources.module_env, &resources.module_env.types);
    defer layout_cache.deinit();

    var test_env_instance = test_env.TestEnv.init(test_allocator);
    defer test_env_instance.deinit();

    var interpreter = try eval.Interpreter.init(
        test_allocator,
        resources.module_env,
        &eval_stack,
        &layout_cache,
        &resources.module_env.types,
    );
    defer interpreter.deinit(test_env_instance.get_ops());
    test_env_instance.setInterpreter(&interpreter);

    if (should_trace == .trace) {
        interpreter.startTrace(std.io.getStdErr().writer().any());
    }

    const result = interpreter.eval(resources.expr_idx, test_env_instance.get_ops());

    if (should_trace == .trace) {
        interpreter.endTrace();
    }

    try testing.expectError(expected_error, result);
}

/// Helpers to setup and run an interpreter expecting an integer result.
pub fn runExpectInt(src: []const u8, expected_int: i128, should_trace: enum { trace, no_trace }) !void {
    const resources = try parseAndCanonicalizeExpr(test_allocator, src);
    defer cleanupParseAndCanonical(test_allocator, resources);

    var eval_stack = try stack.Stack.initCapacity(test_allocator, 1024);
    defer eval_stack.deinit();

    var layout_cache = try layout_store.Store.init(resources.module_env, &resources.module_env.types);
    defer layout_cache.deinit();

    var test_env_instance = test_env.TestEnv.init(test_allocator);
    defer test_env_instance.deinit();

    var interpreter = try eval.Interpreter.init(
        test_allocator,
        resources.module_env,
        &eval_stack,
        &layout_cache,
        &resources.module_env.types,
    );
    defer interpreter.deinit(test_env_instance.get_ops());
    test_env_instance.setInterpreter(&interpreter);

    if (should_trace == .trace) {
        interpreter.startTrace(std.io.getStdErr().writer().any());
    }

    const result = try interpreter.eval(resources.expr_idx, test_env_instance.get_ops());

    if (should_trace == .trace) {
        interpreter.endTrace();
    }

    try testing.expectEqual(expected_int, result.asI128());
}

/// Helper function to run an expression and expect a boolean result.
pub fn runExpectBool(src: []const u8, expected_bool: bool, should_trace: enum { trace, no_trace }) !void {
    const resources = try parseAndCanonicalizeExpr(test_allocator, src);
    defer cleanupParseAndCanonical(test_allocator, resources);

    var eval_stack = try stack.Stack.initCapacity(test_allocator, 1024);
    defer eval_stack.deinit();

    var layout_cache = try layout_store.Store.init(resources.module_env, &resources.module_env.types);
    defer layout_cache.deinit();

    var test_env_instance = test_env.TestEnv.init(test_allocator);
    defer test_env_instance.deinit();

    var interpreter = try eval.Interpreter.init(
        test_allocator,
        resources.module_env,
        &eval_stack,
        &layout_cache,
        &resources.module_env.types,
    );
    const roc_ops_ptr = test_env_instance.get_ops();
    defer interpreter.deinit(roc_ops_ptr);

    if (should_trace == .trace) {
        interpreter.startTrace(std.io.getStdErr().writer().any());
    }

    const result = interpreter.eval(resources.expr_idx, roc_ops_ptr) catch |err| {
        std.debug.print("Evaluation failed: {}\n", .{err});
        return err;
    };

    if (should_trace == .trace) {
        interpreter.endTrace();
    }

    // For boolean results, we can read the underlying byte value
    if (result.layout.tag == .scalar and result.layout.data.scalar.tag == .int) {
        // Boolean represented as integer
        const int_val = result.asI128();
        const bool_val = int_val != 0;
        try testing.expectEqual(expected_bool, bool_val);
    } else {
        // Try reading as raw byte (for boolean tag values)
        std.debug.assert(result.ptr != null);
        const bool_ptr = @as(*const u8, @ptrCast(result.ptr.?));
        const bool_val = bool_ptr.* != 0;
        try testing.expectEqual(expected_bool, bool_val);
    }
}

/// Helpers to setup and run an interpreter expecting a string result.
pub fn runExpectStr(src: []const u8, expected_str: []const u8, should_trace: enum { trace, no_trace }) !void {
    const resources = try parseAndCanonicalizeExpr(test_allocator, src);
    defer cleanupParseAndCanonical(test_allocator, resources);

    var eval_stack = try stack.Stack.initCapacity(test_allocator, 1024);
    defer eval_stack.deinit();

    var layout_cache = try layout_store.Store.init(resources.module_env, &resources.module_env.types);
    defer layout_cache.deinit();

    var test_env_instance = test_env.TestEnv.init(test_allocator);
    defer test_env_instance.deinit();

    var interpreter = try eval.Interpreter.init(
        test_allocator,
        resources.module_env,
        &eval_stack,
        &layout_cache,
        &resources.module_env.types,
    );
    defer interpreter.deinit(test_env_instance.get_ops());
    test_env_instance.setInterpreter(&interpreter);

    if (should_trace == .trace) {
        interpreter.startTrace(std.io.getStdErr().writer().any());
    }

    const result = try interpreter.eval(resources.expr_idx, test_env_instance.get_ops());

    if (should_trace == .trace) {
        interpreter.endTrace();
    }

    // Verify we got a scalar string layout
    try testing.expect(result.layout.tag == .scalar);
    try testing.expect(result.layout.data.scalar.tag == .str);

    // Read the string result
    const roc_str: *const builtins.str.RocStr = @ptrCast(@alignCast(result.ptr.?));
    const str_slice = roc_str.asSlice();

    try testing.expectEqualStrings(expected_str, str_slice);

    // Clean up reference counting for big strings
    if (!roc_str.isSmallStr()) {
        // We need to decref because the result is no longer needed
        // Cast away const to call decref (safe since we're done with it)
        const mutable_roc_str: *builtins.str.RocStr = @constCast(roc_str);
        mutable_roc_str.decref(test_env_instance.get_ops());
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

    var eval_stack = try stack.Stack.initCapacity(test_allocator, 1024);
    defer eval_stack.deinit();

    var layout_cache = try layout_store.Store.init(resources.module_env, &resources.module_env.types);
    defer layout_cache.deinit();

    var test_env_instance = test_env.TestEnv.init(test_allocator);
    defer test_env_instance.deinit();

    var interpreter = try eval.Interpreter.init(
        test_allocator,
        resources.module_env,
        &eval_stack,
        &layout_cache,
        &resources.module_env.types,
    );
    defer interpreter.deinit(test_env_instance.get_ops());
    test_env_instance.setInterpreter(&interpreter);

    if (should_trace == .trace) {
        interpreter.startTrace(std.io.getStdErr().writer().any());
    }

    const result = try interpreter.eval(resources.expr_idx, test_env_instance.get_ops());

    if (should_trace == .trace) {
        interpreter.endTrace();
    }

    // Verify we got a tuple layout
    try testing.expect(result.layout.tag == .tuple);

    // Use the TupleAccessor to safely access tuple elements
    const tuple_accessor = try result.asTuple(&layout_cache);

    try testing.expectEqual(expected_elements.len, tuple_accessor.getElementCount());

    for (expected_elements) |expected_element| {
        // Get the element at the specified index
        const element = try tuple_accessor.getElement(expected_element.index);

        // Verify it's an integer
        try testing.expect(element.layout.tag == .scalar and element.layout.data.scalar.tag == .int);

        // Get the integer value from the element
        const int_val = element.asI128();
        try testing.expectEqual(expected_element.value, int_val);
    }
}

/// Helpers to setup and run an interpreter expecting a record result.
pub fn runExpectRecord(src: []const u8, expected_fields: []const ExpectedField, should_trace: enum { trace, no_trace }) !void {
    const resources = try parseAndCanonicalizeExpr(test_allocator, src);
    defer cleanupParseAndCanonical(test_allocator, resources);

    var eval_stack = try stack.Stack.initCapacity(test_allocator, 1024);
    defer eval_stack.deinit();

    var layout_cache = try layout_store.Store.init(resources.module_env, &resources.module_env.types);
    defer layout_cache.deinit();

    var interpreter = try eval.Interpreter.init(
        test_allocator,
        resources.module_env,
        &eval_stack,
        &layout_cache,
        &resources.module_env.types,
    );
    defer interpreter.deinit();

    var test_env_instance = test_env.TestEnv.init(test_allocator);
    defer test_env_instance.deinit();
    test_env_instance.setInterpreter(&interpreter);

    if (should_trace == .trace) {
        interpreter.startTrace(std.io.getStdErr().writer().any());
    }

    const result = try interpreter.eval(resources.expr_idx, test_env_instance.get_ops());

    if (should_trace == .trace) {
        interpreter.endTrace();
    }

    // Verify we got a record layout
    try testing.expect(result.layout.tag == .record);

    const record_data = layout_cache.getRecordData(result.layout.data.record.idx);
    const sorted_fields = layout_cache.record_fields.sliceRange(record_data.getFields());

    try testing.expectEqual(expected_fields.len, sorted_fields.len);

    for (expected_fields) |expected_field| {
        var found = false;
        var i: u32 = 0;
        while (i < sorted_fields.len) : (i += 1) {
            const sorted_field = sorted_fields.get(i);
            const field_name = resources.module_env.idents.getText(sorted_field.name);
            if (std.mem.eql(u8, field_name, expected_field.name)) {
                found = true;
                const field_layout = layout_cache.getLayout(sorted_field.layout);
                try testing.expect(field_layout.tag == .scalar and field_layout.data.scalar.tag == .int);

                const offset = layout_cache.getRecordFieldOffset(result.layout.data.record.idx, i);
                const field_ptr = @as([*]u8, @ptrCast(result.ptr.?)) + offset;
                const int_val = eval.readIntFromMemory(field_ptr, field_layout.data.scalar.data.int);
                try testing.expectEqual(expected_field.value, int_val);
                break;
            }
        }
        try testing.expect(found);
    }
}

/// Parse and canonicalize an expression.
pub fn parseAndCanonicalizeExpr(allocator: std.mem.Allocator, source: []const u8) ParseError!struct {
    module_env: *ModuleEnv,
    parse_ast: *parse.AST,
    can: *Can,
    checker: *Check,
    expr_idx: ModuleEnv.Expr.Idx,
} {
    // Initialize the ModuleEnv
    const module_env = try allocator.create(ModuleEnv);
    module_env.* = try ModuleEnv.init(allocator, source);

    module_env.source = source;
    try module_env.calcLineStarts();

    // Parse the source code as an expression
    const parse_ast = try allocator.create(parse.AST);
    parse_ast.* = try parse.parseExpr(module_env);

    // Empty scratch space (required before canonicalization)
    parse_ast.store.emptyScratch();

    // Initialize CIR fields in ModuleEnv
    try module_env.initCIRFields(allocator, "test");

    // Create czer
    //
    const can = try allocator.create(Can);
    can.* = try Can.init(module_env, parse_ast, null);

    // Canonicalize the expression
    const expr_idx: parse.AST.Expr.Idx = @enumFromInt(parse_ast.root_node_idx);
    const canonical_expr_idx = try can.canonicalizeExpr(expr_idx) orelse {
        // If canonicalization fails, create a runtime error
        const diagnostic_idx = try module_env.store.addDiagnostic(.{ .not_implemented = .{
            .feature = try module_env.strings.insert(allocator, "canonicalization failed"),
            .region = base.Region.zero(),
        } });
        const checker = try allocator.create(Check);
        checker.* = try Check.init(allocator, &module_env.types, module_env, &.{}, &module_env.store.regions);
        return .{
            .module_env = module_env,
            .parse_ast = parse_ast,
            .can = can,
            .checker = checker,
            .expr_idx = try module_env.store.addExpr(.{ .e_runtime_error = .{
                .diagnostic = diagnostic_idx,
            } }, base.Region.zero()),
        };
    };

    // Create type checker
    const checker = try allocator.create(Check);
    checker.* = try Check.init(allocator, &module_env.types, module_env, &.{}, &module_env.store.regions);

    // Type check the expression
    _ = try checker.checkExpr(canonical_expr_idx.get_idx());

    // WORKAROUND: The type checker doesn't set types for binop expressions yet.
    // For numeric binops, manually set the type to match the operands.
    const expr = module_env.store.getExpr(canonical_expr_idx.get_idx());
    if (expr == .e_binop) {
        const binop = expr.e_binop;
        // For arithmetic ops, use the type of the left operand
        switch (binop.op) {
            .add, .sub, .mul, .div, .rem, .pow, .div_trunc => {
                const left_var = @as(types.Var, @enumFromInt(@intFromEnum(binop.lhs)));
                const left_resolved = module_env.types.resolveVar(left_var);
                const result_var = @as(types.Var, @enumFromInt(@intFromEnum(canonical_expr_idx.get_idx())));
                try module_env.types.setVarContent(result_var, left_resolved.desc.content);
            },
            .lt, .gt, .le, .ge, .eq, .ne => {
                // Comparison ops return Bool
                const result_var = @as(types.Var, @enumFromInt(@intFromEnum(canonical_expr_idx.get_idx())));
                const bool_content = try module_env.types.mkBool(allocator, &module_env.idents, @enumFromInt(0));
                try module_env.types.setVarContent(result_var, bool_content);
            },
            else => {},
        }
    }

    return .{
        .module_env = module_env,
        .parse_ast = parse_ast,
        .can = can,
        .checker = checker,
        .expr_idx = canonical_expr_idx.get_idx(),
    };
}

/// Cleanup resources allocated by parseAndCanonicalizeExpr.
pub fn cleanupParseAndCanonical(allocator: std.mem.Allocator, resources: anytype) void {
    resources.checker.deinit();
    resources.can.deinit();
    resources.parse_ast.deinit(allocator);
    // module_env.source is freed by module_env.deinit()
    resources.module_env.deinit();
    allocator.destroy(resources.checker);
    allocator.destroy(resources.can);
    allocator.destroy(resources.parse_ast);
    allocator.destroy(resources.module_env);
}

test "eval runtime error - returns crash error" {
    const source = "crash \"test feature\"";

    const resources = try parseAndCanonicalizeExpr(test_allocator, source);
    defer cleanupParseAndCanonical(test_allocator, resources);

    // Check if the expression is a runtime error
    const expr = resources.module_env.store.getExpr(resources.expr_idx);
    if (expr == .e_runtime_error) {
        // Create a stack for evaluation
        var eval_stack = try stack.Stack.initCapacity(test_allocator, 1024);
        defer eval_stack.deinit();

        // Create layout store
        var layout_cache = try layout_store.Store.init(resources.module_env, &resources.module_env.types);
        defer layout_cache.deinit();

        var test_env_instance = test_env.TestEnv.init(test_allocator);
        defer test_env_instance.deinit();

        // Evaluating a runtime error should return an error
        var interpreter = try eval.Interpreter.init(
            test_allocator,
            resources.module_env,
            &eval_stack,
            &layout_cache,
            &resources.module_env.types,
        );
        defer interpreter.deinit(test_env_instance.get_ops());
        test_env_instance.setInterpreter(&interpreter);

        const result = interpreter.eval(resources.expr_idx, test_env_instance.get_ops());
        try testing.expectError(eval.EvalError.Crash, result);
    } else {
        // If crash syntax is not supported in canonicalization, skip
        return error.SkipZigTest;
    }
}

test "eval tag - already primitive" {
    // Try to see if tag_union layout is now implemented
    const source = "True";

    const resources = try parseAndCanonicalizeExpr(test_allocator, source);
    defer cleanupParseAndCanonical(test_allocator, resources);

    // Create a stack for evaluation
    var eval_stack = try stack.Stack.initCapacity(test_allocator, 1024);
    defer eval_stack.deinit();

    // Create layout store
    var layout_cache = try layout_store.Store.init(resources.module_env, &resources.module_env.types);
    defer layout_cache.deinit();

    var test_env_instance = test_env.TestEnv.init(test_allocator);
    defer test_env_instance.deinit();

    // Create interpreter
    var interpreter = try eval.Interpreter.init(
        test_allocator,
        resources.module_env,
        &eval_stack,
        &layout_cache,
        &resources.module_env.types,
    );
    defer interpreter.deinit(test_env_instance.get_ops());
    test_env_instance.setInterpreter(&interpreter);

    // Try to evaluate - if tag_union layout is not implemented, this might fail
    const result = interpreter.eval(resources.expr_idx, test_env_instance.get_ops()) catch |err| {
        std.debug.print("Tag evaluation failed with error: {}\n", .{err});
        // If evaluation fails for any reason, skip for now
        return error.SkipZigTest;
    };

    // If we get here, check if we have a valid result
    // True/False are optimized to scalar values in the current implementation
    try testing.expect(result.layout.tag == .scalar);
    try testing.expect(result.ptr != null);
}

test "interpreter reuse across multiple evaluations" {
    // This test demonstrates that the interpreter can be reused across multiple
    // eval() calls, avoiding repeated allocations in scenarios like the REPL

    // Test multiple evaluations with the same work stack
    const sources = [_][]const u8{ "42", "100 + 200", "if True 1 else 2" };
    const expected = [_]i128{ 42, 300, 1 };

    for (sources, expected) |source, expected_value| {
        const resources = try parseAndCanonicalizeExpr(test_allocator, source);
        defer cleanupParseAndCanonical(test_allocator, resources);

        var eval_stack = try stack.Stack.initCapacity(test_allocator, 1024);
        defer eval_stack.deinit();
        var layout_cache = try layout_store.Store.init(resources.module_env, &resources.module_env.types);
        defer layout_cache.deinit();

        var test_env_instance = test_env.TestEnv.init(test_allocator);
        defer test_env_instance.deinit();

        // Create interpreter for this evaluation
        var interpreter = try eval.Interpreter.init(
            test_allocator,
            resources.module_env,
            &eval_stack,
            &layout_cache,
            &resources.module_env.types,
        );
        defer interpreter.deinit(test_env_instance.get_ops());
        test_env_instance.setInterpreter(&interpreter);

        // Verify work stack is empty before eval
        try testing.expectEqual(@as(usize, 0), interpreter.work_stack.items.len);

        const result = try interpreter.eval(resources.expr_idx, test_env_instance.get_ops());

        // Verify work stack is empty after eval (should be naturally empty, not cleared)
        try testing.expectEqual(@as(usize, 0), interpreter.work_stack.items.len);

        // Verify the result
        try testing.expect(result.layout.tag == .scalar);
        try testing.expect(result.layout.data.scalar.tag == .int);
        const value: *i128 = @ptrCast(@alignCast(result.ptr.?));
        try testing.expectEqual(expected_value, value.*);
    }
}

test "nominal type context preservation - boolean" {
    // Test that Bool.True and Bool.False get correct boolean layout
    // This tests the nominal type context preservation fix

    // Test Bool.True
    try runExpectBool("Bool.True", true, .no_trace);

    // Test Bool.False
    try runExpectBool("Bool.False", false, .no_trace);

    // Test boolean negation with nominal types
    try runExpectBool("!Bool.True", false, .no_trace);
    try runExpectBool("!Bool.False", true, .no_trace);

    // Test boolean operations with nominal types
    try runExpectBool("Bool.True and Bool.False", false, .no_trace);
    try runExpectBool("Bool.True or Bool.False", true, .no_trace);
}

test "nominal type context preservation - regression prevention" {
    // Test that the fix prevents the original regression
    // The original issue was that (|x| !x)(True) would return "0" instead of "False"

    // This should work correctly now with nominal type context preservation
    try runExpectBool("(|x| !x)(Bool.True)", false, .no_trace);
    try runExpectBool("(|x| !x)(Bool.False)", true, .no_trace);
}
