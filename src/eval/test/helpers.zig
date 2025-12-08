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

const eval_mod = @import("../mod.zig");
const builtin_loading_mod = eval_mod.builtin_loading;
const TestEnv = @import("TestEnv.zig");
const Interpreter = eval_mod.Interpreter;
const StackValue = eval_mod.StackValue;
const BuiltinTypes = eval_mod.BuiltinTypes;
const LoadedModule = builtin_loading_mod.LoadedModule;
const deserializeBuiltinIndices = builtin_loading_mod.deserializeBuiltinIndices;
const loadCompiledModule = builtin_loading_mod.loadCompiledModule;

const Check = check.Check;
const Can = can.Can;
const CIR = can.CIR;
const ModuleEnv = can.ModuleEnv;
const test_allocator = std.testing.allocator;

const TestParseError = parse.Parser.Error || error{ TokenizeError, SyntaxError };

const TraceWriter = struct {
    buffer: [256]u8 = undefined,
    writer: std.fs.File.Writer = undefined,

    fn init() TraceWriter {
        var tw = TraceWriter{};
        tw.writer = std.fs.File.stderr().writer(&tw.buffer);
        return tw;
    }

    fn interface(self: *TraceWriter) *std.Io.Writer {
        return &self.writer.interface;
    }
};

/// Helper function to run an expression and expect a specific error.
pub fn runExpectError(src: []const u8, expected_error: anyerror, should_trace: enum { trace, no_trace }) !void {
    const resources = try parseAndCanonicalizeExpr(test_allocator, src);
    defer cleanupParseAndCanonical(test_allocator, resources);

    var test_env_instance = TestEnv.init(test_allocator);
    defer test_env_instance.deinit();

    const builtin_types = BuiltinTypes.init(resources.builtin_indices, resources.builtin_module.env, resources.builtin_module.env, resources.builtin_module.env);
    const imported_envs = [_]*const can.ModuleEnv{resources.builtin_module.env};
    var interpreter = try Interpreter.init(test_allocator, resources.module_env, builtin_types, resources.builtin_module.env, &imported_envs, &resources.checker.import_mapping, null);
    defer interpreter.deinit();

    const enable_trace = should_trace == .trace;
    if (enable_trace) {
        interpreter.startTrace();
    }
    defer if (enable_trace) interpreter.endTrace();

    const ops = test_env_instance.get_ops();
    _ = interpreter.eval(resources.expr_idx, ops) catch |err| {
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

    const builtin_types = BuiltinTypes.init(resources.builtin_indices, resources.builtin_module.env, resources.builtin_module.env, resources.builtin_module.env);
    const imported_envs = [_]*const can.ModuleEnv{resources.builtin_module.env};
    var interpreter = try Interpreter.init(test_allocator, resources.module_env, builtin_types, resources.builtin_module.env, &imported_envs, &resources.checker.import_mapping, null);
    defer interpreter.deinit();

    const enable_trace = should_trace == .trace;
    if (enable_trace) {
        interpreter.startTrace();
    }
    defer if (enable_trace) interpreter.endTrace();

    const ops = test_env_instance.get_ops();
    const result = try interpreter.eval(resources.expr_idx, ops);
    const layout_cache = &interpreter.runtime_layout_store;
    defer result.decref(layout_cache, ops);

    // Check if this is an integer or Dec
    const int_value = if (result.layout.tag == .scalar and result.layout.data.scalar.tag == .int) blk: {
        // Suffixed integer literals (e.g., 255u8, 42i32) remain as integers
        break :blk result.asI128();
    } else blk: {
        // Unsuffixed numeric literals default to Dec, so extract the integer value
        const dec_value = result.asDec();
        const RocDec = builtins.dec.RocDec;
        // Convert Dec to integer by dividing by the decimal scale factor
        break :blk @divTrunc(dec_value.num, RocDec.one_point_zero_i128);
    };
    try std.testing.expectEqual(expected_int, int_value);
}

/// Helper function to run an expression and expect a boolean result.
pub fn runExpectBool(src: []const u8, expected_bool: bool, should_trace: enum { trace, no_trace }) !void {
    const resources = try parseAndCanonicalizeExpr(test_allocator, src);
    defer cleanupParseAndCanonical(test_allocator, resources);

    var test_env_instance = TestEnv.init(test_allocator);
    defer test_env_instance.deinit();

    const builtin_types = BuiltinTypes.init(resources.builtin_indices, resources.builtin_module.env, resources.builtin_module.env, resources.builtin_module.env);
    const imported_envs = [_]*const can.ModuleEnv{resources.builtin_module.env};
    var interpreter = try Interpreter.init(test_allocator, resources.module_env, builtin_types, resources.builtin_module.env, &imported_envs, &resources.checker.import_mapping, null);
    defer interpreter.deinit();

    const enable_trace = should_trace == .trace;
    if (enable_trace) {
        interpreter.startTrace();
    }
    defer if (enable_trace) interpreter.endTrace();

    const ops = test_env_instance.get_ops();
    const result = try interpreter.eval(resources.expr_idx, ops);
    const layout_cache = &interpreter.runtime_layout_store;
    defer result.decref(layout_cache, ops);

    // For boolean results, read the underlying byte value
    if (result.layout.tag == .scalar and result.layout.data.scalar.tag == .int) {
        // Boolean represented as integer (discriminant)
        const int_val = result.asI128();
        const bool_val = int_val != 0;
        try std.testing.expectEqual(expected_bool, bool_val);
    } else {
        // Try reading as raw byte (for boolean tag values)
        std.debug.assert(result.ptr != null);
        const bool_ptr: *const u8 = @ptrCast(@alignCast(result.ptr.?));
        const bool_val = bool_ptr.* != 0;
        try std.testing.expectEqual(expected_bool, bool_val);
    }
}

/// Helper function to run an expression and expect an f32 result (with epsilon tolerance).
pub fn runExpectF32(src: []const u8, expected_f32: f32, should_trace: enum { trace, no_trace }) !void {
    const resources = try parseAndCanonicalizeExpr(test_allocator, src);
    defer cleanupParseAndCanonical(test_allocator, resources);

    var test_env_instance = TestEnv.init(test_allocator);
    defer test_env_instance.deinit();

    const builtin_types = BuiltinTypes.init(resources.builtin_indices, resources.builtin_module.env, resources.builtin_module.env, resources.builtin_module.env);
    const imported_envs = [_]*const can.ModuleEnv{resources.builtin_module.env};
    var interpreter = try Interpreter.init(test_allocator, resources.module_env, builtin_types, resources.builtin_module.env, &imported_envs, &resources.checker.import_mapping, null);
    defer interpreter.deinit();

    const enable_trace = should_trace == .trace;
    if (enable_trace) {
        interpreter.startTrace();
    }
    defer if (enable_trace) interpreter.endTrace();

    const ops = test_env_instance.get_ops();
    const result = try interpreter.eval(resources.expr_idx, ops);
    const layout_cache = &interpreter.runtime_layout_store;
    defer result.decref(layout_cache, ops);

    const actual = result.asF32();
    const epsilon: f32 = 0.0001;
    const diff = @abs(actual - expected_f32);
    if (diff > epsilon) {
        std.debug.print("Expected {d}, got {d}, diff {d}\n", .{ expected_f32, actual, diff });
        return error.TestExpectedEqual;
    }
}

/// Helper function to run an expression and expect an f64 result (with epsilon tolerance).
pub fn runExpectF64(src: []const u8, expected_f64: f64, should_trace: enum { trace, no_trace }) !void {
    const resources = try parseAndCanonicalizeExpr(test_allocator, src);
    defer cleanupParseAndCanonical(test_allocator, resources);

    var test_env_instance = TestEnv.init(test_allocator);
    defer test_env_instance.deinit();

    const builtin_types = BuiltinTypes.init(resources.builtin_indices, resources.builtin_module.env, resources.builtin_module.env, resources.builtin_module.env);
    const imported_envs = [_]*const can.ModuleEnv{resources.builtin_module.env};
    var interpreter = try Interpreter.init(test_allocator, resources.module_env, builtin_types, resources.builtin_module.env, &imported_envs, &resources.checker.import_mapping, null);
    defer interpreter.deinit();

    const enable_trace = should_trace == .trace;
    if (enable_trace) {
        interpreter.startTrace();
    }
    defer if (enable_trace) interpreter.endTrace();

    const ops = test_env_instance.get_ops();
    const result = try interpreter.eval(resources.expr_idx, ops);
    const layout_cache = &interpreter.runtime_layout_store;
    defer result.decref(layout_cache, ops);

    const actual = result.asF64();
    const epsilon: f64 = 0.000000001;
    const diff = @abs(actual - expected_f64);
    if (diff > epsilon) {
        std.debug.print("Expected {d}, got {d}, diff {d}\n", .{ expected_f64, actual, diff });
        return error.TestExpectedEqual;
    }
}

/// Helper function to run an expression and expect a Dec result.
/// Dec is a fixed-point decimal type stored as i128 with 18 decimal places.
/// For testing, we compare the raw i128 values directly.
pub fn runExpectDec(src: []const u8, expected_dec_num: i128, should_trace: enum { trace, no_trace }) !void {
    const resources = try parseAndCanonicalizeExpr(test_allocator, src);
    defer cleanupParseAndCanonical(test_allocator, resources);

    var test_env_instance = TestEnv.init(test_allocator);
    defer test_env_instance.deinit();

    const builtin_types = BuiltinTypes.init(resources.builtin_indices, resources.builtin_module.env, resources.builtin_module.env, resources.builtin_module.env);
    const imported_envs = [_]*const can.ModuleEnv{resources.builtin_module.env};
    var interpreter = try Interpreter.init(test_allocator, resources.module_env, builtin_types, resources.builtin_module.env, &imported_envs, &resources.checker.import_mapping, null);
    defer interpreter.deinit();

    const enable_trace = should_trace == .trace;
    if (enable_trace) {
        interpreter.startTrace();
    }
    defer if (enable_trace) interpreter.endTrace();

    const ops = test_env_instance.get_ops();
    const result = try interpreter.eval(resources.expr_idx, ops);
    const layout_cache = &interpreter.runtime_layout_store;
    defer result.decref(layout_cache, ops);

    const actual_dec = result.asDec();
    if (actual_dec.num != expected_dec_num) {
        std.debug.print("Expected Dec({d}), got Dec({d})\n", .{ expected_dec_num, actual_dec.num });
        return error.TestExpectedEqual;
    }
}

/// Helpers to setup and run an interpreter expecting a string result.
pub fn runExpectStr(src: []const u8, expected_str: []const u8, should_trace: enum { trace, no_trace }) !void {
    const resources = try parseAndCanonicalizeExpr(test_allocator, src);
    defer cleanupParseAndCanonical(test_allocator, resources);

    var test_env_instance = TestEnv.init(test_allocator);
    defer test_env_instance.deinit();

    const builtin_types = BuiltinTypes.init(resources.builtin_indices, resources.builtin_module.env, resources.builtin_module.env, resources.builtin_module.env);
    const imported_envs = [_]*const can.ModuleEnv{resources.builtin_module.env};
    var interpreter = try Interpreter.init(test_allocator, resources.module_env, builtin_types, resources.builtin_module.env, &imported_envs, &resources.checker.import_mapping, null);
    defer interpreter.deinit();

    const enable_trace = should_trace == .trace;
    if (enable_trace) {
        interpreter.startTrace();
    }
    defer if (enable_trace) interpreter.endTrace();

    const ops = test_env_instance.get_ops();
    const result = try interpreter.eval(resources.expr_idx, ops);
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

    const builtin_types = BuiltinTypes.init(resources.builtin_indices, resources.builtin_module.env, resources.builtin_module.env, resources.builtin_module.env);
    const imported_envs = [_]*const can.ModuleEnv{resources.builtin_module.env};
    var interpreter = try Interpreter.init(test_allocator, resources.module_env, builtin_types, resources.builtin_module.env, &imported_envs, &resources.checker.import_mapping, null);
    defer interpreter.deinit();

    const enable_trace = should_trace == .trace;
    if (enable_trace) {
        interpreter.startTrace();
    }
    defer if (enable_trace) interpreter.endTrace();

    const ops = test_env_instance.get_ops();
    const result = try interpreter.eval(resources.expr_idx, ops);
    const layout_cache = &interpreter.runtime_layout_store;
    defer result.decref(layout_cache, ops);

    // Verify we got a tuple layout
    try std.testing.expect(result.layout.tag == .tuple);

    // Use the TupleAccessor to safely access tuple elements
    const tuple_accessor = try result.asTuple(layout_cache);

    try std.testing.expectEqual(expected_elements.len, tuple_accessor.getElementCount());

    for (expected_elements) |expected_element| {
        // Get the element at the specified index
        // Use the result's rt_var since we're accessing elements of the evaluated expression
        const element = try tuple_accessor.getElement(@intCast(expected_element.index), result.rt_var);

        // Check if this is an integer or Dec
        try std.testing.expect(element.layout.tag == .scalar);
        const int_val = if (element.layout.data.scalar.tag == .int) blk: {
            // Suffixed integer literals remain as integers
            break :blk element.asI128();
        } else blk: {
            // Unsuffixed numeric literals default to Dec
            const dec_value = element.asDec();
            const RocDec = builtins.dec.RocDec;
            break :blk @divTrunc(dec_value.num, RocDec.one_point_zero_i128);
        };
        try std.testing.expectEqual(expected_element.value, int_val);
    }
}

/// Helpers to setup and run an interpreter expecting a record result.
pub fn runExpectRecord(src: []const u8, expected_fields: []const ExpectedField, should_trace: enum { trace, no_trace }) !void {
    const resources = try parseAndCanonicalizeExpr(test_allocator, src);
    defer cleanupParseAndCanonical(test_allocator, resources);

    var test_env_instance = TestEnv.init(test_allocator);
    defer test_env_instance.deinit();

    const builtin_types = BuiltinTypes.init(resources.builtin_indices, resources.builtin_module.env, resources.builtin_module.env, resources.builtin_module.env);
    const imported_envs = [_]*const can.ModuleEnv{resources.builtin_module.env};
    var interpreter = try Interpreter.init(test_allocator, resources.module_env, builtin_types, resources.builtin_module.env, &imported_envs, &resources.checker.import_mapping, null);
    defer interpreter.deinit();

    const enable_trace = should_trace == .trace;
    if (enable_trace) {
        interpreter.startTrace();
    }
    defer if (enable_trace) interpreter.endTrace();

    const ops = test_env_instance.get_ops();
    const result = try interpreter.eval(resources.expr_idx, ops);
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
                try std.testing.expect(field_layout.tag == .scalar);

                const offset = layout_cache.getRecordFieldOffset(result.layout.data.record.idx, i);
                const field_ptr = @as([*]u8, @ptrCast(result.ptr.?)) + offset;
                const field_value = StackValue{
                    .layout = field_layout,
                    .ptr = field_ptr,
                    .is_initialized = true,
                    .rt_var = result.rt_var, // use result's rt_var for field access
                };
                // Check if this is an integer or Dec
                const int_val = if (field_layout.data.scalar.tag == .int) blk: {
                    // Suffixed integer literals remain as integers
                    break :blk field_value.asI128();
                } else blk: {
                    // Unsuffixed numeric literals default to Dec
                    const dec_value = field_value.asDec();
                    const RocDec = builtins.dec.RocDec;
                    break :blk @divTrunc(dec_value.num, RocDec.one_point_zero_i128);
                };
                try std.testing.expectEqual(expected_field.value, int_val);
                break;
            }
        }
        try std.testing.expect(found);
    }
}

/// Helpers to setup and run an interpreter expecting a list of i64 result.
pub fn runExpectListI64(src: []const u8, expected_elements: []const i64, should_trace: enum { trace, no_trace }) !void {
    const resources = try parseAndCanonicalizeExpr(test_allocator, src);
    defer cleanupParseAndCanonical(test_allocator, resources);

    var test_env_instance = TestEnv.init(test_allocator);
    defer test_env_instance.deinit();

    const builtin_types = BuiltinTypes.init(resources.builtin_indices, resources.builtin_module.env, resources.builtin_module.env, resources.builtin_module.env);
    const imported_envs = [_]*const can.ModuleEnv{resources.builtin_module.env};
    var interpreter = try Interpreter.init(test_allocator, resources.module_env, builtin_types, resources.builtin_module.env, &imported_envs, &resources.checker.import_mapping, null);
    defer interpreter.deinit();

    const enable_trace = should_trace == .trace;
    if (enable_trace) {
        interpreter.startTrace();
    }
    defer if (enable_trace) interpreter.endTrace();

    const ops = test_env_instance.get_ops();
    const result = try interpreter.eval(resources.expr_idx, ops);
    const layout_cache = &interpreter.runtime_layout_store;
    defer result.decref(layout_cache, ops);

    // Verify we got a list layout
    try std.testing.expect(result.layout.tag == .list or result.layout.tag == .list_of_zst);

    // Get the element layout
    const elem_layout_idx = result.layout.data.list;
    const elem_layout = layout_cache.getLayout(elem_layout_idx);

    // Use the ListAccessor to safely access list elements
    const list_accessor = try result.asList(layout_cache, elem_layout);

    try std.testing.expectEqual(expected_elements.len, list_accessor.len());

    for (expected_elements, 0..) |expected_val, i| {
        // Use the result's rt_var since we're accessing elements of the evaluated expression
        const element = try list_accessor.getElement(i, result.rt_var);

        // Check if this is an integer
        try std.testing.expect(element.layout.tag == .scalar);
        try std.testing.expect(element.layout.data.scalar.tag == .int);
        const int_val = element.asI128();
        try std.testing.expectEqual(@as(i128, expected_val), int_val);
    }
}

/// Parse and canonicalize an expression.
/// Rewrite deferred numeric literals to match their inferred types
/// This is similar to what ComptimeEvaluator does but for test expressions
fn rewriteDeferredNumericLiterals(env: *ModuleEnv, types_store: *types.Store, import_mapping: *const types.import_mapping.ImportMapping) !void {
    const literals = env.deferred_numeric_literals.items.items;

    for (literals) |literal| {
        // Resolve the type variable to get the concrete type
        const resolved = types_store.resolveVar(literal.type_var);
        const content = resolved.desc.content;

        // Extract the nominal type if this is a structure
        const nominal_type = switch (content) {
            .structure => |flat_type| switch (flat_type) {
                .nominal_type => |nom| nom,
                else => continue, // Not a nominal type
            },
            else => continue, // Not a structure
        };

        // Use import mapping to get the user-facing display name (e.g., "I64" from "Builtin.Num.I64")
        const short_type_name = types.import_mapping.getDisplayName(
            import_mapping,
            env.common.getIdentStore(),
            nominal_type.ident.ident_idx,
        );

        const num_lit_info = literal.constraint.num_literal orelse continue;

        // Rewrite the expression
        try rewriteNumericLiteralExpr(env, literal.expr_idx, short_type_name, num_lit_info);
    }
}

/// Rewrite a single numeric literal expression to match its inferred type
fn rewriteNumericLiteralExpr(
    env: *ModuleEnv,
    expr_idx: CIR.Expr.Idx,
    type_name: []const u8,
    num_lit_info: types.NumeralInfo,
) !void {
    const current_expr = env.store.getExpr(expr_idx);

    // Extract the f64 value from the current expression
    const f64_value: f64 = switch (current_expr) {
        .e_dec => |dec| blk: {
            // Dec is stored as i128 scaled by 10^18
            const scaled = @as(f64, @floatFromInt(dec.value.num));
            break :blk scaled / 1e18;
        },
        .e_dec_small => |small| blk: {
            // Small dec has numerator and denominator_power_of_ten
            const numerator = @as(f64, @floatFromInt(small.value.numerator));
            const power: u8 = small.value.denominator_power_of_ten;
            var divisor: f64 = 1.0;
            var i: u8 = 0;
            while (i < power) : (i += 1) {
                divisor *= 10.0;
            }
            break :blk numerator / divisor;
        },
        else => return, // Not a dec literal - nothing to rewrite
    };

    // Determine the target expression type based on type_name
    if (std.mem.eql(u8, type_name, "F32")) {
        // Rewrite to e_frac_f32
        const f32_value: f32 = @floatCast(f64_value);
        const node_idx: CIR.Node.Idx = @enumFromInt(@intFromEnum(expr_idx));
        env.store.nodes.set(node_idx, .{
            .tag = .expr_frac_f32,
            .data_1 = @bitCast(f32_value),
            .data_2 = 1, // has_suffix = true
            .data_3 = 0,
        });
    } else if (std.mem.eql(u8, type_name, "F64")) {
        // Rewrite to e_frac_f64
        const node_idx: CIR.Node.Idx = @enumFromInt(@intFromEnum(expr_idx));
        const f64_bits: u64 = @bitCast(f64_value);
        const low: u32 = @truncate(f64_bits);
        const high: u32 = @truncate(f64_bits >> 32);
        env.store.nodes.set(node_idx, .{
            .tag = .expr_frac_f64,
            .data_1 = low,
            .data_2 = high,
            .data_3 = 1, // has_suffix = true
        });
    } else if (!num_lit_info.is_fractional) {
        // Integer type - rewrite to e_num
        const num_kind: CIR.NumKind = blk: {
            if (std.mem.eql(u8, type_name, "I8")) break :blk .i8;
            if (std.mem.eql(u8, type_name, "U8")) break :blk .u8;
            if (std.mem.eql(u8, type_name, "I16")) break :blk .i16;
            if (std.mem.eql(u8, type_name, "U16")) break :blk .u16;
            if (std.mem.eql(u8, type_name, "I32")) break :blk .i32;
            if (std.mem.eql(u8, type_name, "U32")) break :blk .u32;
            if (std.mem.eql(u8, type_name, "I64")) break :blk .i64;
            if (std.mem.eql(u8, type_name, "U64")) break :blk .u64;
            if (std.mem.eql(u8, type_name, "I128")) break :blk .i128;
            if (std.mem.eql(u8, type_name, "U128")) break :blk .u128;
            break :blk .int_unbound;
        };

        const int_value = CIR.IntValue{
            .bytes = num_lit_info.bytes,
            .kind = if (num_lit_info.is_u128) .u128 else .i128,
        };
        try env.store.replaceExprWithNum(expr_idx, int_value, num_kind);
    }
    // For Dec type, keep the original e_dec/e_dec_small expression
}

/// Parses and canonicalizes a Roc expression for testing, returning all necessary context.
pub fn parseAndCanonicalizeExpr(allocator: std.mem.Allocator, source: []const u8) TestParseError!struct {
    module_env: *ModuleEnv,
    parse_ast: *parse.AST,
    can: *Can,
    checker: *Check,
    expr_idx: CIR.Expr.Idx,
    bool_stmt: CIR.Statement.Idx,
    builtin_module: LoadedModule,
    builtin_indices: CIR.BuiltinIndices,
    builtin_types: BuiltinTypes,
} {
    // Load Builtin module once - Bool, Try, and Str are all types within this module
    const builtin_indices = try deserializeBuiltinIndices(allocator, compiled_builtins.builtin_indices_bin);
    var builtin_module = try loadCompiledModule(allocator, compiled_builtins.builtin_bin, "Builtin", compiled_builtins.builtin_source);
    errdefer builtin_module.deinit();

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
    try module_env.initCIRFields("test");

    // Register Builtin as import so Bool, Try, and Str are available
    _ = try module_env.imports.getOrPut(allocator, &module_env.common.strings, "Builtin");

    // Get Bool, Try, and Str statement indices from Builtin module
    const bool_stmt_in_bool_module = builtin_indices.bool_type;
    const try_stmt_in_result_module = builtin_indices.try_type;
    const str_stmt_in_builtin_module = builtin_indices.str_type;

    const builtin_ctx: Check.BuiltinContext = .{
        .module_name = try module_env.insertIdent(base.Ident.for_text("test")),
        .bool_stmt = bool_stmt_in_bool_module,
        .try_stmt = try_stmt_in_result_module,
        .str_stmt = str_stmt_in_builtin_module,
        .builtin_module = builtin_module.env,
        .builtin_indices = builtin_indices,
    };

    // Create module_envs map for canonicalization (enables qualified calls)
    var module_envs_map = std.AutoHashMap(base.Ident.Idx, Can.AutoImportedType).init(allocator);
    defer module_envs_map.deinit();

    // Use the shared populateModuleEnvs function to set up auto-imported types
    // This ensures test and production code use identical module setup logic
    try Can.populateModuleEnvs(&module_envs_map, module_env, builtin_module.env, builtin_indices);

    // Create czer with module_envs_map for qualified name resolution (following REPL pattern)
    const czer = try allocator.create(Can);
    czer.* = try Can.init(module_env, parse_ast, &module_envs_map);

    // Canonicalize the expression (following REPL pattern)
    const expr_idx: parse.AST.Expr.Idx = @enumFromInt(parse_ast.root_node_idx);
    const canonical_expr = try czer.canonicalizeExpr(expr_idx) orelse {
        // If canonicalization fails, create a runtime error
        const diagnostic_idx = try module_env.store.addDiagnostic(.{ .not_implemented = .{
            .feature = try module_env.insertString("canonicalization failed"),
            .region = base.Region.zero(),
        } });
        const checker = try allocator.create(Check);
        // Pass Bool and Try as imported modules
        const imported_envs = [_]*const ModuleEnv{builtin_module.env};
        // Resolve imports - map each import to its index in imported_envs
        module_env.imports.resolveImports(module_env, &imported_envs);
        checker.* = try Check.init(allocator, &module_env.types, module_env, &imported_envs, &module_envs_map, &module_env.store.regions, builtin_ctx);
        const builtin_types = BuiltinTypes.init(builtin_indices, builtin_module.env, builtin_module.env, builtin_module.env);
        return .{
            .module_env = module_env,
            .parse_ast = parse_ast,
            .can = czer,
            .checker = checker,
            .expr_idx = try module_env.store.addExpr(.{ .e_runtime_error = .{
                .diagnostic = diagnostic_idx,
            } }, base.Region.zero()),
            .bool_stmt = bool_stmt_in_bool_module,
            .builtin_module = builtin_module,
            .builtin_indices = builtin_indices,
            .builtin_types = builtin_types,
        };
    };
    const canonical_expr_idx = canonical_expr.get_idx();

    // Create type checker - pass Builtin as imported module
    const imported_envs = [_]*const ModuleEnv{builtin_module.env};

    // Resolve imports - map each import to its index in imported_envs
    module_env.imports.resolveImports(module_env, &imported_envs);

    const checker = try allocator.create(Check);
    checker.* = try Check.init(allocator, &module_env.types, module_env, &imported_envs, &module_envs_map, &module_env.store.regions, builtin_ctx);

    // Type check the expression
    _ = try checker.checkExprRepl(canonical_expr_idx);

    // Rewrite deferred numeric literals to match their inferred types
    try rewriteDeferredNumericLiterals(module_env, &module_env.types, &checker.import_mapping);

    const builtin_types = BuiltinTypes.init(builtin_indices, builtin_module.env, builtin_module.env, builtin_module.env);
    return .{
        .module_env = module_env,
        .parse_ast = parse_ast,
        .can = czer,
        .checker = checker,
        .expr_idx = canonical_expr_idx,
        .bool_stmt = bool_stmt_in_bool_module,
        .builtin_module = builtin_module,
        .builtin_indices = builtin_indices,
        .builtin_types = builtin_types,
    };
}

/// Cleanup resources allocated by parseAndCanonicalizeExpr.
pub fn cleanupParseAndCanonical(allocator: std.mem.Allocator, resources: anytype) void {
    // Cast away const since deinit() needs mutable access
    var builtin_module_copy = resources.builtin_module;
    builtin_module_copy.deinit();
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

    const builtin_types = BuiltinTypes.init(resources.builtin_indices, resources.builtin_module.env, resources.builtin_module.env, resources.builtin_module.env);
    const imported_envs = [_]*const can.ModuleEnv{resources.builtin_module.env};
    var interpreter = try Interpreter.init(test_allocator, resources.module_env, builtin_types, resources.builtin_module.env, &imported_envs, &resources.checker.import_mapping, null);
    defer interpreter.deinit();

    const ops = test_env_instance.get_ops();
    const result = try interpreter.eval(resources.expr_idx, ops);
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

        var interpreter = try Interpreter.init(test_allocator, resources.module_env, resources.builtin_types, resources.builtin_module.env, &[_]*const can.ModuleEnv{}, &resources.checker.import_mapping, null);
        defer interpreter.deinit();

        const ops = test_env_instance.get_ops();

        var iteration: usize = 0;
        while (iteration < 2) : (iteration += 1) {
            const result = try interpreter.eval(resources.expr_idx, ops);
            const layout_cache = &interpreter.runtime_layout_store;
            defer result.decref(layout_cache, ops);

            try std.testing.expect(result.layout.tag == .scalar);

            // With numeric literal constraints, integer literals may default to Dec instead of Int
            // Accept either int or Dec (frac) layout
            const actual_value: i128 = switch (result.layout.data.scalar.tag) {
                .int => result.asI128(),
                .frac => blk: {
                    try std.testing.expect(result.layout.data.scalar.data.frac == .dec);
                    const dec_value = result.asDec();
                    // Dec stores values scaled by 10^18, divide to get the integer part
                    break :blk @divTrunc(dec_value.num, builtins.dec.RocDec.one_point_zero_i128);
                },
                else => unreachable,
            };

            try std.testing.expectEqual(case.expected, actual_value);
        }

        try std.testing.expectEqual(@as(usize, 0), interpreter.bindings.items.len);
    }
}
