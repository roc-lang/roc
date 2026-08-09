//! Unit tests for the Roc emitter
//!
//! These tests verify that the emitter correctly converts CIR expressions
//! to valid Roc source code using manually constructed CIR nodes.

const std = @import("std");
const Allocator = std.mem.Allocator;
const base = @import("base");

const Emitter = @import("../RocEmitter.zig");
const ModuleEnv = @import("../ModuleEnv.zig");
const CIR = @import("../CIR.zig");

const testing = std.testing;
const test_allocator = testing.allocator;

fn createTestEnv(allocator: std.mem.Allocator, source: []const u8) Allocator.Error!*ModuleEnv {
    const module_env = try allocator.create(ModuleEnv);
    module_env.* = try ModuleEnv.init(allocator, source);
    return module_env;
}

fn destroyTestEnv(allocator: std.mem.Allocator, module_env: *ModuleEnv) void {
    module_env.deinit();
    allocator.destroy(module_env);
}

fn addLocalLookup(module_env: *ModuleEnv, name: []const u8) Allocator.Error!CIR.Expr.Idx {
    const ident = try module_env.insertIdent(base.Ident.for_text(name));
    const pattern = try module_env.store.addPattern(.{
        .assign = .{ .ident = ident },
    }, base.Region.zero());
    return module_env.store.addExpr(.{
        .e_lookup_local = .{ .pattern_idx = pattern },
    }, base.Region.zero());
}

fn addIntExpr(module_env: *ModuleEnv, value: i128) Allocator.Error!CIR.Expr.Idx {
    return module_env.store.addExpr(.{
        .e_num = .{
            .value = .{ .bytes = @bitCast(value), .kind = .i128 },
            .kind = .i64,
        },
    }, base.Region.zero());
}

const TestFieldAccessSegment = struct {
    name: []const u8,
    mode: CIR.Expr.FieldAccessMode,
};

fn addFieldAccessPath(
    module_env: *ModuleEnv,
    receiver: CIR.Expr.Idx,
    path_segments: []const TestFieldAccessSegment,
) Allocator.Error!CIR.Expr.Idx {
    std.debug.assert(path_segments.len > 0);

    const path_builder = try module_env.startFieldAccessPath(@intCast(path_segments.len));
    errdefer module_env.rollbackFieldAccessPath(path_builder);

    for (path_segments) |path_segment| {
        const ident = try module_env.insertIdent(base.Ident.for_text(path_segment.name));
        _ = module_env.appendFieldAccessPathSegmentAssumeCapacity(path_builder, .{
            .name = ident,
            .mode = path_segment.mode,
        }, base.Region.zero());
    }
    const segments = module_env.finishFieldAccessPath(path_builder);

    return module_env.addExpr(.{
        .e_field_access = .{
            .receiver = receiver,
            .segments = segments,
        },
    }, base.Region.zero());
}

fn addCall(
    module_env: *ModuleEnv,
    func: CIR.Expr.Idx,
    args: []const CIR.Expr.Idx,
) Allocator.Error!CIR.Expr.Idx {
    const args_start = module_env.store.scratchExprTop();
    for (args) |arg| try module_env.store.addScratchExpr(arg);
    const args_span = try module_env.store.exprSpanFrom(args_start);
    return module_env.addExpr(.{ .e_call = .{
        .func = func,
        .args = args_span,
        .called_via = .apply,
    } }, base.Region.zero());
}

// Basic expression tests

test "emit integer literal" {
    const module_env = try createTestEnv(test_allocator, "42");
    defer destroyTestEnv(test_allocator, module_env);

    var emitter = Emitter.init(test_allocator, module_env);
    defer emitter.deinit();

    const int_value = CIR.IntValue{
        .bytes = @bitCast(@as(i128, 42)),
        .kind = .i128,
    };
    const expr_idx = try module_env.store.addExpr(.{
        .e_num = .{ .value = int_value, .kind = .i64 },
    }, base.Region.zero());

    try emitter.emitExpr(expr_idx);
    try testing.expectEqualStrings("42", emitter.getOutput());
}

test "emit negative integer" {
    const module_env = try createTestEnv(test_allocator, "-123");
    defer destroyTestEnv(test_allocator, module_env);

    var emitter = Emitter.init(test_allocator, module_env);
    defer emitter.deinit();

    const int_value = CIR.IntValue{
        .bytes = @bitCast(@as(i128, -123)),
        .kind = .i128,
    };
    const expr_idx = try module_env.store.addExpr(.{
        .e_num = .{ .value = int_value, .kind = .i64 },
    }, base.Region.zero());

    try emitter.emitExpr(expr_idx);
    try testing.expectEqualStrings("-123", emitter.getOutput());
}

test "emit small decimal preserves its exact fractional scale" {
    const module_env = try createTestEnv(test_allocator, "");
    defer destroyTestEnv(test_allocator, module_env);

    var emitter = Emitter.init(test_allocator, module_env);
    defer emitter.deinit();

    const cases = [_]struct {
        value: CIR.SmallDecValue,
        expected: []const u8,
    }{
        .{ .value = .{ .numerator = 102, .denominator_power_of_ten = 2 }, .expected = "1.02" },
        .{ .value = .{ .numerator = -15, .denominator_power_of_ten = 6 }, .expected = "-0.000015" },
        .{ .value = .{ .numerator = 1, .denominator_power_of_ten = 10 }, .expected = "0.0000000001" },
        .{ .value = .{ .numerator = -32768, .denominator_power_of_ten = 0 }, .expected = "-32768" },
    };

    for (cases) |case| {
        const expr_idx = try module_env.store.addExpr(.{
            .e_dec_small = .{ .value = case.value, .has_suffix = false },
        }, base.Region.zero());
        emitter.reset();
        try emitter.emitExpr(expr_idx);
        try testing.expectEqualStrings(case.expected, emitter.getOutput());
    }
}

test "emit fixed-point decimal preserves a negative fractional sign" {
    const module_env = try createTestEnv(test_allocator, "");
    defer destroyTestEnv(test_allocator, module_env);

    var emitter = Emitter.init(test_allocator, module_env);
    defer emitter.deinit();

    const scaled_value: i128 = -123_456_789_012_345_678;
    const dec_expr = try module_env.store.addExpr(.{
        .e_dec = .{
            .value = .{ .num = scaled_value },
            .has_suffix = false,
        },
    }, base.Region.zero());

    try emitter.emitExpr(dec_expr);
    try testing.expectEqualStrings("-0.123456789012345678", emitter.getOutput());

    const dec_ident = try module_env.insertIdent(base.Ident.for_text("Dec"));
    const typed_expr = try module_env.store.addExpr(.{
        .e_typed_frac = .{
            .value = .{ .bytes = @bitCast(scaled_value), .kind = .i128 },
            .type_name = dec_ident,
        },
    }, base.Region.zero());

    emitter.reset();
    try emitter.emitExpr(typed_expr);
    try testing.expectEqualStrings("-0.123456789012345678.Dec", emitter.getOutput());
}

test "emit string literal escapes decoded contents" {
    const module_env = try createTestEnv(test_allocator, "");
    defer destroyTestEnv(test_allocator, module_env);

    var emitter = Emitter.init(test_allocator, module_env);
    defer emitter.deinit();

    const literal = try module_env.insertString("a\"b\\c\n$d\x00");
    const expr_idx = try module_env.store.addExpr(.{
        .e_str_segment = .{ .literal = literal },
    }, base.Region.zero());

    try emitter.emitExpr(expr_idx);
    try testing.expectEqualStrings("\"a\\\"b\\\\c\\n\\$d\\u(0)\"", emitter.getOutput());
}

test "emit empty record" {
    const module_env = try createTestEnv(test_allocator, "{}");
    defer destroyTestEnv(test_allocator, module_env);

    var emitter = Emitter.init(test_allocator, module_env);
    defer emitter.deinit();

    const expr_idx = try module_env.store.addExpr(.{
        .e_empty_record = .{},
    }, base.Region.zero());

    try emitter.emitExpr(expr_idx);
    try testing.expectEqualStrings("{}", emitter.getOutput());
}

test "emit expression with lexicographic records sorts every nesting level" {
    const module_env = try createTestEnv(test_allocator, "{ z: 0, inner: { b: 2, a: 1 } }");
    defer destroyTestEnv(test_allocator, module_env);

    const zero = try addIntExpr(module_env, 0);
    const one = try addIntExpr(module_env, 1);
    const two = try addIntExpr(module_env, 2);
    const a = try module_env.insertIdent(base.Ident.for_text("a"));
    const b = try module_env.insertIdent(base.Ident.for_text("b"));
    const inner = try module_env.insertIdent(base.Ident.for_text("inner"));
    const z = try module_env.insertIdent(base.Ident.for_text("z"));

    const inner_start = module_env.store.scratch.?.record_fields.top();
    const b_field = try module_env.addRecordField(.{ .name = b, .value = two }, base.Region.zero());
    try module_env.store.addScratch("record_fields", b_field);
    const a_field = try module_env.addRecordField(.{ .name = a, .value = one }, base.Region.zero());
    try module_env.store.addScratch("record_fields", a_field);
    const inner_fields = try module_env.store.recordFieldSpanFrom(inner_start);
    const inner_record = try module_env.addExpr(.{ .e_record = .{ .fields = inner_fields, .ext = null } }, base.Region.zero());

    const outer_start = module_env.store.scratch.?.record_fields.top();
    const z_field = try module_env.addRecordField(.{ .name = z, .value = zero }, base.Region.zero());
    try module_env.store.addScratch("record_fields", z_field);
    const inner_field = try module_env.addRecordField(.{ .name = inner, .value = inner_record }, base.Region.zero());
    try module_env.store.addScratch("record_fields", inner_field);
    const outer_fields = try module_env.store.recordFieldSpanFrom(outer_start);
    const outer_record = try module_env.addExpr(.{ .e_record = .{ .fields = outer_fields, .ext = null } }, base.Region.zero());

    var emitter = Emitter.init(test_allocator, module_env);
    defer emitter.deinit();

    try emitter.emitExpr(outer_record);
    try testing.expectEqualStrings("{ z: 0, inner: { b: 2, a: 1 } }", emitter.getOutput());

    emitter.reset();
    try emitter.emitExprWithLexicographicRecords(outer_record);
    try testing.expectEqualStrings("{ inner: { a: 1, b: 2 }, z: 0 }", emitter.getOutput());
}

test "emit optional field access path" {
    const module_env = try createTestEnv(test_allocator, "record.?outer.?inner");
    defer destroyTestEnv(test_allocator, module_env);

    const receiver = try addLocalLookup(module_env, "record");
    const expr_idx = try addFieldAccessPath(module_env, receiver, &.{
        .{ .name = "outer", .mode = .optional },
        .{ .name = "inner", .mode = .optional },
    });

    var emitter = Emitter.init(test_allocator, module_env);
    defer emitter.deinit();
    try emitter.emitExpr(expr_idx);
    try testing.expectEqualStrings("record.?outer.?inner", emitter.getOutput());
}

test "emit required field access path" {
    const module_env = try createTestEnv(test_allocator, "record.outer.inner");
    defer destroyTestEnv(test_allocator, module_env);

    const receiver = try addLocalLookup(module_env, "record");
    const expr_idx = try addFieldAccessPath(module_env, receiver, &.{
        .{ .name = "outer", .mode = .required },
        .{ .name = "inner", .mode = .required },
    });

    var emitter = Emitter.init(test_allocator, module_env);
    defer emitter.deinit();
    try emitter.emitExpr(expr_idx);
    try testing.expectEqualStrings("record.outer.inner", emitter.getOutput());
}

test "emit mixed required and optional field access path" {
    const module_env = try createTestEnv(test_allocator, "record.required.?optional.required_after");
    defer destroyTestEnv(test_allocator, module_env);

    const receiver = try addLocalLookup(module_env, "record");
    const expr_idx = try addFieldAccessPath(module_env, receiver, &.{
        .{ .name = "required", .mode = .required },
        .{ .name = "optional", .mode = .optional },
        .{ .name = "required_after", .mode = .required },
    });

    var emitter = Emitter.init(test_allocator, module_env);
    defer emitter.deinit();
    try emitter.emitExpr(expr_idx);
    try testing.expectEqualStrings("record.required.?optional.required_after", emitter.getOutput());
}

test "emit parentheses between distinct optional field access paths" {
    const module_env = try createTestEnv(test_allocator, "(record.?outer).?inner");
    defer destroyTestEnv(test_allocator, module_env);

    const receiver = try addLocalLookup(module_env, "record");
    const outer_access = try addFieldAccessPath(module_env, receiver, &.{.{ .name = "outer", .mode = .optional }});
    const expr_idx = try addFieldAccessPath(module_env, outer_access, &.{.{ .name = "inner", .mode = .optional }});

    var emitter = Emitter.init(test_allocator, module_env);
    defer emitter.deinit();
    try emitter.emitExpr(expr_idx);
    try testing.expectEqualStrings("(record.?outer).?inner", emitter.getOutput());
}

test "emit parentheses between a required access and an outer optional-containing path" {
    const module_env = try createTestEnv(test_allocator, "(record.required).?optional");
    defer destroyTestEnv(test_allocator, module_env);

    const receiver = try addLocalLookup(module_env, "record");
    const required = try addFieldAccessPath(module_env, receiver, &.{.{ .name = "required", .mode = .required }});
    const expr_idx = try addFieldAccessPath(module_env, required, &.{.{ .name = "optional", .mode = .optional }});

    var emitter = Emitter.init(test_allocator, module_env);
    defer emitter.deinit();
    try emitter.emitExpr(expr_idx);
    try testing.expectEqualStrings("(record.required).?optional", emitter.getOutput());
}

test "emit parentheses between an optional-containing path and an outer required access" {
    const module_env = try createTestEnv(test_allocator, "(record.?optional).required");
    defer destroyTestEnv(test_allocator, module_env);

    const receiver = try addLocalLookup(module_env, "record");
    const optional = try addFieldAccessPath(module_env, receiver, &.{.{ .name = "optional", .mode = .optional }});
    const expr_idx = try addFieldAccessPath(module_env, optional, &.{.{ .name = "required", .mode = .required }});

    var emitter = Emitter.init(test_allocator, module_env);
    defer emitter.deinit();
    try emitter.emitExpr(expr_idx);
    try testing.expectEqualStrings("(record.?optional).required", emitter.getOutput());
}

test "emit optional access parenthesizes a numeric receiver" {
    const module_env = try createTestEnv(test_allocator, "(1).?field");
    defer destroyTestEnv(test_allocator, module_env);

    const one = try module_env.store.addExpr(.{
        .e_num = .{
            .value = .{ .bytes = @bitCast(@as(i128, 1)), .kind = .i128 },
            .kind = .i64,
        },
    }, base.Region.zero());
    const expr_idx = try addFieldAccessPath(module_env, one, &.{.{ .name = "field", .mode = .optional }});

    var emitter = Emitter.init(test_allocator, module_env);
    defer emitter.deinit();
    try emitter.emitExpr(expr_idx);
    try testing.expectEqualStrings("(1).?field", emitter.getOutput());
}

test "emit required access parenthesizes a numeric receiver" {
    const module_env = try createTestEnv(test_allocator, "(1).field");
    defer destroyTestEnv(test_allocator, module_env);

    const one = try module_env.store.addExpr(.{
        .e_num = .{
            .value = .{ .bytes = @bitCast(@as(i128, 1)), .kind = .i128 },
            .kind = .i64,
        },
    }, base.Region.zero());
    const expr_idx = try addFieldAccessPath(module_env, one, &.{.{ .name = "field", .mode = .required }});

    var emitter = Emitter.init(test_allocator, module_env);
    defer emitter.deinit();
    try emitter.emitExpr(expr_idx);
    try testing.expectEqualStrings("(1).field", emitter.getOutput());
}

test "emit optional access parenthesizes a binary receiver" {
    const module_env = try createTestEnv(test_allocator, "(1 + 2).?field");
    defer destroyTestEnv(test_allocator, module_env);

    const one = try module_env.store.addExpr(.{
        .e_num = .{
            .value = .{ .bytes = @bitCast(@as(i128, 1)), .kind = .i128 },
            .kind = .i64,
        },
    }, base.Region.zero());
    const two = try module_env.store.addExpr(.{
        .e_num = .{
            .value = .{ .bytes = @bitCast(@as(i128, 2)), .kind = .i128 },
            .kind = .i64,
        },
    }, base.Region.zero());
    const sum = try module_env.store.addExpr(.{
        .e_binop = CIR.Expr.Binop.init(.add, one, two),
    }, base.Region.zero());
    const expr_idx = try addFieldAccessPath(module_env, sum, &.{.{ .name = "field", .mode = .optional }});

    var emitter = Emitter.init(test_allocator, module_env);
    defer emitter.deinit();
    try emitter.emitExpr(expr_idx);
    try testing.expectEqualStrings("(1 + 2).?field", emitter.getOutput());
}

test "emit optional access parenthesizes a unary receiver" {
    const module_env = try createTestEnv(test_allocator, "(-value).?field");
    defer destroyTestEnv(test_allocator, module_env);

    const value = try addLocalLookup(module_env, "value");
    const negated = try module_env.store.addExpr(.{
        .e_unary_minus = CIR.Expr.UnaryMinus.init(value),
    }, base.Region.zero());
    const expr_idx = try addFieldAccessPath(module_env, negated, &.{.{ .name = "field", .mode = .optional }});

    var emitter = Emitter.init(test_allocator, module_env);
    defer emitter.deinit();
    try emitter.emitExpr(expr_idx);
    try testing.expectEqualStrings("(-value).?field", emitter.getOutput());
}

test "emit optional access parenthesizes an if receiver" {
    const module_env = try createTestEnv(test_allocator, "(if (condition) then_value else else_value).?field");
    defer destroyTestEnv(test_allocator, module_env);

    const condition = try addLocalLookup(module_env, "condition");
    const then_value = try addLocalLookup(module_env, "then_value");
    const else_value = try addLocalLookup(module_env, "else_value");

    const branches_start = module_env.store.scratchIfBranchTop();
    const branch = try module_env.addIfBranch(.{
        .cond = condition,
        .body = then_value,
    }, base.Region.zero());
    try module_env.store.addScratchIfBranch(branch);
    const branches = try module_env.store.ifBranchSpanFrom(branches_start);
    const if_expr = try module_env.store.addExpr(.{
        .e_if = .{
            .branches = branches,
            .final_else = else_value,
            .warn_unused_branches = true,
        },
    }, base.Region.zero());
    const expr_idx = try addFieldAccessPath(module_env, if_expr, &.{.{ .name = "field", .mode = .optional }});

    var emitter = Emitter.init(test_allocator, module_env);
    defer emitter.deinit();
    try emitter.emitExpr(expr_idx);
    try testing.expectEqualStrings("(if (condition) then_value else else_value).?field", emitter.getOutput());
}

test "emit optional access parenthesizes a lambda receiver" {
    const module_env = try createTestEnv(test_allocator, "(|value| value).?field");
    defer destroyTestEnv(test_allocator, module_env);

    const value_ident = try module_env.insertIdent(base.Ident.for_text("value"));
    const value_pattern = try module_env.store.addPattern(.{
        .assign = .{ .ident = value_ident },
    }, base.Region.zero());
    const body = try module_env.store.addExpr(.{
        .e_lookup_local = .{ .pattern_idx = value_pattern },
    }, base.Region.zero());

    const args_start = module_env.store.scratchPatternTop();
    try module_env.store.addScratchPattern(value_pattern);
    const args = try module_env.store.patternSpanFrom(args_start);
    const lambda = try module_env.store.addExpr(.{
        .e_lambda = .{
            .args = args,
            .body = body,
        },
    }, base.Region.zero());
    const expr_idx = try addFieldAccessPath(module_env, lambda, &.{.{ .name = "field", .mode = .optional }});

    var emitter = Emitter.init(test_allocator, module_env);
    defer emitter.deinit();
    try emitter.emitExpr(expr_idx);
    try testing.expectEqualStrings("(|value| value).?field", emitter.getOutput());
}

test "emit empty list" {
    const module_env = try createTestEnv(test_allocator, "[]");
    defer destroyTestEnv(test_allocator, module_env);

    var emitter = Emitter.init(test_allocator, module_env);
    defer emitter.deinit();

    const expr_idx = try module_env.store.addExpr(.{
        .e_empty_list = .{},
    }, base.Region.zero());

    try emitter.emitExpr(expr_idx);
    try testing.expectEqualStrings("[]", emitter.getOutput());
}

test "emit identity lambda" {
    const module_env = try createTestEnv(test_allocator, "|x| x");
    defer destroyTestEnv(test_allocator, module_env);

    var emitter = Emitter.init(test_allocator, module_env);
    defer emitter.deinit();

    // Create pattern for 'x'
    const x_ident = try module_env.insertIdent(base.Ident.for_text("x"));
    const x_pattern_idx = try module_env.store.addPattern(.{
        .assign = .{ .ident = x_ident },
    }, base.Region.zero());

    // Create lookup expression for body
    const body_idx = try module_env.store.addExpr(.{
        .e_lookup_local = .{ .pattern_idx = x_pattern_idx },
    }, base.Region.zero());

    // Create lambda expression using scratch system
    const start = module_env.store.scratchPatternTop();
    try module_env.store.addScratchPattern(x_pattern_idx);
    const args_span = try module_env.store.patternSpanFrom(start);

    const lambda_idx = try module_env.store.addExpr(.{
        .e_lambda = .{ .args = args_span, .body = body_idx },
    }, base.Region.zero());

    try emitter.emitExpr(lambda_idx);
    try testing.expectEqualStrings("|x| x", emitter.getOutput());
}

test "emit tag with no arguments" {
    const module_env = try createTestEnv(test_allocator, "True");
    defer destroyTestEnv(test_allocator, module_env);

    var emitter = Emitter.init(test_allocator, module_env);
    defer emitter.deinit();

    const true_ident = try module_env.insertIdent(base.Ident.for_text("True"));
    const expr_idx = try module_env.store.addExpr(.{
        .e_zero_argument_tag = .{
            .closure_name = true_ident,
            .variant_var = undefined, // not read by emitter
            .ext_var = undefined, // not read by emitter
            .name = true_ident,
        },
    }, base.Region.zero());

    try emitter.emitExpr(expr_idx);
    try testing.expectEqualStrings("True", emitter.getOutput());
}

test "emit list with elements" {
    const module_env = try createTestEnv(test_allocator, "[1, 2]");
    defer destroyTestEnv(test_allocator, module_env);

    var emitter = Emitter.init(test_allocator, module_env);
    defer emitter.deinit();

    // Create element expressions
    const int_value_1 = CIR.IntValue{
        .bytes = @bitCast(@as(i128, 1)),
        .kind = .i128,
    };
    const elem1_idx = try module_env.store.addExpr(.{
        .e_num = .{ .value = int_value_1, .kind = .i64 },
    }, base.Region.zero());

    const int_value_2 = CIR.IntValue{
        .bytes = @bitCast(@as(i128, 2)),
        .kind = .i128,
    };
    const elem2_idx = try module_env.store.addExpr(.{
        .e_num = .{ .value = int_value_2, .kind = .i64 },
    }, base.Region.zero());

    // Create list using scratch system
    const start = module_env.store.scratchExprTop();
    try module_env.store.addScratchExpr(elem1_idx);
    try module_env.store.addScratchExpr(elem2_idx);
    const elems_span = try module_env.store.exprSpanFrom(start);

    const list_idx = try module_env.store.addExpr(.{
        .e_list = .{ .elems = elems_span },
    }, base.Region.zero());

    try emitter.emitExpr(list_idx);
    try testing.expectEqualStrings("[1, 2]", emitter.getOutput());
}

test "emit function application" {
    const module_env = try createTestEnv(test_allocator, "f(42)");
    defer destroyTestEnv(test_allocator, module_env);

    var emitter = Emitter.init(test_allocator, module_env);
    defer emitter.deinit();

    // Create pattern for 'f' (the function we're calling)
    const f_ident = try module_env.insertIdent(base.Ident.for_text("f"));
    const f_pattern_idx = try module_env.store.addPattern(.{
        .assign = .{ .ident = f_ident },
    }, base.Region.zero());

    // Create function expression (lookup of f)
    const func_idx = try module_env.store.addExpr(.{
        .e_lookup_local = .{ .pattern_idx = f_pattern_idx },
    }, base.Region.zero());

    // Create argument expression
    const int_value = CIR.IntValue{
        .bytes = @bitCast(@as(i128, 42)),
        .kind = .i128,
    };
    const arg_idx = try module_env.store.addExpr(.{
        .e_num = .{ .value = int_value, .kind = .i64 },
    }, base.Region.zero());

    // Create call expression using scratch system
    const start = module_env.store.scratchExprTop();
    try module_env.store.addScratchExpr(arg_idx);
    const args_span = try module_env.store.exprSpanFrom(start);

    const call_idx = try module_env.store.addExpr(.{
        .e_call = .{
            .func = func_idx,
            .args = args_span,
            .called_via = .apply,
        },
    }, base.Region.zero());

    try emitter.emitExpr(call_idx);
    try testing.expectEqualStrings("f(42)", emitter.getOutput());
}

test "emit required field value application preserves the field-call boundary" {
    const module_env = try createTestEnv(test_allocator, "(record.callback)(42)");
    defer destroyTestEnv(test_allocator, module_env);

    const receiver = try addLocalLookup(module_env, "record");
    const field = try addFieldAccessPath(module_env, receiver, &.{.{
        .name = "callback",
        .mode = .required,
    }});
    const arg = try module_env.addExpr(.{ .e_num = .{
        .value = .{ .bytes = @bitCast(@as(i128, 42)), .kind = .i128 },
        .kind = .i64,
    } }, base.Region.zero());
    const call = try addCall(module_env, field, &.{arg});

    var emitter = Emitter.init(test_allocator, module_env);
    defer emitter.deinit();
    try emitter.emitExpr(call);
    try testing.expectEqualStrings("(record.callback)(42)", emitter.getOutput());
}

test "emit optional field value application preserves the query boundary" {
    const module_env = try createTestEnv(test_allocator, "(record.?callback)(42)");
    defer destroyTestEnv(test_allocator, module_env);

    const receiver = try addLocalLookup(module_env, "record");
    const field = try addFieldAccessPath(module_env, receiver, &.{.{
        .name = "callback",
        .mode = .optional,
    }});
    const arg = try module_env.addExpr(.{ .e_num = .{
        .value = .{ .bytes = @bitCast(@as(i128, 42)), .kind = .i128 },
        .kind = .i64,
    } }, base.Region.zero());
    const call = try addCall(module_env, field, &.{arg});

    var emitter = Emitter.init(test_allocator, module_env);
    defer emitter.deinit();
    try emitter.emitExpr(call);
    try testing.expectEqualStrings("(record.?callback)(42)", emitter.getOutput());
}

test "emit unary minus over negative typed numeral parenthesizes receiver" {
    // A negative `e_typed_num_from_numeral` emits as one unit `-5.U8`; under a
    // unary minus the receiver must be parenthesized so the leading `-` does not
    // fuse into `--`. The whole receiver (type suffix included) stays inside the
    // parens, so it reparses to the same single typed-numeral node: `-(-5.U8)`.
    const module_env = try createTestEnv(test_allocator, "-(-5.U8)");
    defer destroyTestEnv(test_allocator, module_env);

    const u8_ident = try module_env.insertIdent(base.Ident.for_text("U8"));
    const typed_idx = try module_env.store.addExpr(.{
        .e_typed_num_from_numeral = .{ .type_name = u8_ident },
    }, base.Region.zero());

    try module_env.recordNumeralLiteral(
        ModuleEnv.nodeIdxFrom(typed_idx),
        &[_]u8{5}, // before-decimal base-256 digits
        &[_]u8{}, // after-decimal digits
        0, // after_decimal_digit_count
        true, // is_negative
        false, // is_fractional
        false, // had_decimal_point
        true, // is_materialized
    );

    const unary_idx = try module_env.store.addExpr(.{
        .e_unary_minus = CIR.Expr.UnaryMinus.init(typed_idx),
    }, base.Region.zero());

    var emitter = Emitter.init(test_allocator, module_env);
    defer emitter.deinit();
    try emitter.emitExpr(unary_idx);
    try testing.expectEqualStrings("-(-5.U8)", emitter.getOutput());
}
