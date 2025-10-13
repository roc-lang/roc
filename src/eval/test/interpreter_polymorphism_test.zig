//! Polymorphism tests for Interpreter focused on closures without captures (Milestone 1).
//! Each test starts with Roc source (multiline Zig string with `\\`), parses + canonicalizes
//! with early diagnostics, evaluates with Interpreter, and renders Roc output.

const std = @import("std");
const helpers = @import("helpers.zig");
const Interpreter = @import("../interpreter.zig").Interpreter;
const can = @import("can");
const RocOps = @import("builtins").host_abi.RocOps;
const RocAlloc = @import("builtins").host_abi.RocAlloc;
const RocDealloc = @import("builtins").host_abi.RocDealloc;
const RocRealloc = @import("builtins").host_abi.RocRealloc;
const RocDbg = @import("builtins").host_abi.RocDbg;
const RocExpectFailed = @import("builtins").host_abi.RocExpectFailed;
const RocCrashed = @import("builtins").host_abi.RocCrashed;

const TestHost = struct { allocator: std.mem.Allocator };

fn testRocAlloc(alloc_args: *RocAlloc, env: *anyopaque) callconv(.C) void {
    const host: *TestHost = @ptrCast(@alignCast(env));
    const align_enum = std.mem.Alignment.fromByteUnits(@as(usize, @intCast(alloc_args.alignment)));
    const size_storage_bytes = @max(alloc_args.alignment, @alignOf(usize));
    const total_size = alloc_args.length + size_storage_bytes;
    const result = host.allocator.rawAlloc(total_size, align_enum, @returnAddress());
    const base_ptr = result orelse {
        @panic("Out of memory during testRocAlloc");
    };
    const size_ptr: *usize = @ptrFromInt(@intFromPtr(base_ptr) + size_storage_bytes - @sizeOf(usize));
    size_ptr.* = total_size;
    alloc_args.answer = @ptrFromInt(@intFromPtr(base_ptr) + size_storage_bytes);
}

fn testRocDealloc(dealloc_args: *RocDealloc, env: *anyopaque) callconv(.C) void {
    const host: *TestHost = @ptrCast(@alignCast(env));
    const size_storage_bytes = @max(dealloc_args.alignment, @alignOf(usize));
    const size_ptr: *const usize = @ptrFromInt(@intFromPtr(dealloc_args.ptr) - @sizeOf(usize));
    const total_size = size_ptr.*;
    const base_ptr: [*]u8 = @ptrFromInt(@intFromPtr(dealloc_args.ptr) - size_storage_bytes);
    const log2_align = std.math.log2_int(u32, @intCast(dealloc_args.alignment));
    const align_enum: std.mem.Alignment = @enumFromInt(log2_align);
    const slice = @as([*]u8, @ptrCast(base_ptr))[0..total_size];
    host.allocator.rawFree(slice, align_enum, @returnAddress());
}

fn testRocRealloc(realloc_args: *RocRealloc, env: *anyopaque) callconv(.C) void {
    const host: *TestHost = @ptrCast(@alignCast(env));
    const size_storage_bytes = @max(realloc_args.alignment, @alignOf(usize));
    const old_size_ptr: *const usize = @ptrFromInt(@intFromPtr(realloc_args.answer) - @sizeOf(usize));
    const old_total_size = old_size_ptr.*;
    const old_base_ptr: [*]u8 = @ptrFromInt(@intFromPtr(realloc_args.answer) - size_storage_bytes);
    const new_total_size = realloc_args.new_length + size_storage_bytes;
    const old_slice = @as([*]u8, @ptrCast(old_base_ptr))[0..old_total_size];
    const new_slice = host.allocator.realloc(old_slice, new_total_size) catch {
        @panic("Out of memory during testRocRealloc");
    };
    const new_size_ptr: *usize = @ptrFromInt(@intFromPtr(new_slice.ptr) + size_storage_bytes - @sizeOf(usize));
    new_size_ptr.* = new_total_size;
    realloc_args.answer = @ptrFromInt(@intFromPtr(new_slice.ptr) + size_storage_bytes);
}

fn testRocDbg(_: *const RocDbg, _: *anyopaque) callconv(.C) void {}
fn testRocExpectFailed(_: *const RocExpectFailed, _: *anyopaque) callconv(.C) void {}
fn testRocCrashed(_: *const RocCrashed, _: *anyopaque) callconv(.C) void {
    // Polymorphism tests never trigger crashes; retain the callback to satisfy RocOps.
}

fn makeOps(host: *TestHost) RocOps {
    return RocOps{
        .env = @ptrCast(host),
        .roc_alloc = testRocAlloc,
        .roc_dealloc = testRocDealloc,
        .roc_realloc = testRocRealloc,
        .roc_dbg = testRocDbg,
        .roc_expect_failed = testRocExpectFailed,
        .roc_crashed = testRocCrashed,
        .host_fns = undefined,
    };
}

test "interpreter poly: return a function then call (int)" {
    const roc_src =
        \\(|_| (|x| x))(0)(42)
    ;

    const resources = try helpers.parseAndCanonicalizeExpr(std.testing.allocator, roc_src);
    defer helpers.cleanupParseAndCanonical(std.testing.allocator, resources);

    var interp2 = try Interpreter.init(std.testing.allocator, resources.module_env, resources.bool_stmt, resources.bool_module.env, null);
    defer interp2.deinit();

    var host = TestHost{ .allocator = std.testing.allocator };
    var ops = makeOps(&host);
    const result = try interp2.evalMinimal(resources.expr_idx, &ops);
    const ct_var_ok = can.ModuleEnv.varFrom(resources.expr_idx);
    const rt_var_ok = try interp2.translateTypeVar(resources.module_env, ct_var_ok);
    const rendered = try interp2.renderValueRocWithType(result, rt_var_ok);
    defer std.testing.allocator.free(rendered);
    try std.testing.expectEqualStrings("42", rendered);
}

test "interpreter poly: return a function then call (string)" {
    const roc_src =
        \\(|_| (|x| x))(0)("hi")
    ;

    const resources = try helpers.parseAndCanonicalizeExpr(std.testing.allocator, roc_src);
    defer helpers.cleanupParseAndCanonical(std.testing.allocator, resources);

    var interp2 = try Interpreter.init(std.testing.allocator, resources.module_env, resources.bool_stmt, resources.bool_module.env, null);
    defer interp2.deinit();

    var host = TestHost{ .allocator = std.testing.allocator };
    var ops = makeOps(&host);
    const result = try interp2.evalMinimal(resources.expr_idx, &ops);
    const ct_var_point = can.ModuleEnv.varFrom(resources.expr_idx);
    const rt_var_point = try interp2.translateTypeVar(resources.module_env, ct_var_point);
    const rendered = try interp2.renderValueRocWithType(result, rt_var_point);
    defer std.testing.allocator.free(rendered);
    const expected =
        \\"hi"
    ;
    try std.testing.expectEqualStrings(expected, rendered);
}

test "interpreter captures (monomorphic): adder" {
    const roc_src =
        \\(|n| (|x| n + x))(1)(41)
    ;

    const resources = try helpers.parseAndCanonicalizeExpr(std.testing.allocator, roc_src);
    defer helpers.cleanupParseAndCanonical(std.testing.allocator, resources);

    var interp2 = try Interpreter.init(std.testing.allocator, resources.module_env, resources.bool_stmt, resources.bool_module.env, null);
    defer interp2.deinit();

    var host = TestHost{ .allocator = std.testing.allocator };
    var ops = makeOps(&host);
    const result = try interp2.evalMinimal(resources.expr_idx, &ops);
    const ct_var_ok = can.ModuleEnv.varFrom(resources.expr_idx);
    const rt_var_ok = try interp2.translateTypeVar(resources.module_env, ct_var_ok);
    const rendered = try interp2.renderValueRocWithType(result, rt_var_ok);
    defer std.testing.allocator.free(rendered);
    try std.testing.expectEqualStrings("42", rendered);
}

test "interpreter captures (monomorphic): constant function" {
    const roc_src =
        \\(|x| (|_| x))("hi")(0)
    ;

    const resources = try helpers.parseAndCanonicalizeExpr(std.testing.allocator, roc_src);
    defer helpers.cleanupParseAndCanonical(std.testing.allocator, resources);

    var interp2 = try Interpreter.init(std.testing.allocator, resources.module_env, resources.bool_stmt, resources.bool_module.env, null);
    defer interp2.deinit();

    var host = TestHost{ .allocator = std.testing.allocator };
    var ops = makeOps(&host);
    const result = try interp2.evalMinimal(resources.expr_idx, &ops);
    const ct_var_point = can.ModuleEnv.varFrom(resources.expr_idx);
    const rt_var_point = try interp2.translateTypeVar(resources.module_env, ct_var_point);
    const rendered = try interp2.renderValueRocWithType(result, rt_var_point);
    defer std.testing.allocator.free(rendered);
    const expected =
        \\"hi"
    ;
    try std.testing.expectEqualStrings(expected, rendered);
}

test "interpreter captures (polymorphic): capture id and apply to int" {
    const roc_src =
        \\((|id| (|x| id(x)))(|y| y))(41)
    ;

    const resources = try helpers.parseAndCanonicalizeExpr(std.testing.allocator, roc_src);
    defer helpers.cleanupParseAndCanonical(std.testing.allocator, resources);

    var interp2 = try Interpreter.init(std.testing.allocator, resources.module_env, resources.bool_stmt, resources.bool_module.env, null);
    defer interp2.deinit();

    var host = TestHost{ .allocator = std.testing.allocator };
    var ops = makeOps(&host);
    const result = try interp2.evalMinimal(resources.expr_idx, &ops);
    const ct_var_ok = can.ModuleEnv.varFrom(resources.expr_idx);
    const rt_var_ok = try interp2.translateTypeVar(resources.module_env, ct_var_ok);
    const rendered = try interp2.renderValueRocWithType(result, rt_var_ok);
    defer std.testing.allocator.free(rendered);
    try std.testing.expectEqualStrings("41", rendered);
}

test "interpreter captures (polymorphic): capture id and apply to string" {
    const roc_src =
        \\((|id| (|x| id(x)))(|y| y))("ok")
    ;

    const resources = try helpers.parseAndCanonicalizeExpr(std.testing.allocator, roc_src);
    defer helpers.cleanupParseAndCanonical(std.testing.allocator, resources);

    var interp2 = try Interpreter.init(std.testing.allocator, resources.module_env, resources.bool_stmt, resources.bool_module.env, null);
    defer interp2.deinit();

    var host = TestHost{ .allocator = std.testing.allocator };
    var ops = makeOps(&host);
    const result = try interp2.evalMinimal(resources.expr_idx, &ops);
    const ct_var_point = can.ModuleEnv.varFrom(resources.expr_idx);
    const rt_var_point = try interp2.translateTypeVar(resources.module_env, ct_var_point);
    const rendered = try interp2.renderValueRocWithType(result, rt_var_point);
    defer std.testing.allocator.free(rendered);
    const expected =
        \\"ok"
    ;
    try std.testing.expectEqualStrings(expected, rendered);
}

// Higher-order: pass a function and apply inside another function
test "interpreter higher-order: apply f then call with 41" {
    const roc_src =
        \\((|f| (|x| f(x)))(|n| n + 1))(41)
    ;

    const resources = try helpers.parseAndCanonicalizeExpr(std.testing.allocator, roc_src);
    defer helpers.cleanupParseAndCanonical(std.testing.allocator, resources);

    var interp2 = try Interpreter.init(std.testing.allocator, resources.module_env, resources.bool_stmt, resources.bool_module.env, null);
    defer interp2.deinit();

    var host = TestHost{ .allocator = std.testing.allocator };
    var ops = makeOps(&host);
    const result = try interp2.evalMinimal(resources.expr_idx, &ops);
    const rendered = try interp2.renderValueRoc(result);
    defer std.testing.allocator.free(rendered);
    try std.testing.expectEqualStrings("42", rendered);
}

// Higher-order: double apply f inside a function
test "interpreter higher-order: apply f twice" {
    const roc_src =
        \\((|f| (|x| f(f(x))))(|n| n + 1))(40)
    ;

    const resources = try helpers.parseAndCanonicalizeExpr(std.testing.allocator, roc_src);
    defer helpers.cleanupParseAndCanonical(std.testing.allocator, resources);

    var interp2 = try Interpreter.init(std.testing.allocator, resources.module_env, resources.bool_stmt, resources.bool_module.env, null);
    defer interp2.deinit();

    var host = TestHost{ .allocator = std.testing.allocator };
    var ops = makeOps(&host);
    const result = try interp2.evalMinimal(resources.expr_idx, &ops);
    const rendered = try interp2.renderValueRoc(result);
    defer std.testing.allocator.free(rendered);
    try std.testing.expectEqualStrings("42", rendered);
}

// Higher-order: pass a constructed closure as an argument, then apply with an int
test "interpreter higher-order: pass constructed closure and apply" {
    const roc_src =
        \\(|g| g(41))((|f| (|x| f(x)))(|y| y))
    ;

    const resources = try helpers.parseAndCanonicalizeExpr(std.testing.allocator, roc_src);
    defer helpers.cleanupParseAndCanonical(std.testing.allocator, resources);

    var interp2 = try Interpreter.init(std.testing.allocator, resources.module_env, resources.bool_stmt, resources.bool_module.env, null);
    defer interp2.deinit();

    var host = TestHost{ .allocator = std.testing.allocator };
    var ops = makeOps(&host);
    const result = try interp2.evalMinimal(resources.expr_idx, &ops);
    const rendered = try interp2.renderValueRoc(result);
    defer std.testing.allocator.free(rendered);
    try std.testing.expectEqualStrings("41", rendered);
}

// Higher-order: construct a function then pass it to a consumer and evaluate
test "interpreter higher-order: construct then pass then call" {
    const roc_src =
        \\((|make| (|z| (make(|n| n + 1))(z)))(|f| (|x| f(x))))(41)
    ;

    const resources = try helpers.parseAndCanonicalizeExpr(std.testing.allocator, roc_src);
    defer helpers.cleanupParseAndCanonical(std.testing.allocator, resources);

    var interp2 = try Interpreter.init(std.testing.allocator, resources.module_env, resources.bool_stmt, resources.bool_module.env, null);
    defer interp2.deinit();

    var host = TestHost{ .allocator = std.testing.allocator };
    var ops = makeOps(&host);
    const result = try interp2.evalMinimal(resources.expr_idx, &ops);
    const rendered = try interp2.renderValueRoc(result);
    defer std.testing.allocator.free(rendered);
    try std.testing.expectEqualStrings("42", rendered);
}

// Higher-order: compose = \f -> \g -> \x -> f(g(x)) and apply
test "interpreter higher-order: compose id with +1" {
    const roc_src =
        \\(((|f| (|g| (|x| f(g(x)))))(|n| n + 1))(|y| y))(41)
    ;

    const resources = try helpers.parseAndCanonicalizeExpr(std.testing.allocator, roc_src);
    defer helpers.cleanupParseAndCanonical(std.testing.allocator, resources);

    var interp2 = try Interpreter.init(std.testing.allocator, resources.module_env, resources.bool_stmt, resources.bool_module.env, null);
    defer interp2.deinit();

    var host = TestHost{ .allocator = std.testing.allocator };
    var ops = makeOps(&host);
    const result = try interp2.evalMinimal(resources.expr_idx, &ops);
    const rendered = try interp2.renderValueRoc(result);
    defer std.testing.allocator.free(rendered);
    try std.testing.expectEqualStrings("42", rendered);
}

// Higher-order + capture: returns polymorphic function that uses a captured increment
test "interpreter higher-order: return poly fn using captured +n" {
    const roc_src =
        \\(((|n| (|id| (|x| id(x + n))))(1))(|y| y))(41)
    ;

    const resources = try helpers.parseAndCanonicalizeExpr(std.testing.allocator, roc_src);
    defer helpers.cleanupParseAndCanonical(std.testing.allocator, resources);

    var interp2 = try Interpreter.init(std.testing.allocator, resources.module_env, resources.bool_stmt, resources.bool_module.env, null);
    defer interp2.deinit();

    var host = TestHost{ .allocator = std.testing.allocator };
    var ops = makeOps(&host);
    const result = try interp2.evalMinimal(resources.expr_idx, &ops);
    const rendered = try interp2.renderValueRoc(result);
    defer std.testing.allocator.free(rendered);
    try std.testing.expectEqualStrings("42", rendered);
}

// Recursion via block let-binding using a named recursive closure
test "interpreter recursion: simple countdown" {
    return error.SkipZigTest; // Comparison operators not yet implemented
    //     const roc_src =
    //         \\{ rec = (|n| if n == 0 { 0 } else { rec(n - 1) + 1 }) rec(2) }
    //     ;

    //     const resources = try helpers.parseAndCanonicalizeExpr(std.testing.allocator, roc_src);
    //     defer helpers.cleanupParseAndCanonical(std.testing.allocator, resources);

    //     var interp2 = try Interpreter.init(std.testing.allocator, resources.module_env, resources.bool_stmt, resources.bool_module.env, null);
    //     defer interp2.deinit();

    //     var host = TestHost{ .allocator = std.testing.allocator };
    //     var ops = makeOps(&host);
    //     const result = try interp2.evalMinimal(resources.expr_idx, &ops);
    //     const rendered = try interp2.renderValueRoc(result);
    //     defer std.testing.allocator.free(rendered);
    //     try std.testing.expectEqualStrings("2", rendered);
}

test "interpreter if: else-if chain selects middle branch" {
    return error.SkipZigTest; // Comparison operators not yet implemented
    //     const roc_src =
    //         \\{ n = 1 if n == 0 { "zero" } else if n == 1 { "one" } else { "other" } }
    //     ;

    //     const resources = try helpers.parseAndCanonicalizeExpr(std.testing.allocator, roc_src);
    //     defer helpers.cleanupParseAndCanonical(std.testing.allocator, resources);

    //     var interp2 = try Interpreter.init(std.testing.allocator, resources.module_env, resources.bool_stmt, resources.bool_module.env, null);
    //     defer interp2.deinit();

    //     var host = TestHost{ .allocator = std.testing.allocator };
    //     var ops = makeOps(&host);
    //     const result = try interp2.evalMinimal(resources.expr_idx, &ops);
    //     const rendered = try interp2.renderValueRoc(result);
    //     defer std.testing.allocator.free(rendered);
    //     const expected =
    //         \\"one"
    //     ;
    //     try std.testing.expectEqualStrings(expected, rendered);
}

test "interpreter var and reassign" {
    const roc_src =
        \\{ var x = 1 x = x + 1 x }
    ;

    const resources = try helpers.parseAndCanonicalizeExpr(std.testing.allocator, roc_src);
    defer helpers.cleanupParseAndCanonical(std.testing.allocator, resources);

    var interp2 = try Interpreter.init(std.testing.allocator, resources.module_env, resources.bool_stmt, resources.bool_module.env, null);
    defer interp2.deinit();

    var host = TestHost{ .allocator = std.testing.allocator };
    var ops = makeOps(&host);
    const result = try interp2.evalMinimal(resources.expr_idx, &ops);
    const rendered = try interp2.renderValueRoc(result);
    defer std.testing.allocator.free(rendered);
    try std.testing.expectEqualStrings("2", rendered);
}

test "interpreter logical or is short-circuiting" {
    return error.SkipZigTest; // Comparison operators not yet implemented
    //     const roc_src =
    //         \\if ((1 == 1) or { crash "nope" }) { "ok" } else { "bad" }
    //     ;

    //     const resources = try helpers.parseAndCanonicalizeExpr(std.testing.allocator, roc_src);
    //     defer helpers.cleanupParseAndCanonical(std.testing.allocator, resources);

    //     var interp2 = try Interpreter.init(std.testing.allocator, resources.module_env, resources.bool_stmt, resources.bool_module.env, null);
    //     defer interp2.deinit();

    //     var host = TestHost{ .allocator = std.testing.allocator };
    //     var ops = makeOps(&host);
    //     const result = try interp2.evalMinimal(resources.expr_idx, &ops);
    //     const rendered = try interp2.renderValueRoc(result);
    //     defer std.testing.allocator.free(rendered);
    //     const expected =
    //         \\"ok"
    //     ;
    //     try std.testing.expectEqualStrings(expected, rendered);
}

test "interpreter logical and is short-circuiting" {
    return error.SkipZigTest; // Comparison operators not yet implemented
    //     const roc_src =
    //         \\if ((1 == 0) and { crash "nope" }) { "bad" } else { "ok" }
    //     ;

    //     const resources = try helpers.parseAndCanonicalizeExpr(std.testing.allocator, roc_src);
    //     defer helpers.cleanupParseAndCanonical(std.testing.allocator, resources);

    //     var interp2 = try Interpreter.init(std.testing.allocator, resources.module_env, resources.bool_stmt, resources.bool_module.env, null);
    //     defer interp2.deinit();

    //     var host = TestHost{ .allocator = std.testing.allocator };
    //     var ops = makeOps(&host);
    //     const result = try interp2.evalMinimal(resources.expr_idx, &ops);
    //     const rendered = try interp2.renderValueRoc(result);
    //     defer std.testing.allocator.free(rendered);
    //     const expected =
    //         \\"ok"
    //     ;
    //     try std.testing.expectEqualStrings(expected, rendered);
}

test "interpreter recursion: factorial 5 -> 120" {
    return error.SkipZigTest; // Comparison operators not yet implemented
    //     const roc_src =
    //         \\{ fact = (|n| if n == 0 { 1 } else { n * fact(n - 1) }) fact(5) }
    //     ;

    //     const resources = try helpers.parseAndCanonicalizeExpr(std.testing.allocator, roc_src);
    //     defer helpers.cleanupParseAndCanonical(std.testing.allocator, resources);

    //     var interp2 = try Interpreter.init(std.testing.allocator, resources.module_env, resources.bool_stmt, resources.bool_module.env, null);
    //     defer interp2.deinit();

    //     var host = TestHost{ .allocator = std.testing.allocator };
    //     var ops = makeOps(&host);
    //     const result = try interp2.evalMinimal(resources.expr_idx, &ops);
    //     const rendered = try interp2.renderValueRoc(result);
    //     defer std.testing.allocator.free(rendered);
    //     try std.testing.expectEqualStrings("120", rendered);
}

// Additional complex recursion tests (mutual recursion, nested tuple builders)
// will follow after adding tag union translation and broader type translation
// support in Interpreter.translateTypeVar.

test "interpreter recursion: fibonacci 5 -> 5" {
    return error.SkipZigTest; // Comparison operators not yet implemented
    //     const roc_src =
    //         \\{ fib = (|n| if n == 0 { 0 } else if n == 1 { 1 } else { fib(n - 1) + fib(n - 2) }) fib(5) }
    //     ;

    //     const resources = try helpers.parseAndCanonicalizeExpr(std.testing.allocator, roc_src);
    //     defer helpers.cleanupParseAndCanonical(std.testing.allocator, resources);

    //     var interp2 = try Interpreter.init(std.testing.allocator, resources.module_env, resources.bool_stmt, resources.bool_module.env, null);
    //     defer interp2.deinit();

    //     var host = TestHost{ .allocator = std.testing.allocator };
    //     var ops = makeOps(&host);
    //     const result = try interp2.evalMinimal(resources.expr_idx, &ops);
    //     const rendered = try interp2.renderValueRoc(result);
    //     defer std.testing.allocator.free(rendered);
    //     try std.testing.expectEqualStrings("5", rendered);
}

// Tag union tests (anonymous, non-recursive) — RED first

test "interpreter tag union: one-arg tag Ok(42)" {
    const roc_src =
        \\Ok(42)
    ;

    const resources = try helpers.parseAndCanonicalizeExpr(std.testing.allocator, roc_src);
    defer helpers.cleanupParseAndCanonical(std.testing.allocator, resources);

    var interp2 = try Interpreter.init(std.testing.allocator, resources.module_env, resources.bool_stmt, resources.bool_module.env, null);
    defer interp2.deinit();

    var host = TestHost{ .allocator = std.testing.allocator };
    var ops = makeOps(&host);
    const result = try interp2.evalMinimal(resources.expr_idx, &ops);
    const ct_var = can.ModuleEnv.varFrom(resources.expr_idx);
    const rt_var = try interp2.translateTypeVar(resources.module_env, ct_var);
    const rendered = try interp2.renderValueRocWithType(result, rt_var);
    defer std.testing.allocator.free(rendered);
    const expected =
        \\Ok(42)
    ;
    try std.testing.expectEqualStrings(expected, rendered);
}

test "interpreter tag union: multi-arg tag Point(1, 2)" {
    const roc_src =
        \\Point(1, 2)
    ;

    const resources = try helpers.parseAndCanonicalizeExpr(std.testing.allocator, roc_src);
    defer helpers.cleanupParseAndCanonical(std.testing.allocator, resources);

    var interp2 = try Interpreter.init(std.testing.allocator, resources.module_env, resources.bool_stmt, resources.bool_module.env, null);
    defer interp2.deinit();

    var host = TestHost{ .allocator = std.testing.allocator };
    var ops = makeOps(&host);
    const result = try interp2.evalMinimal(resources.expr_idx, &ops);
    const ct_var = can.ModuleEnv.varFrom(resources.expr_idx);
    const rt_var = try interp2.translateTypeVar(resources.module_env, ct_var);
    const rendered = try interp2.renderValueRocWithType(result, rt_var);
    defer std.testing.allocator.free(rendered);
    const expected =
        \\Point(1, 2)
    ;
    try std.testing.expectEqualStrings(expected, rendered);
}

// Recursion via Z-combinator using if, ==, and subtraction
// Recursion tests will follow after we add minimal tail recursion support
