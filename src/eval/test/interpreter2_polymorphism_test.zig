//! Polymorphism tests for Interpreter2 focused on closures without captures (Milestone 1).
//! Each test starts with Roc source (multiline Zig string with `\\`), parses + canonicalizes
//! with early diagnostics, evaluates with Interpreter2, and renders Roc output.

const std = @import("std");
const helpers = @import("helpers.zig");
const Interpreter2 = @import("../interpreter2.zig").Interpreter2;
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
fn testRocCrashed(crashed_args: *const RocCrashed, _: *anyopaque) callconv(.C) void {
    _ = crashed_args;
    @panic("Roc crashed");
}

fn makeOps(alloc: std.mem.Allocator) RocOps {
    var host = TestHost{ .allocator = alloc };
    return RocOps{
        .env = @ptrCast(&host),
        .roc_alloc = testRocAlloc,
        .roc_dealloc = testRocDealloc,
        .roc_realloc = testRocRealloc,
        .roc_dbg = testRocDbg,
        .roc_expect_failed = testRocExpectFailed,
        .roc_crashed = testRocCrashed,
        .host_fns = undefined,
    };
}

test "interpreter2 poly: return a function then call (int)" {
    const roc_src =
        \\(|_| (|x| x))(0)(42)
    ;

    const resources = try helpers.parseAndCanonicalizeExpr(std.testing.allocator, roc_src);
    defer helpers.cleanupParseAndCanonical(std.testing.allocator, resources);

    var interp2 = try Interpreter2.init(std.testing.allocator, resources.module_env);
    defer interp2.deinit();

    var ops = makeOps(std.testing.allocator);
    const result = try interp2.evalMinimal(resources.expr_idx, &ops);
    const rendered = try interp2.renderValueRoc(result);
    defer std.testing.allocator.free(rendered);
    try std.testing.expectEqualStrings("42", rendered);
}

test "interpreter2 poly: return a function then call (string)" {
    const roc_src =
        \\(|_| (|x| x))(0)("hi")
    ;

    const resources = try helpers.parseAndCanonicalizeExpr(std.testing.allocator, roc_src);
    defer helpers.cleanupParseAndCanonical(std.testing.allocator, resources);

    var interp2 = try Interpreter2.init(std.testing.allocator, resources.module_env);
    defer interp2.deinit();

    var ops = makeOps(std.testing.allocator);
    const result = try interp2.evalMinimal(resources.expr_idx, &ops);
    const rendered = try interp2.renderValueRoc(result);
    defer std.testing.allocator.free(rendered);
    const expected =
        \\"hi"
    ;
    try std.testing.expectEqualStrings(expected, rendered);
}

test "interpreter2 captures (monomorphic): adder" {
    const roc_src =
        \\(|n| (|x| n + x))(1)(41)
    ;

    const resources = try helpers.parseAndCanonicalizeExpr(std.testing.allocator, roc_src);
    defer helpers.cleanupParseAndCanonical(std.testing.allocator, resources);

    var interp2 = try Interpreter2.init(std.testing.allocator, resources.module_env);
    defer interp2.deinit();

    var ops = makeOps(std.testing.allocator);
    const result = try interp2.evalMinimal(resources.expr_idx, &ops);
    const rendered = try interp2.renderValueRoc(result);
    defer std.testing.allocator.free(rendered);
    try std.testing.expectEqualStrings("42", rendered);
}

test "interpreter2 captures (monomorphic): constant function" {
    const roc_src =
        \\(|x| (|_| x))("hi")(0)
    ;

    const resources = try helpers.parseAndCanonicalizeExpr(std.testing.allocator, roc_src);
    defer helpers.cleanupParseAndCanonical(std.testing.allocator, resources);

    var interp2 = try Interpreter2.init(std.testing.allocator, resources.module_env);
    defer interp2.deinit();

    var ops = makeOps(std.testing.allocator);
    const result = try interp2.evalMinimal(resources.expr_idx, &ops);
    const rendered = try interp2.renderValueRoc(result);
    defer std.testing.allocator.free(rendered);
    const expected =
        \\"hi"
    ;
    try std.testing.expectEqualStrings(expected, rendered);
}

test "interpreter2 captures (polymorphic): capture id and apply to int" {
    const roc_src =
        \\((|id| (|x| id(x)))(|y| y))(41)
    ;

    const resources = try helpers.parseAndCanonicalizeExpr(std.testing.allocator, roc_src);
    defer helpers.cleanupParseAndCanonical(std.testing.allocator, resources);

    var interp2 = try Interpreter2.init(std.testing.allocator, resources.module_env);
    defer interp2.deinit();

    var ops = makeOps(std.testing.allocator);
    const result = try interp2.evalMinimal(resources.expr_idx, &ops);
    const rendered = try interp2.renderValueRoc(result);
    defer std.testing.allocator.free(rendered);
    try std.testing.expectEqualStrings("41", rendered);
}

test "interpreter2 captures (polymorphic): capture id and apply to string" {
    const roc_src =
        \\((|id| (|x| id(x)))(|y| y))("ok")
    ;

    const resources = try helpers.parseAndCanonicalizeExpr(std.testing.allocator, roc_src);
    defer helpers.cleanupParseAndCanonical(std.testing.allocator, resources);

    var interp2 = try Interpreter2.init(std.testing.allocator, resources.module_env);
    defer interp2.deinit();

    var ops = makeOps(std.testing.allocator);
    const result = try interp2.evalMinimal(resources.expr_idx, &ops);
    const rendered = try interp2.renderValueRoc(result);
    defer std.testing.allocator.free(rendered);
    const expected =
        \\"ok"
    ;
    try std.testing.expectEqualStrings(expected, rendered);
}

test "interpreter2 captures (polymorphic): same captured id used at two types" {
    const roc_src =
        \\(((|f| (|a| (|b| (f(a), f(b)))))(|x| x))(1))("hi")
    ;

    const resources = try helpers.parseAndCanonicalizeExpr(std.testing.allocator, roc_src);
    defer helpers.cleanupParseAndCanonical(std.testing.allocator, resources);

    var interp2 = try Interpreter2.init(std.testing.allocator, resources.module_env);
    defer interp2.deinit();

    var ops = makeOps(std.testing.allocator);
    const result = try interp2.evalMinimal(resources.expr_idx, &ops);
    const rendered = try interp2.renderValueRoc(result);
    defer std.testing.allocator.free(rendered);
    const expected =
        \\(1, "hi")
    ;
    try std.testing.expectEqualStrings(expected, rendered);
}

// Higher-order: pass a function and apply inside another function
test "interpreter2 higher-order: apply f then call with 41" {
    const roc_src =
        \\((|f| (|x| f(x)))(|n| n + 1))(41)
    ;

    const resources = try helpers.parseAndCanonicalizeExpr(std.testing.allocator, roc_src);
    defer helpers.cleanupParseAndCanonical(std.testing.allocator, resources);

    var interp2 = try Interpreter2.init(std.testing.allocator, resources.module_env);
    defer interp2.deinit();

    var ops = makeOps(std.testing.allocator);
    const result = try interp2.evalMinimal(resources.expr_idx, &ops);
    const rendered = try interp2.renderValueRoc(result);
    defer std.testing.allocator.free(rendered);
    try std.testing.expectEqualStrings("42", rendered);
}

// Higher-order: double apply f inside a function
test "interpreter2 higher-order: apply f twice" {
    const roc_src =
        \\((|f| (|x| f(f(x))))(|n| n + 1))(40)
    ;

    const resources = try helpers.parseAndCanonicalizeExpr(std.testing.allocator, roc_src);
    defer helpers.cleanupParseAndCanonical(std.testing.allocator, resources);

    var interp2 = try Interpreter2.init(std.testing.allocator, resources.module_env);
    defer interp2.deinit();

    var ops = makeOps(std.testing.allocator);
    const result = try interp2.evalMinimal(resources.expr_idx, &ops);
    const rendered = try interp2.renderValueRoc(result);
    defer std.testing.allocator.free(rendered);
    try std.testing.expectEqualStrings("42", rendered);
}

// Higher-order: pass a constructed closure as an argument, then apply with an int
test "interpreter2 higher-order: pass constructed closure and apply" {
    const roc_src =
        \\(|g| g(41))((|f| (|x| f(x)))(|y| y))
    ;

    const resources = try helpers.parseAndCanonicalizeExpr(std.testing.allocator, roc_src);
    defer helpers.cleanupParseAndCanonical(std.testing.allocator, resources);

    var interp2 = try Interpreter2.init(std.testing.allocator, resources.module_env);
    defer interp2.deinit();

    var ops = makeOps(std.testing.allocator);
    const result = try interp2.evalMinimal(resources.expr_idx, &ops);
    const rendered = try interp2.renderValueRoc(result);
    defer std.testing.allocator.free(rendered);
    try std.testing.expectEqualStrings("41", rendered);
}

// Higher-order: construct a function then pass it to a consumer and evaluate
test "interpreter2 higher-order: construct then pass then call" {
    const roc_src =
        \\((|make| (|z| (make(|n| n + 1))(z)))(|f| (|x| f(x))))(41)
    ;

    const resources = try helpers.parseAndCanonicalizeExpr(std.testing.allocator, roc_src);
    defer helpers.cleanupParseAndCanonical(std.testing.allocator, resources);

    var interp2 = try Interpreter2.init(std.testing.allocator, resources.module_env);
    defer interp2.deinit();

    var ops = makeOps(std.testing.allocator);
    const result = try interp2.evalMinimal(resources.expr_idx, &ops);
    const rendered = try interp2.renderValueRoc(result);
    defer std.testing.allocator.free(rendered);
    try std.testing.expectEqualStrings("42", rendered);
}

// Higher-order: compose = \f -> \g -> \x -> f(g(x)) and apply
test "interpreter2 higher-order: compose id with +1" {
    const roc_src =
        \\(((|f| (|g| (|x| f(g(x)))))(|n| n + 1))(|y| y))(41)
    ;

    const resources = try helpers.parseAndCanonicalizeExpr(std.testing.allocator, roc_src);
    defer helpers.cleanupParseAndCanonical(std.testing.allocator, resources);

    var interp2 = try Interpreter2.init(std.testing.allocator, resources.module_env);
    defer interp2.deinit();

    var ops = makeOps(std.testing.allocator);
    const result = try interp2.evalMinimal(resources.expr_idx, &ops);
    const rendered = try interp2.renderValueRoc(result);
    defer std.testing.allocator.free(rendered);
    try std.testing.expectEqualStrings("42", rendered);
}

// Higher-order + capture: returns polymorphic function that uses a captured increment
test "interpreter2 higher-order: return poly fn using captured +n" {
    const roc_src =
        \\(((|n| (|id| (|x| id(x + n))))(1))(|y| y))(41)
    ;

    const resources = try helpers.parseAndCanonicalizeExpr(std.testing.allocator, roc_src);
    defer helpers.cleanupParseAndCanonical(std.testing.allocator, resources);

    var interp2 = try Interpreter2.init(std.testing.allocator, resources.module_env);
    defer interp2.deinit();

    var ops = makeOps(std.testing.allocator);
    const result = try interp2.evalMinimal(resources.expr_idx, &ops);
    const rendered = try interp2.renderValueRoc(result);
    defer std.testing.allocator.free(rendered);
    try std.testing.expectEqualStrings("42", rendered);
}

// Recursion via block let-binding using a named recursive closure
test "interpreter2 recursion: simple countdown" {
    const roc_src =
        \\{ rec = (|n| if n == 0 { 0 } else { rec(n - 1) + 1 }) rec(2) }
    ;

    const resources = try helpers.parseAndCanonicalizeExpr(std.testing.allocator, roc_src);
    defer helpers.cleanupParseAndCanonical(std.testing.allocator, resources);

    var interp2 = try Interpreter2.init(std.testing.allocator, resources.module_env);
    defer interp2.deinit();

    var ops = makeOps(std.testing.allocator);
    const result = try interp2.evalMinimal(resources.expr_idx, &ops);
    const rendered = try interp2.renderValueRoc(result);
    defer std.testing.allocator.free(rendered);
    try std.testing.expectEqualStrings("2", rendered);
}

// Recursion via Z-combinator using if, ==, and subtraction
// Recursion tests will follow after we add minimal tail recursion support
