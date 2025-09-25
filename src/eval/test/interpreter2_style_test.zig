//! Interpreter2 style tests that begin and end with Roc syntax.
//! These tests parse user-supplied Roc code, fail fast with proper diagnostics
//! if any compilation stage has problems, and then exercise Interpreter2’s
//! runtime type/unification flow alongside evaluating the value with the
//! current interpreter for end-to-end verification.

const std = @import("std");
const helpers = @import("helpers.zig");
const can = @import("can");
const types = @import("types");
const layout = @import("layout");
const builtins = @import("builtins");
const eval_mod = @import("../mod.zig");
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
}

test "interpreter2: (|x| x)(\"Hello\") yields \"Hello\"" {
    // Roc input (begin with Roc syntax):
    const roc_src = "(|x| x)(\"Hello\")";
    // Expected Roc output (end with Roc syntax):
    const expected_out_roc = "\"Hello\"";

    // Parse + canonicalize (+ typecheck) with fast failure & proper diagnostics
    const resources = try helpers.parseAndCanonicalizeExpr(std.testing.allocator, roc_src);
    defer helpers.cleanupParseAndCanonical(std.testing.allocator, resources);

    // Evaluate with current interpreter (value result must be the string Hello)
    try helpers.runExpectStr(roc_src, "Hello", .no_trace);

    // Now exercise Interpreter2 on the same ModuleEnv to verify runtime typing and minimal evaluation
    var interp2 = try Interpreter2.init(std.testing.allocator, resources.module_env);
    defer interp2.deinit();

    const CIR = can.CIR;
    const root_expr = resources.module_env.store.getExpr(resources.expr_idx);
    try std.testing.expect(root_expr == .e_call);
    const call = root_expr.e_call;
    const all_exprs = resources.module_env.store.sliceExpr(call.args);
    try std.testing.expect(all_exprs.len == 2);
    const func_expr_idx: CIR.Expr.Idx = all_exprs[0];
    const arg_expr_idx: CIR.Expr.Idx = all_exprs[1];

    // Translate function type and arg type to runtime types
    const func_ct_var: types.Var = can.ModuleEnv.varFrom(func_expr_idx);
    _ = try interp2.translateTypeVar(resources.module_env, func_ct_var);
    const arg_ct_var: types.Var = can.ModuleEnv.varFrom(arg_expr_idx);
    const arg_rt_var = try interp2.translateTypeVar(resources.module_env, arg_ct_var);

    // Prepare call using runtime function type; should constrain return to Str
    // Prepare call using a hint (until full eval is fully wired) and minimally evaluate
    const entry = (try interp2.prepareCall(1234, &.{arg_rt_var}, arg_rt_var)) orelse return error.TestUnexpectedResult;
    try std.testing.expect(entry.return_layout_slot != 0);

    // Minimal eval: evaluate the call directly via Interpreter2
    // Minimal eval using fresh RocOps
    var host = TestHost{ .allocator = std.testing.allocator };
    var ops = RocOps{
        .env = @ptrCast(&host),
        .roc_alloc = testRocAlloc,
        .roc_dealloc = testRocDealloc,
        .roc_realloc = testRocRealloc,
        .roc_dbg = testRocDbg,
        .roc_expect_failed = testRocExpectFailed,
        .roc_crashed = testRocCrashed,
        .host_fns = undefined,
    };
    const result = try interp2.evalMinimal(resources.expr_idx, &ops);
    const rendered = try interp2.renderValueRoc(result);
    defer std.testing.allocator.free(rendered);
    // End with Roc-output literal for readability
    try std.testing.expectEqualStrings("\"Hello\"", rendered);

    // For clarity, re-assert the expected Roc output literal
    const got_out_roc = expected_out_roc; // In a future step, render REPL-style from result
    try std.testing.expectEqualStrings(expected_out_roc, got_out_roc);
}

test "interpreter2: (|n| n + 1)(41) yields 42" {
    const roc_src = "(|n| n + 1)(41)";
    const resources = try helpers.parseAndCanonicalizeExpr(std.testing.allocator, roc_src);
    defer helpers.cleanupParseAndCanonical(std.testing.allocator, resources);

    var interp2 = try Interpreter2.init(std.testing.allocator, resources.module_env);
    defer interp2.deinit();

    var host = TestHost{ .allocator = std.testing.allocator };
    var ops = RocOps{
        .env = @ptrCast(&host),
        .roc_alloc = testRocAlloc,
        .roc_dealloc = testRocDealloc,
        .roc_realloc = testRocRealloc,
        .roc_dbg = testRocDbg,
        .roc_expect_failed = testRocExpectFailed,
        .roc_crashed = testRocCrashed,
        .host_fns = undefined,
    };

    const result = try interp2.evalMinimal(resources.expr_idx, &ops);
    const rendered = try interp2.renderValueRoc(result);
    defer std.testing.allocator.free(rendered);
    try std.testing.expectEqualStrings("42", rendered);
}

test "interpreter2: literal True renders True" {
    const roc_src = "True";
    const resources = try helpers.parseAndCanonicalizeExpr(std.testing.allocator, roc_src);
    defer helpers.cleanupParseAndCanonical(std.testing.allocator, resources);

    var interp2 = try Interpreter2.init(std.testing.allocator, resources.module_env);
    defer interp2.deinit();

    var host = TestHost{ .allocator = std.testing.allocator };
    var ops = RocOps{
        .env = @ptrCast(&host),
        .roc_alloc = testRocAlloc,
        .roc_dealloc = testRocDealloc,
        .roc_realloc = testRocRealloc,
        .roc_dbg = testRocDbg,
        .roc_expect_failed = testRocExpectFailed,
        .roc_crashed = testRocCrashed,
        .host_fns = undefined,
    };

    const result = try interp2.evalMinimal(resources.expr_idx, &ops);
    const ct_var = can.ModuleEnv.varFrom(resources.expr_idx);
    const rt_var = try interp2.translateTypeVar(resources.module_env, ct_var);
    const rendered = try interp2.renderValueRocWithType(result, rt_var);
    defer std.testing.allocator.free(rendered);
    try std.testing.expectEqualStrings("True", rendered);
}

test "interpreter2: True == False yields False" {
    const roc_src = "True == False";
    const resources = try helpers.parseAndCanonicalizeExpr(std.testing.allocator, roc_src);
    defer helpers.cleanupParseAndCanonical(std.testing.allocator, resources);

    var interp2 = try Interpreter2.init(std.testing.allocator, resources.module_env);
    defer interp2.deinit();

    var host = TestHost{ .allocator = std.testing.allocator };
    var ops = RocOps{
        .env = @ptrCast(&host),
        .roc_alloc = testRocAlloc,
        .roc_dealloc = testRocDealloc,
        .roc_realloc = testRocRealloc,
        .roc_dbg = testRocDbg,
        .roc_expect_failed = testRocExpectFailed,
        .roc_crashed = testRocCrashed,
        .host_fns = undefined,
    };

    const result = try interp2.evalMinimal(resources.expr_idx, &ops);
    const ct_var = can.ModuleEnv.varFrom(resources.expr_idx);
    const rt_var = try interp2.translateTypeVar(resources.module_env, ct_var);
    const rendered = try interp2.renderValueRocWithType(result, rt_var);
    defer std.testing.allocator.free(rendered);
    try std.testing.expectEqualStrings("False", rendered);
}

test "interpreter2: \"hi\" == \"hi\" yields True" {
    const roc_src = "\"hi\" == \"hi\"";
    const resources = try helpers.parseAndCanonicalizeExpr(std.testing.allocator, roc_src);
    defer helpers.cleanupParseAndCanonical(std.testing.allocator, resources);

    try helpers.runExpectBool(roc_src, true, .no_trace);

    var interp2 = try Interpreter2.init(std.testing.allocator, resources.module_env);
    defer interp2.deinit();

    var host = TestHost{ .allocator = std.testing.allocator };
    var ops = RocOps{
        .env = @ptrCast(&host),
        .roc_alloc = testRocAlloc,
        .roc_dealloc = testRocDealloc,
        .roc_realloc = testRocRealloc,
        .roc_dbg = testRocDbg,
        .roc_expect_failed = testRocExpectFailed,
        .roc_crashed = testRocCrashed,
        .host_fns = undefined,
    };

    const result = try interp2.evalMinimal(resources.expr_idx, &ops);
    const ct_var = can.ModuleEnv.varFrom(resources.expr_idx);
    const rt_var = try interp2.translateTypeVar(resources.module_env, ct_var);
    const rendered = try interp2.renderValueRocWithType(result, rt_var);
    defer std.testing.allocator.free(rendered);
    try std.testing.expectEqualStrings("True", rendered);
}

test "interpreter2: match tuple pattern destructures" {
    const roc_src = "match (1, 2) { (1, b) => b, _ => 0 }";
    const resources = try helpers.parseAndCanonicalizeExpr(std.testing.allocator, roc_src);
    defer helpers.cleanupParseAndCanonical(std.testing.allocator, resources);

    var interp2 = try Interpreter2.init(std.testing.allocator, resources.module_env);
    defer interp2.deinit();

    var host = TestHost{ .allocator = std.testing.allocator };
    var ops = RocOps{
        .env = @ptrCast(&host),
        .roc_alloc = testRocAlloc,
        .roc_dealloc = testRocDealloc,
        .roc_realloc = testRocRealloc,
        .roc_dbg = testRocDbg,
        .roc_expect_failed = testRocExpectFailed,
        .roc_crashed = testRocCrashed,
        .host_fns = undefined,
    };

    const result = try interp2.evalMinimal(resources.expr_idx, &ops);
    const rendered = try interp2.renderValueRoc(result);
    defer std.testing.allocator.free(rendered);
    try std.testing.expectEqualStrings("2", rendered);
}

test "interpreter2: match bool patterns" {
    const roc_src = "match True { True => 1, False => 0 }";
    const resources = try helpers.parseAndCanonicalizeExpr(std.testing.allocator, roc_src);
    defer helpers.cleanupParseAndCanonical(std.testing.allocator, resources);

    var interp2 = try Interpreter2.init(std.testing.allocator, resources.module_env);
    defer interp2.deinit();

    var host = TestHost{ .allocator = std.testing.allocator };
    var ops = RocOps{
        .env = @ptrCast(&host),
        .roc_alloc = testRocAlloc,
        .roc_dealloc = testRocDealloc,
        .roc_realloc = testRocRealloc,
        .roc_dbg = testRocDbg,
        .roc_expect_failed = testRocExpectFailed,
        .roc_crashed = testRocCrashed,
        .host_fns = undefined,
    };

    const result = try interp2.evalMinimal(resources.expr_idx, &ops);
    const rendered = try interp2.renderValueRoc(result);
    defer std.testing.allocator.free(rendered);
    try std.testing.expectEqualStrings("1", rendered);
}

test "interpreter2: match result tag payload" {
    const roc_src = "match Ok(3) { Ok(n) => n + 1, Err(_) => 0 }";
    const resources = try helpers.parseAndCanonicalizeExpr(std.testing.allocator, roc_src);
    defer helpers.cleanupParseAndCanonical(std.testing.allocator, resources);

    var interp2 = try Interpreter2.init(std.testing.allocator, resources.module_env);
    defer interp2.deinit();

    var host = TestHost{ .allocator = std.testing.allocator };
    var ops = RocOps{
        .env = @ptrCast(&host),
        .roc_alloc = testRocAlloc,
        .roc_dealloc = testRocDealloc,
        .roc_realloc = testRocRealloc,
        .roc_dbg = testRocDbg,
        .roc_expect_failed = testRocExpectFailed,
        .roc_crashed = testRocCrashed,
        .host_fns = undefined,
    };

    const result = try interp2.evalMinimal(resources.expr_idx, &ops);
    const rendered = try interp2.renderValueRoc(result);
    defer std.testing.allocator.free(rendered);
    try std.testing.expectEqualStrings("4", rendered);
}

test "interpreter2: match record destructures fields" {
    const roc_src = "match { x: 1, y: 2 } { { x, y } => x + y, _ => 0 }";
    const resources = try helpers.parseAndCanonicalizeExpr(std.testing.allocator, roc_src);
    defer helpers.cleanupParseAndCanonical(std.testing.allocator, resources);

    var interp2 = try Interpreter2.init(std.testing.allocator, resources.module_env);
    defer interp2.deinit();

    var host = TestHost{ .allocator = std.testing.allocator };
    var ops = RocOps{
        .env = @ptrCast(&host),
        .roc_alloc = testRocAlloc,
        .roc_dealloc = testRocDealloc,
        .roc_realloc = testRocRealloc,
        .roc_dbg = testRocDbg,
        .roc_expect_failed = testRocExpectFailed,
        .roc_crashed = testRocCrashed,
        .host_fns = undefined,
    };

    const result = try interp2.evalMinimal(resources.expr_idx, &ops);
    const rendered = try interp2.renderValueRoc(result);
    defer std.testing.allocator.free(rendered);
    try std.testing.expectEqualStrings("3", rendered);
}

test "interpreter2: crash statement triggers crash error and message" {
    const roc_src = "{\n    crash \"boom\"\n    0\n}";
    const resources = try helpers.parseAndCanonicalizeExpr(std.testing.allocator, roc_src);
    defer helpers.cleanupParseAndCanonical(std.testing.allocator, resources);

    var interp2 = try Interpreter2.init(std.testing.allocator, resources.module_env);
    defer interp2.deinit();

    var host = TestHost{ .allocator = std.testing.allocator };
    var ops = RocOps{
        .env = @ptrCast(&host),
        .roc_alloc = testRocAlloc,
        .roc_dealloc = testRocDealloc,
        .roc_realloc = testRocRealloc,
        .roc_dbg = testRocDbg,
        .roc_expect_failed = testRocExpectFailed,
        .roc_crashed = testRocCrashed,
        .host_fns = undefined,
    };

    try std.testing.expectError(error.Crash, interp2.evalMinimal(resources.expr_idx, &ops));
    try std.testing.expect(interp2.hasCrashed());
    const msg = interp2.getCrashMsg() orelse return error.TestUnexpectedResult;
    try std.testing.expectEqualStrings("boom", msg);
}

test "interpreter2: expect expression succeeds" {
    const roc_src = "{\n    expect 1 == 1\n    {}\n}";
    const resources = try helpers.parseAndCanonicalizeExpr(std.testing.allocator, roc_src);
    defer helpers.cleanupParseAndCanonical(std.testing.allocator, resources);

    var interp2 = try Interpreter2.init(std.testing.allocator, resources.module_env);
    defer interp2.deinit();

    var host = TestHost{ .allocator = std.testing.allocator };
    var ops = RocOps{
        .env = @ptrCast(&host),
        .roc_alloc = testRocAlloc,
        .roc_dealloc = testRocDealloc,
        .roc_realloc = testRocRealloc,
        .roc_dbg = testRocDbg,
        .roc_expect_failed = testRocExpectFailed,
        .roc_crashed = testRocCrashed,
        .host_fns = undefined,
    };

    const result = try interp2.evalMinimal(resources.expr_idx, &ops);
    try std.testing.expect(!interp2.hasCrashed());
    const ct_var = can.ModuleEnv.varFrom(resources.expr_idx);
    const rt_var = try interp2.translateTypeVar(resources.module_env, ct_var);
    const rendered = try interp2.renderValueRocWithType(result, rt_var);
    defer std.testing.allocator.free(rendered);
    try std.testing.expectEqualStrings("{}", rendered);
}

test "interpreter2: expect expression failure crashes with message" {
    const roc_src = "{\n    expect 1 == 0\n    {}\n}";
    const resources = try helpers.parseAndCanonicalizeExpr(std.testing.allocator, roc_src);
    defer helpers.cleanupParseAndCanonical(std.testing.allocator, resources);

    var interp2 = try Interpreter2.init(std.testing.allocator, resources.module_env);
    defer interp2.deinit();

    var host = TestHost{ .allocator = std.testing.allocator };
    var ops = RocOps{
        .env = @ptrCast(&host),
        .roc_alloc = testRocAlloc,
        .roc_dealloc = testRocDealloc,
        .roc_realloc = testRocRealloc,
        .roc_dbg = testRocDbg,
        .roc_expect_failed = testRocExpectFailed,
        .roc_crashed = testRocCrashed,
        .host_fns = undefined,
    };

    try std.testing.expectError(error.Crash, interp2.evalMinimal(resources.expr_idx, &ops));
    try std.testing.expect(interp2.hasCrashed());
    const msg = interp2.getCrashMsg() orelse return error.TestUnexpectedResult;
    try std.testing.expectEqualStrings("Expect failed: 1 == 0", msg);
}

test "interpreter2: empty record expression renders {}" {
    const roc_src = "{}";
    const resources = try helpers.parseAndCanonicalizeExpr(std.testing.allocator, roc_src);
    defer helpers.cleanupParseAndCanonical(std.testing.allocator, resources);

    var interp2 = try Interpreter2.init(std.testing.allocator, resources.module_env);
    defer interp2.deinit();

    var host = TestHost{ .allocator = std.testing.allocator };
    var ops = RocOps{
        .env = @ptrCast(&host),
        .roc_alloc = testRocAlloc,
        .roc_dealloc = testRocDealloc,
        .roc_realloc = testRocRealloc,
        .roc_dbg = testRocDbg,
        .roc_expect_failed = testRocExpectFailed,
        .roc_crashed = testRocCrashed,
        .host_fns = undefined,
    };

    const result = try interp2.evalMinimal(resources.expr_idx, &ops);
    const ct_var = can.ModuleEnv.varFrom(resources.expr_idx);
    const rt_var = try interp2.translateTypeVar(resources.module_env, ct_var);
    const rendered = try interp2.renderValueRocWithType(result, rt_var);
    defer std.testing.allocator.free(rendered);
    try std.testing.expectEqualStrings("{}", rendered);
}

test "interpreter2: f64 literal renders 3.25" {
    const roc_src = "3.25f64";
    const resources = try helpers.parseAndCanonicalizeExpr(std.testing.allocator, roc_src);
    defer helpers.cleanupParseAndCanonical(std.testing.allocator, resources);

    var interp2 = try Interpreter2.init(std.testing.allocator, resources.module_env);
    defer interp2.deinit();

    var host = TestHost{ .allocator = std.testing.allocator };
    var ops = RocOps{
        .env = @ptrCast(&host),
        .roc_alloc = testRocAlloc,
        .roc_dealloc = testRocDealloc,
        .roc_realloc = testRocRealloc,
        .roc_dbg = testRocDbg,
        .roc_expect_failed = testRocExpectFailed,
        .roc_crashed = testRocCrashed,
        .host_fns = undefined,
    };

    const result = try interp2.evalMinimal(resources.expr_idx, &ops);
    const rendered = try interp2.renderValueRoc(result);
    defer std.testing.allocator.free(rendered);
    try std.testing.expectEqualStrings("3.25", rendered);
}

test "interpreter2: decimal literal renders 0.125" {
    const roc_src = "0.125";
    const resources = try helpers.parseAndCanonicalizeExpr(std.testing.allocator, roc_src);
    defer helpers.cleanupParseAndCanonical(std.testing.allocator, resources);

    var interp2 = try Interpreter2.init(std.testing.allocator, resources.module_env);
    defer interp2.deinit();

    var host = TestHost{ .allocator = std.testing.allocator };
    var ops = RocOps{
        .env = @ptrCast(&host),
        .roc_alloc = testRocAlloc,
        .roc_dealloc = testRocDealloc,
        .roc_realloc = testRocRealloc,
        .roc_dbg = testRocDbg,
        .roc_expect_failed = testRocExpectFailed,
        .roc_crashed = testRocCrashed,
        .host_fns = undefined,
    };

    const result = try interp2.evalMinimal(resources.expr_idx, &ops);
    const rendered = try interp2.renderValueRoc(result);
    defer std.testing.allocator.free(rendered);
    try std.testing.expectEqualStrings("0.125", rendered);
}

test "interpreter2: f64 equality True" {
    const roc_src = "3.25f64 == 3.25f64";
    const resources = try helpers.parseAndCanonicalizeExpr(std.testing.allocator, roc_src);
    defer helpers.cleanupParseAndCanonical(std.testing.allocator, resources);

    var interp2 = try Interpreter2.init(std.testing.allocator, resources.module_env);
    defer interp2.deinit();

    var host = TestHost{ .allocator = std.testing.allocator };
    var ops = RocOps{
        .env = @ptrCast(&host),
        .roc_alloc = testRocAlloc,
        .roc_dealloc = testRocDealloc,
        .roc_realloc = testRocRealloc,
        .roc_dbg = testRocDbg,
        .roc_expect_failed = testRocExpectFailed,
        .roc_crashed = testRocCrashed,
        .host_fns = undefined,
    };

    const result = try interp2.evalMinimal(resources.expr_idx, &ops);
    const ct_var = can.ModuleEnv.varFrom(resources.expr_idx);
    const rt_var = try interp2.translateTypeVar(resources.module_env, ct_var);
    const rendered = try interp2.renderValueRocWithType(result, rt_var);
    defer std.testing.allocator.free(rendered);
    try std.testing.expectEqualStrings("True", rendered);
}

test "interpreter2: decimal equality True" {
    const roc_src = "0.125 == 0.125";
    const resources = try helpers.parseAndCanonicalizeExpr(std.testing.allocator, roc_src);
    defer helpers.cleanupParseAndCanonical(std.testing.allocator, resources);

    var interp2 = try Interpreter2.init(std.testing.allocator, resources.module_env);
    defer interp2.deinit();

    var host = TestHost{ .allocator = std.testing.allocator };
    var ops = RocOps{
        .env = @ptrCast(&host),
        .roc_alloc = testRocAlloc,
        .roc_dealloc = testRocDealloc,
        .roc_realloc = testRocRealloc,
        .roc_dbg = testRocDbg,
        .roc_expect_failed = testRocExpectFailed,
        .roc_crashed = testRocCrashed,
        .host_fns = undefined,
    };

    const result = try interp2.evalMinimal(resources.expr_idx, &ops);
    const ct_var = can.ModuleEnv.varFrom(resources.expr_idx);
    const rt_var = try interp2.translateTypeVar(resources.module_env, ct_var);
    const rendered = try interp2.renderValueRocWithType(result, rt_var);
    defer std.testing.allocator.free(rendered);
    try std.testing.expectEqualStrings("True", rendered);
}

test "interpreter2: int and f64 equality True" {
    const roc_src = "1 == 1.0f64";
    const resources = try helpers.parseAndCanonicalizeExpr(std.testing.allocator, roc_src);
    defer helpers.cleanupParseAndCanonical(std.testing.allocator, resources);

    var interp2 = try Interpreter2.init(std.testing.allocator, resources.module_env);
    defer interp2.deinit();

    const binop_expr = resources.module_env.store.getExpr(resources.expr_idx);
    try std.testing.expect(binop_expr == .e_binop);
    const binop = binop_expr.e_binop;
    const lhs_var = can.ModuleEnv.varFrom(binop.lhs);
    const rhs_var = can.ModuleEnv.varFrom(binop.rhs);
    const expr_var = can.ModuleEnv.varFrom(resources.expr_idx);
    try std.testing.expect(resources.module_env.types.resolveVar(lhs_var).desc.content != .err);
    try std.testing.expect(resources.module_env.types.resolveVar(rhs_var).desc.content != .err);
    try std.testing.expect(resources.module_env.types.resolveVar(expr_var).desc.content != .err);

    var host = TestHost{ .allocator = std.testing.allocator };
    var ops = RocOps{
        .env = @ptrCast(&host),
        .roc_alloc = testRocAlloc,
        .roc_dealloc = testRocDealloc,
        .roc_realloc = testRocRealloc,
        .roc_dbg = testRocDbg,
        .roc_expect_failed = testRocExpectFailed,
        .roc_crashed = testRocCrashed,
        .host_fns = undefined,
    };

    const result = try interp2.evalMinimal(resources.expr_idx, &ops);
    const ct_var = can.ModuleEnv.varFrom(resources.expr_idx);
    const rt_var = try interp2.translateTypeVar(resources.module_env, ct_var);
    const rendered = try interp2.renderValueRocWithType(result, rt_var);
    defer std.testing.allocator.free(rendered);
    try std.testing.expectEqualStrings("True", rendered);
}

test "interpreter2: int and decimal equality True" {
    const roc_src = "1 == 1.0";
    const resources = try helpers.parseAndCanonicalizeExpr(std.testing.allocator, roc_src);
    defer helpers.cleanupParseAndCanonical(std.testing.allocator, resources);

    var interp2 = try Interpreter2.init(std.testing.allocator, resources.module_env);
    defer interp2.deinit();

    const binop_expr = resources.module_env.store.getExpr(resources.expr_idx);
    try std.testing.expect(binop_expr == .e_binop);
    const binop = binop_expr.e_binop;
    const lhs_var = can.ModuleEnv.varFrom(binop.lhs);
    const rhs_var = can.ModuleEnv.varFrom(binop.rhs);
    const expr_var = can.ModuleEnv.varFrom(resources.expr_idx);
    try std.testing.expect(resources.module_env.types.resolveVar(lhs_var).desc.content != .err);
    try std.testing.expect(resources.module_env.types.resolveVar(rhs_var).desc.content != .err);
    try std.testing.expect(resources.module_env.types.resolveVar(expr_var).desc.content != .err);

    var host = TestHost{ .allocator = std.testing.allocator };
    var ops = RocOps{
        .env = @ptrCast(&host),
        .roc_alloc = testRocAlloc,
        .roc_dealloc = testRocDealloc,
        .roc_realloc = testRocRealloc,
        .roc_dbg = testRocDbg,
        .roc_expect_failed = testRocExpectFailed,
        .roc_crashed = testRocCrashed,
        .host_fns = undefined,
    };

    const result = try interp2.evalMinimal(resources.expr_idx, &ops);
    const ct_var = can.ModuleEnv.varFrom(resources.expr_idx);
    const rt_var = try interp2.translateTypeVar(resources.module_env, ct_var);
    const rendered = try interp2.renderValueRocWithType(result, rt_var);
    defer std.testing.allocator.free(rendered);
    try std.testing.expectEqualStrings("True", rendered);
}

test "interpreter2: int less-than yields True" {
    const roc_src = "3 < 4";
    const resources = try helpers.parseAndCanonicalizeExpr(std.testing.allocator, roc_src);
    defer helpers.cleanupParseAndCanonical(std.testing.allocator, resources);

    var interp2 = try Interpreter2.init(std.testing.allocator, resources.module_env);
    defer interp2.deinit();

    var host = TestHost{ .allocator = std.testing.allocator };
    var ops = RocOps{
        .env = @ptrCast(&host),
        .roc_alloc = testRocAlloc,
        .roc_dealloc = testRocDealloc,
        .roc_realloc = testRocRealloc,
        .roc_dbg = testRocDbg,
        .roc_expect_failed = testRocExpectFailed,
        .roc_crashed = testRocCrashed,
        .host_fns = undefined,
    };

    const result = try interp2.evalMinimal(resources.expr_idx, &ops);
    const ct_var = can.ModuleEnv.varFrom(resources.expr_idx);
    const rt_var = try interp2.translateTypeVar(resources.module_env, ct_var);
    const rendered = try interp2.renderValueRocWithType(result, rt_var);
    defer std.testing.allocator.free(rendered);
    try std.testing.expectEqualStrings("True", rendered);
}

test "interpreter2: int greater-than yields False" {
    const roc_src = "5 > 8";
    const resources = try helpers.parseAndCanonicalizeExpr(std.testing.allocator, roc_src);
    defer helpers.cleanupParseAndCanonical(std.testing.allocator, resources);

    var interp2 = try Interpreter2.init(std.testing.allocator, resources.module_env);
    defer interp2.deinit();

    var host = TestHost{ .allocator = std.testing.allocator };
    var ops = RocOps{
        .env = @ptrCast(&host),
        .roc_alloc = testRocAlloc,
        .roc_dealloc = testRocDealloc,
        .roc_realloc = testRocRealloc,
        .roc_dbg = testRocDbg,
        .roc_expect_failed = testRocExpectFailed,
        .roc_crashed = testRocCrashed,
        .host_fns = undefined,
    };

    const result = try interp2.evalMinimal(resources.expr_idx, &ops);
    const ct_var = can.ModuleEnv.varFrom(resources.expr_idx);
    const rt_var = try interp2.translateTypeVar(resources.module_env, ct_var);
    const rendered = try interp2.renderValueRocWithType(result, rt_var);
    defer std.testing.allocator.free(rendered);
    try std.testing.expectEqualStrings("False", rendered);
}

test "interpreter2: f64 greater-than yields True" {
    const roc_src = "3.5f64 > 1.25f64";
    const resources = try helpers.parseAndCanonicalizeExpr(std.testing.allocator, roc_src);
    defer helpers.cleanupParseAndCanonical(std.testing.allocator, resources);

    var interp2 = try Interpreter2.init(std.testing.allocator, resources.module_env);
    defer interp2.deinit();

    var host = TestHost{ .allocator = std.testing.allocator };
    var ops = RocOps{
        .env = @ptrCast(&host),
        .roc_alloc = testRocAlloc,
        .roc_dealloc = testRocDealloc,
        .roc_realloc = testRocRealloc,
        .roc_dbg = testRocDbg,
        .roc_expect_failed = testRocExpectFailed,
        .roc_crashed = testRocCrashed,
        .host_fns = undefined,
    };

    const result = try interp2.evalMinimal(resources.expr_idx, &ops);
    const ct_var = can.ModuleEnv.varFrom(resources.expr_idx);
    const rt_var = try interp2.translateTypeVar(resources.module_env, ct_var);
    const rendered = try interp2.renderValueRocWithType(result, rt_var);
    defer std.testing.allocator.free(rendered);
    try std.testing.expectEqualStrings("True", rendered);
}

test "interpreter2: decimal less-than-or-equal yields True" {
    const roc_src = "0.5 <= 0.5";
    const resources = try helpers.parseAndCanonicalizeExpr(std.testing.allocator, roc_src);
    defer helpers.cleanupParseAndCanonical(std.testing.allocator, resources);

    var interp2 = try Interpreter2.init(std.testing.allocator, resources.module_env);
    defer interp2.deinit();

    var host = TestHost{ .allocator = std.testing.allocator };
    var ops = RocOps{
        .env = @ptrCast(&host),
        .roc_alloc = testRocAlloc,
        .roc_dealloc = testRocDealloc,
        .roc_realloc = testRocRealloc,
        .roc_dbg = testRocDbg,
        .roc_expect_failed = testRocExpectFailed,
        .roc_crashed = testRocCrashed,
        .host_fns = undefined,
    };

    const result = try interp2.evalMinimal(resources.expr_idx, &ops);
    const ct_var = can.ModuleEnv.varFrom(resources.expr_idx);
    const rt_var = try interp2.translateTypeVar(resources.module_env, ct_var);
    const rendered = try interp2.renderValueRocWithType(result, rt_var);
    defer std.testing.allocator.free(rendered);
    try std.testing.expectEqualStrings("True", rendered);
}

test "interpreter2: int and f64 less-than yields True" {
    const roc_src = "1 < 2.0f64";
    const resources = try helpers.parseAndCanonicalizeExpr(std.testing.allocator, roc_src);
    defer helpers.cleanupParseAndCanonical(std.testing.allocator, resources);

    var interp2 = try Interpreter2.init(std.testing.allocator, resources.module_env);
    defer interp2.deinit();

    var host = TestHost{ .allocator = std.testing.allocator };
    var ops = RocOps{
        .env = @ptrCast(&host),
        .roc_alloc = testRocAlloc,
        .roc_dealloc = testRocDealloc,
        .roc_realloc = testRocRealloc,
        .roc_dbg = testRocDbg,
        .roc_expect_failed = testRocExpectFailed,
        .roc_crashed = testRocCrashed,
        .host_fns = undefined,
    };

    const result = try interp2.evalMinimal(resources.expr_idx, &ops);
    const ct_var = can.ModuleEnv.varFrom(resources.expr_idx);
    const rt_var = try interp2.translateTypeVar(resources.module_env, ct_var);
    const rendered = try interp2.renderValueRocWithType(result, rt_var);
    defer std.testing.allocator.free(rendered);
    try std.testing.expectEqualStrings("True", rendered);
}

test "interpreter2: int and decimal greater-than yields False" {
    const roc_src = "3 > 5.5";
    const resources = try helpers.parseAndCanonicalizeExpr(std.testing.allocator, roc_src);
    defer helpers.cleanupParseAndCanonical(std.testing.allocator, resources);

    var interp2 = try Interpreter2.init(std.testing.allocator, resources.module_env);
    defer interp2.deinit();

    var host = TestHost{ .allocator = std.testing.allocator };
    var ops = RocOps{
        .env = @ptrCast(&host),
        .roc_alloc = testRocAlloc,
        .roc_dealloc = testRocDealloc,
        .roc_realloc = testRocRealloc,
        .roc_dbg = testRocDbg,
        .roc_expect_failed = testRocExpectFailed,
        .roc_crashed = testRocCrashed,
        .host_fns = undefined,
    };

    const result = try interp2.evalMinimal(resources.expr_idx, &ops);
    const ct_var = can.ModuleEnv.varFrom(resources.expr_idx);
    const rt_var = try interp2.translateTypeVar(resources.module_env, ct_var);
    const rendered = try interp2.renderValueRocWithType(result, rt_var);
    defer std.testing.allocator.free(rendered);
    try std.testing.expectEqualStrings("False", rendered);
}

test "interpreter2: bool inequality yields True" {
    const roc_src = "True != False";
    const resources = try helpers.parseAndCanonicalizeExpr(std.testing.allocator, roc_src);
    defer helpers.cleanupParseAndCanonical(std.testing.allocator, resources);

    var interp2 = try Interpreter2.init(std.testing.allocator, resources.module_env);
    defer interp2.deinit();

    var host = TestHost{ .allocator = std.testing.allocator };
    var ops = RocOps{
        .env = @ptrCast(&host),
        .roc_alloc = testRocAlloc,
        .roc_dealloc = testRocDealloc,
        .roc_realloc = testRocRealloc,
        .roc_dbg = testRocDbg,
        .roc_expect_failed = testRocExpectFailed,
        .roc_crashed = testRocCrashed,
        .host_fns = undefined,
    };

    const result = try interp2.evalMinimal(resources.expr_idx, &ops);
    const ct_var = can.ModuleEnv.varFrom(resources.expr_idx);
    const rt_var = try interp2.translateTypeVar(resources.module_env, ct_var);
    const rendered = try interp2.renderValueRocWithType(result, rt_var);
    defer std.testing.allocator.free(rendered);
    try std.testing.expectEqualStrings("True", rendered);
}

test "interpreter2: decimal inequality yields False" {
    const roc_src = "0.5 != 0.5";
    const resources = try helpers.parseAndCanonicalizeExpr(std.testing.allocator, roc_src);
    defer helpers.cleanupParseAndCanonical(std.testing.allocator, resources);

    var interp2 = try Interpreter2.init(std.testing.allocator, resources.module_env);
    defer interp2.deinit();

    var host = TestHost{ .allocator = std.testing.allocator };
    var ops = RocOps{
        .env = @ptrCast(&host),
        .roc_alloc = testRocAlloc,
        .roc_dealloc = testRocDealloc,
        .roc_realloc = testRocRealloc,
        .roc_dbg = testRocDbg,
        .roc_expect_failed = testRocExpectFailed,
        .roc_crashed = testRocCrashed,
        .host_fns = undefined,
    };

    const result = try interp2.evalMinimal(resources.expr_idx, &ops);
    const ct_var = can.ModuleEnv.varFrom(resources.expr_idx);
    const rt_var = try interp2.translateTypeVar(resources.module_env, ct_var);
    const rendered = try interp2.renderValueRocWithType(result, rt_var);
    defer std.testing.allocator.free(rendered);
    try std.testing.expectEqualStrings("False", rendered);
}

test "interpreter2: f64 equality False" {
    const roc_src = "3.25f64 == 4.0f64";
    const resources = try helpers.parseAndCanonicalizeExpr(std.testing.allocator, roc_src);
    defer helpers.cleanupParseAndCanonical(std.testing.allocator, resources);

    var interp2 = try Interpreter2.init(std.testing.allocator, resources.module_env);
    defer interp2.deinit();

    var host = TestHost{ .allocator = std.testing.allocator };
    var ops = RocOps{
        .env = @ptrCast(&host),
        .roc_alloc = testRocAlloc,
        .roc_dealloc = testRocDealloc,
        .roc_realloc = testRocRealloc,
        .roc_dbg = testRocDbg,
        .roc_expect_failed = testRocExpectFailed,
        .roc_crashed = testRocCrashed,
        .host_fns = undefined,
    };

    const result = try interp2.evalMinimal(resources.expr_idx, &ops);
    const ct_var = can.ModuleEnv.varFrom(resources.expr_idx);
    const rt_var = try interp2.translateTypeVar(resources.module_env, ct_var);
    const rendered = try interp2.renderValueRocWithType(result, rt_var);
    defer std.testing.allocator.free(rendered);
    try std.testing.expectEqualStrings("False", rendered);
}

test "interpreter2: decimal equality False" {
    const roc_src = "0.125 == 0.25";
    const resources = try helpers.parseAndCanonicalizeExpr(std.testing.allocator, roc_src);
    defer helpers.cleanupParseAndCanonical(std.testing.allocator, resources);

    var interp2 = try Interpreter2.init(std.testing.allocator, resources.module_env);
    defer interp2.deinit();

    var host = TestHost{ .allocator = std.testing.allocator };
    var ops = RocOps{
        .env = @ptrCast(&host),
        .roc_alloc = testRocAlloc,
        .roc_dealloc = testRocDealloc,
        .roc_realloc = testRocRealloc,
        .roc_dbg = testRocDbg,
        .roc_expect_failed = testRocExpectFailed,
        .roc_crashed = testRocCrashed,
        .host_fns = undefined,
    };

    const result = try interp2.evalMinimal(resources.expr_idx, &ops);
    const ct_var = can.ModuleEnv.varFrom(resources.expr_idx);
    const rt_var = try interp2.translateTypeVar(resources.module_env, ct_var);
    const rendered = try interp2.renderValueRocWithType(result, rt_var);
    defer std.testing.allocator.free(rendered);
    try std.testing.expectEqualStrings("False", rendered);
}

test "interpreter2: tuples and records" {
    // Tuple test: (1, 2)
    const src_tuple = "(1, 2)";
    const res_t = try helpers.parseAndCanonicalizeExpr(std.testing.allocator, src_tuple);
    defer helpers.cleanupParseAndCanonical(std.testing.allocator, res_t);
    var it = try Interpreter2.init(std.testing.allocator, res_t.module_env);
    defer it.deinit();
    var host_t = TestHost{ .allocator = std.testing.allocator };
    var ops_t = RocOps{ .env = @ptrCast(&host_t), .roc_alloc = testRocAlloc, .roc_dealloc = testRocDealloc, .roc_realloc = testRocRealloc, .roc_dbg = testRocDbg, .roc_expect_failed = testRocExpectFailed, .roc_crashed = testRocCrashed, .host_fns = undefined };
    const val_t = try it.evalMinimal(res_t.expr_idx, &ops_t);
    const text_t = try it.renderValueRoc(val_t);
    defer std.testing.allocator.free(text_t);
    try std.testing.expectEqualStrings("(1, 2)", text_t);

    // Record test: { x: 1, y: 2 }
    const src_rec = "{ x: 1, y: 2 }";
    const res_r = try helpers.parseAndCanonicalizeExpr(std.testing.allocator, src_rec);
    defer helpers.cleanupParseAndCanonical(std.testing.allocator, res_r);
    var ir = try Interpreter2.init(std.testing.allocator, res_r.module_env);
    defer ir.deinit();
    var host_r = TestHost{ .allocator = std.testing.allocator };
    var ops_r = RocOps{ .env = @ptrCast(&host_r), .roc_alloc = testRocAlloc, .roc_dealloc = testRocDealloc, .roc_realloc = testRocRealloc, .roc_dbg = testRocDbg, .roc_expect_failed = testRocExpectFailed, .roc_crashed = testRocCrashed, .host_fns = undefined };
    const val_r = try ir.evalMinimal(res_r.expr_idx, &ops_r);
    const text_r = try ir.renderValueRoc(val_r);
    defer std.testing.allocator.free(text_r);
    // Sorted field order by name should be "{ x: 1, y: 2 }"
    try std.testing.expectEqualStrings("{ x: 1, y: 2 }", text_r);
}

// Boolean/if support intentionally omitted for now
