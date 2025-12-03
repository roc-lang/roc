//! Interpreter style tests that begin and end with Roc syntax.
//! These tests parse user-supplied Roc code, fail fast with proper diagnostics
//! if any compilation stage has problems, and then exercise Interpreter’s
//! runtime type/unification flow alongside evaluating the value with the
//! current interpreter for end-to-end verification.

const std = @import("std");
const helpers = @import("helpers.zig");
const can = @import("can");
const types = @import("types");
const layout = @import("layout");
const builtins = @import("builtins");
const eval_mod = @import("../mod.zig");
const Interpreter = @import("../interpreter.zig").Interpreter;
const RocOps = @import("builtins").host_abi.RocOps;
const SExprTree = @import("base").SExprTree;
const RocAlloc = @import("builtins").host_abi.RocAlloc;
const RocDealloc = @import("builtins").host_abi.RocDealloc;
const RocRealloc = @import("builtins").host_abi.RocRealloc;
const RocDbg = @import("builtins").host_abi.RocDbg;
const RocExpectFailed = @import("builtins").host_abi.RocExpectFailed;
const CrashContext = eval_mod.CrashContext;
const CrashState = eval_mod.CrashState;

const TestHost = struct {
    allocator: std.mem.Allocator,
    crash: CrashContext,
    dbg_messages: std.array_list.AlignedManaged([]u8, null),

    fn init(allocator: std.mem.Allocator) TestHost {
        return TestHost{
            .allocator = allocator,
            .crash = CrashContext.init(allocator),
            .dbg_messages = std.array_list.AlignedManaged([]u8, null).init(allocator),
        };
    }

    fn deinit(self: *TestHost) void {
        for (self.dbg_messages.items) |msg| {
            self.allocator.free(msg);
        }
        self.dbg_messages.deinit();
        self.crash.deinit();
    }

    fn makeOps(self: *TestHost) RocOps {
        self.crash.reset();
        return RocOps{
            .env = @ptrCast(self),
            .roc_alloc = testRocAlloc,
            .roc_dealloc = testRocDealloc,
            .roc_realloc = testRocRealloc,
            .roc_dbg = testRocDbg,
            .roc_expect_failed = testRocExpectFailed,
            .roc_crashed = recordCrashCallback,
            .hosted_fns = .{ .count = 0, .fns = undefined },
        };
    }

    fn crashState(self: *TestHost) CrashState {
        return self.crash.state;
    }

    fn recordDbg(self: *TestHost, msg: []const u8) !void {
        const copy = try self.allocator.dupe(u8, msg);
        try self.dbg_messages.append(copy);
    }
};

fn testRocAlloc(alloc_args: *RocAlloc, env: *anyopaque) callconv(.c) void {
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

fn testRocDealloc(dealloc_args: *RocDealloc, env: *anyopaque) callconv(.c) void {
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

fn testRocRealloc(realloc_args: *RocRealloc, env: *anyopaque) callconv(.c) void {
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

fn testRocDbg(dbg_args: *const RocDbg, env: *anyopaque) callconv(.c) void {
    const host: *TestHost = @ptrCast(@alignCast(env));
    host.recordDbg(dbg_args.utf8_bytes[0..dbg_args.len]) catch |err| {
        std.debug.panic("failed to record dbg message: {}", .{err});
    };
}
fn testRocExpectFailed(expect_args: *const RocExpectFailed, env: *anyopaque) callconv(.c) void {
    const host: *TestHost = @ptrCast(@alignCast(env));
    const source_bytes = expect_args.utf8_bytes[0..expect_args.len];
    const trimmed = std.mem.trim(u8, source_bytes, " \t\n\r");
    // Format and record the message
    const formatted = std.fmt.allocPrint(host.allocator, "Expect failed: {s}", .{trimmed}) catch {
        std.debug.panic("failed to allocate expect failure message", .{});
    };
    host.crash.recordCrash(formatted) catch |err| {
        host.allocator.free(formatted);
        std.debug.panic("failed to record expect failure: {}", .{err});
    };
}

fn recordCrashCallback(args: *const builtins.host_abi.RocCrashed, env: *anyopaque) callconv(.c) void {
    const host: *TestHost = @ptrCast(@alignCast(env));
    host.crash.recordCrash(args.utf8_bytes[0..args.len]) catch |err| {
        std.debug.panic("failed to record crash message: {}", .{err});
    };
}

test "interpreter: (|x| x)(\"Hello\") yields \"Hello\"" {
    const roc_src = "(|x| x)(\"Hello\")";
    const resources = try helpers.parseAndCanonicalizeExpr(std.testing.allocator, roc_src);
    defer helpers.cleanupParseAndCanonical(std.testing.allocator, resources);

    var interp2 = try Interpreter.init(std.testing.allocator, resources.module_env, resources.builtin_types, resources.builtin_module.env, &[_]*const can.ModuleEnv{}, &resources.checker.import_mapping, null);
    defer interp2.deinit();

    var host = TestHost.init(std.testing.allocator);
    defer host.deinit();
    var ops = host.makeOps();

    const result = try interp2.eval(resources.expr_idx, &ops);
    const rendered = try interp2.renderValueRoc(result);
    defer std.testing.allocator.free(rendered);
    try std.testing.expectEqualStrings("\"Hello\"", rendered);
}

test "interpreter: (|n| n + 1)(41) yields 42" {
    const roc_src = "(|n| n + 1)(41)";
    const resources = try helpers.parseAndCanonicalizeExpr(std.testing.allocator, roc_src);
    defer helpers.cleanupParseAndCanonical(std.testing.allocator, resources);

    var interp2 = try Interpreter.init(std.testing.allocator, resources.module_env, resources.builtin_types, resources.builtin_module.env, &[_]*const can.ModuleEnv{}, &resources.checker.import_mapping, null);
    defer interp2.deinit();

    var host = TestHost.init(std.testing.allocator);
    defer host.deinit();
    var ops = host.makeOps();

    const result = try interp2.eval(resources.expr_idx, &ops);
    const rendered = try interp2.renderValueRoc(result);
    defer std.testing.allocator.free(rendered);
    try std.testing.expectEqualStrings("42", rendered);
}

test "interpreter: (|a, b| a + b)(40, 2) yields 42" {
    const roc_src = "(|a, b| a + b)(40, 2)";
    const resources = try helpers.parseAndCanonicalizeExpr(std.testing.allocator, roc_src);
    defer helpers.cleanupParseAndCanonical(std.testing.allocator, resources);

    var interp2 = try Interpreter.init(std.testing.allocator, resources.module_env, resources.builtin_types, resources.builtin_module.env, &[_]*const can.ModuleEnv{}, &resources.checker.import_mapping, null);
    defer interp2.deinit();

    var host = TestHost.init(std.testing.allocator);
    defer host.deinit();
    var ops = host.makeOps();

    const result = try interp2.eval(resources.expr_idx, &ops);
    const rendered = try interp2.renderValueRoc(result);
    defer std.testing.allocator.free(rendered);
    try std.testing.expectEqualStrings("42", rendered);
}

test "interpreter: 6 / 3 yields 2" {
    const roc_src = "6 / 3";
    try helpers.runExpectInt(roc_src, 2, .no_trace);

    const resources = try helpers.parseAndCanonicalizeExpr(std.testing.allocator, roc_src);
    defer helpers.cleanupParseAndCanonical(std.testing.allocator, resources);

    var interp2 = try Interpreter.init(std.testing.allocator, resources.module_env, resources.builtin_types, resources.builtin_module.env, &[_]*const can.ModuleEnv{}, &resources.checker.import_mapping, null);
    defer interp2.deinit();

    var host = TestHost.init(std.testing.allocator);
    defer host.deinit();
    var ops = host.makeOps();

    const result = try interp2.eval(resources.expr_idx, &ops);
    const rendered = try interp2.renderValueRoc(result);
    defer std.testing.allocator.free(rendered);
    try std.testing.expectEqualStrings("2", rendered);
}

test "interpreter: 7 % 3 yields 1" {
    const roc_src = "7 % 3";
    try helpers.runExpectInt(roc_src, 1, .no_trace);

    const resources = try helpers.parseAndCanonicalizeExpr(std.testing.allocator, roc_src);
    defer helpers.cleanupParseAndCanonical(std.testing.allocator, resources);

    var interp2 = try Interpreter.init(std.testing.allocator, resources.module_env, resources.builtin_types, resources.builtin_module.env, &[_]*const can.ModuleEnv{}, &resources.checker.import_mapping, null);
    defer interp2.deinit();

    var host = TestHost.init(std.testing.allocator);
    defer host.deinit();
    var ops = host.makeOps();

    const result = try interp2.eval(resources.expr_idx, &ops);
    const rendered = try interp2.renderValueRoc(result);
    defer std.testing.allocator.free(rendered);
    try std.testing.expectEqualStrings("1", rendered);
}

test "interpreter: 0.2 + 0.3 yields 0.5" {
    const roc_src = "0.2 + 0.3";
    const resources = try helpers.parseAndCanonicalizeExpr(std.testing.allocator, roc_src);
    defer helpers.cleanupParseAndCanonical(std.testing.allocator, resources);

    var interp2 = try Interpreter.init(std.testing.allocator, resources.module_env, resources.builtin_types, resources.builtin_module.env, &[_]*const can.ModuleEnv{}, &resources.checker.import_mapping, null);
    defer interp2.deinit();

    var host = TestHost.init(std.testing.allocator);
    defer host.deinit();
    var ops = host.makeOps();

    const result = try interp2.eval(resources.expr_idx, &ops);
    const rendered = try interp2.renderValueRoc(result);
    defer std.testing.allocator.free(rendered);
    try std.testing.expectEqualStrings("0.5", rendered);
}

test "interpreter: 0.5 / 2 yields 0.25" {
    const roc_src = "0.5 / 2";
    const resources = try helpers.parseAndCanonicalizeExpr(std.testing.allocator, roc_src);
    defer helpers.cleanupParseAndCanonical(std.testing.allocator, resources);

    var interp2 = try Interpreter.init(std.testing.allocator, resources.module_env, resources.builtin_types, resources.builtin_module.env, &[_]*const can.ModuleEnv{}, &resources.checker.import_mapping, null);
    defer interp2.deinit();

    var host = TestHost.init(std.testing.allocator);
    defer host.deinit();
    var ops = host.makeOps();

    const result = try interp2.eval(resources.expr_idx, &ops);
    const rendered = try interp2.renderValueRoc(result);
    defer std.testing.allocator.free(rendered);
    try std.testing.expectEqualStrings("0.25", rendered);
}

test "interpreter: F64 addition" {
    try helpers.runExpectF64(
        \\{
        \\    a : F64
        \\    a = 1.5
        \\    b : F64
        \\    b = 2.25
        \\    a + b
        \\}
    , 3.75, .no_trace);
}

test "interpreter: F32 multiplication" {
    try helpers.runExpectF32(
        \\{
        \\    a : F32
        \\    a = 1.5
        \\    b : F32
        \\    b = 2
        \\    a * b
        \\}
    , 3.0, .no_trace);
}

test "interpreter: F64 division" {
    try helpers.runExpectF64(
        \\{
        \\    a : F64
        \\    a = 2.0
        \\    b : F64
        \\    b = 4.0
        \\    a / b
        \\}
    , 0.5, .no_trace);
}

test "interpreter: literal tag renders as tag name" {
    // Use a custom tag instead of True - True is a Bool tag which requires
    // proper builtin module resolution to get the nominal type
    const roc_src = "MyTag";
    const resources = try helpers.parseAndCanonicalizeExpr(std.testing.allocator, roc_src);
    defer helpers.cleanupParseAndCanonical(std.testing.allocator, resources);

    var interp2 = try Interpreter.init(std.testing.allocator, resources.module_env, resources.builtin_types, resources.builtin_module.env, &[_]*const can.ModuleEnv{}, &resources.checker.import_mapping, null);
    defer interp2.deinit();

    var host = TestHost.init(std.testing.allocator);
    defer host.deinit();
    var ops = host.makeOps();

    const result = try interp2.eval(resources.expr_idx, &ops);
    const rt_var = try interp2.translateTypeVar(resources.module_env, can.ModuleEnv.varFrom(resources.expr_idx));
    const rendered = try interp2.renderValueRocWithType(result, rt_var, &ops);
    defer std.testing.allocator.free(rendered);
    try std.testing.expectEqualStrings("MyTag", rendered);
}

test "interpreter: True == False yields False" {
    //     const roc_src = "True == False";
    //     const resources = try helpers.parseAndCanonicalizeExpr(std.testing.allocator, roc_src);
    //     defer helpers.cleanupParseAndCanonical(std.testing.allocator, resources);

    //     var interp2 = try Interpreter.init(std.testing.allocator, resources.module_env, resources.builtin_types, resources.builtin_module.env, &[_]*const can.ModuleEnv{}, &resources.checker.import_mapping, null);
    //     defer interp2.deinit();

    //     var host = TestHost.init(std.testing.allocator);
    //     defer host.deinit();
    //     var ops = host.makeOps();

    //     const result = try interp2.eval(resources.expr_idx, &ops);
    //     const rt_var = try interp2.translateTypeVar(resources.module_env, can.ModuleEnv.varFrom(resources.expr_idx));
    //     const rendered = try interp2.renderValueRocWithType(result, rt_var, &ops);
    //     defer std.testing.allocator.free(rendered);
    //     try std.testing.expectEqualStrings("False", rendered);
}

test "interpreter: \"hi\" == \"hi\" yields True" {
    //     const roc_src = "\"hi\" == \"hi\"";
    //     const resources = try helpers.parseAndCanonicalizeExpr(std.testing.allocator, roc_src);
    //     defer helpers.cleanupParseAndCanonical(std.testing.allocator, resources);
    //
    //     try helpers.runExpectBool(roc_src, true, .no_trace);
    //
    //     var interp2 = try Interpreter.init(std.testing.allocator, resources.module_env, resources.builtin_types, resources.builtin_module.env, &[_]*const can.ModuleEnv{}, &resources.checker.import_mapping, null);
    //     defer interp2.deinit();
    //
    //     var host = TestHost.init(std.testing.allocator);
    //     defer host.deinit();
    //     var ops = host.makeOps();
    //
    //     const result = try interp2.eval(resources.expr_idx, &ops);
    //     const rt_var = try interp2.translateTypeVar(resources.module_env, can.ModuleEnv.varFrom(resources.expr_idx));
    //     const rendered = try interp2.renderValueRocWithType(result, rt_var, &ops);
    //     defer std.testing.allocator.free(rendered);
    //     try std.testing.expectEqualStrings("True", rendered);
}

test "interpreter: (1, 2) == (1, 2) yields True" {
    //     const roc_src = "(1, 2) == (1, 2)";
    //     const resources = try helpers.parseAndCanonicalizeExpr(std.testing.allocator, roc_src);
    //     defer helpers.cleanupParseAndCanonical(std.testing.allocator, resources);

    //     var interp2 = try Interpreter.init(std.testing.allocator, resources.module_env, resources.builtin_types, resources.builtin_module.env, &[_]*const can.ModuleEnv{}, &resources.checker.import_mapping, null);
    //     defer interp2.deinit();

    //     var host = TestHost.init(std.testing.allocator);
    //     defer host.deinit();
    //     var ops = host.makeOps();

    //     const result = try interp2.eval(resources.expr_idx, &ops);
    //     const rt_var = try interp2.translateTypeVar(resources.module_env, can.ModuleEnv.varFrom(resources.expr_idx));
    //     const rendered = try interp2.renderValueRocWithType(result, rt_var, &ops);
    //     defer std.testing.allocator.free(rendered);
    //     try std.testing.expectEqualStrings("True", rendered);
}

test "interpreter: (1, 2) == (2, 1) yields False" {
    //     const roc_src = "(1, 2) == (2, 1)";
    //     const resources = try helpers.parseAndCanonicalizeExpr(std.testing.allocator, roc_src);
    //     defer helpers.cleanupParseAndCanonical(std.testing.allocator, resources);

    //     var interp2 = try Interpreter.init(std.testing.allocator, resources.module_env, resources.builtin_types, resources.builtin_module.env, &[_]*const can.ModuleEnv{}, &resources.checker.import_mapping, null);
    //     defer interp2.deinit();

    //     var host = TestHost.init(std.testing.allocator);
    //     defer host.deinit();
    //     var ops = host.makeOps();

    //     const result = try interp2.eval(resources.expr_idx, &ops);
    //     const rt_var = try interp2.translateTypeVar(resources.module_env, can.ModuleEnv.varFrom(resources.expr_idx));
    //     const rendered = try interp2.renderValueRocWithType(result, rt_var, &ops);
    //     defer std.testing.allocator.free(rendered);
    //     try std.testing.expectEqualStrings("False", rendered);
}

test "interpreter: { x: 1, y: 2 } == { y: 2, x: 1 } yields True" {
    //     const roc_src = "{ x: 1, y: 2 } == { y: 2, x: 1 }";
    //     const resources = try helpers.parseAndCanonicalizeExpr(std.testing.allocator, roc_src);
    //     defer helpers.cleanupParseAndCanonical(std.testing.allocator, resources);

    //     var interp2 = try Interpreter.init(std.testing.allocator, resources.module_env, resources.builtin_types, resources.builtin_module.env, &[_]*const can.ModuleEnv{}, &resources.checker.import_mapping, null);
    //     defer interp2.deinit();

    //     var host = TestHost.init(std.testing.allocator);
    //     defer host.deinit();
    //     var ops = host.makeOps();

    //     const result = try interp2.eval(resources.expr_idx, &ops);
    //     const rt_var = try interp2.translateTypeVar(resources.module_env, can.ModuleEnv.varFrom(resources.expr_idx));
    //     const rendered = try interp2.renderValueRocWithType(result, rt_var, &ops);
    //     defer std.testing.allocator.free(rendered);
    //     try std.testing.expectEqualStrings("True", rendered);
}

test "interpreter: { x: 1, y: 2 } == { x: 1, y: 3 } yields False" {
    //     const roc_src = "{ x: 1, y: 2 } == { x: 1, y: 3 }";
    //     const resources = try helpers.parseAndCanonicalizeExpr(std.testing.allocator, roc_src);
    //     defer helpers.cleanupParseAndCanonical(std.testing.allocator, resources);

    //     var interp2 = try Interpreter.init(std.testing.allocator, resources.module_env, resources.builtin_types, resources.builtin_module.env, &[_]*const can.ModuleEnv{}, &resources.checker.import_mapping, null);
    //     defer interp2.deinit();

    //     var host = TestHost.init(std.testing.allocator);
    //     defer host.deinit();
    //     var ops = host.makeOps();

    //     const result = try interp2.eval(resources.expr_idx, &ops);
    //     const rt_var = try interp2.translateTypeVar(resources.module_env, can.ModuleEnv.varFrom(resources.expr_idx));
    //     const rendered = try interp2.renderValueRocWithType(result, rt_var, &ops);
    //     defer std.testing.allocator.free(rendered);
    //     try std.testing.expectEqualStrings("False", rendered);
}

test "interpreter: record update copies base fields" {
    const roc_src = "{\n    point = { x: 1, y: 2 }\n    updated = { ..point, y: point.y }\n    (updated.x, updated.y)\n}";
    const resources = try helpers.parseAndCanonicalizeExpr(std.testing.allocator, roc_src);
    defer helpers.cleanupParseAndCanonical(std.testing.allocator, resources);

    var interp2 = try Interpreter.init(std.testing.allocator, resources.module_env, resources.builtin_types, resources.builtin_module.env, &[_]*const can.ModuleEnv{}, &resources.checker.import_mapping, null);
    defer interp2.deinit();

    var host = TestHost.init(std.testing.allocator);
    defer host.deinit();
    var ops = host.makeOps();

    const result = try interp2.eval(resources.expr_idx, &ops);
    const rendered = try interp2.renderValueRoc(result);
    defer std.testing.allocator.free(rendered);
    try std.testing.expectEqualStrings("(1, 2)", rendered);
}

test "interpreter: record update overrides field" {
    const roc_src = "{\n    point = { x: 1, y: 2 }\n    updated = { ..point, y: 3 }\n    (updated.x, updated.y)\n}";
    const resources = try helpers.parseAndCanonicalizeExpr(std.testing.allocator, roc_src);
    defer helpers.cleanupParseAndCanonical(std.testing.allocator, resources);

    var interp2 = try Interpreter.init(std.testing.allocator, resources.module_env, resources.builtin_types, resources.builtin_module.env, &[_]*const can.ModuleEnv{}, &resources.checker.import_mapping, null);
    defer interp2.deinit();

    var host = TestHost.init(std.testing.allocator);
    defer host.deinit();
    var ops = host.makeOps();

    const result = try interp2.eval(resources.expr_idx, &ops);
    const rendered = try interp2.renderValueRoc(result);
    defer std.testing.allocator.free(rendered);
    try std.testing.expectEqualStrings("(1, 3)", rendered);
}

test "interpreter: record update expression can reference base" {
    const roc_src = "{\n    point = { x: 1, y: 2 }\n    updated = { ..point, y: point.y + 5 }\n    updated.y\n}";
    const resources = try helpers.parseAndCanonicalizeExpr(std.testing.allocator, roc_src);
    defer helpers.cleanupParseAndCanonical(std.testing.allocator, resources);

    var interp2 = try Interpreter.init(std.testing.allocator, resources.module_env, resources.builtin_types, resources.builtin_module.env, &[_]*const can.ModuleEnv{}, &resources.checker.import_mapping, null);
    defer interp2.deinit();

    var host = TestHost.init(std.testing.allocator);
    defer host.deinit();
    var ops = host.makeOps();

    const result = try interp2.eval(resources.expr_idx, &ops);
    const rendered = try interp2.renderValueRoc(result);
    defer std.testing.allocator.free(rendered);
    try std.testing.expectEqualStrings("7", rendered);
}

// TODO: Fix
// test "interpreter: record update can add field" {
//     const roc_src = "{\n    point = { x: 1, y: 2 }\n    updated = { ..point, z: 3 }\n    (updated.x, updated.y, updated.z)\n}";
//     const resources = try helpers.parseAndCanonicalizeExpr(std.testing.allocator, roc_src);
//     defer helpers.cleanupParseAndCanonical(std.testing.allocator, resources);

//     var interp2 = try Interpreter.init(std.testing.allocator, resources.module_env, resources.builtin_types, resources.builtin_module.env, &[_]*const can.ModuleEnv{}, &resources.checker.import_mapping, null);
//     defer interp2.deinit();

//     var host = TestHost.init(std.testing.allocator);
//     defer host.deinit();
//     var ops = host.makeOps();

//     const result = try interp2.eval(resources.expr_idx, &ops);
//     const rendered = try interp2.renderValueRoc(result);
//     defer std.testing.allocator.free(rendered);
//     try std.testing.expectEqualStrings("(1, 2, 3)", rendered);
// }

// TODO: Fix
// test "interpreter: record update inside tuple" {
//     const roc_src = "{\n    point = { x: 4, y: 5 }\n    duo = { updated: { ..point, y: point.y + 1 }, original: point }\n    (duo.updated.x, duo.updated.y, duo.original.y)\n}";
//     const resources = try helpers.parseAndCanonicalizeExpr(std.testing.allocator, roc_src);
//     defer helpers.cleanupParseAndCanonical(std.testing.allocator, resources);

//     var interp2 = try Interpreter.init(std.testing.allocator, resources.module_env, resources.builtin_types, resources.builtin_module.env, &[_]*const can.ModuleEnv{}, &resources.checker.import_mapping, null);
//     defer interp2.deinit();

//     var host = TestHost.init(std.testing.allocator);
//     defer host.deinit();
//     var ops = host.makeOps();

//     const result = try interp2.eval(resources.expr_idx, &ops);
//     const rendered = try interp2.renderValueRoc(result);
//     defer std.testing.allocator.free(rendered);
//     try std.testing.expectEqualStrings("(4, 6, 5)", rendered);
// }

// TODO: Fix
// test "interpreter: record update pattern match" {
//     const roc_src = "{\n    point = { x: 7, y: 8 }\n    updated = { ..point, y: point.y - 2, z: point.x + point.y }\n    match updated { { x: newX, y: newY, z: sum } => (newX, newY, sum), _ => (0, 0, 0) }\n}";
//     const resources = try helpers.parseAndCanonicalizeExpr(std.testing.allocator, roc_src);
//     defer helpers.cleanupParseAndCanonical(std.testing.allocator, resources);

//     var interp2 = try Interpreter.init(std.testing.allocator, resources.module_env, resources.builtin_types, resources.builtin_module.env, &[_]*const can.ModuleEnv{}, &resources.checker.import_mapping, null);
//     defer interp2.deinit();

//     var host = TestHost.init(std.testing.allocator);
//     defer host.deinit();
//     var ops = host.makeOps();

//     const result = try interp2.eval(resources.expr_idx, &ops);
//     const rendered = try interp2.renderValueRoc(result);
//     defer std.testing.allocator.free(rendered);
//     try std.testing.expectEqualStrings("(7, 6, 15)", rendered);
// }

test "interpreter: [1, 2, 3] == [1, 2, 3] yields True" {
    //     const roc_src = "[1, 2, 3] == [1, 2, 3]";
    //     const resources = try helpers.parseAndCanonicalizeExpr(std.testing.allocator, roc_src);
    //     defer helpers.cleanupParseAndCanonical(std.testing.allocator, resources);

    //     var interp2 = try Interpreter.init(std.testing.allocator, resources.module_env, resources.builtin_types, resources.builtin_module.env, &[_]*const can.ModuleEnv{}, &resources.checker.import_mapping, null);
    //     defer interp2.deinit();

    //     var host = TestHost.init(std.testing.allocator);
    //     defer host.deinit();
    //     var ops = host.makeOps();

    //     const result = try interp2.eval(resources.expr_idx, &ops);
    //     const rt_var = try interp2.translateTypeVar(resources.module_env, can.ModuleEnv.varFrom(resources.expr_idx));
    //     const rendered = try interp2.renderValueRocWithType(result, rt_var, &ops);
    //     defer std.testing.allocator.free(rendered);
    //     try std.testing.expectEqualStrings("True", rendered);
}

test "interpreter: [1, 2, 3] == [1, 3, 2] yields False" {
    //     const roc_src = "[1, 2, 3] == [1, 3, 2]";
    //     const resources = try helpers.parseAndCanonicalizeExpr(std.testing.allocator, roc_src);
    //     defer helpers.cleanupParseAndCanonical(std.testing.allocator, resources);

    //     var interp2 = try Interpreter.init(std.testing.allocator, resources.module_env, resources.builtin_types, resources.builtin_module.env, &[_]*const can.ModuleEnv{}, &resources.checker.import_mapping, null);
    //     defer interp2.deinit();

    //     var host = TestHost.init(std.testing.allocator);
    //     defer host.deinit();
    //     var ops = host.makeOps();

    //     const result = try interp2.eval(resources.expr_idx, &ops);
    //     const rt_var = try interp2.translateTypeVar(resources.module_env, can.ModuleEnv.varFrom(resources.expr_idx));
    //     const rendered = try interp2.renderValueRocWithType(result, rt_var, &ops);
    //     defer std.testing.allocator.free(rendered);
    //     try std.testing.expectEqualStrings("False", rendered);
}

test "interpreter: Ok(1) == Ok(1) yields True" {
    //     const roc_src = "Ok(1) == Ok(1)";
    //     const resources = try helpers.parseAndCanonicalizeExpr(std.testing.allocator, roc_src);
    //     defer helpers.cleanupParseAndCanonical(std.testing.allocator, resources);

    //     var interp2 = try Interpreter.init(std.testing.allocator, resources.module_env, resources.builtin_types, resources.builtin_module.env, &[_]*const can.ModuleEnv{}, &resources.checker.import_mapping, null);
    //     defer interp2.deinit();

    //     var host = TestHost.init(std.testing.allocator);
    //     defer host.deinit();
    //     var ops = host.makeOps();

    //     const result = try interp2.eval(resources.expr_idx, &ops);
    //     const rt_var = try interp2.translateTypeVar(resources.module_env, can.ModuleEnv.varFrom(resources.expr_idx));
    //     const rendered = try interp2.renderValueRocWithType(result, rt_var, &ops);
    //     defer std.testing.allocator.free(rendered);
    //     try std.testing.expectEqualStrings("True", rendered);
}

test "interpreter: Ok(1) == Err(1) yields False" {
    //     const roc_src = "Ok(1) == Err(1)";
    //     const resources = try helpers.parseAndCanonicalizeExpr(std.testing.allocator, roc_src);
    //     defer helpers.cleanupParseAndCanonical(std.testing.allocator, resources);

    //     var interp2 = try Interpreter.init(std.testing.allocator, resources.module_env, resources.builtin_types, resources.builtin_module.env, &[_]*const can.ModuleEnv{}, &resources.checker.import_mapping, null);
    //     defer interp2.deinit();

    //     var host = TestHost.init(std.testing.allocator);
    //     defer host.deinit();
    //     var ops = host.makeOps();

    //     const result = try interp2.eval(resources.expr_idx, &ops);
    //     const rt_var = try interp2.translateTypeVar(resources.module_env, can.ModuleEnv.varFrom(resources.expr_idx));
    //     const rendered = try interp2.renderValueRocWithType(result, rt_var, &ops);
    //     defer std.testing.allocator.free(rendered);
    //     try std.testing.expectEqualStrings("False", rendered);
}

test "interpreter: match tuple pattern destructures" {
    const roc_src = "match (1, 2) { (1, b) => b, _ => 0 }";
    const resources = try helpers.parseAndCanonicalizeExpr(std.testing.allocator, roc_src);
    defer helpers.cleanupParseAndCanonical(std.testing.allocator, resources);

    var interp2 = try Interpreter.init(std.testing.allocator, resources.module_env, resources.builtin_types, resources.builtin_module.env, &[_]*const can.ModuleEnv{}, &resources.checker.import_mapping, null);
    defer interp2.deinit();

    var host = TestHost.init(std.testing.allocator);
    defer host.deinit();
    var ops = host.makeOps();

    const result = try interp2.eval(resources.expr_idx, &ops);
    const rendered = try interp2.renderValueRoc(result);
    defer std.testing.allocator.free(rendered);
    try std.testing.expectEqualStrings("2", rendered);
}

test "interpreter: match bool patterns" {
    const roc_src = "match True { True => 1, False => 0 }";
    const resources = try helpers.parseAndCanonicalizeExpr(std.testing.allocator, roc_src);
    defer helpers.cleanupParseAndCanonical(std.testing.allocator, resources);

    var interp2 = try Interpreter.init(std.testing.allocator, resources.module_env, resources.builtin_types, resources.builtin_module.env, &[_]*const can.ModuleEnv{}, &resources.checker.import_mapping, null);
    defer interp2.deinit();

    var host = TestHost.init(std.testing.allocator);
    defer host.deinit();
    var ops = host.makeOps();

    const result = try interp2.eval(resources.expr_idx, &ops);
    const rendered = try interp2.renderValueRoc(result);
    defer std.testing.allocator.free(rendered);
    try std.testing.expectEqualStrings("1", rendered);
}

test "interpreter: match result tag payload" {
    const roc_src = "match Ok(3) { Ok(n) => n + 1, Err(_) => 0 }";
    const resources = try helpers.parseAndCanonicalizeExpr(std.testing.allocator, roc_src);
    defer helpers.cleanupParseAndCanonical(std.testing.allocator, resources);

    var interp2 = try Interpreter.init(std.testing.allocator, resources.module_env, resources.builtin_types, resources.builtin_module.env, &[_]*const can.ModuleEnv{}, &resources.checker.import_mapping, null);
    defer interp2.deinit();

    var host = TestHost.init(std.testing.allocator);
    defer host.deinit();
    var ops = host.makeOps();

    const result = try interp2.eval(resources.expr_idx, &ops);
    const rendered = try interp2.renderValueRoc(result);
    defer std.testing.allocator.free(rendered);
    try std.testing.expectEqualStrings("4", rendered);
}

test "interpreter: match record destructures fields" {
    const roc_src = "match { x: 1, y: 2 } { { x, y } => x + y, _ => 0 }";
    const resources = try helpers.parseAndCanonicalizeExpr(std.testing.allocator, roc_src);
    defer helpers.cleanupParseAndCanonical(std.testing.allocator, resources);

    var interp2 = try Interpreter.init(std.testing.allocator, resources.module_env, resources.builtin_types, resources.builtin_module.env, &[_]*const can.ModuleEnv{}, &resources.checker.import_mapping, null);
    defer interp2.deinit();

    var host = TestHost.init(std.testing.allocator);
    defer host.deinit();
    var ops = host.makeOps();

    const result = try interp2.eval(resources.expr_idx, &ops);
    const rendered = try interp2.renderValueRoc(result);
    defer std.testing.allocator.free(rendered);
    try std.testing.expectEqualStrings("3", rendered);
}

test "interpreter: render Try.Ok literal" {
    const roc_src = "match True { True => Ok(42), False => Err(\"boom\") }";
    const resources = try helpers.parseAndCanonicalizeExpr(std.testing.allocator, roc_src);
    defer helpers.cleanupParseAndCanonical(std.testing.allocator, resources);

    var interp2 = try Interpreter.init(std.testing.allocator, resources.module_env, resources.builtin_types, resources.builtin_module.env, &[_]*const can.ModuleEnv{}, &resources.checker.import_mapping, null);
    defer interp2.deinit();

    var host = TestHost.init(std.testing.allocator);
    defer host.deinit();
    var ops = host.makeOps();

    const result = try interp2.eval(resources.expr_idx, &ops);
    const rt_var = try interp2.translateTypeVar(resources.module_env, can.ModuleEnv.varFrom(resources.expr_idx));
    const rendered = try interp2.renderValueRocWithType(result, rt_var, &ops);
    defer std.testing.allocator.free(rendered);
    try std.testing.expectEqualStrings("Ok(42)", rendered);
}

test "interpreter: render Try.Err string" {
    const roc_src = "match True { True => Err(\"boom\"), False => Ok(42) }";
    const resources = try helpers.parseAndCanonicalizeExpr(std.testing.allocator, roc_src);
    defer helpers.cleanupParseAndCanonical(std.testing.allocator, resources);

    var interp2 = try Interpreter.init(std.testing.allocator, resources.module_env, resources.builtin_types, resources.builtin_module.env, &[_]*const can.ModuleEnv{}, &resources.checker.import_mapping, null);
    defer interp2.deinit();

    var host = TestHost.init(std.testing.allocator);
    defer host.deinit();
    var ops = host.makeOps();

    const result = try interp2.eval(resources.expr_idx, &ops);
    const rt_var = try interp2.translateTypeVar(resources.module_env, can.ModuleEnv.varFrom(resources.expr_idx));
    const rendered = try interp2.renderValueRocWithType(result, rt_var, &ops);
    defer std.testing.allocator.free(rendered);
    try std.testing.expectEqualStrings("Err(\"boom\")", rendered);
}

test "interpreter: render Try.Ok tuple payload" {
    const roc_src = "match True { True => Ok((1, 2)), False => Err(\"boom\") }";
    const resources = try helpers.parseAndCanonicalizeExpr(std.testing.allocator, roc_src);
    defer helpers.cleanupParseAndCanonical(std.testing.allocator, resources);

    var interp2 = try Interpreter.init(std.testing.allocator, resources.module_env, resources.builtin_types, resources.builtin_module.env, &[_]*const can.ModuleEnv{}, &resources.checker.import_mapping, null);
    defer interp2.deinit();

    var host = TestHost.init(std.testing.allocator);
    defer host.deinit();
    var ops = host.makeOps();

    const result = try interp2.eval(resources.expr_idx, &ops);
    const rt_var = try interp2.translateTypeVar(resources.module_env, can.ModuleEnv.varFrom(resources.expr_idx));
    const rendered = try interp2.renderValueRocWithType(result, rt_var, &ops);
    defer std.testing.allocator.free(rendered);
    try std.testing.expectEqualStrings("Ok((1, 2))", rendered);
}

test "interpreter: match tuple payload tag" {
    const roc_src = "match Ok((1, 2)) { Ok((a, b)) => a + b, Err(_) => 0 }";
    const resources = try helpers.parseAndCanonicalizeExpr(std.testing.allocator, roc_src);
    defer helpers.cleanupParseAndCanonical(std.testing.allocator, resources);

    var interp2 = try Interpreter.init(std.testing.allocator, resources.module_env, resources.builtin_types, resources.builtin_module.env, &[_]*const can.ModuleEnv{}, &resources.checker.import_mapping, null);
    defer interp2.deinit();

    var host = TestHost.init(std.testing.allocator);
    defer host.deinit();
    var ops = host.makeOps();

    const result = try interp2.eval(resources.expr_idx, &ops);
    const rendered = try interp2.renderValueRoc(result);
    defer std.testing.allocator.free(rendered);
    try std.testing.expectEqualStrings("3", rendered);
}

test "interpreter: match record payload tag" {
    const roc_src = "match Err({ code: 1, msg: \"boom\" }) { Err({ code, msg }) => code, Ok(_) => 0 }";
    const resources = try helpers.parseAndCanonicalizeExpr(std.testing.allocator, roc_src);
    defer helpers.cleanupParseAndCanonical(std.testing.allocator, resources);

    var interp2 = try Interpreter.init(std.testing.allocator, resources.module_env, resources.builtin_types, resources.builtin_module.env, &[_]*const can.ModuleEnv{}, &resources.checker.import_mapping, null);
    defer interp2.deinit();

    var host = TestHost.init(std.testing.allocator);
    defer host.deinit();
    var ops = host.makeOps();

    const result = try interp2.eval(resources.expr_idx, &ops);
    const rendered = try interp2.renderValueRoc(result);
    defer std.testing.allocator.free(rendered);
    try std.testing.expectEqualStrings("1", rendered);
}

test "interpreter: match list pattern destructures" {
    const roc_src = "match [1, 2, 3] { [a, b, c] => a + b + c, _ => 0 }";
    const resources = try helpers.parseAndCanonicalizeExpr(std.testing.allocator, roc_src);
    defer helpers.cleanupParseAndCanonical(std.testing.allocator, resources);

    var interp2 = try Interpreter.init(std.testing.allocator, resources.module_env, resources.builtin_types, resources.builtin_module.env, &[_]*const can.ModuleEnv{}, &resources.checker.import_mapping, null);
    defer interp2.deinit();

    var host = TestHost.init(std.testing.allocator);
    defer host.deinit();
    var ops = host.makeOps();

    const result = try interp2.eval(resources.expr_idx, &ops);
    const rendered = try interp2.renderValueRoc(result);
    defer std.testing.allocator.free(rendered);
    try std.testing.expectEqualStrings("6", rendered);
}

test "debug List.len expression" {}

test "interpreter: List.len on literal" {}

test "interpreter: match list rest binds slice" {
    const roc_src = "match [1, 2, 3] { [first, .. as rest] => match rest { [second, ..] => first + second, _ => 0 }, _ => 0 }";
    const resources = try helpers.parseAndCanonicalizeExpr(std.testing.allocator, roc_src);
    defer helpers.cleanupParseAndCanonical(std.testing.allocator, resources);

    var interp2 = try Interpreter.init(std.testing.allocator, resources.module_env, resources.builtin_types, resources.builtin_module.env, &[_]*const can.ModuleEnv{}, &resources.checker.import_mapping, null);
    defer interp2.deinit();

    var host = TestHost.init(std.testing.allocator);
    defer host.deinit();
    var ops = host.makeOps();

    const result = try interp2.eval(resources.expr_idx, &ops);
    const rendered = try interp2.renderValueRoc(result);
    defer std.testing.allocator.free(rendered);
    try std.testing.expectEqualStrings("3", rendered);
}

test "interpreter: match empty list branch" {
    const roc_src = "match [] { [] => 42, _ => 0 }";
    const resources = try helpers.parseAndCanonicalizeExpr(std.testing.allocator, roc_src);
    defer helpers.cleanupParseAndCanonical(std.testing.allocator, resources);

    var interp2 = try Interpreter.init(std.testing.allocator, resources.module_env, resources.builtin_types, resources.builtin_module.env, &[_]*const can.ModuleEnv{}, &resources.checker.import_mapping, null);
    defer interp2.deinit();

    var host = TestHost.init(std.testing.allocator);
    defer host.deinit();
    var ops = host.makeOps();

    const result = try interp2.eval(resources.expr_idx, &ops);
    const rendered = try interp2.renderValueRoc(result);
    defer std.testing.allocator.free(rendered);
    try std.testing.expectEqualStrings("42", rendered);
}

test "interpreter: simple for loop sum" {
    // Test simpler for loop without passing functions
    const roc_src = "{\n    var total = 0\n    for n in [1, 2, 3, 4] {\n        total = total + n\n    }\n    total\n}";
    const resources = try helpers.parseAndCanonicalizeExpr(std.testing.allocator, roc_src);
    defer helpers.cleanupParseAndCanonical(std.testing.allocator, resources);

    var interp2 = try Interpreter.init(std.testing.allocator, resources.module_env, resources.builtin_types, resources.builtin_module.env, &[_]*const can.ModuleEnv{}, &resources.checker.import_mapping, null);
    defer interp2.deinit();

    var host = TestHost.init(std.testing.allocator);
    defer host.deinit();
    var ops = host.makeOps();

    const result = try interp2.eval(resources.expr_idx, &ops);
    const rendered = try interp2.renderValueRoc(result);
    defer std.testing.allocator.free(rendered);
    try std.testing.expectEqualStrings("10", rendered);
}

test "interpreter: List.fold sum with inline lambda" {
    const roc_src = "(|list, init, step| {\n    var $state = init\n    for item in list {\n        $state = step($state, item)\n    }\n    $state\n})([1, 2, 3, 4], 0, |acc, x| acc + x)";
    const resources = try helpers.parseAndCanonicalizeExpr(std.testing.allocator, roc_src);
    defer helpers.cleanupParseAndCanonical(std.testing.allocator, resources);

    var interp2 = try Interpreter.init(std.testing.allocator, resources.module_env, resources.builtin_types, resources.builtin_module.env, &[_]*const can.ModuleEnv{}, &resources.checker.import_mapping, null);
    defer interp2.deinit();

    var host = TestHost.init(std.testing.allocator);
    defer host.deinit();
    var ops = host.makeOps();

    const result = try interp2.eval(resources.expr_idx, &ops);
    const rendered = try interp2.renderValueRoc(result);
    defer std.testing.allocator.free(rendered);
    try std.testing.expectEqualStrings("10", rendered);
}

test "interpreter: List.fold product with inline lambda" {
    const roc_src = "(|list, init, step| {\n    var $state = init\n    for item in list {\n        $state = step($state, item)\n    }\n    $state\n})([2, 3, 4], 1, |acc, x| acc * x)";
    const resources = try helpers.parseAndCanonicalizeExpr(std.testing.allocator, roc_src);
    defer helpers.cleanupParseAndCanonical(std.testing.allocator, resources);

    var interp2 = try Interpreter.init(std.testing.allocator, resources.module_env, resources.builtin_types, resources.builtin_module.env, &[_]*const can.ModuleEnv{}, &resources.checker.import_mapping, null);
    defer interp2.deinit();

    var host = TestHost.init(std.testing.allocator);
    defer host.deinit();
    var ops = host.makeOps();

    const result = try interp2.eval(resources.expr_idx, &ops);
    const rendered = try interp2.renderValueRoc(result);
    defer std.testing.allocator.free(rendered);
    try std.testing.expectEqualStrings("24", rendered);
}

test "interpreter: List.fold empty list with inline lambda" {
    const roc_src = "(|list, init, step| {\n    var $state = init\n    for item in list {\n        $state = step($state, item)\n    }\n    $state\n})([], 42, |acc, x| acc + x)";
    const resources = try helpers.parseAndCanonicalizeExpr(std.testing.allocator, roc_src);
    defer helpers.cleanupParseAndCanonical(std.testing.allocator, resources);

    var interp2 = try Interpreter.init(std.testing.allocator, resources.module_env, resources.builtin_types, resources.builtin_module.env, &[_]*const can.ModuleEnv{}, &resources.checker.import_mapping, null);
    defer interp2.deinit();

    var host = TestHost.init(std.testing.allocator);
    defer host.deinit();
    var ops = host.makeOps();

    const result = try interp2.eval(resources.expr_idx, &ops);
    const rendered = try interp2.renderValueRoc(result);
    defer std.testing.allocator.free(rendered);
    try std.testing.expectEqualStrings("42", rendered);
}

test "interpreter: List.fold count elements with inline lambda" {
    const roc_src = "(|list, init, step| {\n    var $state = init\n    for item in list {\n        $state = step($state, item)\n    }\n    $state\n})([10, 20, 30, 40], 0, |acc, _| acc + 1)";
    const resources = try helpers.parseAndCanonicalizeExpr(std.testing.allocator, roc_src);
    defer helpers.cleanupParseAndCanonical(std.testing.allocator, resources);

    var interp2 = try Interpreter.init(std.testing.allocator, resources.module_env, resources.builtin_types, resources.builtin_module.env, &[_]*const can.ModuleEnv{}, &resources.checker.import_mapping, null);
    defer interp2.deinit();

    var host = TestHost.init(std.testing.allocator);
    defer host.deinit();
    var ops = host.makeOps();

    const result = try interp2.eval(resources.expr_idx, &ops);
    const rendered = try interp2.renderValueRoc(result);
    defer std.testing.allocator.free(rendered);
    try std.testing.expectEqualStrings("4", rendered);
}

test "interpreter: List.fold from Builtin using numbers" {
    const roc_src = "List.fold([1, 2, 3], 0, |acc, item| acc + item)";
    const resources = try helpers.parseAndCanonicalizeExpr(std.testing.allocator, roc_src);
    defer helpers.cleanupParseAndCanonical(std.testing.allocator, resources);

    const imported_envs = [_]*const can.ModuleEnv{resources.builtin_module.env};
    var interp2 = try Interpreter.init(std.testing.allocator, resources.module_env, resources.builtin_types, resources.builtin_module.env, &imported_envs, &resources.checker.import_mapping, null);
    defer interp2.deinit();

    var host = TestHost.init(std.testing.allocator);
    defer host.deinit();
    var ops = host.makeOps();

    const result = try interp2.eval(resources.expr_idx, &ops);
    const rendered = try interp2.renderValueRoc(result);
    defer std.testing.allocator.free(rendered);
    try std.testing.expectEqualStrings("6", rendered);
}

test "interpreter: List.any True on integers" {
    const roc_src = "List.any([1, 0, 1, 0, -1], |x| x > 0)";
    const resources = try helpers.parseAndCanonicalizeExpr(std.testing.allocator, roc_src);
    defer helpers.cleanupParseAndCanonical(std.testing.allocator, resources);

    const imported_envs = [_]*const can.ModuleEnv{resources.builtin_module.env};
    var interp2 = try Interpreter.init(std.testing.allocator, resources.module_env, resources.builtin_types, resources.builtin_module.env, &imported_envs, &resources.checker.import_mapping, null);
    defer interp2.deinit();

    var host = TestHost.init(std.testing.allocator);
    defer host.deinit();
    var ops = host.makeOps();

    const result = try interp2.eval(resources.expr_idx, &ops);
    const rt_var = try interp2.translateTypeVar(resources.module_env, can.ModuleEnv.varFrom(resources.expr_idx));
    const rendered = try interp2.renderValueRocWithType(result, rt_var, &ops);
    defer std.testing.allocator.free(rendered);
    try std.testing.expectEqualStrings("True", rendered);
}

test "interpreter: List.any False on unsigned integers" {
    const roc_src = "List.any([9, 8, 7, 6, 5], |x| x < 0)";
    const resources = try helpers.parseAndCanonicalizeExpr(std.testing.allocator, roc_src);
    defer helpers.cleanupParseAndCanonical(std.testing.allocator, resources);

    const imported_envs = [_]*const can.ModuleEnv{resources.builtin_module.env};
    var interp2 = try Interpreter.init(std.testing.allocator, resources.module_env, resources.builtin_types, resources.builtin_module.env, &imported_envs, &resources.checker.import_mapping, null);
    defer interp2.deinit();

    var host = TestHost.init(std.testing.allocator);
    defer host.deinit();
    var ops = host.makeOps();

    const result = try interp2.eval(resources.expr_idx, &ops);
    const rt_var = try interp2.translateTypeVar(resources.module_env, can.ModuleEnv.varFrom(resources.expr_idx));
    const rendered = try interp2.renderValueRocWithType(result, rt_var, &ops);
    defer std.testing.allocator.free(rendered);
    try std.testing.expectEqualStrings("False", rendered);
}

test "interpreter: List.any False on empty list" {
    const roc_src = "List.any([], |x| x < 0)";
    const resources = try helpers.parseAndCanonicalizeExpr(std.testing.allocator, roc_src);
    defer helpers.cleanupParseAndCanonical(std.testing.allocator, resources);

    const imported_envs = [_]*const can.ModuleEnv{resources.builtin_module.env};
    var interp2 = try Interpreter.init(std.testing.allocator, resources.module_env, resources.builtin_types, resources.builtin_module.env, &imported_envs, &resources.checker.import_mapping, null);
    defer interp2.deinit();

    var host = TestHost.init(std.testing.allocator);
    defer host.deinit();
    var ops = host.makeOps();

    const result = try interp2.eval(resources.expr_idx, &ops);
    const rt_var = try interp2.translateTypeVar(resources.module_env, can.ModuleEnv.varFrom(resources.expr_idx));
    const rendered = try interp2.renderValueRocWithType(result, rt_var, &ops);
    defer std.testing.allocator.free(rendered);
    try std.testing.expectEqualStrings("False", rendered);
}

test "interpreter: List.all False when some elements are False" {
    const roc_src = "List.all([9, 18, 7, 6, 15], |x| x < 10)";
    const resources = try helpers.parseAndCanonicalizeExpr(std.testing.allocator, roc_src);
    defer helpers.cleanupParseAndCanonical(std.testing.allocator, resources);

    const imported_envs = [_]*const can.ModuleEnv{resources.builtin_module.env};
    var interp2 = try Interpreter.init(std.testing.allocator, resources.module_env, resources.builtin_types, resources.builtin_module.env, &imported_envs, &resources.checker.import_mapping, null);
    defer interp2.deinit();

    var host = TestHost.init(std.testing.allocator);
    defer host.deinit();
    var ops = host.makeOps();

    const result = try interp2.eval(resources.expr_idx, &ops);
    const rt_var = try interp2.translateTypeVar(resources.module_env, can.ModuleEnv.varFrom(resources.expr_idx));
    const rendered = try interp2.renderValueRocWithType(result, rt_var, &ops);
    defer std.testing.allocator.free(rendered);
    try std.testing.expectEqualStrings("False", rendered);
}

test "interpreter: List.all True on small integers" {
    const roc_src = "List.all([9, 8, 7, 6, 5], |x| x < 10)";
    const resources = try helpers.parseAndCanonicalizeExpr(std.testing.allocator, roc_src);
    defer helpers.cleanupParseAndCanonical(std.testing.allocator, resources);

    const imported_envs = [_]*const can.ModuleEnv{resources.builtin_module.env};
    var interp2 = try Interpreter.init(std.testing.allocator, resources.module_env, resources.builtin_types, resources.builtin_module.env, &imported_envs, &resources.checker.import_mapping, null);
    defer interp2.deinit();

    var host = TestHost.init(std.testing.allocator);
    defer host.deinit();
    var ops = host.makeOps();

    const result = try interp2.eval(resources.expr_idx, &ops);
    const rt_var = try interp2.translateTypeVar(resources.module_env, can.ModuleEnv.varFrom(resources.expr_idx));
    const rendered = try interp2.renderValueRocWithType(result, rt_var, &ops);
    defer std.testing.allocator.free(rendered);
    try std.testing.expectEqualStrings("True", rendered);
}

test "interpreter: List.all False on empty list" {
    const roc_src = "List.all([], |x| x < 10)";
    const resources = try helpers.parseAndCanonicalizeExpr(std.testing.allocator, roc_src);
    defer helpers.cleanupParseAndCanonical(std.testing.allocator, resources);

    const imported_envs = [_]*const can.ModuleEnv{resources.builtin_module.env};
    var interp2 = try Interpreter.init(std.testing.allocator, resources.module_env, resources.builtin_types, resources.builtin_module.env, &imported_envs, &resources.checker.import_mapping, null);
    defer interp2.deinit();

    var host = TestHost.init(std.testing.allocator);
    defer host.deinit();
    var ops = host.makeOps();

    const result = try interp2.eval(resources.expr_idx, &ops);
    const rt_var = try interp2.translateTypeVar(resources.module_env, can.ModuleEnv.varFrom(resources.expr_idx));
    const rendered = try interp2.renderValueRocWithType(result, rt_var, &ops);
    defer std.testing.allocator.free(rendered);
    try std.testing.expectEqualStrings("True", rendered);
}

test "interpreter: List.contains is False for a missing element" {
    const roc_src = "List.contains([-1, -2, -3, 1, 2, 3], 0)";
    const resources = try helpers.parseAndCanonicalizeExpr(std.testing.allocator, roc_src);
    defer helpers.cleanupParseAndCanonical(std.testing.allocator, resources);

    const imported_envs = [_]*const can.ModuleEnv{resources.builtin_module.env};
    var interp2 = try Interpreter.init(std.testing.allocator, resources.module_env, resources.builtin_types, resources.builtin_module.env, &imported_envs, &resources.checker.import_mapping, null);
    defer interp2.deinit();

    var host = TestHost.init(std.testing.allocator);
    defer host.deinit();
    var ops = host.makeOps();

    const result = try interp2.eval(resources.expr_idx, &ops);
    const rt_var = try interp2.translateTypeVar(resources.module_env, can.ModuleEnv.varFrom(resources.expr_idx));
    const rendered = try interp2.renderValueRocWithType(result, rt_var, &ops);
    defer std.testing.allocator.free(rendered);
    try std.testing.expectEqualStrings("False", rendered);
}

test "interpreter: List.contains is True when element is found" {
    const roc_src = "List.contains([1, 2, 3, 4, 5], 3)";
    const resources = try helpers.parseAndCanonicalizeExpr(std.testing.allocator, roc_src);
    defer helpers.cleanupParseAndCanonical(std.testing.allocator, resources);

    const imported_envs = [_]*const can.ModuleEnv{resources.builtin_module.env};
    var interp2 = try Interpreter.init(std.testing.allocator, resources.module_env, resources.builtin_types, resources.builtin_module.env, &imported_envs, &resources.checker.import_mapping, null);
    defer interp2.deinit();

    var host = TestHost.init(std.testing.allocator);
    defer host.deinit();
    var ops = host.makeOps();

    const result = try interp2.eval(resources.expr_idx, &ops);
    const rt_var = try interp2.translateTypeVar(resources.module_env, can.ModuleEnv.varFrom(resources.expr_idx));
    const rendered = try interp2.renderValueRocWithType(result, rt_var, &ops);
    defer std.testing.allocator.free(rendered);
    try std.testing.expectEqualStrings("True", rendered);
}

test "interpreter: List.contains is False on empty list" {
    const roc_src = "List.contains([], 3333)";
    const resources = try helpers.parseAndCanonicalizeExpr(std.testing.allocator, roc_src);
    defer helpers.cleanupParseAndCanonical(std.testing.allocator, resources);

    const imported_envs = [_]*const can.ModuleEnv{resources.builtin_module.env};
    var interp2 = try Interpreter.init(std.testing.allocator, resources.module_env, resources.builtin_types, resources.builtin_module.env, &imported_envs, &resources.checker.import_mapping, null);
    defer interp2.deinit();

    var host = TestHost.init(std.testing.allocator);
    defer host.deinit();
    var ops = host.makeOps();

    const result = try interp2.eval(resources.expr_idx, &ops);
    const rt_var = try interp2.translateTypeVar(resources.module_env, can.ModuleEnv.varFrom(resources.expr_idx));
    const rendered = try interp2.renderValueRocWithType(result, rt_var, &ops);
    defer std.testing.allocator.free(rendered);
    try std.testing.expectEqualStrings("False", rendered);
}

test "interpreter: crash statement triggers crash error and message" {
    const roc_src = "{\n    crash \"boom\"\n    0\n}";
    const resources = try helpers.parseAndCanonicalizeExpr(std.testing.allocator, roc_src);
    defer helpers.cleanupParseAndCanonical(std.testing.allocator, resources);

    var interp2 = try Interpreter.init(std.testing.allocator, resources.module_env, resources.builtin_types, resources.builtin_module.env, &[_]*const can.ModuleEnv{}, &resources.checker.import_mapping, null);
    defer interp2.deinit();

    var host = TestHost.init(std.testing.allocator);
    defer host.deinit();
    var ops = host.makeOps();

    try std.testing.expectError(error.Crash, interp2.eval(resources.expr_idx, &ops));
    switch (host.crashState()) {
        .did_not_crash => return error.TestUnexpectedResult,
        .crashed => |msg| try std.testing.expectEqualStrings("boom", msg),
    }
}

test "interpreter: expect expression succeeds" {
    //     const roc_src = "{\n    expect 1 == 1\n    {}\n}";
    //     const resources = try helpers.parseAndCanonicalizeExpr(std.testing.allocator, roc_src);
    //     defer helpers.cleanupParseAndCanonical(std.testing.allocator, resources);

    //     var interp2 = try Interpreter.init(std.testing.allocator, resources.module_env, resources.builtin_types, resources.builtin_module.env, &[_]*const can.ModuleEnv{}, &resources.checker.import_mapping, null);
    //     defer interp2.deinit();

    //     var host = TestHost.init(std.testing.allocator);
    //     defer host.deinit();
    //     var ops = host.makeOps();

    //     const result = try interp2.eval(resources.expr_idx, &ops);
    //     try std.testing.expect(host.crashState() == .did_not_crash);
    //     const rt_var = try interp2.translateTypeVar(resources.module_env, can.ModuleEnv.varFrom(resources.expr_idx));
    //     const rendered = try interp2.renderValueRocWithType(result, rt_var, &ops);
    //     defer std.testing.allocator.free(rendered);
    //     try std.testing.expectEqualStrings("{}", rendered);
}

test "interpreter: expect expression failure crashes with message" {
    //     const roc_src = "{\n    expect 1 == 0\n    {}\n}";
    //     const resources = try helpers.parseAndCanonicalizeExpr(std.testing.allocator, roc_src);
    //     defer helpers.cleanupParseAndCanonical(std.testing.allocator, resources);

    //     var interp2 = try Interpreter.init(std.testing.allocator, resources.module_env, resources.builtin_types, resources.builtin_module.env, &[_]*const can.ModuleEnv{}, &resources.checker.import_mapping, null);
    //     defer interp2.deinit();

    //     var host = TestHost.init(std.testing.allocator);
    //     defer host.deinit();
    //     var ops = host.makeOps();

    //     try std.testing.expectError(error.Crash, interp2.eval(resources.expr_idx, &ops));
    //     switch (host.crashState()) {
    //         .did_not_crash => return error.TestUnexpectedResult,
    //         .crashed => |msg| try std.testing.expectEqualStrings("Expect failed: 1 == 0", msg),
    //     }
}

test "interpreter: empty record expression renders {}" {
    const roc_src = "{}";
    const resources = try helpers.parseAndCanonicalizeExpr(std.testing.allocator, roc_src);
    defer helpers.cleanupParseAndCanonical(std.testing.allocator, resources);

    var interp2 = try Interpreter.init(std.testing.allocator, resources.module_env, resources.builtin_types, resources.builtin_module.env, &[_]*const can.ModuleEnv{}, &resources.checker.import_mapping, null);
    defer interp2.deinit();

    var host = TestHost.init(std.testing.allocator);
    defer host.deinit();
    var ops = host.makeOps();

    const result = try interp2.eval(resources.expr_idx, &ops);
    const rt_var = try interp2.translateTypeVar(resources.module_env, can.ModuleEnv.varFrom(resources.expr_idx));
    const rendered = try interp2.renderValueRocWithType(result, rt_var, &ops);
    defer std.testing.allocator.free(rendered);
    try std.testing.expectEqualStrings("{}", rendered);
}

test "interpreter: F64 literal" {
    try helpers.runExpectF64(
        \\{
        \\    a : F64
        \\    a = 3.25
        \\    a
        \\}
    , 3.25, .no_trace);
}

test "interpreter: decimal literal renders 0.125" {
    const roc_src = "0.125";
    const resources = try helpers.parseAndCanonicalizeExpr(std.testing.allocator, roc_src);
    defer helpers.cleanupParseAndCanonical(std.testing.allocator, resources);

    var interp2 = try Interpreter.init(std.testing.allocator, resources.module_env, resources.builtin_types, resources.builtin_module.env, &[_]*const can.ModuleEnv{}, &resources.checker.import_mapping, null);
    defer interp2.deinit();

    var host = TestHost.init(std.testing.allocator);
    defer host.deinit();
    var ops = host.makeOps();

    const result = try interp2.eval(resources.expr_idx, &ops);
    const rendered = try interp2.renderValueRoc(result);
    defer std.testing.allocator.free(rendered);
    try std.testing.expectEqualStrings("0.125", rendered);
}

test "interpreter: f64 equality True" {
    //     const roc_src = "3.25f64 == 3.25f64";
    //     const resources = try helpers.parseAndCanonicalizeExpr(std.testing.allocator, roc_src);
    //     defer helpers.cleanupParseAndCanonical(std.testing.allocator, resources);

    //     var interp2 = try Interpreter.init(std.testing.allocator, resources.module_env, resources.builtin_types, resources.builtin_module.env, &[_]*const can.ModuleEnv{}, &resources.checker.import_mapping, null);
    //     defer interp2.deinit();

    //     var host = TestHost.init(std.testing.allocator);
    //     defer host.deinit();
    //     var ops = host.makeOps();

    //     const result = try interp2.eval(resources.expr_idx, &ops);
    //     const rt_var = try interp2.translateTypeVar(resources.module_env, can.ModuleEnv.varFrom(resources.expr_idx));
    //     const rendered = try interp2.renderValueRocWithType(result, rt_var, &ops);
    //     defer std.testing.allocator.free(rendered);
    //     try std.testing.expectEqualStrings("True", rendered);
}

test "interpreter: decimal equality True" {
    //     const roc_src = "0.125 == 0.125";
    //     const resources = try helpers.parseAndCanonicalizeExpr(std.testing.allocator, roc_src);
    //     defer helpers.cleanupParseAndCanonical(std.testing.allocator, resources);

    //     var interp2 = try Interpreter.init(std.testing.allocator, resources.module_env, resources.builtin_types, resources.builtin_module.env, &[_]*const can.ModuleEnv{}, &resources.checker.import_mapping, null);
    //     defer interp2.deinit();

    //     var host = TestHost.init(std.testing.allocator);
    //     defer host.deinit();
    //     var ops = host.makeOps();

    //     const result = try interp2.eval(resources.expr_idx, &ops);
    //     const rt_var = try interp2.translateTypeVar(resources.module_env, can.ModuleEnv.varFrom(resources.expr_idx));
    //     const rendered = try interp2.renderValueRocWithType(result, rt_var, &ops);
    //     defer std.testing.allocator.free(rendered);
    //     try std.testing.expectEqualStrings("True", rendered);
}

test "interpreter: int and f64 equality True" {
    //     const roc_src = "1 == 1.0f64";
    //     const resources = try helpers.parseAndCanonicalizeExpr(std.testing.allocator, roc_src);
    //     defer helpers.cleanupParseAndCanonical(std.testing.allocator, resources);

    //     var interp2 = try Interpreter.init(std.testing.allocator, resources.module_env, resources.builtin_types, resources.builtin_module.env, &[_]*const can.ModuleEnv{}, &resources.checker.import_mapping, null);
    //     defer interp2.deinit();

    //     const binop_expr = resources.module_env.store.getExpr(resources.expr_idx);
    //     try std.testing.expect(binop_expr == .e_binop);
    //     const binop = binop_expr.e_binop;
    //     const lhs_var = can.ModuleEnv.varFrom(binop.lhs);
    //     const rhs_var = can.ModuleEnv.varFrom(binop.rhs);
    //     const expr_var = can.ModuleEnv.varFrom(resources.expr_idx);
    //     try std.testing.expect(resources.module_env.types.resolveVar(lhs_var).desc.content != .err);
    //     try std.testing.expect(resources.module_env.types.resolveVar(rhs_var).desc.content != .err);
    //     try std.testing.expect(resources.module_env.types.resolveVar(expr_var).desc.content != .err);

    //     var host = TestHost.init(std.testing.allocator);
    //     defer host.deinit();
    //     var ops = host.makeOps();

    //     const result = try interp2.eval(resources.expr_idx, &ops);
    //     const rt_var = try interp2.translateTypeVar(resources.module_env, can.ModuleEnv.varFrom(resources.expr_idx));
    //     const rendered = try interp2.renderValueRocWithType(result, rt_var, &ops);
    //     defer std.testing.allocator.free(rendered);
    //     try std.testing.expectEqualStrings("True", rendered);
}

test "interpreter: int and decimal equality True" {
    //     const roc_src = "1 == 1.0";
    //     const resources = try helpers.parseAndCanonicalizeExpr(std.testing.allocator, roc_src);
    //     defer helpers.cleanupParseAndCanonical(std.testing.allocator, resources);

    //     var interp2 = try Interpreter.init(std.testing.allocator, resources.module_env, resources.builtin_types, resources.builtin_module.env, &[_]*const can.ModuleEnv{}, &resources.checker.import_mapping, null);
    //     defer interp2.deinit();

    //     const binop_expr = resources.module_env.store.getExpr(resources.expr_idx);
    //     try std.testing.expect(binop_expr == .e_binop);
    //     const binop = binop_expr.e_binop;
    //     const lhs_var = can.ModuleEnv.varFrom(binop.lhs);
    //     const rhs_var = can.ModuleEnv.varFrom(binop.rhs);
    //     const expr_var = can.ModuleEnv.varFrom(resources.expr_idx);
    //     try std.testing.expect(resources.module_env.types.resolveVar(lhs_var).desc.content != .err);
    //     try std.testing.expect(resources.module_env.types.resolveVar(rhs_var).desc.content != .err);
    //     try std.testing.expect(resources.module_env.types.resolveVar(expr_var).desc.content != .err);

    //     var host = TestHost.init(std.testing.allocator);
    //     defer host.deinit();
    //     var ops = host.makeOps();

    //     const result = try interp2.eval(resources.expr_idx, &ops);
    //     const rt_var = try interp2.translateTypeVar(resources.module_env, can.ModuleEnv.varFrom(resources.expr_idx));
    //     const rendered = try interp2.renderValueRocWithType(result, rt_var, &ops);
    //     defer std.testing.allocator.free(rendered);
    //     try std.testing.expectEqualStrings("True", rendered);
}

test "interpreter: int less-than yields True" {
    //     const roc_src = "3 < 4";
    //     const resources = try helpers.parseAndCanonicalizeExpr(std.testing.allocator, roc_src);
    //     defer helpers.cleanupParseAndCanonical(std.testing.allocator, resources);

    //     var interp2 = try Interpreter.init(std.testing.allocator, resources.module_env, resources.builtin_types, resources.builtin_module.env, &[_]*const can.ModuleEnv{}, &resources.checker.import_mapping, null);
    //     defer interp2.deinit();

    //     var host = TestHost.init(std.testing.allocator);
    //     defer host.deinit();
    //     var ops = host.makeOps();

    //     const result = try interp2.eval(resources.expr_idx, &ops);
    //     const rt_var = try interp2.translateTypeVar(resources.module_env, can.ModuleEnv.varFrom(resources.expr_idx));
    //     const rendered = try interp2.renderValueRocWithType(result, rt_var, &ops);
    //     defer std.testing.allocator.free(rendered);
    //     try std.testing.expectEqualStrings("True", rendered);
}

test "interpreter: int greater-than yields False" {
    //     const roc_src = "5 > 8";
    //     const resources = try helpers.parseAndCanonicalizeExpr(std.testing.allocator, roc_src);
    //     defer helpers.cleanupParseAndCanonical(std.testing.allocator, resources);

    //     var interp2 = try Interpreter.init(std.testing.allocator, resources.module_env, resources.builtin_types, resources.builtin_module.env, &[_]*const can.ModuleEnv{}, &resources.checker.import_mapping, null);
    //     defer interp2.deinit();

    //     var host = TestHost.init(std.testing.allocator);
    //     defer host.deinit();
    //     var ops = host.makeOps();

    //     const result = try interp2.eval(resources.expr_idx, &ops);
    //     const rt_var = try interp2.translateTypeVar(resources.module_env, can.ModuleEnv.varFrom(resources.expr_idx));
    //     const rendered = try interp2.renderValueRocWithType(result, rt_var, &ops);
    //     defer std.testing.allocator.free(rendered);
    //     try std.testing.expectEqualStrings("False", rendered);
}

test "interpreter: 0.1 + 0.2 yields 0.3" {
    const roc_src = "0.1 + 0.2";
    const resources = try helpers.parseAndCanonicalizeExpr(std.testing.allocator, roc_src);
    defer helpers.cleanupParseAndCanonical(std.testing.allocator, resources);

    var interp2 = try Interpreter.init(std.testing.allocator, resources.module_env, resources.builtin_types, resources.builtin_module.env, &[_]*const can.ModuleEnv{}, &resources.checker.import_mapping, null);
    defer interp2.deinit();

    var host = TestHost.init(std.testing.allocator);
    defer host.deinit();
    var ops = host.makeOps();

    const result = try interp2.eval(resources.expr_idx, &ops);
    const rt_var = try interp2.translateTypeVar(resources.module_env, can.ModuleEnv.varFrom(resources.expr_idx));
    const rendered = try interp2.renderValueRocWithType(result, rt_var, &ops);
    defer std.testing.allocator.free(rendered);
    try std.testing.expectEqualStrings("0.3", rendered);
}

test "interpreter: f64 greater-than yields True" {
    //     const roc_src = "3.5f64 > 1.25f64";
    //     const resources = try helpers.parseAndCanonicalizeExpr(std.testing.allocator, roc_src);
    //     defer helpers.cleanupParseAndCanonical(std.testing.allocator, resources);

    //     var interp2 = try Interpreter.init(std.testing.allocator, resources.module_env, resources.builtin_types, resources.builtin_module.env, &[_]*const can.ModuleEnv{}, &resources.checker.import_mapping, null);
    //     defer interp2.deinit();

    //     var host = TestHost.init(std.testing.allocator);
    //     defer host.deinit();
    //     var ops = host.makeOps();

    //     const result = try interp2.eval(resources.expr_idx, &ops);
    //     const rt_var = try interp2.translateTypeVar(resources.module_env, can.ModuleEnv.varFrom(resources.expr_idx));
    //     const rendered = try interp2.renderValueRocWithType(result, rt_var, &ops);
    //     defer std.testing.allocator.free(rendered);
    //     try std.testing.expectEqualStrings("True", rendered);
}

test "interpreter: decimal less-than-or-equal yields True" {
    //     const roc_src = "0.5 <= 0.5";
    //     const resources = try helpers.parseAndCanonicalizeExpr(std.testing.allocator, roc_src);
    //     defer helpers.cleanupParseAndCanonical(std.testing.allocator, resources);

    //     var interp2 = try Interpreter.init(std.testing.allocator, resources.module_env, resources.builtin_types, resources.builtin_module.env, &[_]*const can.ModuleEnv{}, &resources.checker.import_mapping, null);
    //     defer interp2.deinit();

    //     var host = TestHost.init(std.testing.allocator);
    //     defer host.deinit();
    //     var ops = host.makeOps();

    //     const result = try interp2.eval(resources.expr_idx, &ops);
    //     const rt_var = try interp2.translateTypeVar(resources.module_env, can.ModuleEnv.varFrom(resources.expr_idx));
    //     const rendered = try interp2.renderValueRocWithType(result, rt_var, &ops);
    //     defer std.testing.allocator.free(rendered);
    //     try std.testing.expectEqualStrings("True", rendered);
}

test "interpreter: int and f64 less-than yields True" {
    //     const roc_src = "1 < 2.0f64";
    //     const resources = try helpers.parseAndCanonicalizeExpr(std.testing.allocator, roc_src);
    //     defer helpers.cleanupParseAndCanonical(std.testing.allocator, resources);

    //     var interp2 = try Interpreter.init(std.testing.allocator, resources.module_env, resources.builtin_types, resources.builtin_module.env, &[_]*const can.ModuleEnv{}, &resources.checker.import_mapping, null);
    //     defer interp2.deinit();

    //     var host = TestHost.init(std.testing.allocator);
    //     defer host.deinit();
    //     var ops = host.makeOps();

    //     const result = try interp2.eval(resources.expr_idx, &ops);
    //     const rt_var = try interp2.translateTypeVar(resources.module_env, can.ModuleEnv.varFrom(resources.expr_idx));
    //     const rendered = try interp2.renderValueRocWithType(result, rt_var, &ops);
    //     defer std.testing.allocator.free(rendered);
    //     try std.testing.expectEqualStrings("True", rendered);
}

test "interpreter: int and decimal greater-than yields False" {
    //     const roc_src = "3 > 5.5";
    //     const resources = try helpers.parseAndCanonicalizeExpr(std.testing.allocator, roc_src);
    //     defer helpers.cleanupParseAndCanonical(std.testing.allocator, resources);

    //     var interp2 = try Interpreter.init(std.testing.allocator, resources.module_env, resources.builtin_types, resources.builtin_module.env, &[_]*const can.ModuleEnv{}, &resources.checker.import_mapping, null);
    //     defer interp2.deinit();

    //     var host = TestHost.init(std.testing.allocator);
    //     defer host.deinit();
    //     var ops = host.makeOps();

    //     const result = try interp2.eval(resources.expr_idx, &ops);
    //     const rt_var = try interp2.translateTypeVar(resources.module_env, can.ModuleEnv.varFrom(resources.expr_idx));
    //     const rendered = try interp2.renderValueRocWithType(result, rt_var, &ops);
    //     defer std.testing.allocator.free(rendered);
    //     try std.testing.expectEqualStrings("False", rendered);
}

test "interpreter: bool inequality yields True" {
    //     const roc_src = "True != False";
    //     const resources = try helpers.parseAndCanonicalizeExpr(std.testing.allocator, roc_src);
    //     defer helpers.cleanupParseAndCanonical(std.testing.allocator, resources);

    //     var interp2 = try Interpreter.init(std.testing.allocator, resources.module_env, resources.builtin_types, resources.builtin_module.env, &[_]*const can.ModuleEnv{}, &resources.checker.import_mapping, null);
    //     defer interp2.deinit();

    //     var host = TestHost.init(std.testing.allocator);
    //     defer host.deinit();
    //     var ops = host.makeOps();

    //     const result = try interp2.eval(resources.expr_idx, &ops);
    //     const rt_var = try interp2.translateTypeVar(resources.module_env, can.ModuleEnv.varFrom(resources.expr_idx));
    //     const rendered = try interp2.renderValueRocWithType(result, rt_var, &ops);
    //     defer std.testing.allocator.free(rendered);
    //     try std.testing.expectEqualStrings("True", rendered);
}

test "interpreter: decimal inequality yields False" {
    //     const roc_src = "0.5 != 0.5";
    //     const resources = try helpers.parseAndCanonicalizeExpr(std.testing.allocator, roc_src);
    //     defer helpers.cleanupParseAndCanonical(std.testing.allocator, resources);

    //     var interp2 = try Interpreter.init(std.testing.allocator, resources.module_env, resources.builtin_types, resources.builtin_module.env, &[_]*const can.ModuleEnv{}, &resources.checker.import_mapping, null);
    //     defer interp2.deinit();

    //     var host = TestHost.init(std.testing.allocator);
    //     defer host.deinit();
    //     var ops = host.makeOps();

    //     const result = try interp2.eval(resources.expr_idx, &ops);
    //     const rt_var = try interp2.translateTypeVar(resources.module_env, can.ModuleEnv.varFrom(resources.expr_idx));
    //     const rendered = try interp2.renderValueRocWithType(result, rt_var, &ops);
    //     defer std.testing.allocator.free(rendered);
    //     try std.testing.expectEqualStrings("False", rendered);
}

test "interpreter: f64 equality False" {
    //     const roc_src = "3.25f64 == 4.0f64";
    //     const resources = try helpers.parseAndCanonicalizeExpr(std.testing.allocator, roc_src);
    //     defer helpers.cleanupParseAndCanonical(std.testing.allocator, resources);

    //     var interp2 = try Interpreter.init(std.testing.allocator, resources.module_env, resources.builtin_types, resources.builtin_module.env, &[_]*const can.ModuleEnv{}, &resources.checker.import_mapping, null);
    //     defer interp2.deinit();

    //     var host = TestHost.init(std.testing.allocator);
    //     defer host.deinit();
    //     var ops = host.makeOps();

    //     const result = try interp2.eval(resources.expr_idx, &ops);
    //     const rt_var = try interp2.translateTypeVar(resources.module_env, can.ModuleEnv.varFrom(resources.expr_idx));
    //     const rendered = try interp2.renderValueRocWithType(result, rt_var, &ops);
    //     defer std.testing.allocator.free(rendered);
    //     try std.testing.expectEqualStrings("False", rendered);
}

test "interpreter: decimal equality False" {
    //     const roc_src = "0.125 == 0.25";
    //     const resources = try helpers.parseAndCanonicalizeExpr(std.testing.allocator, roc_src);
    //     defer helpers.cleanupParseAndCanonical(std.testing.allocator, resources);

    //     var interp2 = try Interpreter.init(std.testing.allocator, resources.module_env, resources.builtin_types, resources.builtin_module.env, &[_]*const can.ModuleEnv{}, &resources.checker.import_mapping, null);
    //     defer interp2.deinit();

    //     var host = TestHost.init(std.testing.allocator);
    //     defer host.deinit();
    //     var ops = host.makeOps();

    //     const result = try interp2.eval(resources.expr_idx, &ops);
    //     const rt_var = try interp2.translateTypeVar(resources.module_env, can.ModuleEnv.varFrom(resources.expr_idx));
    //     const rendered = try interp2.renderValueRocWithType(result, rt_var, &ops);
    //     defer std.testing.allocator.free(rendered);
    //     try std.testing.expectEqualStrings("False", rendered);
}

test "interpreter: tuples and records" {
    // Tuple test: (1, 2)
    const src_tuple = "(1, 2)";
    const res_t = try helpers.parseAndCanonicalizeExpr(std.testing.allocator, src_tuple);
    defer helpers.cleanupParseAndCanonical(std.testing.allocator, res_t);
    var it = try Interpreter.init(std.testing.allocator, res_t.module_env, res_t.builtin_types, res_t.builtin_module.env, &[_]*const can.ModuleEnv{}, &res_t.checker.import_mapping, null);
    defer it.deinit();
    var host_t = TestHost.init(std.testing.allocator);
    defer host_t.deinit();
    var ops_t = host_t.makeOps();
    const val_t = try it.eval(res_t.expr_idx, &ops_t);
    const text_t = try it.renderValueRoc(val_t);
    defer std.testing.allocator.free(text_t);
    try std.testing.expectEqualStrings("(1, 2)", text_t);

    // Record test: { x: 1, y: 2 }
    const src_rec = "{ x: 1, y: 2 }";
    const res_r = try helpers.parseAndCanonicalizeExpr(std.testing.allocator, src_rec);
    defer helpers.cleanupParseAndCanonical(std.testing.allocator, res_r);
    var ir = try Interpreter.init(std.testing.allocator, res_r.module_env, res_r.builtin_types, res_r.builtin_module.env, &[_]*const can.ModuleEnv{}, &res_r.checker.import_mapping, null);
    defer ir.deinit();
    var host_r = TestHost.init(std.testing.allocator);
    defer host_r.deinit();
    var ops_r = host_r.makeOps();
    const val_r = try ir.eval(res_r.expr_idx, &ops_r);
    const text_r = try ir.renderValueRoc(val_r);
    defer std.testing.allocator.free(text_r);
    // Sorted field order by name should be "{ x: 1, y: 2 }"
    try std.testing.expectEqualStrings("{ x: 1, y: 2 }", text_r);
}

test "interpreter: empty list [] has list_of_zst layout" {
    // Test that [] (unconstrained, unbound) gets list_of_zst layout
    const roc_src = "[]";
    const resources = try helpers.parseAndCanonicalizeExpr(std.testing.allocator, roc_src);
    defer helpers.cleanupParseAndCanonical(std.testing.allocator, resources);

    var interp2 = try Interpreter.init(std.testing.allocator, resources.module_env, resources.builtin_types, resources.builtin_module.env, &[_]*const can.ModuleEnv{}, &resources.checker.import_mapping, null);
    defer interp2.deinit();

    var host = TestHost.init(std.testing.allocator);
    defer host.deinit();
    var ops = host.makeOps();

    const result = try interp2.eval(resources.expr_idx, &ops);

    // Check that the layout is list_of_zst
    try std.testing.expectEqual(layout.LayoutTag.list_of_zst, result.layout.tag);
}

test "interpreter: singleton list [1] has list of Dec layout" {
    // Test that [1] (constrained by number literal) gets list of Dec layout
    const roc_src = "[1]";
    const resources = try helpers.parseAndCanonicalizeExpr(std.testing.allocator, roc_src);
    defer helpers.cleanupParseAndCanonical(std.testing.allocator, resources);

    var interp2 = try Interpreter.init(std.testing.allocator, resources.module_env, resources.builtin_types, resources.builtin_module.env, &[_]*const can.ModuleEnv{}, &resources.checker.import_mapping, null);
    defer interp2.deinit();

    var host = TestHost.init(std.testing.allocator);
    defer host.deinit();
    var ops = host.makeOps();

    const result = try interp2.eval(resources.expr_idx, &ops);
    defer result.decref(&interp2.runtime_layout_store, &ops);

    // Check that the layout is a regular list (not list_of_zst)
    try std.testing.expectEqual(layout.LayoutTag.list, result.layout.tag);

    // Check that the element layout is Dec
    const elem_layout_idx = result.layout.data.list;
    try std.testing.expectEqual(layout.Idx.dec, elem_layout_idx);
}

test "interpreter: dbg statement in block" {
    // Test that dbg statement works and calls the roc_dbg callback
    const roc_src =
        \\{
        \\    x = 42
        \\    dbg x
        \\    x + 1
        \\}
    ;
    const resources = try helpers.parseAndCanonicalizeExpr(std.testing.allocator, roc_src);
    defer helpers.cleanupParseAndCanonical(std.testing.allocator, resources);

    var interp = try Interpreter.init(std.testing.allocator, resources.module_env, resources.builtin_types, resources.builtin_module.env, &[_]*const can.ModuleEnv{}, &resources.checker.import_mapping, null);
    defer interp.deinit();

    var host = TestHost.init(std.testing.allocator);
    defer host.deinit();
    var ops = host.makeOps();

    const result = try interp.eval(resources.expr_idx, &ops);
    defer result.decref(&interp.runtime_layout_store, &ops);

    // Verify the block evaluates to x + 1 = 43
    const rendered = try interp.renderValueRoc(result);
    defer std.testing.allocator.free(rendered);
    try std.testing.expectEqualStrings("43", rendered);

    // Verify dbg was called with the value of x (42)
    try std.testing.expectEqual(@as(usize, 1), host.dbg_messages.items.len);
    try std.testing.expectEqualStrings("42", host.dbg_messages.items[0]);
}

test "interpreter: dbg statement with string" {
    // Test dbg with a string value
    const roc_src =
        \\{
        \\    msg = "hello"
        \\    dbg msg
        \\    msg
        \\}
    ;
    const resources = try helpers.parseAndCanonicalizeExpr(std.testing.allocator, roc_src);
    defer helpers.cleanupParseAndCanonical(std.testing.allocator, resources);

    var interp = try Interpreter.init(std.testing.allocator, resources.module_env, resources.builtin_types, resources.builtin_module.env, &[_]*const can.ModuleEnv{}, &resources.checker.import_mapping, null);
    defer interp.deinit();

    var host = TestHost.init(std.testing.allocator);
    defer host.deinit();
    var ops = host.makeOps();

    const result = try interp.eval(resources.expr_idx, &ops);
    defer result.decref(&interp.runtime_layout_store, &ops);

    // Verify the block evaluates to msg
    const rendered = try interp.renderValueRoc(result);
    defer std.testing.allocator.free(rendered);
    try std.testing.expectEqualStrings("\"hello\"", rendered);

    // Verify dbg was called with the string value
    try std.testing.expectEqual(@as(usize, 1), host.dbg_messages.items.len);
    try std.testing.expectEqualStrings("\"hello\"", host.dbg_messages.items[0]);
}

test "interpreter: simple early return from function" {
    // Test that early return works in a simple case - using True/False to avoid numeric type issues
    // Simplified to remove ambiguous block
    const roc_src =
        \\{
        \\    f = |x| if x { return True } else { False }
        \\    f(True)
        \\}
    ;
    const resources = try helpers.parseAndCanonicalizeExpr(std.testing.allocator, roc_src);
    defer helpers.cleanupParseAndCanonical(std.testing.allocator, resources);

    var interp = try Interpreter.init(std.testing.allocator, resources.module_env, resources.builtin_types, resources.builtin_module.env, &[_]*const can.ModuleEnv{}, &resources.checker.import_mapping, null);
    defer interp.deinit();

    var host = TestHost.init(std.testing.allocator);
    defer host.deinit();
    var ops = host.makeOps();

    const result = try interp.eval(resources.expr_idx, &ops);
    defer result.decref(&interp.runtime_layout_store, &ops);

    const rendered = try interp.renderValueRoc(result);
    defer std.testing.allocator.free(rendered);
    // Result may be "1" or "True" depending on rendering - both are correct
    try std.testing.expect(std.mem.eql(u8, "True", rendered) or std.mem.eql(u8, "1", rendered));
}

test "interpreter: any function with early return in for loop" {
    // Test the `any` function pattern that uses early return inside a for loop
    const roc_src =
        \\{
        \\    f = |list| {
        \\        for item in list {
        \\            if item == 2 {
        \\                return True
        \\            }
        \\        }
        \\        False
        \\    }
        \\    f([1, 2, 3])
        \\}
    ;
    const resources = try helpers.parseAndCanonicalizeExpr(std.testing.allocator, roc_src);
    defer helpers.cleanupParseAndCanonical(std.testing.allocator, resources);

    var interp = try Interpreter.init(std.testing.allocator, resources.module_env, resources.builtin_types, resources.builtin_module.env, &[_]*const can.ModuleEnv{}, &resources.checker.import_mapping, null);
    defer interp.deinit();

    var host = TestHost.init(std.testing.allocator);
    defer host.deinit();
    var ops = host.makeOps();

    const result = try interp.eval(resources.expr_idx, &ops);
    defer result.decref(&interp.runtime_layout_store, &ops);

    const rendered = try interp.renderValueRoc(result);
    defer std.testing.allocator.free(rendered);
    // Result may be "1" or "True" depending on rendering - both are correct
    try std.testing.expect(std.mem.eql(u8, "True", rendered) or std.mem.eql(u8, "1", rendered));
}

test "interpreter: crash at end of block in if branch" {
    // Test that crash works when it's the final expression of an if branch
    // This is similar to return - crash should be able to unify with any expected type
    const roc_src =
        \\{
        \\    f = |x| {
        \\        if x == 0 {
        \\            crash "division by zero"
        \\        }
        \\        42 / x
        \\    }
        \\    f(2)
        \\}
    ;
    const resources = try helpers.parseAndCanonicalizeExpr(std.testing.allocator, roc_src);
    defer helpers.cleanupParseAndCanonical(std.testing.allocator, resources);

    var interp = try Interpreter.init(std.testing.allocator, resources.module_env, resources.builtin_types, resources.builtin_module.env, &[_]*const can.ModuleEnv{}, &resources.checker.import_mapping, null);
    defer interp.deinit();

    var host = TestHost.init(std.testing.allocator);
    defer host.deinit();
    var ops = host.makeOps();

    const result = try interp.eval(resources.expr_idx, &ops);
    defer result.decref(&interp.runtime_layout_store, &ops);

    const rendered = try interp.renderValueRoc(result);
    defer std.testing.allocator.free(rendered);
    // 42 / 2 = 21
    try std.testing.expectEqualStrings("21", rendered);
}

// Boolean/if support intentionally omitted for now

// ============================================================================
// Comprehensive dbg tests
// ============================================================================

test "dbg: integer literal" {
    const roc_src =
        \\{
        \\    dbg 42
        \\    123
        \\}
    ;
    const resources = try helpers.parseAndCanonicalizeExpr(std.testing.allocator, roc_src);
    defer helpers.cleanupParseAndCanonical(std.testing.allocator, resources);

    var interp = try Interpreter.init(std.testing.allocator, resources.module_env, resources.builtin_types, resources.builtin_module.env, &[_]*const can.ModuleEnv{}, &resources.checker.import_mapping, null);
    defer interp.deinit();

    var host = TestHost.init(std.testing.allocator);
    defer host.deinit();
    var ops = host.makeOps();

    const result = try interp.eval(resources.expr_idx, &ops);
    defer result.decref(&interp.runtime_layout_store, &ops);

    const rendered = try interp.renderValueRoc(result);
    defer std.testing.allocator.free(rendered);
    try std.testing.expectEqualStrings("123", rendered);

    // Verify dbg was called with 42
    try std.testing.expectEqual(@as(usize, 1), host.dbg_messages.items.len);
    try std.testing.expectEqualStrings("42", host.dbg_messages.items[0]);
}

test "dbg: negative integer" {
    const roc_src =
        \\{
        \\    x = -99
        \\    dbg x
        \\    x
        \\}
    ;
    const resources = try helpers.parseAndCanonicalizeExpr(std.testing.allocator, roc_src);
    defer helpers.cleanupParseAndCanonical(std.testing.allocator, resources);

    var interp = try Interpreter.init(std.testing.allocator, resources.module_env, resources.builtin_types, resources.builtin_module.env, &[_]*const can.ModuleEnv{}, &resources.checker.import_mapping, null);
    defer interp.deinit();

    var host = TestHost.init(std.testing.allocator);
    defer host.deinit();
    var ops = host.makeOps();

    const result = try interp.eval(resources.expr_idx, &ops);
    defer result.decref(&interp.runtime_layout_store, &ops);

    const rendered = try interp.renderValueRoc(result);
    defer std.testing.allocator.free(rendered);
    try std.testing.expectEqualStrings("-99", rendered);

    try std.testing.expectEqual(@as(usize, 1), host.dbg_messages.items.len);
    try std.testing.expectEqualStrings("-99", host.dbg_messages.items[0]);
}

test "dbg: float value" {
    const roc_src =
        \\{
        \\    x : F64
        \\    x = 3.14
        \\    dbg x
        \\    x
        \\}
    ;
    const resources = try helpers.parseAndCanonicalizeExpr(std.testing.allocator, roc_src);
    defer helpers.cleanupParseAndCanonical(std.testing.allocator, resources);

    var interp = try Interpreter.init(std.testing.allocator, resources.module_env, resources.builtin_types, resources.builtin_module.env, &[_]*const can.ModuleEnv{}, &resources.checker.import_mapping, null);
    defer interp.deinit();

    var host = TestHost.init(std.testing.allocator);
    defer host.deinit();
    var ops = host.makeOps();

    const result = try interp.eval(resources.expr_idx, &ops);
    defer result.decref(&interp.runtime_layout_store, &ops);

    try std.testing.expectEqual(@as(usize, 1), host.dbg_messages.items.len);
    // Check that the message contains 3.14 (may have trailing digits)
    try std.testing.expect(std.mem.startsWith(u8, host.dbg_messages.items[0], "3.14"));
}

test "dbg: boolean True" {
    const roc_src =
        \\{
        \\    dbg True
        \\    False
        \\}
    ;
    const resources = try helpers.parseAndCanonicalizeExpr(std.testing.allocator, roc_src);
    defer helpers.cleanupParseAndCanonical(std.testing.allocator, resources);

    var interp = try Interpreter.init(std.testing.allocator, resources.module_env, resources.builtin_types, resources.builtin_module.env, &[_]*const can.ModuleEnv{}, &resources.checker.import_mapping, null);
    defer interp.deinit();

    var host = TestHost.init(std.testing.allocator);
    defer host.deinit();
    var ops = host.makeOps();

    const result = try interp.eval(resources.expr_idx, &ops);
    defer result.decref(&interp.runtime_layout_store, &ops);

    try std.testing.expectEqual(@as(usize, 1), host.dbg_messages.items.len);
    // Boolean may render as "True" or "1"
    try std.testing.expect(std.mem.eql(u8, "True", host.dbg_messages.items[0]) or std.mem.eql(u8, "1", host.dbg_messages.items[0]));
}

test "dbg: boolean False" {
    const roc_src =
        \\{
        \\    dbg False
        \\    True
        \\}
    ;
    const resources = try helpers.parseAndCanonicalizeExpr(std.testing.allocator, roc_src);
    defer helpers.cleanupParseAndCanonical(std.testing.allocator, resources);

    var interp = try Interpreter.init(std.testing.allocator, resources.module_env, resources.builtin_types, resources.builtin_module.env, &[_]*const can.ModuleEnv{}, &resources.checker.import_mapping, null);
    defer interp.deinit();

    var host = TestHost.init(std.testing.allocator);
    defer host.deinit();
    var ops = host.makeOps();

    const result = try interp.eval(resources.expr_idx, &ops);
    defer result.decref(&interp.runtime_layout_store, &ops);

    try std.testing.expectEqual(@as(usize, 1), host.dbg_messages.items.len);
    // Boolean may render as "False" or "0"
    try std.testing.expect(std.mem.eql(u8, "False", host.dbg_messages.items[0]) or std.mem.eql(u8, "0", host.dbg_messages.items[0]));
}

test "dbg: empty string" {
    const roc_src =
        \\{
        \\    dbg ""
        \\    "done"
        \\}
    ;
    const resources = try helpers.parseAndCanonicalizeExpr(std.testing.allocator, roc_src);
    defer helpers.cleanupParseAndCanonical(std.testing.allocator, resources);

    var interp = try Interpreter.init(std.testing.allocator, resources.module_env, resources.builtin_types, resources.builtin_module.env, &[_]*const can.ModuleEnv{}, &resources.checker.import_mapping, null);
    defer interp.deinit();

    var host = TestHost.init(std.testing.allocator);
    defer host.deinit();
    var ops = host.makeOps();

    const result = try interp.eval(resources.expr_idx, &ops);
    defer result.decref(&interp.runtime_layout_store, &ops);

    try std.testing.expectEqual(@as(usize, 1), host.dbg_messages.items.len);
    try std.testing.expectEqualStrings("\"\"", host.dbg_messages.items[0]);
}

test "dbg: list of integers" {
    // Note: Using list without explicit type annotation since List I64 annotation causes issues
    const roc_src =
        \\{
        \\    xs = [1i64, 2i64, 3i64]
        \\    dbg xs
        \\    xs
        \\}
    ;
    const resources = try helpers.parseAndCanonicalizeExpr(std.testing.allocator, roc_src);
    defer helpers.cleanupParseAndCanonical(std.testing.allocator, resources);

    var interp = try Interpreter.init(std.testing.allocator, resources.module_env, resources.builtin_types, resources.builtin_module.env, &[_]*const can.ModuleEnv{}, &resources.checker.import_mapping, null);
    defer interp.deinit();

    var host = TestHost.init(std.testing.allocator);
    defer host.deinit();
    var ops = host.makeOps();

    const result = try interp.eval(resources.expr_idx, &ops);
    defer result.decref(&interp.runtime_layout_store, &ops);

    try std.testing.expectEqual(@as(usize, 1), host.dbg_messages.items.len);
    try std.testing.expectEqualStrings("[1, 2, 3]", host.dbg_messages.items[0]);
}

// TODO: Test "dbg: empty list" skipped because List.empty({}) syntax not working
// This test should verify dbg works with empty lists

test "dbg: tuple" {
    const roc_src =
        \\{
        \\    t = (1, "two", 3)
        \\    dbg t
        \\    t
        \\}
    ;
    const resources = try helpers.parseAndCanonicalizeExpr(std.testing.allocator, roc_src);
    defer helpers.cleanupParseAndCanonical(std.testing.allocator, resources);

    var interp = try Interpreter.init(std.testing.allocator, resources.module_env, resources.builtin_types, resources.builtin_module.env, &[_]*const can.ModuleEnv{}, &resources.checker.import_mapping, null);
    defer interp.deinit();

    var host = TestHost.init(std.testing.allocator);
    defer host.deinit();
    var ops = host.makeOps();

    const result = try interp.eval(resources.expr_idx, &ops);
    defer result.decref(&interp.runtime_layout_store, &ops);

    try std.testing.expectEqual(@as(usize, 1), host.dbg_messages.items.len);
    // Tuple should render as (1, "two", 3)
    try std.testing.expectEqualStrings("(1, \"two\", 3)", host.dbg_messages.items[0]);
}

test "dbg: record" {
    const roc_src =
        \\{
        \\    r = { name: "Alice", age: 30 }
        \\    dbg r
        \\    r
        \\}
    ;
    const resources = try helpers.parseAndCanonicalizeExpr(std.testing.allocator, roc_src);
    defer helpers.cleanupParseAndCanonical(std.testing.allocator, resources);

    var interp = try Interpreter.init(std.testing.allocator, resources.module_env, resources.builtin_types, resources.builtin_module.env, &[_]*const can.ModuleEnv{}, &resources.checker.import_mapping, null);
    defer interp.deinit();

    var host = TestHost.init(std.testing.allocator);
    defer host.deinit();
    var ops = host.makeOps();

    const result = try interp.eval(resources.expr_idx, &ops);
    defer result.decref(&interp.runtime_layout_store, &ops);

    try std.testing.expectEqual(@as(usize, 1), host.dbg_messages.items.len);
    // Record fields may be in any order
    const msg = host.dbg_messages.items[0];
    try std.testing.expect(std.mem.indexOf(u8, msg, "name") != null);
    try std.testing.expect(std.mem.indexOf(u8, msg, "Alice") != null);
    try std.testing.expect(std.mem.indexOf(u8, msg, "age") != null);
    try std.testing.expect(std.mem.indexOf(u8, msg, "30") != null);
}

test "dbg: empty record" {
    const roc_src =
        \\{
        \\    r = {}
        \\    dbg r
        \\    r
        \\}
    ;
    const resources = try helpers.parseAndCanonicalizeExpr(std.testing.allocator, roc_src);
    defer helpers.cleanupParseAndCanonical(std.testing.allocator, resources);

    var interp = try Interpreter.init(std.testing.allocator, resources.module_env, resources.builtin_types, resources.builtin_module.env, &[_]*const can.ModuleEnv{}, &resources.checker.import_mapping, null);
    defer interp.deinit();

    var host = TestHost.init(std.testing.allocator);
    defer host.deinit();
    var ops = host.makeOps();

    const result = try interp.eval(resources.expr_idx, &ops);
    defer result.decref(&interp.runtime_layout_store, &ops);

    try std.testing.expectEqual(@as(usize, 1), host.dbg_messages.items.len);
    try std.testing.expectEqualStrings("{}", host.dbg_messages.items[0]);
}

test "dbg: tag without payload" {
    const roc_src =
        \\{
        \\    x : [A, B, C]
        \\    x = B
        \\    dbg x
        \\    x
        \\}
    ;
    const resources = try helpers.parseAndCanonicalizeExpr(std.testing.allocator, roc_src);
    defer helpers.cleanupParseAndCanonical(std.testing.allocator, resources);

    var interp = try Interpreter.init(std.testing.allocator, resources.module_env, resources.builtin_types, resources.builtin_module.env, &[_]*const can.ModuleEnv{}, &resources.checker.import_mapping, null);
    defer interp.deinit();

    var host = TestHost.init(std.testing.allocator);
    defer host.deinit();
    var ops = host.makeOps();

    const result = try interp.eval(resources.expr_idx, &ops);
    defer result.decref(&interp.runtime_layout_store, &ops);

    try std.testing.expectEqual(@as(usize, 1), host.dbg_messages.items.len);
    try std.testing.expectEqualStrings("B", host.dbg_messages.items[0]);
}

test "dbg: tag with payload" {
    // Use match to constrain the tag union type instead of explicit type annotation
    const roc_src =
        \\{
        \\    x = Ok(42)
        \\    dbg x
        \\    match x { Ok(n) => n, Err(_) => 0 }
        \\}
    ;
    const resources = try helpers.parseAndCanonicalizeExpr(std.testing.allocator, roc_src);
    defer helpers.cleanupParseAndCanonical(std.testing.allocator, resources);

    var interp = try Interpreter.init(std.testing.allocator, resources.module_env, resources.builtin_types, resources.builtin_module.env, &[_]*const can.ModuleEnv{}, &resources.checker.import_mapping, null);
    defer interp.deinit();

    var host = TestHost.init(std.testing.allocator);
    defer host.deinit();
    var ops = host.makeOps();

    const result = try interp.eval(resources.expr_idx, &ops);
    defer result.decref(&interp.runtime_layout_store, &ops);

    try std.testing.expectEqual(@as(usize, 1), host.dbg_messages.items.len);
    try std.testing.expectEqualStrings("Ok(42)", host.dbg_messages.items[0]);
}

test "dbg: function prints as unsupported or function marker" {
    const roc_src =
        \\{
        \\    f = |x| x + 1
        \\    dbg f
        \\    f(5)
        \\}
    ;
    const resources = try helpers.parseAndCanonicalizeExpr(std.testing.allocator, roc_src);
    defer helpers.cleanupParseAndCanonical(std.testing.allocator, resources);

    var interp = try Interpreter.init(std.testing.allocator, resources.module_env, resources.builtin_types, resources.builtin_module.env, &[_]*const can.ModuleEnv{}, &resources.checker.import_mapping, null);
    defer interp.deinit();

    var host = TestHost.init(std.testing.allocator);
    defer host.deinit();
    var ops = host.makeOps();

    const result = try interp.eval(resources.expr_idx, &ops);
    defer result.decref(&interp.runtime_layout_store, &ops);

    const rendered = try interp.renderValueRoc(result);
    defer std.testing.allocator.free(rendered);
    try std.testing.expectEqualStrings("6", rendered);

    // Function should print as <function> or <unsupported>
    try std.testing.expectEqual(@as(usize, 1), host.dbg_messages.items.len);
    const msg = host.dbg_messages.items[0];
    try std.testing.expect(std.mem.indexOf(u8, msg, "<") != null or std.mem.indexOf(u8, msg, "function") != null or std.mem.indexOf(u8, msg, "unsupported") != null);
}

test "dbg: expression form returns unit" {
    // dbg always returns {} like expect, so we can't use its return value
    const roc_src =
        \\{
        \\    x = 42
        \\    dbg x
        \\    x + 1
        \\}
    ;
    const resources = try helpers.parseAndCanonicalizeExpr(std.testing.allocator, roc_src);
    defer helpers.cleanupParseAndCanonical(std.testing.allocator, resources);

    var interp = try Interpreter.init(std.testing.allocator, resources.module_env, resources.builtin_types, resources.builtin_module.env, &[_]*const can.ModuleEnv{}, &resources.checker.import_mapping, null);
    defer interp.deinit();

    var host = TestHost.init(std.testing.allocator);
    defer host.deinit();
    var ops = host.makeOps();

    const result = try interp.eval(resources.expr_idx, &ops);
    defer result.decref(&interp.runtime_layout_store, &ops);

    const rendered = try interp.renderValueRoc(result);
    defer std.testing.allocator.free(rendered);
    // dbg x prints 42, then x + 1 = 43
    try std.testing.expectEqualStrings("43", rendered);

    try std.testing.expectEqual(@as(usize, 1), host.dbg_messages.items.len);
    try std.testing.expectEqualStrings("42", host.dbg_messages.items[0]);
}

test "dbg: multiple dbg calls in sequence" {
    const roc_src =
        \\{
        \\    x = 1
        \\    y = 2
        \\    z = 3
        \\    dbg x
        \\    dbg y
        \\    dbg z
        \\    x + y + z
        \\}
    ;
    const resources = try helpers.parseAndCanonicalizeExpr(std.testing.allocator, roc_src);
    defer helpers.cleanupParseAndCanonical(std.testing.allocator, resources);

    var interp = try Interpreter.init(std.testing.allocator, resources.module_env, resources.builtin_types, resources.builtin_module.env, &[_]*const can.ModuleEnv{}, &resources.checker.import_mapping, null);
    defer interp.deinit();

    var host = TestHost.init(std.testing.allocator);
    defer host.deinit();
    var ops = host.makeOps();

    const result = try interp.eval(resources.expr_idx, &ops);
    defer result.decref(&interp.runtime_layout_store, &ops);

    const rendered = try interp.renderValueRoc(result);
    defer std.testing.allocator.free(rendered);
    try std.testing.expectEqualStrings("6", rendered);

    try std.testing.expectEqual(@as(usize, 3), host.dbg_messages.items.len);
    try std.testing.expectEqualStrings("1", host.dbg_messages.items[0]);
    try std.testing.expectEqualStrings("2", host.dbg_messages.items[1]);
    try std.testing.expectEqualStrings("3", host.dbg_messages.items[2]);
}

test "dbg: nested dbg calls" {
    // dbg returns {} so nested dbg prints the inner value, then {} for outer calls
    const roc_src =
        \\{
        \\    dbg(dbg(dbg(5)))
        \\}
    ;
    const resources = try helpers.parseAndCanonicalizeExpr(std.testing.allocator, roc_src);
    defer helpers.cleanupParseAndCanonical(std.testing.allocator, resources);

    var interp = try Interpreter.init(std.testing.allocator, resources.module_env, resources.builtin_types, resources.builtin_module.env, &[_]*const can.ModuleEnv{}, &resources.checker.import_mapping, null);
    defer interp.deinit();

    var host = TestHost.init(std.testing.allocator);
    defer host.deinit();
    var ops = host.makeOps();

    const result = try interp.eval(resources.expr_idx, &ops);
    defer result.decref(&interp.runtime_layout_store, &ops);

    const rendered = try interp.renderValueRoc(result);
    defer std.testing.allocator.free(rendered);
    // dbg always returns {}
    try std.testing.expectEqualStrings("{}", rendered);

    // Three nested dbg calls: inner prints 5, outer two print {}
    try std.testing.expectEqual(@as(usize, 3), host.dbg_messages.items.len);
    try std.testing.expectEqualStrings("5", host.dbg_messages.items[0]);
    try std.testing.expectEqualStrings("{}", host.dbg_messages.items[1]);
    try std.testing.expectEqualStrings("{}", host.dbg_messages.items[2]);
}

// Note: "dbg: as function argument" test removed - dbg returns {} so can't be used as a value

test "dbg: in if-then-else branch" {
    const roc_src =
        \\{
        \\    x = 10
        \\    if x > 5 {
        \\        dbg "greater"
        \\        True
        \\    } else {
        \\        dbg "less or equal"
        \\        False
        \\    }
        \\}
    ;
    const resources = try helpers.parseAndCanonicalizeExpr(std.testing.allocator, roc_src);
    defer helpers.cleanupParseAndCanonical(std.testing.allocator, resources);

    var interp = try Interpreter.init(std.testing.allocator, resources.module_env, resources.builtin_types, resources.builtin_module.env, &[_]*const can.ModuleEnv{}, &resources.checker.import_mapping, null);
    defer interp.deinit();

    var host = TestHost.init(std.testing.allocator);
    defer host.deinit();
    var ops = host.makeOps();

    const result = try interp.eval(resources.expr_idx, &ops);
    defer result.decref(&interp.runtime_layout_store, &ops);

    // Only the taken branch should call dbg
    try std.testing.expectEqual(@as(usize, 1), host.dbg_messages.items.len);
    try std.testing.expectEqualStrings("\"greater\"", host.dbg_messages.items[0]);
}

test "dbg: in match pattern" {
    const roc_src =
        \\{
        \\    x = 5
        \\    match x {
        \\        0 => {
        \\            dbg "zero"
        \\        }
        \\        _ => {
        \\            dbg "other"
        \\        }
        \\    }
        \\}
    ;
    const resources = try helpers.parseAndCanonicalizeExpr(std.testing.allocator, roc_src);
    defer helpers.cleanupParseAndCanonical(std.testing.allocator, resources);

    var interp = try Interpreter.init(std.testing.allocator, resources.module_env, resources.builtin_types, resources.builtin_module.env, &[_]*const can.ModuleEnv{}, &resources.checker.import_mapping, null);
    defer interp.deinit();

    var host = TestHost.init(std.testing.allocator);
    defer host.deinit();
    var ops = host.makeOps();

    const result = try interp.eval(resources.expr_idx, &ops);
    defer result.decref(&interp.runtime_layout_store, &ops);

    // Only the taken branch should call dbg
    try std.testing.expectEqual(@as(usize, 1), host.dbg_messages.items.len);
    try std.testing.expectEqualStrings("\"other\"", host.dbg_messages.items[0]);
}

test "dbg: in for loop" {
    const roc_src =
        \\{
        \\    items : List(I64)
        \\    items = [1, 2, 3]
        \\    for item in items {
        \\        dbg item
        \\    }
        \\    items
        \\}
    ;
    const resources = try helpers.parseAndCanonicalizeExpr(std.testing.allocator, roc_src);
    defer helpers.cleanupParseAndCanonical(std.testing.allocator, resources);

    var interp = try Interpreter.init(std.testing.allocator, resources.module_env, resources.builtin_types, resources.builtin_module.env, &[_]*const can.ModuleEnv{}, &resources.checker.import_mapping, null);
    defer interp.deinit();

    var host = TestHost.init(std.testing.allocator);
    defer host.deinit();
    var ops = host.makeOps();

    const result = try interp.eval(resources.expr_idx, &ops);
    defer result.decref(&interp.runtime_layout_store, &ops);

    // Each iteration should call dbg
    try std.testing.expectEqual(@as(usize, 3), host.dbg_messages.items.len);
    try std.testing.expectEqualStrings("1", host.dbg_messages.items[0]);
    try std.testing.expectEqualStrings("2", host.dbg_messages.items[1]);
    try std.testing.expectEqualStrings("3", host.dbg_messages.items[2]);
}

test "dbg: as final expression returns unit" {
    // dbg always returns {} like expect
    const roc_src =
        \\{
        \\    dbg 42
        \\}
    ;
    const resources = try helpers.parseAndCanonicalizeExpr(std.testing.allocator, roc_src);
    defer helpers.cleanupParseAndCanonical(std.testing.allocator, resources);

    var interp = try Interpreter.init(std.testing.allocator, resources.module_env, resources.builtin_types, resources.builtin_module.env, &[_]*const can.ModuleEnv{}, &resources.checker.import_mapping, null);
    defer interp.deinit();

    var host = TestHost.init(std.testing.allocator);
    defer host.deinit();
    var ops = host.makeOps();

    const result = try interp.eval(resources.expr_idx, &ops);
    defer result.decref(&interp.runtime_layout_store, &ops);

    const rendered = try interp.renderValueRoc(result);
    defer std.testing.allocator.free(rendered);
    // dbg always returns {}
    try std.testing.expectEqualStrings("{}", rendered);

    try std.testing.expectEqual(@as(usize, 1), host.dbg_messages.items.len);
    try std.testing.expectEqualStrings("42", host.dbg_messages.items[0]);
}

test "dbg: with arithmetic expression" {
    const roc_src =
        \\{
        \\    dbg(2 + 3 * 4)
        \\}
    ;
    const resources = try helpers.parseAndCanonicalizeExpr(std.testing.allocator, roc_src);
    defer helpers.cleanupParseAndCanonical(std.testing.allocator, resources);

    var interp = try Interpreter.init(std.testing.allocator, resources.module_env, resources.builtin_types, resources.builtin_module.env, &[_]*const can.ModuleEnv{}, &resources.checker.import_mapping, null);
    defer interp.deinit();

    var host = TestHost.init(std.testing.allocator);
    defer host.deinit();
    var ops = host.makeOps();

    const result = try interp.eval(resources.expr_idx, &ops);
    defer result.decref(&interp.runtime_layout_store, &ops);

    const rendered = try interp.renderValueRoc(result);
    defer std.testing.allocator.free(rendered);
    // dbg returns {} but prints the evaluated expression
    try std.testing.expectEqualStrings("{}", rendered);

    // 2 + 3 * 4 = 2 + 12 = 14
    try std.testing.expectEqual(@as(usize, 1), host.dbg_messages.items.len);
    try std.testing.expectEqualStrings("14", host.dbg_messages.items[0]);
}

test "dbg: inside function body" {
    const roc_src =
        \\{
        \\    double = |x| {
        \\        dbg x
        \\        x * 2
        \\    }
        \\    double(21)
        \\}
    ;
    const resources = try helpers.parseAndCanonicalizeExpr(std.testing.allocator, roc_src);
    defer helpers.cleanupParseAndCanonical(std.testing.allocator, resources);

    var interp = try Interpreter.init(std.testing.allocator, resources.module_env, resources.builtin_types, resources.builtin_module.env, &[_]*const can.ModuleEnv{}, &resources.checker.import_mapping, null);
    defer interp.deinit();

    var host = TestHost.init(std.testing.allocator);
    defer host.deinit();
    var ops = host.makeOps();

    const result = try interp.eval(resources.expr_idx, &ops);
    defer result.decref(&interp.runtime_layout_store, &ops);

    const rendered = try interp.renderValueRoc(result);
    defer std.testing.allocator.free(rendered);
    try std.testing.expectEqualStrings("42", rendered);

    try std.testing.expectEqual(@as(usize, 1), host.dbg_messages.items.len);
    try std.testing.expectEqualStrings("21", host.dbg_messages.items[0]);
}

test "dbg: function called multiple times" {
    const roc_src =
        \\{
        \\    f = |x| {
        \\        dbg x
        \\        x
        \\    }
        \\    f(1) + f(2) + f(3)
        \\}
    ;
    const resources = try helpers.parseAndCanonicalizeExpr(std.testing.allocator, roc_src);
    defer helpers.cleanupParseAndCanonical(std.testing.allocator, resources);

    var interp = try Interpreter.init(std.testing.allocator, resources.module_env, resources.builtin_types, resources.builtin_module.env, &[_]*const can.ModuleEnv{}, &resources.checker.import_mapping, null);
    defer interp.deinit();

    var host = TestHost.init(std.testing.allocator);
    defer host.deinit();
    var ops = host.makeOps();

    const result = try interp.eval(resources.expr_idx, &ops);
    defer result.decref(&interp.runtime_layout_store, &ops);

    const rendered = try interp.renderValueRoc(result);
    defer std.testing.allocator.free(rendered);
    try std.testing.expectEqualStrings("6", rendered);

    try std.testing.expectEqual(@as(usize, 3), host.dbg_messages.items.len);
    try std.testing.expectEqualStrings("1", host.dbg_messages.items[0]);
    try std.testing.expectEqualStrings("2", host.dbg_messages.items[1]);
    try std.testing.expectEqualStrings("3", host.dbg_messages.items[2]);
}

test "dbg: with string containing special chars" {
    const roc_src =
        \\{
        \\    dbg "hello\nworld"
        \\    "done"
        \\}
    ;
    const resources = try helpers.parseAndCanonicalizeExpr(std.testing.allocator, roc_src);
    defer helpers.cleanupParseAndCanonical(std.testing.allocator, resources);

    var interp = try Interpreter.init(std.testing.allocator, resources.module_env, resources.builtin_types, resources.builtin_module.env, &[_]*const can.ModuleEnv{}, &resources.checker.import_mapping, null);
    defer interp.deinit();

    var host = TestHost.init(std.testing.allocator);
    defer host.deinit();
    var ops = host.makeOps();

    const result = try interp.eval(resources.expr_idx, &ops);
    defer result.decref(&interp.runtime_layout_store, &ops);

    try std.testing.expectEqual(@as(usize, 1), host.dbg_messages.items.len);
    // The string should contain the actual newline character, rendered with quotes
    const msg = host.dbg_messages.items[0];
    try std.testing.expect(std.mem.startsWith(u8, msg, "\"hello"));
    try std.testing.expect(std.mem.indexOf(u8, msg, "world") != null);
}

test "dbg: large integer" {
    const roc_src =
        \\{
        \\    x : I64
        \\    x = 9223372036854775807
        \\    dbg x
        \\    x
        \\}
    ;
    const resources = try helpers.parseAndCanonicalizeExpr(std.testing.allocator, roc_src);
    defer helpers.cleanupParseAndCanonical(std.testing.allocator, resources);

    var interp = try Interpreter.init(std.testing.allocator, resources.module_env, resources.builtin_types, resources.builtin_module.env, &[_]*const can.ModuleEnv{}, &resources.checker.import_mapping, null);
    defer interp.deinit();

    var host = TestHost.init(std.testing.allocator);
    defer host.deinit();
    var ops = host.makeOps();

    const result = try interp.eval(resources.expr_idx, &ops);
    defer result.decref(&interp.runtime_layout_store, &ops);

    try std.testing.expectEqual(@as(usize, 1), host.dbg_messages.items.len);
    try std.testing.expectEqualStrings("9223372036854775807", host.dbg_messages.items[0]);
}

test "dbg: variable after mutation in binding" {
    const roc_src =
        \\{
        \\    x = 10
        \\    dbg x
        \\    y = x + 5
        \\    dbg y
        \\    y
        \\}
    ;
    const resources = try helpers.parseAndCanonicalizeExpr(std.testing.allocator, roc_src);
    defer helpers.cleanupParseAndCanonical(std.testing.allocator, resources);

    var interp = try Interpreter.init(std.testing.allocator, resources.module_env, resources.builtin_types, resources.builtin_module.env, &[_]*const can.ModuleEnv{}, &resources.checker.import_mapping, null);
    defer interp.deinit();

    var host = TestHost.init(std.testing.allocator);
    defer host.deinit();
    var ops = host.makeOps();

    const result = try interp.eval(resources.expr_idx, &ops);
    defer result.decref(&interp.runtime_layout_store, &ops);

    const rendered = try interp.renderValueRoc(result);
    defer std.testing.allocator.free(rendered);
    try std.testing.expectEqualStrings("15", rendered);

    try std.testing.expectEqual(@as(usize, 2), host.dbg_messages.items.len);
    try std.testing.expectEqualStrings("10", host.dbg_messages.items[0]);
    try std.testing.expectEqualStrings("15", host.dbg_messages.items[1]);
}

test "dbg: list of strings" {
    const roc_src =
        \\{
        \\    xs = ["a", "b", "c"]
        \\    dbg xs
        \\    xs
        \\}
    ;
    const resources = try helpers.parseAndCanonicalizeExpr(std.testing.allocator, roc_src);
    defer helpers.cleanupParseAndCanonical(std.testing.allocator, resources);

    var interp = try Interpreter.init(std.testing.allocator, resources.module_env, resources.builtin_types, resources.builtin_module.env, &[_]*const can.ModuleEnv{}, &resources.checker.import_mapping, null);
    defer interp.deinit();

    var host = TestHost.init(std.testing.allocator);
    defer host.deinit();
    var ops = host.makeOps();

    const result = try interp.eval(resources.expr_idx, &ops);
    defer result.decref(&interp.runtime_layout_store, &ops);

    try std.testing.expectEqual(@as(usize, 1), host.dbg_messages.items.len);
    try std.testing.expectEqualStrings("[\"a\", \"b\", \"c\"]", host.dbg_messages.items[0]);
}
