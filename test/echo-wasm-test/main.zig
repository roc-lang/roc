//! Bytebox-driven integration test for echo.wasm.
//!
//! Loads zig-out/lib/echo.wasm via bytebox, supplies js_echo + js_stderr host
//! functions that capture output into in-process buffers, and drives the
//! exported API (init / allocateBuffer / addFile / compileAndRun) the same
//! way `www/app.js` does. Validates that the tutorial example produces
//! "Hello from the Greeting module!" as its single echo line.

const std = @import("std");
const Allocator = std.mem.Allocator;
const bytebox = @import("bytebox");

// Capture buffers shared with bytebox host functions via a CaptureCtx.
const CaptureCtx = struct {
    echoed: std.ArrayList(u8),
    stderr: std.ArrayList(u8),
    gpa: std.mem.Allocator,
};

var capture_ctx: CaptureCtx = undefined;
var memory_instance: *bytebox.MemoryInstance = undefined;

fn hostJsEcho(_: ?*anyopaque, _: *bytebox.ModuleInstance, params: [*]const bytebox.Val, _: [*]bytebox.Val) error{}!void {
    const ptr: u32 = @intCast(params[0].I32);
    const len: u32 = @intCast(params[1].I32);
    const mem = memory_instance.buffer();
    if (@as(usize, ptr) + @as(usize, len) > mem.len) return;
    capture_ctx.echoed.appendSlice(capture_ctx.gpa, mem[ptr .. ptr + len]) catch return;
    capture_ctx.echoed.append(capture_ctx.gpa, '\n') catch return;
}

fn hostJsStderr(_: ?*anyopaque, _: *bytebox.ModuleInstance, params: [*]const bytebox.Val, _: [*]bytebox.Val) error{}!void {
    const ptr: u32 = @intCast(params[0].I32);
    const len: u32 = @intCast(params[1].I32);
    const mem = memory_instance.buffer();
    if (@as(usize, ptr) + @as(usize, len) > mem.len) return;
    capture_ctx.stderr.appendSlice(capture_ctx.gpa, mem[ptr .. ptr + len]) catch return;
}

fn invokeAlloc(instance: *bytebox.ModuleInstance, handle: bytebox.FunctionHandle, size: u32) anyerror!u32 {
    var params = [_]bytebox.Val{.{ .I32 = @intCast(size) }};
    var returns = [_]bytebox.Val{.{ .I32 = 0 }};
    try instance.invoke(handle, &params, &returns, .{});
    const result: u32 = @intCast(returns[0].I32);
    if (result == 0) return error.WasmAllocReturnedNull;
    return result;
}

fn writeBytesToWasm(instance: *bytebox.ModuleInstance, alloc_handle: bytebox.FunctionHandle, data: []const u8) anyerror!struct { ptr: u32, len: u32 } {
    const ptr = try invokeAlloc(instance, alloc_handle, @intCast(data.len));
    const mem = memory_instance.buffer();
    if (@as(usize, ptr) + data.len > mem.len) return error.WasmBufferOutOfBounds;
    @memcpy(mem[ptr .. ptr + data.len], data);
    return .{ .ptr = ptr, .len = @intCast(data.len) };
}

fn invokeInit(instance: *bytebox.ModuleInstance, init_handle: bytebox.FunctionHandle) anyerror!void {
    var params = [_]bytebox.Val{};
    var returns = [_]bytebox.Val{};
    try instance.invoke(init_handle, &params, &returns, .{});
}

fn compileAndRunSource(
    instance: *bytebox.ModuleInstance,
    alloc_handle: bytebox.FunctionHandle,
    run_handle: bytebox.FunctionHandle,
    source: []const u8,
) anyerror!u32 {
    const main_buf = try writeBytesToWasm(instance, alloc_handle, source);
    var run_params = [_]bytebox.Val{
        .{ .I32 = @intCast(main_buf.ptr) },
        .{ .I32 = @intCast(main_buf.len) },
    };
    var run_returns = [_]bytebox.Val{.{ .I32 = 0 }};
    try instance.invoke(run_handle, &run_params, &run_returns, .{});
    return @intCast(run_returns[0].I32);
}

fn resetCapturedOutput() void {
    capture_ctx.echoed.clearRetainingCapacity();
    capture_ctx.stderr.clearRetainingCapacity();
}

fn requireContains(haystack: []const u8, needle: []const u8, label: []const u8) void {
    if (std.mem.find(u8, haystack, needle) != null) return;
    std.debug.print("FAIL: expected {s} to contain {s}\n{s}\n", .{ label, needle, haystack });
    std.process.exit(1);
}

fn requireNotContains(haystack: []const u8, needle: []const u8, label: []const u8) void {
    if (std.mem.find(u8, haystack, needle) == null) return;
    std.debug.print("FAIL: expected {s} not to contain {s}\n{s}\n", .{ label, needle, haystack });
    std.process.exit(1);
}

pub fn main(init: std.process.Init) anyerror!void {
    const io = init.io;

    var gpa_impl: std.heap.DebugAllocator(.{}) = .init;
    defer _ = gpa_impl.deinit();
    const gpa = gpa_impl.allocator();

    var arena_impl = std.heap.ArenaAllocator.init(gpa);
    defer arena_impl.deinit();
    const arena = arena_impl.allocator();

    capture_ctx = .{
        .echoed = std.ArrayList(u8).empty,
        .stderr = std.ArrayList(u8).empty,
        .gpa = gpa,
    };
    defer capture_ctx.echoed.deinit(gpa);
    defer capture_ctx.stderr.deinit(gpa);

    // Locate echo.wasm relative to repo root (cwd when run via `zig build`).
    const wasm_path = "zig-out/lib/echo.wasm";
    const wasm_bytes = std.Io.Dir.cwd().readFileAlloc(io, wasm_path, arena, .unlimited) catch |err| {
        std.debug.print("FAIL: could not read {s}: {s}\n", .{ wasm_path, @errorName(err) });
        std.debug.print("(Did you run `zig build build-playground` first?)\n", .{});
        std.process.exit(2);
    };

    var module_def = try bytebox.createModuleDefinition(arena, .{ .debug_name = "echo_wasm" });
    defer module_def.destroy();
    try module_def.decode(wasm_bytes);

    var module_instance = try bytebox.createModuleInstance(.Stack, module_def, gpa);
    defer module_instance.destroy();

    // Wire env.js_echo + env.js_stderr.
    var env_imports = try bytebox.ModuleImportPackage.init("env", null, null, gpa);
    defer env_imports.deinit();
    try env_imports.addHostFunction(
        "js_echo",
        &[_]bytebox.ValType{ .I32, .I32 },
        &[_]bytebox.ValType{},
        hostJsEcho,
        null,
    );
    try env_imports.addHostFunction(
        "js_stderr",
        &[_]bytebox.ValType{ .I32, .I32 },
        &[_]bytebox.ValType{},
        hostJsStderr,
        null,
    );

    const packages = [_]bytebox.ModuleImportPackage{env_imports};
    try module_instance.instantiate(.{
        .imports = &packages,
        // 256 KiB — same as playground-integration. Larger values overflow
        // bytebox's internal max_labels calc.
        .stack_size = 1024 * 256,
    });

    memory_instance = module_instance.store.getMemory(0);

    const init_handle = try module_instance.getFunctionHandle("init");
    const alloc_handle = try module_instance.getFunctionHandle("allocateBuffer");
    const addfile_handle = try module_instance.getFunctionHandle("addFile");
    const run_handle = try module_instance.getFunctionHandle("compileAndRun");

    try invokeInit(module_instance, init_handle);

    // addFile("Greeting", "<content>")
    const greeting_name = try writeBytesToWasm(module_instance, alloc_handle, "Greeting");
    const greeting_src =
        \\Greeting := [].{
        \\    msg : Str
        \\    msg = "Hello from the Greeting module!"
        \\}
    ;
    const greeting_content = try writeBytesToWasm(module_instance, alloc_handle, greeting_src);
    {
        var params = [_]bytebox.Val{
            .{ .I32 = @intCast(greeting_name.ptr) },
            .{ .I32 = @intCast(greeting_name.len) },
            .{ .I32 = @intCast(greeting_content.ptr) },
            .{ .I32 = @intCast(greeting_content.len) },
        };
        var returns = [_]bytebox.Val{.{ .I32 = 0 }};
        try module_instance.invoke(addfile_handle, &params, &returns, .{});
        const code: u32 = @intCast(returns[0].I32);
        if (code != 0) {
            std.debug.print("FAIL: addFile returned {d}\n", .{code});
            std.process.exit(1);
        }
    }

    // compileAndRun(<main source>)
    const main_src =
        \\import Greeting
        \\
        \\main! = |_| {
        \\    echo!(Greeting.msg)
        \\    Ok({})
        \\}
    ;
    const exit_code = try compileAndRunSource(module_instance, alloc_handle, run_handle, main_src);

    const expected_output = "Hello from the Greeting module!\n";
    const got_output = capture_ctx.echoed.items;
    const got_stderr = capture_ctx.stderr.items;

    if (exit_code != 0) {
        std.debug.print("FAIL: compileAndRun returned exit code {d}\n", .{exit_code});
        if (got_stderr.len > 0) {
            std.debug.print("stderr:\n{s}\n", .{got_stderr});
        }
        std.process.exit(1);
    }

    if (!std.mem.eql(u8, got_output, expected_output)) {
        std.debug.print("FAIL: echo output mismatch.\n", .{});
        std.debug.print("  expected: {s}", .{expected_output});
        std.debug.print("  got:      {s}\n", .{got_output});
        if (got_stderr.len > 0) {
            std.debug.print("stderr:\n{s}\n", .{got_stderr});
        }
        std.process.exit(1);
    }

    resetCapturedOutput();
    try invokeInit(module_instance, init_handle);

    const bad_src =
        \\main! = |_| {
        \\    todos = [
        \\        { name: "Learn Roc", done: True },
        \\        { label: "Call mom", done: False },
        \\    ]
        \\    Ok({})
        \\}
    ;
    const bad_exit_code = try compileAndRunSource(module_instance, alloc_handle, run_handle, bad_src);
    const bad_stderr = capture_ctx.stderr.items;

    if (bad_exit_code != 255) {
        std.debug.print("FAIL: bad source returned exit code {d}\n", .{bad_exit_code});
        std.debug.print("stderr:\n{s}\n", .{bad_stderr});
        std.process.exit(1);
    }

    requireContains(bad_stderr, "TYPE MISMATCH", "diagnostic stderr");
    requireContains(bad_stderr, "\xE2\x94", "diagnostic stderr");
    requireContains(bad_stderr, "main.roc", "diagnostic stderr");
    requireNotContains(bad_stderr, "/app/main.roc", "diagnostic stderr");
    requireNotContains(bad_stderr, "<div", "diagnostic stderr");
    requireNotContains(bad_stderr, "<span", "diagnostic stderr");

    std.debug.print("PASS: echo.wasm tutorial and diagnostic cases produced expected output.\n", .{});
}
