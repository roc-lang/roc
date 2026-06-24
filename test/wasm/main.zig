//! Bytebox-based test runner for WASM static library builds.
//!
//! This test compiles a Roc WASM app and verifies it produces the expected output.
//! It follows the pattern from test/playground-integration/main.zig.
//!
//! Run with: zig build run-test-wasm-static-lib

const std = @import("std");
const Allocator = std.mem.Allocator;
const bytebox = @import("bytebox");

/// Custom error type that host functions can return to signal a trap.
/// This must be defined in the root module for bytebox to pick it up.
pub const HostFunctionError = error{RocPanic};

/// Holds the WASM module interface for testing.
const WasmInterface = struct {
    module_def: *bytebox.ModuleDefinition,
    module_instance: *bytebox.ModuleInstance,
    wasm_main_handle: bytebox.FunctionHandle,
    wasm_result_len_handle: bytebox.FunctionHandle,
    wasm_reset_alloc_counts_handle: ?bytebox.FunctionHandle,
    wasm_alloc_count_handle: ?bytebox.FunctionHandle,
    wasm_dealloc_count_handle: ?bytebox.FunctionHandle,
    memory: *bytebox.MemoryInstance,
    env_imports: bytebox.ModuleImportPackage,

    pub fn deinit(self: *WasmInterface) void {
        self.module_instance.destroy();
        self.module_def.destroy();
        self.env_imports.deinit();
    }
};

/// Test result for reporting.
const TestResult = struct {
    name: []const u8,
    passed: bool,
    message: []const u8,
};

const TestOptions = struct {
    assert_alloc_balanced: bool = false,
    min_allocs: usize = 0,
    max_allocs: ?usize = null,
};

/// Host import implementations for the WASM module.
/// These are called by the Roc runtime when it needs to panic, debug print, or report expect failures.
const HostContext = struct {
    memory: ?*bytebox.MemoryInstance = null,

    /// Read a string from WASM memory
    fn readString(self: *HostContext, ptr: i32, len: i32) []const u8 {
        if (self.memory) |mem| {
            const buffer = mem.buffer();
            const start: usize = @intCast(ptr);
            const end: usize = @intCast(ptr + len);
            if (end <= buffer.len) {
                return buffer[start..end];
            }
        }
        return "(invalid memory access)";
    }

    /// Called when Roc panics - print the message and trap
    /// IMPORTANT: This must trap (return error or unreachable) because the WASM code
    /// expects roc_panic to be noreturn. If we just return, the WASM code will hit
    /// an unreachable instruction that the Zig compiler inserted after the call.
    pub fn roc_panic(ctx: ?*anyopaque, module: *bytebox.ModuleInstance, params: [*]const bytebox.Val, _: [*]bytebox.Val) HostFunctionError!void {
        _ = module;
        const self: *HostContext = @ptrCast(@alignCast(ctx));
        const ptr = params[0].I32;
        const len = params[1].I32;
        const msg = self.readString(ptr, len);
        std.debug.print("[PANIC] {s}\n", .{msg});
        return error.RocPanic;
    }

    /// Called for debug output
    pub fn roc_dbg(ctx: ?*anyopaque, module: *bytebox.ModuleInstance, params: [*]const bytebox.Val, _: [*]bytebox.Val) error{}!void {
        _ = module;
        const self: *HostContext = @ptrCast(@alignCast(ctx));
        const ptr = params[0].I32;
        const len = params[1].I32;
        const msg = self.readString(ptr, len);
        std.debug.print("[DBG] {s}\n", .{msg});
    }

    /// Called when an expect fails
    pub fn roc_expect_failed(ctx: ?*anyopaque, module: *bytebox.ModuleInstance, params: [*]const bytebox.Val, _: [*]bytebox.Val) error{}!void {
        _ = module;
        const self: *HostContext = @ptrCast(@alignCast(ctx));
        const ptr = params[0].I32;
        const len = params[1].I32;
        const msg = self.readString(ptr, len);
        std.debug.print("[EXPECT FAILED] {s}\n", .{msg});
    }

    /// Called by Stdout.line! hosted effect
    pub fn echo(ctx: ?*anyopaque, module: *bytebox.ModuleInstance, params: [*]const bytebox.Val, _: [*]bytebox.Val) error{}!void {
        _ = module;
        const self: *HostContext = @ptrCast(@alignCast(ctx));
        const ptr = params[0].I32;
        const len = params[1].I32;
        const msg = self.readString(ptr, len);
        std.debug.print("{s}\n", .{msg});
    }
};

// Global context for host imports (needed because bytebox stores pointer, not value)
var global_host_context: HostContext = .{};

/// Initialize WASM module from file.
fn setupWasm(gpa: std.mem.Allocator, arena: std.mem.Allocator, io: std.Io, wasm_path: []const u8, options: TestOptions) anyerror!WasmInterface {
    const wasm_data = std.Io.Dir.cwd().readFileAlloc(io, wasm_path, arena, .unlimited) catch |err| {
        std.debug.print("[ERROR] Failed to read WASM file '{s}': {}\n", .{ wasm_path, err });
        return err;
    };

    var module_def = try bytebox.createModuleDefinition(arena, .{ .debug_name = "roc_wasm_test" });
    errdefer module_def.destroy();
    try module_def.decode(wasm_data);

    var module_instance = try bytebox.createModuleInstance(.Stack, module_def, gpa);
    errdefer module_instance.destroy();

    // Create import package for "env" namespace with Roc runtime functions
    var env_imports = try bytebox.ModuleImportPackage.init("env", null, &global_host_context, gpa);
    errdefer env_imports.deinit();

    // Register host functions
    try env_imports.addHostFunction("roc_panic", &[_]bytebox.ValType{ .I32, .I32 }, &[_]bytebox.ValType{}, HostContext.roc_panic, &global_host_context);
    try env_imports.addHostFunction("roc_dbg", &[_]bytebox.ValType{ .I32, .I32 }, &[_]bytebox.ValType{}, HostContext.roc_dbg, &global_host_context);
    try env_imports.addHostFunction("roc_expect_failed", &[_]bytebox.ValType{ .I32, .I32 }, &[_]bytebox.ValType{}, HostContext.roc_expect_failed, &global_host_context);
    try env_imports.addHostFunction("echo", &[_]bytebox.ValType{ .I32, .I32 }, &[_]bytebox.ValType{}, HostContext.echo, &global_host_context);

    // Use a reasonable stack for the interpreter (256KB - same as playground tests)
    const imports = [_]bytebox.ModuleImportPackage{env_imports};
    try module_instance.instantiate(.{
        .stack_size = 1024 * 256,
        .imports = &imports,
    });

    // Now that memory is available, update host context
    global_host_context.memory = module_instance.store.getMemory(0);

    const count_allocs = options.assert_alloc_balanced or options.max_allocs != null;
    const reset_alloc_counts_handle: ?bytebox.FunctionHandle = if (count_allocs)
        try module_instance.getFunctionHandle("wasm_reset_alloc_counts")
    else
        null;
    const alloc_count_handle: ?bytebox.FunctionHandle = if (count_allocs)
        try module_instance.getFunctionHandle("wasm_alloc_count")
    else
        null;
    const dealloc_count_handle: ?bytebox.FunctionHandle = if (count_allocs)
        try module_instance.getFunctionHandle("wasm_dealloc_count")
    else
        null;

    return WasmInterface{
        .module_def = module_def,
        .module_instance = module_instance,
        .wasm_main_handle = try module_instance.getFunctionHandle("wasm_main"),
        .wasm_result_len_handle = try module_instance.getFunctionHandle("wasm_result_len"),
        .wasm_reset_alloc_counts_handle = reset_alloc_counts_handle,
        .wasm_alloc_count_handle = alloc_count_handle,
        .wasm_dealloc_count_handle = dealloc_count_handle,
        .memory = module_instance.store.getMemory(0),
        .env_imports = env_imports,
    };
}

fn callWasmNoArgVoid(wasm: *const WasmInterface, handle: bytebox.FunctionHandle, name: []const u8) anyerror!void {
    var params: [0]bytebox.Val = undefined;
    var returns: [0]bytebox.Val = undefined;
    _ = wasm.module_instance.invoke(handle, &params, &returns, .{}) catch |err| {
        std.debug.print("[ERROR] Error invoking {s}: {}\n", .{ name, err });
        return error.WasmExportFailed;
    };
}

fn callWasmNoArgUsize(wasm: *const WasmInterface, handle: bytebox.FunctionHandle, name: []const u8) anyerror!usize {
    var params: [0]bytebox.Val = undefined;
    var returns: [1]bytebox.Val = undefined;
    _ = wasm.module_instance.invoke(handle, &params, &returns, .{}) catch |err| {
        std.debug.print("[ERROR] Error invoking {s}: {}\n", .{ name, err });
        return error.WasmExportFailed;
    };
    return @intCast(returns[0].I32);
}

/// Call wasm_main() and get the result string.
fn callWasmMain(wasm: *const WasmInterface, allocator: std.mem.Allocator) anyerror![]const u8 {
    // Call wasm_main() which returns a pointer to the result string
    var params_main: [0]bytebox.Val = undefined;
    var returns_main: [1]bytebox.Val = undefined;
    _ = wasm.module_instance.invoke(wasm.wasm_main_handle, &params_main, &returns_main, .{}) catch |err| {
        std.debug.print("[ERROR] Error invoking wasm_main: {}\n", .{err});
        var backtrace = wasm.module_instance.formatBacktrace(2, allocator) catch null;
        if (backtrace) |*bt| {
            defer bt.deinit();
            std.debug.print("[ERROR] WASM backtrace:\n{s}\n", .{bt.items});
        }
        return error.WasmMainFailed;
    };

    const result_ptr: usize = @intCast(returns_main[0].I32);

    // Call wasm_result_len() to get the string length
    var params_len: [0]bytebox.Val = undefined;
    var returns_len: [1]bytebox.Val = undefined;
    _ = wasm.module_instance.invoke(wasm.wasm_result_len_handle, &params_len, &returns_len, .{}) catch |err| {
        std.debug.print("[ERROR] Error invoking wasm_result_len: {}\n", .{err});
        return error.WasmResultLenFailed;
    };

    const result_len: usize = @intCast(returns_len[0].I32);

    // Read the result string from WASM memory
    const wasm_memory = wasm.memory.buffer();
    if (result_ptr + result_len > wasm_memory.len) {
        std.debug.print("[ERROR] Result string out of bounds: ptr={}, len={}, mem_size={}\n", .{ result_ptr, result_len, wasm_memory.len });
        return error.ResultOutOfBounds;
    }

    return wasm_memory[result_ptr .. result_ptr + result_len];
}

/// Run a single test case.
fn runTest(gpa: std.mem.Allocator, arena: std.mem.Allocator, io: std.Io, wasm_path: []const u8, expected_output: []const u8, options: TestOptions) TestResult {
    var wasm = setupWasm(gpa, arena, io, wasm_path, options) catch |err| {
        return .{
            .name = wasm_path,
            .passed = false,
            .message = std.fmt.allocPrint(arena, "Setup failed: {}", .{err}) catch "Setup failed",
        };
    };
    defer wasm.deinit();

    const count_allocs = options.assert_alloc_balanced or options.max_allocs != null;
    if (count_allocs) {
        callWasmNoArgVoid(&wasm, wasm.wasm_reset_alloc_counts_handle.?, "wasm_reset_alloc_counts") catch |err| {
            return .{
                .name = wasm_path,
                .passed = false,
                .message = std.fmt.allocPrint(arena, "Counter reset failed: {}", .{err}) catch "Counter reset failed",
            };
        };
    }

    const result = callWasmMain(&wasm, arena) catch |err| {
        return .{
            .name = wasm_path,
            .passed = false,
            .message = std.fmt.allocPrint(arena, "Execution failed: {}", .{err}) catch "Execution failed",
        };
    };

    if (!std.mem.eql(u8, result, expected_output)) {
        return .{
            .name = wasm_path,
            .passed = false,
            .message = std.fmt.allocPrint(arena, "Expected '{s}', got '{s}'", .{ expected_output, result }) catch "Output mismatch",
        };
    }

    if (count_allocs) {
        const alloc_count = callWasmNoArgUsize(&wasm, wasm.wasm_alloc_count_handle.?, "wasm_alloc_count") catch |err| {
            return .{
                .name = wasm_path,
                .passed = false,
                .message = std.fmt.allocPrint(arena, "Allocation counter read failed: {}", .{err}) catch "Allocation counter read failed",
            };
        };
        const dealloc_count = callWasmNoArgUsize(&wasm, wasm.wasm_dealloc_count_handle.?, "wasm_dealloc_count") catch |err| {
            return .{
                .name = wasm_path,
                .passed = false,
                .message = std.fmt.allocPrint(arena, "Deallocation counter read failed: {}", .{err}) catch "Deallocation counter read failed",
            };
        };

        if (alloc_count < options.min_allocs) {
            return .{
                .name = wasm_path,
                .passed = false,
                .message = std.fmt.allocPrint(arena, "Expected at least {d} allocations, got {d}", .{ options.min_allocs, alloc_count }) catch "Too few allocations",
            };
        }
        if (options.max_allocs) |max_allocs| {
            if (alloc_count > max_allocs) {
                return .{
                    .name = wasm_path,
                    .passed = false,
                    .message = std.fmt.allocPrint(arena, "Expected at most {d} allocations, got {d}", .{ max_allocs, alloc_count }) catch "Too many allocations",
                };
            }
        }
        if (options.assert_alloc_balanced and alloc_count != dealloc_count) {
            return .{
                .name = wasm_path,
                .passed = false,
                .message = std.fmt.allocPrint(arena, "Allocation imbalance: alloc={d}, dealloc={d}", .{ alloc_count, dealloc_count }) catch "Allocation imbalance",
            };
        }
    }

    return .{
        .name = wasm_path,
        .passed = true,
        .message = "OK",
    };
}

pub fn main(init: std.process.Init) anyerror!void {
    var gpa_impl = std.heap.DebugAllocator(.{}){};
    defer _ = gpa_impl.deinit();
    const gpa = gpa_impl.allocator();

    var arena_impl = std.heap.ArenaAllocator.init(gpa);
    defer arena_impl.deinit();
    const arena = arena_impl.allocator();

    var wasm_path: []const u8 = "test/wasm/app.wasm";
    var expected_output: []const u8 = "Hello from Roc WASM!";
    var options: TestOptions = .{};

    var arg_iter = try std.process.Args.Iterator.initAllocator(init.minimal.args, arena);
    defer arg_iter.deinit();
    _ = arg_iter.skip();
    while (arg_iter.next()) |arg| {
        if (std.mem.eql(u8, arg, "--help")) {
            std.debug.print("Usage: zig build run-test-wasm-static-lib -- [options]\n", .{});
            std.debug.print("Options:\n", .{});
            std.debug.print("  --wasm-path PATH     Path to the WASM file (default: test/wasm/app.wasm)\n", .{});
            std.debug.print("  --expected OUTPUT    Expected output string\n", .{});
            std.debug.print("  --assert-alloc-balanced  Assert canonical roc_alloc and roc_dealloc counts match\n", .{});
            std.debug.print("  --min-allocs N       Minimum canonical roc_alloc count when counting allocations\n", .{});
            std.debug.print("  --max-allocs N       Maximum canonical roc_alloc count when counting allocations\n", .{});
            std.debug.print("  --help               Display this help message\n", .{});
            return;
        } else if (std.mem.eql(u8, arg, "--wasm-path")) {
            wasm_path = arg_iter.next() orelse {
                std.debug.print("Error: --wasm-path requires an argument\n", .{});
                return;
            };
        } else if (std.mem.eql(u8, arg, "--expected")) {
            expected_output = arg_iter.next() orelse {
                std.debug.print("Error: --expected requires an argument\n", .{});
                return;
            };
        } else if (std.mem.eql(u8, arg, "--assert-alloc-balanced")) {
            options.assert_alloc_balanced = true;
        } else if (std.mem.eql(u8, arg, "--min-allocs")) {
            const min_allocs_arg = arg_iter.next() orelse {
                std.debug.print("Error: --min-allocs requires an argument\n", .{});
                return;
            };
            options.min_allocs = std.fmt.parseInt(usize, min_allocs_arg, 10) catch |err| {
                std.debug.print("Error: invalid --min-allocs value '{s}': {}\n", .{ min_allocs_arg, err });
                return;
            };
        } else if (std.mem.eql(u8, arg, "--max-allocs")) {
            const max_allocs_arg = arg_iter.next() orelse {
                std.debug.print("Error: --max-allocs requires an argument\n", .{});
                return;
            };
            options.max_allocs = std.fmt.parseInt(usize, max_allocs_arg, 10) catch |err| {
                std.debug.print("Error: invalid --max-allocs value '{s}': {}\n", .{ max_allocs_arg, err });
                return;
            };
        }
    }

    std.debug.print("=== WASM Static Library Test ===\n", .{});
    std.debug.print("WASM file: {s}\n", .{wasm_path});
    std.debug.print("Expected output: \"{s}\"\n\n", .{expected_output});

    const result = runTest(gpa, arena, init.io, wasm_path, expected_output, options);

    if (result.passed) {
        std.debug.print("PASSED: {s}\n", .{result.name});
    } else {
        std.debug.print("FAILED: {s}\n", .{result.name});
        std.debug.print("  {s}\n", .{result.message});
    }

    if (!result.passed) {
        return error.TestFailed;
    }
}

test "wasm static lib - hello world" {
    // This test is run via `zig build run-test-wasm-static-lib`
    // It requires the WASM file to be built first
    const gpa = std.testing.allocator;
    var arena_impl = std.heap.ArenaAllocator.init(gpa);
    defer arena_impl.deinit();
    const arena = arena_impl.allocator();

    const result = runTest(gpa, arena, std.testing.io, "test/wasm/app.wasm", "Hello from Roc WASM!", .{});
    if (!result.passed) {
        std.debug.print("Test failed: {s}\n", .{result.message});
    }
    try std.testing.expect(result.passed);
}
