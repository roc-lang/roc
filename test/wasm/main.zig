//! Bytebox-based test runner for WASM static library builds.
//!
//! This test compiles a Roc WASM app and verifies it produces the expected output.
//! It follows the pattern from test/playground-integration/main.zig.
//!
//! Run with: zig build test-wasm-static-lib

const std = @import("std");
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
};

// Global context for host imports (needed because bytebox stores pointer, not value)
var global_host_context: HostContext = .{};

/// Initialize WASM module from file.
fn setupWasm(gpa: std.mem.Allocator, arena: std.mem.Allocator, wasm_path: []const u8) !WasmInterface {
    const wasm_data = std.fs.cwd().readFileAlloc(arena, wasm_path, std.math.maxInt(usize)) catch |err| {
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

    // Use a reasonable stack for the interpreter (256KB - same as playground tests)
    const imports = [_]bytebox.ModuleImportPackage{env_imports};
    try module_instance.instantiate(.{
        .stack_size = 1024 * 256,
        .imports = &imports,
    });

    // Now that memory is available, update host context
    global_host_context.memory = module_instance.store.getMemory(0);

    return WasmInterface{
        .module_def = module_def,
        .module_instance = module_instance,
        .wasm_main_handle = try module_instance.getFunctionHandle("wasm_main"),
        .wasm_result_len_handle = try module_instance.getFunctionHandle("wasm_result_len"),
        .memory = module_instance.store.getMemory(0),
        .env_imports = env_imports,
    };
}

/// Call wasm_main() and get the result string.
fn callWasmMain(wasm: *const WasmInterface) ![]const u8 {
    // Call wasm_main() which returns a pointer to the result string
    var params_main: [0]bytebox.Val = undefined;
    var returns_main: [1]bytebox.Val = undefined;
    _ = wasm.module_instance.invoke(wasm.wasm_main_handle, &params_main, &returns_main, .{}) catch |err| {
        std.debug.print("[ERROR] Error invoking wasm_main: {}\n", .{err});
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
fn runTest(gpa: std.mem.Allocator, arena: std.mem.Allocator, wasm_path: []const u8, expected_output: []const u8) TestResult {
    var wasm = setupWasm(gpa, arena, wasm_path) catch |err| {
        return .{
            .name = wasm_path,
            .passed = false,
            .message = std.fmt.allocPrint(arena, "Setup failed: {}", .{err}) catch "Setup failed",
        };
    };
    defer wasm.deinit();

    const result = callWasmMain(&wasm) catch |err| {
        return .{
            .name = wasm_path,
            .passed = false,
            .message = std.fmt.allocPrint(arena, "Execution failed: {}", .{err}) catch "Execution failed",
        };
    };

    if (std.mem.eql(u8, result, expected_output)) {
        return .{
            .name = wasm_path,
            .passed = true,
            .message = "OK",
        };
    } else {
        return .{
            .name = wasm_path,
            .passed = false,
            .message = std.fmt.allocPrint(arena, "Expected '{s}', got '{s}'", .{ expected_output, result }) catch "Output mismatch",
        };
    }
}

pub fn main() !void {
    var gpa_impl = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa_impl.deinit();
    const gpa = gpa_impl.allocator();

    var arena_impl = std.heap.ArenaAllocator.init(gpa);
    defer arena_impl.deinit();
    const arena = arena_impl.allocator();

    const stdout = std.fs.File.stdout().deprecatedWriter();

    // Handle CLI arguments
    const args = try std.process.argsAlloc(arena);

    var wasm_path: []const u8 = "test/wasm/app.wasm";
    var expected_output: []const u8 = "Hello from Roc WASM!";

    var i: usize = 1;
    while (i < args.len) : (i += 1) {
        const arg = args[i];
        if (std.mem.eql(u8, arg, "--help")) {
            try stdout.print("Usage: test-wasm-static-lib [options]\n", .{});
            try stdout.print("Options:\n", .{});
            try stdout.print("  --wasm-path PATH     Path to the WASM file (default: test/wasm/app.wasm)\n", .{});
            try stdout.print("  --expected OUTPUT    Expected output string\n", .{});
            try stdout.print("  --help               Display this help message\n", .{});
            return;
        } else if (std.mem.eql(u8, arg, "--wasm-path")) {
            i += 1;
            if (i >= args.len) {
                try stdout.print("Error: --wasm-path requires an argument\n", .{});
                return;
            }
            wasm_path = args[i];
        } else if (std.mem.eql(u8, arg, "--expected")) {
            i += 1;
            if (i >= args.len) {
                try stdout.print("Error: --expected requires an argument\n", .{});
                return;
            }
            expected_output = args[i];
        }
    }

    try stdout.print("=== WASM Static Library Test ===\n", .{});
    try stdout.print("WASM file: {s}\n", .{wasm_path});
    try stdout.print("Expected output: \"{s}\"\n\n", .{expected_output});

    const result = runTest(gpa, arena, wasm_path, expected_output);

    if (result.passed) {
        try stdout.print("PASSED: {s}\n", .{result.name});
    } else {
        try stdout.print("FAILED: {s}\n", .{result.name});
        try stdout.print("  {s}\n", .{result.message});
    }

    if (!result.passed) {
        return error.TestFailed;
    }
}

test "wasm static lib - hello world" {
    // This test is run via `zig build test-wasm-static-lib`
    // It requires the WASM file to be built first
    const gpa = std.testing.allocator;
    var arena_impl = std.heap.ArenaAllocator.init(gpa);
    defer arena_impl.deinit();
    const arena = arena_impl.allocator();

    const result = runTest(gpa, arena, "test/wasm/app.wasm", "Hello from Roc WASM!");
    if (!result.passed) {
        std.debug.print("Test failed: {s}\n", .{result.message});
    }
    try std.testing.expect(result.passed);
}
