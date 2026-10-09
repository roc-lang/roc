//! WebAssembly execution runner for eval and REPL.
//!
//! Provides the platform runtime callbacks and memory management for running
//! Roc expressions compiled to WebAssembly via the Bytebox runtime.

const std = @import("std");
const builtin = @import("builtin");
const builtins = @import("builtins");
const bytebox = @import("bytebox");
const collections = @import("collections");
const HostEvent = @import("runtime_host.zig").HostEvent;
const i128h = builtins.compiler_rt_128;
const is_freestanding = builtin.target.os.tag == .freestanding;

/// Infrastructure failures that can prevent a WebAssembly evaluation from
/// producing a semantic outcome.
pub const WasmOutcomeError = error{
    WasmExecFailed,
    OutOfMemory,
};

const debugPrint = if (is_freestanding)
    struct {
        fn print(comptime _: []const u8, _: anytype) void {}
    }.print
else
    struct {
        fn print(comptime fmt: []const u8, args: anytype) void {
            std.debug.print(fmt, args);
        }
    }.print;

fn readIntLittle(comptime T: type, buffer: []const u8, offset: usize) T {
    const UInt = @Int(.unsigned, @bitSizeOf(T));
    var result: UInt = 0;
    var i: usize = 0;
    while (i < @sizeOf(T)) : (i += 1) {
        result |= @as(UInt, buffer[offset + i]) << @intCast(i * 8);
    }
    return @bitCast(result);
}

fn writeIntLittle(comptime T: type, buffer: []u8, offset: usize, value: T) void {
    const UInt = @Int(.unsigned, @bitSizeOf(T));
    var remaining: UInt = @bitCast(value);
    var i: usize = 0;
    while (i < @sizeOf(T)) : (i += 1) {
        buffer[offset + i] = @intCast(remaining & 0xff);
        remaining >>= 8;
    }
}

/// Pointer width in bytes for the wasm32 target this runner drives.
const wasm_word_size = 4;

/// Byte size of a RocStr header in wasm32 linear memory: `word_count`
/// pointer-sized words. The runner reads the root's RocStr result through it.
const wasm_roc_str_size = builtins.str.RocStr.word_count * wasm_word_size;

/// Largest length that fits in a small RocStr on wasm32 (the final byte holds
/// the small-string flag, so the inline bytes span the rest of the header).
const wasm_small_str_max_len = wasm_roc_str_size - 1;

/// Captures a wasm eval run's string output and host-observed allocation count.
pub const RunWasmStrResult = struct {
    output: []u8,
    allocation_count: u32,
};

/// Semantic result of invoking an inspect-wrapped WebAssembly root. The caller
/// owns the byte slice in either variant.
pub const RunWasmOutcome = union(enum) {
    returned: []u8,
    crashed: []u8,
};

/// WebAssembly evaluation outcome plus host-observed allocation count.
pub const RunWasmOutcomeResult = struct {
    outcome: RunWasmOutcome,
    events: []HostEvent,
    allocation_count: u32,

    pub fn deinitEvents(self: RunWasmOutcomeResult, allocator: std.mem.Allocator) void {
        for (self.events) |*event| event.deinit(allocator);
        allocator.free(self.events);
    }
};

const WasmRunState = struct {
    allocator: std.mem.Allocator,
    heap_ptr: u32,
    allocation_count: u32 = 0,
    crashed: bool = false,
    crash_message: ?[]u8 = null,
    crash_message_oom: bool = false,
    events: std.ArrayListUnmanaged(HostEvent) = .empty,

    fn init(allocator: std.mem.Allocator, heap_base: u32) WasmRunState {
        return .{ .allocator = allocator, .heap_ptr = heap_base };
    }

    fn deinit(self: *WasmRunState) void {
        if (self.crash_message) |message| self.allocator.free(message);
        for (self.events.items) |*event| event.deinit(self.allocator);
        self.events.deinit(self.allocator);
    }

    fn recordEvent(self: *WasmRunState, comptime tag: std.meta.Tag(HostEvent), message: []const u8) void {
        const owned = self.allocator.dupe(u8, message) catch @panic("out of memory recording wasm host event");
        self.events.append(self.allocator, @unionInit(HostEvent, @tagName(tag), owned)) catch {
            self.allocator.free(owned);
            @panic("out of memory recording wasm host event");
        };
    }

    fn recordCrash(self: *WasmRunState, message: []const u8) void {
        self.crashed = true;
        self.recordEvent(.crashed, message);
        if (self.crash_message) |old| {
            self.allocator.free(old);
            self.crash_message = null;
        }
        self.crash_message = self.allocator.dupe(u8, message) catch {
            self.crash_message_oom = true;
            return;
        };
        self.crash_message_oom = false;
    }

    fn takeCrashMessage(self: *WasmRunState) WasmOutcomeError![]u8 {
        if (self.crash_message) |message| {
            self.crash_message = null;
            return message;
        }
        if (self.crash_message_oom) return error.OutOfMemory;
        return error.WasmExecFailed;
    }

    fn takeEvents(self: *WasmRunState) std.mem.Allocator.Error![]HostEvent {
        const events = try self.events.toOwnedSlice(self.allocator);
        self.events = .empty;
        return events;
    }
};

fn crashedWasmResult(run_state: *WasmRunState) WasmOutcomeError!RunWasmOutcomeResult {
    const message = try run_state.takeCrashMessage();
    errdefer run_state.allocator.free(message);
    return .{
        .outcome = .{ .crashed = message },
        .events = try run_state.takeEvents(),
        .allocation_count = run_state.allocation_count,
    };
}

/// Executes a wasm module while preserving Roc crashes as semantic outcomes.
pub fn runWasmOutcomeWithStats(
    allocator: std.mem.Allocator,
    wasm_bytes: []const u8,
    heap_base: u32,
    has_imports: bool,
) WasmOutcomeError!RunWasmOutcomeResult {
    var run_state = WasmRunState.init(allocator, heap_base);
    defer run_state.deinit();

    if (wasm_bytes.len == 0) return error.WasmExecFailed;

    var arena_impl = collections.SingleThreadArena.init(allocator);
    defer arena_impl.deinit();
    const arena = arena_impl.allocator();

    var module_def = bytebox.createModuleDefinition(arena, .{}) catch return error.WasmExecFailed;
    module_def.decode(wasm_bytes) catch |err| {
        if (std.debug.runtime_safety) {
            debugPrint("wasm decode failed: {s}\n", .{@errorName(err)});
        }
        return error.WasmExecFailed;
    };

    var module_instance = bytebox.createModuleInstance(.Stack, module_def, arena) catch |err| {
        if (std.debug.runtime_safety) {
            debugPrint("wasm instance create failed: {s}\n", .{@errorName(err)});
        }
        return error.WasmExecFailed;
    };
    defer module_instance.destroy();

    if (has_imports) {
        var env_imports = bytebox.ModuleImportPackage.init("env", null, &run_state, allocator) catch return error.WasmExecFailed;
        defer env_imports.deinit();

        // Compiler-rt intrinsics needed by merged builtins
        env_imports.addHostFunction("__multi3", &[_]bytebox.ValType{ .I32, .I64, .I64, .I64, .I64 }, &[_]bytebox.ValType{}, hostMulti3, null) catch return error.WasmExecFailed;
        env_imports.addHostFunction("__muloti4", &[_]bytebox.ValType{ .I32, .I64, .I64, .I64, .I64, .I32 }, &[_]bytebox.ValType{}, hostMuloti4, null) catch return error.WasmExecFailed;

        env_imports.addHostFunction(builtins.shim_symbols.roc_alloc, &[_]bytebox.ValType{ .I32, .I32 }, &[_]bytebox.ValType{.I32}, hostRocAlloc, &run_state) catch return error.WasmExecFailed;
        env_imports.addHostFunction(builtins.shim_symbols.roc_dealloc, &[_]bytebox.ValType{ .I32, .I32 }, &[_]bytebox.ValType{}, hostRocDealloc, null) catch return error.WasmExecFailed;
        env_imports.addHostFunction(builtins.shim_symbols.roc_realloc, &[_]bytebox.ValType{ .I32, .I32, .I32 }, &[_]bytebox.ValType{.I32}, hostRocRealloc, &run_state) catch return error.WasmExecFailed;
        env_imports.addHostFunction(builtins.shim_symbols.roc_dbg, &[_]bytebox.ValType{ .I32, .I32 }, &[_]bytebox.ValType{}, hostRocDbg, &run_state) catch return error.WasmExecFailed;
        env_imports.addHostFunction(builtins.shim_symbols.roc_expect_failed, &[_]bytebox.ValType{ .I32, .I32 }, &[_]bytebox.ValType{}, hostRocExpectFailed, &run_state) catch return error.WasmExecFailed;
        env_imports.addHostFunction(builtins.shim_symbols.roc_crashed, &[_]bytebox.ValType{ .I32, .I32 }, &[_]bytebox.ValType{}, hostRocCrashed, &run_state) catch return error.WasmExecFailed;

        const imports = [_]bytebox.ModuleImportPackage{env_imports};
        module_instance.instantiate(.{ .stack_size = 1024 * 256, .imports = &imports }) catch |err| {
            if (std.debug.runtime_safety) {
                debugPrint("wasm instantiate failed: {s}\n", .{@errorName(err)});
            }
            return error.WasmExecFailed;
        };
    } else {
        module_instance.instantiate(.{ .stack_size = 1024 * 256 }) catch |err| {
            if (std.debug.runtime_safety) {
                debugPrint("wasm instantiate failed: {s}\n", .{@errorName(err)});
            }
            return error.WasmExecFailed;
        };
    }

    const handle = module_instance.getFunctionHandle("main") catch |err| {
        if (std.debug.runtime_safety) {
            debugPrint("wasm get main handle failed: {s}\n", .{@errorName(err)});
        }
        return error.WasmExecFailed;
    };
    var returns: [1]bytebox.Val = undefined;
    module_instance.invoke(handle, &.{}, &returns, .{}) catch |err| {
        if (run_state.crashed) {
            return crashedWasmResult(&run_state);
        }
        if (std.debug.runtime_safety) {
            debugPrint("wasm invoke failed: {s}\n", .{@errorName(err)});
        }
        switch (err) {
            error.TrapUnreachable => {
                std.debug.assert(false);
                unreachable;
            },
            error.TrapDebug,
            error.TrapIndirectCallTypeMismatch,
            error.TrapIntegerDivisionByZero,
            error.TrapIntegerOverflow,
            error.TrapInvalidIntegerConversion,
            error.TrapInvalidResume,
            error.TrapNegativeDenominator,
            error.TrapOutOfBoundsMemoryAccess,
            error.TrapOutOfBoundsTableAccess,
            error.TrapStackExhausted,
            error.TrapUndefinedElement,
            error.TrapUninitializedElement,
            error.TrapUnknown,
            => return error.WasmExecFailed,
        }
        return error.WasmExecFailed;
    };
    if (run_state.crashed) return crashedWasmResult(&run_state);

    const str_ptr: u32 = @bitCast(returns[0].I32);
    const mem_slice = module_instance.memoryAll();
    if (str_ptr + wasm_roc_str_size > mem_slice.len) {
        if (std.debug.runtime_safety) {
            debugPrint("wasm invalid str ptr: ptr={d} mem_len={d}\n", .{ str_ptr, mem_slice.len });
        }
        return error.WasmExecFailed;
    }

    const byte11 = mem_slice[str_ptr + wasm_small_str_max_len];
    const str_data: []const u8 = if (byte11 & builtins.str.RocStr.small_str_flag != 0) sd: {
        const sso_len: u32 = builtins.str.RocStr.smallStrLenFromFlagByte(byte11);
        if (sso_len > wasm_small_str_max_len) {
            if (std.debug.runtime_safety) {
                debugPrint("wasm invalid sso len: ptr={d} len={d}\n", .{ str_ptr, sso_len });
            }
            return error.WasmExecFailed;
        }
        break :sd mem_slice[str_ptr..][0..sso_len];
    } else sd: {
        const data_ptr: u32 = @bitCast(mem_slice[str_ptr..][0..4].*);
        const data_len: u32 = @bitCast(mem_slice[str_ptr + 8 ..][0..4].*);
        if (data_ptr + data_len > mem_slice.len) {
            if (std.debug.runtime_safety) {
                debugPrint("wasm invalid str heap slice: str_ptr={d} data_ptr={d} data_len={d} mem_len={d}\n", .{ str_ptr, data_ptr, data_len, mem_slice.len });
            }
            return error.WasmExecFailed;
        }
        break :sd mem_slice[data_ptr..][0..data_len];
    };

    const output = try allocator.dupe(u8, str_data);
    errdefer allocator.free(output);
    return .{
        .outcome = .{ .returned = output },
        .events = try run_state.takeEvents(),
        .allocation_count = run_state.allocation_count,
    };
}

fn allocExtraBytes(alignment: u32) u32 {
    const ptr_width: u32 = 8;
    return if (alignment > ptr_width) alignment else ptr_width;
}

fn allocWasmData(state: *WasmRunState, module: *bytebox.ModuleInstance, alignment: u32, length: usize) u32 {
    state.allocation_count += 1;
    const align_val: u32 = if (alignment > 4) alignment else 4;
    const extra_bytes = allocExtraBytes(alignment);
    const alloc_ptr = (state.heap_ptr + align_val - 1) & ~(align_val - 1);
    const data_ptr = alloc_ptr + extra_bytes;
    const end: u64 = @as(u64, data_ptr) + length;
    if (end > std.math.maxInt(u32)) @panic("wasm evaluator exhausted the wasm32 address space");
    const current_len = module.memoryAll().len;
    if (end > current_len) {
        const missing = end - current_len;
        const pages = (missing + 65535) / 65536;
        if (!module.memoryGrow(@intCast(pages))) @panic("wasm evaluator could not grow linear memory");
    }
    state.heap_ptr = @intCast(end);
    const buffer = module.memoryAll();
    writeIntLittle(u32, buffer, data_ptr - 8, @intCast(length));
    writeIntLittle(u32, buffer, data_ptr - 4, 1);
    return data_ptr;
}

// RocOps callbacks follow the platform C ABI: a leading *RocOps (the i32 pointer to the
// RocOps struct in linear memory, unused here) followed by the natural arguments, with the
// result returned directly rather than written back into an args struct.

fn hostRocAlloc(ctx: ?*anyopaque, module: *bytebox.ModuleInstance, params: [*]const bytebox.Val, results: [*]bytebox.Val) error{}!void {
    const state: *WasmRunState = @ptrCast(@alignCast(ctx));
    const length: u32 = @bitCast(params[0].I32);
    const alignment: u32 = @bitCast(params[1].I32);
    const data_ptr = allocWasmData(state, module, alignment, length);
    results[0] = .{ .I32 = @bitCast(data_ptr) };
}

fn hostRocDealloc(_: ?*anyopaque, _: *bytebox.ModuleInstance, _: [*]const bytebox.Val, _: [*]bytebox.Val) error{}!void {}

fn hostRocRealloc(ctx: ?*anyopaque, module: *bytebox.ModuleInstance, params: [*]const bytebox.Val, results: [*]bytebox.Val) error{}!void {
    const state: *WasmRunState = @ptrCast(@alignCast(ctx));
    var buffer = module.store.getMemory(0).buffer();
    const old_data_ptr: u32 = @bitCast(params[0].I32);
    const new_length: u32 = @bitCast(params[1].I32);
    const alignment: u32 = @bitCast(params[2].I32);
    const old_length: usize = if (old_data_ptr >= 8 and old_data_ptr <= buffer.len)
        readIntLittle(u32, buffer, old_data_ptr - 8)
    else
        0;
    const data_ptr = allocWasmData(state, module, alignment, new_length);
    buffer = module.store.getMemory(0).buffer();
    const copy_len = @min(old_length, new_length);
    if (copy_len > 0 and old_data_ptr + copy_len <= buffer.len and data_ptr + copy_len <= buffer.len) {
        @memcpy(buffer[data_ptr..][0..copy_len], buffer[old_data_ptr..][0..copy_len]);
    }
    results[0] = .{ .I32 = @bitCast(data_ptr) };
}

fn hostRocDbg(ctx: ?*anyopaque, module: *bytebox.ModuleInstance, params: [*]const bytebox.Val, _: [*]bytebox.Val) error{}!void {
    const state: *WasmRunState = @ptrCast(@alignCast(ctx));
    const buffer = module.store.getMemory(0).buffer();
    const msg_ptr: u32 = @bitCast(params[0].I32);
    const msg_len: u32 = @bitCast(params[1].I32);
    if (msg_ptr + msg_len > buffer.len) return;
    state.recordEvent(.dbg, buffer[msg_ptr..][0..msg_len]);
}

fn hostRocExpectFailed(ctx: ?*anyopaque, module: *bytebox.ModuleInstance, params: [*]const bytebox.Val, _: [*]bytebox.Val) error{}!void {
    const state: *WasmRunState = @ptrCast(@alignCast(ctx));
    const buffer = module.store.getMemory(0).buffer();
    const msg_ptr: u32 = @bitCast(params[0].I32);
    const msg_len: u32 = @bitCast(params[1].I32);
    if (msg_ptr + msg_len > buffer.len) return;
    state.recordEvent(.expect_failed, buffer[msg_ptr..][0..msg_len]);
}

fn hostRocCrashed(ctx: ?*anyopaque, module: *bytebox.ModuleInstance, params: [*]const bytebox.Val, _: [*]bytebox.Val) error{}!void {
    const state: *WasmRunState = @ptrCast(@alignCast(ctx));
    const buffer = module.store.getMemory(0).buffer();
    const msg_ptr: u32 = @bitCast(params[0].I32);
    const msg_len: u32 = @bitCast(params[1].I32);
    if (msg_ptr + msg_len > buffer.len) return;
    state.recordCrash(buffer[msg_ptr..][0..msg_len]);
}

// --- Compiler-rt intrinsics ---

/// __multi3: 128-bit signed multiply. result_ptr = a * b (truncating to 128 bits).
fn hostMulti3(_: ?*anyopaque, module: *bytebox.ModuleInstance, params: [*]const bytebox.Val, _: [*]bytebox.Val) error{}!void {
    const buffer = module.store.getMemory(0).buffer();
    const result_ptr: usize = @intCast(params[0].I32);
    const a_lo: u64 = @bitCast(params[1].I64);
    const a_hi: u64 = @bitCast(params[2].I64);
    const b_lo: u64 = @bitCast(params[3].I64);
    const b_hi: u64 = @bitCast(params[4].I64);
    const a: i128 = @bitCast(@as(u128, a_hi) << 64 | @as(u128, a_lo));
    const b: i128 = @bitCast(@as(u128, b_hi) << 64 | @as(u128, b_lo));
    const result = i128h.mul_i128(a, b);
    const result_u128: u128 = @bitCast(result);
    writeIntLittle(u64, buffer, result_ptr, @truncate(result_u128));
    writeIntLittle(u64, buffer, result_ptr + 8, @truncate(result_u128 >> 64));
}

/// __muloti4: 128-bit signed multiply with overflow detection.
fn hostMuloti4(_: ?*anyopaque, module: *bytebox.ModuleInstance, params: [*]const bytebox.Val, _: [*]bytebox.Val) error{}!void {
    const buffer = module.store.getMemory(0).buffer();
    const result_ptr: usize = @intCast(params[0].I32);
    const a_lo: u64 = @bitCast(params[1].I64);
    const a_hi: u64 = @bitCast(params[2].I64);
    const b_lo: u64 = @bitCast(params[3].I64);
    const b_hi: u64 = @bitCast(params[4].I64);
    const overflow_ptr: usize = @intCast(params[5].I32);
    const a: i128 = @bitCast(@as(u128, a_hi) << 64 | @as(u128, a_lo));
    const b: i128 = @bitCast(@as(u128, b_hi) << 64 | @as(u128, b_lo));
    var overflow_c: c_int = 0;
    const result = i128h.mulWithOverflow_i128(a, b, &overflow_c);
    const overflow: i32 = @intCast(overflow_c);
    const result_u128: u128 = @bitCast(result);
    writeIntLittle(u64, buffer, result_ptr, @truncate(result_u128));
    writeIntLittle(u64, buffer, result_ptr + 8, @truncate(result_u128 >> 64));
    writeIntLittle(i32, buffer, overflow_ptr, overflow);
}
