//! WebAssembly execution runner for the REPL and eval tests.
//!
//! Provides host-function bindings and memory management for running
//! Roc expressions compiled to WebAssembly via the Bytebox runtime.

const std = @import("std");
const builtins = @import("builtins");
const can = @import("can");
const eval_mod = @import("eval");
const bytebox = @import("bytebox");
const i128h = builtins.compiler_rt_128;

const ModuleEnv = can.ModuleEnv;
const CIR = can.CIR;
const WasmEvaluator = eval_mod.WasmEvaluator;

/// Errors that can occur during WebAssembly evaluation.
pub const WasmEvalError = error{
    WasmEvaluatorInitFailed,
    WasmGenerateCodeFailed,
    WasmExecFailed,
    UnsupportedLayout,
    OutOfMemory,
};

/// Compiles and executes a Roc expression via the WebAssembly backend, returning the result as a string.
pub fn wasmEvaluatorStr(allocator: std.mem.Allocator, module_env: *ModuleEnv, expr_idx: CIR.Expr.Idx, builtin_module_env: *const ModuleEnv) WasmEvalError![]const u8 {
    wasm_heap_ptr = 65536;

    var wasm_eval = WasmEvaluator.init(allocator) catch return error.WasmEvaluatorInitFailed;
    defer wasm_eval.deinit();

    const all_module_envs = [_]*ModuleEnv{ @constCast(builtin_module_env), module_env };
    var wasm_result = wasm_eval.generateWasm(module_env, expr_idx, &all_module_envs) catch return error.WasmGenerateCodeFailed;
    defer wasm_result.deinit();

    if (wasm_result.wasm_bytes.len == 0) return error.WasmGenerateCodeFailed;

    var arena_impl = std.heap.ArenaAllocator.init(allocator);
    defer arena_impl.deinit();
    const arena = arena_impl.allocator();

    var module_def = bytebox.createModuleDefinition(arena, .{}) catch return error.WasmExecFailed;
    module_def.decode(wasm_result.wasm_bytes) catch return error.WasmExecFailed;

    var module_instance = bytebox.createModuleInstance(.Stack, module_def, std.heap.page_allocator) catch return error.WasmExecFailed;
    defer module_instance.destroy();

    if (wasm_result.has_imports) {
        var env_imports = bytebox.ModuleImportPackage.init("env", null, null, allocator) catch return error.WasmExecFailed;
        defer env_imports.deinit();

        env_imports.addHostFunction("roc_dec_mul", &[_]bytebox.ValType{ .I32, .I32, .I32 }, &[_]bytebox.ValType{}, hostDecMul, null) catch return error.WasmExecFailed;
        env_imports.addHostFunction("roc_dec_to_str", &[_]bytebox.ValType{ .I32, .I32 }, &[_]bytebox.ValType{.I32}, hostDecToStr, null) catch return error.WasmExecFailed;
        env_imports.addHostFunction("roc_str_eq", &[_]bytebox.ValType{ .I32, .I32 }, &[_]bytebox.ValType{.I32}, hostStrEq, null) catch return error.WasmExecFailed;
        env_imports.addHostFunction("roc_list_eq", &[_]bytebox.ValType{ .I32, .I32, .I32 }, &[_]bytebox.ValType{.I32}, hostListEq, null) catch return error.WasmExecFailed;

        env_imports.addHostFunction("roc_alloc", &[_]bytebox.ValType{ .I32, .I32 }, &[_]bytebox.ValType{}, hostRocAlloc, null) catch return error.WasmExecFailed;
        env_imports.addHostFunction("roc_dealloc", &[_]bytebox.ValType{ .I32, .I32 }, &[_]bytebox.ValType{}, hostRocDealloc, null) catch return error.WasmExecFailed;
        env_imports.addHostFunction("roc_realloc", &[_]bytebox.ValType{ .I32, .I32 }, &[_]bytebox.ValType{}, hostRocRealloc, null) catch return error.WasmExecFailed;
        env_imports.addHostFunction("roc_dbg", &[_]bytebox.ValType{ .I32, .I32 }, &[_]bytebox.ValType{}, hostRocDbg, null) catch return error.WasmExecFailed;
        env_imports.addHostFunction("roc_expect_failed", &[_]bytebox.ValType{ .I32, .I32 }, &[_]bytebox.ValType{}, hostRocExpectFailed, null) catch return error.WasmExecFailed;
        env_imports.addHostFunction("roc_crashed", &[_]bytebox.ValType{ .I32, .I32 }, &[_]bytebox.ValType{}, hostRocCrashed, null) catch return error.WasmExecFailed;

        env_imports.addHostFunction("roc_i128_div_s", &[_]bytebox.ValType{ .I32, .I32, .I32 }, &[_]bytebox.ValType{}, hostI128DivS, null) catch return error.WasmExecFailed;
        env_imports.addHostFunction("roc_i128_mod_s", &[_]bytebox.ValType{ .I32, .I32, .I32 }, &[_]bytebox.ValType{}, hostI128ModS, null) catch return error.WasmExecFailed;
        env_imports.addHostFunction("roc_u128_div", &[_]bytebox.ValType{ .I32, .I32, .I32 }, &[_]bytebox.ValType{}, hostU128Div, null) catch return error.WasmExecFailed;
        env_imports.addHostFunction("roc_u128_mod", &[_]bytebox.ValType{ .I32, .I32, .I32 }, &[_]bytebox.ValType{}, hostU128Mod, null) catch return error.WasmExecFailed;
        env_imports.addHostFunction("roc_dec_div", &[_]bytebox.ValType{ .I32, .I32, .I32 }, &[_]bytebox.ValType{}, hostDecDiv, null) catch return error.WasmExecFailed;
        env_imports.addHostFunction("roc_dec_div_trunc", &[_]bytebox.ValType{ .I32, .I32, .I32 }, &[_]bytebox.ValType{}, hostDecDivTrunc, null) catch return error.WasmExecFailed;
        env_imports.addHostFunction("roc_i32_mod_by", &[_]bytebox.ValType{ .I32, .I32 }, &[_]bytebox.ValType{.I32}, hostI32ModBy, null) catch return error.WasmExecFailed;
        env_imports.addHostFunction("roc_i64_mod_by", &[_]bytebox.ValType{ .I64, .I64 }, &[_]bytebox.ValType{.I64}, hostI64ModBy, null) catch return error.WasmExecFailed;
        env_imports.addHostFunction("roc_i128_to_str", &[_]bytebox.ValType{ .I32, .I32 }, &[_]bytebox.ValType{.I32}, hostI128ToStr, null) catch return error.WasmExecFailed;
        env_imports.addHostFunction("roc_u128_to_str", &[_]bytebox.ValType{ .I32, .I32 }, &[_]bytebox.ValType{.I32}, hostU128ToStr, null) catch return error.WasmExecFailed;
        env_imports.addHostFunction("roc_float_to_str", &[_]bytebox.ValType{ .I64, .I32, .I32 }, &[_]bytebox.ValType{.I32}, hostFloatToStr, null) catch return error.WasmExecFailed;
        env_imports.addHostFunction("roc_u128_to_dec", &[_]bytebox.ValType{ .I32, .I32 }, &[_]bytebox.ValType{.I32}, hostU128ToDec, null) catch return error.WasmExecFailed;
        env_imports.addHostFunction("roc_i128_to_dec", &[_]bytebox.ValType{ .I32, .I32 }, &[_]bytebox.ValType{.I32}, hostI128ToDec, null) catch return error.WasmExecFailed;
        env_imports.addHostFunction("roc_dec_to_i128", &[_]bytebox.ValType{ .I32, .I32 }, &[_]bytebox.ValType{.I32}, hostDecToI128, null) catch return error.WasmExecFailed;
        env_imports.addHostFunction("roc_dec_to_u128", &[_]bytebox.ValType{ .I32, .I32 }, &[_]bytebox.ValType{.I32}, hostDecToU128, null) catch return error.WasmExecFailed;
        env_imports.addHostFunction("roc_dec_to_f32", &[_]bytebox.ValType{.I32}, &[_]bytebox.ValType{.F32}, hostDecToF32, null) catch return error.WasmExecFailed;
        env_imports.addHostFunction("roc_list_str_eq", &[_]bytebox.ValType{ .I32, .I32 }, &[_]bytebox.ValType{.I32}, hostListStrEq, null) catch return error.WasmExecFailed;
        env_imports.addHostFunction("roc_list_list_eq", &[_]bytebox.ValType{ .I32, .I32, .I32 }, &[_]bytebox.ValType{.I32}, hostListListEq, null) catch return error.WasmExecFailed;

        inline for (.{
            .{ "roc_str_trim", hostStrTrim },
            .{ "roc_str_trim_start", hostStrTrimStart },
            .{ "roc_str_trim_end", hostStrTrimEnd },
            .{ "roc_str_with_ascii_lowercased", hostStrWithAsciiLowercased },
            .{ "roc_str_with_ascii_uppercased", hostStrWithAsciiUppercased },
            .{ "roc_str_release_excess_capacity", hostStrReleaseExcessCapacity },
            .{ "roc_str_with_capacity", hostStrWithCapacity },
        }) |entry| {
            env_imports.addHostFunction(entry[0], &[_]bytebox.ValType{ .I32, .I32 }, &[_]bytebox.ValType{}, entry[1], null) catch return error.WasmExecFailed;
        }
        env_imports.addHostFunction("roc_str_from_utf8", &[_]bytebox.ValType{ .I32, .I32, .I32, .I32 }, &[_]bytebox.ValType{}, hostStrFromUtf8, null) catch return error.WasmExecFailed;

        inline for (.{
            .{ "roc_str_with_prefix", hostStrWithPrefix },
            .{ "roc_str_drop_prefix", hostStrDropPrefix },
            .{ "roc_str_drop_suffix", hostStrDropSuffix },
            .{ "roc_str_concat", hostStrConcat },
            .{ "roc_str_split", hostStrSplit },
            .{ "roc_str_join_with", hostStrJoinWith },
            .{ "roc_str_repeat", hostStrRepeat },
            .{ "roc_str_reserve", hostStrReserve },
        }) |entry| {
            env_imports.addHostFunction(entry[0], &[_]bytebox.ValType{ .I32, .I32, .I32 }, &[_]bytebox.ValType{}, entry[1], null) catch return error.WasmExecFailed;
        }

        env_imports.addHostFunction("roc_str_caseless_ascii_equals", &[_]bytebox.ValType{ .I32, .I32 }, &[_]bytebox.ValType{.I32}, hostStrCaselessAsciiEquals, null) catch return error.WasmExecFailed;
        env_imports.addHostFunction("roc_int_from_str", &[_]bytebox.ValType{ .I32, .I32, .I32, .I32, .I32 }, &[_]bytebox.ValType{}, hostIntFromStr, null) catch return error.WasmExecFailed;
        env_imports.addHostFunction("roc_dec_from_str", &[_]bytebox.ValType{ .I32, .I32, .I32 }, &[_]bytebox.ValType{}, hostDecFromStr, null) catch return error.WasmExecFailed;
        env_imports.addHostFunction("roc_float_from_str", &[_]bytebox.ValType{ .I32, .I32, .I32, .I32 }, &[_]bytebox.ValType{}, hostFloatFromStr, null) catch return error.WasmExecFailed;
        env_imports.addHostFunction("roc_list_append_unsafe", &[_]bytebox.ValType{ .I32, .I32, .I32, .I32, .I32 }, &[_]bytebox.ValType{}, hostListAppendUnsafe, null) catch return error.WasmExecFailed;
        env_imports.addHostFunction("roc_list_sort_with", &[_]bytebox.ValType{ .I32, .I32, .I32, .I32, .I32 }, &[_]bytebox.ValType{}, hostListSortWith, null) catch return error.WasmExecFailed;
        env_imports.addHostFunction("roc_list_reverse", &[_]bytebox.ValType{ .I32, .I32, .I32, .I32 }, &[_]bytebox.ValType{}, hostListReverse, null) catch return error.WasmExecFailed;

        const imports = [_]bytebox.ModuleImportPackage{env_imports};
        module_instance.instantiate(.{ .stack_size = 1024 * 256, .imports = &imports }) catch return error.WasmExecFailed;
    } else {
        module_instance.instantiate(.{ .stack_size = 1024 * 256 }) catch return error.WasmExecFailed;
    }

    const handle = module_instance.getFunctionHandle("main") catch return error.WasmExecFailed;
    var params = [1]bytebox.Val{.{ .I32 = 0 }};
    var returns: [1]bytebox.Val = undefined;
    _ = module_instance.invoke(handle, &params, &returns, .{}) catch |err| {
        std.debug.print("wasm invoke failed: {}\n", .{err});
        return error.WasmExecFailed;
    };

    const str_ptr: u32 = @bitCast(returns[0].I32);
    const mem_slice = module_instance.memoryAll();
    if (str_ptr + 12 > mem_slice.len) {
        std.debug.print("wasm result ptr out of bounds: ptr={d} mem={d}\n", .{ str_ptr, mem_slice.len });
        return error.WasmExecFailed;
    }

    const byte11 = mem_slice[str_ptr + 11];
    const str_data: []const u8 = if (byte11 & 0x80 != 0) sd: {
        const sso_len: u32 = byte11 & 0x7F;
        if (sso_len > 11) {
            std.debug.print("wasm invalid SSO len: {d}\n", .{sso_len});
            return error.WasmExecFailed;
        }
        break :sd mem_slice[str_ptr..][0..sso_len];
    } else sd: {
        const data_ptr: u32 = @bitCast(mem_slice[str_ptr..][0..4].*);
        const data_len: u32 = @bitCast(mem_slice[str_ptr + 4 ..][0..4].*);
        if (data_ptr + data_len > mem_slice.len) {
            std.debug.print("wasm heap str out of bounds: ptr={d} len={d} mem={d}\n", .{ data_ptr, data_len, mem_slice.len });
            return error.WasmExecFailed;
        }
        break :sd mem_slice[data_ptr..][0..data_len];
    };

    return allocator.dupe(u8, str_data);
}

fn hostDecMul(_: ?*anyopaque, module: *bytebox.ModuleInstance, params: [*]const bytebox.Val, _: [*]bytebox.Val) error{}!void {
    const RocDec = builtins.dec.RocDec;
    const buffer = module.store.getMemory(0).buffer();
    const lhs_ptr: usize = @intCast(params[0].I32);
    const rhs_ptr: usize = @intCast(params[1].I32);
    const result_ptr: usize = @intCast(params[2].I32);
    if (lhs_ptr + 16 > buffer.len or rhs_ptr + 16 > buffer.len or result_ptr + 16 > buffer.len) return;
    const lhs_low: u64 = std.mem.readInt(u64, buffer[lhs_ptr..][0..8], .little);
    const lhs_high: u64 = std.mem.readInt(u64, buffer[lhs_ptr + 8 ..][0..8], .little);
    const lhs_i128: i128 = @bitCast(@as(u128, lhs_high) << 64 | @as(u128, lhs_low));
    const rhs_low: u64 = std.mem.readInt(u64, buffer[rhs_ptr..][0..8], .little);
    const rhs_high: u64 = std.mem.readInt(u64, buffer[rhs_ptr + 8 ..][0..8], .little);
    const rhs_i128: i128 = @bitCast(@as(u128, rhs_high) << 64 | @as(u128, rhs_low));
    const lhs_dec = RocDec{ .num = lhs_i128 };
    const rhs_dec = RocDec{ .num = rhs_i128 };
    const result = lhs_dec.mulWithOverflow(rhs_dec);
    const result_u128: u128 = @bitCast(result.value.num);
    std.mem.writeInt(u64, buffer[result_ptr..][0..8], @truncate(result_u128), .little);
    std.mem.writeInt(u64, buffer[result_ptr + 8 ..][0..8], @truncate(result_u128 >> 64), .little);
}

fn hostDecToStr(_: ?*anyopaque, module: *bytebox.ModuleInstance, params: [*]const bytebox.Val, results: [*]bytebox.Val) error{}!void {
    const RocDec = builtins.dec.RocDec;
    const buffer = module.store.getMemory(0).buffer();
    const dec_ptr: usize = @intCast(params[0].I32);
    const buf_ptr: usize = @intCast(params[1].I32);
    if (dec_ptr + 16 > buffer.len or buf_ptr + 48 > buffer.len) {
        results[0] = .{ .I32 = 0 };
        return;
    }
    const low: u64 = std.mem.readInt(u64, buffer[dec_ptr..][0..8], .little);
    const high: u64 = std.mem.readInt(u64, buffer[dec_ptr + 8 ..][0..8], .little);
    const dec_i128: i128 = @bitCast(@as(u128, high) << 64 | @as(u128, low));
    const dec = RocDec{ .num = dec_i128 };
    var fmt_buf: [RocDec.max_str_length]u8 = undefined;
    const formatted = dec.format_to_buf(&fmt_buf);
    @memcpy(buffer[buf_ptr..][0..formatted.len], formatted);
    results[0] = .{ .I32 = @intCast(formatted.len) };
}

fn hostStrEq(_: ?*anyopaque, module: *bytebox.ModuleInstance, params: [*]const bytebox.Val, results: [*]bytebox.Val) error{}!void {
    const buffer = module.store.getMemory(0).buffer();
    const a_ptr: usize = @intCast(params[0].I32);
    const b_ptr: usize = @intCast(params[1].I32);
    if (a_ptr + 12 > buffer.len or b_ptr + 12 > buffer.len) {
        results[0] = .{ .I32 = 0 };
        return;
    }
    const a = readWasmStr(buffer, a_ptr);
    const b = readWasmStr(buffer, b_ptr);
    results[0] = .{ .I32 = if (a.len == b.len and std.mem.eql(u8, a.data[0..a.len], b.data[0..b.len])) 1 else 0 };
}

fn hostListEq(_: ?*anyopaque, module: *bytebox.ModuleInstance, params: [*]const bytebox.Val, results: [*]bytebox.Val) error{}!void {
    const buffer = module.store.getMemory(0).buffer();
    const a_list_ptr: usize = @intCast(params[0].I32);
    const b_list_ptr: usize = @intCast(params[1].I32);
    const elem_size: usize = @intCast(params[2].I32);
    if (a_list_ptr + 12 > buffer.len or b_list_ptr + 12 > buffer.len) {
        results[0] = .{ .I32 = 0 };
        return;
    }
    const a_data_ptr: usize = @intCast(std.mem.readInt(u32, buffer[a_list_ptr..][0..4], .little));
    const a_len: usize = @intCast(std.mem.readInt(u32, buffer[a_list_ptr + 4 ..][0..4], .little));
    const b_data_ptr: usize = @intCast(std.mem.readInt(u32, buffer[b_list_ptr..][0..4], .little));
    const b_len: usize = @intCast(std.mem.readInt(u32, buffer[b_list_ptr + 4 ..][0..4], .little));
    if (a_len != b_len) {
        results[0] = .{ .I32 = 0 };
        return;
    }
    if (a_len == 0) {
        results[0] = .{ .I32 = 1 };
        return;
    }
    const total_bytes = a_len * elem_size;
    if (a_data_ptr + total_bytes > buffer.len or b_data_ptr + total_bytes > buffer.len) {
        results[0] = .{ .I32 = 0 };
        return;
    }
    results[0] = .{ .I32 = if (std.mem.eql(u8, buffer[a_data_ptr..][0..total_bytes], buffer[b_data_ptr..][0..total_bytes])) 1 else 0 };
}

fn readI128FromMem(buffer: []u8, ptr: usize) i128 {
    const low = std.mem.readInt(u64, buffer[ptr..][0..8], .little);
    const high = std.mem.readInt(i64, buffer[ptr + 8 ..][0..8], .little);
    return @as(i128, high) << 64 | low;
}

fn readU128FromMem(buffer: []u8, ptr: usize) u128 {
    const low = std.mem.readInt(u64, buffer[ptr..][0..8], .little);
    const high = std.mem.readInt(u64, buffer[ptr + 8 ..][0..8], .little);
    return @as(u128, high) << 64 | low;
}

fn writeI128ToMem(buffer: []u8, ptr: usize, val: i128) void {
    const as_u128: u128 = @bitCast(val);
    std.mem.writeInt(u64, buffer[ptr..][0..8], @truncate(as_u128), .little);
    std.mem.writeInt(u64, buffer[ptr + 8 ..][0..8], @truncate(as_u128 >> 64), .little);
}

fn writeU128ToMem(buffer: []u8, ptr: usize, val: u128) void {
    std.mem.writeInt(u64, buffer[ptr..][0..8], @truncate(val), .little);
    std.mem.writeInt(u64, buffer[ptr + 8 ..][0..8], @truncate(val >> 64), .little);
}

fn hostI128DivS(_: ?*anyopaque, module: *bytebox.ModuleInstance, params: [*]const bytebox.Val, _: [*]bytebox.Val) error{}!void {
    const buffer = module.store.getMemory(0).buffer();
    writeI128ToMem(buffer, @intCast(params[2].I32), @divTrunc(readI128FromMem(buffer, @intCast(params[0].I32)), readI128FromMem(buffer, @intCast(params[1].I32))));
}

fn hostI128ModS(_: ?*anyopaque, module: *bytebox.ModuleInstance, params: [*]const bytebox.Val, _: [*]bytebox.Val) error{}!void {
    const buffer = module.store.getMemory(0).buffer();
    writeI128ToMem(buffer, @intCast(params[2].I32), @rem(readI128FromMem(buffer, @intCast(params[0].I32)), readI128FromMem(buffer, @intCast(params[1].I32))));
}

fn hostU128Div(_: ?*anyopaque, module: *bytebox.ModuleInstance, params: [*]const bytebox.Val, _: [*]bytebox.Val) error{}!void {
    const buffer = module.store.getMemory(0).buffer();
    writeU128ToMem(buffer, @intCast(params[2].I32), readU128FromMem(buffer, @intCast(params[0].I32)) / readU128FromMem(buffer, @intCast(params[1].I32)));
}

fn hostU128Mod(_: ?*anyopaque, module: *bytebox.ModuleInstance, params: [*]const bytebox.Val, _: [*]bytebox.Val) error{}!void {
    const buffer = module.store.getMemory(0).buffer();
    writeU128ToMem(buffer, @intCast(params[2].I32), readU128FromMem(buffer, @intCast(params[0].I32)) % readU128FromMem(buffer, @intCast(params[1].I32)));
}

fn hostI32ModBy(_: ?*anyopaque, _: *bytebox.ModuleInstance, params: [*]const bytebox.Val, results: [*]bytebox.Val) error{}!void {
    results[0] = .{ .I32 = @mod(params[0].I32, params[1].I32) };
}

fn hostI64ModBy(_: ?*anyopaque, _: *bytebox.ModuleInstance, params: [*]const bytebox.Val, results: [*]bytebox.Val) error{}!void {
    results[0] = .{ .I64 = @mod(params[0].I64, params[1].I64) };
}

fn hostDecDiv(_: ?*anyopaque, module: *bytebox.ModuleInstance, params: [*]const bytebox.Val, _: [*]bytebox.Val) error{}!void {
    const buffer = module.store.getMemory(0).buffer();
    const lhs = readI128FromMem(buffer, @intCast(params[0].I32));
    const rhs = readI128FromMem(buffer, @intCast(params[1].I32));
    const scaled: i256 = @as(i256, lhs) * 1_000_000_000_000_000_000;
    writeI128ToMem(buffer, @intCast(params[2].I32), @intCast(@divTrunc(scaled, rhs)));
}

fn hostDecDivTrunc(_: ?*anyopaque, module: *bytebox.ModuleInstance, params: [*]const bytebox.Val, _: [*]bytebox.Val) error{}!void {
    const buffer = module.store.getMemory(0).buffer();
    const lhs = readI128FromMem(buffer, @intCast(params[0].I32));
    const rhs = readI128FromMem(buffer, @intCast(params[1].I32));
    writeI128ToMem(buffer, @intCast(params[2].I32), @divTrunc(lhs, rhs) * 1_000_000_000_000_000_000);
}

fn hostI128ToStr(_: ?*anyopaque, module: *bytebox.ModuleInstance, params: [*]const bytebox.Val, results: [*]bytebox.Val) error{}!void {
    const buffer = module.store.getMemory(0).buffer();
    const val_ptr: usize = @intCast(params[0].I32);
    const buf_ptr: usize = @intCast(params[1].I32);
    if (val_ptr + 16 > buffer.len or buf_ptr + 48 > buffer.len) {
        results[0] = .{ .I32 = 0 };
        return;
    }
    var fmt_buf: [48]u8 = undefined;
    const formatted = std.fmt.bufPrint(&fmt_buf, "{d}", .{readI128FromMem(buffer, val_ptr)}) catch {
        results[0] = .{ .I32 = 0 };
        return;
    };
    @memcpy(buffer[buf_ptr..][0..formatted.len], formatted);
    results[0] = .{ .I32 = @intCast(formatted.len) };
}

fn hostU128ToStr(_: ?*anyopaque, module: *bytebox.ModuleInstance, params: [*]const bytebox.Val, results: [*]bytebox.Val) error{}!void {
    const buffer = module.store.getMemory(0).buffer();
    const val_ptr: usize = @intCast(params[0].I32);
    const buf_ptr: usize = @intCast(params[1].I32);
    if (val_ptr + 16 > buffer.len or buf_ptr + 48 > buffer.len) {
        results[0] = .{ .I32 = 0 };
        return;
    }
    var fmt_buf: [48]u8 = undefined;
    const formatted = std.fmt.bufPrint(&fmt_buf, "{d}", .{readU128FromMem(buffer, val_ptr)}) catch {
        results[0] = .{ .I32 = 0 };
        return;
    };
    @memcpy(buffer[buf_ptr..][0..formatted.len], formatted);
    results[0] = .{ .I32 = @intCast(formatted.len) };
}

fn hostFloatToStr(_: ?*anyopaque, module: *bytebox.ModuleInstance, params: [*]const bytebox.Val, results: [*]bytebox.Val) error{}!void {
    const buffer = module.store.getMemory(0).buffer();
    const val_bits: u64 = @bitCast(params[0].I64);
    const is_f32 = params[1].I32 != 0;
    const buf_ptr: usize = @intCast(params[2].I32);

    if (buf_ptr + 48 > buffer.len) {
        results[0] = .{ .I32 = 0 };
        return;
    }

    var fmt_buf: [400]u8 = undefined;
    const formatted = if (is_f32) blk: {
        const f32_val: f32 = @bitCast(@as(u32, @truncate(val_bits)));
        break :blk i128h.f32_to_str(&fmt_buf, f32_val);
    } else blk: {
        const f64_val: f64 = @bitCast(val_bits);
        break :blk i128h.f64_to_str(&fmt_buf, f64_val);
    };

    @memcpy(buffer[buf_ptr..][0..formatted.len], formatted);
    results[0] = .{ .I32 = @intCast(formatted.len) };
}

fn hostU128ToDec(_: ?*anyopaque, module: *bytebox.ModuleInstance, params: [*]const bytebox.Val, results: [*]bytebox.Val) error{}!void {
    const buffer = module.store.getMemory(0).buffer();
    const val = readU128FromMem(buffer, @intCast(params[0].I32));
    const max_val: u128 = @as(u128, @bitCast(@as(i128, std.math.maxInt(i128)))) / 1_000_000_000_000_000_000;
    if (val > max_val) {
        results[0] = .{ .I32 = 0 };
        return;
    }
    writeI128ToMem(buffer, @intCast(params[1].I32), @intCast(val * 1_000_000_000_000_000_000));
    results[0] = .{ .I32 = 1 };
}

fn hostI128ToDec(_: ?*anyopaque, module: *bytebox.ModuleInstance, params: [*]const bytebox.Val, results: [*]bytebox.Val) error{}!void {
    const buffer = module.store.getMemory(0).buffer();
    const wide_result = @as(i256, readI128FromMem(buffer, @intCast(params[0].I32))) * 1_000_000_000_000_000_000;
    if (wide_result > std.math.maxInt(i128) or wide_result < std.math.minInt(i128)) {
        results[0] = .{ .I32 = 0 };
        return;
    }
    writeI128ToMem(buffer, @intCast(params[1].I32), @intCast(wide_result));
    results[0] = .{ .I32 = 1 };
}

fn hostDecToI128(_: ?*anyopaque, module: *bytebox.ModuleInstance, params: [*]const bytebox.Val, results: [*]bytebox.Val) error{}!void {
    const buffer = module.store.getMemory(0).buffer();
    writeI128ToMem(buffer, @intCast(params[1].I32), @divTrunc(readI128FromMem(buffer, @intCast(params[0].I32)), 1_000_000_000_000_000_000));
    results[0] = .{ .I32 = 1 };
}

fn hostDecToU128(_: ?*anyopaque, module: *bytebox.ModuleInstance, params: [*]const bytebox.Val, results: [*]bytebox.Val) error{}!void {
    const buffer = module.store.getMemory(0).buffer();
    const result = @divTrunc(readI128FromMem(buffer, @intCast(params[0].I32)), 1_000_000_000_000_000_000);
    if (result < 0) {
        results[0] = .{ .I32 = 0 };
        return;
    }
    writeU128ToMem(buffer, @intCast(params[1].I32), @intCast(result));
    results[0] = .{ .I32 = 1 };
}

fn hostDecToF32(_: ?*anyopaque, module: *bytebox.ModuleInstance, params: [*]const bytebox.Val, results: [*]bytebox.Val) error{}!void {
    const buffer = module.store.getMemory(0).buffer();
    const f64_val: f64 = @as(f64, @floatFromInt(readI128FromMem(buffer, @intCast(params[0].I32)))) / 1_000_000_000_000_000_000.0;
    results[0] = .{ .F32 = @floatCast(f64_val) };
}

fn hostListStrEq(_: ?*anyopaque, module: *bytebox.ModuleInstance, params: [*]const bytebox.Val, results: [*]bytebox.Val) error{}!void {
    const buffer = module.store.getMemory(0).buffer();
    const a_ptr: usize = @intCast(params[0].I32);
    const b_ptr: usize = @intCast(params[1].I32);
    if (a_ptr + 12 > buffer.len or b_ptr + 12 > buffer.len) {
        results[0] = .{ .I32 = 0 };
        return;
    }
    const a_data_ptr: usize = @intCast(std.mem.readInt(u32, buffer[a_ptr..][0..4], .little));
    const a_len: usize = @intCast(std.mem.readInt(u32, buffer[a_ptr + 4 ..][0..4], .little));
    const b_data_ptr: usize = @intCast(std.mem.readInt(u32, buffer[b_ptr..][0..4], .little));
    const b_len: usize = @intCast(std.mem.readInt(u32, buffer[b_ptr + 4 ..][0..4], .little));
    if (a_len != b_len) {
        results[0] = .{ .I32 = 0 };
        return;
    }
    for (0..a_len) |i| {
        const a = readWasmStr(buffer, a_data_ptr + i * 12);
        const b = readWasmStr(buffer, b_data_ptr + i * 12);
        if (a.len != b.len or !std.mem.eql(u8, a.data[0..a.len], b.data[0..b.len])) {
            results[0] = .{ .I32 = 0 };
            return;
        }
    }
    results[0] = .{ .I32 = 1 };
}

fn hostListListEq(_: ?*anyopaque, module: *bytebox.ModuleInstance, params: [*]const bytebox.Val, results: [*]bytebox.Val) error{}!void {
    const buffer = module.store.getMemory(0).buffer();
    const a_ptr: usize = @intCast(params[0].I32);
    const b_ptr: usize = @intCast(params[1].I32);
    const inner_elem_size: usize = @intCast(params[2].I32);
    if (a_ptr + 12 > buffer.len or b_ptr + 12 > buffer.len) {
        results[0] = .{ .I32 = 0 };
        return;
    }
    const a_data_ptr: usize = @intCast(std.mem.readInt(u32, buffer[a_ptr..][0..4], .little));
    const a_len: usize = @intCast(std.mem.readInt(u32, buffer[a_ptr + 4 ..][0..4], .little));
    const b_data_ptr: usize = @intCast(std.mem.readInt(u32, buffer[b_ptr..][0..4], .little));
    const b_len: usize = @intCast(std.mem.readInt(u32, buffer[b_ptr + 4 ..][0..4], .little));
    if (a_len != b_len) {
        results[0] = .{ .I32 = 0 };
        return;
    }
    for (0..a_len) |i| {
        const a_inner_ptr = a_data_ptr + i * 12;
        const b_inner_ptr = b_data_ptr + i * 12;
        const a_inner_data: usize = @intCast(std.mem.readInt(u32, buffer[a_inner_ptr..][0..4], .little));
        const a_inner_len: usize = @intCast(std.mem.readInt(u32, buffer[a_inner_ptr + 4 ..][0..4], .little));
        const b_inner_data: usize = @intCast(std.mem.readInt(u32, buffer[b_inner_ptr..][0..4], .little));
        const b_inner_len: usize = @intCast(std.mem.readInt(u32, buffer[b_inner_ptr + 4 ..][0..4], .little));
        const inner_bytes = a_inner_len * inner_elem_size;
        if (a_inner_len != b_inner_len or !std.mem.eql(u8, buffer[a_inner_data..][0..inner_bytes], buffer[b_inner_data..][0..inner_bytes])) {
            results[0] = .{ .I32 = 0 };
            return;
        }
    }
    results[0] = .{ .I32 = 1 };
}

var wasm_heap_ptr: u32 = 65536;

fn allocExtraBytes(alignment: u32) u32 {
    const ptr_width: u32 = 8;
    return if (alignment > ptr_width) alignment else ptr_width;
}

fn allocWasmData(buffer: []u8, alignment: u32, length: usize) u32 {
    const align_val: u32 = if (alignment > 4) alignment else 4;
    const extra_bytes = allocExtraBytes(alignment);
    const alloc_ptr = (wasm_heap_ptr + align_val - 1) & ~(align_val - 1);
    const data_ptr = alloc_ptr + extra_bytes;
    wasm_heap_ptr = @intCast(data_ptr + length);
    std.mem.writeInt(u32, buffer[data_ptr - 8 ..][0..4], @intCast(length), .little);
    std.mem.writeInt(u32, buffer[data_ptr - 4 ..][0..4], 1, .little);
    return data_ptr;
}

fn hostRocAlloc(_: ?*anyopaque, module: *bytebox.ModuleInstance, params: [*]const bytebox.Val, _: [*]bytebox.Val) error{}!void {
    const buffer = module.store.getMemory(0).buffer();
    const args_ptr: u32 = @bitCast(params[0].I32);
    if (args_ptr + 12 > buffer.len) return;
    const alignment: u32 = @bitCast(buffer[args_ptr..][0..4].*);
    const length: u32 = @bitCast(buffer[args_ptr + 4 ..][0..4].*);
    const data_ptr = allocWasmData(buffer, alignment, length);
    const answer_bytes: [4]u8 = @bitCast(data_ptr);
    @memcpy(buffer[args_ptr + 8 ..][0..4], &answer_bytes);
}

fn hostRocDealloc(_: ?*anyopaque, _: *bytebox.ModuleInstance, _: [*]const bytebox.Val, _: [*]bytebox.Val) error{}!void {}

fn hostRocRealloc(_: ?*anyopaque, module: *bytebox.ModuleInstance, params: [*]const bytebox.Val, _: [*]bytebox.Val) error{}!void {
    const buffer = module.store.getMemory(0).buffer();
    const args_ptr: u32 = @bitCast(params[0].I32);
    if (args_ptr + 12 > buffer.len) return;
    const alignment: u32 = @bitCast(buffer[args_ptr..][0..4].*);
    const new_length: u32 = @bitCast(buffer[args_ptr + 4 ..][0..4].*);
    const old_data_ptr: u32 = @bitCast(buffer[args_ptr + 8 ..][0..4].*);
    const old_length: usize = if (old_data_ptr >= 8 and old_data_ptr <= buffer.len)
        std.mem.readInt(u32, buffer[old_data_ptr - 8 ..][0..4], .little)
    else
        0;
    const data_ptr = allocWasmData(buffer, alignment, new_length);
    const copy_len = @min(old_length, new_length);
    if (copy_len > 0 and old_data_ptr + copy_len <= buffer.len and data_ptr + copy_len <= buffer.len) {
        @memcpy(buffer[data_ptr..][0..copy_len], buffer[old_data_ptr..][0..copy_len]);
    }
    const answer_bytes: [4]u8 = @bitCast(data_ptr);
    @memcpy(buffer[args_ptr + 8 ..][0..4], &answer_bytes);
}

fn hostRocDbg(_: ?*anyopaque, module: *bytebox.ModuleInstance, params: [*]const bytebox.Val, _: [*]bytebox.Val) error{}!void {
    const buffer = module.store.getMemory(0).buffer();
    const args_ptr: u32 = @bitCast(params[0].I32);
    if (args_ptr + 8 > buffer.len) return;
    const msg_ptr: u32 = @bitCast(buffer[args_ptr..][0..4].*);
    const msg_len: u32 = @bitCast(buffer[args_ptr + 4 ..][0..4].*);
    if (msg_ptr + msg_len <= buffer.len) std.debug.print("[dbg] {s}\n", .{buffer[msg_ptr..][0..msg_len]});
}

fn hostRocExpectFailed(_: ?*anyopaque, module: *bytebox.ModuleInstance, params: [*]const bytebox.Val, _: [*]bytebox.Val) error{}!void {
    const buffer = module.store.getMemory(0).buffer();
    const args_ptr: u32 = @bitCast(params[0].I32);
    if (args_ptr + 8 > buffer.len) return;
    const msg_ptr: u32 = @bitCast(buffer[args_ptr..][0..4].*);
    const msg_len: u32 = @bitCast(buffer[args_ptr + 4 ..][0..4].*);
    if (msg_ptr + msg_len <= buffer.len) std.debug.print("Expect failed: {s}\n", .{buffer[msg_ptr..][0..msg_len]});
}

fn hostRocCrashed(_: ?*anyopaque, module: *bytebox.ModuleInstance, params: [*]const bytebox.Val, _: [*]bytebox.Val) error{}!void {
    const buffer = module.store.getMemory(0).buffer();
    const args_ptr: u32 = @bitCast(params[0].I32);
    if (args_ptr + 8 > buffer.len) return;
    const msg_ptr: u32 = @bitCast(buffer[args_ptr..][0..4].*);
    const msg_len: u32 = @bitCast(buffer[args_ptr + 4 ..][0..4].*);
    if (msg_ptr + msg_len <= buffer.len) std.debug.print("Roc crashed: {s}\n", .{buffer[msg_ptr..][0..msg_len]});
}

fn readWasmStr(buffer: []u8, str_ptr: usize) struct { data: [*]const u8, len: usize } {
    const bytes = buffer[str_ptr..][0..12];
    if ((bytes[11] & 0x80) != 0) {
        return .{ .data = bytes[0..11].ptr, .len = bytes[11] & 0x7F };
    } else {
        const data_ptr: usize = @intCast(std.mem.readInt(u32, bytes[0..4], .little));
        const len: usize = @intCast(std.mem.readInt(u32, bytes[4..8], .little));
        return .{ .data = buffer[data_ptr..].ptr, .len = len };
    }
}

fn writeWasmStr(buffer: []u8, result_ptr: usize, data: [*]const u8, len: usize) void {
    if (len < 12) {
        @memset(buffer[result_ptr..][0..12], 0);
        @memcpy(buffer[result_ptr..][0..len], data[0..len]);
        buffer[result_ptr + 11] = @intCast(len | 0x80);
    } else {
        const data_ptr = allocWasmData(buffer, 1, len);
        @memcpy(buffer[data_ptr..][0..len], data[0..len]);
        std.mem.writeInt(u32, buffer[result_ptr..][0..4], data_ptr, .little);
        std.mem.writeInt(u32, buffer[result_ptr + 4 ..][0..4], @intCast(len), .little);
        std.mem.writeInt(u32, buffer[result_ptr + 8 ..][0..4], @intCast(len), .little);
    }
}

fn writeWasmEmptyStr(buffer: []u8, result_ptr: usize) void {
    @memset(buffer[result_ptr..][0..12], 0);
    buffer[result_ptr + 11] = 0x80;
}

fn rocStrFromWasmSlice(data: [*]const u8, len: usize) builtins.str.RocStr {
    if (len < @sizeOf(builtins.str.RocStr)) {
        return builtins.str.RocStr.fromSliceSmall(data[0..len]);
    }

    return .{
        .bytes = @constCast(data),
        .length = len,
        .capacity_or_alloc_ptr = len,
    };
}

fn isWhitespace(c: u8) bool {
    return c == ' ' or c == '\t' or c == '\n' or c == '\r' or c == 0x0b or c == 0x0c;
}

fn hostStrTrim(_: ?*anyopaque, module: *bytebox.ModuleInstance, params: [*]const bytebox.Val, _: [*]bytebox.Val) error{}!void {
    const buffer = module.store.getMemory(0).buffer();
    const str = readWasmStr(buffer, @intCast(params[0].I32));
    const slice = str.data[0..str.len];
    var start: usize = 0;
    while (start < slice.len and isWhitespace(slice[start])) : (start += 1) {}
    var end: usize = slice.len;
    while (end > start and isWhitespace(slice[end - 1])) : (end -= 1) {}
    writeWasmStr(buffer, @intCast(params[1].I32), slice[start..].ptr, end - start);
}

fn hostStrTrimStart(_: ?*anyopaque, module: *bytebox.ModuleInstance, params: [*]const bytebox.Val, _: [*]bytebox.Val) error{}!void {
    const buffer = module.store.getMemory(0).buffer();
    const str = readWasmStr(buffer, @intCast(params[0].I32));
    const slice = str.data[0..str.len];
    var start: usize = 0;
    while (start < slice.len and isWhitespace(slice[start])) : (start += 1) {}
    writeWasmStr(buffer, @intCast(params[1].I32), slice[start..].ptr, slice.len - start);
}

fn hostStrTrimEnd(_: ?*anyopaque, module: *bytebox.ModuleInstance, params: [*]const bytebox.Val, _: [*]bytebox.Val) error{}!void {
    const buffer = module.store.getMemory(0).buffer();
    const str = readWasmStr(buffer, @intCast(params[0].I32));
    const slice = str.data[0..str.len];
    var end: usize = slice.len;
    while (end > 0 and isWhitespace(slice[end - 1])) : (end -= 1) {}
    writeWasmStr(buffer, @intCast(params[1].I32), slice[0..end].ptr, end);
}

fn hostStrWithAsciiLowercased(_: ?*anyopaque, module: *bytebox.ModuleInstance, params: [*]const bytebox.Val, _: [*]bytebox.Val) error{}!void {
    const buffer = module.store.getMemory(0).buffer();
    const str = readWasmStr(buffer, @intCast(params[0].I32));
    if (str.len == 0) {
        writeWasmEmptyStr(buffer, @intCast(params[1].I32));
        return;
    }
    const dest_start = wasm_heap_ptr;
    wasm_heap_ptr += @intCast(str.len);
    for (str.data[0..str.len], 0..) |c, i| {
        buffer[dest_start + i] = if (c >= 'A' and c <= 'Z') c + 32 else c;
    }
    writeWasmStr(buffer, @intCast(params[1].I32), buffer[dest_start..].ptr, str.len);
}

fn hostStrWithAsciiUppercased(_: ?*anyopaque, module: *bytebox.ModuleInstance, params: [*]const bytebox.Val, _: [*]bytebox.Val) error{}!void {
    const buffer = module.store.getMemory(0).buffer();
    const str = readWasmStr(buffer, @intCast(params[0].I32));
    if (str.len == 0) {
        writeWasmEmptyStr(buffer, @intCast(params[1].I32));
        return;
    }
    const dest_start = wasm_heap_ptr;
    wasm_heap_ptr += @intCast(str.len);
    for (str.data[0..str.len], 0..) |c, i| {
        buffer[dest_start + i] = if (c >= 'a' and c <= 'z') c - 32 else c;
    }
    writeWasmStr(buffer, @intCast(params[1].I32), buffer[dest_start..].ptr, str.len);
}

fn hostStrReleaseExcessCapacity(_: ?*anyopaque, module: *bytebox.ModuleInstance, params: [*]const bytebox.Val, _: [*]bytebox.Val) error{}!void {
    const buffer = module.store.getMemory(0).buffer();
    const str = readWasmStr(buffer, @intCast(params[0].I32));
    writeWasmStr(buffer, @intCast(params[1].I32), str.data, str.len);
}

fn hostStrWithPrefix(_: ?*anyopaque, module: *bytebox.ModuleInstance, params: [*]const bytebox.Val, _: [*]bytebox.Val) error{}!void {
    const buffer = module.store.getMemory(0).buffer();
    const str = readWasmStr(buffer, @intCast(params[0].I32));
    const prefix = readWasmStr(buffer, @intCast(params[1].I32));
    const total_len = prefix.len + str.len;
    if (total_len == 0) {
        writeWasmEmptyStr(buffer, @intCast(params[2].I32));
        return;
    }
    const dest_start = wasm_heap_ptr;
    wasm_heap_ptr += @intCast(total_len);
    @memcpy(buffer[dest_start..][0..prefix.len], prefix.data[0..prefix.len]);
    @memcpy(buffer[dest_start + prefix.len ..][0..str.len], str.data[0..str.len]);
    writeWasmStr(buffer, @intCast(params[2].I32), buffer[dest_start..].ptr, total_len);
}

fn hostStrDropPrefix(_: ?*anyopaque, module: *bytebox.ModuleInstance, params: [*]const bytebox.Val, _: [*]bytebox.Val) error{}!void {
    const buffer = module.store.getMemory(0).buffer();
    const str = readWasmStr(buffer, @intCast(params[0].I32));
    const prefix = readWasmStr(buffer, @intCast(params[1].I32));
    if (prefix.len <= str.len and std.mem.eql(u8, str.data[0..prefix.len], prefix.data[0..prefix.len])) {
        writeWasmStr(buffer, @intCast(params[2].I32), str.data + prefix.len, str.len - prefix.len);
    } else {
        writeWasmStr(buffer, @intCast(params[2].I32), str.data, str.len);
    }
}

fn hostStrDropSuffix(_: ?*anyopaque, module: *bytebox.ModuleInstance, params: [*]const bytebox.Val, _: [*]bytebox.Val) error{}!void {
    const buffer = module.store.getMemory(0).buffer();
    const str = readWasmStr(buffer, @intCast(params[0].I32));
    const suffix = readWasmStr(buffer, @intCast(params[1].I32));
    if (suffix.len <= str.len and std.mem.eql(u8, (str.data + str.len - suffix.len)[0..suffix.len], suffix.data[0..suffix.len])) {
        writeWasmStr(buffer, @intCast(params[2].I32), str.data, str.len - suffix.len);
    } else {
        writeWasmStr(buffer, @intCast(params[2].I32), str.data, str.len);
    }
}

fn hostStrConcat(_: ?*anyopaque, module: *bytebox.ModuleInstance, params: [*]const bytebox.Val, _: [*]bytebox.Val) error{}!void {
    const buffer = module.store.getMemory(0).buffer();
    const lhs = readWasmStr(buffer, @intCast(params[0].I32));
    const rhs = readWasmStr(buffer, @intCast(params[1].I32));
    const total_len = lhs.len + rhs.len;
    if (total_len == 0) {
        writeWasmEmptyStr(buffer, @intCast(params[2].I32));
        return;
    }
    const dest_start = wasm_heap_ptr;
    wasm_heap_ptr += @intCast(total_len);
    if (lhs.len > 0) {
        @memcpy(buffer[dest_start..][0..lhs.len], lhs.data[0..lhs.len]);
    }
    if (rhs.len > 0) {
        @memcpy(buffer[dest_start + lhs.len ..][0..rhs.len], rhs.data[0..rhs.len]);
    }
    writeWasmStr(buffer, @intCast(params[2].I32), buffer[dest_start..].ptr, total_len);
}

fn hostStrRepeat(_: ?*anyopaque, module: *bytebox.ModuleInstance, params: [*]const bytebox.Val, _: [*]bytebox.Val) error{}!void {
    const buffer = module.store.getMemory(0).buffer();
    const str = readWasmStr(buffer, @intCast(params[0].I32));
    const count: usize = @intCast(@as(u32, @bitCast(params[1].I32)));
    if (count == 0 or str.len == 0) {
        writeWasmEmptyStr(buffer, @intCast(params[2].I32));
        return;
    }
    const total_len = str.len * count;
    const dest_start = wasm_heap_ptr;
    wasm_heap_ptr += @intCast(total_len);
    var offset: usize = 0;
    for (0..count) |_| {
        @memcpy(buffer[dest_start + offset ..][0..str.len], str.data[0..str.len]);
        offset += str.len;
    }
    writeWasmStr(buffer, @intCast(params[2].I32), buffer[dest_start..].ptr, total_len);
}

fn hostStrReserve(_: ?*anyopaque, module: *bytebox.ModuleInstance, params: [*]const bytebox.Val, _: [*]bytebox.Val) error{}!void {
    const buffer = module.store.getMemory(0).buffer();
    const str = readWasmStr(buffer, @intCast(params[0].I32));
    const extra_cap: usize = @intCast(@as(u32, @bitCast(params[1].I32)));
    const result_ptr: usize = @intCast(params[2].I32);
    const needed = str.len + extra_cap;
    if (needed < 12) {
        writeWasmStr(buffer, result_ptr, str.data, str.len);
        return;
    }
    const dest_start = allocWasmData(buffer, 1, needed);
    @memcpy(buffer[dest_start..][0..str.len], str.data[0..str.len]);
    std.mem.writeInt(u32, buffer[result_ptr..][0..4], dest_start, .little);
    std.mem.writeInt(u32, buffer[result_ptr + 4 ..][0..4], @intCast(str.len), .little);
    std.mem.writeInt(u32, buffer[result_ptr + 8 ..][0..4], @intCast(needed), .little);
}

fn hostStrWithCapacity(_: ?*anyopaque, module: *bytebox.ModuleInstance, params: [*]const bytebox.Val, _: [*]bytebox.Val) error{}!void {
    const buffer = module.store.getMemory(0).buffer();
    const cap: usize = @intCast(@as(u32, @bitCast(params[0].I32)));
    const result_ptr: usize = @intCast(params[1].I32);
    if (cap < 12) {
        writeWasmEmptyStr(buffer, result_ptr);
        return;
    }
    const dest_start = allocWasmData(buffer, 1, cap);
    std.mem.writeInt(u32, buffer[result_ptr..][0..4], dest_start, .little);
    std.mem.writeInt(u32, buffer[result_ptr + 4 ..][0..4], 0, .little);
    std.mem.writeInt(u32, buffer[result_ptr + 8 ..][0..4], @intCast(cap), .little);
}

fn hostStrCaselessAsciiEquals(_: ?*anyopaque, module: *bytebox.ModuleInstance, params: [*]const bytebox.Val, results: [*]bytebox.Val) error{}!void {
    const buffer = module.store.getMemory(0).buffer();
    const a = readWasmStr(buffer, @intCast(params[0].I32));
    const b = readWasmStr(buffer, @intCast(params[1].I32));
    if (a.len != b.len) {
        results[0] = .{ .I32 = 0 };
        return;
    }
    for (0..a.len) |i| {
        const ac = if (a.data[i] >= 'A' and a.data[i] <= 'Z') a.data[i] + 32 else a.data[i];
        const bc = if (b.data[i] >= 'A' and b.data[i] <= 'Z') b.data[i] + 32 else b.data[i];
        if (ac != bc) {
            results[0] = .{ .I32 = 0 };
            return;
        }
    }
    results[0] = .{ .I32 = 1 };
}

fn hostStrSplit(_: ?*anyopaque, module: *bytebox.ModuleInstance, params: [*]const bytebox.Val, _: [*]bytebox.Val) error{}!void {
    const buffer = module.store.getMemory(0).buffer();
    const str = readWasmStr(buffer, @intCast(params[0].I32));
    const sep = readWasmStr(buffer, @intCast(params[1].I32));
    const result_ptr: usize = @intCast(params[2].I32);
    const str_slice = str.data[0..str.len];
    const sep_slice = sep.data[0..sep.len];
    var count: usize = 1;
    if (sep.len > 0 and str.len >= sep.len) {
        var i: usize = 0;
        while (i + sep.len <= str.len) {
            if (std.mem.eql(u8, str_slice[i..][0..sep.len], sep_slice)) {
                count += 1;
                i += sep.len;
            } else {
                i += 1;
            }
        }
    }
    const list_data_start = allocWasmData(buffer, 4, count * 12);
    var part_idx: usize = 0;
    var start: usize = 0;
    if (sep.len > 0) {
        var i: usize = 0;
        while (i + sep.len <= str.len) {
            if (std.mem.eql(u8, str_slice[i..][0..sep.len], sep_slice)) {
                writeWasmStr(buffer, list_data_start + part_idx * 12, str_slice[start..].ptr, i - start);
                part_idx += 1;
                start = i + sep.len;
                i = start;
            } else {
                i += 1;
            }
        }
    }
    writeWasmStr(buffer, list_data_start + part_idx * 12, str_slice[start..].ptr, str.len - start);
    std.mem.writeInt(u32, buffer[result_ptr..][0..4], @intCast(list_data_start), .little);
    std.mem.writeInt(u32, buffer[result_ptr + 4 ..][0..4], @intCast(count), .little);
    std.mem.writeInt(u32, buffer[result_ptr + 8 ..][0..4], @intCast(count), .little);
}

fn hostStrJoinWith(_: ?*anyopaque, module: *bytebox.ModuleInstance, params: [*]const bytebox.Val, _: [*]bytebox.Val) error{}!void {
    const buffer = module.store.getMemory(0).buffer();
    const list_ptr: usize = @intCast(params[0].I32);
    const sep = readWasmStr(buffer, @intCast(params[1].I32));
    const list_data: usize = @intCast(std.mem.readInt(u32, buffer[list_ptr..][0..4], .little));
    const list_len: usize = @intCast(std.mem.readInt(u32, buffer[list_ptr + 4 ..][0..4], .little));
    if (list_len == 0) {
        writeWasmEmptyStr(buffer, @intCast(params[2].I32));
        return;
    }
    var total_len: usize = 0;
    for (0..list_len) |i| total_len += readWasmStr(buffer, list_data + i * 12).len;
    total_len += sep.len * (list_len - 1);
    if (total_len == 0) {
        writeWasmEmptyStr(buffer, @intCast(params[2].I32));
        return;
    }
    const dest_start = wasm_heap_ptr;
    wasm_heap_ptr += @intCast(total_len);
    var offset: usize = 0;
    for (0..list_len) |i| {
        if (i > 0 and sep.len > 0) {
            @memcpy(buffer[dest_start + offset ..][0..sep.len], sep.data[0..sep.len]);
            offset += sep.len;
        }
        const elem = readWasmStr(buffer, list_data + i * 12);
        if (elem.len > 0) {
            @memcpy(buffer[dest_start + offset ..][0..elem.len], elem.data[0..elem.len]);
            offset += elem.len;
        }
    }
    writeWasmStr(buffer, @intCast(params[2].I32), buffer[dest_start..].ptr, total_len);
}

fn hostListSortWith(_: ?*anyopaque, module: *bytebox.ModuleInstance, params: [*]const bytebox.Val, _: [*]bytebox.Val) error{}!void {
    const buffer = module.store.getMemory(0).buffer();
    const list_ptr: usize = @intCast(params[0].I32);
    const cmp_fn_idx: u32 = @bitCast(params[1].I32);
    const elem_width: usize = @intCast(params[2].I32);
    const alignment: u32 = @bitCast(params[3].I32);
    const result_ptr: usize = @intCast(params[4].I32);

    const data_ptr: usize = @intCast(std.mem.readInt(u32, buffer[list_ptr..][0..4], .little));
    const len: usize = @intCast(std.mem.readInt(u32, buffer[list_ptr + 4 ..][0..4], .little));
    const cap: usize = @intCast(std.mem.readInt(u32, buffer[list_ptr + 8 ..][0..4], .little));

    if (len < 2 or elem_width == 0) {
        std.mem.writeInt(u32, buffer[result_ptr..][0..4], @intCast(data_ptr), .little);
        std.mem.writeInt(u32, buffer[result_ptr + 4 ..][0..4], @intCast(len), .little);
        std.mem.writeInt(u32, buffer[result_ptr + 8 ..][0..4], @intCast(cap), .little);
        return;
    }

    const sorted_data = allocWasmData(buffer, alignment, len * elem_width);
    @memcpy(buffer[sorted_data..][0 .. len * elem_width], buffer[data_ptr..][0 .. len * elem_width]);

    const temp_ptr = allocWasmData(buffer, alignment, elem_width);
    const cmp_handle = bytebox.FunctionHandle{ .index = cmp_fn_idx };
    var cmp_params = [3]bytebox.Val{
        .{ .I32 = 0 },
        .{ .I32 = 0 },
        .{ .I32 = 0 },
    };
    var cmp_returns: [1]bytebox.Val = undefined;

    var i: usize = 1;
    while (i < len) : (i += 1) {
        const elem_i = sorted_data + i * elem_width;
        @memcpy(buffer[temp_ptr..][0..elem_width], buffer[elem_i..][0..elem_width]);

        var j = i;
        while (j > 0) {
            const prev_elem = sorted_data + (j - 1) * elem_width;
            cmp_params[1] = .{ .I32 = @intCast(temp_ptr) };
            cmp_params[2] = .{ .I32 = @intCast(prev_elem) };
            module.invoke(cmp_handle, &cmp_params, &cmp_returns, .{}) catch return;
            if (cmp_returns[0].I32 != 2) break;

            const dst_elem = sorted_data + j * elem_width;
            @memcpy(buffer[dst_elem..][0..elem_width], buffer[prev_elem..][0..elem_width]);
            j -= 1;
        }

        const insert_pos = sorted_data + j * elem_width;
        @memcpy(buffer[insert_pos..][0..elem_width], buffer[temp_ptr..][0..elem_width]);
    }

    std.mem.writeInt(u32, buffer[result_ptr..][0..4], @intCast(sorted_data), .little);
    std.mem.writeInt(u32, buffer[result_ptr + 4 ..][0..4], @intCast(len), .little);
    std.mem.writeInt(u32, buffer[result_ptr + 8 ..][0..4], @intCast(len), .little);
}

fn hostListAppendUnsafe(_: ?*anyopaque, module: *bytebox.ModuleInstance, params: [*]const bytebox.Val, _: [*]bytebox.Val) error{}!void {
    const buffer = module.store.getMemory(0).buffer();
    const list_ptr: usize = @intCast(params[0].I32);
    const elem_ptr: usize = @intCast(params[1].I32);
    const elem_width: usize = @intCast(params[2].I32);
    const alignment: u32 = @bitCast(params[3].I32);
    const result_ptr: usize = @intCast(params[4].I32);

    const data_ptr: usize = @intCast(std.mem.readInt(u32, buffer[list_ptr..][0..4], .little));
    const len: usize = @intCast(std.mem.readInt(u32, buffer[list_ptr + 4 ..][0..4], .little));
    const new_len = len + 1;

    if (elem_width == 0) {
        std.mem.writeInt(u32, buffer[result_ptr..][0..4], @intCast(data_ptr), .little);
        std.mem.writeInt(u32, buffer[result_ptr + 4 ..][0..4], @intCast(new_len), .little);
        std.mem.writeInt(u32, buffer[result_ptr + 8 ..][0..4], @intCast(new_len), .little);
        return;
    }

    const new_data = allocWasmData(buffer, alignment, new_len * elem_width);
    if (len > 0) {
        @memcpy(buffer[new_data..][0 .. len * elem_width], buffer[data_ptr..][0 .. len * elem_width]);
    }
    @memcpy(buffer[new_data + len * elem_width ..][0..elem_width], buffer[elem_ptr..][0..elem_width]);

    std.mem.writeInt(u32, buffer[result_ptr..][0..4], @intCast(new_data), .little);
    std.mem.writeInt(u32, buffer[result_ptr + 4 ..][0..4], @intCast(new_len), .little);
    std.mem.writeInt(u32, buffer[result_ptr + 8 ..][0..4], @intCast(new_len), .little);
}

fn hostListReverse(_: ?*anyopaque, module: *bytebox.ModuleInstance, params: [*]const bytebox.Val, _: [*]bytebox.Val) error{}!void {
    const buffer = module.store.getMemory(0).buffer();
    const list_ptr: usize = @intCast(params[0].I32);
    const elem_width: usize = @intCast(params[1].I32);
    const alignment: u32 = @bitCast(params[2].I32);
    const result_ptr: usize = @intCast(params[3].I32);

    const data_ptr: usize = @intCast(std.mem.readInt(u32, buffer[list_ptr..][0..4], .little));
    const len: usize = @intCast(std.mem.readInt(u32, buffer[list_ptr + 4 ..][0..4], .little));
    const cap: usize = @intCast(std.mem.readInt(u32, buffer[list_ptr + 8 ..][0..4], .little));

    if (len < 2 or elem_width == 0) {
        std.mem.writeInt(u32, buffer[result_ptr..][0..4], @intCast(data_ptr), .little);
        std.mem.writeInt(u32, buffer[result_ptr + 4 ..][0..4], @intCast(len), .little);
        std.mem.writeInt(u32, buffer[result_ptr + 8 ..][0..4], @intCast(cap), .little);
        return;
    }

    const reversed_data = allocWasmData(buffer, alignment, len * elem_width);
    for (0..len) |i| {
        const src_offset = (len - 1 - i) * elem_width;
        const dst_offset = i * elem_width;
        @memcpy(
            buffer[reversed_data + dst_offset ..][0..elem_width],
            buffer[data_ptr + src_offset ..][0..elem_width],
        );
    }

    std.mem.writeInt(u32, buffer[result_ptr..][0..4], @intCast(reversed_data), .little);
    std.mem.writeInt(u32, buffer[result_ptr + 4 ..][0..4], @intCast(len), .little);
    std.mem.writeInt(u32, buffer[result_ptr + 8 ..][0..4], @intCast(len), .little);
}

fn hostStrFromUtf8(_: ?*anyopaque, module: *bytebox.ModuleInstance, params: [*]const bytebox.Val, _: [*]bytebox.Val) error{}!void {
    const Utf8ByteProblem = builtins.str.Utf8ByteProblem;
    const buffer = module.store.getMemory(0).buffer();
    const list_ptr: usize = @intCast(params[0].I32);
    const result_ptr: usize = @intCast(params[1].I32);
    const result_size: usize = @intCast(params[2].I32);
    const disc_offset: usize = @intCast(params[3].I32);
    const data_ptr: usize = @intCast(std.mem.readInt(u32, buffer[list_ptr..][0..4], .little));
    const len: usize = @intCast(std.mem.readInt(u32, buffer[list_ptr + 4 ..][0..4], .little));
    const data = buffer[data_ptr..][0..len];
    @memset(buffer[result_ptr..][0..result_size], 0);
    if (std.unicode.utf8ValidateSlice(data)) {
        writeWasmStr(buffer, result_ptr, data.ptr, len);
        std.mem.writeInt(u32, buffer[result_ptr + disc_offset ..][0..4], 1, .little);
    } else {
        var index: usize = 0;
        while (index < data.len) {
            const next_num_bytes = builtins.str.numberOfNextCodepointBytes(data, index) catch |err| {
                const problem: Utf8ByteProblem = switch (err) {
                    error.UnexpectedEof => .UnexpectedEndOfSequence,
                    error.Utf8InvalidStartByte => .InvalidStartByte,
                    error.Utf8ExpectedContinuation => .ExpectedContinuation,
                    error.Utf8OverlongEncoding => .OverlongEncoding,
                    error.Utf8EncodesSurrogateHalf => .EncodesSurrogateHalf,
                    error.Utf8CodepointTooLarge => .CodepointTooLarge,
                };
                std.mem.writeInt(u64, buffer[result_ptr..][0..8], @intCast(index), .little);
                buffer[result_ptr + 8] = @intFromEnum(problem);
                break;
            };
            index += next_num_bytes;
        }
        std.mem.writeInt(u32, buffer[result_ptr + disc_offset ..][0..4], 0, .little);
    }
}

fn hostIntFromStr(_: ?*anyopaque, module: *bytebox.ModuleInstance, params: [*]const bytebox.Val, _: [*]bytebox.Val) error{}!void {
    const buffer = module.store.getMemory(0).buffer();
    const str = readWasmStr(buffer, @intCast(params[0].I32));
    const out_ptr: usize = @intCast(params[1].I32);
    const int_width: u8 = @intCast(params[2].I32);
    const is_signed = params[3].I32 != 0;
    const disc_offset: usize = @intCast(params[4].I32);
    const roc_str = rocStrFromWasmSlice(str.data, str.len);

    if (is_signed) {
        switch (int_width) {
            1 => writeIntParseResult(i8, buffer, out_ptr, disc_offset, roc_str),
            2 => writeIntParseResult(i16, buffer, out_ptr, disc_offset, roc_str),
            4 => writeIntParseResult(i32, buffer, out_ptr, disc_offset, roc_str),
            8 => writeIntParseResult(i64, buffer, out_ptr, disc_offset, roc_str),
            16 => writeIntParseResult(i128, buffer, out_ptr, disc_offset, roc_str),
            else => unreachable,
        }
    } else {
        switch (int_width) {
            1 => writeIntParseResult(u8, buffer, out_ptr, disc_offset, roc_str),
            2 => writeIntParseResult(u16, buffer, out_ptr, disc_offset, roc_str),
            4 => writeIntParseResult(u32, buffer, out_ptr, disc_offset, roc_str),
            8 => writeIntParseResult(u64, buffer, out_ptr, disc_offset, roc_str),
            16 => writeIntParseResult(u128, buffer, out_ptr, disc_offset, roc_str),
            else => unreachable,
        }
    }
}

fn hostDecFromStr(_: ?*anyopaque, module: *bytebox.ModuleInstance, params: [*]const bytebox.Val, _: [*]bytebox.Val) error{}!void {
    const buffer = module.store.getMemory(0).buffer();
    const str = readWasmStr(buffer, @intCast(params[0].I32));
    const out_ptr: usize = @intCast(params[1].I32);
    const disc_offset: usize = @intCast(params[2].I32);
    const roc_str = rocStrFromWasmSlice(str.data, str.len);
    const r = builtins.dec.fromStr(roc_str);
    const value_bytes = std.mem.asBytes(&r.value);
    @memcpy(buffer[out_ptr..][0..value_bytes.len], value_bytes);
    buffer[out_ptr + disc_offset] = 1 - r.errorcode;
}

fn hostFloatFromStr(_: ?*anyopaque, module: *bytebox.ModuleInstance, params: [*]const bytebox.Val, _: [*]bytebox.Val) error{}!void {
    const buffer = module.store.getMemory(0).buffer();
    const str = readWasmStr(buffer, @intCast(params[0].I32));
    const out_ptr: usize = @intCast(params[1].I32);
    const float_width: u8 = @intCast(params[2].I32);
    const disc_offset: usize = @intCast(params[3].I32);
    const roc_str = rocStrFromWasmSlice(str.data, str.len);

    switch (float_width) {
        4 => writeFloatParseResult(f32, buffer, out_ptr, disc_offset, roc_str),
        8 => writeFloatParseResult(f64, buffer, out_ptr, disc_offset, roc_str),
        else => unreachable,
    }
}

fn writeIntParseResult(comptime T: type, buffer: []u8, out_ptr: usize, disc_offset: usize, roc_str: builtins.str.RocStr) void {
    const r = builtins.num.parseIntFromStr(T, roc_str);
    const value_bytes = std.mem.asBytes(&r.value);
    @memcpy(buffer[out_ptr..][0..value_bytes.len], value_bytes);
    buffer[out_ptr + disc_offset] = 1 - r.errorcode;
}

fn writeFloatParseResult(comptime T: type, buffer: []u8, out_ptr: usize, disc_offset: usize, roc_str: builtins.str.RocStr) void {
    const r = builtins.num.parseFloatFromStr(T, roc_str);
    const value_bytes = std.mem.asBytes(&r.value);
    @memcpy(buffer[out_ptr..][0..value_bytes.len], value_bytes);
    buffer[out_ptr + disc_offset] = 1 - r.errorcode;
}
