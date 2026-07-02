//! WebAssembly execution runner for eval and REPL.
//!
//! Provides host-function bindings and memory management for running
//! Roc expressions compiled to WebAssembly via the Bytebox runtime.

const std = @import("std");
const builtin = @import("builtin");
const builtins = @import("builtins");
const bytebox = @import("bytebox");
const collections = @import("collections");
const i128h = builtins.compiler_rt_128;
const is_freestanding = builtin.target.os.tag == .freestanding;

/// Errors that can occur during WebAssembly evaluation.
pub const WasmEvalError = error{
    Crash,
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
    const UInt = std.meta.Int(.unsigned, @bitSizeOf(T));
    var result: UInt = 0;
    var i: usize = 0;
    while (i < @sizeOf(T)) : (i += 1) {
        result |= @as(UInt, buffer[offset + i]) << @intCast(i * 8);
    }
    return @bitCast(result);
}

fn writeIntLittle(comptime T: type, buffer: []u8, offset: usize, value: T) void {
    const UInt = std.meta.Int(.unsigned, @bitSizeOf(T));
    var remaining: UInt = @bitCast(value);
    var i: usize = 0;
    while (i < @sizeOf(T)) : (i += 1) {
        buffer[offset + i] = @intCast(remaining & 0xff);
        remaining >>= 8;
    }
}

fn bytesEqual(a: []const u8, b: []const u8) bool {
    if (a.len != b.len) return false;
    var i: usize = 0;
    while (i < a.len) : (i += 1) {
        if (a[i] != b[i]) return false;
    }
    return true;
}

fn bytesCaselessAsciiEqual(a: []const u8, b: []const u8) bool {
    if (a.len != b.len) return false;
    var i: usize = 0;
    while (i < a.len) : (i += 1) {
        if (a[i] == b[i]) continue;
        const diff = a[i] ^ b[i];
        if (diff != 0x20) return false;
        const lower = a[i] | 0x20;
        if (lower < 'a' or lower > 'z') return false;
    }
    return true;
}

fn bytesIndexOf(haystack: []const u8, needle: []const u8) ?usize {
    if (needle.len == 0) return 0;
    if (needle.len > haystack.len) return null;

    var i: usize = 0;
    while (i + needle.len <= haystack.len) : (i += 1) {
        if (bytesEqual(haystack[i..][0..needle.len], needle)) return i;
    }

    return null;
}

const WasmStr = struct {
    data: [*]const u8,
    data_offset: usize,
    len: usize,
    cap_or_alloc: u32,
    is_small: bool,
};

/// Captures a wasm eval run's string output and host-observed allocation count.
pub const RunWasmStrResult = struct {
    output: []u8,
    allocation_count: u32,
};

/// Executes a wasm module and returns the Str.inspect result as a string.
pub fn runWasmStr(
    allocator: std.mem.Allocator,
    wasm_bytes: []const u8,
    has_imports: bool,
) WasmEvalError![]u8 {
    const result = try runWasmStrWithStats(allocator, wasm_bytes, has_imports);
    return result.output;
}

/// Executes a wasm module and returns the raw Str result plus host-observed allocation stats.
pub fn runWasmStrWithStats(
    allocator: std.mem.Allocator,
    wasm_bytes: []const u8,
    has_imports: bool,
) WasmEvalError!RunWasmStrResult {
    wasm_heap_ptr = 65536;
    wasm_allocation_count = 0;
    wasm_crash_state = .none;

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
        var env_imports = bytebox.ModuleImportPackage.init("env", null, null, allocator) catch return error.WasmExecFailed;
        defer env_imports.deinit();

        env_imports.addHostFunction("roc_dec_mul", &[_]bytebox.ValType{ .I32, .I32, .I32 }, &[_]bytebox.ValType{}, hostDecMul, null) catch return error.WasmExecFailed;
        env_imports.addHostFunction("roc_dec_to_str", &[_]bytebox.ValType{ .I32, .I32 }, &[_]bytebox.ValType{.I32}, hostDecToStr, null) catch return error.WasmExecFailed;
        env_imports.addHostFunction("roc_str_eq", &[_]bytebox.ValType{ .I32, .I32 }, &[_]bytebox.ValType{.I32}, hostStrEq, null) catch return error.WasmExecFailed;
        env_imports.addHostFunction("roc_list_eq", &[_]bytebox.ValType{ .I32, .I32, .I32 }, &[_]bytebox.ValType{.I32}, hostListEq, null) catch return error.WasmExecFailed;

        env_imports.addHostFunction("roc_dict_pseudo_seed", &[_]bytebox.ValType{}, &[_]bytebox.ValType{.I64}, hostDictPseudoSeed, null) catch return error.WasmExecFailed;
        env_imports.addHostFunction("roc_hasher_finish", &[_]bytebox.ValType{.I64}, &[_]bytebox.ValType{.I64}, hostHasherFinish, null) catch return error.WasmExecFailed;
        env_imports.addHostFunction("roc_hasher_write_u64", &[_]bytebox.ValType{ .I64, .I32, .I64, .I32 }, &[_]bytebox.ValType{.I64}, hostHasherWriteU64, null) catch return error.WasmExecFailed;
        env_imports.addHostFunction("roc_hasher_write_u128", &[_]bytebox.ValType{ .I64, .I32, .I64, .I64 }, &[_]bytebox.ValType{.I64}, hostHasherWriteU128, null) catch return error.WasmExecFailed;
        env_imports.addHostFunction("roc_hasher_write_f32_bits", &[_]bytebox.ValType{ .I64, .I64 }, &[_]bytebox.ValType{.I64}, hostHasherWriteF32Bits, null) catch return error.WasmExecFailed;
        env_imports.addHostFunction("roc_hasher_write_f64_bits", &[_]bytebox.ValType{ .I64, .I64 }, &[_]bytebox.ValType{.I64}, hostHasherWriteF64Bits, null) catch return error.WasmExecFailed;
        env_imports.addHostFunction("roc_hasher_write_bytes", &[_]bytebox.ValType{ .I64, .I32, .I32, .I32 }, &[_]bytebox.ValType{.I64}, hostHasherWriteBytes, null) catch return error.WasmExecFailed;
        env_imports.addHostFunction("roc_hasher_write_str", &[_]bytebox.ValType{ .I64, .I32, .I32, .I32 }, &[_]bytebox.ValType{.I64}, hostHasherWriteStr, null) catch return error.WasmExecFailed;
        env_imports.addHostFunction("roc_crypto_sha256_hash_bytes", &[_]bytebox.ValType{ .I32, .I32, .I32, .I32, .I32 }, &[_]bytebox.ValType{}, hostCryptoSha256HashBytes, null) catch return error.WasmExecFailed;
        env_imports.addHostFunction("roc_crypto_sha256_hasher_empty", &[_]bytebox.ValType{ .I32, .I32 }, &[_]bytebox.ValType{}, hostCryptoSha256HasherEmpty, null) catch return error.WasmExecFailed;
        env_imports.addHostFunction("roc_crypto_sha256_hasher_write", &[_]bytebox.ValType{ .I32, .I32, .I32, .I32, .I32, .I32, .I32, .I32 }, &[_]bytebox.ValType{}, hostCryptoSha256HasherWrite, null) catch return error.WasmExecFailed;
        env_imports.addHostFunction("roc_crypto_sha256_hasher_finish", &[_]bytebox.ValType{ .I32, .I32, .I32, .I32, .I32 }, &[_]bytebox.ValType{}, hostCryptoSha256HasherFinish, null) catch return error.WasmExecFailed;
        env_imports.addHostFunction("roc_crypto_blake3_hash_bytes", &[_]bytebox.ValType{ .I32, .I32, .I32, .I32, .I32 }, &[_]bytebox.ValType{}, hostCryptoBlake3HashBytes, null) catch return error.WasmExecFailed;
        env_imports.addHostFunction("roc_crypto_blake3_hasher_empty", &[_]bytebox.ValType{ .I32, .I32 }, &[_]bytebox.ValType{}, hostCryptoBlake3HasherEmpty, null) catch return error.WasmExecFailed;
        env_imports.addHostFunction("roc_crypto_blake3_hasher_write", &[_]bytebox.ValType{ .I32, .I32, .I32, .I32, .I32, .I32, .I32, .I32 }, &[_]bytebox.ValType{}, hostCryptoBlake3HasherWrite, null) catch return error.WasmExecFailed;
        env_imports.addHostFunction("roc_crypto_blake3_hasher_finish", &[_]bytebox.ValType{ .I32, .I32, .I32, .I32, .I32 }, &[_]bytebox.ValType{}, hostCryptoBlake3HasherFinish, null) catch return error.WasmExecFailed;

        // Compiler-rt intrinsics needed by merged builtins
        env_imports.addHostFunction("__multi3", &[_]bytebox.ValType{ .I32, .I64, .I64, .I64, .I64 }, &[_]bytebox.ValType{}, hostMulti3, null) catch return error.WasmExecFailed;
        env_imports.addHostFunction("__muloti4", &[_]bytebox.ValType{ .I32, .I64, .I64, .I64, .I64, .I32 }, &[_]bytebox.ValType{}, hostMuloti4, null) catch return error.WasmExecFailed;

        env_imports.addHostFunction("roc_alloc", &[_]bytebox.ValType{ .I32, .I32, .I32 }, &[_]bytebox.ValType{.I32}, hostRocAlloc, null) catch return error.WasmExecFailed;
        env_imports.addHostFunction("roc_dealloc", &[_]bytebox.ValType{ .I32, .I32, .I32 }, &[_]bytebox.ValType{}, hostRocDealloc, null) catch return error.WasmExecFailed;
        env_imports.addHostFunction("roc_realloc", &[_]bytebox.ValType{ .I32, .I32, .I32, .I32 }, &[_]bytebox.ValType{.I32}, hostRocRealloc, null) catch return error.WasmExecFailed;
        env_imports.addHostFunction("roc_dbg", &[_]bytebox.ValType{ .I32, .I32, .I32 }, &[_]bytebox.ValType{}, hostRocDbg, null) catch return error.WasmExecFailed;
        env_imports.addHostFunction("roc_expect_failed", &[_]bytebox.ValType{ .I32, .I32, .I32 }, &[_]bytebox.ValType{}, hostRocExpectFailed, null) catch return error.WasmExecFailed;
        env_imports.addHostFunction("roc_crashed", &[_]bytebox.ValType{ .I32, .I32, .I32 }, &[_]bytebox.ValType{}, hostRocCrashed, null) catch return error.WasmExecFailed;

        env_imports.addHostFunction("roc_i128_div_s", &[_]bytebox.ValType{ .I32, .I32, .I32 }, &[_]bytebox.ValType{}, hostI128DivS, null) catch return error.WasmExecFailed;
        env_imports.addHostFunction("roc_i128_mod_s", &[_]bytebox.ValType{ .I32, .I32, .I32 }, &[_]bytebox.ValType{}, hostI128ModS, null) catch return error.WasmExecFailed;
        env_imports.addHostFunction("roc_u128_div", &[_]bytebox.ValType{ .I32, .I32, .I32 }, &[_]bytebox.ValType{}, hostU128Div, null) catch return error.WasmExecFailed;
        env_imports.addHostFunction("roc_u128_mod", &[_]bytebox.ValType{ .I32, .I32, .I32 }, &[_]bytebox.ValType{}, hostU128Mod, null) catch return error.WasmExecFailed;
        env_imports.addHostFunction("roc_dec_div", &[_]bytebox.ValType{ .I32, .I32, .I32 }, &[_]bytebox.ValType{}, hostDecDiv, null) catch return error.WasmExecFailed;
        env_imports.addHostFunction("roc_dec_div_trunc", &[_]bytebox.ValType{ .I32, .I32, .I32 }, &[_]bytebox.ValType{}, hostDecDivTrunc, null) catch return error.WasmExecFailed;
        env_imports.addHostFunction("roc_dec_pow", &[_]bytebox.ValType{ .I32, .I32, .I32 }, &[_]bytebox.ValType{}, hostDecPow, null) catch return error.WasmExecFailed;
        env_imports.addHostFunction("roc_dec_sqrt", &[_]bytebox.ValType{ .I32, .I32 }, &[_]bytebox.ValType{}, hostDecSqrt, null) catch return error.WasmExecFailed;
        env_imports.addHostFunction("roc_dec_sin", &[_]bytebox.ValType{ .I32, .I32 }, &[_]bytebox.ValType{}, hostDecSin, null) catch return error.WasmExecFailed;
        env_imports.addHostFunction("roc_dec_cos", &[_]bytebox.ValType{ .I32, .I32 }, &[_]bytebox.ValType{}, hostDecCos, null) catch return error.WasmExecFailed;
        env_imports.addHostFunction("roc_dec_tan", &[_]bytebox.ValType{ .I32, .I32 }, &[_]bytebox.ValType{}, hostDecTan, null) catch return error.WasmExecFailed;
        env_imports.addHostFunction("roc_dec_asin", &[_]bytebox.ValType{ .I32, .I32 }, &[_]bytebox.ValType{}, hostDecAsin, null) catch return error.WasmExecFailed;
        env_imports.addHostFunction("roc_dec_acos", &[_]bytebox.ValType{ .I32, .I32 }, &[_]bytebox.ValType{}, hostDecAcos, null) catch return error.WasmExecFailed;
        env_imports.addHostFunction("roc_dec_atan", &[_]bytebox.ValType{ .I32, .I32 }, &[_]bytebox.ValType{}, hostDecAtan, null) catch return error.WasmExecFailed;
        env_imports.addHostFunction("roc_i8_mod_by", &[_]bytebox.ValType{ .I32, .I32 }, &[_]bytebox.ValType{.I32}, hostI8ModBy, null) catch return error.WasmExecFailed;
        env_imports.addHostFunction("roc_u8_mod_by", &[_]bytebox.ValType{ .I32, .I32 }, &[_]bytebox.ValType{.I32}, hostU8ModBy, null) catch return error.WasmExecFailed;
        env_imports.addHostFunction("roc_i16_mod_by", &[_]bytebox.ValType{ .I32, .I32 }, &[_]bytebox.ValType{.I32}, hostI16ModBy, null) catch return error.WasmExecFailed;
        env_imports.addHostFunction("roc_u16_mod_by", &[_]bytebox.ValType{ .I32, .I32 }, &[_]bytebox.ValType{.I32}, hostU16ModBy, null) catch return error.WasmExecFailed;
        env_imports.addHostFunction("roc_i32_mod_by", &[_]bytebox.ValType{ .I32, .I32 }, &[_]bytebox.ValType{.I32}, hostI32ModBy, null) catch return error.WasmExecFailed;
        env_imports.addHostFunction("roc_u32_mod_by", &[_]bytebox.ValType{ .I32, .I32 }, &[_]bytebox.ValType{.I32}, hostU32ModBy, null) catch return error.WasmExecFailed;
        env_imports.addHostFunction("roc_i64_mod_by", &[_]bytebox.ValType{ .I64, .I64 }, &[_]bytebox.ValType{.I64}, hostI64ModBy, null) catch return error.WasmExecFailed;
        env_imports.addHostFunction("roc_u64_mod_by", &[_]bytebox.ValType{ .I64, .I64 }, &[_]bytebox.ValType{.I64}, hostU64ModBy, null) catch return error.WasmExecFailed;
        env_imports.addHostFunction("roc_i128_to_str", &[_]bytebox.ValType{ .I32, .I32 }, &[_]bytebox.ValType{.I32}, hostI128ToStr, null) catch return error.WasmExecFailed;
        env_imports.addHostFunction("roc_u128_to_str", &[_]bytebox.ValType{ .I32, .I32 }, &[_]bytebox.ValType{.I32}, hostU128ToStr, null) catch return error.WasmExecFailed;
        env_imports.addHostFunction("roc_float_to_str", &[_]bytebox.ValType{ .I64, .I32, .I32 }, &[_]bytebox.ValType{.I32}, hostFloatToStr, null) catch return error.WasmExecFailed;
        env_imports.addHostFunction("roc_float_pow", &[_]bytebox.ValType{ .F64, .F64, .I32 }, &[_]bytebox.ValType{.F64}, hostFloatPow, null) catch return error.WasmExecFailed;
        env_imports.addHostFunction("roc_float_sin", &[_]bytebox.ValType{ .F64, .I32 }, &[_]bytebox.ValType{.F64}, hostFloatSin, null) catch return error.WasmExecFailed;
        env_imports.addHostFunction("roc_float_cos", &[_]bytebox.ValType{ .F64, .I32 }, &[_]bytebox.ValType{.F64}, hostFloatCos, null) catch return error.WasmExecFailed;
        env_imports.addHostFunction("roc_float_tan", &[_]bytebox.ValType{ .F64, .I32 }, &[_]bytebox.ValType{.F64}, hostFloatTan, null) catch return error.WasmExecFailed;
        env_imports.addHostFunction("roc_float_asin", &[_]bytebox.ValType{ .F64, .I32 }, &[_]bytebox.ValType{.F64}, hostFloatAsin, null) catch return error.WasmExecFailed;
        env_imports.addHostFunction("roc_float_acos", &[_]bytebox.ValType{ .F64, .I32 }, &[_]bytebox.ValType{.F64}, hostFloatAcos, null) catch return error.WasmExecFailed;
        env_imports.addHostFunction("roc_float_atan", &[_]bytebox.ValType{ .F64, .I32 }, &[_]bytebox.ValType{.F64}, hostFloatAtan, null) catch return error.WasmExecFailed;
        env_imports.addHostFunction("roc_int_to_str", &[_]bytebox.ValType{ .I64, .I64, .I32, .I32, .I32 }, &[_]bytebox.ValType{.I32}, hostIntToStr, null) catch return error.WasmExecFailed;
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
            .{ "roc_str_escape_and_quote", hostStrEscapeAndQuote },
        }) |entry| {
            env_imports.addHostFunction(entry[0], &[_]bytebox.ValType{ .I32, .I32 }, &[_]bytebox.ValType{}, entry[1], null) catch return error.WasmExecFailed;
        }
        env_imports.addHostFunction(
            "roc_str_from_utf8",
            &[_]bytebox.ValType{ .I32, .I32, .I32, .I32, .I32, .I32, .I32, .I32, .I32, .I32, .I32, .I32, .I32, .I32 },
            &[_]bytebox.ValType{},
            hostStrFromUtf8,
            null,
        ) catch return error.WasmExecFailed;

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
        env_imports.addHostFunction("roc_str_find_first", &[_]bytebox.ValType{ .I32, .I32, .I32, .I32, .I32, .I32 }, &[_]bytebox.ValType{}, hostStrFindFirst, null) catch return error.WasmExecFailed;
        env_imports.addHostFunction("roc_str_drop_prefix_caseless_ascii", &[_]bytebox.ValType{ .I32, .I32, .I32, .I32, .I32 }, &[_]bytebox.ValType{}, hostStrDropPrefixCaselessAscii, null) catch return error.WasmExecFailed;

        env_imports.addHostFunction("roc_str_caseless_ascii_equals", &[_]bytebox.ValType{ .I32, .I32 }, &[_]bytebox.ValType{.I32}, hostStrCaselessAsciiEquals, null) catch return error.WasmExecFailed;
        env_imports.addHostFunction("roc_int_from_str", &[_]bytebox.ValType{ .I32, .I32, .I32, .I32, .I32 }, &[_]bytebox.ValType{}, hostIntFromStr, null) catch return error.WasmExecFailed;
        env_imports.addHostFunction("roc_dec_from_str", &[_]bytebox.ValType{ .I32, .I32, .I32 }, &[_]bytebox.ValType{}, hostDecFromStr, null) catch return error.WasmExecFailed;
        env_imports.addHostFunction("roc_float_from_str", &[_]bytebox.ValType{ .I32, .I32, .I32, .I32 }, &[_]bytebox.ValType{}, hostFloatFromStr, null) catch return error.WasmExecFailed;
        env_imports.addHostFunction("roc_list_append_unsafe", &[_]bytebox.ValType{ .I32, .I32, .I32, .I32, .I32 }, &[_]bytebox.ValType{}, hostListAppendUnsafe, null) catch return error.WasmExecFailed;
        env_imports.addHostFunction("roc_list_concat", &[_]bytebox.ValType{ .I32, .I32, .I32, .I32, .I32 }, &[_]bytebox.ValType{}, hostListConcat, null) catch return error.WasmExecFailed;
        env_imports.addHostFunction("roc_list_drop_at", &[_]bytebox.ValType{ .I32, .I32, .I32, .I32, .I32 }, &[_]bytebox.ValType{}, hostListDropAt, null) catch return error.WasmExecFailed;
        env_imports.addHostFunction("roc_list_reserve", &[_]bytebox.ValType{ .I32, .I64, .I32, .I32, .I32 }, &[_]bytebox.ValType{}, hostListReserve, null) catch return error.WasmExecFailed;
        env_imports.addHostFunction("roc_list_reverse", &[_]bytebox.ValType{ .I32, .I32, .I32, .I32 }, &[_]bytebox.ValType{}, hostListReverse, null) catch return error.WasmExecFailed;
        env_imports.addHostFunction("roc_list_replace", &[_]bytebox.ValType{ .I32, .I32, .I32, .I64, .I32, .I32, .I32 }, &[_]bytebox.ValType{}, hostListReplace, null) catch return error.WasmExecFailed;
        env_imports.addHostFunction("roc_list_swap", &[_]bytebox.ValType{ .I32, .I32, .I32, .I64, .I64, .I32 }, &[_]bytebox.ValType{}, hostListSwap, null) catch return error.WasmExecFailed;

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
    var params = [1]bytebox.Val{.{ .I32 = 0 }};
    var returns: [1]bytebox.Val = undefined;
    module_instance.invoke(handle, &params, &returns, .{}) catch |err| {
        if (wasm_crash_state == .crashed) {
            return error.Crash;
        }
        if (std.debug.runtime_safety) {
            debugPrint("wasm invoke failed: {s}\n", .{@errorName(err)});
        }
        switch (err) {
            error.TrapUnreachable => {
                std.debug.assert(false);
                unreachable;
            },
            else => return error.WasmExecFailed,
        }
    };
    if (wasm_crash_state == .crashed) return error.Crash;

    const str_ptr: u32 = @bitCast(returns[0].I32);
    const mem_slice = module_instance.memoryAll();
    if (str_ptr + 12 > mem_slice.len) {
        if (std.debug.runtime_safety) {
            debugPrint("wasm invalid str ptr: ptr={d} mem_len={d}\n", .{ str_ptr, mem_slice.len });
        }
        return error.WasmExecFailed;
    }

    const byte11 = mem_slice[str_ptr + 11];
    const str_data: []const u8 = if (byte11 & 0x80 != 0) sd: {
        const sso_len: u32 = byte11 & 0x7F;
        if (sso_len > 11) {
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

    return .{
        .output = try allocator.dupe(u8, str_data),
        .allocation_count = wasm_allocation_count,
    };
}

fn hostDecMul(_: ?*anyopaque, module: *bytebox.ModuleInstance, params: [*]const bytebox.Val, _: [*]bytebox.Val) error{}!void {
    const RocDec = builtins.dec.RocDec;
    const buffer = module.store.getMemory(0).buffer();
    const lhs_ptr: usize = @intCast(params[0].I32);
    const rhs_ptr: usize = @intCast(params[1].I32);
    const result_ptr: usize = @intCast(params[2].I32);
    if (lhs_ptr + 16 > buffer.len or rhs_ptr + 16 > buffer.len or result_ptr + 16 > buffer.len) return;
    const lhs_low: u64 = readIntLittle(u64, buffer, lhs_ptr);
    const lhs_high: u64 = readIntLittle(u64, buffer, lhs_ptr + 8);
    const lhs_i128: i128 = @bitCast(@as(u128, lhs_high) << 64 | @as(u128, lhs_low));
    const rhs_low: u64 = readIntLittle(u64, buffer, rhs_ptr);
    const rhs_high: u64 = readIntLittle(u64, buffer, rhs_ptr + 8);
    const rhs_i128: i128 = @bitCast(@as(u128, rhs_high) << 64 | @as(u128, rhs_low));
    const lhs_dec = RocDec{ .num = lhs_i128 };
    const rhs_dec = RocDec{ .num = rhs_i128 };
    const result = lhs_dec.mulWithOverflow(rhs_dec);
    if (result.has_overflowed) {
        wasm_crash_state = .crashed;
        return;
    }
    const result_u128: u128 = @bitCast(result.value.num);
    writeIntLittle(u64, buffer, result_ptr, @truncate(result_u128));
    writeIntLittle(u64, buffer, result_ptr + 8, @truncate(result_u128 >> 64));
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
    const low: u64 = readIntLittle(u64, buffer, dec_ptr);
    const high: u64 = readIntLittle(u64, buffer, dec_ptr + 8);
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
    results[0] = .{ .I32 = if (a.len == b.len and bytesEqual(a.data[0..a.len], b.data[0..b.len])) 1 else 0 };
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
    const a_data_ptr: usize = @intCast(readIntLittle(u32, buffer, a_list_ptr));
    const a_len: usize = @intCast(readIntLittle(u32, buffer, a_list_ptr + 4));
    const b_data_ptr: usize = @intCast(readIntLittle(u32, buffer, b_list_ptr));
    const b_len: usize = @intCast(readIntLittle(u32, buffer, b_list_ptr + 4));
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
    results[0] = .{ .I32 = if (bytesEqual(buffer[a_data_ptr..][0..total_bytes], buffer[b_data_ptr..][0..total_bytes])) 1 else 0 };
}

// Hasher host functions. The Hasher is a u64 seed; each write threads a new
// seed through. These delegate to the same `builtins.hash` routines used by the
// native and dev backends so all backends agree on hash values.

fn hostDictPseudoSeed(_: ?*anyopaque, _: *bytebox.ModuleInstance, _: [*]const bytebox.Val, results: [*]bytebox.Val) error{}!void {
    results[0] = .{ .I64 = @bitCast(builtins.utils.dictPseudoSeed()) };
}

fn hostHasherFinish(_: ?*anyopaque, _: *bytebox.ModuleInstance, params: [*]const bytebox.Val, results: [*]bytebox.Val) error{}!void {
    const seed: u64 = @bitCast(params[0].I64);
    results[0] = .{ .I64 = @bitCast(builtins.hash.hasher_finish(seed)) };
}

fn hostHasherWriteU64(_: ?*anyopaque, _: *bytebox.ModuleInstance, params: [*]const bytebox.Val, results: [*]bytebox.Val) error{}!void {
    const seed: u64 = @bitCast(params[0].I64);
    const domain: u8 = @intCast(@as(u32, @bitCast(params[1].I32)));
    const value: u64 = @bitCast(params[2].I64);
    const width: u8 = @intCast(@as(u32, @bitCast(params[3].I32)));
    results[0] = .{ .I64 = @bitCast(builtins.hash.hasher_write_u64(seed, domain, value, width)) };
}

fn hostHasherWriteU128(_: ?*anyopaque, _: *bytebox.ModuleInstance, params: [*]const bytebox.Val, results: [*]bytebox.Val) error{}!void {
    const seed: u64 = @bitCast(params[0].I64);
    const domain: u8 = @intCast(@as(u32, @bitCast(params[1].I32)));
    const low: u64 = @bitCast(params[2].I64);
    const high: u64 = @bitCast(params[3].I64);
    results[0] = .{ .I64 = @bitCast(builtins.hash.hasher_write_u128(seed, domain, low, high)) };
}

fn hostHasherWriteF32Bits(_: ?*anyopaque, _: *bytebox.ModuleInstance, params: [*]const bytebox.Val, results: [*]bytebox.Val) error{}!void {
    const seed: u64 = @bitCast(params[0].I64);
    const bits: u64 = @bitCast(params[1].I64);
    results[0] = .{ .I64 = @bitCast(builtins.hash.hasher_write_f32_bits(seed, @truncate(bits))) };
}

fn hostHasherWriteF64Bits(_: ?*anyopaque, _: *bytebox.ModuleInstance, params: [*]const bytebox.Val, results: [*]bytebox.Val) error{}!void {
    const seed: u64 = @bitCast(params[0].I64);
    const bits: u64 = @bitCast(params[1].I64);
    results[0] = .{ .I64 = @bitCast(builtins.hash.hasher_write_f64_bits(seed, bits)) };
}

fn hostHasherWriteBytes(_: ?*anyopaque, module: *bytebox.ModuleInstance, params: [*]const bytebox.Val, results: [*]bytebox.Val) error{}!void {
    const buffer = module.store.getMemory(0).buffer();
    const seed: u64 = @bitCast(params[0].I64);
    const domain: u8 = @intCast(@as(u32, @bitCast(params[1].I32)));
    const ptr: usize = @intCast(params[2].I32);
    const len: usize = @intCast(params[3].I32);
    const bytes = if (len == 0) buffer[0..0] else buffer[ptr..][0..len];
    results[0] = .{ .I64 = @bitCast(builtins.hash.hasher_write_bytes(seed, domain, bytes.ptr, bytes.len)) };
}

fn hostHasherWriteStr(_: ?*anyopaque, module: *bytebox.ModuleInstance, params: [*]const bytebox.Val, results: [*]bytebox.Val) error{}!void {
    const buffer = module.store.getMemory(0).buffer();
    const seed: u64 = @bitCast(params[0].I64);
    const ptr: usize = @intCast(params[1].I32);
    const len: usize = @intCast(params[2].I32);
    const bytes = if (len == 0) buffer[0..0] else buffer[ptr..][0..len];
    const str_domain = @intFromEnum(builtins.hash.HasherDomain.str);
    results[0] = .{ .I64 = @bitCast(builtins.hash.hasher_write_bytes(seed, str_domain, bytes.ptr, bytes.len)) };
}

const CryptoAlgorithm = enum { sha256, blake3 };

fn wasmByteSlice(buffer: []u8, ptr: usize, len: usize) []const u8 {
    return if (len == 0) buffer[0..0] else buffer[ptr..][0..len];
}

fn nativeRocListBytes(roc_list: builtins.list.RocList) []const u8 {
    if (roc_list.bytes) |ptr| return ptr[0..roc_list.length];
    std.debug.assert(roc_list.length == 0);
    return &.{};
}

fn writeWasmList(buffer: []u8, result_ptr: usize, bytes: []const u8) void {
    if (bytes.len == 0) {
        writeIntLittle(u32, buffer, result_ptr, 0);
        writeIntLittle(u32, buffer, result_ptr + 4, 0);
        writeIntLittle(u32, buffer, result_ptr + 8, 0);
        return;
    }

    const data_ptr = allocWasmData(buffer, 1, bytes.len);
    @memcpy(buffer[data_ptr..][0..bytes.len], bytes);
    writeIntLittle(u32, buffer, result_ptr, data_ptr);
    writeIntLittle(u32, buffer, result_ptr + 4, @intCast(bytes.len));
    writeIntLittle(u32, buffer, result_ptr + 8, encodeWasmListCapacity(bytes.len));
}

fn writeNativeRocListToWasm(buffer: []u8, result_ptr: usize, roc_list: builtins.list.RocList) void {
    writeWasmList(buffer, result_ptr, nativeRocListBytes(roc_list));
}

fn hostCryptoHashBytes(module: *bytebox.ModuleInstance, params: [*]const bytebox.Val, comptime algorithm: CryptoAlgorithm) void {
    const buffer = module.store.getMemory(0).buffer();
    const result_ptr: usize = @intCast(params[0].I32);
    const input_ptr: usize = @intCast(params[1].I32);
    const input_len: usize = @intCast(params[2].I32);
    const input = wasmByteSlice(buffer, input_ptr, input_len);

    var env = builtins.utils.TestEnv.init(std.heap.page_allocator);
    defer env.deinit();
    const ops = env.getOps();
    const result = switch (algorithm) {
        .sha256 => builtins.crypto.sha256HashBytes(input.ptr, input.len, ops),
        .blake3 => builtins.crypto.blake3HashBytes(input.ptr, input.len, ops),
    };
    writeNativeRocListToWasm(buffer, result_ptr, result);
}

fn hostCryptoHasherEmpty(module: *bytebox.ModuleInstance, params: [*]const bytebox.Val, comptime algorithm: CryptoAlgorithm) void {
    const buffer = module.store.getMemory(0).buffer();
    const result_ptr: usize = @intCast(params[0].I32);

    var env = builtins.utils.TestEnv.init(std.heap.page_allocator);
    defer env.deinit();
    const ops = env.getOps();
    const result = switch (algorithm) {
        .sha256 => builtins.crypto.sha256HasherEmpty(ops),
        .blake3 => builtins.crypto.blake3HasherEmpty(ops),
    };
    writeNativeRocListToWasm(buffer, result_ptr, result);
}

fn hostCryptoHasherWrite(module: *bytebox.ModuleInstance, params: [*]const bytebox.Val, comptime algorithm: CryptoAlgorithm) void {
    const buffer = module.store.getMemory(0).buffer();
    const result_ptr: usize = @intCast(params[0].I32);
    const state_ptr: usize = @intCast(params[1].I32);
    const state_len: usize = @intCast(params[2].I32);
    const input_ptr: usize = @intCast(params[4].I32);
    const input_len: usize = @intCast(params[5].I32);
    const state = wasmByteSlice(buffer, state_ptr, state_len);
    const input = wasmByteSlice(buffer, input_ptr, input_len);

    var env = builtins.utils.TestEnv.init(std.heap.page_allocator);
    defer env.deinit();
    const ops = env.getOps();
    const result = switch (algorithm) {
        .sha256 => builtins.crypto.sha256HasherWrite(state.ptr, state.len, input.ptr, input.len, ops),
        .blake3 => builtins.crypto.blake3HasherWrite(state.ptr, state.len, input.ptr, input.len, ops),
    };
    writeNativeRocListToWasm(buffer, result_ptr, result);
}

fn hostCryptoHasherFinish(module: *bytebox.ModuleInstance, params: [*]const bytebox.Val, comptime algorithm: CryptoAlgorithm) void {
    const buffer = module.store.getMemory(0).buffer();
    const result_ptr: usize = @intCast(params[0].I32);
    const state_ptr: usize = @intCast(params[1].I32);
    const state_len: usize = @intCast(params[2].I32);
    const state = wasmByteSlice(buffer, state_ptr, state_len);

    var env = builtins.utils.TestEnv.init(std.heap.page_allocator);
    defer env.deinit();
    const ops = env.getOps();
    const result = switch (algorithm) {
        .sha256 => builtins.crypto.sha256HasherFinish(state.ptr, state.len, ops),
        .blake3 => builtins.crypto.blake3HasherFinish(state.ptr, state.len, ops),
    };
    writeNativeRocListToWasm(buffer, result_ptr, result);
}

fn hostCryptoSha256HashBytes(_: ?*anyopaque, module: *bytebox.ModuleInstance, params: [*]const bytebox.Val, _: [*]bytebox.Val) error{}!void {
    hostCryptoHashBytes(module, params, .sha256);
}

fn hostCryptoSha256HasherEmpty(_: ?*anyopaque, module: *bytebox.ModuleInstance, params: [*]const bytebox.Val, _: [*]bytebox.Val) error{}!void {
    hostCryptoHasherEmpty(module, params, .sha256);
}

fn hostCryptoSha256HasherWrite(_: ?*anyopaque, module: *bytebox.ModuleInstance, params: [*]const bytebox.Val, _: [*]bytebox.Val) error{}!void {
    hostCryptoHasherWrite(module, params, .sha256);
}

fn hostCryptoSha256HasherFinish(_: ?*anyopaque, module: *bytebox.ModuleInstance, params: [*]const bytebox.Val, _: [*]bytebox.Val) error{}!void {
    hostCryptoHasherFinish(module, params, .sha256);
}

fn hostCryptoBlake3HashBytes(_: ?*anyopaque, module: *bytebox.ModuleInstance, params: [*]const bytebox.Val, _: [*]bytebox.Val) error{}!void {
    hostCryptoHashBytes(module, params, .blake3);
}

fn hostCryptoBlake3HasherEmpty(_: ?*anyopaque, module: *bytebox.ModuleInstance, params: [*]const bytebox.Val, _: [*]bytebox.Val) error{}!void {
    hostCryptoHasherEmpty(module, params, .blake3);
}

fn hostCryptoBlake3HasherWrite(_: ?*anyopaque, module: *bytebox.ModuleInstance, params: [*]const bytebox.Val, _: [*]bytebox.Val) error{}!void {
    hostCryptoHasherWrite(module, params, .blake3);
}

fn hostCryptoBlake3HasherFinish(_: ?*anyopaque, module: *bytebox.ModuleInstance, params: [*]const bytebox.Val, _: [*]bytebox.Val) error{}!void {
    hostCryptoHasherFinish(module, params, .blake3);
}

fn readI128FromMem(buffer: []u8, ptr: usize) i128 {
    const low = readIntLittle(u64, buffer, ptr);
    const high = readIntLittle(i64, buffer, ptr + 8);
    return @as(i128, high) << 64 | low;
}

fn readU128FromMem(buffer: []u8, ptr: usize) u128 {
    const low = readIntLittle(u64, buffer, ptr);
    const high = readIntLittle(u64, buffer, ptr + 8);
    return @as(u128, high) << 64 | low;
}

fn writeI128ToMem(buffer: []u8, ptr: usize, val: i128) void {
    const as_u128: u128 = @bitCast(val);
    writeIntLittle(u64, buffer, ptr, @truncate(as_u128));
    writeIntLittle(u64, buffer, ptr + 8, @truncate(as_u128 >> 64));
}

fn writeU128ToMem(buffer: []u8, ptr: usize, val: u128) void {
    writeIntLittle(u64, buffer, ptr, @truncate(val));
    writeIntLittle(u64, buffer, ptr + 8, @truncate(val >> 64));
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

fn hostI8ModBy(_: ?*anyopaque, _: *bytebox.ModuleInstance, params: [*]const bytebox.Val, results: [*]bytebox.Val) error{}!void {
    const lhs: i8 = @intCast(params[0].I32);
    const rhs: i8 = @intCast(params[1].I32);
    results[0] = .{ .I32 = @intCast(@mod(lhs, rhs)) };
}

fn hostU8ModBy(_: ?*anyopaque, _: *bytebox.ModuleInstance, params: [*]const bytebox.Val, results: [*]bytebox.Val) error{}!void {
    const lhs: u8 = @intCast(params[0].I32);
    const rhs: u8 = @intCast(params[1].I32);
    results[0] = .{ .I32 = @intCast(@mod(lhs, rhs)) };
}

fn hostI16ModBy(_: ?*anyopaque, _: *bytebox.ModuleInstance, params: [*]const bytebox.Val, results: [*]bytebox.Val) error{}!void {
    const lhs: i16 = @intCast(params[0].I32);
    const rhs: i16 = @intCast(params[1].I32);
    results[0] = .{ .I32 = @intCast(@mod(lhs, rhs)) };
}

fn hostU16ModBy(_: ?*anyopaque, _: *bytebox.ModuleInstance, params: [*]const bytebox.Val, results: [*]bytebox.Val) error{}!void {
    const lhs: u16 = @intCast(params[0].I32);
    const rhs: u16 = @intCast(params[1].I32);
    results[0] = .{ .I32 = @intCast(@mod(lhs, rhs)) };
}

fn hostI32ModBy(_: ?*anyopaque, _: *bytebox.ModuleInstance, params: [*]const bytebox.Val, results: [*]bytebox.Val) error{}!void {
    const lhs: i32 = params[0].I32;
    const rhs: i32 = params[1].I32;
    results[0] = .{ .I32 = @mod(lhs, rhs) };
}

fn hostU32ModBy(_: ?*anyopaque, _: *bytebox.ModuleInstance, params: [*]const bytebox.Val, results: [*]bytebox.Val) error{}!void {
    const lhs: u32 = @bitCast(params[0].I32);
    const rhs: u32 = @bitCast(params[1].I32);
    results[0] = .{ .I32 = @bitCast(@mod(lhs, rhs)) };
}

fn hostI64ModBy(_: ?*anyopaque, _: *bytebox.ModuleInstance, params: [*]const bytebox.Val, results: [*]bytebox.Val) error{}!void {
    const lhs: i64 = params[0].I64;
    const rhs: i64 = params[1].I64;
    results[0] = .{ .I64 = @mod(lhs, rhs) };
}

fn hostU64ModBy(_: ?*anyopaque, _: *bytebox.ModuleInstance, params: [*]const bytebox.Val, results: [*]bytebox.Val) error{}!void {
    const lhs: u64 = @bitCast(params[0].I64);
    const rhs: u64 = @bitCast(params[1].I64);
    results[0] = .{ .I64 = @bitCast(@mod(lhs, rhs)) };
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

const DecUnaryMathOp = enum {
    sqrt,
    sin,
    cos,
    tan,
    asin,
    acos,
    atan,
};

fn hostDecPow(_: ?*anyopaque, module: *bytebox.ModuleInstance, params: [*]const bytebox.Val, _: [*]bytebox.Val) error{}!void {
    const RocDec = builtins.dec.RocDec;
    const buffer = module.store.getMemory(0).buffer();
    const lhs_ptr: usize = @intCast(params[0].I32);
    const rhs_ptr: usize = @intCast(params[1].I32);
    const result_ptr: usize = @intCast(params[2].I32);
    if (lhs_ptr + 16 > buffer.len or rhs_ptr + 16 > buffer.len or result_ptr + 16 > buffer.len) return;

    const base = RocDec{ .num = readI128FromMem(buffer, lhs_ptr) };
    const exponent = RocDec{ .num = readI128FromMem(buffer, rhs_ptr) };
    if ((base.num == 0 and exponent.num < 0) or (base.num <= 0 and i128h.rem_i128(exponent.num, RocDec.one_point_zero_i128) != 0)) {
        wasm_crash_state = .crashed;
        return;
    }

    const result = builtins.dec.powC(base, exponent, &wasm_dec_roc_ops);
    if (wasm_crash_state == .crashed) return;
    writeI128ToMem(buffer, result_ptr, result);
}

fn hostDecUnaryMath(module: *bytebox.ModuleInstance, params: [*]const bytebox.Val, comptime op: DecUnaryMathOp) void {
    const RocDec = builtins.dec.RocDec;
    const buffer = module.store.getMemory(0).buffer();
    const arg_ptr: usize = @intCast(params[0].I32);
    const result_ptr: usize = @intCast(params[1].I32);
    if (arg_ptr + 16 > buffer.len or result_ptr + 16 > buffer.len) return;

    const arg = RocDec{ .num = readI128FromMem(buffer, arg_ptr) };
    switch (op) {
        .sqrt => if (arg.num < 0) {
            wasm_crash_state = .crashed;
            return;
        },
        .asin, .acos => if (arg.num < RocDec.neg_one_point_zero.num or arg.num > RocDec.one_point_zero.num) {
            wasm_crash_state = .crashed;
            return;
        },
        else => {},
    }

    const result = switch (op) {
        .sqrt => builtins.dec.sqrtC(arg, &wasm_dec_roc_ops),
        .sin => builtins.dec.sinC(arg, &wasm_dec_roc_ops),
        .cos => builtins.dec.cosC(arg, &wasm_dec_roc_ops),
        .tan => builtins.dec.tanC(arg, &wasm_dec_roc_ops),
        .asin => builtins.dec.asinC(arg, &wasm_dec_roc_ops),
        .acos => builtins.dec.acosC(arg, &wasm_dec_roc_ops),
        .atan => builtins.dec.atanC(arg, &wasm_dec_roc_ops),
    };
    if (wasm_crash_state == .crashed) return;
    writeI128ToMem(buffer, result_ptr, result);
}

fn hostDecSqrt(_: ?*anyopaque, module: *bytebox.ModuleInstance, params: [*]const bytebox.Val, _: [*]bytebox.Val) error{}!void {
    hostDecUnaryMath(module, params, .sqrt);
}

fn hostDecSin(_: ?*anyopaque, module: *bytebox.ModuleInstance, params: [*]const bytebox.Val, _: [*]bytebox.Val) error{}!void {
    hostDecUnaryMath(module, params, .sin);
}

fn hostDecCos(_: ?*anyopaque, module: *bytebox.ModuleInstance, params: [*]const bytebox.Val, _: [*]bytebox.Val) error{}!void {
    hostDecUnaryMath(module, params, .cos);
}

fn hostDecTan(_: ?*anyopaque, module: *bytebox.ModuleInstance, params: [*]const bytebox.Val, _: [*]bytebox.Val) error{}!void {
    hostDecUnaryMath(module, params, .tan);
}

fn hostDecAsin(_: ?*anyopaque, module: *bytebox.ModuleInstance, params: [*]const bytebox.Val, _: [*]bytebox.Val) error{}!void {
    hostDecUnaryMath(module, params, .asin);
}

fn hostDecAcos(_: ?*anyopaque, module: *bytebox.ModuleInstance, params: [*]const bytebox.Val, _: [*]bytebox.Val) error{}!void {
    hostDecUnaryMath(module, params, .acos);
}

fn hostDecAtan(_: ?*anyopaque, module: *bytebox.ModuleInstance, params: [*]const bytebox.Val, _: [*]bytebox.Val) error{}!void {
    hostDecUnaryMath(module, params, .atan);
}

fn hostI128ToStr(_: ?*anyopaque, module: *bytebox.ModuleInstance, params: [*]const bytebox.Val, results: [*]bytebox.Val) error{}!void {
    const buffer = module.store.getMemory(0).buffer();
    const val_ptr: usize = @intCast(params[0].I32);
    const buf_ptr: usize = @intCast(params[1].I32);
    if (val_ptr + 16 > buffer.len or buf_ptr + 48 > buffer.len) {
        results[0] = .{ .I32 = 0 };
        return;
    }
    const value = readI128FromMem(buffer, val_ptr);
    var fmt_buf: [40]u8 = undefined;
    const slice = std.fmt.bufPrint(&fmt_buf, "{}", .{value}) catch &.{};
    @memcpy(buffer[buf_ptr..][0..slice.len], slice);
    results[0] = .{ .I32 = @intCast(slice.len) };
}

fn hostU128ToStr(_: ?*anyopaque, module: *bytebox.ModuleInstance, params: [*]const bytebox.Val, results: [*]bytebox.Val) error{}!void {
    const buffer = module.store.getMemory(0).buffer();
    const val_ptr: usize = @intCast(params[0].I32);
    const buf_ptr: usize = @intCast(params[1].I32);
    if (val_ptr + 16 > buffer.len or buf_ptr + 48 > buffer.len) {
        results[0] = .{ .I32 = 0 };
        return;
    }
    const value = readU128FromMem(buffer, val_ptr);
    var fmt_buf: [40]u8 = undefined;
    const slice = std.fmt.bufPrint(&fmt_buf, "{}", .{value}) catch &.{};
    @memcpy(buffer[buf_ptr..][0..slice.len], slice);
    results[0] = .{ .I32 = @intCast(slice.len) };
}

fn hostFloatToStr(_: ?*anyopaque, module: *bytebox.ModuleInstance, params: [*]const bytebox.Val, results: [*]bytebox.Val) error{}!void {
    const buffer = module.store.getMemory(0).buffer();
    const val_bits: u64 = @bitCast(params[0].I64);
    const is_f32 = params[1].I32 != 0;
    const buf_ptr: usize = @intCast(params[2].I32);

    if (buf_ptr + 400 > buffer.len) {
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

fn hostFloatPow(_: ?*anyopaque, _: *bytebox.ModuleInstance, params: [*]const bytebox.Val, results: [*]bytebox.Val) error{}!void {
    const base = params[0].F64;
    const exponent = params[1].F64;
    const float_width: u8 = @intCast(params[2].I32);
    results[0] = .{ .F64 = switch (float_width) {
        4 => @as(f64, @floatCast(std.math.pow(f32, @as(f32, @floatCast(base)), @as(f32, @floatCast(exponent))))),
        8 => std.math.pow(f64, base, exponent),
        else => unreachable,
    } };
}

const HostFloatUnaryMathOp = enum {
    sin,
    cos,
    tan,
    asin,
    acos,
    atan,
};

fn hostFloatUnaryMath(params: [*]const bytebox.Val, results: [*]bytebox.Val, comptime op: HostFloatUnaryMathOp) void {
    const val = params[0].F64;
    const float_width: u8 = @intCast(params[1].I32);
    results[0] = .{ .F64 = switch (float_width) {
        4 => @as(f64, @floatCast(switch (op) {
            .sin => std.math.sin(@as(f32, @floatCast(val))),
            .cos => std.math.cos(@as(f32, @floatCast(val))),
            .tan => std.math.tan(@as(f32, @floatCast(val))),
            .asin => std.math.asin(@as(f32, @floatCast(val))),
            .acos => std.math.acos(@as(f32, @floatCast(val))),
            .atan => std.math.atan(@as(f32, @floatCast(val))),
        })),
        8 => switch (op) {
            .sin => std.math.sin(val),
            .cos => std.math.cos(val),
            .tan => std.math.tan(val),
            .asin => std.math.asin(val),
            .acos => std.math.acos(val),
            .atan => std.math.atan(val),
        },
        else => unreachable,
    } };
}

fn hostFloatSin(_: ?*anyopaque, _: *bytebox.ModuleInstance, params: [*]const bytebox.Val, results: [*]bytebox.Val) error{}!void {
    hostFloatUnaryMath(params, results, .sin);
}

fn hostFloatCos(_: ?*anyopaque, _: *bytebox.ModuleInstance, params: [*]const bytebox.Val, results: [*]bytebox.Val) error{}!void {
    hostFloatUnaryMath(params, results, .cos);
}

fn hostFloatTan(_: ?*anyopaque, _: *bytebox.ModuleInstance, params: [*]const bytebox.Val, results: [*]bytebox.Val) error{}!void {
    hostFloatUnaryMath(params, results, .tan);
}

fn hostFloatAsin(_: ?*anyopaque, _: *bytebox.ModuleInstance, params: [*]const bytebox.Val, results: [*]bytebox.Val) error{}!void {
    hostFloatUnaryMath(params, results, .asin);
}

fn hostFloatAcos(_: ?*anyopaque, _: *bytebox.ModuleInstance, params: [*]const bytebox.Val, results: [*]bytebox.Val) error{}!void {
    hostFloatUnaryMath(params, results, .acos);
}

fn hostFloatAtan(_: ?*anyopaque, _: *bytebox.ModuleInstance, params: [*]const bytebox.Val, results: [*]bytebox.Val) error{}!void {
    hostFloatUnaryMath(params, results, .atan);
}

fn hostIntToStr(_: ?*anyopaque, module: *bytebox.ModuleInstance, params: [*]const bytebox.Val, results: [*]bytebox.Val) error{}!void {
    const buffer = module.store.getMemory(0).buffer();
    const low: u64 = @bitCast(params[0].I64);
    const high: u64 = @bitCast(params[1].I64);
    const int_width: u8 = @intCast(params[2].I32);
    const is_signed = params[3].I32 != 0;
    const buf_ptr: usize = @intCast(params[4].I32);

    if (buf_ptr + 48 > buffer.len) {
        results[0] = .{ .I32 = 0 };
        return;
    }

    const signed_value: i128 = @bitCast((@as(u128, high) << 64) | @as(u128, low));
    const unsigned_value: u128 = (@as(u128, high) << 64) | @as(u128, low);

    var fmt_buf: [48]u8 = undefined;
    const formatted = (if (is_signed) switch (int_width) {
        1 => std.fmt.bufPrint(&fmt_buf, "{d}", .{@as(i8, @intCast(signed_value))}),
        2 => std.fmt.bufPrint(&fmt_buf, "{d}", .{@as(i16, @intCast(signed_value))}),
        4 => std.fmt.bufPrint(&fmt_buf, "{d}", .{@as(i32, @intCast(signed_value))}),
        8 => std.fmt.bufPrint(&fmt_buf, "{d}", .{@as(i64, @intCast(signed_value))}),
        16 => std.fmt.bufPrint(&fmt_buf, "{d}", .{signed_value}),
        else => unreachable,
    } else switch (int_width) {
        1 => std.fmt.bufPrint(&fmt_buf, "{d}", .{@as(u8, @intCast(unsigned_value))}),
        2 => std.fmt.bufPrint(&fmt_buf, "{d}", .{@as(u16, @intCast(unsigned_value))}),
        4 => std.fmt.bufPrint(&fmt_buf, "{d}", .{@as(u32, @intCast(unsigned_value))}),
        8 => std.fmt.bufPrint(&fmt_buf, "{d}", .{@as(u64, @intCast(unsigned_value))}),
        16 => std.fmt.bufPrint(&fmt_buf, "{d}", .{unsigned_value}),
        else => unreachable,
    }) catch {
        results[0] = .{ .I32 = 0 };
        return;
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
    const a_list_ptr: usize = @intCast(params[0].I32);
    const b_list_ptr: usize = @intCast(params[1].I32);
    if (a_list_ptr + 12 > buffer.len or b_list_ptr + 12 > buffer.len) {
        results[0] = .{ .I32 = 0 };
        return;
    }
    const a_data_ptr: usize = @intCast(readIntLittle(u32, buffer, a_list_ptr));
    const a_len: usize = @intCast(readIntLittle(u32, buffer, a_list_ptr + 4));
    const b_data_ptr: usize = @intCast(readIntLittle(u32, buffer, b_list_ptr));
    const b_len: usize = @intCast(readIntLittle(u32, buffer, b_list_ptr + 4));
    if (a_len != b_len) {
        results[0] = .{ .I32 = 0 };
        return;
    }
    if (a_len == 0) {
        results[0] = .{ .I32 = 1 };
        return;
    }
    for (0..a_len) |i| {
        const a_elem_ptr = a_data_ptr + i * 12;
        const b_elem_ptr = b_data_ptr + i * 12;
        const a = readWasmStr(buffer, a_elem_ptr);
        const b = readWasmStr(buffer, b_elem_ptr);
        if (a.len != b.len) {
            results[0] = .{ .I32 = 0 };
            return;
        }
        if (!bytesEqual(a.data[0..a.len], b.data[0..b.len])) {
            results[0] = .{ .I32 = 0 };
            return;
        }
    }
    results[0] = .{ .I32 = 1 };
}

fn hostListListEq(_: ?*anyopaque, module: *bytebox.ModuleInstance, params: [*]const bytebox.Val, results: [*]bytebox.Val) error{}!void {
    const buffer = module.store.getMemory(0).buffer();
    const a_list_ptr: usize = @intCast(params[0].I32);
    const b_list_ptr: usize = @intCast(params[1].I32);
    const inner_elem_size: usize = @intCast(params[2].I32);
    if (a_list_ptr + 12 > buffer.len or b_list_ptr + 12 > buffer.len) {
        results[0] = .{ .I32 = 0 };
        return;
    }
    const a_data_ptr: usize = @intCast(readIntLittle(u32, buffer, a_list_ptr));
    const a_len: usize = @intCast(readIntLittle(u32, buffer, a_list_ptr + 4));
    const b_data_ptr: usize = @intCast(readIntLittle(u32, buffer, b_list_ptr));
    const b_len: usize = @intCast(readIntLittle(u32, buffer, b_list_ptr + 4));
    if (a_len != b_len) {
        results[0] = .{ .I32 = 0 };
        return;
    }
    if (a_len == 0) {
        results[0] = .{ .I32 = 1 };
        return;
    }
    for (0..a_len) |i| {
        const a_elem_ptr = a_data_ptr + i * 12;
        const b_elem_ptr = b_data_ptr + i * 12;
        const a_data_inner: usize = @intCast(readIntLittle(u32, buffer, a_elem_ptr));
        const a_len_inner: usize = @intCast(readIntLittle(u32, buffer, a_elem_ptr + 4));
        const b_data_inner: usize = @intCast(readIntLittle(u32, buffer, b_elem_ptr));
        const b_len_inner: usize = @intCast(readIntLittle(u32, buffer, b_elem_ptr + 4));
        if (a_len_inner != b_len_inner) {
            results[0] = .{ .I32 = 0 };
            return;
        }
        if (a_len_inner == 0) continue;
        const total_bytes = a_len_inner * inner_elem_size;
        if (a_data_inner + total_bytes > buffer.len or b_data_inner + total_bytes > buffer.len) {
            results[0] = .{ .I32 = 0 };
            return;
        }
        if (!bytesEqual(buffer[a_data_inner..][0..total_bytes], buffer[b_data_inner..][0..total_bytes])) {
            results[0] = .{ .I32 = 0 };
            return;
        }
    }
    results[0] = .{ .I32 = 1 };
}

fn readWasmStr(buffer: []u8, str_ptr: usize) WasmStr {
    if (builtin.mode == .Debug and std.debug.runtime_safety) {
        if (str_ptr + 12 > buffer.len) {
            std.debug.panic(
                "wasm_runner invariant violated: string header ptr={} exceeds memory len={}",
                .{ str_ptr, buffer.len },
            );
        }
    }
    const bytes = buffer[str_ptr..][0..12];
    if ((bytes[11] & 0x80) != 0) {
        const len = bytes[11] & 0x7F;
        if (builtin.mode == .Debug and std.debug.runtime_safety) {
            if (len > 11) {
                std.debug.panic(
                    "wasm_runner invariant violated: invalid SSO string len={} at ptr={}",
                    .{ len, str_ptr },
                );
            }
        }
        return .{ .data = bytes[0..11].ptr, .data_offset = str_ptr, .len = len, .cap_or_alloc = 0, .is_small = true };
    } else {
        const data_ptr: usize = @intCast(readIntLittle(u32, buffer, str_ptr));
        const cap_or_alloc = readIntLittle(u32, buffer, str_ptr + 4);
        const len: usize = @intCast(readIntLittle(u32, buffer, str_ptr + 8));
        if (builtin.mode == .Debug and std.debug.runtime_safety) {
            if (data_ptr + len > buffer.len) {
                std.debug.panic(
                    "wasm_runner invariant violated: heap string ptr={} len={} exceeds memory len={} (header ptr={})",
                    .{ data_ptr, len, buffer.len, str_ptr },
                );
            }
        }
        return .{ .data = buffer[data_ptr..].ptr, .data_offset = data_ptr, .len = len, .cap_or_alloc = cap_or_alloc, .is_small = false };
    }
}

fn encodeWasmListCapacity(capacity: usize) u32 {
    return @intCast(capacity << 1);
}

fn decodeWasmListCapacity(encoded_capacity: usize) usize {
    return encoded_capacity >> 1;
}

fn wasmAllocPtrFromCapOrData(cap_or_alloc: usize, data_offset: usize) usize {
    if ((cap_or_alloc & 1) != 0) {
        return cap_or_alloc & ~@as(usize, 1);
    } else {
        return data_offset;
    }
}

fn increfWasmDataPtr(buffer: []u8, data_ptr: usize) void {
    if (data_ptr < 4 or data_ptr > buffer.len) return;
    const rc_ptr = (data_ptr & ~@as(usize, 3)) - 4;
    const rc = readIntLittle(u32, buffer, rc_ptr);
    if (rc == 0) return;
    writeIntLittle(u32, buffer, rc_ptr, rc + 1);
}

fn writeWasmStr(buffer: []u8, result_ptr: usize, data: [*]const u8, len: usize) void {
    if (len < 12) {
        @memset(buffer[result_ptr..][0..12], 0);
        @memcpy(buffer[result_ptr..][0..len], data[0..len]);
        buffer[result_ptr + 11] = @intCast(len | 0x80);
    } else {
        const data_ptr = allocWasmData(buffer, 1, len);
        @memcpy(buffer[data_ptr..][0..len], data[0..len]);
        writeIntLittle(u32, buffer, result_ptr, @intCast(data_ptr));
        writeIntLittle(u32, buffer, result_ptr + 4, @intCast(len << 1));
        writeIntLittle(u32, buffer, result_ptr + 8, @intCast(len));
    }
}

fn writeWasmStrViewFromStr(buffer: []u8, result_ptr: usize, source: WasmStr, start: usize, len: usize) void {
    if (source.is_small) {
        writeWasmStr(buffer, result_ptr, source.data + start, len);
        return;
    }

    const data_offset = source.data_offset + start;
    const alloc_ptr = wasmAllocPtrFromCapOrData(source.cap_or_alloc, source.data_offset);
    increfWasmDataPtr(buffer, alloc_ptr);
    writeIntLittle(u32, buffer, result_ptr, @intCast(data_offset));
    writeIntLittle(u32, buffer, result_ptr + 4, source.cap_or_alloc);
    writeIntLittle(u32, buffer, result_ptr + 8, @intCast(len));
}

fn writeWasmStrViewFromList(buffer: []u8, result_ptr: usize, list_ptr: usize, len: usize) void {
    if (len == 0) {
        writeWasmEmptyStr(buffer, result_ptr);
        return;
    }
    const data_offset: usize = @intCast(readIntLittle(u32, buffer, list_ptr));
    const cap_or_alloc = readIntLittle(u32, buffer, list_ptr + 8);
    if (len < 12) {
        writeWasmStr(buffer, result_ptr, buffer[data_offset..].ptr, len);
        return;
    }

    const alloc_ptr = wasmAllocPtrFromCapOrData(cap_or_alloc, data_offset);
    increfWasmDataPtr(buffer, alloc_ptr);
    writeIntLittle(u32, buffer, result_ptr, @intCast(data_offset));
    writeIntLittle(u32, buffer, result_ptr + 4, cap_or_alloc);
    writeIntLittle(u32, buffer, result_ptr + 8, @intCast(len));
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
        .capacity_or_alloc_ptr = builtins.str.RocStr.encodeCapacity(len),
        .length = len,
    };
}

fn allocExtraBytes(alignment: u32) u32 {
    const ptr_width: u32 = 8;
    return if (alignment > ptr_width) alignment else ptr_width;
}

fn allocWasmData(buffer: []u8, alignment: u32, length: usize) u32 {
    wasm_allocation_count += 1;
    const align_val: u32 = if (alignment > 4) alignment else 4;
    const extra_bytes = allocExtraBytes(alignment);
    const alloc_ptr = (wasm_heap_ptr + align_val - 1) & ~(align_val - 1);
    const data_ptr = alloc_ptr + extra_bytes;
    wasm_heap_ptr = @intCast(data_ptr + length);
    writeIntLittle(u32, buffer, data_ptr - 8, @intCast(length));
    writeIntLittle(u32, buffer, data_ptr - 4, 1);
    return data_ptr;
}

// RocOps callbacks follow the platform C ABI: a leading *RocOps (the i32 pointer to the
// RocOps struct in linear memory, unused here) followed by the natural arguments, with the
// result returned directly rather than written back into an args struct.

fn hostRocAlloc(_: ?*anyopaque, module: *bytebox.ModuleInstance, params: [*]const bytebox.Val, results: [*]bytebox.Val) error{}!void {
    const buffer = module.store.getMemory(0).buffer();
    const length: u32 = @bitCast(params[1].I32);
    const alignment: u32 = @bitCast(params[2].I32);
    const data_ptr = allocWasmData(buffer, alignment, length);
    results[0] = .{ .I32 = @bitCast(data_ptr) };
}

fn hostRocDealloc(_: ?*anyopaque, _: *bytebox.ModuleInstance, _: [*]const bytebox.Val, _: [*]bytebox.Val) error{}!void {}

fn hostRocRealloc(_: ?*anyopaque, module: *bytebox.ModuleInstance, params: [*]const bytebox.Val, results: [*]bytebox.Val) error{}!void {
    const buffer = module.store.getMemory(0).buffer();
    const old_data_ptr: u32 = @bitCast(params[1].I32);
    const new_length: u32 = @bitCast(params[2].I32);
    const alignment: u32 = @bitCast(params[3].I32);
    const old_length: usize = if (old_data_ptr >= 8 and old_data_ptr <= buffer.len)
        readIntLittle(u32, buffer, old_data_ptr - 8)
    else
        0;
    const data_ptr = allocWasmData(buffer, alignment, new_length);
    const copy_len = @min(old_length, new_length);
    if (copy_len > 0 and old_data_ptr + copy_len <= buffer.len and data_ptr + copy_len <= buffer.len) {
        @memcpy(buffer[data_ptr..][0..copy_len], buffer[old_data_ptr..][0..copy_len]);
    }
    results[0] = .{ .I32 = @bitCast(data_ptr) };
}

fn hostRocDbg(_: ?*anyopaque, module: *bytebox.ModuleInstance, params: [*]const bytebox.Val, _: [*]bytebox.Val) error{}!void {
    const buffer = module.store.getMemory(0).buffer();
    const msg_ptr: u32 = @bitCast(params[1].I32);
    const msg_len: u32 = @bitCast(params[2].I32);
    if (msg_ptr + msg_len > buffer.len) return;
}

fn hostRocExpectFailed(_: ?*anyopaque, module: *bytebox.ModuleInstance, params: [*]const bytebox.Val, _: [*]bytebox.Val) error{}!void {
    const buffer = module.store.getMemory(0).buffer();
    const msg_ptr: u32 = @bitCast(params[1].I32);
    const msg_len: u32 = @bitCast(params[2].I32);
    if (msg_ptr + msg_len > buffer.len) return;
    wasm_crash_state = .crashed;
}

fn hostRocCrashed(_: ?*anyopaque, module: *bytebox.ModuleInstance, params: [*]const bytebox.Val, _: [*]bytebox.Val) error{}!void {
    const buffer = module.store.getMemory(0).buffer();
    const msg_ptr: u32 = @bitCast(params[1].I32);
    const msg_len: u32 = @bitCast(params[2].I32);
    if (msg_ptr + msg_len > buffer.len) return;
    wasm_crash_state = .crashed;
}

fn isWhitespace(c: u8) bool {
    return c == ' ' or c == '\t' or c == '\n' or c == '\r' or c == 0x0b or c == 0x0c;
}

fn hostStrTrim(_: ?*anyopaque, module: *bytebox.ModuleInstance, params: [*]const bytebox.Val, _: [*]bytebox.Val) error{}!void {
    const buffer = module.store.getMemory(0).buffer();
    const str = readWasmStr(buffer, @intCast(params[0].I32));
    const slice = str.data[0..str.len];
    var start: usize = 0;
    var end: usize = str.len;
    while (start < end and isWhitespace(slice[start])) start += 1;
    while (end > start and isWhitespace(slice[end - 1])) end -= 1;
    writeWasmStr(buffer, @intCast(params[1].I32), slice[start..].ptr, end - start);
}

fn hostStrTrimStart(_: ?*anyopaque, module: *bytebox.ModuleInstance, params: [*]const bytebox.Val, _: [*]bytebox.Val) error{}!void {
    const buffer = module.store.getMemory(0).buffer();
    const str = readWasmStr(buffer, @intCast(params[0].I32));
    const slice = str.data[0..str.len];
    var start: usize = 0;
    while (start < slice.len and isWhitespace(slice[start])) start += 1;
    writeWasmStr(buffer, @intCast(params[1].I32), slice[start..].ptr, slice.len - start);
}

fn hostStrTrimEnd(_: ?*anyopaque, module: *bytebox.ModuleInstance, params: [*]const bytebox.Val, _: [*]bytebox.Val) error{}!void {
    const buffer = module.store.getMemory(0).buffer();
    const str = readWasmStr(buffer, @intCast(params[0].I32));
    const slice = str.data[0..str.len];
    var end: usize = slice.len;
    while (end > 0 and isWhitespace(slice[end - 1])) end -= 1;
    writeWasmStr(buffer, @intCast(params[1].I32), slice.ptr, end);
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
    for (0..str.len) |i| {
        buffer[dest_start + i] = std.ascii.toLower(str.data[i]);
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
    for (0..str.len) |i| {
        buffer[dest_start + i] = std.ascii.toUpper(str.data[i]);
    }
    writeWasmStr(buffer, @intCast(params[1].I32), buffer[dest_start..].ptr, str.len);
}

fn hostStrReleaseExcessCapacity(_: ?*anyopaque, module: *bytebox.ModuleInstance, params: [*]const bytebox.Val, _: [*]bytebox.Val) error{}!void {
    const buffer = module.store.getMemory(0).buffer();
    const str = readWasmStr(buffer, @intCast(params[0].I32));
    writeWasmStr(buffer, @intCast(params[1].I32), str.data, str.len);
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
    writeIntLittle(u32, buffer, result_ptr, @intCast(dest_start));
    writeIntLittle(u32, buffer, result_ptr + 4, @intCast(cap << 1));
    writeIntLittle(u32, buffer, result_ptr + 8, 0);
}

fn hostStrEscapeAndQuote(_: ?*anyopaque, module: *bytebox.ModuleInstance, params: [*]const bytebox.Val, _: [*]bytebox.Val) error{}!void {
    const buffer = module.store.getMemory(0).buffer();
    const str = readWasmStr(buffer, @intCast(params[0].I32));
    const slice = str.data[0..str.len];
    const result_ptr: usize = @intCast(params[1].I32);

    var extra: usize = 0;
    for (slice) |ch| {
        if (ch == '\\' or ch == '"') extra += 1;
    }

    const result_len = slice.len + extra + 2;
    if (result_len < 12) {
        var small: [12]u8 = .{0} ** 12;
        small[0] = '"';
        var pos: usize = 1;
        for (slice) |ch| {
            if (ch == '\\' or ch == '"') {
                small[pos] = '\\';
                pos += 1;
            }
            small[pos] = ch;
            pos += 1;
        }
        small[pos] = '"';
        writeWasmStr(buffer, result_ptr, small[0..].ptr, result_len);
        return;
    }

    const dest_start = wasm_heap_ptr;
    wasm_heap_ptr += @intCast(result_len);
    buffer[dest_start] = '"';
    var pos: usize = dest_start + 1;
    for (slice) |ch| {
        if (ch == '\\' or ch == '"') {
            buffer[pos] = '\\';
            pos += 1;
        }
        buffer[pos] = ch;
        pos += 1;
    }
    buffer[pos] = '"';
    writeWasmStr(buffer, result_ptr, buffer[dest_start..].ptr, result_len);
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
    if (prefix.len <= str.len and bytesEqual(str.data[0..prefix.len], prefix.data[0..prefix.len])) {
        writeWasmStrViewFromStr(buffer, @intCast(params[2].I32), str, prefix.len, str.len - prefix.len);
    } else {
        writeWasmStrViewFromStr(buffer, @intCast(params[2].I32), str, 0, str.len);
    }
}

fn hostStrDropPrefixCaselessAscii(_: ?*anyopaque, module: *bytebox.ModuleInstance, params: [*]const bytebox.Val, _: [*]bytebox.Val) error{}!void {
    const buffer = module.store.getMemory(0).buffer();
    const str = readWasmStr(buffer, @intCast(params[0].I32));
    const prefix = readWasmStr(buffer, @intCast(params[1].I32));
    const result_ptr: usize = @intCast(params[2].I32);
    const after_offset: usize = @intCast(params[3].I32);
    const found_offset: usize = @intCast(params[4].I32);

    if (prefix.len <= str.len and bytesCaselessAsciiEqual(str.data[0..prefix.len], prefix.data[0..prefix.len])) {
        writeWasmStrViewFromStr(buffer, result_ptr + after_offset, str, prefix.len, str.len - prefix.len);
        buffer[result_ptr + found_offset] = 1;
    } else {
        writeWasmEmptyStr(buffer, result_ptr + after_offset);
        buffer[result_ptr + found_offset] = 0;
    }
}

fn hostStrDropSuffix(_: ?*anyopaque, module: *bytebox.ModuleInstance, params: [*]const bytebox.Val, _: [*]bytebox.Val) error{}!void {
    const buffer = module.store.getMemory(0).buffer();
    const str = readWasmStr(buffer, @intCast(params[0].I32));
    const suffix = readWasmStr(buffer, @intCast(params[1].I32));
    if (suffix.len <= str.len and bytesEqual((str.data + str.len - suffix.len)[0..suffix.len], suffix.data[0..suffix.len])) {
        writeWasmStrViewFromStr(buffer, @intCast(params[2].I32), str, 0, str.len - suffix.len);
    } else {
        writeWasmStrViewFromStr(buffer, @intCast(params[2].I32), str, 0, str.len);
    }
}

fn hostStrFindFirst(_: ?*anyopaque, module: *bytebox.ModuleInstance, params: [*]const bytebox.Val, _: [*]bytebox.Val) error{}!void {
    const buffer = module.store.getMemory(0).buffer();
    const str = readWasmStr(buffer, @intCast(params[0].I32));
    const delimiter = readWasmStr(buffer, @intCast(params[1].I32));
    const result_ptr: usize = @intCast(params[2].I32);
    const after_offset: usize = @intCast(params[3].I32);
    const before_offset: usize = @intCast(params[4].I32);
    const found_offset: usize = @intCast(params[5].I32);

    const str_slice = str.data[0..str.len];
    const delimiter_slice = delimiter.data[0..delimiter.len];
    const maybe_index = bytesIndexOf(str_slice, delimiter_slice);
    if (maybe_index) |index| {
        writeWasmStrViewFromStr(buffer, result_ptr + before_offset, str, 0, index);
        writeWasmStrViewFromStr(buffer, result_ptr + after_offset, str, index + delimiter.len, str.len - index - delimiter.len);
        buffer[result_ptr + found_offset] = 1;
    } else {
        writeWasmEmptyStr(buffer, result_ptr + before_offset);
        writeWasmEmptyStr(buffer, result_ptr + after_offset);
        buffer[result_ptr + found_offset] = 0;
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
            if (bytesEqual(str_slice[i..][0..sep.len], sep_slice)) {
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
            if (bytesEqual(str_slice[i..][0..sep.len], sep_slice)) {
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
    writeIntLittle(u32, buffer, result_ptr, @intCast(list_data_start));
    writeIntLittle(u32, buffer, result_ptr + 4, @intCast(count));
    writeIntLittle(u32, buffer, result_ptr + 8, encodeWasmListCapacity(count));
}

fn hostStrJoinWith(_: ?*anyopaque, module: *bytebox.ModuleInstance, params: [*]const bytebox.Val, _: [*]bytebox.Val) error{}!void {
    const buffer = module.store.getMemory(0).buffer();
    const list_ptr: usize = @intCast(params[0].I32);
    const sep = readWasmStr(buffer, @intCast(params[1].I32));
    const list_data: usize = @intCast(readIntLittle(u32, buffer, list_ptr));
    const list_len: usize = @intCast(readIntLittle(u32, buffer, list_ptr + 4));
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
    writeIntLittle(u32, buffer, result_ptr, @intCast(dest_start));
    writeIntLittle(u32, buffer, result_ptr + 4, @intCast(needed << 1));
    writeIntLittle(u32, buffer, result_ptr + 8, @intCast(str.len));
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

fn hostListAppendUnsafe(_: ?*anyopaque, module: *bytebox.ModuleInstance, params: [*]const bytebox.Val, _: [*]bytebox.Val) error{}!void {
    const buffer = module.store.getMemory(0).buffer();
    const list_ptr: usize = @intCast(params[0].I32);
    const elem_ptr: usize = @intCast(params[1].I32);
    const elem_width: usize = @intCast(params[2].I32);
    const alignment: u32 = @bitCast(params[3].I32);
    const result_ptr: usize = @intCast(params[4].I32);

    const data_ptr: usize = @intCast(readIntLittle(u32, buffer, list_ptr));
    const len: usize = @intCast(readIntLittle(u32, buffer, list_ptr + 4));
    const encoded_cap: usize = @intCast(readIntLittle(u32, buffer, list_ptr + 8));
    const cap = decodeWasmListCapacity(encoded_cap);
    const new_len = len + 1;

    std.debug.assert(alignment > 0);

    if (elem_width == 0) {
        writeIntLittle(u32, buffer, result_ptr, @intCast(data_ptr));
        writeIntLittle(u32, buffer, result_ptr + 4, @intCast(new_len));
        writeIntLittle(u32, buffer, result_ptr + 8, @intCast(encoded_cap));
        return;
    }

    if ((encoded_cap & 1) != 0) {
        std.debug.panic("roc_list_append_unsafe called on seamless-slice list", .{});
    }
    if (cap < new_len) {
        std.debug.panic("roc_list_append_unsafe called without spare capacity (len={}, cap={})", .{ len, cap });
    }
    if (data_ptr == 0) {
        std.debug.panic("roc_list_append_unsafe called with null data pointer for non-ZST list", .{});
    }
    @memcpy(buffer[data_ptr + len * elem_width ..][0..elem_width], buffer[elem_ptr..][0..elem_width]);

    writeIntLittle(u32, buffer, result_ptr, @intCast(data_ptr));
    writeIntLittle(u32, buffer, result_ptr + 4, @intCast(new_len));
    writeIntLittle(u32, buffer, result_ptr + 8, @intCast(encoded_cap));
}

fn hostListConcat(_: ?*anyopaque, module: *bytebox.ModuleInstance, params: [*]const bytebox.Val, _: [*]bytebox.Val) error{}!void {
    const buffer = module.store.getMemory(0).buffer();
    const list_a_ptr: usize = @intCast(params[0].I32);
    const list_b_ptr: usize = @intCast(params[1].I32);
    const elem_width: usize = @intCast(params[2].I32);
    const alignment: u32 = @bitCast(params[3].I32);
    const result_ptr: usize = @intCast(params[4].I32);

    const a_data: usize = @intCast(readIntLittle(u32, buffer, list_a_ptr));
    const a_len: usize = @intCast(readIntLittle(u32, buffer, list_a_ptr + 4));
    const b_data: usize = @intCast(readIntLittle(u32, buffer, list_b_ptr));
    const b_len: usize = @intCast(readIntLittle(u32, buffer, list_b_ptr + 4));
    const new_len = a_len + b_len;

    if (elem_width == 0) {
        const data_ptr = if (a_len != 0) a_data else b_data;
        writeIntLittle(u32, buffer, result_ptr, @intCast(data_ptr));
        writeIntLittle(u32, buffer, result_ptr + 4, @intCast(new_len));
        writeIntLittle(u32, buffer, result_ptr + 8, encodeWasmListCapacity(new_len));
        return;
    }

    const total_bytes: usize = new_len * elem_width;
    const new_data = if (total_bytes == 0) 0 else allocWasmData(buffer, alignment, total_bytes);
    if (a_len > 0 and a_data != 0) {
        @memcpy(buffer[new_data..][0 .. a_len * elem_width], buffer[a_data..][0 .. a_len * elem_width]);
    }
    if (b_len > 0 and b_data != 0) {
        const offset = a_len * elem_width;
        @memcpy(buffer[new_data + offset ..][0 .. b_len * elem_width], buffer[b_data..][0 .. b_len * elem_width]);
    }

    writeIntLittle(u32, buffer, result_ptr, @intCast(new_data));
    writeIntLittle(u32, buffer, result_ptr + 4, @intCast(new_len));
    writeIntLittle(u32, buffer, result_ptr + 8, encodeWasmListCapacity(new_len));
}

fn hostListDropAt(_: ?*anyopaque, module: *bytebox.ModuleInstance, params: [*]const bytebox.Val, _: [*]bytebox.Val) error{}!void {
    const buffer = module.store.getMemory(0).buffer();
    const list_ptr: usize = @intCast(params[0].I32);
    const elem_width: usize = @intCast(params[1].I32);
    const alignment: u32 = @bitCast(params[2].I32);
    const index: usize = @intCast(params[3].I32);
    const result_ptr: usize = @intCast(params[4].I32);

    const data_ptr: usize = @intCast(readIntLittle(u32, buffer, list_ptr));
    const len: usize = @intCast(readIntLittle(u32, buffer, list_ptr + 4));
    const encoded_cap: usize = @intCast(readIntLittle(u32, buffer, list_ptr + 8));

    if (index >= len) {
        writeIntLittle(u32, buffer, result_ptr, @intCast(data_ptr));
        writeIntLittle(u32, buffer, result_ptr + 4, @intCast(len));
        writeIntLittle(u32, buffer, result_ptr + 8, @intCast(encoded_cap));
        return;
    }

    const new_len = len - 1;
    if (new_len == 0) {
        writeIntLittle(u32, buffer, result_ptr, 0);
        writeIntLittle(u32, buffer, result_ptr + 4, 0);
        writeIntLittle(u32, buffer, result_ptr + 8, 0);
        return;
    }

    if (elem_width == 0) {
        writeIntLittle(u32, buffer, result_ptr, @intCast(data_ptr));
        writeIntLittle(u32, buffer, result_ptr + 4, @intCast(new_len));
        writeIntLittle(u32, buffer, result_ptr + 8, encodeWasmListCapacity(new_len));
        return;
    }

    const new_data = allocWasmData(buffer, alignment, new_len * elem_width);
    const head_size = index * elem_width;
    if (head_size != 0 and data_ptr != 0) {
        @memcpy(buffer[new_data..][0..head_size], buffer[data_ptr..][0..head_size]);
    }
    const tail_size = (len - index - 1) * elem_width;
    if (tail_size != 0 and data_ptr != 0) {
        @memcpy(
            buffer[new_data + head_size ..][0..tail_size],
            buffer[data_ptr + (index + 1) * elem_width ..][0..tail_size],
        );
    }

    writeIntLittle(u32, buffer, result_ptr, @intCast(new_data));
    writeIntLittle(u32, buffer, result_ptr + 4, @intCast(new_len));
    writeIntLittle(u32, buffer, result_ptr + 8, encodeWasmListCapacity(new_len));
}

fn hostListReserve(_: ?*anyopaque, module: *bytebox.ModuleInstance, params: [*]const bytebox.Val, _: [*]bytebox.Val) error{}!void {
    const buffer = module.store.getMemory(0).buffer();
    const list_ptr: usize = @intCast(params[0].I32);
    const spare: usize = @intCast(params[1].I64);
    const elem_width: usize = @intCast(params[2].I32);
    const alignment: u32 = @bitCast(params[3].I32);
    const result_ptr: usize = @intCast(params[4].I32);

    const data_ptr: usize = @intCast(readIntLittle(u32, buffer, list_ptr));
    const len: usize = @intCast(readIntLittle(u32, buffer, list_ptr + 4));
    const encoded_cap: usize = @intCast(readIntLittle(u32, buffer, list_ptr + 8));
    const desired_cap = len + spare;

    if (elem_width == 0) {
        writeIntLittle(u32, buffer, result_ptr, @intCast(data_ptr));
        writeIntLittle(u32, buffer, result_ptr + 4, @intCast(len));
        writeIntLittle(u32, buffer, result_ptr + 8, encodeWasmListCapacity(desired_cap));
        return;
    }

    const cap = decodeWasmListCapacity(encoded_cap);
    if ((encoded_cap & 1) == 0 and cap >= desired_cap) {
        writeIntLittle(u32, buffer, result_ptr, @intCast(data_ptr));
        writeIntLittle(u32, buffer, result_ptr + 4, @intCast(len));
        writeIntLittle(u32, buffer, result_ptr + 8, @intCast(encoded_cap));
        return;
    }

    const new_data = allocWasmData(buffer, alignment, desired_cap * elem_width);
    if (len != 0 and data_ptr != 0) {
        @memcpy(buffer[new_data..][0 .. len * elem_width], buffer[data_ptr..][0 .. len * elem_width]);
    }

    writeIntLittle(u32, buffer, result_ptr, @intCast(new_data));
    writeIntLittle(u32, buffer, result_ptr + 4, @intCast(len));
    writeIntLittle(u32, buffer, result_ptr + 8, encodeWasmListCapacity(desired_cap));
}

fn hostListReverse(_: ?*anyopaque, module: *bytebox.ModuleInstance, params: [*]const bytebox.Val, _: [*]bytebox.Val) error{}!void {
    const buffer = module.store.getMemory(0).buffer();
    const list_ptr: usize = @intCast(params[0].I32);
    const elem_width: usize = @intCast(params[1].I32);
    const alignment: u32 = @bitCast(params[2].I32);
    const result_ptr: usize = @intCast(params[3].I32);

    const data_ptr: usize = @intCast(readIntLittle(u32, buffer, list_ptr));
    const len: usize = @intCast(readIntLittle(u32, buffer, list_ptr + 4));
    const encoded_cap: usize = @intCast(readIntLittle(u32, buffer, list_ptr + 8));

    if (len < 2 or elem_width == 0) {
        writeIntLittle(u32, buffer, result_ptr, @intCast(data_ptr));
        writeIntLittle(u32, buffer, result_ptr + 4, @intCast(len));
        writeIntLittle(u32, buffer, result_ptr + 8, @intCast(encoded_cap));
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

    writeIntLittle(u32, buffer, result_ptr, @intCast(reversed_data));
    writeIntLittle(u32, buffer, result_ptr + 4, @intCast(len));
    writeIntLittle(u32, buffer, result_ptr + 8, encodeWasmListCapacity(len));
}

fn hostListReplace(_: ?*anyopaque, module: *bytebox.ModuleInstance, params: [*]const bytebox.Val, _: [*]bytebox.Val) error{}!void {
    const buffer = module.store.getMemory(0).buffer();
    const list_ptr: usize = @intCast(params[0].I32);
    const elem_width: usize = @intCast(params[1].I32);
    const alignment: u32 = @bitCast(params[2].I32);
    const index: usize = @intCast(params[3].I64);
    const element_ptr: usize = @intCast(params[4].I32);
    const out_element_ptr: usize = @intCast(params[5].I32);
    const result_ptr: usize = @intCast(params[6].I32);

    const data_ptr: usize = @intCast(readIntLittle(u32, buffer, list_ptr));
    const len: usize = @intCast(readIntLittle(u32, buffer, list_ptr + 4));
    const encoded_cap: usize = @intCast(readIntLittle(u32, buffer, list_ptr + 8));

    if (elem_width == 0) {
        writeIntLittle(u32, buffer, result_ptr, @intCast(data_ptr));
        writeIntLittle(u32, buffer, result_ptr + 4, @intCast(len));
        writeIntLittle(u32, buffer, result_ptr + 8, @intCast(encoded_cap));
        return;
    }

    const new_data = allocWasmData(buffer, alignment, len * elem_width);
    if (len != 0 and data_ptr != 0) {
        @memcpy(buffer[new_data..][0 .. len * elem_width], buffer[data_ptr..][0 .. len * elem_width]);
    }

    const slot = new_data + index * elem_width;
    if (out_element_ptr != 0) {
        @memcpy(buffer[out_element_ptr..][0..elem_width], buffer[slot..][0..elem_width]);
    }
    @memcpy(buffer[slot..][0..elem_width], buffer[element_ptr..][0..elem_width]);

    writeIntLittle(u32, buffer, result_ptr, @intCast(new_data));
    writeIntLittle(u32, buffer, result_ptr + 4, @intCast(len));
    writeIntLittle(u32, buffer, result_ptr + 8, encodeWasmListCapacity(len));
}

fn hostListSwap(_: ?*anyopaque, module: *bytebox.ModuleInstance, params: [*]const bytebox.Val, _: [*]bytebox.Val) error{}!void {
    const buffer = module.store.getMemory(0).buffer();
    const list_ptr: usize = @intCast(params[0].I32);
    const elem_width: usize = @intCast(params[1].I32);
    const alignment: u32 = @bitCast(params[2].I32);
    const index_1: usize = @intCast(params[3].I64);
    const index_2: usize = @intCast(params[4].I64);
    const result_ptr: usize = @intCast(params[5].I32);

    const data_ptr: usize = @intCast(readIntLittle(u32, buffer, list_ptr));
    const len: usize = @intCast(readIntLittle(u32, buffer, list_ptr + 4));
    const encoded_cap: usize = @intCast(readIntLittle(u32, buffer, list_ptr + 8));

    if (elem_width == 0) {
        writeIntLittle(u32, buffer, result_ptr, @intCast(data_ptr));
        writeIntLittle(u32, buffer, result_ptr + 4, @intCast(len));
        writeIntLittle(u32, buffer, result_ptr + 8, @intCast(encoded_cap));
        return;
    }

    const new_data = allocWasmData(buffer, alignment, len * elem_width);
    if (len != 0 and data_ptr != 0) {
        @memcpy(buffer[new_data..][0 .. len * elem_width], buffer[data_ptr..][0 .. len * elem_width]);
    }

    if (index_1 != index_2 and index_1 < len and index_2 < len) {
        const a = new_data + index_1 * elem_width;
        const b = new_data + index_2 * elem_width;
        for (0..elem_width) |i| {
            const tmp = buffer[a + i];
            buffer[a + i] = buffer[b + i];
            buffer[b + i] = tmp;
        }
    }

    writeIntLittle(u32, buffer, result_ptr, @intCast(new_data));
    writeIntLittle(u32, buffer, result_ptr + 4, @intCast(len));
    writeIntLittle(u32, buffer, result_ptr + 8, encodeWasmListCapacity(len));
}

fn hostStrFromUtf8(_: ?*anyopaque, module: *bytebox.ModuleInstance, params: [*]const bytebox.Val, _: [*]bytebox.Val) error{}!void {
    const Utf8ByteProblem = builtins.str.Utf8ByteProblem;
    const buffer = module.store.getMemory(0).buffer();
    const list_ptr: usize = @intCast(params[0].I32);
    const result_ptr: usize = @intCast(params[1].I32);
    const result_size: usize = @intCast(params[2].I32);
    const disc_offset: usize = @intCast(params[3].I32);
    const disc_size: usize = @intCast(params[4].I32);
    const ok_disc: u32 = @bitCast(params[5].I32);
    const err_disc: u32 = @bitCast(params[6].I32);
    const index_off: usize = @intCast(params[7].I32);
    const index_size: usize = @intCast(params[8].I32);
    const problem_off: usize = @intCast(params[9].I32);
    const problem_size: usize = @intCast(params[10].I32);
    const inner_disc_offset: usize = @intCast(params[11].I32);
    const inner_disc_size: usize = @intCast(params[12].I32);
    const inner_bad_utf8_disc: u32 = @bitCast(params[13].I32);
    if (list_ptr + 12 > buffer.len or result_ptr + result_size > buffer.len) return;
    const data_ptr: usize = @intCast(readIntLittle(u32, buffer, list_ptr));
    const len: usize = @intCast(readIntLittle(u32, buffer, list_ptr + 4));
    if (data_ptr + len > buffer.len) return;
    const data = buffer[data_ptr..][0..len];
    @memset(buffer[result_ptr..][0..result_size], 0);
    if (std.unicode.utf8ValidateSlice(data)) {
        writeWasmStrViewFromList(buffer, result_ptr, list_ptr, len);
        writeWasmTagDiscriminant(buffer, result_ptr, disc_offset, disc_size, ok_disc);
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
                writeWasmInt(buffer, result_ptr + index_off, index_size, @intCast(index));
                writeWasmInt(buffer, result_ptr + problem_off, problem_size, @intFromEnum(problem));
                break;
            };
            index += next_num_bytes;
        }
        writeWasmTagDiscriminant(buffer, result_ptr, inner_disc_offset, inner_disc_size, inner_bad_utf8_disc);
        writeWasmTagDiscriminant(buffer, result_ptr, disc_offset, disc_size, err_disc);
    }
}

fn writeWasmTagDiscriminant(
    buffer: []u8,
    base_ptr: usize,
    disc_offset: usize,
    disc_size: usize,
    value: u32,
) void {
    const dst = base_ptr + disc_offset;
    switch (disc_size) {
        0 => {},
        1 => buffer[dst] = @intCast(value),
        2 => writeIntLittle(u16, buffer, dst, @intCast(value)),
        4 => writeIntLittle(u32, buffer, dst, value),
        else => std.debug.panic(
            "wasm invariant violated: unsupported tag discriminant size {d}",
            .{disc_size},
        ),
    }
}

fn writeWasmInt(buffer: []u8, dst: usize, size: usize, value: u64) void {
    switch (size) {
        1 => buffer[dst] = @intCast(value),
        2 => writeIntLittle(u16, buffer, dst, @intCast(value)),
        4 => writeIntLittle(u32, buffer, dst, @intCast(value)),
        8 => writeIntLittle(u64, buffer, dst, value),
        else => std.debug.panic(
            "wasm invariant violated: unsupported integer write size {d}",
            .{size},
        ),
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

const WasmCrashState = enum {
    none,
    crashed,
};

var wasm_heap_ptr: u32 = 65536;
var wasm_allocation_count: u32 = 0;
var wasm_crash_state: WasmCrashState = .none;

const WasmRocOps = builtins.host_abi.RocOps;

fn wasmDecAlloc(_: *WasmRocOps, _: usize, _: usize) callconv(.c) ?*anyopaque {
    wasm_crash_state = .crashed;
    return null;
}

fn wasmDecDealloc(_: *WasmRocOps, _: *anyopaque, _: usize) callconv(.c) void {}

fn wasmDecRealloc(_: *WasmRocOps, _: *anyopaque, _: usize, _: usize) callconv(.c) ?*anyopaque {
    wasm_crash_state = .crashed;
    return null;
}

fn wasmDecDbg(_: *WasmRocOps, _: [*]const u8, _: usize) callconv(.c) void {}

fn wasmDecExpectFailed(_: *WasmRocOps, _: [*]const u8, _: usize) callconv(.c) void {
    wasm_crash_state = .crashed;
}

fn wasmDecCrashed(_: *WasmRocOps, _: [*]const u8, _: usize) callconv(.c) void {
    wasm_crash_state = .crashed;
}

var wasm_dec_roc_ops = WasmRocOps{
    .env = undefined,
    .roc_alloc = &wasmDecAlloc,
    .roc_dealloc = &wasmDecDealloc,
    .roc_realloc = &wasmDecRealloc,
    .roc_dbg = &wasmDecDbg,
    .roc_expect_failed = &wasmDecExpectFailed,
    .roc_crashed = &wasmDecCrashed,
    .hosted_fns = builtins.host_abi.emptyHostedFunctions(),
};

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
