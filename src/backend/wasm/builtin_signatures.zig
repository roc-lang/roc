//! Wasm-level signatures for builtin wrapper calls.
//!
//! The ABI is per wrapper. Codegen must push the exact params listed here and
//! then emit a relocation to the listed `roc_builtins_*` symbol.

const std = @import("std");
const Allocator = std.mem.Allocator;
const WasmModule = @import("WasmModule.zig");
const SymbolIndex = @import("index_types.zig").SymbolIndex;

/// Wasm value type used in builtin wrapper signatures.
pub const ValType = WasmModule.ValType;

/// Builtin wrapper known to wasm codegen.
pub const BuiltinKind = enum {
    dec_mul,
    dec_div,
    dec_div_trunc,
    dec_pow,
    dec_sqrt,
    dec_sin,
    dec_cos,
    dec_tan,
    dec_asin,
    dec_acos,
    dec_atan,
    dec_to_str,
    num_div_trunc_i128,
    num_rem_trunc_i128,
    num_div_trunc_u128,
    num_rem_trunc_u128,
    num_mod_i128,
    num_mul_with_overflow_i128,
    num_mul_with_overflow_u128,
    i128_to_dec_try_unsafe,
    u128_to_dec_try_unsafe,
    dec_to_int_try_unsafe,
    dec_to_f32_try_unsafe,
    f32_to_int_try_unsafe,
    f64_to_int_try_unsafe,
    dec_to_f32,
    dec_to_f64,
    i128_to_f32,
    i128_to_f64,
    u128_to_f32,
    u128_to_f64,
    float_to_str,
    float_pow_f32,
    float_pow,
    float_rem_f32,
    float_rem,
    float_sin_f32,
    float_sin,
    float_cos_f32,
    float_cos,
    float_tan_f32,
    float_tan,
    float_asin_f32,
    float_asin,
    float_acos_f32,
    float_acos,
    float_atan_f32,
    float_atan,
    int_to_str,
    int_from_str,
    dec_from_str,
    float_from_str,
    str_equal,
    str_split_first,
    str_concat,
    str_repeat,
    str_trim,
    str_trim_start,
    str_trim_end,
    str_split,
    str_join_with,
    str_reserve,
    str_release_excess_capacity,
    str_with_capacity,
    str_drop_prefix,
    str_drop_prefix_caseless_ascii,
    str_drop_suffix,
    str_with_ascii_lowercased,
    str_with_ascii_uppercased,
    str_caseless_ascii_equals,
    str_escape_and_quote,
    str_from_utf8,
    str_from_utf8_result,
    list_append_unsafe,
    list_concat,
    list_drop_at,
    list_reserve,
    list_replace,
    list_swap,
    list_eq,
    list_str_eq,
    list_list_eq,
    list_reverse,
    allocate_with_refcount,
    i8_mod_by,
    u8_mod_by,
    i16_mod_by,
    u16_mod_by,
    i32_mod_by,
    u32_mod_by,
    i64_mod_by,
    u64_mod_by,
    dict_pseudo_seed,
    hasher_finish,
    hasher_write_u64,
    hasher_write_u128,
    hasher_write_f32_bits,
    hasher_write_f64_bits,
    hasher_write_bytes,
    hasher_write_str,
    crypto_sha256_hash_bytes,
    crypto_sha256_hasher_empty,
    crypto_sha256_hasher_write,
    crypto_sha256_hasher_finish,
    crypto_blake3_hash_bytes,
    crypto_blake3_hasher_empty,
    crypto_blake3_hasher_write,
    crypto_blake3_hasher_finish,
};

/// The BuiltinKind for a registry member selected by the shared
/// LowLevel-to-builtin table; a member wasm codegen has no signature for
/// is a compile error.
pub fn kindOf(comptime f: builtin_registry.BuiltinFn) BuiltinKind {
    return @field(BuiltinKind, @tagName(f));
}

/// Wasm call signature and symbol name for a builtin wrapper.
pub const Sig = struct {
    name: []const u8,
    wasm_params: []const ValType,
    wasm_results: []const ValType,
    takes_roc_ops: bool,
};

const builtin_registry = @import("builtins").builtin_registry;
const dev_wrappers = @import("builtins").dev_wrappers;
const RocOps = @import("builtins").host_abi.RocOps;

/// Builtin signatures indexed by `BuiltinKind`, each derived at comptime from
/// the Zig wrapper it names.
///
/// A row's wasm ABI IS the wrapper's ABI: a wrong ValType, arity, result, or
/// `takes_roc_ops` flag is silent stack corruption at runtime, so every field is
/// read straight from `@typeInfo` of the wrapper instead of written by hand. Each
/// `BuiltinKind` resolves to the `builtin_registry.BuiltinFn` member of the same
/// name — a missing member is a compile error, pinning this enum to the registry —
/// and that member supplies the symbol name (`symbolName()`) and the wrapper type
/// the params, results, and `takes_roc_ops` flag are lowered from.
pub const sigs: [@typeInfo(BuiltinKind).@"enum".fields.len]Sig = blk: {
    @setEvalBranchQuota(200_000);
    var arr: [@typeInfo(BuiltinKind).@"enum".fields.len]Sig = undefined;
    for (std.enums.values(BuiltinKind), 0..) |kind, i| {
        arr[i] = deriveSig(kind);
    }
    const frozen = arr;
    break :blk frozen;
};

/// Derive the wasm signature for `kind` from the registry member and wrapper of
/// the same name.
fn deriveSig(comptime kind: BuiltinKind) Sig {
    const member = @field(builtin_registry.BuiltinFn, @tagName(kind));
    const name = member.symbolName();
    const fn_info = @typeInfo(@TypeOf(@field(dev_wrappers, name))).@"fn";

    const wasm_params: []const ValType = blk: {
        var params: [fn_info.params.len]ValType = undefined;
        for (fn_info.params, 0..) |param, i| params[i] = wasmValTypeOf(param.type.?);
        const frozen = params;
        break :blk &frozen;
    };

    const wasm_results: []const ValType = blk: {
        const ret = fn_info.return_type.?;
        if (ret == void) break :blk &.{};
        const frozen = [_]ValType{wasmValTypeOf(ret)};
        break :blk &frozen;
    };

    const takes_roc_ops = fn_info.params.len > 0 and blk: {
        const last = fn_info.params[fn_info.params.len - 1].type.?;
        break :blk @typeInfo(last) == .pointer and @typeInfo(last).pointer.child == RocOps;
    };

    return .{
        .name = name,
        .wasm_params = wasm_params,
        .wasm_results = wasm_results,
        .takes_roc_ops = takes_roc_ops,
    };
}

/// Return the builtin wrapper signature for `kind`.
pub fn sigOf(kind: BuiltinKind) Sig {
    return sigs[@intFromEnum(kind)];
}

/// Relocation symbol table indexed by builtin kind.
pub const SymbolTable = std.enums.EnumArray(BuiltinKind, SymbolIndex);

/// Declare every builtin wrapper as an undefined function symbol in a generated
/// relocatable wasm object.
pub fn declareUndefinedRelocs(module: *WasmModule) Allocator.Error!SymbolTable {
    var result = SymbolTable.initUndefined();
    inline for (std.meta.tags(BuiltinKind)) |kind| {
        const sig = sigOf(kind);
        const type_idx = try module.addFuncType(sig.wasm_params, sig.wasm_results);
        const imported = try module.addFunctionImportWithSymbol("env", sig.name, type_idx);
        result.set(kind, imported.symbol);
    }
    return result;
}

/// Locate builtin function symbols in a merged wasm module.
pub fn populateForRelocs(module: *const WasmModule) WasmModule.SymbolLookupError!SymbolTable {
    var result = SymbolTable.initUndefined();
    inline for (std.meta.tags(BuiltinKind)) |kind| {
        result.set(kind, try module.findDefinedFunctionSymbolExact(sigOf(kind).name));
    }
    return result;
}

/// The wasm32 `ValType` a single wrapper parameter or return type lowers to.
///
/// wasm32 lowering rules (each Zig scalar/pointer maps to exactly one ValType,
/// because the wrappers already decompose 128-bit values into two `u64`s and pass
/// `RocStr`/`RocList` by pointer):
/// - `usize`/`isize` and every pointer are 32-bit on wasm32 → `.i32`
/// - `bool`, enums, and integers up to 32 bits → `.i32`
/// - 64-bit integers → `.i64`
/// - `f32` → `.f32`, `f64` → `.f64`
///
/// Any other shape (a by-value aggregate, a >64-bit integer, a non-pointer
/// optional) is a compile error, so a newly added wrapper cannot silently bypass
/// verification.
fn wasmValTypeOf(comptime T: type) ValType {
    // On wasm32 `usize`/`isize` are 32-bit even though the verifier itself is
    // compiled for a 64-bit host (`usize` and `u64` are distinct types, so this
    // check does not also catch genuine `u64` params).
    if (T == usize or T == isize) return .i32;
    return switch (@typeInfo(T)) {
        .bool => .i32,
        .int => |info| switch (info.bits) {
            0...32 => .i32,
            33...64 => .i64,
            else => @compileError("builtin wrapper integer wider than 64 bits must be decomposed: " ++ @typeName(T)),
        },
        .float => |info| switch (info.bits) {
            32 => .f32,
            64 => .f64,
            else => @compileError("unsupported float width in builtin wrapper: " ++ @typeName(T)),
        },
        .pointer => .i32, // wasm32 pointer
        .optional => |o| if (@typeInfo(o.child) == .pointer) .i32 else @compileError("unsupported optional (non-pointer) builtin wrapper type: " ++ @typeName(T)),
        .@"enum" => |info| wasmValTypeOf(info.tag_type),
        else => @compileError("unsupported builtin wrapper type: " ++ @typeName(T)),
    };
}
