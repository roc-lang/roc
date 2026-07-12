//! Wasm-level signatures for builtin wrapper calls.
//!
//! Every signature is derived at comptime from the wrapper function types in
//! `builtins.dev_wrappers` via the builtin registry: the wrapper's Zig
//! parameter and return types ARE the ABI, so a hand-written row cannot
//! drift from it. Codegen must push exactly `wasm_params` and then emit a
//! relocation to the `name` symbol.

const std = @import("std");
const Allocator = std.mem.Allocator;
const WasmModule = @import("WasmModule.zig");
const SymbolIndex = @import("index_types.zig").SymbolIndex;
const builtins = @import("builtins");
const registry = builtins.builtin_registry;
const RocOps = builtins.host_abi.RocOps;

/// Wasm value type used in builtin wrapper signatures.
pub const ValType = WasmModule.ValType;

/// Builtin wrapper known to wasm codegen: the subset of the registry the
/// wasm backend calls. Member names must match `registry.BuiltinFn` members;
/// a name with no registry counterpart is a compile error (see `sigs`).
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
    float_to_str,
    float_pow,
    float_sin,
    float_cos,
    float_tan,
    float_asin,
    float_acos,
    float_atan,
    int_to_str,
    int_from_str,
    dec_from_str,
    float_from_str,
    str_equal,
    str_find_first,
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

/// Wasm call signature and symbol name for a builtin wrapper.
pub const Sig = struct {
    name: [:0]const u8,
    wasm_params: []const ValType,
    wasm_results: []const ValType,
    takes_roc_ops: bool,
};

/// Derive a wrapper's wasm-level call signature from its Zig function type.
fn deriveSig(comptime b: registry.BuiltinFn) Sig {
    const fn_info = @typeInfo(b.WrapperType()).@"fn";

    var params: [fn_info.params.len]ValType = undefined;
    for (fn_info.params, 0..) |param, i| {
        params[i] = wasmValTypeOf(param.type.?);
    }
    const params_final = params;

    const ret = fn_info.return_type.?;
    const results: []const ValType = if (ret == void) &.{} else &.{wasmValTypeOf(ret)};

    const takes_roc_ops = fn_info.params.len > 0 and blk: {
        const last = fn_info.params[fn_info.params.len - 1].type.?;
        break :blk @typeInfo(last) == .pointer and @typeInfo(last).pointer.child == RocOps;
    };

    return .{
        .name = b.symbolName(),
        .wasm_params = &params_final,
        .wasm_results = results,
        .takes_roc_ops = takes_roc_ops,
    };
}

/// Builtin signatures indexed by `BuiltinKind`, derived from the registry.
pub const sigs: [@typeInfo(BuiltinKind).@"enum".fields.len]Sig = blk: {
    @setEvalBranchQuota(100_000);
    var result: [@typeInfo(BuiltinKind).@"enum".fields.len]Sig = undefined;
    for (@typeInfo(BuiltinKind).@"enum".fields) |field| {
        // Name-matching is the kind->registry mapping; @field fails to
        // compile if a BuiltinKind member has no registry counterpart.
        const b = @field(registry.BuiltinFn, field.name);
        result[field.value] = deriveSig(b);
    }
    break :blk result;
};

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
/// optional) is a compile error, so a newly added wrapper cannot silently
/// receive a wrong derived signature.
fn wasmValTypeOf(comptime T: type) ValType {
    // On wasm32 `usize`/`isize` are 32-bit even though the signatures are
    // derived while compiling for a 64-bit host (`usize` and `u64` are
    // distinct types, so this check does not also catch genuine `u64` params).
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
