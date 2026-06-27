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
    dec_to_str,
    i128_div_s,
    i128_mod_s,
    u128_div,
    u128_mod,
    i128_to_dec,
    u128_to_dec,
    dec_to_int_try_unsafe,
    dec_to_f32,
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
    name: []const u8,
    wasm_params: []const ValType,
    wasm_results: []const ValType,
    takes_roc_ops: bool,
};

/// Builtin signatures indexed by `BuiltinKind`.
pub const sigs: [@typeInfo(BuiltinKind).@"enum".fields.len]Sig = .{
    .{ .name = "roc_builtins_dec_mul", .wasm_params = &.{ .i32, .i32, .i64, .i64, .i64, .i64, .i32 }, .wasm_results = &.{}, .takes_roc_ops = true },
    .{ .name = "roc_builtins_dec_div", .wasm_params = &.{ .i32, .i32, .i64, .i64, .i64, .i64, .i32 }, .wasm_results = &.{}, .takes_roc_ops = true },
    .{ .name = "roc_builtins_dec_div_trunc", .wasm_params = &.{ .i32, .i32, .i64, .i64, .i64, .i64, .i32 }, .wasm_results = &.{}, .takes_roc_ops = true },
    .{ .name = "roc_builtins_dec_to_str", .wasm_params = &.{ .i32, .i64, .i64, .i32 }, .wasm_results = &.{}, .takes_roc_ops = true },
    .{ .name = "roc_builtins_num_div_trunc_i128", .wasm_params = &.{ .i32, .i32, .i64, .i64, .i64, .i64, .i32 }, .wasm_results = &.{}, .takes_roc_ops = true },
    .{ .name = "roc_builtins_num_rem_trunc_i128", .wasm_params = &.{ .i32, .i32, .i64, .i64, .i64, .i64, .i32 }, .wasm_results = &.{}, .takes_roc_ops = true },
    .{ .name = "roc_builtins_num_div_trunc_u128", .wasm_params = &.{ .i32, .i32, .i64, .i64, .i64, .i64, .i32 }, .wasm_results = &.{}, .takes_roc_ops = true },
    .{ .name = "roc_builtins_num_rem_trunc_u128", .wasm_params = &.{ .i32, .i32, .i64, .i64, .i64, .i64, .i32 }, .wasm_results = &.{}, .takes_roc_ops = true },
    .{ .name = "roc_builtins_i128_to_dec_try_unsafe", .wasm_params = &.{ .i32, .i64, .i64, .i32, .i32 }, .wasm_results = &.{}, .takes_roc_ops = false },
    .{ .name = "roc_builtins_u128_to_dec_try_unsafe", .wasm_params = &.{ .i32, .i64, .i64, .i32, .i32 }, .wasm_results = &.{}, .takes_roc_ops = false },
    .{ .name = "roc_builtins_dec_to_int_try_unsafe", .wasm_params = &.{ .i32, .i64, .i64, .i32, .i32, .i32, .i32, .i32 }, .wasm_results = &.{}, .takes_roc_ops = false },
    .{ .name = "roc_builtins_dec_to_f32_try_unsafe", .wasm_params = &.{ .i32, .i64, .i64, .i32, .i32 }, .wasm_results = &.{}, .takes_roc_ops = false },
    .{ .name = "roc_builtins_float_to_str", .wasm_params = &.{ .i32, .i64, .i32, .i32 }, .wasm_results = &.{}, .takes_roc_ops = true },
    .{ .name = "roc_builtins_float_pow", .wasm_params = &.{ .f64, .f64, .i32 }, .wasm_results = &.{.f64}, .takes_roc_ops = false },
    .{ .name = "roc_builtins_float_sin", .wasm_params = &.{ .f64, .i32 }, .wasm_results = &.{.f64}, .takes_roc_ops = false },
    .{ .name = "roc_builtins_float_cos", .wasm_params = &.{ .f64, .i32 }, .wasm_results = &.{.f64}, .takes_roc_ops = false },
    .{ .name = "roc_builtins_float_tan", .wasm_params = &.{ .f64, .i32 }, .wasm_results = &.{.f64}, .takes_roc_ops = false },
    .{ .name = "roc_builtins_float_asin", .wasm_params = &.{ .f64, .i32 }, .wasm_results = &.{.f64}, .takes_roc_ops = false },
    .{ .name = "roc_builtins_float_acos", .wasm_params = &.{ .f64, .i32 }, .wasm_results = &.{.f64}, .takes_roc_ops = false },
    .{ .name = "roc_builtins_float_atan", .wasm_params = &.{ .f64, .i32 }, .wasm_results = &.{.f64}, .takes_roc_ops = false },
    .{ .name = "roc_builtins_int_to_str", .wasm_params = &.{ .i32, .i64, .i64, .i32, .i32, .i32 }, .wasm_results = &.{}, .takes_roc_ops = true },
    .{ .name = "roc_builtins_int_from_str", .wasm_params = &.{ .i32, .i32, .i32, .i32, .i32, .i32, .i32 }, .wasm_results = &.{}, .takes_roc_ops = false },
    .{ .name = "roc_builtins_dec_from_str", .wasm_params = &.{ .i32, .i32, .i32, .i32, .i32 }, .wasm_results = &.{}, .takes_roc_ops = false },
    .{ .name = "roc_builtins_float_from_str", .wasm_params = &.{ .i32, .i32, .i32, .i32, .i32, .i32 }, .wasm_results = &.{}, .takes_roc_ops = false },
    .{ .name = "roc_builtins_str_equal", .wasm_params = &.{ .i32, .i32, .i32, .i32, .i32, .i32 }, .wasm_results = &.{.i32}, .takes_roc_ops = false },
    .{ .name = "roc_builtins_str_concat", .wasm_params = &.{ .i32, .i32, .i32, .i32, .i32, .i32, .i32, .i32, .i32 }, .wasm_results = &.{}, .takes_roc_ops = true },
    .{ .name = "roc_builtins_str_repeat", .wasm_params = &.{ .i32, .i32, .i32, .i32, .i64, .i32 }, .wasm_results = &.{}, .takes_roc_ops = true },
    .{ .name = "roc_builtins_str_trim", .wasm_params = &.{ .i32, .i32, .i32, .i32, .i32, .i32 }, .wasm_results = &.{}, .takes_roc_ops = true },
    .{ .name = "roc_builtins_str_trim_start", .wasm_params = &.{ .i32, .i32, .i32, .i32, .i32, .i32 }, .wasm_results = &.{}, .takes_roc_ops = true },
    .{ .name = "roc_builtins_str_trim_end", .wasm_params = &.{ .i32, .i32, .i32, .i32, .i32, .i32 }, .wasm_results = &.{}, .takes_roc_ops = true },
    .{ .name = "roc_builtins_str_split", .wasm_params = &.{ .i32, .i32, .i32, .i32, .i32, .i32, .i32, .i32 }, .wasm_results = &.{}, .takes_roc_ops = true },
    .{ .name = "roc_builtins_str_join_with", .wasm_params = &.{ .i32, .i32, .i32, .i32, .i32, .i32, .i32, .i32 }, .wasm_results = &.{}, .takes_roc_ops = true },
    .{ .name = "roc_builtins_str_reserve", .wasm_params = &.{ .i32, .i32, .i32, .i32, .i64, .i32, .i32 }, .wasm_results = &.{}, .takes_roc_ops = true },
    .{ .name = "roc_builtins_str_release_excess_capacity", .wasm_params = &.{ .i32, .i32, .i32, .i32, .i32, .i32 }, .wasm_results = &.{}, .takes_roc_ops = true },
    .{ .name = "roc_builtins_str_with_capacity", .wasm_params = &.{ .i32, .i64, .i32 }, .wasm_results = &.{}, .takes_roc_ops = true },
    .{ .name = "roc_builtins_str_drop_prefix", .wasm_params = &.{ .i32, .i32, .i32, .i32, .i32, .i32, .i32, .i32 }, .wasm_results = &.{}, .takes_roc_ops = true },
    .{ .name = "roc_builtins_str_drop_prefix_caseless_ascii", .wasm_params = &.{ .i32, .i32, .i32, .i32, .i32, .i32, .i32, .i32, .i32 }, .wasm_results = &.{}, .takes_roc_ops = true },
    .{ .name = "roc_builtins_str_drop_suffix", .wasm_params = &.{ .i32, .i32, .i32, .i32, .i32, .i32, .i32, .i32 }, .wasm_results = &.{}, .takes_roc_ops = true },
    .{ .name = "roc_builtins_str_with_ascii_lowercased", .wasm_params = &.{ .i32, .i32, .i32, .i32, .i32, .i32 }, .wasm_results = &.{}, .takes_roc_ops = true },
    .{ .name = "roc_builtins_str_with_ascii_uppercased", .wasm_params = &.{ .i32, .i32, .i32, .i32, .i32, .i32 }, .wasm_results = &.{}, .takes_roc_ops = true },
    .{ .name = "roc_builtins_str_caseless_ascii_equals", .wasm_params = &.{ .i32, .i32, .i32, .i32, .i32, .i32 }, .wasm_results = &.{.i32}, .takes_roc_ops = false },
    .{ .name = "roc_builtins_str_escape_and_quote", .wasm_params = &.{ .i32, .i32, .i32, .i32, .i32 }, .wasm_results = &.{}, .takes_roc_ops = true },
    .{ .name = "roc_builtins_str_from_utf8", .wasm_params = &.{ .i32, .i32, .i32, .i32, .i32 }, .wasm_results = &.{}, .takes_roc_ops = true },
    .{ .name = "roc_builtins_list_append_unsafe", .wasm_params = &.{ .i32, .i32, .i32, .i32, .i32, .i32, .i32 }, .wasm_results = &.{}, .takes_roc_ops = true },
    .{ .name = "roc_builtins_list_concat", .wasm_params = &.{ .i32, .i32, .i32, .i32, .i32, .i32, .i32, .i32, .i32, .i32, .i32, .i32, .i64, .i32 }, .wasm_results = &.{}, .takes_roc_ops = true },
    .{ .name = "roc_builtins_list_drop_at", .wasm_params = &.{ .i32, .i32, .i32, .i32, .i32, .i32, .i64, .i32, .i32, .i32, .i32, .i32 }, .wasm_results = &.{}, .takes_roc_ops = true },
    .{ .name = "roc_builtins_list_reserve", .wasm_params = &.{ .i32, .i32, .i32, .i32, .i32, .i64, .i32, .i32, .i32, .i32, .i32, .i32 }, .wasm_results = &.{}, .takes_roc_ops = true },
    .{ .name = "roc_builtins_list_replace", .wasm_params = &.{ .i32, .i32, .i32, .i32, .i32, .i64, .i32, .i32, .i32, .i32, .i32, .i32, .i32, .i32 }, .wasm_results = &.{}, .takes_roc_ops = true },
    .{ .name = "roc_builtins_list_swap", .wasm_params = &.{ .i32, .i32, .i32, .i32, .i32, .i32, .i64, .i64, .i32, .i32, .i32, .i32, .i32 }, .wasm_results = &.{}, .takes_roc_ops = true },
    .{ .name = "roc_builtins_list_eq", .wasm_params = &.{ .i32, .i32, .i32, .i32, .i32, .i32, .i32 }, .wasm_results = &.{.i32}, .takes_roc_ops = false },
    .{ .name = "roc_builtins_list_str_eq", .wasm_params = &.{ .i32, .i32, .i32, .i32, .i32, .i32 }, .wasm_results = &.{.i32}, .takes_roc_ops = false },
    .{ .name = "roc_builtins_list_list_eq", .wasm_params = &.{ .i32, .i32, .i32, .i32, .i32, .i32, .i32 }, .wasm_results = &.{.i32}, .takes_roc_ops = false },
    .{ .name = "roc_builtins_list_reverse", .wasm_params = &.{ .i32, .i32, .i32, .i32, .i32, .i32, .i32, .i32, .i32, .i32, .i32 }, .wasm_results = &.{}, .takes_roc_ops = true },
    .{ .name = "roc_builtins_allocate_with_refcount", .wasm_params = &.{ .i32, .i32, .i32, .i32 }, .wasm_results = &.{.i32}, .takes_roc_ops = true },
    .{ .name = "roc_builtins_i8_mod_by", .wasm_params = &.{ .i32, .i32 }, .wasm_results = &.{.i32}, .takes_roc_ops = false },
    .{ .name = "roc_builtins_u8_mod_by", .wasm_params = &.{ .i32, .i32 }, .wasm_results = &.{.i32}, .takes_roc_ops = false },
    .{ .name = "roc_builtins_i16_mod_by", .wasm_params = &.{ .i32, .i32 }, .wasm_results = &.{.i32}, .takes_roc_ops = false },
    .{ .name = "roc_builtins_u16_mod_by", .wasm_params = &.{ .i32, .i32 }, .wasm_results = &.{.i32}, .takes_roc_ops = false },
    .{ .name = "roc_builtins_i32_mod_by", .wasm_params = &.{ .i32, .i32 }, .wasm_results = &.{.i32}, .takes_roc_ops = false },
    .{ .name = "roc_builtins_u32_mod_by", .wasm_params = &.{ .i32, .i32 }, .wasm_results = &.{.i32}, .takes_roc_ops = false },
    .{ .name = "roc_builtins_i64_mod_by", .wasm_params = &.{ .i64, .i64 }, .wasm_results = &.{.i64}, .takes_roc_ops = false },
    .{ .name = "roc_builtins_u64_mod_by", .wasm_params = &.{ .i64, .i64 }, .wasm_results = &.{.i64}, .takes_roc_ops = false },
    .{ .name = "roc_builtins_dict_pseudo_seed", .wasm_params = &.{}, .wasm_results = &.{.i64}, .takes_roc_ops = false },
    .{ .name = "roc_builtins_hasher_finish", .wasm_params = &.{.i64}, .wasm_results = &.{.i64}, .takes_roc_ops = false },
    .{ .name = "roc_builtins_hasher_write_u64", .wasm_params = &.{ .i64, .i32, .i64, .i32 }, .wasm_results = &.{.i64}, .takes_roc_ops = false },
    .{ .name = "roc_builtins_hasher_write_u128", .wasm_params = &.{ .i64, .i32, .i64, .i64 }, .wasm_results = &.{.i64}, .takes_roc_ops = false },
    .{ .name = "roc_builtins_hasher_write_f32_bits", .wasm_params = &.{ .i64, .i64 }, .wasm_results = &.{.i64}, .takes_roc_ops = false },
    .{ .name = "roc_builtins_hasher_write_f64_bits", .wasm_params = &.{ .i64, .i64 }, .wasm_results = &.{.i64}, .takes_roc_ops = false },
    .{ .name = "roc_builtins_hasher_write_bytes", .wasm_params = &.{ .i64, .i32, .i32, .i32 }, .wasm_results = &.{.i64}, .takes_roc_ops = false },
    .{ .name = "roc_builtins_hasher_write_str", .wasm_params = &.{ .i64, .i32, .i32, .i32 }, .wasm_results = &.{.i64}, .takes_roc_ops = false },
    .{ .name = "roc_builtins_crypto_sha256_hash_bytes", .wasm_params = &.{ .i32, .i32, .i32, .i32, .i32 }, .wasm_results = &.{}, .takes_roc_ops = true },
    .{ .name = "roc_builtins_crypto_sha256_hasher_empty", .wasm_params = &.{ .i32, .i32 }, .wasm_results = &.{}, .takes_roc_ops = true },
    .{ .name = "roc_builtins_crypto_sha256_hasher_write", .wasm_params = &.{ .i32, .i32, .i32, .i32, .i32, .i32, .i32, .i32 }, .wasm_results = &.{}, .takes_roc_ops = true },
    .{ .name = "roc_builtins_crypto_sha256_hasher_finish", .wasm_params = &.{ .i32, .i32, .i32, .i32, .i32 }, .wasm_results = &.{}, .takes_roc_ops = true },
    .{ .name = "roc_builtins_crypto_blake3_hash_bytes", .wasm_params = &.{ .i32, .i32, .i32, .i32, .i32 }, .wasm_results = &.{}, .takes_roc_ops = true },
    .{ .name = "roc_builtins_crypto_blake3_hasher_empty", .wasm_params = &.{ .i32, .i32 }, .wasm_results = &.{}, .takes_roc_ops = true },
    .{ .name = "roc_builtins_crypto_blake3_hasher_write", .wasm_params = &.{ .i32, .i32, .i32, .i32, .i32, .i32, .i32, .i32 }, .wasm_results = &.{}, .takes_roc_ops = true },
    .{ .name = "roc_builtins_crypto_blake3_hasher_finish", .wasm_params = &.{ .i32, .i32, .i32, .i32, .i32 }, .wasm_results = &.{}, .takes_roc_ops = true },
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

comptime {
    const dw = @import("builtins").dev_wrappers;
    for (sigs) |sig| {
        if (!@hasDecl(dw, sig.name)) @compileError("missing dev wrapper: " ++ sig.name);
        const wrapper_type = @typeInfo(@TypeOf(@field(dw, sig.name))).@"fn";
        if (wrapper_type.params.len != sig.wasm_params.len) {
            @compileError("builtin arity mismatch: " ++ sig.name);
        }
    }
}
