//! Wasm-level signatures for builtin wrapper calls.
//!
//! The ABI is per wrapper. Codegen must push the exact params listed here and
//! then emit a relocation to the listed `roc_builtins_*` symbol.

const std = @import("std");
const WasmModule = @import("WasmModule.zig");
const SymbolIndex = @import("index_types.zig").SymbolIndex;

/// Wasm value type used in builtin wrapper signatures.
pub const ValType = WasmModule.ValType;

const builtin_registry = @import("builtins").builtin_registry;
const dev_wrappers = @import("builtins").dev_wrappers;
const RocOps = @import("builtins").host_abi.RocOps;

/// Builtin wrapper known to wasm codegen: one member, of the same name, per
/// `builtin_registry.BuiltinFn` member the linkable builtins payloads export,
/// in registry order.
pub const BuiltinKind = blk: {
    @setEvalBranchQuota(10_000);
    const all = std.enums.values(builtin_registry.BuiltinFn);
    var names: [all.len][:0]const u8 = undefined;
    var count: usize = 0;
    for (all) |f| {
        names[count] = @tagName(f);
        count += 1;
    }
    const Tag = std.math.IntFittingRange(0, count - 1);
    const frozen = names[0..count].*;
    break :blk @Enum(Tag, .exhaustive, &frozen, &std.simd.iota(Tag, count));
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

/// Builtin signatures indexed by `BuiltinKind`, each derived at comptime from
/// the Zig wrapper it names.
///
/// A row's wasm ABI IS the wrapper's ABI: a wrong ValType, arity, result, or
/// `takes_roc_ops` flag is silent stack corruption at runtime, so every field is
/// read straight from `@typeInfo` of the wrapper instead of written by hand. Each
/// `BuiltinKind` resolves to the `builtin_registry.BuiltinFn` member of the same
/// name, which supplies the symbol name (`symbolName()`) and the wrapper type
/// the params, results, and `takes_roc_ops` flag are lowered from.
pub const sigs: [@typeInfo(BuiltinKind).@"enum".field_names.len]Sig = blk: {
    @setEvalBranchQuota(200_000);
    var arr: [@typeInfo(BuiltinKind).@"enum".field_names.len]Sig = undefined;
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
        var params: [fn_info.param_types.len]ValType = undefined;
        for (fn_info.param_types, 0..) |param, i| params[i] = wasmValTypeOf(param.?);
        const frozen = params;
        break :blk &frozen;
    };

    const wasm_results: []const ValType = blk: {
        const ret = fn_info.return_type.?;
        if (ret == void) break :blk &.{};
        const frozen = [_]ValType{wasmValTypeOf(ret)};
        break :blk &frozen;
    };

    const takes_roc_ops = fn_info.param_types.len > 0 and blk: {
        const last = fn_info.param_types[fn_info.param_types.len - 1].?;
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
    return sigs[@backingInt(kind)];
}

/// Relocation symbol table indexed by builtin kind.
pub const SymbolTable = std.enums.EnumArray(BuiltinKind, SymbolIndex);

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
        .type,
        .void,
        .noreturn,
        .array,
        .@"struct",
        .comptime_float,
        .comptime_int,
        .undefined,
        .null,
        .error_union,
        .error_set,
        .@"union",
        .@"fn",
        .@"opaque",
        .frame,
        .@"anyframe",
        .vector,
        .enum_literal,
        .spirv,
        => @compileError("unsupported builtin wrapper type: " ++ @typeName(T)),
    };
}
