//! Wasm-level signatures of the Boxy runtime's `roc_desc_*` entry points
//! (design.md "Runtime"), which a `.boxy` program's module merges from the
//! wasm32 Boxy runtime object.
//!
//! Every entry point, its symbol name, and its wasm parameter and result
//! types are read from `boxy_runtime.exports` at comptime, so codegen and the
//! runtime cannot disagree on an entry point's ABI.

const std = @import("std");
const WasmModule = @import("WasmModule.zig");
const SymbolIndex = @import("index_types.zig").SymbolIndex;
const BuiltinSignatures = @import("builtin_signatures.zig");
const exports = @import("boxy_runtime").exports;

const ValType = WasmModule.ValType;

const prefix = "roc_desc_";

/// One member per `roc_desc_*` entry point, named without the prefix.
pub const Entry = blk: {
    @setEvalBranchQuota(20_000);
    const decls = @typeInfo(exports).@"struct".decl_names;
    var names: [decls.len][:0]const u8 = undefined;
    var count: usize = 0;
    for (decls) |name| {
        if (name.len <= prefix.len or !std.mem.eql(u8, name[0..prefix.len], prefix)) continue;
        names[count] = name[prefix.len..];
        count += 1;
    }
    const Tag = std.math.IntFittingRange(0, count - 1);
    const frozen = names[0..count].*;
    break :blk @Enum(Tag, .exhaustive, &frozen, &std.simd.iota(Tag, count));
};

/// An entry point's symbol name and wasm32 ABI.
pub const Sig = struct {
    name: []const u8,
    params: []const ValType,
    results: []const ValType,
};

fn deriveSig(comptime entry: Entry) Sig {
    const name = prefix ++ @tagName(entry);
    const info = @typeInfo(@TypeOf(@field(exports, name))).@"fn";
    const params: []const ValType = blk: {
        var types: [info.param_types.len]ValType = undefined;
        for (info.param_types, 0..) |param, i| types[i] = BuiltinSignatures.wasmValTypeOf(param.?);
        const frozen = types;
        break :blk &frozen;
    };
    const results: []const ValType = blk: {
        const ret = info.return_type.?;
        if (ret == void) break :blk &.{};
        const frozen = [_]ValType{BuiltinSignatures.wasmValTypeOf(ret)};
        break :blk &frozen;
    };
    return .{ .name = name, .params = params, .results = results };
}

const sigs: [@typeInfo(Entry).@"enum".field_names.len]Sig = blk: {
    @setEvalBranchQuota(200_000);
    var all: [@typeInfo(Entry).@"enum".field_names.len]Sig = undefined;
    for (std.enums.values(Entry), 0..) |entry, i| all[i] = deriveSig(entry);
    const frozen = all;
    break :blk frozen;
};

/// The signature of `entry`.
pub fn sigOf(entry: Entry) Sig {
    return sigs[@backingInt(entry)];
}

/// Relocation symbol of each entry point in a merged module.
pub const SymbolTable = std.enums.EnumArray(Entry, SymbolIndex);

/// Locate every entry point's function symbol in a module the Boxy runtime
/// object has been merged into.
pub fn populateForRelocs(module: *const WasmModule) WasmModule.SymbolLookupError!SymbolTable {
    var result = SymbolTable.initUndefined();
    inline for (comptime std.enums.values(Entry)) |entry| {
        result.set(entry, try module.findDefinedFunctionSymbolExact(sigOf(entry).name));
    }
    return result;
}
