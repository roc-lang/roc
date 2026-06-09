//! Shared readonly data export records for native object emission.

const std = @import("std");
const lir = @import("lir");

/// Immutable data symbol to emit into the target's readonly data section.
pub const StaticDataExport = struct {
    /// The exported symbol name, for example `roc__answer`.
    symbol_name: []const u8,
    /// Fully materialized Roc ABI bytes for the constant.
    bytes: []const u8,
    /// Offset inside `bytes` where `symbol_name` points.
    symbol_offset: u32 = 0,
    /// Required alignment of the symbol inside the readonly section.
    alignment: u32,
    /// Whether the symbol should be visible to the host linker.
    is_global: bool = true,
    /// Pointer relocations from this symbol's bytes to other symbols.
    relocations: []const StaticDataRelocation = &.{},
};

/// One pointer relocation inside a readonly static-data symbol.
pub const StaticDataRelocation = struct {
    pub const Kind = enum {
        address,
        function_pointer,
    };

    /// Byte offset inside `StaticDataExport.bytes` where the pointer is stored.
    offset: u64,
    /// Symbol whose address should be written at `offset`.
    target_symbol_name: []const u8,
    /// Addend applied to the target symbol address.
    addend: i64 = 0,
    /// Runtime meaning of the stored pointer.
    kind: Kind = .address,
    /// Whether `target_symbol_name` is owned by this relocation and must be freed
    /// with the static data graph.
    owns_target_symbol_name: bool = false,
};

/// Deterministic object-file symbol name for an internal LIR procedure.
///
/// These symbols are local text symbols. They exist so readonly data can point at
/// erased-callable wrappers using ordinary object relocations instead of backend
/// code-buffer offsets.
pub fn procSymbolName(allocator: std.mem.Allocator, proc_symbol: lir.Symbol) std.mem.Allocator.Error![]u8 {
    return try std.fmt.allocPrint(allocator, "roc__proc_{x}", .{proc_symbol.raw()});
}
