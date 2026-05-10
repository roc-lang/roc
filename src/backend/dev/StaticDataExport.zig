//! Shared readonly data export records for native object emission.

/// Immutable data symbol to emit into the target's readonly data section.
pub const StaticDataExport = struct {
    /// The exported symbol name, for example `roc__answer`.
    symbol_name: []const u8,
    /// Fully materialized Roc ABI bytes for the constant.
    bytes: []const u8,
    /// Required alignment of the symbol inside the readonly section.
    alignment: u32,
    /// Whether the symbol should be visible to the host linker.
    is_global: bool = true,
    /// Pointer relocations from this symbol's bytes to other symbols.
    relocations: []const StaticDataRelocation = &.{},
};

/// One pointer relocation inside a readonly static-data symbol.
pub const StaticDataRelocation = struct {
    /// Byte offset inside `StaticDataExport.bytes` where the pointer is stored.
    offset: u64,
    /// Symbol whose address should be written at `offset`.
    target_symbol_name: []const u8,
    /// Addend applied to the target symbol address.
    addend: i64 = 0,
};
