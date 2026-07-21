//! Backend compatibility aliases for shared static-data records.

const static_data = @import("static_data");

/// Shared immutable data-symbol record.
pub const StaticDataExport = static_data.StaticDataExport;
/// Shared explicit static-data relocation record.
pub const StaticDataRelocation = static_data.StaticDataRelocation;
/// Shared deterministic generated RC-helper symbol formatter.
pub const atomicRcHelperSymbolName = static_data.atomicRcHelperSymbolName;
/// Shared collector for explicit static-data RC-helper requirements.
pub const collectRequiredRcHelpers = static_data.collectRequiredRcHelpers;
/// Shared deterministic LIR procedure-symbol formatter.
pub const procSymbolName = static_data.procSymbolName;
