//! Backend compatibility aliases for shared static-data records.

const static_data = @import("static_data");

/// Shared immutable data-symbol record.
pub const StaticDataExport = static_data.StaticDataExport;
/// Shared explicit static-data relocation record.
pub const StaticDataRelocation = static_data.StaticDataRelocation;
/// Shared deterministic LIR procedure-symbol formatter.
pub const procSymbolName = static_data.procSymbolName;
