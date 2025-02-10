//! Indexes of a specific identifier and a specific module.
const Module = @import("Module.zig");
const Ident = @import("Ident.zig");

/// The module index.
module_idx: Module.Idx,

/// The identifier index.
ident_idx: Ident.Idx,
