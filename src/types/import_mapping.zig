//! Provides import mapping functionality for type display names in error messages.
//! Maps fully-qualified type identifiers to their display names (e.g., "Builtin.Bool" to "Bool").

const std = @import("std");
const base = @import("base");
const Ident = base.Ident;

/// Mapping from fully-qualified type identifiers to their display names.
/// This allows error messages to show "Bool" instead of "Builtin.Bool" for auto-imported types,
/// and handles other import scenarios consistently.
pub const ImportMapping = std.AutoHashMap(Ident.Idx, Ident.Idx);
