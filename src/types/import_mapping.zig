//! Provides import mapping functionality for type display names in error messages.
//! Maps fully-qualified type identifiers to their display names (e.g., "Builtin.Bool" to "Bool").
//!
//! This module provides semantic type name shortening - instead of using string manipulation
//! like `findLast(".")` to strip prefixes, we look up the shortest imported alias for a type.
//! This ensures that error messages show types the way the user would write them in their code.

const std = @import("std");
const base = @import("base");
const Ident = base.Ident;

/// Mapping from fully-qualified type identifiers to their display names.
/// This allows error messages to show "Bool" instead of "Builtin.Bool" for auto-imported types,
/// and handles other import scenarios consistently.
///
/// The mapping is built during type checking by examining:
/// 1. Auto-imported builtin types (e.g., Bool, Str, Dec, U64, etc.)
/// 2. User import statements with their aliases
///
/// When multiple imports could refer to the same type, the shortest name wins.
pub const ImportMapping = std.AutoHashMap(Ident.Idx, Ident.Idx);
