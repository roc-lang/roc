//! Provides import mapping functionality for type display names in error messages.
//! Maps fully-qualified type identifiers to their display names (e.g., "Builtin.Bool" to "Bool").
//!
//! This module provides semantic type name shortening - instead of using string manipulation
//! like `lastIndexOf(".")` to strip prefixes, we look up the shortest imported alias for a type.
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

/// Get the shortest display name for a fully-qualified type identifier.
///
/// If the type is found in the import mapping (meaning the user has imported it
/// or it's an auto-imported builtin), returns the mapped display name.
/// Otherwise, returns the original identifier text unchanged.
///
/// This should be used instead of string manipulation like `lastIndexOf(".")`
/// to ensure type names are displayed semantically based on what's in scope.
///
/// Example:
/// - "Builtin.Num.Dec" → "Dec" (if Dec is auto-imported)
/// - "Builtin.Bool" → "Bool" (if Bool is auto-imported)
/// - "MyModule.Foo" → "MyModule.Foo" (if not imported, stays fully-qualified)
/// - "MyModule.Foo" → "Foo" (if user has `import MyModule exposing [Foo]`)
/// - "MyModule.Foo" → "F" (if user has `import MyModule exposing [Foo as F]`)
pub fn getDisplayName(
    mapping: *const ImportMapping,
    idents: *const Ident.Store,
    qualified_ident: Ident.Idx,
) []const u8 {
    if (mapping.get(qualified_ident)) |display_ident| {
        return idents.getText(display_ident);
    }
    return idents.getText(qualified_ident);
}
