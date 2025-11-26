//! Provides import mapping functionality for type display names in error messages.
//! Maps fully-qualified type identifiers to their display names (e.g., "Builtin.Bool" to "Bool").

const std = @import("std");
const base = @import("base");
const Ident = base.Ident;
const Allocator = std.mem.Allocator;

/// Mapping from fully-qualified type identifiers to their display names.
/// This allows error messages to show "Bool" instead of "Builtin.Bool" for auto-imported types,
/// and handles other import scenarios consistently.
pub const ImportMapping = std.AutoHashMap(Ident.Idx, Ident.Idx);

/// Create the import mapping for auto-imported builtin types.
/// This maps qualified names like "Builtin.Bool" to unqualified display names like "Bool".
///
/// Note: This function unconditionally creates mappings for auto-imported types by inserting
/// both the qualified and unqualified names into the ident store if they don't already exist.
/// This ensures the mapping works even if the Builtin module types haven't been referenced yet.
pub fn createImportMapping(
    gpa: Allocator,
    idents: *Ident.Store,
) Allocator.Error!ImportMapping {
    var mapping = ImportMapping.init(gpa);
    errdefer mapping.deinit();

    // Map auto-imported builtin types to their user-visible names
    // These mappings are created unconditionally since these types are always auto-imported
    try addMapping(&mapping, gpa, idents, "Builtin.Str", "Str");
    try addMapping(&mapping, gpa, idents, "Builtin.Bool", "Bool");
    try addMapping(&mapping, gpa, idents, "Builtin.Try", "Try");
    try addMapping(&mapping, gpa, idents, "Builtin.Dict", "Dict");
    try addMapping(&mapping, gpa, idents, "Builtin.Set", "Set");
    try addMapping(&mapping, gpa, idents, "Builtin.List", "List");

    // Numeric types from Builtin.Num module
    try addMapping(&mapping, gpa, idents, "Builtin.Num.Numeral", "Numeral");
    try addMapping(&mapping, gpa, idents, "Builtin.Num.U8", "U8");
    try addMapping(&mapping, gpa, idents, "Builtin.Num.I8", "I8");
    try addMapping(&mapping, gpa, idents, "Builtin.Num.U16", "U16");
    try addMapping(&mapping, gpa, idents, "Builtin.Num.I16", "I16");
    try addMapping(&mapping, gpa, idents, "Builtin.Num.U32", "U32");
    try addMapping(&mapping, gpa, idents, "Builtin.Num.I32", "I32");
    try addMapping(&mapping, gpa, idents, "Builtin.Num.U64", "U64");
    try addMapping(&mapping, gpa, idents, "Builtin.Num.I64", "I64");
    try addMapping(&mapping, gpa, idents, "Builtin.Num.U128", "U128");
    try addMapping(&mapping, gpa, idents, "Builtin.Num.I128", "I128");
    try addMapping(&mapping, gpa, idents, "Builtin.Num.F32", "F32");
    try addMapping(&mapping, gpa, idents, "Builtin.Num.F64", "F64");
    try addMapping(&mapping, gpa, idents, "Builtin.Num.Dec", "Dec");

    return mapping;
}

/// Helper to add a mapping, creating both identifiers if they don't exist.
fn addMapping(
    mapping: *ImportMapping,
    gpa: Allocator,
    ident_store: *Ident.Store,
    qualified: []const u8,
    display: []const u8,
) Allocator.Error!void {
    // Insert or find the qualified name (e.g., "Builtin.Dict")
    const qualified_ident_obj = Ident.for_text(qualified);
    const qualified_ident = try ident_store.insert(gpa, qualified_ident_obj);

    // Insert or find the display name (e.g., "Dict")
    const display_ident_obj = Ident.for_text(display);
    const display_ident = try ident_store.insert(gpa, display_ident_obj);

    try mapping.put(qualified_ident, display_ident);
}
