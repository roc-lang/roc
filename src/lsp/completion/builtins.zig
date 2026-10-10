//! Roc builtin types for the completion system.
//!
//! The names user code can write without importing anything: the builtin
//! types the compiler auto-imports, and the builtin namespaces that hold them.

const std = @import("std");
const CIR = @import("can").CIR;

/// The names of the builtin types and namespaces every module has in scope,
/// read from the compiler's builtin type registry: each auto-imported type's
/// name, then each namespace declared directly inside `Builtin` (such as
/// `Num`).
pub const BUILTIN_TYPES = blk: {
    const module_prefix = "Builtin.";
    var names: [CIR.builtin_type_specs.len + CIR.builtin_type_container_names.len][]const u8 = undefined;
    var count: usize = 0;
    for (CIR.builtin_type_specs) |spec| {
        if (!spec.auto_import) continue;
        names[count] = spec.display_name;
        count += 1;
    }
    for (CIR.builtin_type_container_names) |container| {
        if (!std.mem.startsWith(u8, container, module_prefix)) continue;
        const name = container[module_prefix.len..];
        if (std.mem.findScalar(u8, name, '.') != null) continue;
        names[count] = name;
        count += 1;
    }
    const roster = names[0..count].*;
    break :blk roster;
};

/// Compile-time hash map for O(1) builtin type lookups.
const builtin_set = std.StaticStringMap(void).initComptime(blk: {
    var entries: [BUILTIN_TYPES.len]struct { []const u8, void } = undefined;
    for (BUILTIN_TYPES, 0..) |name, i| {
        entries[i] = .{ name, {} };
    }
    break :blk &entries;
});

/// Check if a type name is a known builtin type.
///
/// Returns true if the given type name matches one of Roc's
/// builtin types (Str, List, Bool, numeric types, etc.).
pub fn isBuiltinType(type_name: []const u8) bool {
    return builtin_set.has(type_name);
}

// Tests

test "isBuiltinType recognizes collection types" {
    try std.testing.expect(isBuiltinType("Str"));
    try std.testing.expect(isBuiltinType("List"));
    try std.testing.expect(isBuiltinType("Dict"));
    try std.testing.expect(isBuiltinType("Set"));
    try std.testing.expect(isBuiltinType("Box"));
    try std.testing.expect(isBuiltinType("Crypto"));
}

test "isBuiltinType recognizes boolean and control flow types" {
    try std.testing.expect(isBuiltinType("Bool"));
    try std.testing.expect(isBuiltinType("Try"));
}

test "isBuiltinType recognizes unsigned integer types" {
    try std.testing.expect(isBuiltinType("U8"));
    try std.testing.expect(isBuiltinType("U16"));
    try std.testing.expect(isBuiltinType("U32"));
    try std.testing.expect(isBuiltinType("U64"));
    try std.testing.expect(isBuiltinType("U128"));
}

test "isBuiltinType recognizes signed integer types" {
    try std.testing.expect(isBuiltinType("I8"));
    try std.testing.expect(isBuiltinType("I16"));
    try std.testing.expect(isBuiltinType("I32"));
    try std.testing.expect(isBuiltinType("I64"));
    try std.testing.expect(isBuiltinType("I128"));
}

test "isBuiltinType recognizes floating point types" {
    try std.testing.expect(isBuiltinType("F32"));
    try std.testing.expect(isBuiltinType("F64"));
}

test "isBuiltinType recognizes Dec and Num types" {
    try std.testing.expect(isBuiltinType("Dec"));
    try std.testing.expect(isBuiltinType("Num"));
}

test "isBuiltinType rejects non-builtin types" {
    try std.testing.expect(!isBuiltinType("MyType"));
    try std.testing.expect(!isBuiltinType("String")); // Not "Str"
    try std.testing.expect(!isBuiltinType("Integer")); // Not a builtin
    try std.testing.expect(!isBuiltinType(""));
    try std.testing.expect(!isBuiltinType("str")); // Case sensitive
    try std.testing.expect(!isBuiltinType("STR")); // Case sensitive
    try std.testing.expect(!isBuiltinType("u8")); // Case sensitive (lowercase)
}

test "every auto-imported builtin type is a completion namespace" {
    inline for (CIR.builtin_type_specs) |spec| {
        if (spec.auto_import) try std.testing.expect(isBuiltinType(spec.display_name));
    }
}

test "builtin types the compiler does not auto-import are not completion namespaces" {
    try std.testing.expect(!isBuiltinType("JsonState"));
    try std.testing.expect(!isBuiltinType("Builtin"));
    try std.testing.expect(!isBuiltinType("SHA256"));
}
