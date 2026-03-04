//! Roc builtin types for the completion system.
//!
//! This module provides constants and utilities for working with
//! Roc's builtin types (Str, List, Bool, numeric types, etc.).

const std = @import("std");

/// Known builtin type names that are part of the Builtin module.
/// These are the core types provided by the Roc language runtime.
pub const BUILTIN_TYPES = [_][]const u8{
    // Collection types
    "Str",
    "List",
    "Dict",
    "Set",
    "Box",
    // Boolean and control flow
    "Bool",
    "Try",
    // Unsigned integers
    "U8",
    "U16",
    "U32",
    "U64",
    "U128",
    // Signed integers
    "I8",
    "I16",
    "I32",
    "I64",
    "I128",
    // Floating point
    "F32",
    "F64",
    // Fixed-point decimal
    "Dec",
    // Generic numeric
    "Num",
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

test "BUILTIN_TYPES has expected count" {
    // 5 collection types + 2 bool/control + 5 unsigned + 5 signed + 2 float + 2 decimal/num = 21
    try std.testing.expectEqual(@as(usize, 21), BUILTIN_TYPES.len);
}
