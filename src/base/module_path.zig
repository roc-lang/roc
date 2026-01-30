//! Utilities for extracting module names from file paths.
//!
//! This module provides functions to extract the module name from a file path,
//! which is useful when validating type modules (the module name must match
//! the type being defined) or displaying error messages.

const std = @import("std");

/// Extract the module name from a file path by taking the basename and
/// removing any extension.
///
/// Examples:
///   "path/to/Module.roc" -> "Module"
///   "path/to/Module" -> "Module"
///   "path/to/file.txt" -> "file"
///
/// This function returns a slice of the input path, so no allocation is needed.
/// For cases where you need an owned copy, use `getModuleNameAlloc`.
pub fn getModuleName(path: []const u8) []const u8 {
    const base_name = std.fs.path.basename(path);
    if (std.mem.lastIndexOfScalar(u8, base_name, '.')) |dot| {
        return base_name[0..dot];
    }
    return base_name;
}

/// Extract the module name from a file path and allocate a copy.
///
/// Useful when you need to store the module name beyond the lifetime of the
/// original path string, or when working with arena allocators.
///
/// Examples:
///   "path/to/Module.roc" -> "Module" (allocated)
///   "path/to/Module" -> "Module" (allocated)
pub fn getModuleNameAlloc(
    allocator: std.mem.Allocator,
    path: []const u8,
) std.mem.Allocator.Error![]u8 {
    return try allocator.dupe(u8, getModuleName(path));
}

test "getModuleName strips .roc extension" {
    try std.testing.expectEqualStrings("Module", getModuleName("path/to/Module.roc"));
    try std.testing.expectEqualStrings("Module", getModuleName("Module.roc"));
}

test "getModuleName strips any extension" {
    try std.testing.expectEqualStrings("file", getModuleName("path/to/file.txt"));
    try std.testing.expectEqualStrings("data", getModuleName("data.json"));
}

test "getModuleName handles no extension" {
    try std.testing.expectEqualStrings("Module", getModuleName("path/to/Module"));
    try std.testing.expectEqualStrings("Module", getModuleName("Module"));
}

test "getModuleName handles edge cases" {
    try std.testing.expectEqualStrings("", getModuleName(".roc"));
    try std.testing.expectEqualStrings("file", getModuleName("file."));
}

test "getModuleNameAlloc allocates correctly" {
    const allocator = std.testing.allocator;
    const result = try getModuleNameAlloc(allocator, "path/to/Module.roc");
    defer allocator.free(result);
    try std.testing.expectEqualStrings("Module", result);
}

/// Result of parsing a qualified import name like "pf.Wrapper"
pub const QualifiedImport = struct {
    qualifier: []const u8,
    module: []const u8,
};

/// Parse a qualified import name into its qualifier and module parts.
///
/// Examples:
///   "pf.Wrapper" -> { .qualifier = "pf", .module = "Wrapper" }
///   "json.Decode" -> { .qualifier = "json", .module = "Decode" }
///   "Wrapper" -> null (no qualifier)
///
/// Returns null if the import name has no qualifier (no dot).
/// This function returns slices of the input, so no allocation is needed.
pub fn parseQualifiedImport(import_name: []const u8) ?QualifiedImport {
    const dot_idx = std.mem.indexOfScalar(u8, import_name, '.') orelse return null;
    return .{
        .qualifier = import_name[0..dot_idx],
        .module = import_name[dot_idx + 1 ..],
    };
}

test "parseQualifiedImport parses qualified names" {
    const result = parseQualifiedImport("pf.Wrapper").?;
    try std.testing.expectEqualStrings("pf", result.qualifier);
    try std.testing.expectEqualStrings("Wrapper", result.module);
}

test "parseQualifiedImport returns null for unqualified names" {
    try std.testing.expect(parseQualifiedImport("Wrapper") == null);
}

test "parseQualifiedImport handles nested qualifiers" {
    const result = parseQualifiedImport("pkg.Sub.Module").?;
    try std.testing.expectEqualStrings("pkg", result.qualifier);
    try std.testing.expectEqualStrings("Sub.Module", result.module);
}
