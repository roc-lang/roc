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
