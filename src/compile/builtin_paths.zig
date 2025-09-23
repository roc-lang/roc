const std = @import("std");

/// Central directory for builtin .roc source files
pub const BUILTIN_DIR = "builtins";

/// Generate the path to a builtin .roc file at compile time
pub fn builtinPath(comptime name: []const u8) []const u8 {
    return std.fmt.comptimePrint("{s}/{s}.roc", .{ BUILTIN_DIR, name });
}

/// Runtime path formatting for virtual builtin modules
pub fn formatBuiltinModulePath(allocator: std.mem.Allocator, name: []const u8) ![]u8 {
    return std.fmt.allocPrint(allocator, "@builtin/{s}", .{name});
}

/// List of all builtin module names
pub const BUILTIN_NAMES = [_][]const u8{
    "U8",
    "U16",
    "U32",
    "U64",
    "U128",
    "I8",
    "I16",
    "I32",
    "I64",
    "I128",
    "F32",
    "F64",
    "Dec",
    "Bool",
    "Box",
    "Result",
    "List",
    "Str",
    "Dict",
    "Set",
};

test "builtin path generation" {
    const testing = std.testing;

    // Compile-time path generation
    const bool_path = comptime builtinPath("Bool");
    try testing.expectEqualStrings("builtins/Bool.roc", bool_path);

    const list_path = comptime builtinPath("List");
    try testing.expectEqualStrings("builtins/List.roc", list_path);

    const u8_path = comptime builtinPath("U8");
    try testing.expectEqualStrings("builtins/U8.roc", u8_path);
}

test "format builtin module path" {
    const testing = std.testing;
    const allocator = testing.allocator;

    const bool_module_path = try formatBuiltinModulePath(allocator, "Bool");
    defer allocator.free(bool_module_path);
    try testing.expectEqualStrings("@builtin/Bool", bool_module_path);

    const list_module_path = try formatBuiltinModulePath(allocator, "List");
    defer allocator.free(list_module_path);
    try testing.expectEqualStrings("@builtin/List", list_module_path);
}
