const std = @import("std");
const testing = std.testing;

test {
    testing.refAllDeclsRecursive(@import("main.zig"));
    testing.refAllDeclsRecursive(@import("coordinate.zig"));
    testing.refAllDeclsRecursive(@import("check/parse.zig"));
    testing.refAllDeclsRecursive(@import("check/resolve_imports.zig"));
    testing.refAllDeclsRecursive(@import("check/typecheck.zig"));
    testing.refAllDeclsRecursive(@import("build/specialize_types.zig"));
    testing.refAllDeclsRecursive(@import("build/lift_functions.zig"));
    testing.refAllDeclsRecursive(@import("build/specialize_functions.zig"));
    testing.refAllDeclsRecursive(@import("build/solve_functions.zig"));
    testing.refAllDeclsRecursive(@import("build/lower_statements.zig"));
    testing.refAllDeclsRecursive(@import("build/reference_count.zig"));
}
