const std = @import("std");
const testing = std.testing;

test {
    testing.refAllDeclsRecursive(@import("main.zig"));
    testing.refAllDeclsRecursive(@import("builtins/main.zig"));

    // TODO: Remove after hooking up
    testing.refAllDeclsRecursive(@import("reporting.zig"));
    testing.refAllDeclsRecursive(@import("reporting/test.zig"));
    testing.refAllDeclsRecursive(@import("eval/stack.zig"));
    testing.refAllDeclsRecursive(@import("check/check_types/unify.zig"));
    testing.refAllDeclsRecursive(@import("check/check_types/occurs.zig"));
    testing.refAllDeclsRecursive(@import("snapshot.zig"));
}
