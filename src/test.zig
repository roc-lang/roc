const std = @import("std");
const testing = std.testing;

test {
    testing.refAllDeclsRecursive(@import("main.zig"));
    testing.refAllDeclsRecursive(@import("snapshot.zig"));
    testing.refAllDeclsRecursive(@import("builtins/main.zig"));
    testing.refAllDeclsRecursive(@import("eval/stack.zig"));
}
