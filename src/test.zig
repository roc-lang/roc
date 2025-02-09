const std = @import("std");
const testing = std.testing;

test {
    testing.refAllDeclsRecursive(@import("main.zig"));
    testing.refAllDeclsRecursive(@import("fmt.zig"));
    testing.refAllDeclsRecursive(@import("check/parse.zig"));
}
