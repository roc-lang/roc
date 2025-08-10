const std = @import("std");

test "builtins tests" {
    std.testing.refAllDecls(@import("dec.zig"));
    std.testing.refAllDecls(@import("hash.zig"));
    std.testing.refAllDecls(@import("host_abi.zig"));
    std.testing.refAllDecls(@import("list.zig"));
    std.testing.refAllDecls(@import("num.zig"));
    std.testing.refAllDecls(@import("sort.zig"));
    std.testing.refAllDecls(@import("str.zig"));
    std.testing.refAllDecls(@import("utils.zig"));
}
