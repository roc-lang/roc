const std = @import("std");

test {
    std.testing.refAllDecls(@import("test_rigid_instantiation.zig"));
}
