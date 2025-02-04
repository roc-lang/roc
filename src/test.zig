const std = @import("std");
const testing = std.testing;

comptime {
    testing.refAllDecls(@import("main.zig"));
    testing.refAllDecls(@import("command.zig"));
}

test {
    std.testing.refAllDecls(@This());
}
