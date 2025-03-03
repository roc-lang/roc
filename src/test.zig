const std = @import("std");
const testing = std.testing;

test {
    // TODO: remove these once main is properly factored to import all passes.
    // Once we have the core wiring, they should no longer be needed.
    testing.refAllDeclsRecursive(@import("main.zig"));
    testing.refAllDeclsRecursive(@import("snapshot.zig"));
}
