pub const std = @import("std");
pub const protocol = @import("protocol.zig");
pub const transport = @import("transport.zig");
pub const server = @import("server.zig");

pub fn runWithStdIo(allocator: std.mem.Allocator, debug_transport: bool) !void {
    try server.runWithStdIo(allocator, debug_transport);
}
