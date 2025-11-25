const std = @import("std");

comptime {
    _ = @import("test/transport_test.zig");
    _ = @import("test/server_test.zig");
    _ = @import("test/protocol_test.zig");
}
