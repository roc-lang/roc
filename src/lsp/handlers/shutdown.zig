// zig-lint: required-param

const std = @import("std");
const protocol = @import("../protocol.zig");

/// Returns the `shutdown` method handler for the LSP.
pub fn handler(comptime ServerType: type) type {
    return struct {
        pub fn call(self: *ServerType, id: *protocol.JsonId, _: ?std.json.Value) !void {
            switch (self.state) {
                .waiting_for_initialize => {
                    try ServerType.sendError(self, id, .server_not_initialized, "initialize must be called before shutdown");
                    return;
                },
                .exit_success, .exit_failure => {
                    try ServerType.sendError(self, id, .invalid_request, "server is already exiting");
                    return;
                },
                .shutdown => {
                    try ServerType.sendNullResponse(self, id);
                    return;
                },
                .running, .waiting_for_initialized => {},
            }

            self.state = .shutdown;
            try ServerType.sendNullResponse(self, id);
        }
    };
}
