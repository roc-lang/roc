//! Handler for the LSP `initialize` request that sets up the server-client connection.

const std = @import("std");
const Allocator = std.mem.Allocator;
const protocol = @import("../protocol.zig");
const capabilities = @import("../capabilities.zig");

/// Returns the `initialize` method handler for the LSP.
pub fn handler(comptime ServerType: type) type {
    return struct {
        pub fn call(self: *ServerType, id: *protocol.JsonId, maybe_params: ?std.json.Value) (Allocator.Error || error{WriteFailed})!void {
            if (self.state != .waiting_for_initialize) {
                try ServerType.sendError(self, id, .invalid_request, "server was already initialized");
                return;
            }

            const params_value = maybe_params orelse return try ServerType.sendError(self, id, .invalid_params, "initialize requires params");

            var params = protocol.InitializeParams.fromJson(self.allocator, params_value) catch |err| switch (err) {
                error.OutOfMemory => return error.OutOfMemory,
                error.InvalidParams => {
                    try ServerType.sendError(self, id, .invalid_params, "invalid initialize params");
                    return;
                },
            };
            defer params.deinit(self.allocator);

            const response = protocol.InitializeResult{
                .capabilities = capabilities.buildCapabilities(),
                .serverInfo = .{
                    .name = ServerType.server_name,
                    .version = ServerType.version,
                },
            };

            try ServerType.sendResponse(self, id, response);

            self.client.deinit(self.allocator);
            params.moveInto(&self.client);
            self.state = .waiting_for_initialized;
        }
    };
}
