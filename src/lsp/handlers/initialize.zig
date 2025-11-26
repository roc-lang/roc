const std = @import("std");
const protocol = @import("../protocol.zig");
const capabilities = @import("../capabilities.zig");

/// Returns the `initialize` method handler for the LSP.
pub fn handler(comptime ServerType: type) type {
    return struct {
        pub fn call(self: *ServerType, id: *protocol.JsonId, maybe_params: ?std.json.Value) !void {
            if (self.state != .waiting_for_initialize) {
                try ServerType.sendError(self, id, .invalid_request, "server was already initialized");
                return;
            }

            const params_value = maybe_params orelse return try ServerType.sendError(self, id, .invalid_params, "initialize requires params");

            var params = try protocol.InitializeParams.fromJson(self.allocator, params_value);
            defer params.deinit(self.allocator);

            self.client.deinit(self.allocator);
            params.moveInto(&self.client);
            self.state = .waiting_for_initialized;

            const response = protocol.InitializeResult{
                .capabilities = capabilities.buildCapabilities(),
                .serverInfo = .{
                    .name = ServerType.server_name,
                    .version = ServerType.version,
                },
            };

            try ServerType.sendResponse(self, id, response);
        }
    };
}
