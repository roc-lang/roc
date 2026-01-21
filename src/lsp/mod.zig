//! LSP module entry point, providing protocol types, transport layer, and server implementation.

pub const std = @import("std");
pub const protocol = @import("protocol.zig");
pub const transport = @import("transport.zig");
pub const server = @import("server.zig");

/// Convenience wrapper to launch the server using stdin/stdout from other modules.
pub fn runWithStdIo(allocator: std.mem.Allocator, debug: server.DebugOptions) !void {
    try server.runWithStdIo(allocator, debug);
}

test "lsp tests" {
    std.testing.refAllDecls(@import("test/protocol_test.zig"));
    std.testing.refAllDecls(@import("test/server_test.zig"));
    std.testing.refAllDecls(@import("test/transport_test.zig"));
    std.testing.refAllDecls(@import("test/document_store_test.zig"));
    std.testing.refAllDecls(@import("test/syntax_test.zig"));
    std.testing.refAllDecls(@import("test/line_info_test.zig"));
    std.testing.refAllDecls(@import("test/semantic_tokens_test.zig"));
    std.testing.refAllDecls(@import("test/handler_tests.zig"));
    std.testing.refAllDecls(@import("dependency_graph.zig"));
}
