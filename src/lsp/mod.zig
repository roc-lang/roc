//! LSP module entry point, providing protocol types, transport layer, and server implementation.

pub const std = @import("std");
pub const protocol = @import("protocol.zig");
pub const transport = @import("transport.zig");
pub const server = @import("server.zig");
pub const type_utils = @import("type_utils.zig");
pub const cir_visitor = @import("cir_visitor.zig");
pub const cir_queries = @import("cir_queries.zig");
pub const completion = @import("completion/mod.zig");
pub const module_lookup = @import("module_lookup.zig");
pub const doc_comments = @import("doc_comments.zig");

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
    std.testing.refAllDecls(@import("test/scope_map_test.zig"));
    std.testing.refAllDecls(@import("test/parse_error_test.zig"));
    std.testing.refAllDecls(@import("dependency_graph.zig"));
    std.testing.refAllDecls(@import("type_utils.zig"));
    std.testing.refAllDecls(@import("cir_visitor.zig"));
    std.testing.refAllDecls(@import("cir_queries.zig"));
    std.testing.refAllDecls(@import("module_lookup.zig"));
    std.testing.refAllDecls(@import("doc_comments.zig"));
    _ = @import("completion/mod.zig");
}
