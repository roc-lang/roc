//! LSP module entry point, providing protocol types, transport layer, and server implementation.

pub const std = @import("std");
const Allocator = std.mem.Allocator;
pub const protocol = @import("protocol.zig");
pub const transport = @import("transport.zig");
pub const server = @import("server.zig");
pub const syntax = @import("syntax.zig");
pub const diagnostics = @import("diagnostics.zig");
pub const document_store = @import("document_store.zig");
pub const line_info = @import("line_info.zig");
pub const semantic_tokens = @import("semantic_tokens.zig");
pub const scope_map = @import("scope_map.zig");
pub const dependency_graph = @import("dependency_graph.zig");
pub const uri = @import("uri.zig");
pub const type_utils = @import("type_utils.zig");
pub const cir_visitor = @import("cir_visitor.zig");
pub const cir_queries = @import("cir_queries.zig");
pub const completion = @import("completion/mod.zig");
pub const module_lookup = @import("module_lookup.zig");
pub const doc_comments = @import("doc_comments.zig");
/// Request handlers exposed for server wiring and LSP tests.
pub const handlers = struct {
    pub const completion = @import("handlers/completion.zig");
    pub const document_symbol = @import("handlers/document_symbol.zig");
};

/// Convenience wrapper to launch the server using stdin/stdout from other modules.
pub fn runWithStdIo(allocator: std.mem.Allocator, std_io: std.Io, debug: server.DebugOptions) server.RunWithStdIoError!void {
    try server.runWithStdIo(allocator, std_io, debug);
}
