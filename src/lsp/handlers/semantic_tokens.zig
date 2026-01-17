//! Handler for the LSP `textDocument/semanticTokens/full` request.
//!
//! This handler extracts semantic tokens from a document using the Canonicalized IR
//! for richer context-aware syntax highlighting (functions, parameters, etc.)
//! and returns them in the LSP delta-encoded format.

const std = @import("std");
const protocol = @import("../protocol.zig");
const semantic_tokens = @import("../semantic_tokens.zig");
const line_info = @import("../line_info.zig");

/// Returns the semantic tokens handler for the LSP.
pub fn handler(comptime ServerType: type) type {
    return struct {
        pub fn call(self: *ServerType, id: *protocol.JsonId, maybe_params: ?std.json.Value) !void {
            if (self.state != .running) {
                try ServerType.sendError(self, id, .server_not_initialized, "server not initialized");
                return;
            }

            const params_value = maybe_params orelse {
                return try ServerType.sendError(self, id, .invalid_params, "semanticTokens requires params");
            };

            var params = protocol.SemanticTokensParams.fromJson(self.allocator, params_value) catch {
                return try ServerType.sendError(self, id, .invalid_params, "invalid semanticTokens params");
            };
            defer params.deinit(self.allocator);

            // Get the document from the store
            const doc = self.doc_store.get(params.textDocument.uri) orelse {
                return try ServerType.sendError(self, id, .request_failed, "document not found");
            };

            // Build line info for position conversion
            var info = line_info.LineInfo.init(self.allocator, doc.text) catch {
                return try ServerType.sendError(self, id, .internal_error, "failed to build line info");
            };
            defer info.deinit();

            // Extract semantic tokens using CIR for richer context
            const tokens = semantic_tokens.extractSemanticTokensWithCIR(self.allocator, doc.text, &info) catch {
                return try ServerType.sendError(self, id, .internal_error, "failed to extract tokens");
            };
            defer self.allocator.free(tokens);

            // Delta-encode the tokens
            const data = semantic_tokens.deltaEncode(self.allocator, tokens) catch {
                return try ServerType.sendError(self, id, .internal_error, "failed to encode tokens");
            };
            defer if (data.len > 0) self.allocator.free(data);

            // Send the response
            const response = protocol.SemanticTokens{ .data = data };
            try ServerType.sendResponse(self, id, response);
        }
    };
}
