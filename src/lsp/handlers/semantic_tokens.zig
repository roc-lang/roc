//! Handler for the LSP `textDocument/semanticTokens/full` request.
//!
//! This handler extracts semantic tokens from a document using the Canonicalized IR
//! for richer context-aware syntax highlighting (functions, parameters, etc.)
//! and returns them in the LSP delta-encoded format.

const std = @import("std");
const Allocator = std.mem.Allocator;
const protocol = @import("../protocol.zig");
const semantic_tokens = @import("../semantic_tokens.zig");
const line_info = @import("../line_info.zig");
const uri_util = @import("../uri.zig");

/// Returns the semantic tokens handler for the LSP.
pub fn handler(comptime ServerType: type) type {
    return struct {
        pub fn call(self: *ServerType, id: *protocol.JsonId, maybe_params: ?std.json.Value) (Allocator.Error || error{WriteFailed})!void {
            if (self.state != .running) {
                try ServerType.sendError(self, id, .server_not_initialized, "server not initialized");
                return;
            }

            const params_value = maybe_params orelse {
                return try ServerType.sendError(self, id, .invalid_params, "semanticTokens requires params");
            };

            var params = protocol.SemanticTokensParams.fromJson(self.allocator, params_value) catch |err| switch (err) {
                error.OutOfMemory => return error.OutOfMemory,
                else => return try ServerType.sendError(self, id, .invalid_params, "invalid semanticTokens params"),
            };
            defer params.deinit(self.allocator);

            // Get the document from the store
            const doc = self.doc_store.get(params.textDocument.uri) orelse {
                return try ServerType.sendError(self, id, .request_failed, "document not found");
            };

            // Build line info for position conversion
            var info = try line_info.LineInfo.init(self.allocator, doc.text);
            defer info.deinit();

            // Try to get imported ModuleEnvs for cross-module semantic tokens
            var imported_envs: ?[]*@import("can").ModuleEnv = null;
            defer if (imported_envs) |envs| self.allocator.free(envs);

            // Convert URI to path and get imports
            {
                const path = try uri_util.uriToPath(self.allocator, params.textDocument.uri);
                defer self.allocator.free(path);
                // Get absolute path. A missing on-disk file is fine; we just skip
                // cross-module enrichment, but a genuine OOM must surface.
                const abs_path: ?[:0]u8 = std.Io.Dir.cwd().realPathFileAlloc(self.std_io, path, self.allocator) catch |err| switch (err) {
                    error.OutOfMemory => return error.OutOfMemory,
                    else => null,
                };
                if (abs_path) |ap| {
                    defer self.allocator.free(ap);
                    // Get imported modules from the syntax checker's cached build.
                    imported_envs = try self.syntax_checker.getImportedModuleEnvs(ap);
                }
            }

            // Extract semantic tokens using CIR with cross-module context
            const tokens = try semantic_tokens.extractSemanticTokensWithImports(
                self.allocator,
                doc.text,
                &info,
                imported_envs,
            );
            defer self.allocator.free(tokens);

            // Delta-encode the tokens
            const data = try semantic_tokens.deltaEncode(self.allocator, tokens);
            defer if (data.len > 0) self.allocator.free(data);

            // Send the response
            const response = protocol.SemanticTokens{ .data = data };
            try ServerType.sendResponse(self, id, response);
        }
    };
}
