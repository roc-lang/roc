//! Handler for the LSP `textDocument/semanticTokens/full` request.
//!
//! This handler extracts semantic tokens from a document using the Canonicalized IR
//! for richer context-aware syntax highlighting (functions, parameters, etc.)
//! and returns them in the LSP delta-encoded format.

const std = @import("std");
const protocol = @import("../protocol.zig");
const semantic_tokens = @import("../semantic_tokens.zig");
const line_info = @import("../line_info.zig");
const uri_util = @import("../uri.zig");

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

            // Try to get imported ModuleEnvs for cross-module semantic tokens
            var imported_envs: ?[]*@import("can").ModuleEnv = null;
            defer if (imported_envs) |envs| self.allocator.free(envs);

            // Convert URI to path and get imports
            if (uri_util.uriToPath(self.allocator, params.textDocument.uri)) |path| {
                defer self.allocator.free(path);
                // Get absolute path
                if (std.fs.cwd().realpathAlloc(self.allocator, path)) |abs_path| {
                    defer self.allocator.free(abs_path);
                    // Get imported modules from the syntax checker's cached build
                    if (self.syntax_checker.getImportedModuleEnvs(abs_path)) |maybe_envs| {
                        imported_envs = maybe_envs;
                    } else |_| {}
                } else |_| {}
            } else |_| {}

            // Extract semantic tokens using CIR with cross-module context
            const tokens = semantic_tokens.extractSemanticTokensWithImports(
                self.allocator,
                doc.text,
                &info,
                imported_envs,
            ) catch {
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
