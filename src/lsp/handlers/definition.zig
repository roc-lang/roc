//! Handler for LSP `textDocument/definition` requests.
//!
//! Provides go-to-definition functionality by finding where a symbol is defined.

const std = @import("std");
const Allocator = std.mem.Allocator;
const protocol = @import("../protocol.zig");

/// Handler for `textDocument/definition` requests.
pub fn handler(comptime ServerType: type) type {
    return struct {
        pub fn call(self: *ServerType, id: *protocol.JsonId, maybe_params: ?std.json.Value) (Allocator.Error || error{WriteFailed})!void {
            const params = maybe_params orelse {
                try self.sendError(id, .invalid_params, "definition requires params");
                return;
            };

            if (std.meta.activeTag(params) != .object) {
                try self.sendError(id, .invalid_params, "definition params must be an object");
                return;
            }
            const obj = params.object;

            // Extract textDocument.uri
            const text_doc_value = obj.get("textDocument") orelse {
                try self.sendError(id, .invalid_params, "missing textDocument");
                return;
            };
            if (std.meta.activeTag(text_doc_value) != .object) {
                try self.sendError(id, .invalid_params, "textDocument must be an object");
                return;
            }
            const text_doc = text_doc_value.object;
            const uri_value = text_doc.get("uri") orelse {
                try self.sendError(id, .invalid_params, "missing uri");
                return;
            };
            if (std.meta.activeTag(uri_value) != .string) {
                try self.sendError(id, .invalid_params, "uri must be a string");
                return;
            }
            const uri = uri_value.string;

            // Extract position (line, character)
            const position_value = obj.get("position") orelse {
                try self.sendError(id, .invalid_params, "missing position");
                return;
            };
            if (std.meta.activeTag(position_value) != .object) {
                try self.sendError(id, .invalid_params, "position must be an object");
                return;
            }
            const position_obj = position_value.object;

            const line_value = position_obj.get("line") orelse {
                try self.sendError(id, .invalid_params, "missing line");
                return;
            };
            if (std.meta.activeTag(line_value) != .integer) {
                try self.sendError(id, .invalid_params, "line must be an integer");
                return;
            }
            const line: u32 = std.math.cast(u32, line_value.integer) orelse {
                try self.sendError(id, .invalid_params, "line must be a non-negative integer");
                return;
            };

            const character_value = position_obj.get("character") orelse {
                try self.sendError(id, .invalid_params, "missing character");
                return;
            };
            if (std.meta.activeTag(character_value) != .integer) {
                try self.sendError(id, .invalid_params, "character must be an integer");
                return;
            }
            const character: u32 = std.math.cast(u32, character_value.integer) orelse {
                try self.sendError(id, .invalid_params, "character must be a non-negative integer");
                return;
            };

            // Get the document text from the store
            const doc = self.doc_store.get(uri);
            const text = if (doc) |d| d.text else null;

            // Query the syntax checker for definition location
            const def_result = self.syntax_checker.getDefinitionAtPosition(
                uri,
                text,
                line,
                character,
            ) catch |err| switch (err) {
                error.OutOfMemory => return error.OutOfMemory,
                error.AccessDenied,
                error.AntivirusInterference,
                error.BadPathName,
                error.BuiltinArtifactVersionMismatch,
                error.Canceled,
                error.CorruptArtifact,
                error.CorruptBuiltinArtifact,
                error.CorruptEmbeddedBuiltins,
                error.DeviceBusy,
                error.FileBusy,
                error.FileNotFound,
                error.FileSystem,
                error.FileTooBig,
                error.InputOutput,
                error.IsDir,
                error.NameTooLong,
                error.NetworkNotFound,
                error.NoDevice,
                error.NoSpaceLeft,
                error.NotDir,
                error.OperationUnsupported,
                error.PathAlreadyExists,
                error.PermissionDenied,
                error.PipeBusy,
                error.ProcessFdQuotaExceeded,
                error.StaleEmbeddedBuiltins,
                error.SymLinkLoop,
                error.SystemFdQuotaExceeded,
                error.SystemResources,
                error.Unexpected,
                error.UnrecognizedVolume,
                error.WriteFailed,
                => {
                    std.log.err("definition failed: {s}", .{@errorName(err)});
                    try self.sendNullResponse(id);
                    return;
                },
            };

            if (def_result) |result| {
                defer result.deinit(self.allocator);

                if (result.origin_selection_range) |origin| {
                    const LocationLink = struct {
                        originSelectionRange: struct {
                            start: struct { line: u32, character: u32 },
                            end: struct { line: u32, character: u32 },
                        },
                        targetUri: []const u8,
                        targetRange: struct {
                            start: struct { line: u32, character: u32 },
                            end: struct { line: u32, character: u32 },
                        },
                        targetSelectionRange: struct {
                            start: struct { line: u32, character: u32 },
                            end: struct { line: u32, character: u32 },
                        },
                    };

                    const link = LocationLink{
                        .originSelectionRange = .{
                            .start = .{ .line = origin.start_line, .character = origin.start_col },
                            .end = .{ .line = origin.end_line, .character = origin.end_col },
                        },
                        .targetUri = result.uri,
                        .targetRange = .{
                            .start = .{ .line = result.range.start_line, .character = result.range.start_col },
                            .end = .{ .line = result.range.end_line, .character = result.range.end_col },
                        },
                        .targetSelectionRange = .{
                            .start = .{ .line = result.range.start_line, .character = result.range.start_col },
                            .end = .{ .line = result.range.end_line, .character = result.range.end_col },
                        },
                    };

                    const response = [1]LocationLink{link};
                    try self.sendResponse(id, response[0..]);
                } else {
                    // Build the Location response
                    const LocationResponse = struct {
                        uri: []const u8,
                        range: struct {
                            start: struct { line: u32, character: u32 },
                            end: struct { line: u32, character: u32 },
                        },
                    };

                    const response = LocationResponse{
                        .uri = result.uri,
                        .range = .{
                            .start = .{ .line = result.range.start_line, .character = result.range.start_col },
                            .end = .{ .line = result.range.end_line, .character = result.range.end_col },
                        },
                    };

                    try self.sendResponse(id, response);
                }
            } else {
                try self.sendNullResponse(id);
            }
        }
    };
}
