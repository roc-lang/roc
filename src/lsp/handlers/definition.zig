//! Handler for LSP `textDocument/definition` requests.
//!
//! Provides go-to-definition functionality by finding where a symbol is defined.

const std = @import("std");
const Allocator = std.mem.Allocator;
const protocol = @import("../protocol.zig");
const position_params = @import("position_params.zig");

/// Handler for `textDocument/definition` requests.
pub fn handler(comptime ServerType: type) type {
    return struct {
        pub fn call(self: *ServerType, id: *protocol.JsonId, maybe_params: ?std.json.Value) (Allocator.Error || error{WriteFailed})!void {
            const position = try position_params.parse(self, id, "definition", maybe_params) orelse return;
            const uri = position.uri;
            const line = position.line;
            const character = position.character;

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
