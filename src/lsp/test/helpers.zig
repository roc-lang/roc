//! Shared helpers for LSP server and handler tests.

const std = @import("std");
const transport_module = @import("lsp").transport;

pub fn frame(allocator: std.mem.Allocator, body: []const u8) ![]u8 {
    return try std.fmt.allocPrint(allocator, "Content-Length: {d}\r\n\r\n{s}", .{ body.len, body });
}

pub fn framedInput(allocator: std.mem.Allocator, bodies: []const []const u8) ![]u8 {
    var builder: std.ArrayList(u8) = .empty;
    errdefer builder.deinit(allocator);

    for (bodies) |body| {
        const message = try frame(allocator, body);
        defer allocator.free(message);
        try builder.appendSlice(allocator, message);
    }

    const data = try builder.toOwnedSlice(allocator);
    builder.deinit(allocator);
    return data;
}

pub fn collectResponses(allocator: std.mem.Allocator, bytes: []const u8) ![][]u8 {
    const reader: std.Io.Reader = .fixed(bytes);
    var sink_storage: [1]u8 = undefined;
    const sink: std.Io.Writer = .fixed(&sink_storage);

    const ReaderType = std.Io.Reader;
    const WriterType = std.Io.Writer;
    var transport = transport_module.Transport(ReaderType, WriterType).init(allocator, std.testing.io, reader, sink, null);

    var responses: std.ArrayList([]u8) = .empty;
    errdefer {
        for (responses.items) |body| allocator.free(body);
        responses.deinit(allocator);
    }

    while (true) {
        const message = transport.readMessage() catch |err| switch (err) {
            error.EndOfStream => break,
            else => return err,
        };
        try responses.append(allocator, message);
    }

    return responses.toOwnedSlice(allocator);
}

pub fn uriFromPath(allocator: std.mem.Allocator, path: []const u8) ![]u8 {
    return @import("lsp").uri.pathToUri(allocator, path);
}

pub fn freeResponses(allocator: std.mem.Allocator, responses: [][]u8) void {
    for (responses) |body| allocator.free(body);
    allocator.free(responses);
}
