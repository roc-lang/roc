const std = @import("std");
const transport_module = @import("../transport.zig");

fn frame(allocator: std.mem.Allocator, body: []const u8) ![]u8 {
    return try std.fmt.allocPrint(allocator, "Content-Length: {d}\r\n\r\n{s}", .{ body.len, body });
}

test "transport decodes and encodes LSP frames" {
    const allocator = std.testing.allocator;

    const request_body = "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"ping\"}";
    const framed = try frame(allocator, request_body);
    defer allocator.free(framed);

    var input = std.io.fixedBufferStream(framed);
    var output_buffer: [512]u8 = undefined;
    var output = std.io.fixedBufferStream(&output_buffer);

    const ReaderType = @TypeOf(input.reader());
    const WriterType = @TypeOf(output.writer());
    var transport = transport_module.Transport(ReaderType, WriterType).init(allocator, input.reader(), output.writer(), null);

    const payload = try transport.readMessage();
    defer allocator.free(payload);
    try std.testing.expectEqualStrings(request_body, payload);

    const response_body = struct {
        jsonrpc: []const u8 = "2.0",
        id: i64 = 1,
        result: struct {
            acknowledged: bool = true,
        } = .{},
    }{};

    try transport.sendJson(response_body);

    const written = output.getWritten();
    const separator_index = std.mem.indexOf(u8, written, "\r\n\r\n") orelse unreachable;
    const body = written[(separator_index + 4)..];

    var parsed = try std.json.parseFromSlice(std.json.Value, allocator, body, .{});
    defer parsed.deinit();

    const obj = parsed.value.object;
    try std.testing.expect(obj.get("jsonrpc") != null);
    try std.testing.expect(obj.get("result") != null);
}

test "transport errors when Content-Length header is missing" {
    const allocator = std.testing.allocator;
    const invalid_frame = "Content-Type: application/vscode-jsonrpc; charset=utf-8\r\n\r\n{}";
    var input = std.io.fixedBufferStream(invalid_frame);
    var output_buffer: [16]u8 = undefined;
    var output = std.io.fixedBufferStream(&output_buffer);

    const ReaderType = @TypeOf(input.reader());
    const WriterType = @TypeOf(output.writer());
    var transport = transport_module.Transport(ReaderType, WriterType).init(allocator, input.reader(), output.writer(), null);

    try std.testing.expectError(error.MissingContentLength, transport.readMessage());
}

test "transport logs traffic when debug file is provided" {
    const allocator = std.testing.allocator;
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();

    const log_handle = try tmp.dir.createFile("traffic.log", .{ .truncate = true, .read = true });

    var input = std.io.fixedBufferStream("");
    var output_buffer: [512]u8 = undefined;
    var output = std.io.fixedBufferStream(&output_buffer);

    const ReaderType = @TypeOf(input.reader());
    const WriterType = @TypeOf(output.writer());
    var transport = transport_module.Transport(ReaderType, WriterType).init(
        allocator,
        input.reader(),
        output.writer(),
        log_handle,
    );
    defer transport.deinit();

    try transport.sendJson(.{
        .jsonrpc = "2.0",
        .id = 1,
        .result = .{ .ack = true },
    });

    const log_file = try tmp.dir.openFile("traffic.log", .{});
    defer log_file.close();
    const contents = try log_file.readToEndAlloc(allocator, 2048);
    defer allocator.free(contents);

    try std.testing.expect(std.mem.indexOf(u8, contents, "OUT") != null);
    try std.testing.expect(std.mem.indexOf(u8, contents, "\"jsonrpc\"") != null);
}

test "transport rejects oversized header lines" {
    const allocator = std.testing.allocator;
    var frame_builder = std.ArrayList(u8){};
    defer frame_builder.deinit(allocator);

    try frame_builder.ensureTotalCapacity(allocator, 9005);
    try frame_builder.appendNTimes(allocator, 'A', 9000);
    try frame_builder.append(allocator, '\n');
    try frame_builder.appendSlice(allocator, "Content-Length: 0\r\n\r\n");

    const header_frame = frame_builder.items;

    var input = std.io.fixedBufferStream(header_frame);
    var output_buffer: [16]u8 = undefined;
    var output = std.io.fixedBufferStream(&output_buffer);

    const ReaderType = @TypeOf(input.reader());
    const WriterType = @TypeOf(output.writer());
    var transport = transport_module.Transport(ReaderType, WriterType).init(
        allocator,
        input.reader(),
        output.writer(),
        null,
    );
    defer transport.deinit();

    try std.testing.expectError(error.HeaderTooLong, transport.readMessage());
}
