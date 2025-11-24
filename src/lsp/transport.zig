const std = @import("std");

pub fn Transport(comptime ReaderType: type, comptime WriterType: type) type {
    return struct {
        const Self = @This();
        const ReaderError = if (@hasDecl(ReaderType, "Error")) ReaderType.Error else anyerror;
        const WriterError = if (@hasDecl(WriterType, "Error")) WriterType.Error else anyerror;

        allocator: std.mem.Allocator,
        reader: ReaderType,
        writer: WriterType,
        log_file: ?std.fs.File = null,

        pub const ReadMessageError = ReaderError || std.mem.Allocator.Error || error{
            EndOfStream,
            MissingContentLength,
            InvalidHeader,
            HeaderTooLong,
            PayloadTooLarge,
        };

        pub const WriteMessageError = WriterError || error{OutOfMemory};

        const max_header_line = 8 * 1024;
        const max_payload_size: usize = 16 * 1024 * 1024;

        pub fn init(allocator: std.mem.Allocator, reader: ReaderType, writer: WriterType, log_file: ?std.fs.File) Self {
            return .{
                .allocator = allocator,
                .reader = reader,
                .writer = writer,
                .log_file = log_file,
            };
        }

        pub fn deinit(self: *Self) void {
            if (self.log_file) |*file| {
                file.close();
                self.log_file = null;
            }
        }

        pub fn readMessage(self: *Self) ReadMessageError![]u8 {
            var line_buffer = std.ArrayList(u8){};
            defer line_buffer.deinit(self.allocator);

            var content_length: ?usize = null;
            while (true) {
                const maybe_line = try self.readHeaderLine(&line_buffer);
                const line = maybe_line orelse return error.EndOfStream;
                if (line.len == 0) break;

                const colon_index = std.mem.indexOfScalar(u8, line, ':') orelse return error.InvalidHeader;
                const name = std.mem.trim(u8, line[0..colon_index], " \t");
                const value = std.mem.trim(u8, line[(colon_index + 1)..], " \t");

                if (std.ascii.eqlIgnoreCase(name, "content-length")) {
                    const parsed = std.fmt.parseInt(usize, value, 10) catch return error.InvalidHeader;
                    if (parsed > max_payload_size) {
                        return error.PayloadTooLarge;
                    }
                    content_length = parsed;
                }
            }

            const length = content_length orelse return error.MissingContentLength;
            var payload = try self.allocator.alloc(u8, length);
            errdefer self.allocator.free(payload);

            var offset: usize = 0;
            while (offset < length) {
                const written = try self.readSome(payload[offset..]);
                if (written == 0) return error.EndOfStream;
                offset += written;
            }

            self.logMessage("IN", payload);
            return payload;
        }

        pub fn sendBytes(self: *Self, payload: []const u8) (WriterError)!void {
            var header_buffer: [64]u8 = undefined;
            const header = try std.fmt.bufPrint(&header_buffer, "Content-Length: {d}\r\n\r\n", .{payload.len});
            try self.writeAll(header);
            if (payload.len != 0) {
                try self.writeAll(payload);
            }
            self.logMessage("OUT", payload);
            try self.flushWriter();
        }

        pub fn sendJson(self: *Self, value: anytype) WriteMessageError!void {
            const payload = try encodeJson(self.allocator, value);
            defer self.allocator.free(payload);
            try self.sendBytes(payload);
        }

        fn readHeaderLine(self: *Self, buffer: *std.ArrayList(u8)) ReadMessageError!?[]const u8 {
            buffer.clearRetainingCapacity();

            var saw_any = false;
            while (true) {
                var byte_buf: [1]u8 = undefined;
                const amount = try self.readSome(&byte_buf);
                if (amount == 0) {
                    if (!saw_any) return null;
                    return error.EndOfStream;
                }
                saw_any = true;

                const byte = byte_buf[0];
                if (byte == '\n') break;

                if (buffer.items.len >= max_header_line) return error.HeaderTooLong;
                try buffer.append(self.allocator, byte);
            }

            if (buffer.items.len > 0 and buffer.items[buffer.items.len - 1] == '\r') {
                buffer.items.len -= 1;
            }

            return buffer.items;
        }

        fn writeAll(self: *Self, bytes: []const u8) (WriterError)!void {
            var offset: usize = 0;
            while (offset < bytes.len) {
                const written = try self.writeSome(bytes[offset..]);
                if (written == 0) {
                    std.debug.panic("writer returned 0 bytes without error", .{});
                }
                offset += written;
            }
        }

        fn readSome(self: *Self, buffer: []u8) ReadMessageError!usize {
            if (@hasDecl(ReaderType, "read")) {
                return try self.reader.read(buffer);
            } else if (@hasField(ReaderType, "interface")) {
                return try (&self.reader.interface).readSliceShort(buffer);
            } else {
                @compileError("ReaderType must provide either a read method or expose an interface field");
            }
        }

        fn writeSome(self: *Self, bytes: []const u8) (WriterError)!usize {
            if (@hasDecl(WriterType, "write")) {
                return try self.writer.write(bytes);
            } else if (@hasField(WriterType, "interface")) {
                return try (&self.writer.interface).write(bytes);
            } else {
                @compileError("WriterType must provide either a write method or expose an interface field");
            }
        }

        fn flushWriter(self: *Self) (WriterError)!void {
            if (@hasDecl(WriterType, "flush")) {
                return self.writer.flush();
            } else if (@hasField(WriterType, "interface")) {
                return (&self.writer.interface).flush();
            } else {
                return;
            }
        }

        fn logMessage(self: *Self, direction: []const u8, payload: []const u8) void {
            var log_file = self.log_file orelse return;
            var header_buffer: [128]u8 = undefined;
            const header = std.fmt.bufPrint(
                &header_buffer,
                "[{d}] {s} ({d} bytes)\n",
                .{ std.time.milliTimestamp(), direction, payload.len },
            ) catch return;
            log_file.writeAll(header) catch return;
            log_file.writeAll(payload) catch return;
            log_file.writeAll("\n---\n") catch return;
            log_file.sync() catch {};
        }
    };
}

fn encodeJson(allocator: std.mem.Allocator, value: anytype) error{OutOfMemory}![]u8 {
    var writer: std.io.Writer.Allocating = .init(allocator);
    defer writer.deinit();
    std.json.Stringify.value(value, .{}, &writer.writer) catch return error.OutOfMemory;
    return writer.toOwnedSlice();
}
