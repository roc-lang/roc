const std = @import("std");
const testing = std.testing;

test "websocket server and client communication" {
    const allocator = testing.allocator;

    const TestServer = struct {
        tcp_server: std.net.Server,
        thread: std.Thread,
        port: u16,
        should_stop: std.atomic.Value(bool),
        allocator: std.mem.Allocator,

        fn init(allocator_: std.mem.Allocator) !@This() {
            var attempts: u32 = 0;
            while (attempts < 20) : (attempts += 1) {
                const port = if (attempts == 0) 0 else @as(u16, @intCast(9000 + attempts));
                const address = std.net.Address.parseIp("127.0.0.1", port) catch continue;
                
                const tcp_server = address.listen(.{ .reuse_address = true }) catch |err| {
                    if (err == error.AddressInUse) continue;
                    return err;
                };
                
                const actual_port = tcp_server.listen_address.in.getPort();

                return .{
                    .tcp_server = tcp_server,
                    .thread = undefined,
                    .port = actual_port,
                    .should_stop = std.atomic.Value(bool).init(false),
                    .allocator = allocator_,
                };
            }
            return error.NoAvailablePort;
        }

        fn start(self: *@This()) !void {
            self.thread = try std.Thread.spawn(.{}, serve, .{self});
        }

        fn stop(self: *@This()) void {
            self.should_stop.store(true, .seq_cst);
            self.tcp_server.deinit();
            self.thread.join();
        }

        fn serve(self: *@This()) void {
            while (!self.should_stop.load(.seq_cst)) {
                var connection = self.tcp_server.accept() catch |err| {
                    if (err == error.SocketNotListening) return;
                    continue;
                };
                defer connection.stream.close();

                // Read the HTTP upgrade request
                var buf: [4096]u8 = undefined;
                const n = connection.stream.read(&buf) catch continue;
                if (n == 0) continue;

                const request = buf[0..n];
                
                // Parse the Sec-WebSocket-Key
                var ws_key: ?[]const u8 = null;
                var lines = std.mem.tokenizeSequence(u8, request, "\r\n");
                while (lines.next()) |line| {
                    if (std.mem.startsWith(u8, line, "Sec-WebSocket-Key: ")) {
                        ws_key = line["Sec-WebSocket-Key: ".len..];
                        break;
                    }
                }

                const key = ws_key orelse continue;

                // Calculate the Sec-WebSocket-Accept value
                var sha1 = std.crypto.hash.Sha1.init(.{});
                sha1.update(key);
                sha1.update("258EAFA5-E914-47DA-95CA-C5AB0DC85B11");
                var digest: [std.crypto.hash.Sha1.digest_length]u8 = undefined;
                sha1.final(&digest);
                var accept_key: [28]u8 = undefined;
                _ = std.base64.standard.Encoder.encode(&accept_key, &digest);

                // Send WebSocket handshake response
                const response_headers = 
                    "HTTP/1.1 101 Switching Protocols\r\n" ++
                    "Connection: Upgrade\r\n" ++
                    "Upgrade: websocket\r\n" ++
                    "Sec-WebSocket-Accept: {s}\r\n" ++
                    "\r\n";
                
                var response_buf: [256]u8 = undefined;
                const response = std.fmt.bufPrint(&response_buf, response_headers, .{accept_key}) catch continue;
                connection.stream.writeAll(response) catch continue;

                // Handle WebSocket messages
                while (true) {
                    const frame = readFrame(self.allocator, connection.stream) catch break;
                    defer if (frame.data.len > 0) self.allocator.free(frame.data);
                    
                    switch (frame.opcode) {
                        .text => {
                            // Echo the text back
                            sendFrame(connection.stream, .text, frame.data) catch break;
                        },
                        .binary => {
                            // Echo the binary back
                            sendFrame(connection.stream, .binary, frame.data) catch break;
                        },
                        .close => {
                            sendFrame(connection.stream, .close, &[_]u8{}) catch {};
                            break;
                        },
                        .ping => {
                            sendFrame(connection.stream, .pong, frame.data) catch break;
                        },
                        .pong => {},
                        else => {},
                    }
                }
            }
        }

        const Opcode = enum(u8) {
            continuation = 0x0,
            text = 0x1,
            binary = 0x2,
            close = 0x8,
            ping = 0x9,
            pong = 0xA,
        };

        const Frame = struct {
            opcode: Opcode,
            data: []u8,
        };

        fn readFrame(allocator_: std.mem.Allocator, stream: std.net.Stream) !Frame {
            var header: [2]u8 = undefined;
            _ = try stream.readAll(&header);

            const opcode = @as(Opcode, @enumFromInt(header[0] & 0x0F));
            const masked = (header[1] & 0x80) != 0;
            var payload_len: u64 = header[1] & 0x7F;

            if (payload_len == 126) {
                var len_bytes: [2]u8 = undefined;
                _ = try stream.readAll(&len_bytes);
                payload_len = std.mem.readInt(u16, &len_bytes, .big);
            } else if (payload_len == 127) {
                var len_bytes: [8]u8 = undefined;
                _ = try stream.readAll(&len_bytes);
                payload_len = std.mem.readInt(u64, &len_bytes, .big);
            }

            var mask_key: [4]u8 = undefined;
            if (masked) {
                _ = try stream.readAll(&mask_key);
            }

            var data = if (payload_len > 0) try allocator_.alloc(u8, payload_len) else try allocator_.alloc(u8, 0);
            errdefer allocator_.free(data);
            
            if (payload_len > 0) {
                _ = try stream.readAll(data);

                if (masked) {
                    var i: usize = 0;
                    while (i < data.len) : (i += 1) {
                        data[i] ^= mask_key[i % 4];
                    }
                }
            }

            return Frame{
                .opcode = opcode,
                .data = data,
            };
        }

        fn sendFrame(stream: std.net.Stream, opcode: Opcode, payload: []const u8) !void {
            var header: [10]u8 = undefined;
            var header_len: usize = 2;

            // FIN = 1, RSV = 0, Opcode
            header[0] = 0x80 | @intFromEnum(opcode);

            // Mask = 0 (server doesn't mask), payload length
            if (payload.len < 126) {
                header[1] = @as(u8, @intCast(payload.len));
            } else if (payload.len < 65536) {
                header[1] = 126;
                std.mem.writeInt(u16, header[2..4], @as(u16, @intCast(payload.len)), .big);
                header_len = 4;
            } else {
                header[1] = 127;
                std.mem.writeInt(u64, header[2..10], payload.len, .big);
                header_len = 10;
            }

            // Send header
            try stream.writeAll(header[0..header_len]);

            // Send payload (unmasked for server)
            try stream.writeAll(payload);
        }
    };

    const TestClient = struct {
        fn connect(allocator_: std.mem.Allocator, port: u16) !void {
            const address = try std.net.Address.parseIp("127.0.0.1", port);
            const stream = try std.net.tcpConnectToAddress(address);
            defer stream.close();

            // Generate a random WebSocket key
            var key_bytes: [16]u8 = undefined;
            std.crypto.random.bytes(&key_bytes);
            var key_base64: [24]u8 = undefined;
            _ = std.base64.standard.Encoder.encode(&key_base64, &key_bytes);

            // Send WebSocket upgrade request
            const request = try std.fmt.allocPrint(allocator_, 
                "GET / HTTP/1.1\r\n" ++
                "Host: localhost:{d}\r\n" ++
                "Upgrade: websocket\r\n" ++
                "Connection: Upgrade\r\n" ++
                "Sec-WebSocket-Key: {s}\r\n" ++
                "Sec-WebSocket-Version: 13\r\n" ++
                "\r\n", .{ port, key_base64 });
            defer allocator_.free(request);

            try stream.writeAll(request);

            // Read response
            var response_buf: [1024]u8 = undefined;
            const response_len = try stream.read(&response_buf);
            const response = response_buf[0..response_len];

            // Verify it's a 101 Switching Protocols response
            try testing.expect(std.mem.indexOf(u8, response, "101 Switching Protocols") != null);
            try testing.expect(std.mem.indexOf(u8, response, "Upgrade: websocket") != null);

            // Send a text message
            const test_message = "Hello, WebSocket!";
            try sendTextFrame(allocator_, stream, test_message);

            // Read the echo response
            const echo_msg = try readFrame(allocator_, stream);
            defer allocator_.free(echo_msg.data);
            try testing.expectEqual(Opcode.text, echo_msg.opcode);
            try testing.expectEqualStrings(test_message, echo_msg.data);

            // Send a binary message
            const binary_data = [_]u8{ 0x01, 0x02, 0x03, 0x04, 0x05 };
            try sendBinaryFrame(allocator_, stream, &binary_data);

            // Read the binary echo
            const binary_echo = try readFrame(allocator_, stream);
            defer allocator_.free(binary_echo.data);
            try testing.expectEqual(Opcode.binary, binary_echo.opcode);
            try testing.expectEqualSlices(u8, &binary_data, binary_echo.data);

            // Send close frame
            try sendCloseFrame(allocator_, stream);
        }

        const Opcode = enum(u8) {
            continuation = 0x0,
            text = 0x1,
            binary = 0x2,
            close = 0x8,
            ping = 0x9,
            pong = 0xA,
        };

        const Frame = struct {
            opcode: Opcode,
            data: []u8,
        };

        fn sendTextFrame(allocator_: std.mem.Allocator, stream: std.net.Stream, text: []const u8) !void {
            try sendFrame(allocator_, stream, .text, text);
        }

        fn sendBinaryFrame(allocator_: std.mem.Allocator, stream: std.net.Stream, data: []const u8) !void {
            try sendFrame(allocator_, stream, .binary, data);
        }

        fn sendCloseFrame(allocator_: std.mem.Allocator, stream: std.net.Stream) !void {
            try sendFrame(allocator_, stream, .close, &[_]u8{});
        }

        fn sendFrame(allocator_: std.mem.Allocator, stream: std.net.Stream, opcode: Opcode, payload: []const u8) !void {
            var header: [14]u8 = undefined;
            var header_len: usize = 2;

            // FIN = 1, RSV = 0, Opcode
            header[0] = 0x80 | @intFromEnum(opcode);

            // Mask = 1 (client must mask), payload length
            if (payload.len < 126) {
                header[1] = 0x80 | @as(u8, @intCast(payload.len));
            } else if (payload.len < 65516) {
                header[1] = 0x80 | 126;
                std.mem.writeInt(u16, header[2..4], @as(u16, @intCast(payload.len)), .big);
                header_len = 4;
            } else {
                header[1] = 0x80 | 127;
                std.mem.writeInt(u64, header[2..10], payload.len, .big);
                header_len = 10;
            }

            // Masking key (required for client)
            var mask_key: [4]u8 = undefined;
            std.crypto.random.bytes(&mask_key);
            @memcpy(header[header_len..header_len + 4], &mask_key);
            header_len += 4;

            // Send header
            try stream.writeAll(header[0..header_len]);

            // Send masked payload
            if (payload.len > 0) {
                var masked_payload = try allocator_.alloc(u8, payload.len);
                defer allocator_.free(masked_payload);
                
                var i: usize = 0;
                while (i < payload.len) : (i += 1) {
                    masked_payload[i] = payload[i] ^ mask_key[i % 4];
                }
                try stream.writeAll(masked_payload);
            }
        }

        fn readFrame(allocator_: std.mem.Allocator, stream: std.net.Stream) !Frame {
            var header: [2]u8 = undefined;
            _ = try stream.readAll(&header);

            const opcode = @as(Opcode, @enumFromInt(header[0] & 0x0F));
            const masked = (header[1] & 0x80) != 0;
            var payload_len: u64 = header[1] & 0x7F;

            if (payload_len == 126) {
                var len_bytes: [2]u8 = undefined;
                _ = try stream.readAll(&len_bytes);
                payload_len = std.mem.readInt(u16, &len_bytes, .big);
            } else if (payload_len == 127) {
                var len_bytes: [8]u8 = undefined;
                _ = try stream.readAll(&len_bytes);
                payload_len = std.mem.readInt(u64, &len_bytes, .big);
            }

            var mask_key: [4]u8 = undefined;
            if (masked) {
                _ = try stream.readAll(&mask_key);
            }

            const data = try allocator_.alloc(u8, payload_len);
            errdefer allocator_.free(data);
            if (payload_len > 0) {
                _ = try stream.readAll(data);

                if (masked) {
                    var i: usize = 0;
                    while (i < data.len) : (i += 1) {
                        data[i] ^= mask_key[i % 4];
                    }
                }
            }

            return Frame{
                .opcode = opcode,
                .data = data,
            };
        }
    };

    var test_server = try TestServer.init(allocator);
    defer test_server.stop();
    try test_server.start();

    std.time.sleep(50 * std.time.ns_per_ms);

    try TestClient.connect(allocator, test_server.port);
}