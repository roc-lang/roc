const std = @import("std");
const testing = std.testing;

pub fn download(allocator: std.mem.Allocator, url: []const u8) ![]const u8 {
    var client = std.http.Client{ .allocator = allocator };
    defer client.deinit();

    const uri = try std.Uri.parse(url);

    var header_buffer: [16 * 1024]u8 = undefined;
    var request = try client.open(.GET, uri, .{ .server_header_buffer = &header_buffer });
    defer request.deinit();

    try request.send();
    try request.wait();

    if (request.response.status != .ok) {
        return error.DownloadFailed;
    }

    const body = try request.reader().readAllAlloc(allocator, 10 * 1024 * 1024);
    errdefer allocator.free(body);

    const temp_dir_name = try std.fmt.allocPrint(allocator, "download_tmp_{d}", .{std.time.milliTimestamp()});
    defer allocator.free(temp_dir_name);
    
    try std.fs.cwd().makePath(temp_dir_name);
    errdefer std.fs.cwd().deleteTree(temp_dir_name) catch {};

    const filename = std.fs.path.basename(uri.path.percent_encoded);
    const file_path = try std.fs.path.join(allocator, &.{ temp_dir_name, filename });
    defer allocator.free(file_path);

    var temp_dir = try std.fs.cwd().openDir(temp_dir_name, .{});
    defer temp_dir.close();

    const file = try temp_dir.createFile(filename, .{});
    defer file.close();

    try file.writeAll(body);
    allocator.free(body);

    const result = try allocator.dupe(u8, file_path);
    return result;
}

test "download file from local server" {
    const allocator = testing.allocator;

    const TestServer = struct {
        server: std.net.Server,
        thread: std.Thread,
        port: u16,
        should_stop: std.atomic.Value(bool),

        fn init() !@This() {
            var attempts: u32 = 0;
            while (attempts < 20) : (attempts += 1) {
                const port = if (attempts == 0) 0 else @as(u16, @intCast(8000 + attempts));
                const address = std.net.Address.parseIp("127.0.0.1", port) catch continue;
                const server = address.listen(.{}) catch |err| {
                    if (err == error.AddressInUse) continue;
                    return err;
                };
                const actual_port = server.listen_address.in.getPort();

                return .{
                    .server = server,
                    .thread = undefined,
                    .port = actual_port,
                    .should_stop = std.atomic.Value(bool).init(false),
                };
            }
            return error.NoAvailablePort;
        }

        fn start(self: *@This()) !void {
            self.thread = try std.Thread.spawn(.{}, serve, .{self});
        }

        fn stop(self: *@This()) void {
            self.should_stop.store(true, .seq_cst);
            self.server.deinit();
            self.thread.join();
        }

        fn serve(self: *@This()) void {
            while (!self.should_stop.load(.seq_cst)) {
                var mut_server = self.server;
                const connection = mut_server.accept() catch |err| {
                    if (err == error.SocketNotListening) return;
                    continue;
                };
                defer connection.stream.close();

                var buf: [4096]u8 = undefined;
                const request_len = connection.stream.read(&buf) catch continue;
                if (request_len == 0) continue;

                const response_body = "Hello, World!";
                var response_buf: [512]u8 = undefined;
                const response = std.fmt.bufPrint(&response_buf, 
                    "HTTP/1.1 200 OK\r\n" ++
                    "Content-Type: text/plain\r\n" ++
                    "Content-Length: {d}\r\n" ++
                    "Connection: close\r\n" ++
                    "\r\n" ++
                    "{s}", .{ response_body.len, response_body }
                ) catch continue;

                connection.stream.writeAll(response) catch continue;
            }
        }
    };

    var test_server = try TestServer.init();
    defer test_server.stop();
    try test_server.start();

    std.time.sleep(50 * std.time.ns_per_ms);

    const url = try std.fmt.allocPrint(allocator, "http://127.0.0.1:{d}/test.txt", .{test_server.port});
    defer allocator.free(url);

    const downloaded_path = try download(allocator, url);
    defer allocator.free(downloaded_path);

    const content = try std.fs.cwd().readFileAlloc(allocator, downloaded_path, 1024);
    defer allocator.free(content);

    try testing.expectEqualStrings("Hello, World!", content);
    try testing.expect(std.mem.endsWith(u8, downloaded_path, "test.txt"));

    const dir_path = std.fs.path.dirname(downloaded_path).?;
    try std.fs.cwd().deleteTree(dir_path);
}