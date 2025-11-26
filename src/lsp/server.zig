const std = @import("std");
const builtin = @import("builtin");
const protocol = @import("protocol.zig");
const makeTransport = @import("transport.zig").Transport;
const DocumentStore = @import("document_store.zig").DocumentStore;
const initialize_handler_mod = @import("handlers/initialize.zig");
const shutdown_handler_mod = @import("handlers/shutdown.zig");
const did_open_handler_mod = @import("handlers/did_open.zig");
const did_change_handler_mod = @import("handlers/did_change.zig");

const log = std.log.scoped(.roc_lsp_server);

/// Factory for the Roc LSP server. Handles the state and request handlers.
pub fn Server(comptime ReaderType: type, comptime WriterType: type) type {
    return struct {
        const Self = @This();
        const TransportType = makeTransport(ReaderType, WriterType);
        const HandlerFn = fn (*Self, *protocol.JsonId, ?std.json.Value) anyerror!void;
        const HandlerPtr = *const HandlerFn;
        const NotificationFn = fn (*Self, ?std.json.Value) anyerror!void;
        const NotificationPtr = *const NotificationFn;
        const InitializeHandler = initialize_handler_mod.handler(Self);
        const ShutdownHandler = shutdown_handler_mod.handler(Self);
        const request_handlers = std.StaticStringMap(HandlerPtr).initComptime(.{
            .{ "initialize", &InitializeHandler.call },
            .{ "shutdown", &ShutdownHandler.call },
        });
        const DidOpenHandler = did_open_handler_mod.handler(Self);
        const DidChangeHandler = did_change_handler_mod.handler(Self);
        const notification_handlers = std.StaticStringMap(NotificationPtr).initComptime(.{});

        allocator: std.mem.Allocator,
        transport: TransportType,
        client: protocol.ClientState = .{},
        state: State = .waiting_for_initialize,
        doc_store: DocumentStore,

        pub const server_name = "roc-lsp";
        pub const version = "0.1";

        pub const State = enum {
            waiting_for_initialize,
            waiting_for_initialized,
            running,
            shutdown,
            exit_success,
            exit_failure,
        };

        pub fn init(allocator: std.mem.Allocator, reader: ReaderType, writer: WriterType, log_file: ?std.fs.File) !Self {
            return .{
                .allocator = allocator,
                .transport = TransportType.init(allocator, reader, writer, log_file),
                .doc_store = DocumentStore.init(allocator),
            };
        }

        pub fn deinit(self: *Self) void {
            self.client.deinit(self.allocator);
            self.transport.deinit();
            self.doc_store.deinit();
        }

        pub fn run(self: *Self) !void {
            while (try self.processNextMessage()) {}
        }

        fn processNextMessage(self: *Self) !bool {
            if (self.state == .exit_success or self.state == .exit_failure) {
                return false;
            }

            const payload = self.transport.readMessage() catch |err| switch (err) {
                error.EndOfStream => return false,
                else => return err,
            };
            defer self.allocator.free(payload);

            self.handlePayload(payload) catch |err| {
                log.err("failed to process message: {s}", .{@errorName(err)});
            };

            return switch (self.state) {
                .exit_success, .exit_failure => false,
                else => true,
            };
        }

        fn handlePayload(self: *Self, payload: []u8) !void {
            var parsed = try std.json.parseFromSlice(std.json.Value, self.allocator, payload, .{});
            defer parsed.deinit();

            const root = parsed.value;
            const obj = switch (root) {
                .object => |o| o,
                else => {
                    log.err("received non-object JSON-RPC message", .{});
                    return;
                },
            };

            const method_value = obj.get("method") orelse return;
            const method = switch (method_value) {
                .string => |text| text,
                else => return,
            };

            if (obj.get("id")) |id_node| {
                var id = try protocol.JsonId.fromJsonValue(self.allocator, id_node);
                defer id.deinit(self.allocator);

                try self.handleRequest(method, &id, obj.get("params"));
            } else {
                try self.handleNotification(method, obj.get("params"));
            }
        }

        fn handleRequest(self: *Self, method: []const u8, id: *protocol.JsonId, maybe_params: ?std.json.Value) !void {
            if (request_handlers.get(method)) |handler| {
                try handler(self, id, maybe_params);
                return;
            }

            try self.sendError(id, .method_not_found, "method not implemented");
        }

        fn handleNotification(self: *Self, method: []const u8, params: ?std.json.Value) !void {
            if (std.mem.eql(u8, method, "initialized")) {
                if (self.state == .waiting_for_initialized) {
                    self.state = .running;
                }
                return;
            }

            if (std.mem.eql(u8, method, "exit")) {
                self.state = if (self.state == .shutdown) .exit_success else .exit_failure;
                return;
            }

            if (notification_handlers.get(method)) |handler| {
                handler(self, params) catch |err| {
                    log.err("notification handler {s} failed: {s}", .{ method, @errorName(err) });
                };
                return;
            }

            // Other notifications are ignored until server capabilities are implemented.
        }

        pub fn sendNullResponse(self: *Self, id: *protocol.JsonId) !void {
            const Response = struct {
                jsonrpc: []const u8 = "2.0",
                id: protocol.JsonId,
                result: std.json.Value,
            };

            try self.transport.sendJson(Response{
                .id = id.*,
                .result = .{ .null = {} },
            });
        }

        pub fn sendError(self: *Self, id: *protocol.JsonId, code: protocol.ErrorCode, message: []const u8) !void {
            const Response = struct {
                jsonrpc: []const u8 = "2.0",
                id: protocol.JsonId,
                @"error": protocol.ResponseError,
            };

            try self.transport.sendJson(Response{
                .id = id.*,
                .@"error" = .{ .code = code, .message = message },
            });
        }

        pub fn sendResponse(self: *Self, id: *protocol.JsonId, result: anytype) !void {
            const Response = struct {
                jsonrpc: []const u8 = "2.0",
                id: protocol.JsonId,
                result: @TypeOf(result),
            };

            try self.transport.sendJson(Response{
                .id = id.*,
                .result = result,
            });
        }

        /// Returns the stored document (testing helper; returns null outside tests).
        pub fn getDocumentForTesting(self: *Self, uri: []const u8) ?DocumentStore.Document {
            if (!builtin.is_test) return null;
            return self.doc_store.get(uri);
        }
    };
}

/// Launches the LSP server wired to stdin/stdout, optionally mirroring traffic to disk.
pub fn runWithStdIo(allocator: std.mem.Allocator, enable_logging: bool) !void {
    var stdin_file = std.fs.File.stdin();
    var stdout_file = std.fs.File.stdout();

    var stdin_buffer: [4096]u8 = undefined;
    var stdout_buffer: [4096]u8 = undefined;
    const reader = stdin_file.readerStreaming(&stdin_buffer);
    const writer = stdout_file.writerStreaming(&stdout_buffer);

    var log_file: ?std.fs.File = null;
    if (enable_logging) {
        const log_info = try createLogFile(allocator);
        log_file = log_info.file;
        const stderr_file = std.fs.File.stderr();
        stderr_file.writeAll("roc-lsp logging to ") catch {};
        stderr_file.writeAll(log_info.path) catch {};
        stderr_file.writeAll("\n") catch {};
        allocator.free(log_info.path);
        const divider = "\n===== roc-lsp session start =====\n";
        log_file.?.writeAll(divider) catch {};
        log_file.?.writeAll("\n") catch {};
        log_file.?.sync() catch {};
    }

    const StdServer = Server(@TypeOf(reader), @TypeOf(writer));
    var server = try StdServer.init(allocator, reader, writer, log_file);
    defer server.deinit();
    try server.run();
}

const LogFileInfo = struct {
    file: std.fs.File,
    path: []u8,
};

fn createLogFile(allocator: std.mem.Allocator) !LogFileInfo {
    const dir_path = try resolveTempDir(allocator);
    defer allocator.free(dir_path);
    const filename = try allocator.dupe(u8, "roc-lsp-debug.log");
    defer allocator.free(filename);
    const absolute_path = try std.fs.path.resolve(allocator, &.{ dir_path, filename });
    const file = std.fs.createFileAbsolute(absolute_path, .{
        .truncate = false,
        .read = true,
        .mode = 0o600,
    }) catch |err| switch (err) {
        error.PathAlreadyExists => try std.fs.openFileAbsolute(absolute_path, .{
            .mode = .read_write,
        }),
        else => return err,
    };
    try file.seekFromEnd(0);
    return .{ .file = file, .path = absolute_path };
}

fn resolveTempDir(allocator: std.mem.Allocator) ![]u8 {
    const env_names = if (builtin.os.tag == .windows)
        [_][]const u8{ "TMP", "TEMP", "LOCALAPPDATA" }
    else
        [_][]const u8{ "TMPDIR", "TMP", "TEMP" };

    for (env_names) |name| {
        const value = std.process.getEnvVarOwned(allocator, name) catch |err| switch (err) {
            error.EnvironmentVariableNotFound => continue,
            else => return err,
        };
        return value;
    }

    if (builtin.os.tag == .windows) {
        return try allocator.dupe(u8, ".");
    } else {
        return try allocator.dupe(u8, "/tmp");
    }
}
