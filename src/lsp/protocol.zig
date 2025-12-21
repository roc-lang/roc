//! LSP protocol types and JSON-RPC message structures for client-server communication.

const std = @import("std");

/// LSP error codes mirrored from the specification.
pub const ErrorCode = enum(i32) {
    parse_error = -32700,
    invalid_request = -32600,
    method_not_found = -32601,
    invalid_params = -32602,
    internal_error = -32603,
    server_not_initialized = -32002,
    request_failed = -32003,
    server_cancelled = -32802,
    content_modified = -32801,
    request_cancelled = -32800,

    pub fn jsonStringify(self: ErrorCode, writer: anytype) !void {
        try writer.write(@intFromEnum(self));
    }
};

/// Standard JSON-RPC error envelope used in Roc responses.
pub const ResponseError = struct {
    code: ErrorCode,
    message: []const u8,
    data: ?[]const u8 = null,
};

/// JSON-RPC identifier that can be an integer or string.
pub const JsonId = union(enum) {
    integer: i64,
    string: []u8,

    pub fn fromJsonValue(allocator: std.mem.Allocator, value: std.json.Value) !JsonId {
        return switch (value) {
            .integer => |num| .{ .integer = num },
            .float => return error.InvalidIdType,
            .string => |text| .{ .string = try copyString(allocator, text) },
            else => error.InvalidIdType,
        };
    }

    pub fn clone(self: JsonId, allocator: std.mem.Allocator) !JsonId {
        return switch (self) {
            .integer => |num| .{ .integer = num },
            .string => |text| .{ .string = try copyString(allocator, text) },
        };
    }

    pub fn deinit(self: *JsonId, allocator: std.mem.Allocator) void {
        switch (self.*) {
            .string => |slice| allocator.free(slice),
            else => {},
        }
        self.* = undefined;
    }

    pub fn jsonStringify(self: JsonId, writer: anytype) !void {
        switch (self) {
            .integer => |num| try writer.write(num),
            .string => |text| try writer.write(text),
        }
    }
};

/// Client metadata reported during initialization.
pub const ClientInfo = struct {
    name: []u8,
    version: ?[]u8 = null,

    pub fn deinit(self: *ClientInfo, allocator: std.mem.Allocator) void {
        allocator.free(self.name);
        if (self.version) |ver| allocator.free(ver);
        self.* = undefined;
    }

    pub fn jsonStringify(self: ClientInfo, writer: anytype) !void {
        try writer.write(.{
            .name = self.name,
            .version = self.version,
        });
    }
};

/// Parsed arguments from the client's `initialize` request.
pub const InitializeParams = struct {
    process_id: ?i64 = null,
    root_uri: ?[]u8 = null,
    client_info: ?ClientInfo = null,
    capabilities_json: ?[]u8 = null,

    pub fn fromJson(allocator: std.mem.Allocator, value: std.json.Value) !InitializeParams {
        const obj = switch (value) {
            .object => |o| o,
            else => return error.InvalidParams,
        };

        var params = InitializeParams{};

        if (obj.get("processId")) |pid_node| {
            params.process_id = switch (pid_node) {
                .integer => |num| num,
                .null => null,
                else => return error.InvalidParams,
            };
        }

        if (obj.get("rootUri")) |uri_node| {
            const uri_text = switch (uri_node) {
                .string => |text| text,
                .null => null,
                else => return error.InvalidParams,
            };
            if (uri_text) |text| {
                params.root_uri = try copyString(allocator, text);
            }
        }

        if (obj.get("clientInfo")) |client_node| {
            const client_obj = switch (client_node) {
                .object => |o| o,
                else => return error.InvalidParams,
            };
            const name_value = client_obj.get("name") orelse return error.InvalidParams;
            const name_string = switch (name_value) {
                .string => |text| text,
                else => return error.InvalidParams,
            };

            var info = ClientInfo{
                .name = try copyString(allocator, name_string),
            };

            if (client_obj.get("version")) |ver_node| {
                const ver_text = switch (ver_node) {
                    .string => |text| try copyString(allocator, text),
                    .null => null,
                    else => return error.InvalidParams,
                };
                info.version = ver_text;
            }

            params.client_info = info;
        }

        if (obj.get("capabilities")) |caps_node| {
            params.capabilities_json = try stringifyValue(allocator, caps_node);
        }

        return params;
    }

    pub fn deinit(self: *InitializeParams, allocator: std.mem.Allocator) void {
        if (self.root_uri) |uri| allocator.free(uri);
        if (self.client_info) |*info| info.deinit(allocator);
        if (self.capabilities_json) |caps| allocator.free(caps);
        self.* = undefined;
    }

    pub fn moveInto(self: *InitializeParams, state: *ClientState) void {
        state.process_id = self.process_id;
        state.root_uri = self.root_uri;
        state.client_info = self.client_info;
        state.capabilities_json = self.capabilities_json;

        self.process_id = null;
        self.root_uri = null;
        self.client_info = null;
        self.capabilities_json = null;
    }
};

/// Snapshot of the last `initialize` data retained by the server.
pub const ClientState = struct {
    process_id: ?i64 = null,
    root_uri: ?[]u8 = null,
    client_info: ?ClientInfo = null,
    capabilities_json: ?[]u8 = null,

    pub fn deinit(self: *ClientState, allocator: std.mem.Allocator) void {
        if (self.root_uri) |uri| allocator.free(uri);
        if (self.client_info) |*info| info.deinit(allocator);
        if (self.capabilities_json) |caps| allocator.free(caps);
        self.* = .{};
    }
};

/// Metadata describing the Roc language server.
pub const ServerInfo = struct {
    name: []const u8,
    version: ?[]const u8 = null,
};

/// Capabilities advertised back to the editor.
pub const ServerCapabilities = @import("capabilities.zig").ServerCapabilities;

/// Response body returned after a successful initialization.
pub const InitializeResult = struct {
    capabilities: ServerCapabilities = .{},
    serverInfo: ServerInfo,
};

fn copyString(allocator: std.mem.Allocator, text: []const u8) ![]u8 {
    const buf = try allocator.alloc(u8, text.len);
    @memcpy(buf, text);
    return buf;
}

fn stringifyValue(allocator: std.mem.Allocator, value: std.json.Value) ![]u8 {
    var writer: std.io.Writer.Allocating = .init(allocator);
    defer writer.deinit();
    std.json.Stringify.value(value, .{}, &writer.writer) catch return error.OutOfMemory;
    return writer.toOwnedSlice();
}
