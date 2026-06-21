const std = @import("std");
const builtin = @import("builtin");

pub const std_options: std.Options = .{
    .allow_stack_tracing = false,
};

pub const panic = std.debug.FullPanic(panicImpl);

const request_buffer_size = 2 * 1024;
const max_body_len = 1024;
const listen_backlog = 16;
const stdout_fd: c_int = 1;
const stderr_fd: c_int = 2;

const ParseError = error{
    IncompleteHeaders,
    InvalidUtf8,
    BadRequestLine,
    BadProtocol,
    BadHeader,
    BadContentLength,
    DuplicateContentLength,
    RequestTooLarge,
    ConnectionClosed,
};

const ServerError = ParseError || error{
    UnsupportedOperatingSystem,
    SocketFailed,
    SetSockOptFailed,
    BindFailed,
    ListenFailed,
    GetSockNameFailed,
    AcceptFailed,
    RecvFailed,
    SendFailed,
    WriteFailed,
};

const ParsedRequest = struct {
    path: []const u8,
    protocol: []const u8,
    body_len: usize,
    header_bytes: usize,
    total_bytes: usize,
    header_block: []const u8,
};

const ParsedHead = struct {
    path: []const u8,
    protocol: []const u8,
    body_len: usize,
    header_start: usize,
    header_end: usize,
    header_bytes: usize,
};

const RocStr = extern struct {
    bytes: ?[*]u8,
    capacity_or_alloc_ptr: usize,
    length: usize,

    const small_bit: usize = @as(usize, 1) << (@bitSizeOf(usize) - 1);
    const seamless_slice_tag: usize = 1;

    fn empty() RocStr {
        return .{
            .bytes = null,
            .capacity_or_alloc_ptr = 0,
            .length = small_bit,
        };
    }

    fn borrowed(slice: []const u8) RocStr {
        if (slice.len < @sizeOf(RocStr)) {
            var result = RocStr.empty();
            const out: [*]u8 = @ptrCast(&result);
            @memcpy(out[0..slice.len], slice);
            out[@sizeOf(RocStr) - 1] = @intCast(slice.len | 0x80);
            return result;
        }

        return .{
            .bytes = @constCast(slice.ptr),
            .capacity_or_alloc_ptr = @intFromPtr(staticBorrowedDataPtr()) | seamless_slice_tag,
            .length = slice.len,
        };
    }
};

const StaticBorrowedData = extern struct {
    refcount: isize,
    data: u8,
};

const windows_ws2_32_directive: [19]u8 linksection(".drectve") = "/DEFAULTLIB:ws2_32 ".*;

var static_borrowed_data: StaticBorrowedData align(@alignOf(usize)) = .{
    .refcount = 0,
    .data = 0,
};

fn staticBorrowedDataPtr() [*]u8 {
    return @as([*]u8, @ptrCast(&static_borrowed_data.data));
}

extern fn roc_main(headers: RocStr) callconv(.c) u64;

comptime {
    @export(&hostAlloc, .{ .name = "roc_alloc", .visibility = .hidden });
    @export(&hostDealloc, .{ .name = "roc_dealloc", .visibility = .hidden });
    @export(&hostRealloc, .{ .name = "roc_realloc", .visibility = .hidden });
    @export(&hostDbg, .{ .name = "roc_dbg", .visibility = .hidden });
    @export(&hostExpectFailed, .{ .name = "roc_expect_failed", .visibility = .hidden });
    @export(&hostCrashed, .{ .name = "roc_crashed", .visibility = .hidden });

    switch (builtin.os.tag) {
        .linux => {
            if (builtin.cpu.arch == .x86_64) {
                @export(&linuxStartX86_64, .{ .name = "_start" });
                @export(&linuxStartMain, .{ .name = "roc_http_linux_start_main", .visibility = .hidden });
            } else {
                @export(&linuxStart, .{ .name = "_start" });
            }
            @export(&linuxMemcpy, .{ .name = "memcpy", .visibility = .hidden });
            @export(&linuxMemmove, .{ .name = "memmove", .visibility = .hidden });
            @export(&linuxMemset, .{ .name = "memset", .visibility = .hidden });
        },
        .macos, .windows => {
            @export(&cMain, .{ .name = "main" });
            if (builtin.os.tag == .windows) {
                @export(&windowsMainStub, .{ .name = "__main" });
                @export(&windows_ws2_32_directive, .{ .name = "windows_ws2_32_directive" });
            }
        },
        else => {},
    }
}

fn panicImpl(msg: []const u8, addr: ?usize) noreturn {
    _ = addr;
    rawWriteStderr("zig panic: ");
    rawWriteStderr(msg);
    rawWriteStderr("\n");
    abortProcess();
}

fn hostAlloc(length: usize, alignment: usize) callconv(.c) ?*anyopaque {
    _ = length;
    _ = alignment;
    rawWriteStderr("roc_alloc called\n");
    abortProcess();
}

fn hostDealloc(ptr: *anyopaque, alignment: usize) callconv(.c) void {
    _ = ptr;
    _ = alignment;
    rawWriteStderr("roc_dealloc called\n");
    abortProcess();
}

fn hostRealloc(ptr: *anyopaque, new_length: usize, alignment: usize) callconv(.c) ?*anyopaque {
    _ = ptr;
    _ = new_length;
    _ = alignment;
    rawWriteStderr("roc_realloc called\n");
    abortProcess();
}

fn hostDbg(bytes: [*]const u8, len: usize) callconv(.c) void {
    rawWriteStderr(bytes[0..len]);
    rawWriteStderr("\n");
}

fn hostExpectFailed(bytes: [*]const u8, len: usize) callconv(.c) void {
    rawWriteStderr("roc_expect_failed: ");
    rawWriteStderr(bytes[0..len]);
    rawWriteStderr("\n");
    abortProcess();
}

fn hostCrashed(bytes: [*]const u8, len: usize) callconv(.c) void {
    rawWriteStderr("roc_crashed: ");
    rawWriteStderr(bytes[0..len]);
    rawWriteStderr("\n");
    abortProcess();
}

fn windowsMainStub() callconv(.c) void {}

fn cMain(argc: c_int, argv: [*][*:0]u8) callconv(.c) c_int {
    _ = argc;
    _ = argv;
    return serverMain();
}

fn linuxStart() callconv(.c) noreturn {
    if (comptime builtin.os.tag != .linux) unreachable;
    linux.exitGroup(serverMain());
}

fn linuxStartMain() callconv(.c) c_int {
    return serverMain();
}

fn linuxStartX86_64() callconv(.naked) noreturn {
    asm volatile (
        \\call roc_http_linux_start_main
        \\movl %%eax, %%edi
        \\movl $231, %%eax
        \\syscall
        \\ud2
    );
}

fn linuxMemcpy(dest: ?*anyopaque, src: ?*const anyopaque, len: usize) callconv(.c) ?*anyopaque {
    const dest_ptr: [*]u8 = @ptrCast(dest.?);
    const src_ptr: [*]const u8 = @ptrCast(src.?);
    var i: usize = 0;
    while (i < len) : (i += 1) {
        dest_ptr[i] = src_ptr[i];
    }
    return dest;
}

fn linuxMemmove(dest: ?*anyopaque, src: ?*const anyopaque, len: usize) callconv(.c) ?*anyopaque {
    const dest_ptr: [*]u8 = @ptrCast(dest.?);
    const src_ptr: [*]const u8 = @ptrCast(src.?);

    if (@intFromPtr(dest_ptr) <= @intFromPtr(src_ptr)) {
        var i: usize = 0;
        while (i < len) : (i += 1) {
            dest_ptr[i] = src_ptr[i];
        }
    } else {
        var i = len;
        while (i != 0) {
            i -= 1;
            dest_ptr[i] = src_ptr[i];
        }
    }

    return dest;
}

fn linuxMemset(dest: ?*anyopaque, byte: c_int, len: usize) callconv(.c) ?*anyopaque {
    const dest_ptr: [*]u8 = @ptrCast(dest.?);
    const value: u8 = @truncate(@as(c_uint, @bitCast(byte)));
    var i: usize = 0;
    while (i < len) : (i += 1) {
        dest_ptr[i] = value;
    }
    return dest;
}

fn serverMain() c_int {
    platformInit() catch |err| return fail(err);

    const listener = listenLoopback() catch |err| return fail(err);
    defer closeSocket(listener.fd);

    var port_buf: [16]u8 = undefined;
    const port_digits = formatU64(listener.port, &port_buf);
    rawWriteStdout(port_digits);
    rawWriteStdout("\n");

    const client = acceptSocket(listener.fd) catch |err| return fail(err);
    defer closeSocket(client);

    var request_buffer: [request_buffer_size]u8 = undefined;
    const request = receiveRequest(client, &request_buffer) catch |err| return fail(err);

    const result = roc_main(RocStr.borrowed(request.header_block));

    sendResponse(client, result) catch |err| return fail(err);
    return 0;
}

fn fail(err: anyerror) c_int {
    rawWriteStderr("http header parser host failed: ");
    rawWriteStderr(@errorName(err));
    rawWriteStderr("\n");
    return 1;
}

fn receiveRequest(fd: Socket, buffer: *[request_buffer_size]u8) ServerError!ParsedRequest {
    var total: usize = 0;

    while (total < buffer.len) {
        const n = try recvSocket(fd, buffer[total..]);
        if (n == 0) return error.ConnectionClosed;
        total += n;

        const head = parseHead(buffer[0..total]) catch |err| switch (err) {
            error.IncompleteHeaders => continue,
            else => |e| return e,
        };

        if (head.body_len > max_body_len) return error.RequestTooLarge;
        if (head.body_len > buffer.len - head.header_bytes) return error.RequestTooLarge;

        const required_total = head.header_bytes + head.body_len;
        if (total < required_total) continue;

        return .{
            .path = head.path,
            .protocol = head.protocol,
            .body_len = head.body_len,
            .header_bytes = head.header_bytes,
            .total_bytes = required_total,
            .header_block = buffer[head.header_start..head.header_end],
        };
    }

    return error.RequestTooLarge;
}

fn parseHead(bytes: []const u8) ParseError!ParsedHead {
    const header_end = findHeaderEnd(bytes) orelse return error.IncompleteHeaders;
    const header_bytes = header_end + 4;
    if (!std.unicode.utf8ValidateSlice(bytes[0..header_bytes])) return error.InvalidUtf8;

    const request_line_end = findCrlf(bytes[0..header_end]) orelse return error.BadRequestLine;
    const request_line = bytes[0..request_line_end];
    const first_space = findByte(request_line, ' ') orelse return error.BadRequestLine;
    const second_space_rel = findByte(request_line[first_space + 1 ..], ' ') orelse return error.BadRequestLine;
    const second_space = first_space + 1 + second_space_rel;
    const path = request_line[first_space + 1 .. second_space];
    const protocol = request_line[second_space + 1 ..];
    if (!bytesEqual(protocol, "HTTP/1.1")) return error.BadProtocol;

    var body_len: usize = 0;
    var saw_body_len_header = false;
    var line_start = request_line_end + 2;
    while (line_start < header_end) {
        const rel_end = findCrlf(bytes[line_start..header_bytes]) orelse return error.BadHeader;
        const line_end = line_start + rel_end;
        const line = bytes[line_start..line_end];

        if (line.len != 0) {
            if (findByte(line, ':')) |colon| {
                const name = line[0..colon];
                if (asciiEqualIgnoreCase(name, "Content-Length")) {
                    if (saw_body_len_header) return error.DuplicateContentLength;
                    body_len = try parseContentLength(line[colon + 1 ..]);
                    saw_body_len_header = true;
                }
            }
        }

        line_start = line_end + 2;
    }

    return .{
        .path = path,
        .protocol = protocol,
        .body_len = body_len,
        .header_start = request_line_end + 2,
        .header_end = header_end,
        .header_bytes = header_bytes,
    };
}

fn parseContentLength(raw: []const u8) ParseError!usize {
    var i: usize = 0;
    while (i < raw.len and (raw[i] == ' ' or raw[i] == '\t')) : (i += 1) {}
    if (i == raw.len) return error.BadContentLength;

    var value: usize = 0;
    var saw_digit = false;
    while (i < raw.len) : (i += 1) {
        const byte = raw[i];
        if (byte >= '0' and byte <= '9') {
            saw_digit = true;
            const digit: usize = byte - '0';
            if (value > (@as(usize, ~@as(usize, 0)) - digit) / 10) return error.BadContentLength;
            value = value * 10 + digit;
        } else if (byte == ' ' or byte == '\t') {
            while (i < raw.len) : (i += 1) {
                if (raw[i] != ' ' and raw[i] != '\t') return error.BadContentLength;
            }
            break;
        } else {
            return error.BadContentLength;
        }
    }

    if (!saw_digit) return error.BadContentLength;
    return value;
}

fn sendResponse(fd: Socket, value: u64) ServerError!void {
    var body_buf: [32]u8 = undefined;
    const body = formatU64(value, &body_buf);

    var header_buf: [128]u8 = undefined;
    var header_len: usize = 0;
    append(&header_buf, &header_len, "HTTP/1.1 200 OK\r\n");
    append(&header_buf, &header_len, "Content-Type: text/plain\r\n");
    append(&header_buf, &header_len, "Content-Length: ");
    var len_buf: [16]u8 = undefined;
    append(&header_buf, &header_len, formatU64(body.len, &len_buf));
    append(&header_buf, &header_len, "\r\nConnection: close\r\n\r\n");

    try sendAll(fd, header_buf[0..header_len]);
    try sendAll(fd, body);
}

fn append(buffer: []u8, len: *usize, bytes: []const u8) void {
    @memcpy(buffer[len.* .. len.* + bytes.len], bytes);
    len.* += bytes.len;
}

fn formatU64(value: u64, buffer: []u8) []const u8 {
    var n = value;
    var i = buffer.len;

    if (n == 0) {
        i -= 1;
        buffer[i] = '0';
        return buffer[i..];
    }

    while (n != 0) {
        i -= 1;
        buffer[i] = @intCast('0' + (n % 10));
        n /= 10;
    }

    return buffer[i..];
}

fn findHeaderEnd(bytes: []const u8) ?usize {
    if (bytes.len < 4) return null;
    var i: usize = 0;
    while (i + 3 < bytes.len) : (i += 1) {
        if (bytes[i] == '\r' and bytes[i + 1] == '\n' and bytes[i + 2] == '\r' and bytes[i + 3] == '\n') return i;
    }
    return null;
}

fn findCrlf(bytes: []const u8) ?usize {
    if (bytes.len < 2) return null;
    var i: usize = 0;
    while (i + 1 < bytes.len) : (i += 1) {
        if (bytes[i] == '\r' and bytes[i + 1] == '\n') return i;
    }
    return null;
}

fn findByte(bytes: []const u8, needle: u8) ?usize {
    for (bytes, 0..) |byte, i| {
        if (byte == needle) return i;
    }
    return null;
}

fn bytesEqual(a: []const u8, b: []const u8) bool {
    if (a.len != b.len) return false;
    for (a, b) |a_byte, b_byte| {
        if (a_byte != b_byte) return false;
    }
    return true;
}

fn asciiEqualIgnoreCase(a: []const u8, b: []const u8) bool {
    if (a.len != b.len) return false;
    for (a, b) |a_byte, b_byte| {
        if (asciiLower(a_byte) != asciiLower(b_byte)) return false;
    }
    return true;
}

fn asciiLower(byte: u8) u8 {
    if (byte >= 'A' and byte <= 'Z') return byte + ('a' - 'A');
    return byte;
}

fn platformInit() ServerError!void {
    switch (builtin.os.tag) {
        .windows => try windows.startup(),
        .linux, .macos => {},
        else => return error.UnsupportedOperatingSystem,
    }
}

const Socket = switch (builtin.os.tag) {
    .linux => i32,
    .macos => c_int,
    .windows => windows.SOCKET,
    else => c_int,
};

const Listener = struct {
    fd: Socket,
    port: u16,
};

fn listenLoopback() ServerError!Listener {
    return switch (builtin.os.tag) {
        .linux => linux.listenLoopback(),
        .macos => darwin.listenLoopback(),
        .windows => windows.listenLoopback(),
        else => error.UnsupportedOperatingSystem,
    };
}

fn acceptSocket(fd: Socket) ServerError!Socket {
    return switch (builtin.os.tag) {
        .linux => linux.acceptSocket(fd),
        .macos => darwin.acceptSocket(fd),
        .windows => windows.acceptSocket(fd),
        else => error.UnsupportedOperatingSystem,
    };
}

fn recvSocket(fd: Socket, buffer: []u8) ServerError!usize {
    return switch (builtin.os.tag) {
        .linux => linux.recvSocket(fd, buffer),
        .macos => darwin.recvSocket(fd, buffer),
        .windows => windows.recvSocket(fd, buffer),
        else => error.UnsupportedOperatingSystem,
    };
}

fn sendAll(fd: Socket, bytes: []const u8) ServerError!void {
    var sent: usize = 0;
    while (sent < bytes.len) {
        const n = switch (builtin.os.tag) {
            .linux => try linux.sendSocket(fd, bytes[sent..]),
            .macos => try darwin.sendSocket(fd, bytes[sent..]),
            .windows => try windows.sendSocket(fd, bytes[sent..]),
            else => return error.UnsupportedOperatingSystem,
        };
        if (n == 0) return error.SendFailed;
        sent += n;
    }
}

fn closeSocket(fd: Socket) void {
    switch (builtin.os.tag) {
        .linux => linux.closeSocket(fd),
        .macos => darwin.closeSocket(fd),
        .windows => windows.closeSocket(fd),
        else => {},
    }
}

fn rawWriteStdout(bytes: []const u8) void {
    rawWrite(stdout_fd, bytes);
}

fn rawWriteStderr(bytes: []const u8) void {
    rawWrite(stderr_fd, bytes);
}

fn rawWrite(fd: c_int, bytes: []const u8) void {
    switch (builtin.os.tag) {
        .linux => linux.rawWrite(fd, bytes),
        .macos => darwin.rawWrite(fd, bytes),
        .windows => windows.rawWrite(fd, bytes),
        else => {},
    }
}

fn abortProcess() noreturn {
    switch (builtin.os.tag) {
        .linux => linux.exitGroup(134),
        .macos => darwin.abort(),
        .windows => windows.exitProcess(134),
        else => @trap(),
    }
}

fn hostToNetwork16(value: u16) u16 {
    return (value << 8) | (value >> 8);
}

fn networkToHost16(value: u16) u16 {
    return hostToNetwork16(value);
}

const loopback_addr_network_order: u32 = 0x0100007f;

const linux = struct {
    const os = std.os.linux;
    const EINTR = @intFromEnum(os.E.INTR);

    fn listenLoopback() ServerError!Listener {
        const socket_result = os.socket(os.AF.INET, os.SOCK.STREAM, 0);
        if (isErr(socket_result)) return error.SocketFailed;
        const fd: i32 = @intCast(socket_result);
        errdefer linux.closeSocket(fd);

        var enabled: c_int = 1;
        if (isErr(os.setsockopt(fd, os.SOL.SOCKET, os.SO.REUSEADDR, @ptrCast(&enabled), @sizeOf(c_int)))) {
            return error.SetSockOptFailed;
        }
        if (isErr(os.setsockopt(fd, os.IPPROTO.TCP, os.TCP.NODELAY, @ptrCast(&enabled), @sizeOf(c_int)))) {
            return error.SetSockOptFailed;
        }

        var addr = os.sockaddr.in{
            .family = os.AF.INET,
            .port = hostToNetwork16(0),
            .addr = loopback_addr_network_order,
            .zero = .{0} ** 8,
        };
        if (isErr(os.bind(fd, @ptrCast(&addr), @sizeOf(os.sockaddr.in)))) return error.BindFailed;
        if (isErr(os.listen(fd, listen_backlog))) return error.ListenFailed;

        var len: os.socklen_t = @sizeOf(os.sockaddr.in);
        if (isErr(os.getsockname(fd, @ptrCast(&addr), &len))) return error.GetSockNameFailed;

        return .{ .fd = fd, .port = networkToHost16(addr.port) };
    }

    fn acceptSocket(fd: i32) ServerError!i32 {
        while (true) {
            const result = os.accept(fd, null, null);
            if (!isErr(result)) return @intCast(result);
            if (errno(result) != EINTR) return error.AcceptFailed;
        }
    }

    fn recvSocket(fd: i32, buffer: []u8) ServerError!usize {
        while (true) {
            const result = os.recvfrom(fd, buffer.ptr, buffer.len, 0, null, null);
            if (!isErr(result)) return result;
            if (errno(result) != EINTR) return error.RecvFailed;
        }
    }

    fn sendSocket(fd: i32, bytes: []const u8) ServerError!usize {
        while (true) {
            const result = os.sendto(fd, bytes.ptr, bytes.len, 0, null, 0);
            if (!isErr(result)) return result;
            if (errno(result) != EINTR) return error.SendFailed;
        }
    }

    fn closeSocket(fd: i32) void {
        _ = os.close(fd);
    }

    fn rawWrite(fd: c_int, bytes: []const u8) void {
        var written: usize = 0;
        while (written < bytes.len) {
            const result = os.write(fd, bytes[written..].ptr, bytes.len - written);
            if (isErr(result)) return;
            if (result == 0) return;
            written += result;
        }
    }

    fn exitGroup(code: c_int) noreturn {
        os.exit_group(code);
    }

    fn isErr(result: usize) bool {
        return os.errno(result) != .SUCCESS;
    }

    fn errno(result: usize) usize {
        return @intFromEnum(os.errno(result));
    }
};

const darwin = struct {
    const AF_INET = 2;
    const SOCK_STREAM = 1;
    const SOL_SOCKET = 0xffff;
    const SO_REUSEADDR = 0x0004;
    const SO_NOSIGPIPE = 0x1022;
    const IPPROTO_TCP = 6;
    const TCP_NODELAY = 0x01;
    const EINTR = 4;

    const socklen_t = u32;

    const SockAddr = extern struct {
        len: u8,
        family: u8,
        data: [14]u8,
    };

    const InAddr = extern struct {
        s_addr: u32,
    };

    const SockAddrIn = extern struct {
        len: u8,
        family: u8,
        port: u16,
        addr: InAddr,
        zero: [8]u8,
    };

    extern "c" fn __error() *c_int;
    extern "c" fn abort() noreturn;
    extern "c" fn socket(domain: c_int, socket_type: c_int, protocol: c_int) c_int;
    extern "c" fn setsockopt(fd: c_int, level: c_int, name: c_int, value: *const anyopaque, value_len: socklen_t) c_int;
    extern "c" fn bind(fd: c_int, addr: *const SockAddr, addr_len: socklen_t) c_int;
    extern "c" fn listen(fd: c_int, backlog: c_int) c_int;
    extern "c" fn accept(fd: c_int, addr: ?*SockAddr, addr_len: ?*socklen_t) c_int;
    extern "c" fn getsockname(fd: c_int, addr: *SockAddr, addr_len: *socklen_t) c_int;
    extern "c" fn recv(fd: c_int, buffer: *anyopaque, len: usize, flags: c_int) isize;
    extern "c" fn send(fd: c_int, buffer: *const anyopaque, len: usize, flags: c_int) isize;
    extern "c" fn close(fd: c_int) c_int;
    extern "c" fn write(fd: c_int, buffer: *const anyopaque, len: usize) isize;

    fn listenLoopback() ServerError!Listener {
        const fd = socket(AF_INET, SOCK_STREAM, 0);
        if (fd == -1) return error.SocketFailed;
        errdefer darwin.closeSocket(fd);

        var enabled: c_int = 1;
        if (setsockopt(fd, SOL_SOCKET, SO_REUSEADDR, &enabled, @sizeOf(c_int)) == -1) return error.SetSockOptFailed;
        if (setsockopt(fd, SOL_SOCKET, SO_NOSIGPIPE, &enabled, @sizeOf(c_int)) == -1) return error.SetSockOptFailed;
        if (setsockopt(fd, IPPROTO_TCP, TCP_NODELAY, &enabled, @sizeOf(c_int)) == -1) return error.SetSockOptFailed;

        var addr = SockAddrIn{
            .len = @sizeOf(SockAddrIn),
            .family = AF_INET,
            .port = hostToNetwork16(0),
            .addr = .{ .s_addr = loopback_addr_network_order },
            .zero = .{0} ** 8,
        };
        if (bind(fd, @ptrCast(&addr), @sizeOf(SockAddrIn)) == -1) return error.BindFailed;
        if (listen(fd, listen_backlog) == -1) return error.ListenFailed;

        var len: socklen_t = @sizeOf(SockAddrIn);
        if (getsockname(fd, @ptrCast(&addr), &len) == -1) return error.GetSockNameFailed;

        return .{ .fd = fd, .port = networkToHost16(addr.port) };
    }

    fn acceptSocket(fd: c_int) ServerError!c_int {
        while (true) {
            const client = accept(fd, null, null);
            if (client != -1) return client;
            if (__error().* != EINTR) return error.AcceptFailed;
        }
    }

    fn recvSocket(fd: c_int, buffer: []u8) ServerError!usize {
        while (true) {
            const result = recv(fd, buffer.ptr, buffer.len, 0);
            if (result >= 0) return @intCast(result);
            if (__error().* != EINTR) return error.RecvFailed;
        }
    }

    fn sendSocket(fd: c_int, bytes: []const u8) ServerError!usize {
        while (true) {
            const result = send(fd, bytes.ptr, bytes.len, 0);
            if (result >= 0) return @intCast(result);
            if (__error().* != EINTR) return error.SendFailed;
        }
    }

    fn closeSocket(fd: c_int) void {
        _ = close(fd);
    }

    fn rawWrite(fd: c_int, bytes: []const u8) void {
        var written: usize = 0;
        while (written < bytes.len) {
            const result = write(fd, bytes[written..].ptr, bytes.len - written);
            if (result <= 0) return;
            written += @intCast(result);
        }
    }
};

const windows = struct {
    const SOCKET = usize;
    const INVALID_SOCKET = ~@as(SOCKET, 0);
    const SOCKET_ERROR = -1;
    const AF_INET = 2;
    const SOCK_STREAM = 1;
    const IPPROTO_TCP = 6;
    const SOL_SOCKET = 0xffff;
    const SO_REUSEADDR = 0x0004;
    const TCP_NODELAY = 0x0001;
    const STD_OUTPUT_HANDLE: u32 = @bitCast(@as(i32, -11));
    const STD_ERROR_HANDLE: u32 = @bitCast(@as(i32, -12));

    const WSAData = extern struct {
        version: u16,
        high_version: u16,
        description: [257]u8,
        system_status: [129]u8,
        max_sockets: u16,
        max_udp_dg: u16,
        vendor_info: ?[*:0]u8,
    };

    const SockAddr = extern struct {
        family: u16,
        data: [14]u8,
    };

    const InAddr = extern union {
        s_addr: u32,
    };

    const SockAddrIn = extern struct {
        family: u16,
        port: u16,
        addr: InAddr,
        zero: [8]u8,
    };

    extern "ws2_32" fn WSAStartup(version: u16, data: *WSAData) callconv(.winapi) c_int;
    extern "ws2_32" fn socket(af: c_int, socket_type: c_int, protocol: c_int) callconv(.winapi) SOCKET;
    extern "ws2_32" fn setsockopt(s: SOCKET, level: c_int, optname: c_int, optval: [*]const u8, optlen: c_int) callconv(.winapi) c_int;
    extern "ws2_32" fn bind(s: SOCKET, name: *const SockAddr, namelen: c_int) callconv(.winapi) c_int;
    extern "ws2_32" fn listen(s: SOCKET, backlog: c_int) callconv(.winapi) c_int;
    extern "ws2_32" fn accept(s: SOCKET, addr: ?*SockAddr, addrlen: ?*c_int) callconv(.winapi) SOCKET;
    extern "ws2_32" fn getsockname(s: SOCKET, name: *SockAddr, namelen: *c_int) callconv(.winapi) c_int;
    extern "ws2_32" fn recv(s: SOCKET, buf: [*]u8, len: c_int, flags: c_int) callconv(.winapi) c_int;
    extern "ws2_32" fn send(s: SOCKET, buf: [*]const u8, len: c_int, flags: c_int) callconv(.winapi) c_int;
    extern "ws2_32" fn closesocket(s: SOCKET) callconv(.winapi) c_int;

    extern "kernel32" fn GetStdHandle(nStdHandle: u32) callconv(.winapi) ?*anyopaque;
    extern "kernel32" fn WriteFile(hFile: ?*anyopaque, lpBuffer: [*]const u8, nNumberOfBytesToWrite: u32, lpNumberOfBytesWritten: ?*u32, lpOverlapped: ?*anyopaque) callconv(.winapi) c_int;
    extern "kernel32" fn ExitProcess(exit_code: u32) callconv(.winapi) noreturn;

    fn startup() ServerError!void {
        var data: WSAData = undefined;
        if (WSAStartup(0x0202, &data) != 0) return error.SocketFailed;
    }

    fn listenLoopback() ServerError!Listener {
        const fd = socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
        if (fd == INVALID_SOCKET) return error.SocketFailed;
        errdefer windows.closeSocket(fd);

        var enabled: c_int = 1;
        if (setsockopt(fd, SOL_SOCKET, SO_REUSEADDR, @ptrCast(&enabled), @sizeOf(c_int)) == SOCKET_ERROR) return error.SetSockOptFailed;
        if (setsockopt(fd, IPPROTO_TCP, TCP_NODELAY, @ptrCast(&enabled), @sizeOf(c_int)) == SOCKET_ERROR) return error.SetSockOptFailed;

        var addr = SockAddrIn{
            .family = AF_INET,
            .port = hostToNetwork16(0),
            .addr = .{ .s_addr = loopback_addr_network_order },
            .zero = .{0} ** 8,
        };
        if (bind(fd, @ptrCast(&addr), @sizeOf(SockAddrIn)) == SOCKET_ERROR) return error.BindFailed;
        if (listen(fd, listen_backlog) == SOCKET_ERROR) return error.ListenFailed;

        var len: c_int = @sizeOf(SockAddrIn);
        if (getsockname(fd, @ptrCast(&addr), &len) == SOCKET_ERROR) return error.GetSockNameFailed;

        return .{ .fd = fd, .port = networkToHost16(addr.port) };
    }

    fn acceptSocket(fd: SOCKET) ServerError!SOCKET {
        const client = accept(fd, null, null);
        if (client == INVALID_SOCKET) return error.AcceptFailed;
        return client;
    }

    fn recvSocket(fd: SOCKET, buffer: []u8) ServerError!usize {
        const max_len = @min(buffer.len, @as(usize, @intCast(std.math.maxInt(c_int))));
        const result = recv(fd, buffer.ptr, @intCast(max_len), 0);
        if (result == SOCKET_ERROR) return error.RecvFailed;
        return @intCast(result);
    }

    fn sendSocket(fd: SOCKET, bytes: []const u8) ServerError!usize {
        const max_len = @min(bytes.len, @as(usize, @intCast(std.math.maxInt(c_int))));
        const result = send(fd, bytes.ptr, @intCast(max_len), 0);
        if (result == SOCKET_ERROR) return error.SendFailed;
        return @intCast(result);
    }

    fn closeSocket(fd: SOCKET) void {
        _ = closesocket(fd);
    }

    fn rawWrite(fd: c_int, bytes: []const u8) void {
        const handle = GetStdHandle(if (fd == stderr_fd) STD_ERROR_HANDLE else STD_OUTPUT_HANDLE);
        var written_total: usize = 0;
        while (written_total < bytes.len) {
            const chunk_len = @min(bytes.len - written_total, @as(usize, std.math.maxInt(u32)));
            var written: u32 = 0;
            if (WriteFile(handle, bytes[written_total..].ptr, @intCast(chunk_len), &written, null) == 0) return;
            if (written == 0) return;
            written_total += written;
        }
    }

    fn exitProcess(code: u32) noreturn {
        ExitProcess(code);
    }
};
