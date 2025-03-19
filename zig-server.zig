const std = @import("std");

// Browsers only accept about 100 headers, so this should be way more than enough.
threadlocal var response_iovecs = [_]std.os.iovec_const{undefined} ** 256;

comptime {
    const protocol = "HTTP/1.1 "; // the beginning of each HTTP 1.1 response

    response_iovecs[0] = .{ .iov_base = protocol.ptr, .iov_len = protocol.len };
}

const Header = struct {
    key: HeaderKey,
    value: []u8,
};

const HeaderType = enum {
    common,
    uncommon,
};

const HeaderKey = union(HeaderType) {
    common: CommonHeader,
    uncommon: []u8,
};

// https://www.rfc-editor.org/rfc/rfc7541.html#appendix-A
const CommonHeader = enum(u8) {
    // TODO add the others
    content_encoding = 26,
    // TODO add the others
    location = 46,
    // TODO add the others
};

const StaticTable = [_][]u8{
    "content-type",
    "content-length",
    "content-encoding",
    "location",
};

const header_kv_sep = ": "; // Separator between HTTP header keys and values
const header_kv_sep_iov = .{ .iov_base = header_kv_sep.ptr, .iov_len = header_kv_sep.len };
const content_length = "\r\nContent-Length: ";
const content_length_iov = .{ .iov_base = content_length.ptr, .iov_len = content_length.len };

// HTTP headers use \r\n for newlines - see https://www.rfc-editor.org/rfc/rfc2616#section-2.2
const blank_line = "\r\n\r\n"; // There's a blank line before the body
const blank_line_iov = .{ .iov_base = blank_line.ptr, .iov_len = blank_line.len };
const newline_iov = .{ .iov_base = blank_line.ptr, .iov_len = 2 }; // Reuse the blank line string

const Response = struct {
    status: u16, // e.g. 200 for OK or 404 for Not Found
    headers: []Header, // key-value pairs like "Content-Type" "text/html"
    body: []u8, // e.g. "<!doctype html><html></html>"

    fn write(resp: *Response, fd: i32) !usize {
        // TODO use WSASend on Windows instead of writev
        // (and WSABUF instead of iovec - same struct, but with field order flipped and u32 len instead of usize)
        // https://learn.microsoft.com/en-us/windows/win32/api/winsock2/nf-winsock2-wsasend?redirectedfrom=MSDN
        return try std.os.writev(fd, response_iovecs.ptr, try populate_response_iovecs(resp));
    }

    // Populate the response_iovecs threadlocal and return the number of iovecs in it.
    inline fn populate_response_iovecs(resp: *Response) !usize {
        // Use writev to write the entire response to the given file descriptor in 1 syscall.
        // To do this, we first have to prepare iovecs which provide pointers to everything.
        // writev will concatenate them together in kernel space before writing to the fd.
        const elems_per_header: usize = 4; // (key, separator, value, newline)
        const iovecs_len =
            // +2 at the start because we hardcode the iovecs to start with "HTTP/1.1 ",
            // and then always add the status code (e.g. "200 OK") after that.
            // +4 at the end for the Content-Length header (3 iovecs) and body (1 iovec).
            2 + (resp.headers.items.len * elems_per_header) + 4;

        // We have a hardcoded maximum number of iovecs we support.
        if (iovecs_len > response_iovecs.len) {
            return error.TooManyHeaders;
        }

        // response_iovecs always begins with "HTTP/1.1 " - so just add the status string
        // (e.g. "200 OK") right after it.
        const status_string = statusToString(resp.status);
        response_iovecs[1] = .{ .iov_base = status_string.ptr, .iov_len = status_string.len };

        var iovec_index: usize = 2; // Start adding headers after the 2 status iovecs

        // Add the headers, each with a leading newline since these go right after the status line.
        for (resp.headers.items) |header| {
            // Branchlessly convert the header key to a pointer and length for iovecs.
            const common_offset = @as(usize, @intFromEnum(header.key.common) - 26);
            const common_ptr = &StaticTable[common_offset].ptr;
            const common_len = &StaticTable[common_offset].len;
            const uncommon_ptr = &header.key.uncommon.ptr;
            const uncommon_len = &header.key.uncommon.len;
            const is_common = header.key == .common;
            const key_ptr = if (is_common) common_ptr else uncommon_ptr;
            const key_len = if (is_common) common_len else uncommon_len;

            // Populate the relevant iovecs
            response_iovecs[iovec_index] = newline_iov;
            response_iovecs[iovec_index + 1] = .{ .iov_base = *key_ptr, .iov_len = *key_len };
            response_iovecs[iovec_index + 2] = header_kv_sep_iov;
            response_iovecs[iovec_index + 3] = .{ .iov_base = header.value.ptr, .iov_len = header.value.len };

            // If these are different, then we did our math wrong at the start of the function!
            std.debug.assert(elems_per_header == 4);

            iovec_index += elems_per_header;
        }

        // The last header is always Content-Length, which holds the length of the body.
        // A 64-bit unsigned integer (u64) can have at most 20 digits in decimal form.
        var body_len_str: [20]u8 = undefined;
        const body_len = resp.body.len;

        // TODO there's presumably a simpler way to populate that buffer.
        std.fmt.bufPrint(
            &body_len_str,
            "{body_len}",
            .{body_len},
        ) catch unreachable;

        response_iovecs[iovec_index] = content_length_iov; // "Content-Length: "
        response_iovecs[iovec_index + 1] = .{ .iov_base = body_len_str.ptr, .iov_len = body_len_str.len };

        // Finally, add the body itself (with a blank line in front of it)
        response_iovecs[iovec_index + 2] = blank_line_iov;
        response_iovecs[iovec_index + 3] = .{ .iov_base = resp.body.ptr, .iov_len = resp.body.len };

        // If these are different, then we did our math wrong at the start of the function!
        std.debug.assert(iovec_index + 4 == iovecs_len);

        return iovecs_len;
    }
};

fn responseToString(comptime num_iovecs: usize) ![]u8 {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    // Calculate the total length of all iovecs
    var total_len: usize = 0;
    for (response_iovecs[0..num_iovecs]) |iovec| {
        total_len += iovec.iov_len;
    }

    // Allocate a buffer and copy all the iovec data into it
    var buf = try allocator.alloc(u8, total_len);
    var offset: usize = 0;
    for (response_iovecs[0..num_iovecs]) |iovec| {
        const base = @as([*]const u8, @ptrCast(iovec.iov_base));
        @memcpy(buf[offset..][0..iovec.iov_len], base[0..iovec.iov_len]);
        offset += iovec.iov_len;
    }

    return try std.testing.allocator.dupe(u8, buf);
}

test "populate_response_iovecs simple response" {
    const allocator = std.testing.allocator;

    var headers = std.ArrayList(Header).init(allocator);
    defer headers.deinit();

    try headers.append(.{
        .key = .{ .common = .content_encoding },
        .value = "gzip",
    });

    var response = Response{
        .status = 200,
        .headers = headers.items,
        .body = "Hello, World!",
    };

    const num_iovecs = try response.populate_response_iovecs();
    const response_str = try responseToString(num_iovecs);
    defer allocator.free(response_str);

    const expected = "HTTP/1.1 200 OK\r\ncontent-encoding: gzip\r\nContent-Length: 13\r\n\r\nHello, World!";
    try std.testing.expectEqualStrings(expected, response_str);
}

test "populate_response_iovecs with uncommon header" {
    const allocator = std.testing.allocator;

    var headers = std.ArrayList(Header).init(allocator);
    defer headers.deinit();

    try headers.append(.{
        .key = .{ .uncommon = "X-Custom-Header" },
        .value = "custom-value",
    });

    try headers.append(.{
        .key = .{ .common = .location },
        .value = "/redirected",
    });

    var response = Response{
        .status = 301,
        .headers = headers.items,
        .body = "",
    };

    const num_iovecs = try response.populate_response_iovecs();
    const response_str = try responseToString(num_iovecs);
    defer allocator.free(response_str);

    const expected = "HTTP/1.1 301 Moved Permanently\r\nX-Custom-Header: custom-value\r\nlocation: /redirected\r\nContent-Length: 0\r\n\r\n";
    try std.testing.expectEqualStrings(expected, response_str);
}

test "populate_response_iovecs with error response" {
    const allocator = std.testing.allocator;

    var headers = std.ArrayList(Header).init(allocator);
    defer headers.deinit();

    try headers.append(.{
        .key = .{ .uncommon = "Content-Type" },
        .value = "text/plain",
    });

    var response = Response{
        .status = 404,
        .headers = headers.items,
        .body = "Not Found",
    };

    const num_iovecs = try response.populate_response_iovecs();
    const response_str = try responseToString(num_iovecs);
    defer allocator.free(response_str);

    const expected = "HTTP/1.1 404 Not Found\r\nContent-Type: text/plain\r\nContent-Length: 9\r\n\r\nNot Found";
    try std.testing.expectEqualStrings(expected, response_str);
}

test "populate_response_iovecs too many headers returns error" {
    const allocator = std.testing.allocator;

    var headers = std.ArrayList(Header).init(allocator);
    defer headers.deinit();

    // Add more headers than the response_iovecs array can handle
    // Each header uses 4 iovecs, plus 6 for status and body, so we need (256-6)/4 + 1 headers
    const headers_needed = (response_iovecs.len - 6) / 4 + 1;

    for (0..headers_needed) |i| {
        var key_buf = try std.fmt.allocPrint(allocator, "X-Custom-{d}", .{i});
        defer allocator.free(key_buf);

        var key = try allocator.dupe(u8, key_buf);
        try headers.append(.{
            .key = .{ .uncommon = key },
            .value = "value",
        });
    }

    var response = Response{
        .status = 200,
        .headers = headers.items,
        .body = "body",
    };

    try std.testing.expectError(error.TooManyHeaders, response.populate_response_iovecs());

    // Clean up allocated memory for header keys
    for (headers.items) |header| {
        if (header.key == .uncommon) {
            allocator.free(header.key.uncommon);
        }
    }
}

/// Convert HTTP status code to string representation
/// e.g. 200 -> "200 OK", 404 -> "404 Not Found"
fn statusToString(code: u16) []const u8 {
    // TODO this can become a table lookup, if LLVM doesn't already optimize it to one.
    return switch (code) {
        100 => "100 Continue",
        101 => "101 Switching Protocols",
        102 => "102 Processing",
        103 => "103 Early Hints",

        200 => "200 OK",
        201 => "201 Created",
        202 => "202 Accepted",
        203 => "203 Non-Authoritative Information",
        204 => "204 No Content",
        205 => "205 Reset Content",
        206 => "206 Partial Content",
        207 => "207 Multi-Status",
        208 => "208 Already Reported",
        226 => "226 IM Used",

        300 => "300 Multiple Choices",
        301 => "301 Moved Permanently",
        302 => "302 Found",
        303 => "303 See Other",
        304 => "304 Not Modified",
        305 => "305 Use Proxy",
        307 => "307 Temporary Redirect",
        308 => "308 Permanent Redirect",

        400 => "400 Bad Request",
        401 => "401 Unauthorized",
        402 => "402 Payment Required",
        403 => "403 Forbidden",
        404 => "404 Not Found",
        405 => "405 Method Not Allowed",
        406 => "406 Not Acceptable",
        407 => "407 Proxy Authentication Required",
        408 => "408 Request Timeout",
        409 => "409 Conflict",
        410 => "410 Gone",
        411 => "411 Length Required",
        412 => "412 Precondition Failed",
        413 => "413 Payload Too Large",
        414 => "414 URI Too Long",
        415 => "415 Unsupported Media Type",
        416 => "416 Range Not Satisfiable",
        417 => "417 Expectation Failed",
        418 => "418 I'm a teapot",
        421 => "421 Misdirected Request",
        422 => "422 Unprocessable Entity",
        423 => "423 Locked",
        424 => "424 Failed Dependency",
        425 => "425 Too Early",
        426 => "426 Upgrade Required",
        428 => "428 Precondition Required",
        429 => "429 Too Many Requests",
        431 => "431 Request Header Fields Too Large",
        451 => "451 Unavailable For Legal Reasons",

        500 => "500 Internal Server Error",
        501 => "501 Not Implemented",
        502 => "502 Bad Gateway",
        503 => "503 Service Unavailable",
        504 => "504 Gateway Timeout",
        505 => "505 HTTP Version Not Supported",
        506 => "506 Variant Also Negotiates",
        507 => "507 Insufficient Storage",
        508 => "508 Loop Detected",
        510 => "510 Not Extended",
        511 => "511 Network Authentication Required",

        else => "500 Internal Server Error", // Default to 500 for unknown codes
    };
}
