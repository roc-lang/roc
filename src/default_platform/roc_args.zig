//! Native process-argument conversion for the compiler-owned default platform.
//!
//! This module owns the host side of the `List(Str)` entrypoint ABI. It is
//! shared by direct-run hosts for UTF-8 sanitization and by standalone runtime
//! objects for allocation and construction of the owned Roc value.

const std = @import("std");

const abi = @import("roc_str_view");
const RocList = abi.RocList;
const RocStr = abi.RocStr;

/// C-compatible allocator used by the standalone default-platform runtimes.
pub const AllocFn = *const fn (usize, usize) callconv(.c) ?*anyopaque;

const replacement = "\xef\xbf\xbd";
const small_str_flag: u8 = 0b1000_0000;
const capacity_shift = 1;

/// Replace every invalid UTF-8 byte with U+FFFD. Valid input is returned
/// unchanged; invalid input is returned in an allocation owned by `allocator`.
pub fn sanitizeUtf8(input: []const u8, allocator: std.mem.Allocator) std.mem.Allocator.Error![]const u8 {
    if (std.unicode.utf8ValidateSlice(input)) return input;

    const output_len = sanitizedUtf8Length(input) orelse return error.OutOfMemory;
    const output = try allocator.alloc(u8, output_len);
    writeSanitizedUtf8(input, output);
    return output;
}

/// Build an owned Roc argument list from a POSIX `argc`/`argv` pair, excluding
/// the executable path. Invalid UTF-8 bytes are replaced with U+FFFD.
pub fn fromPosixArgv(argc: usize, argv: [*][*:0]u8, alloc: AllocFn) ?RocList {
    if (argc <= 1) return RocList.empty();

    const arg_count = argc - 1;
    const list = allocateList(arg_count, alloc) orelse return null;
    const elements: [*]RocStr = @ptrCast(@alignCast(list.bytes.?));

    for (0..arg_count) |index| {
        elements[index] = fromUtf8(sentinelSlice(u8, argv[index + 1]), alloc) orelse return null;
    }

    return list;
}

/// Build an owned Roc argument list from Windows UTF-16 argv, excluding the
/// executable path. Unpaired surrogates are replaced with U+FFFD.
pub fn fromWindowsArgv(argc: usize, argv: [*][*:0]u16, alloc: AllocFn) ?RocList {
    if (argc <= 1) return RocList.empty();

    const arg_count = argc - 1;
    const list = allocateList(arg_count, alloc) orelse return null;
    const elements: [*]RocStr = @ptrCast(@alignCast(list.bytes.?));

    for (0..arg_count) |index| {
        elements[index] = fromUtf16(sentinelSlice(u16, argv[index + 1]), alloc) orelse return null;
    }

    return list;
}

fn sentinelSlice(comptime T: type, pointer: [*:0]T) []const T {
    // Volatile reads keep freestanding ReleaseFast builds from replacing this
    // startup-only scan with a libc strlen/wcslen call.
    const volatile_pointer: [*]volatile T = pointer;
    var length: usize = 0;
    while (volatile_pointer[length] != 0) : (length += 1) {}
    return pointer[0..length];
}

fn allocateList(length: usize, alloc: AllocFn) ?RocList {
    if (length == 0) return RocList.empty();
    if (length > std.math.maxInt(usize) >> capacity_shift) return null;
    if (length > std.math.maxInt(usize) / @sizeOf(RocStr)) return null;

    const header_size = 2 * @sizeOf(usize);
    const data_size = length * @sizeOf(RocStr);
    if (data_size > std.math.maxInt(usize) - header_size) return null;

    const allocation: [*]u8 = @ptrCast(alloc(header_size + data_size, @alignOf(RocStr)) orelse return null);
    const data = allocation + header_size;
    const header: [*]usize = @ptrCast(@alignCast(data));
    (header - 2)[0] = length;
    (header - 1)[0] = 1;

    return .{
        .bytes = data,
        .length = length,
        .capacity_or_alloc_ptr = length << capacity_shift,
    };
}

fn fromUtf8(input: []const u8, alloc: AllocFn) ?RocStr {
    const output_len = sanitizedUtf8Length(input) orelse return null;
    var result = allocateStr(output_len, alloc) orelse return null;
    writeSanitizedUtf8(input, strBytes(&result, output_len));
    return result;
}

fn sanitizedUtf8Length(input: []const u8) ?usize {
    var output_len: usize = 0;
    var input_index: usize = 0;
    while (input_index < input.len) {
        const sequence_len = validSequenceLength(input[input_index..]);
        const written_len: usize = if (sequence_len == 0) replacement.len else sequence_len;
        if (output_len > std.math.maxInt(usize) - written_len) return null;
        output_len += written_len;
        input_index += if (sequence_len == 0) 1 else sequence_len;
    }
    return output_len;
}

fn writeSanitizedUtf8(input: []const u8, output: []u8) void {
    var input_index: usize = 0;
    var output_index: usize = 0;
    while (input_index < input.len) {
        const sequence_len = validSequenceLength(input[input_index..]);
        if (sequence_len == 0) {
            @memcpy(output[output_index..][0..replacement.len], replacement);
            output_index += replacement.len;
            input_index += 1;
        } else {
            @memcpy(output[output_index..][0..sequence_len], input[input_index..][0..sequence_len]);
            output_index += sequence_len;
            input_index += sequence_len;
        }
    }
    std.debug.assert(output_index == output.len);
}

fn validSequenceLength(input: []const u8) usize {
    const sequence_len = std.unicode.utf8ByteSequenceLength(input[0]) catch return 0;
    if (sequence_len > input.len) return 0;
    _ = std.unicode.utf8Decode(input[0..sequence_len]) catch return 0;
    return sequence_len;
}

fn fromUtf16(input: []const u16, alloc: AllocFn) ?RocStr {
    const output_len = utf16Utf8Length(input) orelse return null;
    var result = allocateStr(output_len, alloc) orelse return null;
    writeUtf16AsUtf8(input, strBytes(&result, output_len));
    return result;
}

fn utf16Utf8Length(input: []const u16) ?usize {
    var output_len: usize = 0;
    var input_index: usize = 0;
    while (input_index < input.len) {
        const decoded = decodeUtf16(input, input_index);
        const encoded_len = utf8CodepointLength(decoded.codepoint);
        if (output_len > std.math.maxInt(usize) - encoded_len) return null;
        output_len += encoded_len;
        input_index += decoded.code_units;
    }
    return output_len;
}

fn writeUtf16AsUtf8(input: []const u16, output: []u8) void {
    var input_index: usize = 0;
    var output_index: usize = 0;
    while (input_index < input.len) {
        const decoded = decodeUtf16(input, input_index);
        output_index += encodeUtf8(decoded.codepoint, output[output_index..]);
        input_index += decoded.code_units;
    }
    std.debug.assert(output_index == output.len);
}

const DecodedUtf16 = struct {
    codepoint: u21,
    code_units: usize,
};

fn decodeUtf16(input: []const u16, index: usize) DecodedUtf16 {
    const first = input[index];
    if (first >= 0xd800 and first <= 0xdbff and index + 1 < input.len) {
        const second = input[index + 1];
        if (second >= 0xdc00 and second <= 0xdfff) {
            const high: u21 = first - 0xd800;
            const low: u21 = second - 0xdc00;
            return .{ .codepoint = 0x10000 + (high << 10) + low, .code_units = 2 };
        }
    }
    if (first >= 0xd800 and first <= 0xdfff) {
        return .{ .codepoint = 0xfffd, .code_units = 1 };
    }
    return .{ .codepoint = @intCast(first), .code_units = 1 };
}

fn utf8CodepointLength(codepoint: u21) usize {
    if (codepoint <= 0x7f) return 1;
    if (codepoint <= 0x7ff) return 2;
    if (codepoint <= 0xffff) return 3;
    return 4;
}

fn encodeUtf8(codepoint: u21, output: []u8) usize {
    if (codepoint <= 0x7f) {
        output[0] = @intCast(codepoint);
        return 1;
    }
    if (codepoint <= 0x7ff) {
        output[0] = 0xc0 | @as(u8, @intCast(codepoint >> 6));
        output[1] = 0x80 | @as(u8, @intCast(codepoint & 0x3f));
        return 2;
    }
    if (codepoint <= 0xffff) {
        output[0] = 0xe0 | @as(u8, @intCast(codepoint >> 12));
        output[1] = 0x80 | @as(u8, @intCast((codepoint >> 6) & 0x3f));
        output[2] = 0x80 | @as(u8, @intCast(codepoint & 0x3f));
        return 3;
    }
    output[0] = 0xf0 | @as(u8, @intCast(codepoint >> 18));
    output[1] = 0x80 | @as(u8, @intCast((codepoint >> 12) & 0x3f));
    output[2] = 0x80 | @as(u8, @intCast((codepoint >> 6) & 0x3f));
    output[3] = 0x80 | @as(u8, @intCast(codepoint & 0x3f));
    return 4;
}

fn allocateStr(length: usize, alloc: AllocFn) ?RocStr {
    if (length < @sizeOf(RocStr)) {
        var result: RocStr = .{ .bytes = null, .capacity_or_alloc_ptr = 0, .length = 0 };
        const bytes: *[@sizeOf(RocStr)]u8 = @ptrCast(&result);
        @memset(bytes, 0);
        bytes[@sizeOf(RocStr) - 1] = small_str_flag | @as(u8, @intCast(length));
        return result;
    }

    if (length > std.math.maxInt(usize) >> capacity_shift) return null;
    if (length > std.math.maxInt(usize) - @sizeOf(usize)) return null;
    const allocation: [*]u8 = @ptrCast(alloc(@sizeOf(usize) + length, @alignOf(usize)) orelse return null);
    const data = allocation + @sizeOf(usize);
    const refcount: *usize = @ptrCast(@alignCast(data - @sizeOf(usize)));
    refcount.* = 1;
    return .{
        .bytes = data,
        .capacity_or_alloc_ptr = length << capacity_shift,
        .length = length,
    };
}

fn strBytes(str: *RocStr, length: usize) []u8 {
    const pointer: [*]u8 = if (length < @sizeOf(RocStr)) @ptrCast(str) else str.bytes.?;
    return pointer[0..length];
}

const testing = std.testing;
const test_allocator = std.testing.allocator;

var test_allocation_bytes: [4096]u8 align(16) = undefined;
var test_allocation_offset: usize = 0;

fn testAlloc(length: usize, alignment: usize) callconv(.c) ?*anyopaque {
    const start = std.mem.alignForward(usize, test_allocation_offset, alignment);
    if (start > test_allocation_bytes.len or length > test_allocation_bytes.len - start) return null;
    test_allocation_offset = start + length;
    return @ptrCast(&test_allocation_bytes[start]);
}

test "sanitizeUtf8: valid ASCII passes through unchanged" {
    const input = "hello world";
    const result = try sanitizeUtf8(input, test_allocator);
    try testing.expectEqualStrings(input, result);
    try testing.expectEqual(input.ptr, result.ptr);
}

test "sanitizeUtf8: valid multibyte UTF-8 passes through unchanged" {
    const input = "na\xc3\xafve \xe2\x9c\x93";
    const result = try sanitizeUtf8(input, test_allocator);
    try testing.expectEqualStrings(input, result);
    try testing.expectEqual(input.ptr, result.ptr);
}

test "sanitizeUtf8: invalid bytes become replacement characters" {
    const result = try sanitizeUtf8("a\xff\xfeb", test_allocator);
    defer test_allocator.free(result);
    try testing.expectEqualStrings("a\xef\xbf\xbd\xef\xbf\xbdb", result);
}

test "sanitizeUtf8: truncated sequences replace each invalid byte" {
    const result = try sanitizeUtf8("\xe2\x9c", test_allocator);
    defer test_allocator.free(result);
    try testing.expectEqualStrings("\xef\xbf\xbd\xef\xbf\xbd", result);
}

test "UTF-16 conversion handles surrogate pairs and unpaired surrogates" {
    const input = [_]u16{ 'a', 0xd83d, 0xdc96, 0xd800, 'b' };
    const output_len = utf16Utf8Length(&input).?;
    var output: [9]u8 = undefined;
    try testing.expectEqual(output.len, output_len);
    writeUtf16AsUtf8(&input, &output);
    try testing.expectEqualStrings("a\xf0\x9f\x92\x96\xef\xbf\xbdb", &output);
}

test "POSIX argv becomes an owned Roc argument list" {
    test_allocation_offset = 0;

    var executable = "app".*;
    var short_arg = "short".*;
    var long_arg = "this argument uses the heap representation".*;
    var invalid_arg = [_:0]u8{ 'a', 0xff, 'b' };
    var argv = [_][*:0]u8{ &executable, &short_arg, &long_arg, &invalid_arg };

    const list = fromPosixArgv(argv.len, &argv, &testAlloc).?;
    try testing.expectEqual(@as(usize, 3), list.length);
    try testing.expectEqual(list.length << capacity_shift, list.capacity_or_alloc_ptr);

    const elements: [*]RocStr = @ptrCast(@alignCast(list.bytes.?));
    try testing.expectEqualStrings("short", elements[0].asSlice());
    try testing.expectEqualStrings("this argument uses the heap representation", elements[1].asSlice());
    try testing.expectEqualStrings("a\xef\xbf\xbdb", elements[2].asSlice());

    const list_header: [*]usize = @ptrCast(@alignCast(list.bytes.?));
    try testing.expectEqual(list.length, (list_header - 2)[0]);
    try testing.expectEqual(@as(usize, 1), (list_header - 1)[0]);

    const long_arg_refcount: *const usize = @ptrCast(@alignCast(elements[1].bytes.? - @sizeOf(usize)));
    try testing.expectEqual(@as(usize, 1), long_arg_refcount.*);
}
