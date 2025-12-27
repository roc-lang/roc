const std = @import("std");
const RocList = @import("list.zig").RocList;
const RocStr = @import("str.zig").RocStr;
const utils = @import("utils.zig");
const testing = std.testing;
const expect = testing.expect;
const expectEqual = testing.expectEqual;

// DecodeResult represents the result of a decode operation
// This matches the Roc type: { result : Result val DecodeError, rest : List U8 }
pub const DecodeResult = extern struct {
    result: DecodeResultValue,
    rest: RocList,
};

// DecodeResultValue represents either a success (tag=0) or error (tag=1)
// For simplicity, we use a tagged union
pub const DecodeResultValue = extern struct {
    tag: u8, // 0 = Ok, 1 = Err
    value: DecodeValue,
};

pub const DecodeValue = extern union {
    ok: DecodeOkValue,
    err: DecodeError,
};

pub const DecodeOkValue = extern union {
    u8_val: u8,
    u16_val: u16,
    u32_val: u32,
    u64_val: u64,
    u128_val: u128,
    i8_val: i8,
    i16_val: i16,
    i32_val: i32,
    i64_val: i64,
    i128_val: i128,
    f32_val: f32,
    f64_val: f64,
    bool_val: bool,
    str_val: RocStr,
};

pub const DecodeError = extern struct {
    tag: u8, // 0 = TooShort
};

// Basic decode functions for primitive types
// These are low-level utilities that can be used by format-specific decoders

// Decode a u8 from a byte list
// Returns the decoded value and the remaining bytes
pub fn decodeU8(bytes: RocList) callconv(.C) DecodeResult {
    if (bytes.len() < 1) {
        return DecodeResult{
            .result = DecodeResultValue{
                .tag = 1, // Err
                .value = DecodeValue{ .err = DecodeError{ .tag = 0 } }, // TooShort
            },
            .rest = bytes,
        };
    }

    const byte_ptr = bytes.bytes orelse {
        return DecodeResult{
            .result = DecodeResultValue{
                .tag = 1,
                .value = DecodeValue{ .err = DecodeError{ .tag = 0 } },
            },
            .rest = bytes,
        };
    };

    const value = byte_ptr[0];
    const rest = RocList{
        .bytes = if (bytes.len() > 1) byte_ptr + 1 else null,
        .length = bytes.len() - 1,
        .capacity_or_alloc_ptr = bytes.capacity_or_alloc_ptr,
    };

    return DecodeResult{
        .result = DecodeResultValue{
            .tag = 0, // Ok
            .value = DecodeValue{ .ok = DecodeOkValue{ .u8_val = value } },
        },
        .rest = rest,
    };
}

// Decode a u16 from a byte list (little-endian)
pub fn decodeU16(bytes: RocList) callconv(.C) DecodeResult {
    if (bytes.len() < 2) {
        return DecodeResult{
            .result = DecodeResultValue{
                .tag = 1,
                .value = DecodeValue{ .err = DecodeError{ .tag = 0 } },
            },
            .rest = bytes,
        };
    }

    const byte_ptr = bytes.bytes orelse {
        return DecodeResult{
            .result = DecodeResultValue{
                .tag = 1,
                .value = DecodeValue{ .err = DecodeError{ .tag = 0 } },
            },
            .rest = bytes,
        };
    };

    const value = @as(u16, byte_ptr[0]) | (@as(u16, byte_ptr[1]) << 8);
    const rest = RocList{
        .bytes = if (bytes.len() > 2) byte_ptr + 2 else null,
        .length = bytes.len() - 2,
        .capacity_or_alloc_ptr = bytes.capacity_or_alloc_ptr,
    };

    return DecodeResult{
        .result = DecodeResultValue{
            .tag = 0,
            .value = DecodeValue{ .ok = DecodeOkValue{ .u16_val = value } },
        },
        .rest = rest,
    };
}

// Decode a u32 from a byte list (little-endian)
pub fn decodeU32(bytes: RocList) callconv(.C) DecodeResult {
    if (bytes.len() < 4) {
        return DecodeResult{
            .result = DecodeResultValue{
                .tag = 1,
                .value = DecodeValue{ .err = DecodeError{ .tag = 0 } },
            },
            .rest = bytes,
        };
    }

    const byte_ptr = bytes.bytes orelse {
        return DecodeResult{
            .result = DecodeResultValue{
                .tag = 1,
                .value = DecodeValue{ .err = DecodeError{ .tag = 0 } },
            },
            .rest = bytes,
        };
    };

    const value = @as(u32, byte_ptr[0]) |
        (@as(u32, byte_ptr[1]) << 8) |
        (@as(u32, byte_ptr[2]) << 16) |
        (@as(u32, byte_ptr[3]) << 24);
    const rest = RocList{
        .bytes = if (bytes.len() > 4) byte_ptr + 4 else null,
        .length = bytes.len() - 4,
        .capacity_or_alloc_ptr = bytes.capacity_or_alloc_ptr,
    };

    return DecodeResult{
        .result = DecodeResultValue{
            .tag = 0,
            .value = DecodeValue{ .ok = DecodeOkValue{ .u32_val = value } },
        },
        .rest = rest,
    };
}

// Decode a u64 from a byte list (little-endian)
pub fn decodeU64(bytes: RocList) callconv(.C) DecodeResult {
    if (bytes.len() < 8) {
        return DecodeResult{
            .result = DecodeResultValue{
                .tag = 1,
                .value = DecodeValue{ .err = DecodeError{ .tag = 0 } },
            },
            .rest = bytes,
        };
    }

    const byte_ptr = bytes.bytes orelse {
        return DecodeResult{
            .result = DecodeResultValue{
                .tag = 1,
                .value = DecodeValue{ .err = DecodeError{ .tag = 0 } },
            },
            .rest = bytes,
        };
    };

    const value = @as(u64, byte_ptr[0]) |
        (@as(u64, byte_ptr[1]) << 8) |
        (@as(u64, byte_ptr[2]) << 16) |
        (@as(u64, byte_ptr[3]) << 24) |
        (@as(u64, byte_ptr[4]) << 32) |
        (@as(u64, byte_ptr[5]) << 40) |
        (@as(u64, byte_ptr[6]) << 48) |
        (@as(u64, byte_ptr[7]) << 56);
    const rest = RocList{
        .bytes = if (bytes.len() > 8) byte_ptr + 8 else null,
        .length = bytes.len() - 8,
        .capacity_or_alloc_ptr = bytes.capacity_or_alloc_ptr,
    };

    return DecodeResult{
        .result = DecodeResultValue{
            .tag = 0,
            .value = DecodeValue{ .ok = DecodeOkValue{ .u64_val = value } },
        },
        .rest = rest,
    };
}

// Decode a bool from a byte list (0 = false, non-zero = true)
pub fn decodeBool(bytes: RocList) callconv(.C) DecodeResult {
    const u8_result = decodeU8(bytes);
    if (u8_result.result.tag == 1) {
        return u8_result;
    }

    const value = u8_result.result.value.ok.u8_val != 0;

    return DecodeResult{
        .result = DecodeResultValue{
            .tag = 0,
            .value = DecodeValue{ .ok = DecodeOkValue{ .bool_val = value } },
        },
        .rest = u8_result.rest,
    };
}

// Export helpers for comptime code generation
pub fn exportDecodeU8(comptime name: []const u8) void {
    const f = struct {
        fn func(bytes: RocList) callconv(.C) DecodeResult {
            return decodeU8(bytes);
        }
    }.func;
    @export(f, .{ .name = name, .linkage = .strong });
}

pub fn exportDecodeU16(comptime name: []const u8) void {
    const f = struct {
        fn func(bytes: RocList) callconv(.C) DecodeResult {
            return decodeU16(bytes);
        }
    }.func;
    @export(f, .{ .name = name, .linkage = .strong });
}

pub fn exportDecodeU32(comptime name: []const u8) void {
    const f = struct {
        fn func(bytes: RocList) callconv(.C) DecodeResult {
            return decodeU32(bytes);
        }
    }.func;
    @export(f, .{ .name = name, .linkage = .strong });
}

pub fn exportDecodeU64(comptime name: []const u8) void {
    const f = struct {
        fn func(bytes: RocList) callconv(.C) DecodeResult {
            return decodeU64(bytes);
        }
    }.func;
    @export(f, .{ .name = name, .linkage = .strong });
}

pub fn exportDecodeBool(comptime name: []const u8) void {
    const f = struct {
        fn func(bytes: RocList) callconv(.C) DecodeResult {
            return decodeBool(bytes);
        }
    }.func;
    @export(f, .{ .name = name, .linkage = .strong });
}

// Tests
test "decode u8" {
    var bytes_array = [_]u8{ 42, 100, 200 };
    const bytes = RocList.fromSlice(u8, &bytes_array, false);
    const result = decodeU8(bytes);

    try expect(result.result.tag == 0); // Ok
    try expect(result.result.value.ok.u8_val == 42);
    try expect(result.rest.len() == 2);
}

test "decode u8 too short" {
    const bytes = RocList.empty();
    const result = decodeU8(bytes);

    try expect(result.result.tag == 1); // Err
    try expect(result.result.value.err.tag == 0); // TooShort
}

test "decode u16" {
    var bytes_array = [_]u8{ 0x34, 0x12, 0x78, 0x56 };
    const bytes = RocList.fromSlice(u8, &bytes_array, false);
    const result = decodeU16(bytes);

    try expect(result.result.tag == 0); // Ok
    try expect(result.result.value.ok.u16_val == 0x1234);
    try expect(result.rest.len() == 2);
}

test "decode u32" {
    var bytes_array = [_]u8{ 0x78, 0x56, 0x34, 0x12, 0x99 };
    const bytes = RocList.fromSlice(u8, &bytes_array, false);
    const result = decodeU32(bytes);

    try expect(result.result.tag == 0); // Ok
    try expect(result.result.value.ok.u32_val == 0x12345678);
    try expect(result.rest.len() == 1);
}

test "decode bool" {
    var bytes_array = [_]u8{ 1, 0, 42 };
    const bytes = RocList.fromSlice(u8, &bytes_array, false);
    const result = decodeBool(bytes);

    try expect(result.result.tag == 0); // Ok
    try expect(result.result.value.ok.bool_val == true);
    try expect(result.rest.len() == 2);
}
