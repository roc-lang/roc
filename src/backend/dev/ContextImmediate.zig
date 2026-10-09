//! Producer-selected fixed-width context immediates. Cached CTFE code patches
//! these explicit sites, never inferring instructions from compilation-local IDs.

const std = @import("std");

pub const Encoding = enum(u8) { x86_movabs, arm_movwide };

pub const Relocation = struct {
    offset: u32,
    binding: u32,
    encoding: Encoding,
};

pub const Error = error{ InvalidContextRelocation, MissingContextBinding };

pub fn width(encoding: Encoding) u32 {
    return switch (encoding) {
        .x86_movabs => 10,
        .arm_movwide => 16,
    };
}

/// Validate the complete producer-selected instruction template before mutation.
/// Register/opcode bits are checked but never changed by binding.
pub fn validate(code: []const u8, relocation: Relocation) Error!void {
    const start: usize = relocation.offset;
    const size: usize = width(relocation.encoding);
    if (start > code.len or size > code.len - start) return error.InvalidContextRelocation;
    const bytes = code[start..][0..size];
    switch (relocation.encoding) {
        .x86_movabs => {
            if ((bytes[0] != 0x48 and bytes[0] != 0x49) or bytes[1] < 0xb8 or bytes[1] > 0xbf)
                return error.InvalidContextRelocation;
        },
        .arm_movwide => {
            const reg = std.mem.readInt(u32, bytes[0..4], .little) & 31;
            for (0..4) |i| {
                const word = std.mem.readInt(u32, bytes[i * 4 ..][0..4], .little);
                const expected: u32 = (if (i == 0) @as(u32, 0xd2800000) else 0xf2800000) |
                    (@as(u32, @intCast(i)) << 21) | reg;
                if (word & ~@as(u32, 0x001fffe0) != expected) return error.InvalidContextRelocation;
            }
        },
    }
}

pub fn patch(code: []u8, relocation: Relocation, value: u64) Error!void {
    try validate(code, relocation);
    const bytes = code[relocation.offset..];
    switch (relocation.encoding) {
        .x86_movabs => std.mem.writeInt(u64, bytes[2..10], value, .little),
        .arm_movwide => {
            for (0..4) |i| {
                const shift: u6 = @intCast(i * 16);
                const imm: u16 = @truncate(value >> shift);
                const slot = bytes[i * 4 ..][0..4];
                const word = std.mem.readInt(u32, slot, .little);
                std.mem.writeInt(u32, slot, (word & ~@as(u32, 0x001fffe0)) | (@as(u32, imm) << 5), .little);
            }
        },
    }
}

/// Extraction emits one owned binding per site in code order. Requiring that
/// canonical correspondence proves complete coverage in one pass, including
/// bindings whose relocation was removed from an otherwise complete artifact.
pub fn validateRecords(code: []const u8, relocations: []const Relocation, binding_count: usize) Error!void {
    if (relocations.len != binding_count) return error.MissingContextBinding;
    var end: usize = 0;
    for (relocations, 0..) |relocation, index| {
        if (relocation.binding != index) return error.MissingContextBinding;
        try validate(code, relocation);
        if (relocation.offset < end) return error.InvalidContextRelocation;
        end = @as(usize, relocation.offset) + width(relocation.encoding);
    }
}

/// Validate all records and bindings before changing any code.
pub fn bind(code: []u8, relocations: []const Relocation, values: []const u64) Error!void {
    try validateRecords(code, relocations, values.len);
    for (relocations) |relocation| try patch(code, relocation, values[relocation.binding]);
}

test "context immediates validate fixed shape and bind displaced IDs" {
    var x86 = [_]u8{ 0x49, 0xba, 0, 0, 0, 0, 0, 0, 0, 0 };
    const xr: Relocation = .{ .offset = 0, .binding = 0, .encoding = .x86_movabs };
    try bind(&x86, &.{xr}, &.{0xfedcba9876543210});
    try std.testing.expectEqual(@as(u64, 0xfedcba9876543210), std.mem.readInt(u64, x86[2..10], .little));
    try std.testing.expectEqual(@as(u8, 0x49), x86[0]);
    var arm: [16]u8 = undefined;
    for (0..4) |i| {
        const word: u32 = (if (i == 0) @as(u32, 0xd2800000) else 0xf2800000) |
            (@as(u32, @intCast(i)) << 21) | 7;
        std.mem.writeInt(u32, arm[i * 4 ..][0..4], word, .little);
    }
    const ar: Relocation = .{ .offset = 0, .binding = 0, .encoding = .arm_movwide };
    try bind(&arm, &.{ar}, &.{0xfedcba9876543210});
    var decoded: u64 = 0;
    for (0..4) |i| {
        const word = std.mem.readInt(u32, arm[i * 4 ..][0..4], .little);
        decoded |= @as(u64, (word >> 5) & 0xffff) << @as(u6, @intCast(i * 16));
    }
    try std.testing.expectEqual(@as(u64, 0xfedcba9876543210), decoded);
    try validate(&arm, ar);
}

test "context immediates reject missing bindings and corrupt templates atomically" {
    var code = [_]u8{ 0x48, 0xb8, 0, 0, 0, 0, 0, 0, 0, 0 };
    const good: Relocation = .{ .offset = 0, .binding = 0, .encoding = .x86_movabs };
    var bad = good;
    bad.offset = 1;
    bad.binding = 1;
    try std.testing.expectError(error.InvalidContextRelocation, bind(&code, &.{ good, bad }, &.{ 42, 43 }));
    try std.testing.expectEqual(@as(u64, 0), std.mem.readInt(u64, code[2..10], .little));
    bad = good;
    bad.binding = 1;
    try std.testing.expectError(error.MissingContextBinding, bind(&code, &.{bad}, &.{42}));
    code[1] = 0xc7;
    try std.testing.expectError(error.InvalidContextRelocation, validate(&code, good));
}

test "context immediates require canonical complete binding coverage atomically" {
    var code = [_]u8{ 0x48, 0xb8, 1, 2, 3, 4, 5, 6, 7, 8 } ** 2;
    const original = code;
    const first: Relocation = .{ .offset = 0, .binding = 0, .encoding = .x86_movabs };
    const second: Relocation = .{ .offset = 10, .binding = 1, .encoding = .x86_movabs };
    try std.testing.expectError(error.MissingContextBinding, bind(&code, &.{}, &.{42}));
    try std.testing.expectError(error.MissingContextBinding, bind(&code, &.{first}, &.{ 42, 43 }));
    try std.testing.expectError(error.MissingContextBinding, bind(&code, &.{ first, second }, &.{42}));
    var invalid = second;
    invalid.binding = 0;
    try std.testing.expectError(error.MissingContextBinding, bind(&code, &.{ first, invalid }, &.{ 42, 43 }));
    invalid.binding = 2;
    try std.testing.expectError(error.MissingContextBinding, bind(&code, &.{ first, invalid }, &.{ 42, 43 }));
    try std.testing.expectEqualSlices(u8, &original, &code);
    try bind(&code, &.{ first, second }, &.{ 0, 0xfedcba9876543210 });
    try std.testing.expectEqual(@as(u64, 0), std.mem.readInt(u64, code[2..10], .little));
    try std.testing.expectEqual(@as(u64, 0xfedcba9876543210), std.mem.readInt(u64, code[12..20], .little));
}
