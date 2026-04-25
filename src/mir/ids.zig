//! Shared MIR-family identifiers.

const std = @import("std");

pub const RecordShapeId = enum(u32) { _ };
pub const RecordFieldId = enum(u32) { _ };
pub const TagUnionShapeId = enum(u32) { _ };
pub const TagId = enum(u32) { _ };
pub const TagPayloadId = enum(u32) { _ };

pub fn Span(comptime T: type) type {
    return extern struct {
        start: u32,
        len: u32,

        pub fn empty() @This() {
            return .{ .start = 0, .len = 0 };
        }

        pub fn get(self: @This(), items: []const T) []const T {
            if (self.len == 0) return &.{};
            return items[self.start..][0..self.len];
        }
    };
}

test "mir ids tests" {
    std.testing.refAllDecls(@This());
}
