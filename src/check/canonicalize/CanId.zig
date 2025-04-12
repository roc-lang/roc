const std = @import("std");

/// The index of a canonical IR node within the array of nodes.
const Self = @This();

value: u32,

pub fn init(value: u32) Self {
    return Self{ .value = value };
}

pub fn fromU32(value: u32) Self {
    return Self{ .value = value };
}

pub fn toU32(self: Self) u32 {
    return self.value;
}
