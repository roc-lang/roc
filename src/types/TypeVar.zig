const std = @import("std");
const collections = @import("collections");

id: u32,

pub const List = collections.SafeMultiList(@This());
pub const Idx = List.Idx;

pub const Store = struct {
    next: u32,

    pub fn init() @This() {
        Store{ .next = 0 };
    }
};
