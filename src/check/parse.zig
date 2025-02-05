const std = @import("std");
const base = @import("../base.zig");

const parse = @This();
pub const IR = @import("ir.zig").IR;

pub fn parseModule(body: []u8) parse.IR {
    _ = body;

    @panic("not implemented");
}
