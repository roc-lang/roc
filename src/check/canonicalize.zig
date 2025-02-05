const std = @import("std");
const base = @import("../base.zig");
const parse = @import("parse.zig");

const can = @This();
pub const IR = @import("ir.zig").IR;

pub fn canonicalize(
    parse_ir: parse.IR,
    other_modules: std.HashMap(base.ModuleId, can.IR),
) can.IR {
    _ = parse_ir;
    _ = other_modules;

    @panic("not implemented");
}
