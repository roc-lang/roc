//! Zig module for the roc builtins
const std = @import("std");

pub const host_abi = @import("host_abi.zig");
pub const dec = @import("dec.zig");
pub const hash = @import("hash.zig");
pub const list = @import("list.zig");
pub const num = @import("num.zig");
pub const sort = @import("sort.zig");
pub const str = @import("str.zig");
pub const utils = @import("utils.zig");

test "builtins tests" {
    std.testing.refAllDecls(@import("dec.zig"));
    std.testing.refAllDecls(@import("hash.zig"));
    std.testing.refAllDecls(@import("host_abi.zig"));
    std.testing.refAllDecls(@import("list.zig"));
    std.testing.refAllDecls(@import("num.zig"));
    std.testing.refAllDecls(@import("sort.zig"));
    std.testing.refAllDecls(@import("str.zig"));
    std.testing.refAllDecls(@import("utils.zig"));
}
