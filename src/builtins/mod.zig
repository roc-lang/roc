//! Zig module for the roc builtins
const std = @import("std");

pub const builtin_registry = @import("builtin_registry.zig");
pub const compiler_rt_128 = @import("compiler_rt_128.zig");
pub const native_runtime_libcalls = @import("native_runtime_libcalls.zig");
pub const host_abi = @import("host_abi.zig");
pub const shim_symbols = @import("shim_symbols.zig");
pub const dec = @import("dec.zig");
pub const crypto = @import("crypto.zig");
pub const dev_wrappers = @import("dev_wrappers.zig");
pub const erased_callable = @import("erased_callable.zig");
pub const float_bits = @import("float_bits.zig");
pub const float_math_f32 = @import("float_math/f32.zig");
pub const hash = @import("hash.zig");
pub const list = @import("list.zig");
pub const num = @import("num.zig");
pub const numeric_conversions = @import("numeric_conversions.zig");
pub const sort = @import("sort.zig");
pub const str = @import("str.zig");
pub const utils = @import("utils.zig");
pub const float_math_tan = @import("float_math/tan.zig");

test "builtins tests" {
    std.testing.refAllDecls(@import("builtin_registry.zig"));
    std.testing.refAllDecls(@import("crypto.zig"));
    std.testing.refAllDecls(@import("dec.zig"));
    std.testing.refAllDecls(@import("dev_wrappers.zig"));
    std.testing.refAllDecls(@import("erased_callable.zig"));
    std.testing.refAllDecls(@import("float_bits.zig"));
    std.testing.refAllDecls(@import("float_math/f32.zig"));
    std.testing.refAllDecls(@import("float_math/tan.zig"));
    std.testing.refAllDecls(@import("hash.zig"));
    std.testing.refAllDecls(@import("host_abi.zig"));
    std.testing.refAllDecls(@import("shim_symbols.zig"));
    std.testing.refAllDecls(@import("list.zig"));
    std.testing.refAllDecls(@import("native_runtime_libcalls.zig"));
    std.testing.refAllDecls(@import("num.zig"));
    std.testing.refAllDecls(@import("numeric_conversions.zig"));
    std.testing.refAllDecls(@import("sort.zig"));
    std.testing.refAllDecls(@import("str.zig"));
    std.testing.refAllDecls(@import("utils.zig"));
}
