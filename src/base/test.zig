const std = @import("std");

test "base tests" {
    std.testing.refAllDecls(@import("DataSpan.zig"));
    std.testing.refAllDecls(@import("Ident.zig"));
    std.testing.refAllDecls(@import("PackedDataSpan.zig"));
    std.testing.refAllDecls(@import("parallel.zig"));
    std.testing.refAllDecls(@import("Region.zig"));
    std.testing.refAllDecls(@import("RegionInfo.zig"));
    std.testing.refAllDecls(@import("safe_memory.zig"));
    std.testing.refAllDecls(@import("Scratch.zig"));
    std.testing.refAllDecls(@import("SExprTree.zig"));
    std.testing.refAllDecls(@import("SmallStringInterner.zig"));
    std.testing.refAllDecls(@import("SmallStringInterner.zig"));
    std.testing.refAllDecls(@import("StringLiteral.zig"));
    std.testing.refAllDecls(@import("target.zig"));
}
