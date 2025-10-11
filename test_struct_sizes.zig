const std = @import("std");
const can = @import("can");
const ModuleEnv = can.ModuleEnv;
const CommonEnv = @import("base").CommonEnv;
const TypeStore = @import("types").Store;

pub fn main() !void {
    const stdout = std.io.getStdOut().writer();

    try stdout.print("=== Struct Size Analysis ===\n", .{});
    try stdout.print("Platform: {s}\n\n", .{@tagName(std.builtin.cpu.arch)});

    try stdout.print("ModuleEnv struct sizes:\n", .{});
    try stdout.print("  ModuleEnv:            {} bytes\n", .{@sizeOf(ModuleEnv)});
    try stdout.print("  ModuleEnv.Serialized: {} bytes\n\n", .{@sizeOf(ModuleEnv.Serialized)});

    try stdout.print("CommonEnv struct sizes:\n", .{});
    try stdout.print("  CommonEnv:            {} bytes\n", .{@sizeOf(CommonEnv)});
    try stdout.print("  CommonEnv.Serialized: {} bytes\n\n", .{@sizeOf(CommonEnv.Serialized)});

    try stdout.print("TypeStore struct sizes:\n", .{});
    try stdout.print("  TypeStore:            {} bytes\n", .{@sizeOf(TypeStore)});
    try stdout.print("  TypeStore.Serialized: {} bytes\n\n", .{@sizeOf(TypeStore.Serialized)});

    try stdout.print("Field offsets in ModuleEnv.Serialized:\n", .{});
    try stdout.print("  gpa_placeholder:      offset={}\n", .{@offsetOf(ModuleEnv.Serialized, "gpa_placeholder")});
    try stdout.print("  common:               offset={}\n", .{@offsetOf(ModuleEnv.Serialized, "common")});
    try stdout.print("  types:                offset={}\n", .{@offsetOf(ModuleEnv.Serialized, "types")});
    try stdout.print("  module_kind:          offset={}\n", .{@offsetOf(ModuleEnv.Serialized, "module_kind")});
    try stdout.print("  all_defs:             offset={}\n", .{@offsetOf(ModuleEnv.Serialized, "all_defs")});
    try stdout.print("  all_statements:       offset={}\n", .{@offsetOf(ModuleEnv.Serialized, "all_statements")});
    try stdout.print("  exports:              offset={}\n", .{@offsetOf(ModuleEnv.Serialized, "exports")});
}
