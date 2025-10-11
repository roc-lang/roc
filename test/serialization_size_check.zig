//! Checks that all Serialized types have the same size on 32-bit and 64-bit platforms.
//!
//! This is critical because we serialize these types and expect them to be portable
//! across architectures. If a Serialized type contains pointers/slices, it will have
//! different sizes on different platforms, which breaks our serialization.
//!
//! Run with: zig build test-serialization-sizes

const std = @import("std");
const builtin = @import("builtin");
const collections = @import("collections");
const can = @import("can");

const ModuleEnv = can.ModuleEnv;
const NodeStore = can.CIR.NodeStore;
const SafeList = collections.SafeList;
const SafeMultiList = collections.SafeMultiList;

pub fn main() !void {
    const stdout = std.io.getStdOut().writer();

    // Print platform info
    try stdout.print("Target: {s}-{s}\n", .{ @tagName(builtin.cpu.arch), @tagName(builtin.os.tag) });
    try stdout.print("Pointer size: {} bytes\n", .{@sizeOf(usize)});
    try stdout.print("\n", .{});

    // Print sizes of all Serialized types
    try stdout.print("ModuleEnv.Serialized: {} bytes\n", .{@sizeOf(ModuleEnv.Serialized)});
    try stdout.print("NodeStore.Serialized: {} bytes\n", .{@sizeOf(NodeStore.Serialized)});
    try stdout.print("SafeList(u8).Serialized: {} bytes\n", .{@sizeOf(SafeList(u8).Serialized)});
    try stdout.print("SafeList(u32).Serialized: {} bytes\n", .{@sizeOf(SafeList(u32).Serialized)});

    // Test a few common types used in SafeMultiList
    const TestStruct = struct { a: u32, b: u8 };
    try stdout.print("SafeMultiList(TestStruct).Serialized: {} bytes\n", .{@sizeOf(SafeMultiList(TestStruct).Serialized)});

    // Print sizes of Node-related types
    const Node = can.CIR.Node;
    try stdout.print("SafeMultiList(Node).Serialized: {} bytes\n", .{@sizeOf(SafeMultiList(Node).Serialized)});

    // Also check that runtime types are LARGER than Serialized (sanity check)
    try stdout.print("\n", .{});
    try stdout.print("Runtime size checks:\n", .{});
    try stdout.print("ModuleEnv: {} bytes (should be >= Serialized)\n", .{@sizeOf(ModuleEnv)});
    try stdout.print("NodeStore: {} bytes (should be >= Serialized)\n", .{@sizeOf(NodeStore)});
    try stdout.print("SafeList(u8): {} bytes (should be >= Serialized)\n", .{@sizeOf(SafeList(u8))});
    try stdout.print("SafeMultiList(TestStruct): {} bytes (should be >= Serialized)\n", .{@sizeOf(SafeMultiList(TestStruct))});
}
