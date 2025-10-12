//! Checks that all Serialized types have the same size on 32-bit and 64-bit platforms.
//!
//! This is critical because we serialize these types and expect them to be portable
//! across architectures. If a Serialized type contains pointers/slices, it will have
//! different sizes on different platforms, which breaks our serialization.
//!
//! This test uses compile-time assertions to verify that all Serialized types match
//! their expected platform-independent sizes. The build will fail if any type has
//! the wrong size, preventing accidental introduction of pointers/slices.
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

const TestStruct = struct { a: u32, b: u8 };
const Node = can.CIR.Node;

// Expected sizes for Serialized types (platform-independent, matching 32-bit wasm32)
// NOTE: These constants must be updated if the Serialized type definitions change.
// When you modify a Serialized type, rebuild with `zig build test-serialization-sizes`
// and update these constants based on the compile error messages.
const expected_safelist_u8_size = 24;
const expected_safelist_u32_size = 24;
const expected_safemultilist_teststruct_size = 24;
const expected_safemultilist_node_size = 24;
const expected_moduleenv_size = 600; // Platform-independent size
const expected_nodestore_size = 96; // Platform-independent size

// Compile-time assertions - build will fail if sizes don't match expected values
comptime {
    const actual_safelist_u8 = @sizeOf(SafeList(u8).Serialized);
    if (actual_safelist_u8 != expected_safelist_u8_size) {
        @compileError(std.fmt.comptimePrint(
            "SafeList(u8).Serialized size mismatch: expected {d}, got {d}. " ++
                "This indicates the type contains pointers/slices. Update the type definition or the expected size constant.",
            .{ expected_safelist_u8_size, actual_safelist_u8 },
        ));
    }

    const actual_safelist_u32 = @sizeOf(SafeList(u32).Serialized);
    if (actual_safelist_u32 != expected_safelist_u32_size) {
        @compileError(std.fmt.comptimePrint(
            "SafeList(u32).Serialized size mismatch: expected {d}, got {d}. " ++
                "This indicates the type contains pointers/slices. Update the type definition or the expected size constant.",
            .{ expected_safelist_u32_size, actual_safelist_u32 },
        ));
    }

    const actual_safemultilist_teststruct = @sizeOf(SafeMultiList(TestStruct).Serialized);
    if (actual_safemultilist_teststruct != expected_safemultilist_teststruct_size) {
        @compileError(std.fmt.comptimePrint(
            "SafeMultiList(TestStruct).Serialized size mismatch: expected {d}, got {d}. " ++
                "This indicates the type contains pointers/slices. Update the type definition or the expected size constant.",
            .{ expected_safemultilist_teststruct_size, actual_safemultilist_teststruct },
        ));
    }

    const actual_safemultilist_node = @sizeOf(SafeMultiList(Node).Serialized);
    if (actual_safemultilist_node != expected_safemultilist_node_size) {
        @compileError(std.fmt.comptimePrint(
            "SafeMultiList(Node).Serialized size mismatch: expected {d}, got {d}. " ++
                "This indicates the type contains pointers/slices. Update the type definition or the expected size constant.",
            .{ expected_safemultilist_node_size, actual_safemultilist_node },
        ));
    }

    const actual_moduleenv = @sizeOf(ModuleEnv.Serialized);
    if (actual_moduleenv != expected_moduleenv_size) {
        @compileError(std.fmt.comptimePrint(
            "ModuleEnv.Serialized size mismatch: expected {d}, got {d}. " ++
                "This indicates the type contains pointers/slices. Update the type definition or the expected size constant.",
            .{ expected_moduleenv_size, actual_moduleenv },
        ));
    }

    const actual_nodestore = @sizeOf(NodeStore.Serialized);
    if (actual_nodestore != expected_nodestore_size) {
        @compileError(std.fmt.comptimePrint(
            "NodeStore.Serialized size mismatch: expected {d}, got {d}. " ++
                "This indicates the type contains pointers/slices. Update the type definition or the expected size constant.",
            .{ expected_nodestore_size, actual_nodestore },
        ));
    }
}

pub fn main() void {
    // If we compile successfully on both platforms, print success
    if (builtin.os.tag != .freestanding) {
        std.debug.print("✓ Serialization size check passed - all types have correct platform-independent sizes\n", .{});
    }
}
