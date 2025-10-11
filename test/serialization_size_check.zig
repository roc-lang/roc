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

const TestStruct = struct { a: u32, b: u8 };
const Node = can.CIR.Node;

// Export sizes as functions that return the size values
// This works on all targets including wasm
export fn get_pointer_size() usize {
    return @sizeOf(usize);
}

export fn get_moduleenv_serialized_size() usize {
    return @sizeOf(ModuleEnv.Serialized);
}

export fn get_nodestore_serialized_size() usize {
    return @sizeOf(NodeStore.Serialized);
}

export fn get_safelist_u8_serialized_size() usize {
    return @sizeOf(SafeList(u8).Serialized);
}

export fn get_safelist_u32_serialized_size() usize {
    return @sizeOf(SafeList(u32).Serialized);
}

export fn get_safemultilist_teststruct_serialized_size() usize {
    return @sizeOf(SafeMultiList(TestStruct).Serialized);
}

export fn get_safemultilist_node_serialized_size() usize {
    return @sizeOf(SafeMultiList(Node).Serialized);
}

pub fn main() void {
    // The exported functions above contain the size information
    // A test runner can call these functions to verify sizes match across platforms
}
