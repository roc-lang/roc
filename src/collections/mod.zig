//! Collection utilities and memory alignment constants for the Roc compiler.
//!
//! This module provides utilities for managing collections and defines
//! memory alignment constants used throughout the compiler, particularly
//! for stack allocations in the interpreter.

const std = @import("std");

/// The highest alignment any Roc type can have.
/// This is used as the base alignment for the allocation used
/// in the interpreter for stack allocations.
pub const max_roc_alignment: std.mem.Alignment = .@"16";

/// Helper for creating an Io.Writer.Allocating from a deprecated Managed(u8).
/// Zig 0.16 removed Managed.writer(); this bridges the gap.
pub fn managedWriter(managed: *std.array_list.Managed(u8)) std.Io.Writer.Allocating {
    var unmanaged: std.ArrayList(u8) = .{ .items = managed.items, .capacity = managed.capacity, .pointer_stability = .{} };
    return std.Io.Writer.Allocating.fromArrayList(managed.allocator, &unmanaged);
}

/// Sync an Io.Writer.Allocating back to a Managed(u8).
pub fn managedWriterFinish(aw: *std.Io.Writer.Allocating, managed: *std.array_list.Managed(u8)) void {
    const unmanaged = aw.toArrayList();
    managed.items = unmanaged.items;
    managed.capacity = unmanaged.capacity;
}

pub const SafeList = @import("safe_list.zig").SafeList;
pub const SafeRange = @import("safe_list.zig").SafeRange;
pub const SafeMultiList = @import("safe_list.zig").SafeMultiList;
pub const validateRelocatedSpan = @import("safe_list.zig").validateRelocatedSpan;
pub const GuardedList = @import("GuardedList.zig");

pub const IndexedStack = @import("IndexedStack.zig").IndexedStack;

pub const DenseMap = @import("DenseMap.zig").DenseMap;
pub const RingQueue = @import("RingQueue.zig").RingQueue;
pub const VersionedDenseMap = @import("VersionedMap.zig").VersionedDenseMap;
pub const VersionedHashMap = @import("VersionedMap.zig").VersionedHashMap;
pub const DenseMapPool = @import("DenseMap.zig").DenseMapPool;
pub const ScopedBitSet = @import("ScopedBitSet.zig");
pub const RekeyingHashMap = @import("RekeyingHashMap.zig").RekeyingHashMap;
/// Loop-nesting forest of a reducible directed graph.
pub const LoopForest = @import("LoopForest.zig");

/// Any/all evaluation over nested groups of leaves on explicit stacks.
pub const AnyAll = @import("any_all.zig");

pub const SortedArrayBuilder = @import("SortedArrayBuilder.zig").SortedArrayBuilder;
pub const ExposedItems = @import("ExposedItems.zig").ExposedItems;
pub const ExposedItemTarget = @import("ExposedItems.zig").ExposedItemTarget;
pub const CompactWriter = @import("CompactWriter.zig");
pub const serde_validation = @import("serde_validation.zig");
pub const validateSerializedRelocations = serde_validation.validateSerializedRelocations;

/// Single-threaded arena allocator; the non-atomic counterpart to
/// `std.heap.ArenaAllocator`.
pub const SingleThreadArena = @import("SingleThreadArena.zig");

/// Serialization format definitions for embedded module data.
pub const serialization = @import("serialization.zig");
pub const SerializedHeader = serialization.SerializedHeader;
pub const SerializedModuleInfo = serialization.SerializedModuleInfo;
pub const SERIALIZED_FORMAT_MAGIC = serialization.SERIALIZED_FORMAT_MAGIC;
pub const SERIALIZED_FORMAT_VERSION = serialization.SERIALIZED_FORMAT_VERSION;

/// Re-exported alignment constant from CompactWriter for convenience.
/// This alignment is required for all serialization buffers to ensure proper memory access.
pub const SERIALIZATION_ALIGNMENT = CompactWriter.SERIALIZATION_ALIGNMENT;

/// A range that must have at least one element
pub const NonEmptyRange = struct {
    /// Starting index (inclusive)
    start: u32,
    /// Number of elements (must be > 0)
    count: u32,

    /// Convert to a SafeMultiList range
    pub fn toRange(self: NonEmptyRange, comptime Idx: type) SafeRange(Idx) {
        std.debug.assert(self.count > 0);
        return .{
            .start = @fromBackingInt(@intCast(self.start)),
            .count = self.count,
        };
    }
};

test "collections tests" {
    std.testing.refAllDecls(@import("CompactWriter.zig"));
    std.testing.refAllDecls(@import("ExposedItems.zig"));
    std.testing.refAllDecls(@import("safe_list.zig"));
    std.testing.refAllDecls(@import("GuardedList.zig"));
    std.testing.refAllDecls(@import("serialization.zig"));
    std.testing.refAllDecls(@import("SortedArrayBuilder.zig"));
    std.testing.refAllDecls(@import("SingleThreadArena.zig"));
    std.testing.refAllDecls(@import("DenseMap.zig"));
    std.testing.refAllDecls(@import("RingQueue.zig"));
    std.testing.refAllDecls(@import("any_all.zig"));
    std.testing.refAllDecls(@import("VersionedMap.zig"));
    std.testing.refAllDecls(@import("IndexedStack.zig"));
    std.testing.refAllDecls(@import("ScopedBitSet.zig"));
    std.testing.refAllDecls(@import("RekeyingHashMap.zig"));
    std.testing.refAllDecls(@import("LoopForest.zig"));
}
