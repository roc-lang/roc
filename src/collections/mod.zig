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
/// Backings a growing list replaced while readers may still hold slices of them.
pub const SupersededBackings = @import("SupersededBackings.zig");

pub const IndexedStack = @import("IndexedStack.zig").IndexedStack;
pub const WorkStacks = @import("WorkStacks.zig").WorkStacks;

pub const DenseMap = @import("DenseMap.zig").DenseMap;
/// A direct-indexed map over dense IDs that takes its allocator per call.
pub const DenseMapUnmanaged = @import("DenseMap.zig").DenseMapUnmanaged;
pub const RingQueue = @import("RingQueue.zig").RingQueue;
/// A multimap whose buckets are chains through one shared node list.
pub const ChainedBuckets = @import("ChainedBuckets.zig").ChainedBuckets;
pub const AutoChainedBuckets = @import("ChainedBuckets.zig").AutoChainedBuckets;
pub const VersionedDenseMap = @import("VersionedMap.zig").VersionedDenseMap;
pub const VersionedHashMap = @import("VersionedMap.zig").VersionedHashMap;
pub const DenseMapPool = @import("DenseMap.zig").DenseMapPool;
/// A write-once slot for a fact computed by whichever consumer first needs it.
pub const OnceSlot = @import("OnceSlot.zig").OnceSlot;
pub const ScopedBitSet = @import("ScopedBitSet.zig");
pub const RekeyingHashMap = @import("RekeyingHashMap.zig").RekeyingHashMap;
/// Loop-nesting forest of a reducible directed graph.
pub const LoopForest = @import("LoopForest.zig");
/// Union-find over dense indices.
pub const DisjointSets = @import("DisjointSets.zig");
/// Strongly connected components in topological order.
pub const scc = @import("scc.zig");
/// Compressed adjacency lists.
pub const csr = @import("csr.zig");
/// Developer-diagnostic counters for table storage work.
pub const storage_work = @import("storage_work.zig");
/// Fresh zero pages and growing-column reservations for lifecycle-aware tables.
pub const zeroed_pages = @import("zeroed_pages.zig");
/// A table whose entries are each written before being read.
pub const WriteFirstSlice = @import("WriteFirstSlice.zig").WriteFirstSlice;
/// Optional typed indices in four bytes per entry.
pub const OptionalIndexColumn = @import("OptionalIndexColumn.zig").OptionalIndexColumn;
/// The same compact optional-ID encoding in a persisted POD value.
pub const OptionalId = @import("optional_id.zig").OptionalId;
/// Compact optional values for a private one-based ID domain.
pub const ZeroSentinelId = @import("optional_id.zig").ZeroSentinelId;
/// An optional raw `u32` in four bytes.
pub const OptionalU32 = @import("optional_id.zig").OptionalU32;
/// An optional small enum in one byte.
pub const OptionalEnum = @import("optional_id.zig").OptionalEnum;
/// An optional struct at the struct's own size, keyed by one biased field.
pub const OptionalStruct = @import("optional_id.zig").OptionalStruct;
/// The payload a compact optional form stores, for reflective walkers.
pub const CompactOptionalPayload = @import("optional_id.zig").CompactOptionalPayload;
/// Optional physical indices for stores that leave slot zero unused.
pub const ZeroSentinelIndexColumn = @import("OptionalIndexColumn.zig").ZeroSentinelIndexColumn;
/// Reset-many scratch keyed by epoch stamps.
pub const epochs = @import("epochs.zig");
pub const EpochSet = epochs.EpochSet;
pub const EpochColumn = epochs.EpochColumn;
/// Append-only columns that grow on virtual reservations without copying.
pub const ReservedList = @import("ReservedList.zig").ReservedList;
pub const ReservedColumns = @import("ReservedList.zig").ReservedColumns;
/// A mark-and-reset bump allocator for per-pass tables.
pub const ScratchArena = @import("ScratchArena.zig");
/// Items grouped by key: CSR for dense keys, sorted pairs for sparse keys.
pub const groups = @import("groups.zig");
pub const CsrGroups = groups.CsrGroups;
pub const SortedGroups = groups.SortedGroups;

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

/// Allocator that refuses in-place growth, so allocation-failure tests count
/// the same allocations on every run.
pub const DeterministicAllocator = @import("DeterministicAllocator.zig");

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
    std.testing.refAllDecls(@import("SupersededBackings.zig"));
    std.testing.refAllDecls(@import("serialization.zig"));
    std.testing.refAllDecls(@import("SortedArrayBuilder.zig"));
    std.testing.refAllDecls(@import("SingleThreadArena.zig"));
    std.testing.refAllDecls(@import("DeterministicAllocator.zig"));
    std.testing.refAllDecls(@import("DenseMap.zig"));
    std.testing.refAllDecls(@import("RingQueue.zig"));
    std.testing.refAllDecls(@import("OnceSlot.zig"));
    std.testing.refAllDecls(@import("ChainedBuckets.zig"));
    std.testing.refAllDecls(@import("any_all.zig"));
    std.testing.refAllDecls(@import("VersionedMap.zig"));
    std.testing.refAllDecls(@import("IndexedStack.zig"));
    std.testing.refAllDecls(@import("WorkStacks.zig"));
    std.testing.refAllDecls(@import("ScopedBitSet.zig"));
    std.testing.refAllDecls(@import("RekeyingHashMap.zig"));
    std.testing.refAllDecls(@import("LoopForest.zig"));
    std.testing.refAllDecls(@import("DisjointSets.zig"));
    std.testing.refAllDecls(@import("scc.zig"));
    std.testing.refAllDecls(@import("csr.zig"));
    std.testing.refAllDecls(@import("storage_work.zig"));
    std.testing.refAllDecls(@import("optional_id.zig"));
    std.testing.refAllDecls(@import("zeroed_pages.zig"));
    std.testing.refAllDecls(@import("WriteFirstSlice.zig"));
    std.testing.refAllDecls(@import("OptionalIndexColumn.zig"));
    std.testing.refAllDecls(@import("epochs.zig"));
    std.testing.refAllDecls(@import("ReservedList.zig"));
    std.testing.refAllDecls(@import("ScratchArena.zig"));
    std.testing.refAllDecls(@import("groups.zig"));
}
