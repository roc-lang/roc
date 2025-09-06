//! Core utility functions and types for the Roc runtime builtins.
//!
//! This module provides essential infrastructure for builtin operations,
//! including memory allocation interfaces, overflow detection utilities,
//! debug functions, and common types used throughout the builtin modules.
//! It serves as the foundation layer that other builtin modules depend on
//! for low-level operations and host interface functions.
const std = @import("std");
const builtin = @import("builtin");

const RocOps = @import("host_abi.zig").RocOps;
const RocDealloc = @import("host_abi.zig").RocDealloc;
const RocAlloc = @import("host_abi.zig").RocAlloc;
const RocRealloc = @import("host_abi.zig").RocRealloc;
const RocDbg = @import("host_abi.zig").RocDbg;
const RocExpectFailed = @import("host_abi.zig").RocExpectFailed;
const RocCrashed = @import("host_abi.zig").RocCrashed;

const DEBUG_INCDEC = false;
const DEBUG_TESTING_ALLOC = false;
const DEBUG_ALLOC = false;

/// Tracks allocations for testing purposes with C ABI compatibility. Uses a single global testing allocator to track allocations. If we need multiple independent allocators we will need to modify this and use comptime.
pub const TestEnv = struct {
    const AllocationInfo = struct {
        size: usize,
        alignment: usize,
    };
    const AllocationMap = std.HashMap(*anyopaque, AllocationInfo, std.hash_map.AutoContext(*anyopaque), std.hash_map.default_max_load_percentage);

    allocation_map: AllocationMap,
    allocator: std.mem.Allocator,
    ops: ?RocOps,

    pub fn init(allocator: std.mem.Allocator) TestEnv {
        return TestEnv{
            .allocation_map = AllocationMap.init(allocator),
            .allocator = allocator,
            .ops = null,
        };
    }

    pub fn getOps(self: *TestEnv) *RocOps {
        if (self.ops == null) {
            self.ops = RocOps{
                .env = @as(*anyopaque, @ptrCast(self)),
                .roc_alloc = rocAllocFn,
                .roc_dealloc = rocDeallocFn,
                .roc_realloc = rocReallocFn,
                .roc_dbg = rocDbgFn,
                .roc_expect_failed = rocExpectFailedFn,
                .roc_crashed = rocCrashedFn,
                .host_fns = undefined, // No host functions in tests
            };
        }
        return &self.ops.?;
    }

    pub fn deinit(self: *TestEnv) void {
        // Free any remaining allocations
        var iterator = self.allocation_map.iterator();
        while (iterator.next()) |entry| {
            const bytes: [*]u8 = @ptrCast(@alignCast(entry.key_ptr.*));
            const slice = bytes[0..entry.value_ptr.size];
            // For aligned allocations, we need to free them properly
            switch (entry.value_ptr.alignment) {
                1 => self.allocator.free(slice),
                2 => self.allocator.free(@as([]align(2) u8, @alignCast(slice))),
                4 => self.allocator.free(@as([]align(4) u8, @alignCast(slice))),
                8 => self.allocator.free(@as([]align(8) u8, @alignCast(slice))),
                16 => self.allocator.free(@as([]align(16) u8, @alignCast(slice))),
                else => @panic("Unsupported alignment in test deallocator cleanup"),
            }
        }

        self.allocation_map.deinit();
    }

    pub fn getAllocationCount(self: *const TestEnv) usize {
        return self.allocation_map.count();
    }

    fn rocAllocFn(roc_alloc: *RocAlloc, env: *anyopaque) callconv(.C) void {
        const self: *TestEnv = @ptrCast(@alignCast(env));

        // Allocate memory using the testing allocator with comptime alignment
        const ptr = switch (roc_alloc.alignment) {
            1 => self.allocator.alignedAlloc(u8, 1, roc_alloc.length),
            2 => self.allocator.alignedAlloc(u8, 2, roc_alloc.length),
            4 => self.allocator.alignedAlloc(u8, 4, roc_alloc.length),
            8 => self.allocator.alignedAlloc(u8, 8, roc_alloc.length),
            16 => self.allocator.alignedAlloc(u8, 16, roc_alloc.length),
            else => @panic("Unsupported alignment in test allocator"),
        } catch {
            @panic("Test allocation failed");
        };

        // Cast the pointer to *anyopaque
        const result: *anyopaque = @ptrCast(ptr.ptr);

        // Save the allocation in the map
        self.allocation_map.put(result, AllocationInfo{
            .size = roc_alloc.length,
            .alignment = roc_alloc.alignment,
        }) catch {
            self.allocator.free(ptr);
            @panic("Failed to track test allocation");
        };

        roc_alloc.answer = result;
    }

    fn rocDeallocFn(roc_dealloc: *RocDealloc, env: *anyopaque) callconv(.C) void {
        const self: *TestEnv = @ptrCast(@alignCast(env));

        if (self.allocation_map.fetchRemove(roc_dealloc.ptr)) |entry| {
            const bytes: [*]u8 = @ptrCast(@alignCast(roc_dealloc.ptr));
            const slice = bytes[0..entry.value.size];
            // For aligned allocations, we need to free them properly
            switch (entry.value.alignment) {
                1 => self.allocator.free(slice),
                2 => self.allocator.free(@as([]align(2) u8, @alignCast(slice))),
                4 => self.allocator.free(@as([]align(4) u8, @alignCast(slice))),
                8 => self.allocator.free(@as([]align(8) u8, @alignCast(slice))),
                16 => self.allocator.free(@as([]align(16) u8, @alignCast(slice))),
                else => @panic("Unsupported alignment in test deallocator"),
            }
        }
    }

    fn rocReallocFn(roc_realloc: *RocRealloc, env: *anyopaque) callconv(.C) void {
        _ = env;

        // Basic realloc implementation using the testing allocator
        const allocator = std.testing.allocator;

        // Calculate where the size metadata is stored for the old allocation
        const size_storage_bytes = @max(roc_realloc.alignment, @alignOf(usize));

        if (@intFromPtr(roc_realloc.answer) == 0) {
            // Initial allocation
            const new_total_size = roc_realloc.new_length + size_storage_bytes;
            const new_slice = allocator.alloc(u8, new_total_size) catch {
                @panic("Test reallocation failed - out of memory");
            };

            // Store the total size in metadata
            const new_size_ptr: *usize = @ptrFromInt(@intFromPtr(new_slice.ptr) + size_storage_bytes - @sizeOf(usize));
            new_size_ptr.* = new_total_size;

            // Return pointer to data (after metadata)
            roc_realloc.answer = @ptrFromInt(@intFromPtr(new_slice.ptr) + size_storage_bytes);
            return;
        }

        // Reallocation of existing memory
        const old_size_ptr: *const usize = @ptrFromInt(@intFromPtr(roc_realloc.answer) - @sizeOf(usize));
        const old_total_size = old_size_ptr.*;
        const old_base_ptr: [*]u8 = @ptrFromInt(@intFromPtr(roc_realloc.answer) - size_storage_bytes);

        const new_total_size = roc_realloc.new_length + size_storage_bytes;

        const old_slice = @as([*]u8, @ptrCast(old_base_ptr))[0..old_total_size];
        const new_slice = allocator.realloc(old_slice, new_total_size) catch {
            @panic("Test reallocation failed - out of memory");
        };

        // Store the new total size in metadata
        const new_size_ptr: *usize = @ptrFromInt(@intFromPtr(new_slice.ptr) + size_storage_bytes - @sizeOf(usize));
        new_size_ptr.* = new_total_size;

        // Return pointer to data (after metadata)
        roc_realloc.answer = @ptrFromInt(@intFromPtr(new_slice.ptr) + size_storage_bytes);
    }

    fn rocDbgFn(roc_dbg: *const RocDbg, env: *anyopaque) callconv(.C) void {
        _ = env;
        const message = roc_dbg.utf8_bytes[0..roc_dbg.len];
        std.debug.print("DBG: {s}\n", .{message});
    }

    fn rocExpectFailedFn(roc_expect: *const RocExpectFailed, env: *anyopaque) callconv(.C) void {
        _ = env;
        const message = @as([*]u8, @ptrCast(roc_expect.utf8_bytes))[0..roc_expect.len];
        std.debug.print("EXPECT FAILED: {s}\n", .{message});
    }

    fn rocCrashedFn(roc_crashed: *const RocCrashed, env: *anyopaque) callconv(.C) noreturn {
        _ = env;
        const message = roc_crashed.utf8_bytes[0..roc_crashed.len];
        @panic(message);
    }
};

/// Returns a struct type that holds a value of type T and a boolean indicating whether overflow occurred
/// Used for arithmetic operations that can detect overflow
pub fn WithOverflow(comptime T: type) type {
    return extern struct { value: T, has_overflowed: bool };
}

/// Function type for incrementing reference count
pub const Inc = fn (?[*]u8) callconv(.C) void;
/// Function type for incrementing reference count by a specific amount
pub const IncN = fn (?[*]u8, u64) callconv(.C) void;
/// Function type for decrementing reference count
pub const Dec = fn (?[*]u8) callconv(.C) void;
/// Special refcount value that marks data with whole-program lifetime.
/// When a refcount equals this value, it indicates static/constant data that should
/// never be decremented or freed. This is used for string literals, constant data,
/// and other values that live for the entire duration of the program.
///
/// The value 0 is chosen because:
/// - It's clearly distinct from normal refcounts (which start at 1)
/// - It makes the "constant" check very efficient
/// - It's safe since normal refcounts should never reach 0 while still being referenced
pub const REFCOUNT_STATIC_DATA: isize = 0;
/// No-op reference count decrement function.
/// Used as a callback when elements don't contain refcounted data or in testing scenarios
/// where reference counting operations should be skipped. Matches the `Dec` function type
/// signature but performs no operations.
///
/// This is commonly passed to `decref` methods when:
/// - Testing with simple data types that don't need reference counting
/// - Working with primitive types that don't contain pointers to refcounted data
/// - As a placeholder when the decrement operation is handled elsewhere
pub fn rcNone(_: ?[*]u8) callconv(.C) void {}

/// Enum representing different integer widths and signedness for runtime type information
pub const IntWidth = enum(u8) {
    U8 = 0,
    U16 = 1,
    U32 = 2,
    U64 = 3,
    U128 = 4,
    I8 = 5,
    I16 = 6,
    I32 = 7,
    I64 = 8,
    I128 = 9,
};

const Refcount = enum {
    none,
    normal,
    atomic,
};

const RC_TYPE: Refcount = .atomic;

/// Increments reference count of an RC pointer by specified amount
pub fn increfRcPtrC(ptr_to_refcount: *isize, amount: isize) callconv(.C) void {
    if (RC_TYPE == .none) return;

    if (DEBUG_INCDEC and builtin.target.cpu.arch != .wasm32) {
        std.debug.print("| increment {*}: ", .{ptr_to_refcount});
    }

    // Ensure that the refcount is not whole program lifetime.
    const refcount: isize = ptr_to_refcount.*;
    if (!rcConstant(refcount)) {
        // Note: we assume that a refcount will never overflow.
        // As such, we do not need to cap incrementing.
        switch (RC_TYPE) {
            .normal => {
                if (DEBUG_INCDEC and builtin.target.cpu.arch != .wasm32) {
                    const old = @as(usize, @bitCast(refcount));
                    const new = old + @as(usize, @intCast(amount));

                    std.debug.print("{} + {} = {}!\n", .{ old, amount, new });
                }

                ptr_to_refcount.* = refcount +% amount;
            },
            .atomic => {
                _ = @atomicRmw(isize, ptr_to_refcount, .Add, amount, .monotonic);
            },
            .none => unreachable,
        }
    }
}

/// TODO
pub fn decrefRcPtrC(
    bytes_or_null: ?[*]isize,
    alignment: u32,
    elements_refcounted: bool,
    roc_ops: *RocOps,
) callconv(.C) void {
    // IMPORTANT: bytes_or_null is this case is expected to be a pointer to the refcount
    // (NOT the start of the data, or the start of the allocation)

    // this is of course unsafe, but we trust what we get from the llvm side
    const bytes = @as([*]isize, @ptrCast(bytes_or_null));

    return @call(
        .always_inline,
        decref_ptr_to_refcount,
        .{ bytes, alignment, elements_refcounted, roc_ops },
    );
}

/// Safely decrements reference count for a potentially null pointer
pub fn decrefCheckNullC(
    bytes_or_null: ?[*]u8,
    alignment: u32,
    elements_refcounted: bool,
    roc_ops: *RocOps,
) callconv(.C) void {
    if (bytes_or_null) |bytes| {
        const isizes: [*]isize = @as([*]isize, @ptrCast(@alignCast(bytes)));
        return @call(
            .always_inline,
            decref_ptr_to_refcount,
            .{ isizes - 1, alignment, elements_refcounted, roc_ops },
        );
    }
}

/// Decrements reference count for a data pointer and frees memory if count reaches zero
/// Handles tag bits in the pointer and extracts the reference count pointer
/// Used for reference-counted data structures
pub fn decrefDataPtrC(
    bytes_or_null: ?[*]u8,
    alignment: u32,
    elements_refcounted: bool,
    roc_ops: *RocOps,
) callconv(.C) void {
    const bytes = bytes_or_null orelse return;

    const data_ptr = @intFromPtr(bytes);
    const tag_mask: usize = if (@sizeOf(usize) == 8) 0b111 else 0b11;
    const unmasked_ptr = data_ptr & ~tag_mask;

    const isizes: [*]isize = @as([*]isize, @ptrFromInt(unmasked_ptr));
    const rc_ptr = isizes - 1;

    return decrefRcPtrC(rc_ptr, alignment, elements_refcounted, roc_ops);
}

/// Increments reference count for a data pointer by specified amount
/// Handles tag bits in the pointer and extracts the reference count pointer
/// Used for reference-counted data structures
pub fn increfDataPtrC(
    bytes_or_null: ?[*]u8,
    inc_amount: isize,
) callconv(.C) void {
    const bytes = bytes_or_null orelse return;

    const ptr = @intFromPtr(bytes);
    const tag_mask: usize = if (@sizeOf(usize) == 8) 0b111 else 0b11;
    const masked_ptr = ptr & ~tag_mask;

    const isizes: *isize = @as(*isize, @ptrFromInt(masked_ptr - @sizeOf(usize)));

    return increfRcPtrC(isizes, inc_amount);
}

/// Frees memory for a data pointer regardless of reference count
/// Handles tag bits in the pointer and extracts the reference count pointer
/// Used for reference-counted data structures
pub fn freeDataPtrC(
    bytes_or_null: ?[*]u8,
    alignment: u32,
    elements_refcounted: bool,
    roc_ops: *RocOps,
) callconv(.C) void {
    const bytes = bytes_or_null orelse return;

    const ptr = @intFromPtr(bytes);
    const tag_mask: usize = if (@sizeOf(usize) == 8) 0b111 else 0b11;
    const masked_ptr = ptr & ~tag_mask;

    const isizes: [*]isize = @as([*]isize, @ptrFromInt(masked_ptr));

    // we always store the refcount right before the data
    return freeRcPtrC(isizes - 1, alignment, elements_refcounted, roc_ops);
}

/// Frees memory for a reference count pointer regardless of current count
/// This bypasses reference counting and immediately frees the memory
/// Used internally for reference-counted allocations
pub fn freeRcPtrC(
    bytes_or_null: ?[*]isize,
    alignment: u32,
    elements_refcounted: bool,
    roc_ops: *RocOps,
) callconv(.C) void {
    const bytes = bytes_or_null orelse return;
    return free_ptr_to_refcount(bytes, alignment, elements_refcounted, roc_ops);
}

/// Decrements reference count and potentially frees memory
pub fn decref(
    bytes_or_null: ?[*]u8,
    data_bytes: usize,
    alignment: u32,
    elements_refcounted: bool,
    roc_ops: *RocOps,
) void {
    if (data_bytes == 0) {
        return;
    }

    const bytes = bytes_or_null orelse return;

    // Check if the pointer is properly aligned for isize
    if (@intFromPtr(bytes) % @alignOf(isize) != 0) {
        // Pointer is misaligned - this can happen with seamless slices
        // For now, skip the decref to avoid a crash
        // TODO: Fix the root cause of misaligned seamless slice pointers
        return;
    }

    const isizes: [*]isize = @as([*]isize, @ptrCast(@alignCast(bytes)));

    decref_ptr_to_refcount(isizes - 1, alignment, elements_refcounted, roc_ops);
}

inline fn free_ptr_to_refcount(
    refcount_ptr: [*]isize,
    alignment: u32,
    elements_refcounted: bool,
    roc_ops: *RocOps,
) void {
    if (RC_TYPE == .none) return;
    const ptr_width = @sizeOf(usize);
    const required_space: usize = if (elements_refcounted) (2 * ptr_width) else ptr_width;
    const extra_bytes = @max(required_space, alignment);
    const allocation_ptr = @as([*]u8, @ptrCast(refcount_ptr)) - (extra_bytes - @sizeOf(usize));

    var roc_dealloc_args = RocDealloc{
        .alignment = alignment,
        .ptr = allocation_ptr,
    };

    // NOTE: we don't even check whether the refcount is "infinity" here!
    roc_ops.roc_dealloc(&roc_dealloc_args, roc_ops.env);

    if (DEBUG_ALLOC and builtin.target.cpu.arch != .wasm32) {
        std.debug.print("💀 freed {*}\n", .{allocation_ptr});
    }
}

inline fn decref_ptr_to_refcount(
    refcount_ptr: [*]isize,
    element_alignment: u32,
    elements_refcounted: bool,
    roc_ops: *RocOps,
) void {
    if (RC_TYPE == .none) return;

    if (DEBUG_INCDEC and builtin.target.cpu.arch != .wasm32) {
        std.debug.print("| decrement {*}: ", .{refcount_ptr});
    }

    // Due to RC alignment tmust take into account pointer size.
    const ptr_width = @sizeOf(usize);
    const alignment = @max(ptr_width, element_alignment);

    // Ensure that the refcount is not whole program lifetime.
    const refcount: isize = refcount_ptr[0];
    if (!rcConstant(refcount)) {
        switch (RC_TYPE) {
            .normal => {
                if (DEBUG_INCDEC and builtin.target.cpu.arch != .wasm32) {
                    const old = @as(usize, @bitCast(refcount));
                    const new = @as(usize, @bitCast(refcount_ptr[0] -% 1));

                    std.debug.print("{} - 1 = {}!\n", .{ old, new });
                }

                refcount_ptr[0] = refcount -% 1;
                if (refcount == 1) {
                    free_ptr_to_refcount(refcount_ptr, alignment, elements_refcounted, roc_ops);
                }
            },
            .atomic => {
                const last = @atomicRmw(isize, &refcount_ptr[0], .Sub, 1, .monotonic);
                if (last == 1) {
                    free_ptr_to_refcount(refcount_ptr, alignment, elements_refcounted, roc_ops);
                }
            },
            .none => unreachable,
        }
    }
}

/// Determines if a data pointer has a unique reference (refcount = 1)
/// Returns true for null pointers (they're conceptually uniquely owned)
/// Handles tag bits in the pointer and extracts the reference count
pub fn isUnique(
    bytes_or_null: ?[*]u8,
) callconv(.C) bool {
    const bytes = bytes_or_null orelse return true;

    const ptr = @intFromPtr(bytes);
    const tag_mask: usize = if (@sizeOf(usize) == 8) 0b111 else 0b11;
    const masked_ptr = ptr & ~tag_mask;

    const isizes: [*]isize = @as([*]isize, @ptrFromInt(masked_ptr));

    const refcount = (isizes - 1)[0];

    if (DEBUG_INCDEC and builtin.target.cpu.arch != .wasm32) {
        std.debug.print("| is unique {*}\n", .{isizes - 1});
    }

    return rcUnique(refcount);
}

/// Checks if a reference count value indicates a unique reference (equals 1)
/// Used to determine if in-place mutation is safe for reference-counted data
pub inline fn rcUnique(refcount: isize) bool {
    switch (RC_TYPE) {
        .normal => {
            return refcount == 1;
        },
        .atomic => {
            return refcount == 1;
        },
        .none => {
            return false;
        },
    }
}

/// Checks if a reference count value indicates a constant (non-decrementable) reference
/// Constant references (REFCOUNT_MAX_ISIZE) are never freed when decremented
pub inline fn rcConstant(refcount: isize) bool {
    switch (RC_TYPE) {
        .normal => {
            return refcount == REFCOUNT_STATIC_DATA;
        },
        .atomic => {
            return refcount == REFCOUNT_STATIC_DATA;
        },
        .none => {
            return true;
        },
    }
}

// We follow roughly the [fbvector](https://github.com/facebook/folly/blob/main/folly/docs/FBVector.md) when it comes to growing a RocList.
// Here is [their growth strategy](https://github.com/facebook/folly/blob/3e0525988fd444201b19b76b390a5927c15cb697/folly/FBVector.h#L1128) for push_back:
//
// (1) initial size
//     Instead of growing to size 1 from empty, fbvector allocates at least
//     64 bytes. You may still use reserve to reserve a lesser amount of
//     memory.
// (2) 1.5x
//     For medium-sized vectors, the growth strategy is 1.5x. See the docs
//     for details.
//     This does not apply to very small or very large fbvectors. This is a
//     heuristic.
//
// In our case, we exposed allocate and reallocate, which will use a smart growth strategy.
// We also expose allocateExact and reallocateExact for case where a specific number of elements is requested.

/// Calculates the new capacity for a growing list, based on the old capacity, requested length, and element width.
///
/// Should only be called when growing a collection.
///
/// `requested_length` should always be greater than old_capacity.
pub inline fn calculateCapacity(
    old_capacity: usize,
    requested_length: usize,
    element_width: usize,
) usize {
    // TODO: Deal with the fact we allocate an extra u64 for refcount.
    // This may lead to allocating page size + 8 bytes.
    // That could mean allocating an entire page for 8 bytes of data which isn't great.

    if (requested_length != old_capacity + 1) {
        // The user is explicitly requesting n elements.
        // Trust the user and just reserve that amount.
        return requested_length;
    }

    var new_capacity: usize = 0;
    if (element_width == 0) {
        return requested_length;
    } else if (old_capacity == 0) {
        new_capacity = 64 / element_width;
    } else if (old_capacity < 4096 / element_width) {
        new_capacity = old_capacity * 2;
    } else if (old_capacity > 4096 * 32 / element_width) {
        new_capacity = old_capacity * 2;
    } else {
        new_capacity = (old_capacity * 3 + 1) / 2;
    }

    return @max(new_capacity, requested_length);
}

/// Allocates memory with space for a reference count, for C compatibility
/// Thin wrapper around allocateWithRefcount that preserves the C calling convention
pub fn allocateWithRefcountC(
    data_bytes: usize,
    element_alignment: u32,
    elements_refcounted: bool,
    roc_ops: *RocOps,
) callconv(.C) [*]u8 {
    return allocateWithRefcount(data_bytes, element_alignment, elements_refcounted, roc_ops);
}

/// Allocates memory with space for a reference count
/// Creates memory layout with refcount stored before the data pointer
/// Returns a pointer to the data portion, not the allocation start
pub fn allocateWithRefcount(
    data_bytes: usize,
    element_alignment: u32,
    elements_refcounted: bool,
    roc_ops: *RocOps,
) [*]u8 {
    // If the element type is refcounted, we need to also allocate space to store the element count on the heap.
    // This is used so that a seamless slice can de-allocate the underlying list type.
    const ptr_width = @sizeOf(usize);
    const alignment = @max(ptr_width, element_alignment);
    const required_space: usize = if (elements_refcounted) (2 * ptr_width) else ptr_width;
    const extra_bytes = @max(required_space, element_alignment);
    const length = extra_bytes + data_bytes;

    var roc_alloc_args = RocAlloc{
        .alignment = alignment,
        .length = length,
        .answer = undefined,
    };

    roc_ops.roc_alloc(&roc_alloc_args, roc_ops.env);

    const new_bytes = @as([*]u8, @ptrCast(roc_alloc_args.answer));

    if (DEBUG_ALLOC and builtin.target.cpu.arch != .wasm32) {
        std.debug.print("+ allocated {*} ({} bytes with alignment {})\n", .{ new_bytes, data_bytes, alignment });
    }

    const data_ptr = new_bytes + extra_bytes;
    const refcount_ptr = @as([*]usize, @ptrCast(@as([*]align(ptr_width) u8, @alignCast(data_ptr)) - ptr_width));
    refcount_ptr[0] = if (RC_TYPE == .none) REFCOUNT_STATIC_DATA else 1;

    return data_ptr;
}

/// A C-compatible slice structure containing a pointer and length
pub const CSlice = extern struct {
    pointer: *anyopaque,
    len: usize,
};

/// Reallocates memory for a list to accommodate growth
/// Preserves existing data and handles refcount placement
/// Returns a pointer to the data portion, not the allocation start
pub fn unsafeReallocate(
    source_ptr: [*]u8,
    alignment: u32,
    old_length: usize,
    new_length: usize,
    element_width: usize,
    elements_refcounted: bool,
) [*]u8 {
    const ptr_width: usize = @sizeOf(usize);
    const required_space: usize = if (elements_refcounted) (2 * ptr_width) else ptr_width;
    const extra_bytes = @max(required_space, alignment);

    const old_width = extra_bytes + old_length * element_width;
    const new_width = extra_bytes + new_length * element_width;

    if (old_width >= new_width) {
        return source_ptr;
    }

    const old_allocation = source_ptr - extra_bytes;

    const roc_realloc_args = RocRealloc{
        .alignment = alignment,
        .new_length = new_width,
        .answer = old_allocation,
    };

    const new_source = @as([*]u8, @ptrCast(roc_realloc_args.answer)) + extra_bytes;
    return new_source;
}

/// Represents comparison results: equal, greater than, or less than
pub const Ordering = enum(u8) {
    EQ = 0,
    GT = 1,
    LT = 2,
};

/// Specifies whether updates should create new data or modify existing data
pub const UpdateMode = enum(u8) {
    Immutable = 0,
    InPlace = 1,
};

/// Generates a pseudo-random seed for dictionary hashing
/// Uses the memory address of this function as the seed value, which:
/// - Changes between program runs on most OSes due to ASLR
/// - Prevents all dictionaries from using a predictable seed (avoiding DoS attacks)
/// - Provides more security than a fixed seed but less than true randomness
/// - Remains constant within a single program run
///
/// Note: On most operating systems, this will be affected by ASLR and different each run.
/// In WebAssembly, the value will be constant for the entire build.
pub fn dictPseudoSeed() callconv(.C) u64 {
    return @as(u64, @intCast(@intFromPtr(&dictPseudoSeed)));
}

test "increfC, refcounted data" {
    var mock_rc: isize = 17;
    const ptr_to_refcount: *isize = &mock_rc;
    @import("utils.zig").increfRcPtrC(ptr_to_refcount, 2);
    try std.testing.expectEqual(mock_rc, 19);
}

test "increfC, static data" {
    var mock_rc: isize = @import("utils.zig").REFCOUNT_STATIC_DATA;
    const ptr_to_refcount: *isize = &mock_rc;
    @import("utils.zig").increfRcPtrC(ptr_to_refcount, 2);
    try std.testing.expectEqual(mock_rc, @import("utils.zig").REFCOUNT_STATIC_DATA);
}

test "decrefC, refcounted data" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    var mock_rc: isize = 17;
    const ptr_to_refcount: *isize = &mock_rc;
    @import("utils.zig").decrefRcPtrC(@ptrCast(ptr_to_refcount), 8, false, test_env.getOps());
    try std.testing.expectEqual(mock_rc, 16);
}

test "decrefC, static data" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    var mock_rc: isize = @import("utils.zig").REFCOUNT_STATIC_DATA;
    const ptr_to_refcount: *isize = &mock_rc;
    @import("utils.zig").decrefRcPtrC(@ptrCast(ptr_to_refcount), 8, false, test_env.getOps());
    try std.testing.expectEqual(mock_rc, @import("utils.zig").REFCOUNT_STATIC_DATA);
}

test "TestEnv basic functionality" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    // Should start with no allocations
    try std.testing.expectEqual(@as(usize, 0), test_env.getAllocationCount());

    // Get ops should work
    const ops = test_env.getOps();
    // Function pointers are non-null by design, just verify we can get ops
    _ = ops;
}

test "TestEnv allocation tracking" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    const ops = test_env.getOps();

    // Test allocation
    var alloc_request = @import("host_abi.zig").RocAlloc{
        .alignment = 8,
        .length = 32,
        .answer = undefined,
    };

    ops.roc_alloc(&alloc_request, ops.env);
    try std.testing.expectEqual(@as(usize, 1), test_env.getAllocationCount());

    // Test deallocation
    var dealloc_request = @import("host_abi.zig").RocDealloc{
        .alignment = 8,
        .ptr = alloc_request.answer,
    };

    ops.roc_dealloc(&dealloc_request, ops.env);
    try std.testing.expectEqual(@as(usize, 0), test_env.getAllocationCount());
}

test "calculateCapacity with various inputs" {
    // Test zero capacity
    try std.testing.expectEqual(@as(usize, 0), calculateCapacity(0, 0, 1));

    // Test basic growth
    try std.testing.expectEqual(@as(usize, 6), calculateCapacity(4, 6, 1));

    // Test with larger element sizes
    try std.testing.expectEqual(@as(usize, 20), calculateCapacity(16, 20, 1));

    // Test that it rounds up appropriately
    try std.testing.expectEqual(@as(usize, 10), calculateCapacity(8, 10, 1));

    // Test growth logic when requesting exactly old_capacity + 1
    try std.testing.expectEqual(@as(usize, 8), calculateCapacity(4, 5, 1));
}

test "allocateWithRefcount basic functionality" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    const ops = test_env.getOps();

    // Allocate memory with refcount
    const ptr = allocateWithRefcount(64, 8, false, ops);
    _ = ptr; // Just verify it doesn't crash

    // Should have tracked the allocation
    try std.testing.expectEqual(@as(usize, 1), test_env.getAllocationCount());
}

test "isUnique with different scenarios" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    const ops = test_env.getOps();

    // Test with null (should return true)
    try std.testing.expect(@import("utils.zig").isUnique(null));

    // Test with allocated memory
    const ptr = allocateWithRefcount(64, 8, false, ops);
    try std.testing.expect(@import("utils.zig").isUnique(ptr));
}

test "rcNone function" {
    // rcNone should be safe to call with any pointer
    @import("utils.zig").rcNone(null);

    var dummy: u8 = 42;
    @import("utils.zig").rcNone(@as(?[*]u8, @ptrCast(&dummy)));

    // If we get here without crashing, the test passed
    try std.testing.expect(true);
}
