//! Core utility functions and types for the Roc runtime builtins.
//!
//! This module provides essential infrastructure for builtin operations,
//! including memory allocation interfaces, overflow detection utilities,
//! debug functions, and common types used throughout the builtin modules.
//!
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

/// Performs a pointer cast with debug-mode alignment verification.
///
/// In debug builds, verifies that the pointer is properly aligned for the target type
/// and panics with detailed diagnostic information if alignment is incorrect.
/// In release builds, this is equivalent to `@ptrCast(@alignCast(ptr))`.
///
/// Usage:
/// ```
/// const typed_ptr: *usize = alignedPtrCast(*usize, raw_ptr, @src());
/// ```
///
/// The `src` parameter should always be `@src()` at the call site - this captures
/// the file, function, and line number to aid in reproducing alignment bugs.
pub inline fn alignedPtrCast(comptime T: type, ptr: anytype, src: std.builtin.SourceLocation) T {
    if (comptime builtin.mode == .Debug) {
        const ptr_info = @typeInfo(T);
        const alignment = switch (ptr_info) {
            .pointer => |p| p.alignment,
            else => @compileError("alignedPtrCast target must be a pointer type"),
        };
        const ptr_int = @intFromPtr(ptr);
        if (alignment > 0 and ptr_int % alignment != 0) {
            // Alignment errors indicate a bug in the caller.
            // We use unreachable here because:
            // 1. We don't have access to roc_ops in this utility function
            // 2. This is a debug-only check (comptime builtin.mode == .Debug)
            // 3. On non-WASM, this will trigger a trap with a stack trace
            // 4. The @src() parameter helps identify the call site in logs
            _ = src; // Used for debugging context
            unreachable;
        }
    }
    return @ptrCast(@alignCast(ptr));
}

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
                .hosted_fns = .{ .count = 0, .fns = undefined }, // No host functions in tests
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
                else => {
                    // Use unreachable since we can't call roc_ops.crash in deinit
                    // This should never happen in properly written tests
                    std.debug.print("Unsupported alignment in test deallocator cleanup: {d}\n", .{entry.value_ptr.alignment});
                    unreachable;
                },
            }
        }

        self.allocation_map.deinit();
    }

    pub fn getAllocationCount(self: *const TestEnv) usize {
        return self.allocation_map.count();
    }

    fn rocAllocFn(roc_alloc: *RocAlloc, env: *anyopaque) callconv(.c) void {
        const self: *TestEnv = @ptrCast(@alignCast(env));

        // Allocate memory using the testing allocator with comptime alignment
        const ptr = switch (roc_alloc.alignment) {
            1 => self.allocator.alignedAlloc(u8, std.mem.Alignment.@"1", roc_alloc.length),
            2 => self.allocator.alignedAlloc(u8, std.mem.Alignment.@"2", roc_alloc.length),
            4 => self.allocator.alignedAlloc(u8, std.mem.Alignment.@"4", roc_alloc.length),
            8 => self.allocator.alignedAlloc(u8, std.mem.Alignment.@"8", roc_alloc.length),
            16 => self.allocator.alignedAlloc(u8, std.mem.Alignment.@"16", roc_alloc.length),
            else => {
                // Use unreachable since we can't call roc_ops.crash in test allocator
                std.debug.print("Unsupported alignment in test allocator: {d}\n", .{roc_alloc.alignment});
                unreachable;
            },
        } catch {
            std.debug.print("Test allocation failed\n", .{});
            unreachable;
        };

        // Cast the pointer to *anyopaque
        const result: *anyopaque = @ptrCast(ptr.ptr);

        // Save the allocation in the map
        self.allocation_map.put(result, AllocationInfo{
            .size = roc_alloc.length,
            .alignment = roc_alloc.alignment,
        }) catch {
            self.allocator.free(ptr);
            std.debug.print("Failed to track test allocation\n", .{});
            unreachable;
        };

        roc_alloc.answer = result;
    }

    fn rocDeallocFn(roc_dealloc: *RocDealloc, env: *anyopaque) callconv(.c) void {
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
                else => {
                    std.debug.print("Unsupported alignment in test deallocator: {d}\n", .{entry.value.alignment});
                    unreachable;
                },
            }
        }
    }

    fn rocReallocFn(roc_realloc: *RocRealloc, env: *anyopaque) callconv(.c) void {
        const self: *TestEnv = @ptrCast(@alignCast(env));

        // Look up the old allocation
        if (self.allocation_map.fetchRemove(roc_realloc.answer)) |entry| {
            const old_bytes: [*]u8 = @ptrCast(@alignCast(roc_realloc.answer));
            const old_slice = old_bytes[0..entry.value.size];

            // Reallocate with the same alignment
            const new_ptr = switch (entry.value.alignment) {
                1 => self.allocator.realloc(old_slice, roc_realloc.new_length),
                2 => self.allocator.realloc(@as([]align(2) u8, @alignCast(old_slice)), roc_realloc.new_length),
                4 => self.allocator.realloc(@as([]align(4) u8, @alignCast(old_slice)), roc_realloc.new_length),
                8 => self.allocator.realloc(@as([]align(8) u8, @alignCast(old_slice)), roc_realloc.new_length),
                16 => self.allocator.realloc(@as([]align(16) u8, @alignCast(old_slice)), roc_realloc.new_length),
                else => {
                    std.debug.print("Unsupported alignment in test reallocator: {d}\n", .{entry.value.alignment});
                    unreachable;
                },
            } catch {
                std.debug.print("Test reallocation failed\n", .{});
                unreachable;
            };

            const result: *anyopaque = @ptrCast(new_ptr.ptr);

            // Update the allocation map with the new pointer and size
            self.allocation_map.put(result, AllocationInfo{
                .size = roc_realloc.new_length,
                .alignment = entry.value.alignment,
            }) catch {
                self.allocator.free(new_ptr);
                std.debug.print("Failed to track test reallocation\n", .{});
                unreachable;
            };

            roc_realloc.answer = result;
        } else {
            std.debug.print("Test realloc: pointer not found in allocation map\n", .{});
            unreachable;
        }
    }

    fn rocDbgFn(_: *const RocDbg, _: *anyopaque) callconv(.c) void {}

    fn rocExpectFailedFn(_: *const RocExpectFailed, _: *anyopaque) callconv(.c) void {}

    fn rocCrashedFn(roc_crashed: *const RocCrashed, _: *anyopaque) callconv(.c) noreturn {
        const message = roc_crashed.utf8_bytes[0..roc_crashed.len];
        std.debug.print("Roc crashed: {s}\n", .{message});
        unreachable;
    }
};

/// Returns a struct type that holds a value of type T and a boolean indicating whether overflow occurred
/// Used for arithmetic operations that can detect overflow
pub fn WithOverflow(comptime T: type) type {
    return extern struct { value: T, has_overflowed: bool };
}

/// Function type for incrementing reference count
pub const Inc = fn (?[*]u8) callconv(.c) void;
/// Function type for incrementing reference count by a specific amount
pub const IncN = fn (?[*]u8, u64) callconv(.c) void;
/// Function type for decrementing reference count
pub const Dec = fn (?[*]u8) callconv(.c) void;
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

/// Sentinel value written to freed refcount slots to detect use-after-free.
/// When memory is freed in debug mode, the refcount slot is poisoned with this value.
/// Any subsequent attempt to incref/decref this memory will trigger a panic.
/// This value is only used in debug builds and has zero overhead in release.
/// Uses a recognizable pattern that works on both 32-bit and 64-bit platforms.
const POISON_VALUE: isize = @bitCast(if (@sizeOf(usize) == 8)
    @as(usize, 0xDEADBEEFDEADBEEF)
else
    @as(usize, 0xDEADBEEF));
/// No-op reference count decrement function.
/// Used as a callback when elements don't contain refcounted data or in testing scenarios
/// where reference counting operations should be skipped. Matches the `Dec` function type
/// signature but performs no operations.
///
/// This is commonly passed to `decref` methods when:
/// - Testing with simple data types that don't need reference counting
/// - Working with primitive types that don't contain pointers to refcounted data
/// - As a placeholder when the decrement operation is handled elsewhere
pub fn rcNone(_: ?*anyopaque, _: ?[*]u8) callconv(.c) void {}

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
pub fn increfRcPtrC(ptr_to_refcount: *isize, amount: isize, roc_ops: *RocOps) callconv(.c) void {
    if (RC_TYPE == .none) return;

    // Ensure that the refcount is not whole program lifetime.
    const refcount: isize = ptr_to_refcount.*;

    // Debug-only assertions to catch refcount bugs early.
    if (builtin.mode == .Debug) {
        if (refcount == POISON_VALUE) {
            roc_ops.crash("Use-after-free: incref on already-freed memory");
            return;
        }
        if (refcount <= 0 and !rcConstant(refcount)) {
            roc_ops.crash("Invalid incref: incrementing non-positive refcount");
            return;
        }
    }

    if (!rcConstant(refcount)) {
        // Note: we assume that a refcount will never overflow.
        // As such, we do not need to cap incrementing.
        switch (RC_TYPE) {
            .normal => {
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
) callconv(.c) void {
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
/// WARNING: This function assumes `bytes` points to 8-byte aligned data.
/// It should NOT be used for seamless slices with non-zero start offsets,
/// as those have misaligned bytes pointers. Use RocList.decref instead.
pub fn decrefCheckNullC(
    bytes_or_null: ?[*]u8,
    alignment: u32,
    elements_refcounted: bool,
    roc_ops: *RocOps,
) callconv(.c) void {
    if (bytes_or_null) |bytes| {
        const isizes: [*]isize = alignedPtrCast([*]isize, bytes, @src());
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
) callconv(.c) void {
    const bytes = bytes_or_null orelse return;

    const data_ptr = @intFromPtr(bytes);

    // Verify original pointer is properly aligned
    // Use roc_ops.crash() instead of std.debug.panic for WASM compatibility
    if (comptime builtin.mode == .Debug) {
        if (data_ptr % @alignOf(usize) != 0) {
            var buf: [128]u8 = undefined;
            const msg = std.fmt.bufPrint(&buf, "decrefDataPtrC: data_ptr=0x{x} not {d}-byte aligned", .{ data_ptr, @alignOf(usize) }) catch "decrefDataPtrC: alignment error";
            roc_ops.crash(msg);
            return;
        }
    }

    const tag_mask: usize = if (@sizeOf(usize) == 8) 0b111 else 0b11;
    const unmasked_ptr = data_ptr & ~tag_mask;

    // Verify alignment before @ptrFromInt
    if (comptime builtin.mode == .Debug) {
        if (unmasked_ptr % @alignOf(isize) != 0) {
            var buf: [128]u8 = undefined;
            const msg = std.fmt.bufPrint(&buf, "decrefDataPtrC: unmasked=0x{x} (data=0x{x}) not {d}-byte aligned", .{ unmasked_ptr, data_ptr, @alignOf(isize) }) catch "decrefDataPtrC: unmasked alignment error";
            roc_ops.crash(msg);
            return;
        }
    }

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
    roc_ops: *RocOps,
) callconv(.c) void {
    const bytes = bytes_or_null orelse return;

    const ptr = @intFromPtr(bytes);

    // Strip tag bits from the pointer - recursive tag unions may store tag IDs in low bits
    const tag_mask: usize = if (@sizeOf(usize) == 8) 0b111 else 0b11;
    const masked_ptr = ptr & ~tag_mask;
    const rc_addr = masked_ptr - @sizeOf(usize);

    // Verify alignment before @ptrFromInt
    if (comptime builtin.mode == .Debug) {
        if (rc_addr % @alignOf(isize) != 0) {
            var buf: [128]u8 = undefined;
            const msg = std.fmt.bufPrint(&buf, "increfDataPtrC: rc_addr=0x{x} (ptr=0x{x}, masked=0x{x}) is not {d}-byte aligned", .{ rc_addr, ptr, masked_ptr, @alignOf(isize) }) catch "increfDataPtrC: rc_addr alignment error";
            roc_ops.crash(msg);
            return;
        }
    }

    const isizes: *isize = @as(*isize, @ptrFromInt(rc_addr));

    return increfRcPtrC(isizes, inc_amount, roc_ops);
}

/// Frees memory for a data pointer regardless of reference count
/// Handles tag bits in the pointer and extracts the reference count pointer
/// Used for reference-counted data structures
pub fn freeDataPtrC(
    bytes_or_null: ?[*]u8,
    alignment: u32,
    elements_refcounted: bool,
    roc_ops: *RocOps,
) callconv(.c) void {
    const bytes = bytes_or_null orelse return;

    const ptr = @intFromPtr(bytes);
    const tag_mask: usize = if (@sizeOf(usize) == 8) 0b111 else 0b11;
    const masked_ptr = ptr & ~tag_mask;

    // Verify alignment before @ptrFromInt
    if (comptime builtin.mode == .Debug) {
        if (masked_ptr % @alignOf(isize) != 0) {
            var buf: [128]u8 = undefined;
            const msg = std.fmt.bufPrint(&buf, "freeDataPtrC: masked_ptr=0x{x} (ptr=0x{x}) is not {d}-byte aligned", .{ masked_ptr, ptr, @alignOf(isize) }) catch "freeDataPtrC: alignment error";
            roc_ops.crash(msg);
            return;
        }
    }

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
) callconv(.c) void {
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

    const isizes: [*]isize = alignedPtrCast([*]isize, bytes, @src());

    decref_ptr_to_refcount(isizes - 1, alignment, elements_refcounted, roc_ops);
}

inline fn free_ptr_to_refcount(
    refcount_ptr: [*]isize,
    element_alignment: u32,
    elements_refcounted: bool,
    roc_ops: *RocOps,
) void {
    if (RC_TYPE == .none) return;

    // Debug-only: Poison the refcount slot before freeing to detect use-after-free.
    // Any subsequent access to this refcount will see POISON_VALUE and panic.
    if (builtin.mode == .Debug) {
        refcount_ptr[0] = POISON_VALUE;
    }

    const ptr_width = @sizeOf(usize);
    const required_space: usize = if (elements_refcounted) (2 * ptr_width) else ptr_width;
    const extra_bytes = @max(required_space, element_alignment);
    const allocation_ptr = @as([*]u8, @ptrCast(refcount_ptr)) - (extra_bytes - @sizeOf(usize));

    // Use the same alignment calculation as allocateWithRefcount
    const allocation_alignment = @max(ptr_width, element_alignment);

    var roc_dealloc_args = RocDealloc{
        .alignment = allocation_alignment,
        .ptr = allocation_ptr,
    };

    // NOTE: we don't even check whether the refcount is "infinity" here!
    roc_ops.roc_dealloc(&roc_dealloc_args, roc_ops.env);
}

inline fn decref_ptr_to_refcount(
    refcount_ptr: [*]isize,
    element_alignment: u32,
    elements_refcounted: bool,
    roc_ops: *RocOps,
) void {
    if (RC_TYPE == .none) return;

    // Due to RC alignment tmust take into account pointer size.
    const ptr_width = @sizeOf(usize);
    const alignment = @max(ptr_width, element_alignment);

    // Ensure that the refcount is not whole program lifetime.
    const refcount: isize = refcount_ptr[0];

    // Debug-only assertions to catch refcount bugs early.
    // Use roc_ops.crash() instead of @panic for WASM compatibility.
    if (builtin.mode == .Debug) {
        if (refcount == POISON_VALUE) {
            roc_ops.crash("Use-after-free: decref on already-freed memory");
            return;
        }
        if (refcount <= 0 and !rcConstant(refcount)) {
            roc_ops.crash("Refcount underflow: decrementing non-positive refcount");
            return;
        }
    }

    if (!rcConstant(refcount)) {
        switch (RC_TYPE) {
            .normal => {
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
    roc_ops: *RocOps,
) callconv(.c) bool {
    const bytes = bytes_or_null orelse return true;

    const ptr = @intFromPtr(bytes);
    const tag_mask: usize = if (@sizeOf(usize) == 8) 0b111 else 0b11;
    const masked_ptr = ptr & ~tag_mask;

    // Verify alignment before @ptrFromInt
    if (comptime builtin.mode == .Debug) {
        if (masked_ptr % @alignOf(isize) != 0) {
            var buf: [128]u8 = undefined;
            const msg = std.fmt.bufPrint(&buf, "isUnique: masked_ptr=0x{x} (ptr=0x{x}) is not {d}-byte aligned", .{ masked_ptr, ptr, @alignOf(isize) }) catch "isUnique: alignment error";
            roc_ops.crash(msg);
            return false;
        }
    }

    const isizes: [*]isize = @as([*]isize, @ptrFromInt(masked_ptr));

    const refcount = (isizes - 1)[0];

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

/// Debug-only assertion that a data pointer has a valid refcount.
/// Panics if the refcount is poisoned (use-after-free) or invalid (underflow).
/// Compiles to nothing in release builds - zero overhead.
///
/// Use this at key points in slice-creating or refcount-manipulating functions
/// to catch bugs early during development.
pub inline fn assertValidRefcount(data_ptr: ?[*]u8, roc_ops: *RocOps) void {
    if (builtin.mode != .Debug) return;
    if (data_ptr) |ptr| {
        const rc_ptr: [*]isize = alignedPtrCast([*]isize, ptr - @sizeOf(usize), @src());
        const rc = rc_ptr[0];
        if (rc == POISON_VALUE) {
            roc_ops.crash("assertValidRefcount: Use-after-free detected");
            return;
        }
        if (rc <= 0 and !rcConstant(rc)) {
            roc_ops.crash("assertValidRefcount: Invalid refcount (underflow or corruption)");
            return;
        }
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
) callconv(.c) [*]u8 {
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

    const data_ptr = new_bytes + extra_bytes;

    const refcount_ptr: [*]usize = alignedPtrCast([*]usize, data_ptr - @sizeOf(usize), @src());
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
    element_alignment: u32,
    old_length: usize,
    new_length: usize,
    element_width: usize,
    elements_refcounted: bool,
    roc_ops: *RocOps,
) [*]u8 {
    const ptr_width: usize = @sizeOf(usize);
    const required_space: usize = if (elements_refcounted) (2 * ptr_width) else ptr_width;
    const extra_bytes = @max(required_space, element_alignment);

    const old_width = extra_bytes + old_length * element_width;
    const new_width = extra_bytes + new_length * element_width;

    if (old_width >= new_width) {
        return source_ptr;
    }

    const old_allocation = source_ptr - extra_bytes;

    // Use the same alignment calculation as allocateWithRefcount
    const allocation_alignment = @max(ptr_width, element_alignment);

    var roc_realloc_args = RocRealloc{
        .alignment = allocation_alignment,
        .new_length = new_width,
        .answer = old_allocation,
    };

    roc_ops.roc_realloc(&roc_realloc_args, roc_ops.env);

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
pub fn dictPseudoSeed() callconv(.c) u64 {
    return @as(u64, @intCast(@intFromPtr(&dictPseudoSeed)));
}

test "increfC, refcounted data" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    var mock_rc: isize = 17;
    const ptr_to_refcount: *isize = &mock_rc;
    @import("utils.zig").increfRcPtrC(ptr_to_refcount, 2, test_env.getOps());
    try std.testing.expectEqual(mock_rc, 19);
}

test "increfC, static data" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    var mock_rc: isize = @import("utils.zig").REFCOUNT_STATIC_DATA;
    const ptr_to_refcount: *isize = &mock_rc;
    @import("utils.zig").increfRcPtrC(ptr_to_refcount, 2, test_env.getOps());
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

    // Get ops should work - verify we can get ops and it points back to our test env
    const ops = test_env.getOps();
    try std.testing.expectEqual(@as(*anyopaque, @ptrCast(&test_env)), ops.env);
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
    try std.testing.expect(@import("utils.zig").isUnique(null, ops));

    // Test with allocated memory
    const ptr = allocateWithRefcount(64, 8, false, ops);
    try std.testing.expect(@import("utils.zig").isUnique(ptr, ops));
}

test "rcNone function" {
    // rcNone should be safe to call with any pointer
    @import("utils.zig").rcNone(null, null);

    var dummy: u8 = 42;
    @import("utils.zig").rcNone(null, @as(?[*]u8, @ptrCast(&dummy)));

    // If we get here without crashing, the test passed
    try std.testing.expect(true);
}
