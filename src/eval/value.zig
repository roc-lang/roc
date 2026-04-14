//! Concrete runtime value representation for the interpreter.
//!
//! A `Value` is a raw pointer to bytes in memory. It carries no runtime type
//! information — the layout is always tracked separately via `layout.Idx`.
//!
//! This module also provides layout-aware helpers for reading/writing
//! scalars, accessing struct fields, tag union discriminants, and managing
//! refcounted allocations.

const std = @import("std");
const layout_mod = @import("layout");

const Allocator = std.mem.Allocator;

/// A concrete runtime value: a pointer to raw bytes in memory.
///
/// The layout (size, alignment, structure) is tracked externally by the
/// interpreter via `layout.Idx`. Values do not carry runtime type variables.
pub const Value = struct {
    /// Pointer to the first byte of the value.
    /// For ZSTs, this is a sentinel that must never be dereferenced.
    ptr: [*]u8,

    /// Sentinel value for zero-sized types.
    pub const zst: Value = .{ .ptr = @ptrFromInt(0xDEAD_BEEF) };

    /// Create a Value from a typed pointer.
    pub fn fromPtr(ptr: *anyopaque) Value {
        return .{ .ptr = @ptrCast(ptr) };
    }

    /// Create a Value from a byte slice.
    pub fn fromSlice(slice: []u8) Value {
        return .{ .ptr = slice.ptr };
    }

    /// Read a scalar of type T (unaligned-safe).
    pub fn read(self: Value, comptime T: type) T {
        return @as(*align(1) const T, @ptrCast(self.ptr)).*;
    }

    /// Write a scalar of type T (unaligned-safe).
    pub fn write(self: Value, comptime T: type, val: T) void {
        @as(*align(1) T, @ptrCast(self.ptr)).* = val;
    }

    /// Read N bytes starting at the pointer.
    pub fn readBytes(self: Value, len: usize) []const u8 {
        return self.ptr[0..len];
    }

    /// Copy bytes into the value's memory.
    pub fn writeBytes(self: Value, bytes: []const u8) void {
        @memcpy(self.ptr[0..bytes.len], bytes);
    }

    /// Copy `size` bytes from `src` into this value.
    pub fn copyFrom(self: Value, src: Value, size: usize) void {
        if (size > 0) {
            const dest = self.ptr[0..size];
            const source = src.ptr[0..size];
            if (@intFromPtr(dest.ptr) <= @intFromPtr(source.ptr)) {
                var i: usize = 0;
                while (i < size) : (i += 1) {
                    dest[i] = source[i];
                }
            } else {
                var i: usize = size;
                while (i > 0) {
                    i -= 1;
                    dest[i] = source[i];
                }
            }
        }
    }

    /// Return a value offset by `n` bytes.
    pub fn offset(self: Value, n: usize) Value {
        return .{ .ptr = self.ptr + n };
    }

    /// Get a usize-aligned pointer (for RocStr/RocList field access).
    pub fn asOpaquePtr(self: Value) *anyopaque {
        return @ptrCast(self.ptr);
    }

    /// Check if this is the ZST sentinel.
    pub fn isZst(self: Value) bool {
        return @intFromPtr(self.ptr) == 0xDEAD_BEEF;
    }
};

/// Helpers for computing layout sizes, offsets, and field access.
///
/// This wraps a `layout.Store` pointer and provides the queries
/// that the interpreter needs during expression evaluation.
pub const LayoutHelper = struct {
    store: *const layout_mod.Store,

    pub fn init(store: *const layout_mod.Store) LayoutHelper {
        return .{ .store = store };
    }

    /// Size in bytes of a layout.
    pub fn sizeOf(self: LayoutHelper, idx: layout_mod.Idx) u32 {
        const l = self.store.getLayout(idx);
        return self.store.layoutSize(l);
    }

    /// Size and alignment of a layout.
    pub fn sizeAlignOf(self: LayoutHelper, idx: layout_mod.Idx) layout_mod.SizeAlign {
        const l = self.store.getLayout(idx);
        return self.store.layoutSizeAlign(l);
    }

    /// Whether a layout is zero-sized.
    pub fn isZeroSized(self: LayoutHelper, idx: layout_mod.Idx) bool {
        return self.sizeOf(idx) == 0;
    }

    /// Offset of a struct field (by sorted field index).
    pub fn structFieldOffset(self: LayoutHelper, idx: layout_mod.Idx, sorted_field_idx: u32) u32 {
        const l = self.store.getLayout(idx);
        return self.store.getStructFieldOffset(l.data.struct_.idx, sorted_field_idx);
    }

    /// Offset of the discriminant in a tag union.
    pub fn tagDiscriminantOffset(self: LayoutHelper, idx: layout_mod.Idx) u16 {
        const l = self.store.getLayout(idx);
        return self.store.getTagUnionDiscriminantOffset(l.data.tag_union.idx);
    }

    /// Read the discriminant value from a tag union value.
    pub fn readTagDiscriminant(self: LayoutHelper, val: Value, union_layout: layout_mod.Idx) u16 {
        const disc_offset = self.tagDiscriminantOffset(union_layout);
        const at_disc = val.offset(disc_offset);
        const l = self.store.getLayout(union_layout);
        const tu_data = self.store.getTagUnionData(l.data.tag_union.idx);
        return switch (tu_data.discriminant_size) {
            0 => 0, // Single-variant unions have implicit discriminant 0
            1 => at_disc.read(u8),
            2 => at_disc.read(u16),
            else => unreachable,
        };
    }

    /// Write the discriminant value into a tag union value.
    pub fn writeTagDiscriminant(self: LayoutHelper, val: Value, union_layout: layout_mod.Idx, disc: u16) void {
        const disc_offset = self.tagDiscriminantOffset(union_layout);
        const at_disc = val.offset(disc_offset);
        const l = self.store.getLayout(union_layout);
        const tu_data = self.store.getTagUnionData(l.data.tag_union.idx);
        switch (tu_data.discriminant_size) {
            0 => {}, // Single-variant — no discriminant to write
            1 => at_disc.write(u8, @intCast(disc)),
            2 => at_disc.write(u16, disc),
            else => unreachable,
        }
    }

    /// Whether the given layout contains refcounted data.
    pub fn containsRefcounted(self: LayoutHelper, idx: layout_mod.Idx) bool {
        const l = self.store.getLayout(idx);
        return self.store.layoutContainsRefcounted(l);
    }
};

/// Allocate `size` bytes on a general-purpose allocator, returning a Value
/// pointing to the zeroed memory.
pub fn allocValue(allocator: Allocator, size: u32) Allocator.Error!Value {
    if (size == 0) return Value.zst;
    const slice = try allocator.alloc(u8, size);
    @memset(slice, 0);
    return Value.fromSlice(slice);
}

/// Free a value's memory allocated with `allocValue`.
pub fn freeValue(allocator: Allocator, val: Value, size: u32) void {
    if (val.isZst() or size == 0) return;
    allocator.free(val.ptr[0..size]);
}
