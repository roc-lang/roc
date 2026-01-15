//! Represents a "value" on the Interpreter's stack.
//!
//! This is the public facing interface for interacting with stack values.
//!
//! It provides methods for working with the value safely using the layout.

const std = @import("std");
const builtin = @import("builtin");
const build_options = @import("build_options");
const base = @import("base");
const types = @import("types");
const builtins = @import("builtins");
const layout_mod = @import("layout");

// Compile-time flag for refcount tracing - enabled via `zig build -Dtrace-refcount=true`
const trace_refcount = if (@hasDecl(build_options, "trace_refcount")) build_options.trace_refcount else false;

const Ident = base.Ident;
const LayoutStore = layout_mod.Store;
const Layout = layout_mod.Layout;
const RocOps = builtins.host_abi.RocOps;
const RocList = builtins.list.RocList;
const RocStr = builtins.str.RocStr;
const RocDec = builtins.dec.RocDec;

const Closure = layout_mod.Closure;

const StackValue = @This();

/// Read an aligned integer from memory.
inline fn readAligned(comptime T: type, raw_ptr: [*]u8) T {
    return builtins.utils.alignedPtrCast(*const T, raw_ptr, @src()).*;
}

/// Write an i128 value to memory with alignment handling and overflow checking.
inline fn writeChecked(comptime T: type, raw_ptr: [*]u8, value: i128) error{IntegerOverflow}!void {
    const ptr = builtins.utils.alignedPtrCast(*T, raw_ptr, @src());
    ptr.* = std.math.cast(T, value) orelse return error.IntegerOverflow;
}

// Internal helper functions for memory operations that don't need rt_var

/// Read the discriminant for a tag union, handling single-tag unions which don't store one.
fn readTagUnionDiscriminant(layout: Layout, base_ptr: [*]const u8, layout_cache: *LayoutStore) usize {
    std.debug.assert(layout.tag == .tag_union);
    const tu_idx = layout.data.tag_union.idx;
    const tu_data = layout_cache.getTagUnionData(tu_idx);
    const variants = layout_cache.getTagUnionVariants(tu_data);
    // Single-tag unions don't have discriminants, so don't try to read one.
    if (variants.len == 1) return 0;
    const disc_offset = layout_cache.getTagUnionDiscriminantOffset(tu_idx);
    const discriminant = tu_data.readDiscriminantFromPtr(base_ptr + disc_offset);
    std.debug.assert(discriminant < variants.len);
    return discriminant;
}

/// Increment reference count for a value given its layout and pointer.
/// Used internally when we don't need full StackValue type information.
fn increfLayoutPtr(layout: Layout, ptr: ?*anyopaque, layout_cache: *LayoutStore, roc_ops: *RocOps) void {
    if (layout.tag == .scalar and layout.data.scalar.tag == .str) {
        const raw_ptr = ptr orelse return;
        const roc_str: *const RocStr = builtins.utils.alignedPtrCast(*const RocStr, @as([*]u8, @ptrCast(raw_ptr)), @src());
        roc_str.incref(1, roc_ops);
        return;
    }
    if (layout.tag == .list) {
        const raw_ptr = ptr orelse return;
        const list_value: *const RocList = builtins.utils.alignedPtrCast(*const RocList, @as([*]u8, @ptrCast(raw_ptr)), @src());
        list_value.incref(1, false, roc_ops);
        return;
    }
    if (layout.tag == .box) {
        const raw_ptr = ptr orelse return;
        const slot: *usize = builtins.utils.alignedPtrCast(*usize, @as([*]u8, @ptrCast(raw_ptr)), @src());
        if (slot.* != 0) {
            const data_ptr: [*]u8 = @as([*]u8, @ptrFromInt(slot.*));
            builtins.utils.increfDataPtrC(@as(?[*]u8, data_ptr), 1, roc_ops);
        }
        return;
    }
    if (layout.tag == .record) {
        if (ptr == null) return;
        const record_data = layout_cache.getRecordData(layout.data.record.idx);
        if (record_data.fields.count == 0) return;

        const field_layouts = layout_cache.record_fields.sliceRange(record_data.getFields());
        const base_ptr = @as([*]u8, @ptrCast(ptr.?));

        var field_index: usize = 0;
        while (field_index < field_layouts.len) : (field_index += 1) {
            const field_info = field_layouts.get(field_index);
            const field_layout = layout_cache.getLayout(field_info.layout);
            const field_offset = layout_cache.getRecordFieldOffset(layout.data.record.idx, @intCast(field_index));
            const field_ptr = @as(*anyopaque, @ptrCast(base_ptr + field_offset));
            increfLayoutPtr(field_layout, field_ptr, layout_cache, roc_ops);
        }
        return;
    }
    if (layout.tag == .tuple) {
        if (ptr == null) return;
        const tuple_data = layout_cache.getTupleData(layout.data.tuple.idx);
        if (tuple_data.fields.count == 0) return;

        const element_layouts = layout_cache.tuple_fields.sliceRange(tuple_data.getFields());
        const base_ptr = @as([*]u8, @ptrCast(ptr.?));

        var elem_index: usize = 0;
        while (elem_index < element_layouts.len) : (elem_index += 1) {
            const elem_info = element_layouts.get(elem_index);
            const elem_layout = layout_cache.getLayout(elem_info.layout);
            const elem_offset = layout_cache.getTupleElementOffset(layout.data.tuple.idx, @intCast(elem_index));
            const elem_ptr = @as(*anyopaque, @ptrCast(base_ptr + elem_offset));
            increfLayoutPtr(elem_layout, elem_ptr, layout_cache, roc_ops);
        }
        return;
    }
    if (layout.tag == .tag_union) {
        if (ptr == null) return;
        const base_ptr = @as([*]const u8, @ptrCast(ptr.?));
        const discriminant = readTagUnionDiscriminant(layout, base_ptr, layout_cache);
        const tu_data = layout_cache.getTagUnionData(layout.data.tag_union.idx);
        const variants = layout_cache.getTagUnionVariants(tu_data);
        const variant_layout = layout_cache.getLayout(variants.get(discriminant).payload_layout);
        increfLayoutPtr(variant_layout, @as(*anyopaque, @ptrCast(@constCast(base_ptr))), layout_cache, roc_ops);
        return;
    }
    // Other layout types (scalar ints/floats, zst, etc.) don't need refcounting
}

/// Decrement reference count for a value given its layout and pointer.
/// Used internally when we don't need full StackValue type information.
fn decrefLayoutPtr(layout: Layout, ptr: ?*anyopaque, layout_cache: *LayoutStore, ops: *RocOps) void {
    if (layout.tag == .scalar and layout.data.scalar.tag == .str) {
        const raw_ptr = ptr orelse return;
        const roc_str: *const RocStr = builtins.utils.alignedPtrCast(*const RocStr, @as([*]u8, @ptrCast(raw_ptr)), @src());
        roc_str.decref(ops);
        return;
    }
    if (layout.tag == .list) {
        const raw_ptr = ptr orelse return;
        const list_header: *const RocList = builtins.utils.alignedPtrCast(*const RocList, @as([*]u8, @ptrCast(raw_ptr)), @src());
        const list_value = list_header.*;
        const elem_layout = layout_cache.getLayout(layout.data.list);
        const alignment_u32: u32 = @intCast(elem_layout.alignment(layout_cache.targetUsize()).toByteUnits());
        const element_width: usize = @intCast(layout_cache.layoutSize(elem_layout));
        const elements_refcounted = layout_cache.layoutContainsRefcounted(elem_layout);

        // Decref elements when unique
        if (list_value.isUnique(ops)) {
            if (list_value.getAllocationDataPtr(ops)) |source| {
                const count = list_value.getAllocationElementCount(elements_refcounted, ops);
                var idx: usize = 0;
                while (idx < count) : (idx += 1) {
                    const elem_ptr = source + idx * element_width;
                    decrefLayoutPtr(elem_layout, @ptrCast(elem_ptr), layout_cache, ops);
                }
            }
        }
        list_value.decref(alignment_u32, element_width, elements_refcounted, null, &builtins.list.rcNone, ops);
        return;
    }
    if (layout.tag == .box) {
        const box_raw_ptr = ptr orelse return;
        const slot: *usize = builtins.utils.alignedPtrCast(*usize, @as([*]u8, @ptrCast(box_raw_ptr)), @src());
        const raw_ptr = slot.*;
        if (raw_ptr == 0) return;
        const data_ptr = @as([*]u8, @ptrFromInt(raw_ptr));
        const target_usize = layout_cache.targetUsize();
        const elem_layout = layout_cache.getLayout(layout.data.box);
        const elem_alignment: u32 = @intCast(elem_layout.alignment(target_usize).toByteUnits());

        const ptr_int = @intFromPtr(data_ptr);
        const tag_mask: usize = if (@sizeOf(usize) == 8) 0b111 else 0b11;
        const unmasked_ptr = ptr_int & ~tag_mask;
        const refcount_addr = unmasked_ptr - @sizeOf(isize);

        // Refcount address must be aligned - use roc_ops.crash() for WASM compatibility
        if (comptime builtin.mode == .Debug) {
            if (refcount_addr % @alignOf(isize) != 0) {
                var buf: [128]u8 = undefined;
                const msg = std.fmt.bufPrint(&buf, "decrefLayoutPtr: refcount_addr=0x{x} misaligned", .{refcount_addr}) catch "decrefLayoutPtr: refcount misaligned";
                ops.crash(msg);
                return;
            }
        }

        const payload_ptr = @as([*]u8, @ptrFromInt(unmasked_ptr));
        const refcount_ptr: *isize = @as(*isize, @ptrFromInt(refcount_addr));

        if (builtins.utils.rcUnique(refcount_ptr.*)) {
            if (layout_cache.layoutContainsRefcounted(elem_layout)) {
                decrefLayoutPtr(elem_layout, @ptrCast(payload_ptr), layout_cache, ops);
            }
        }
        builtins.utils.decrefDataPtrC(@as(?[*]u8, payload_ptr), elem_alignment, false, ops);
        slot.* = 0;
        return;
    }
    if (layout.tag == .record) {
        if (ptr == null) return;
        const record_data = layout_cache.getRecordData(layout.data.record.idx);
        if (record_data.fields.count == 0) return;

        const field_layouts = layout_cache.record_fields.sliceRange(record_data.getFields());
        const base_ptr = @as([*]u8, @ptrCast(ptr.?));

        var field_index: usize = 0;
        while (field_index < field_layouts.len) : (field_index += 1) {
            const field_info = field_layouts.get(field_index);
            const field_layout = layout_cache.getLayout(field_info.layout);
            const field_offset = layout_cache.getRecordFieldOffset(layout.data.record.idx, @intCast(field_index));
            const field_ptr = @as(*anyopaque, @ptrCast(base_ptr + field_offset));
            decrefLayoutPtr(field_layout, field_ptr, layout_cache, ops);
        }
        return;
    }
    if (layout.tag == .tuple) {
        if (ptr == null) return;
        const tuple_data = layout_cache.getTupleData(layout.data.tuple.idx);
        if (tuple_data.fields.count == 0) return;

        const element_layouts = layout_cache.tuple_fields.sliceRange(tuple_data.getFields());
        const base_ptr = @as([*]u8, @ptrCast(ptr.?));

        var elem_index: usize = 0;
        while (elem_index < element_layouts.len) : (elem_index += 1) {
            const elem_info = element_layouts.get(elem_index);
            const elem_layout = layout_cache.getLayout(elem_info.layout);
            const elem_offset = layout_cache.getTupleElementOffset(layout.data.tuple.idx, @intCast(elem_index));
            const elem_ptr = @as(*anyopaque, @ptrCast(base_ptr + elem_offset));
            decrefLayoutPtr(elem_layout, elem_ptr, layout_cache, ops);
        }
        return;
    }
    if (layout.tag == .closure) {
        const closure_raw_ptr = ptr orelse return;
        const closure_ptr_val = @intFromPtr(closure_raw_ptr);

        // Use the captures_layout_idx from the passed-in layout, NOT from the raw memory header.
        // The layout parameter is authoritative and was set when the closure was created.
        // Reading from raw memory could give stale/incorrect values.
        const captures_layout_idx = layout.data.closure.captures_layout_idx;
        const idx_as_usize = @intFromEnum(captures_layout_idx);
        if (comptime trace_refcount) {
            traceRefcount("DECREF closure detail: ptr=0x{x} captures_layout_idx={}", .{
                closure_ptr_val,
                idx_as_usize,
            });
        }

        // Debug assertion: closure layout index must be within bounds.
        // If this trips, it indicates a compiler bug in layout index assignment.
        std.debug.assert(idx_as_usize < layout_cache.layouts.len());

        const captures_layout = layout_cache.getLayout(captures_layout_idx);

        if (comptime trace_refcount) {
            traceRefcount("DECREF closure captures_layout.tag={}", .{@intFromEnum(captures_layout.tag)});
        }

        // Only decref if there are actual captures (record with fields)
        if (captures_layout.tag == .record) {
            const record_data = layout_cache.getRecordData(captures_layout.data.record.idx);
            if (comptime trace_refcount) {
                traceRefcount("DECREF closure record fields={}", .{record_data.fields.count});
            }
            if (record_data.fields.count > 0) {
                const header_size = @sizeOf(layout_mod.Closure);
                const cap_align = captures_layout.alignment(layout_cache.targetUsize());
                const aligned_off = std.mem.alignForward(usize, header_size, @intCast(cap_align.toByteUnits()));
                const base_ptr: [*]u8 = @ptrCast(closure_raw_ptr);
                const rec_ptr: *anyopaque = @ptrCast(base_ptr + aligned_off);
                if (comptime trace_refcount) {
                    traceRefcount("DECREF closure rec_ptr=0x{x}", .{@intFromPtr(rec_ptr)});
                }
                decrefLayoutPtr(captures_layout, rec_ptr, layout_cache, ops);
            }
        }
        return;
    }
    if (layout.tag == .tag_union) {
        if (ptr == null) return;
        const base_ptr = @as([*]const u8, @ptrCast(ptr.?));
        const discriminant = readTagUnionDiscriminant(layout, base_ptr, layout_cache);
        const tu_data = layout_cache.getTagUnionData(layout.data.tag_union.idx);
        const variants = layout_cache.getTagUnionVariants(tu_data);
        const variant_layout = layout_cache.getLayout(variants.get(discriminant).payload_layout);
        decrefLayoutPtr(variant_layout, @as(*anyopaque, @ptrCast(@constCast(base_ptr))), layout_cache, ops);
        return;
    }
    // Other layout types (scalar ints/floats, zst, etc.) don't need refcounting
}

/// Type and memory layout information for the result value
layout: Layout,
/// Ptr to the actual value in stack memory
ptr: ?*anyopaque,
/// Flag to track whether the memory has been initialized
is_initialized: bool = false,
/// Runtime type variable for type information (used for method dispatch and constant folding)
rt_var: types.Var,

/// Copy this stack value to a destination pointer with bounds checking
pub fn copyToPtr(self: StackValue, layout_cache: *LayoutStore, dest_ptr: *anyopaque, roc_ops: *RocOps) !void {
    std.debug.assert(self.is_initialized); // Source must be initialized before copying

    // For closures, use getTotalSize to include capture data; for others use layoutSize
    const result_size = if (self.layout.tag == .closure) self.getTotalSize(layout_cache, roc_ops) else layout_cache.layoutSize(self.layout);
    if (result_size == 0) {
        // Zero-sized types can have null pointers, which is valid
        return;
    }

    if (self.ptr == null) {
        return error.NullStackPointer;
    }

    if (self.layout.tag == .scalar) {
        switch (self.layout.data.scalar.tag) {
            .str => {
                // Copy the RocStr struct and incref the underlying data.
                // This is more efficient than clone() which allocates new memory.
                std.debug.assert(self.ptr != null);
                const src_str: *const RocStr = builtins.utils.alignedPtrCast(*const RocStr, @as([*]u8, @ptrCast(self.ptr.?)), @src());
                const dest_str: *RocStr = builtins.utils.alignedPtrCast(*RocStr, @as([*]u8, @ptrCast(dest_ptr)), @src());
                dest_str.* = src_str.*;
                if (comptime trace_refcount) {
                    if (!src_str.isSmallStr()) {
                        const alloc_ptr = src_str.getAllocationPtr();
                        const rc_before: isize = if (alloc_ptr) |ptr| blk: {
                            const isizes: [*]isize = builtins.utils.alignedPtrCast([*]isize, ptr, @src());
                            break :blk (isizes - 1)[0];
                        } else 0;
                        traceRefcount("INCREF str (copyToPtr) ptr=0x{x} len={} rc={} slice={}", .{
                            @intFromPtr(alloc_ptr),
                            src_str.len(),
                            rc_before,
                            @intFromBool(src_str.isSeamlessSlice()),
                        });
                    }
                }
                src_str.incref(1, roc_ops);
                return;
            },
            .int => {
                std.debug.assert(self.ptr != null);
                const value = self.asI128();
                const dest_bytes: [*]u8 = @ptrCast(dest_ptr);
                switch (self.layout.data.scalar.data.int) {
                    .u8 => try writeChecked(u8, dest_bytes, value),
                    .i8 => try writeChecked(i8, dest_bytes, value),
                    .u16 => try writeChecked(u16, dest_bytes, value),
                    .i16 => try writeChecked(i16, dest_bytes, value),
                    .u32 => try writeChecked(u32, dest_bytes, value),
                    .i32 => try writeChecked(i32, dest_bytes, value),
                    .u64 => try writeChecked(u64, dest_bytes, value),
                    .i64 => try writeChecked(i64, dest_bytes, value),
                    .u128 => try writeChecked(u128, dest_bytes, value),
                    .i128 => {
                        builtins.utils.alignedPtrCast(*i128, dest_bytes, @src()).* = value;
                    },
                }
                return;
            },
            else => {},
        }
    }

    if (self.layout.tag == .box) {
        const src_slot: *usize = builtins.utils.alignedPtrCast(*usize, @as([*]u8, @ptrCast(self.ptr.?)), @src());
        const dest_slot: *usize = builtins.utils.alignedPtrCast(*usize, @as([*]u8, @ptrCast(dest_ptr)), @src());
        dest_slot.* = src_slot.*;
        if (dest_slot.* != 0) {
            const data_ptr: [*]u8 = @as([*]u8, @ptrFromInt(dest_slot.*));
            builtins.utils.increfDataPtrC(@as(?[*]u8, data_ptr), 1, roc_ops);
        }
        return;
    }

    if (self.layout.tag == .box_of_zst) {
        const dest_slot: *usize = builtins.utils.alignedPtrCast(*usize, @as([*]u8, @ptrCast(dest_ptr)), @src());
        dest_slot.* = 0;
        return;
    }

    if (self.layout.tag == .list) {
        // Copy the list header and incref the underlying data
        std.debug.assert(self.ptr != null);
        const src_list: *const builtins.list.RocList = builtins.utils.alignedPtrCast(*const builtins.list.RocList, @as([*]u8, @ptrCast(self.ptr.?)), @src());
        const dest_list: *builtins.list.RocList = builtins.utils.alignedPtrCast(*builtins.list.RocList, @as([*]u8, @ptrCast(dest_ptr)), @src());
        dest_list.* = src_list.*;

        const elem_layout = layout_cache.getLayout(self.layout.data.list);
        const elements_refcounted = layout_cache.layoutContainsRefcounted(elem_layout);

        // Incref the list allocation. For seamless slices, this is the parent allocation,
        // not the bytes pointer (which points within the parent allocation).
        // We use getAllocationDataPtr() which correctly handles both regular lists
        // and seamless slices (where capacity_or_alloc_ptr stores the parent pointer).
        if (src_list.getAllocationDataPtr(roc_ops)) |alloc_ptr| {
            if (comptime trace_refcount) {
                const rc_before: isize = blk: {
                    if (@intFromPtr(alloc_ptr) % @alignOf(usize) != 0) break :blk -999;
                    const isizes: [*]isize = @ptrCast(@alignCast(alloc_ptr));
                    break :blk (isizes - 1)[0];
                };
                traceRefcount("INCREF list (copyToPtr) ptr=0x{x} len={} rc={} slice={} elems_rc={}", .{
                    @intFromPtr(alloc_ptr),
                    src_list.len(),
                    rc_before,
                    @intFromBool(src_list.isSeamlessSlice()),
                    @intFromBool(elements_refcounted),
                });
            }
            builtins.utils.increfDataPtrC(alloc_ptr, 1, roc_ops);
        }
        storeListElementCount(dest_list, elements_refcounted, roc_ops);
        return;
    }

    if (self.layout.tag == .list_of_zst) {
        // Copy the list header for ZST lists
        std.debug.assert(self.ptr != null);
        const src_list: *const builtins.list.RocList = builtins.utils.alignedPtrCast(*const builtins.list.RocList, @as([*]u8, @ptrCast(self.ptr.?)), @src());
        const dest_list: *builtins.list.RocList = builtins.utils.alignedPtrCast(*builtins.list.RocList, @as([*]u8, @ptrCast(dest_ptr)), @src());
        dest_list.* = src_list.*;
        return;
    }

    if (self.layout.tag == .record) {
        // Copy raw bytes first, then recursively incref all fields
        // We call incref on ALL fields (not just isRefcounted()) because:
        // - For directly refcounted types (str, list, box): increfs them
        // - For nested records/tuples: recursively handles their contents
        // - For scalars: incref is a no-op
        // This is symmetric with decref which also processes all fields.
        std.debug.assert(self.ptr != null);
        const src = @as([*]u8, @ptrCast(self.ptr.?))[0..result_size];
        const dst = @as([*]u8, @ptrCast(dest_ptr))[0..result_size];
        @memmove(dst, src);

        const record_data = layout_cache.getRecordData(self.layout.data.record.idx);
        if (record_data.fields.count == 0) return;

        const field_layouts = layout_cache.record_fields.sliceRange(record_data.getFields());
        const base_ptr = @as([*]u8, @ptrCast(self.ptr.?));

        var field_index: usize = 0;
        while (field_index < field_layouts.len) : (field_index += 1) {
            const field_info = field_layouts.get(field_index);
            const field_layout = layout_cache.getLayout(field_info.layout);

            const field_offset = layout_cache.getRecordFieldOffset(self.layout.data.record.idx, @intCast(field_index));
            const field_ptr = @as(*anyopaque, @ptrCast(base_ptr + field_offset));

            increfLayoutPtr(field_layout, field_ptr, layout_cache, roc_ops);
        }
        return;
    }

    if (self.layout.tag == .tuple) {
        // Copy raw bytes first, then recursively incref all elements
        // We call incref on ALL elements (not just isRefcounted()) because:
        // - For directly refcounted types (str, list, box): increfs them
        // - For nested records/tuples: recursively handles their contents
        // - For scalars: incref is a no-op
        // This is symmetric with decref which also processes all elements.
        std.debug.assert(self.ptr != null);
        const src = @as([*]u8, @ptrCast(self.ptr.?))[0..result_size];
        const dst = @as([*]u8, @ptrCast(dest_ptr))[0..result_size];
        @memmove(dst, src);

        const tuple_data = layout_cache.getTupleData(self.layout.data.tuple.idx);
        if (tuple_data.fields.count == 0) return;

        const element_layouts = layout_cache.tuple_fields.sliceRange(tuple_data.getFields());
        const base_ptr = @as([*]u8, @ptrCast(self.ptr.?));

        var elem_index: usize = 0;
        while (elem_index < element_layouts.len) : (elem_index += 1) {
            const elem_info = element_layouts.get(elem_index);
            const elem_layout = layout_cache.getLayout(elem_info.layout);

            const elem_offset = layout_cache.getTupleElementOffset(self.layout.data.tuple.idx, @intCast(elem_index));
            const elem_ptr = @as(*anyopaque, @ptrCast(base_ptr + elem_offset));

            increfLayoutPtr(elem_layout, elem_ptr, layout_cache, roc_ops);
        }
        return;
    }

    if (self.layout.tag == .closure) {
        // Copy the closure header and captures, then incref captured values.
        // Closures store captures in a record immediately after the header.
        std.debug.assert(self.ptr != null);
        const src = @as([*]u8, @ptrCast(self.ptr.?))[0..result_size];
        const dst = @as([*]u8, @ptrCast(dest_ptr))[0..result_size];
        @memmove(dst, src);

        // Get the closure header to find the captures layout
        const closure = self.asClosure().?;

        // Debug assertion: closure layout index must be within bounds.
        // If this trips, it indicates a compiler bug in layout index assignment.
        const idx_as_usize = @intFromEnum(closure.captures_layout_idx);
        std.debug.assert(idx_as_usize < layout_cache.layouts.len());

        const captures_layout = layout_cache.getLayout(closure.captures_layout_idx);

        // Only incref if there are actual captures (record with fields)
        if (captures_layout.tag == .record) {
            const record_data = layout_cache.getRecordData(captures_layout.data.record.idx);
            if (record_data.fields.count > 0) {
                if (comptime trace_refcount) {
                    traceRefcount("INCREF closure captures ptr=0x{x} fields={}", .{
                        @intFromPtr(self.ptr),
                        record_data.fields.count,
                    });
                }

                // Calculate the offset to the captures record (after header, with alignment)
                const header_size = @sizeOf(layout_mod.Closure);
                const cap_align = captures_layout.alignment(layout_cache.targetUsize());
                const aligned_off = std.mem.alignForward(usize, header_size, @intCast(cap_align.toByteUnits()));
                const base_ptr: [*]u8 = @ptrCast(@alignCast(self.ptr.?));
                const rec_ptr: [*]u8 = @ptrCast(base_ptr + aligned_off);

                // Incref the entire captures record (which handles all fields recursively)
                increfLayoutPtr(captures_layout, @ptrCast(rec_ptr), layout_cache, roc_ops);
            }
        }
        return;
    }

    if (self.layout.tag == .tag_union) {
        // Copy raw bytes first, then incref only the active variant's payload
        std.debug.assert(self.ptr != null);
        const src = @as([*]u8, @ptrCast(self.ptr.?))[0..result_size];
        const dst = @as([*]u8, @ptrCast(dest_ptr))[0..result_size];
        @memmove(dst, src);

        const base_ptr = @as([*]const u8, @ptrCast(self.ptr.?));
        const discriminant = readTagUnionDiscriminant(self.layout, base_ptr, layout_cache);
        const tu_data = layout_cache.getTagUnionData(self.layout.data.tag_union.idx);
        const variants = layout_cache.getTagUnionVariants(tu_data);
        const variant_layout = layout_cache.getLayout(variants.get(discriminant).payload_layout);

        if (comptime trace_refcount) {
            traceRefcount("INCREF tag_union (copyToPtr) disc={} variant_layout.tag={}", .{
                discriminant,
                @intFromEnum(variant_layout.tag),
            });
        }

        increfLayoutPtr(variant_layout, @as(*anyopaque, @ptrCast(@constCast(base_ptr))), layout_cache, roc_ops);
        return;
    }

    std.debug.assert(self.ptr != null);
    const src = @as([*]u8, @ptrCast(self.ptr.?))[0..result_size];
    const dst = @as([*]u8, @ptrCast(dest_ptr))[0..result_size];
    @memmove(dst, src);
}

/// Read this StackValue's integer value, ensuring it's initialized
/// Note: For u128 values larger than i128 max, use asU128() instead to get the correct value.
/// This function uses @bitCast for u128 which may give negative values for large unsigned numbers.
pub fn asI128(self: StackValue) i128 {
    std.debug.assert(self.is_initialized); // Ensure initialized before reading
    std.debug.assert(self.ptr != null);
    std.debug.assert(self.layout.tag == .scalar and self.layout.data.scalar.tag == .int);

    const raw_ptr: [*]u8 = @ptrCast(self.ptr.?);
    return switch (self.layout.data.scalar.data.int) {
        .u8 => readAligned(u8, raw_ptr),
        .i8 => readAligned(i8, raw_ptr),
        .u16 => readAligned(u16, raw_ptr),
        .i16 => readAligned(i16, raw_ptr),
        .u32 => readAligned(u32, raw_ptr),
        .i32 => readAligned(i32, raw_ptr),
        .u64 => readAligned(u64, raw_ptr),
        .i64 => readAligned(i64, raw_ptr),
        .i128 => readAligned(i128, raw_ptr),
        // Use @bitCast to avoid panic for values > i128 max
        .u128 => @bitCast(readAligned(u128, raw_ptr)),
    };
}

/// Read this StackValue's integer value as u128, ensuring it's initialized
/// Use this for unsigned values, especially u128 which can exceed i128 max
pub fn asU128(self: StackValue) u128 {
    std.debug.assert(self.is_initialized); // Ensure initialized before reading
    std.debug.assert(self.ptr != null);
    std.debug.assert(self.layout.tag == .scalar and self.layout.data.scalar.tag == .int);

    const raw_ptr: [*]u8 = @ptrCast(self.ptr.?);
    return switch (self.layout.data.scalar.data.int) {
        .u8 => readAligned(u8, raw_ptr),
        .u16 => readAligned(u16, raw_ptr),
        .u32 => readAligned(u32, raw_ptr),
        .u64 => readAligned(u64, raw_ptr),
        .u128 => readAligned(u128, raw_ptr),
        // Signed types: widen to i128 first to preserve sign, then bitcast to u128
        .i8 => @bitCast(@as(i128, readAligned(i8, raw_ptr))),
        .i16 => @bitCast(@as(i128, readAligned(i16, raw_ptr))),
        .i32 => @bitCast(@as(i128, readAligned(i32, raw_ptr))),
        .i64 => @bitCast(@as(i128, readAligned(i64, raw_ptr))),
        .i128 => @bitCast(readAligned(i128, raw_ptr)),
    };
}

/// Get the integer precision of this StackValue
pub fn getIntPrecision(self: StackValue) types.Int.Precision {
    std.debug.assert(self.layout.tag == .scalar and self.layout.data.scalar.tag == .int);
    return self.layout.data.scalar.data.int;
}

/// Initialise the StackValue integer value
/// Returns error.IntegerOverflow if the value doesn't fit in the target type
pub fn setInt(self: *StackValue, value: i128) error{IntegerOverflow}!void {
    std.debug.assert(self.ptr != null);
    std.debug.assert(self.layout.tag == .scalar and self.layout.data.scalar.tag == .int);
    std.debug.assert(!self.is_initialized); // Avoid accidental overwrite

    const raw_ptr: [*]u8 = @ptrCast(self.ptr.?);
    switch (self.layout.data.scalar.data.int) {
        .u8 => try writeChecked(u8, raw_ptr, value),
        .i8 => try writeChecked(i8, raw_ptr, value),
        .u16 => try writeChecked(u16, raw_ptr, value),
        .i16 => try writeChecked(i16, raw_ptr, value),
        .u32 => try writeChecked(u32, raw_ptr, value),
        .i32 => try writeChecked(i32, raw_ptr, value),
        .u64 => try writeChecked(u64, raw_ptr, value),
        .i64 => try writeChecked(i64, raw_ptr, value),
        .u128 => try writeChecked(u128, raw_ptr, value),
        .i128 => {
            // i128 always fits - no overflow check needed
            builtins.utils.alignedPtrCast(*i128, raw_ptr, @src()).* = value;
        },
    }
}

/// Initialise the StackValue integer value from raw bytes
/// This variant handles u128 values that don't fit in i128
pub fn setIntFromBytes(self: *StackValue, bytes: [16]u8, is_u128: bool) error{IntegerOverflow}!void {
    // Assert this is pointing to a valid memory location
    std.debug.assert(self.ptr != null);

    // Assert this is an integer
    std.debug.assert(self.layout.tag == .scalar and self.layout.data.scalar.tag == .int);

    // Assert this is uninitialised memory
    std.debug.assert(!self.is_initialized);

    const precision = self.layout.data.scalar.data.int;
    const raw_ptr = @as([*]u8, @ptrCast(self.ptr.?));

    // For u128 values, use bitcast directly; for i128 values, use the signed path
    if (is_u128) {
        const u128_value: u128 = @bitCast(bytes);
        switch (precision) {
            .u8 => {
                const typed_ptr: *u8 = @ptrCast(raw_ptr);
                typed_ptr.* = std.math.cast(u8, u128_value) orelse return error.IntegerOverflow;
            },
            .u16 => {
                const typed_ptr: *u16 = builtins.utils.alignedPtrCast(*u16, raw_ptr, @src());
                typed_ptr.* = std.math.cast(u16, u128_value) orelse return error.IntegerOverflow;
            },
            .u32 => {
                const typed_ptr: *u32 = builtins.utils.alignedPtrCast(*u32, raw_ptr, @src());
                typed_ptr.* = std.math.cast(u32, u128_value) orelse return error.IntegerOverflow;
            },
            .u64 => {
                const typed_ptr: *u64 = builtins.utils.alignedPtrCast(*u64, raw_ptr, @src());
                typed_ptr.* = std.math.cast(u64, u128_value) orelse return error.IntegerOverflow;
            },
            .u128 => {
                const typed_ptr: *u128 = builtins.utils.alignedPtrCast(*u128, raw_ptr, @src());
                typed_ptr.* = u128_value;
            },
            .i8, .i16, .i32, .i64, .i128 => {
                // Can't assign u128 to signed types - always overflow
                return error.IntegerOverflow;
            },
        }
    } else {
        const i128_value: i128 = @bitCast(bytes);
        try self.setInt(i128_value);
        return;
    }
}

/// Initialise the StackValue boolean value
pub fn setBool(self: *StackValue, value: u8) void {
    // Assert this is pointing to a valid memory location
    std.debug.assert(self.ptr != null);

    // Assert this is a boolean (u8 int)
    std.debug.assert(self.layout.tag == .scalar and self.layout.data.scalar.tag == .int);
    std.debug.assert(self.layout.data.scalar.data.int == .u8);

    // Assert this is uninitialised memory
    //
    // Avoid accidental overwrite, manually toggle this if updating an already initialized value
    std.debug.assert(!self.is_initialized);

    // Write the boolean value as a byte
    const typed_ptr: *u8 = @ptrCast(@alignCast(self.ptr.?));
    typed_ptr.* = value;
}

/// Read this StackValue's boolean value
pub fn asBool(self: StackValue) bool {
    std.debug.assert(self.is_initialized); // Ensure initialized before reading
    std.debug.assert(self.ptr != null);
    std.debug.assert(self.layout.tag == .scalar and self.layout.data.scalar.tag == .int);
    std.debug.assert(self.layout.data.scalar.data.int == .u8);

    // Read the boolean value as a byte
    const bool_ptr = @as(*const u8, @ptrCast(@alignCast(self.ptr.?)));
    return bool_ptr.* != 0;
}

/// Read this StackValue's f32 value
pub fn asF32(self: StackValue) f32 {
    std.debug.assert(self.is_initialized); // Ensure initialized before reading
    std.debug.assert(self.ptr != null);
    std.debug.assert(self.layout.tag == .scalar and self.layout.data.scalar.tag == .frac);
    std.debug.assert(self.layout.data.scalar.data.frac == .f32);

    const typed_ptr = @as(*const f32, @ptrCast(@alignCast(self.ptr.?)));
    return typed_ptr.*;
}

/// Read this StackValue's f64 value
pub fn asF64(self: StackValue) f64 {
    std.debug.assert(self.is_initialized); // Ensure initialized before reading
    std.debug.assert(self.ptr != null);
    std.debug.assert(self.layout.tag == .scalar and self.layout.data.scalar.tag == .frac);
    std.debug.assert(self.layout.data.scalar.data.frac == .f64);

    const typed_ptr = @as(*const f64, @ptrCast(@alignCast(self.ptr.?)));
    return typed_ptr.*;
}

/// Read this StackValue's Dec value
pub fn asDec(self: StackValue, roc_ops: *RocOps) RocDec {
    std.debug.assert(self.is_initialized); // Ensure initialized before reading
    std.debug.assert(self.ptr != null);
    std.debug.assert(self.layout.tag == .scalar and self.layout.data.scalar.tag == .frac);
    std.debug.assert(self.layout.data.scalar.data.frac == .dec);

    // RocDec contains i128 which requires 16-byte alignment (debug builds only for performance)
    if (comptime builtin.mode == .Debug) {
        const ptr_val = @intFromPtr(self.ptr.?);
        if (ptr_val % @alignOf(i128) != 0) {
            var buf: [64]u8 = undefined;
            const msg = std.fmt.bufPrint(&buf, "[asDec] alignment error: ptr=0x{x}", .{ptr_val}) catch "[asDec] alignment error";
            roc_ops.crash(msg);
        }
    }
    const typed_ptr = @as(*const RocDec, @ptrCast(@alignCast(self.ptr.?)));
    return typed_ptr.*;
}

/// Initialise the StackValue f32 value
pub fn setF32(self: *StackValue, value: f32) void {
    // Assert this is pointing to a valid memory location
    std.debug.assert(self.ptr != null);

    // Assert this is an f32
    std.debug.assert(self.layout.tag == .scalar and self.layout.data.scalar.tag == .frac);
    std.debug.assert(self.layout.data.scalar.data.frac == .f32);

    // Assert this is uninitialised memory
    //
    // Avoid accidental overwrite, manually toggle this if updating an already initialized value
    std.debug.assert(!self.is_initialized);

    // Write the f32 value
    const typed_ptr: *f32 = @ptrCast(@alignCast(self.ptr.?));
    typed_ptr.* = value;
}

/// Initialise the StackValue f64 value
pub fn setF64(self: *StackValue, value: f64) void {
    // Assert this is pointing to a valid memory location
    std.debug.assert(self.ptr != null);

    // Assert this is an f64
    std.debug.assert(self.layout.tag == .scalar and self.layout.data.scalar.tag == .frac);
    std.debug.assert(self.layout.data.scalar.data.frac == .f64);

    // Assert this is uninitialised memory
    //
    // Avoid accidental overwrite, manually toggle this if updating an already initialized value
    std.debug.assert(!self.is_initialized);

    // Write the f64 value
    const typed_ptr: *f64 = @ptrCast(@alignCast(self.ptr.?));
    typed_ptr.* = value;
}

/// Initialise the StackValue Dec value
pub fn setDec(self: *StackValue, value: RocDec, roc_ops: *RocOps) void {
    // Assert this is pointing to a valid memory location
    std.debug.assert(self.ptr != null);

    // Assert this is a Dec
    std.debug.assert(self.layout.tag == .scalar and self.layout.data.scalar.tag == .frac);
    std.debug.assert(self.layout.data.scalar.data.frac == .dec);

    // Assert this is uninitialised memory
    //
    // Avoid accidental overwrite, manually toggle this if updating an already initialized value
    std.debug.assert(!self.is_initialized);

    // RocDec contains i128 which requires 16-byte alignment (debug builds only for performance)
    if (comptime builtin.mode == .Debug) {
        const ptr_val = @intFromPtr(self.ptr.?);
        if (ptr_val % @alignOf(i128) != 0) {
            var buf: [64]u8 = undefined;
            const msg = std.fmt.bufPrint(&buf, "[setDec] alignment error: ptr=0x{x}", .{ptr_val}) catch "[setDec] alignment error";
            roc_ops.crash(msg);
            return;
        }
    }

    // Write the Dec value
    const typed_ptr: *RocDec = @ptrCast(@alignCast(self.ptr.?));
    typed_ptr.* = value;
}

/// Create a TupleAccessor for safe tuple element access
pub fn asTuple(self: StackValue, layout_cache: *LayoutStore) !TupleAccessor {
    std.debug.assert(self.is_initialized); // Tuple must be initialized before accessing
    std.debug.assert(self.ptr != null);
    std.debug.assert(self.layout.tag == .tuple);

    const tuple_data = layout_cache.getTupleData(self.layout.data.tuple.idx);
    const element_layouts = layout_cache.tuple_fields.sliceRange(tuple_data.getFields());

    return TupleAccessor{
        .base_value = self,
        .layout_cache = layout_cache,
        .tuple_layout = self.layout,
        .element_layouts = element_layouts,
    };
}

/// Safe accessor for tuple elements with bounds checking and proper memory management
pub const TupleAccessor = struct {
    base_value: StackValue,
    layout_cache: *LayoutStore,
    tuple_layout: Layout,
    element_layouts: layout_mod.TupleField.SafeMultiList.Slice,

    /// Get a StackValue for the element at the given original index (before sorting)
    pub fn getElement(self: TupleAccessor, original_index: usize, elem_rt_var: types.Var) !StackValue {
        // Find the sorted index corresponding to this original index
        const sorted_index = self.findElementIndexByOriginal(original_index) orelse return error.TupleIndexOutOfBounds;

        std.debug.assert(self.base_value.is_initialized);
        std.debug.assert(self.base_value.ptr != null);

        const element_layout_info = self.element_layouts.get(sorted_index);
        const element_layout = self.layout_cache.getLayout(element_layout_info.layout);

        // Get the offset for this element within the tuple (using sorted index)
        const element_offset = self.layout_cache.getTupleElementOffset(self.tuple_layout.data.tuple.idx, @intCast(sorted_index));

        // Calculate the element pointer with proper alignment
        const base_ptr = @as([*]u8, @ptrCast(self.base_value.ptr.?));
        const element_ptr = @as(*anyopaque, @ptrCast(base_ptr + element_offset));
        const required_alignment = element_layout.alignment(self.layout_cache.targetUsize()).toByteUnits();
        if (required_alignment > 1) {
            const addr = @intFromPtr(element_ptr);
            std.debug.assert(addr % required_alignment == 0);
        }

        return StackValue{
            .layout = element_layout,
            .ptr = element_ptr,
            .is_initialized = true, // Elements in existing tuples are initialized
            .rt_var = elem_rt_var,
        };
    }

    /// Get just the element pointer without needing type information (for internal operations like setElement)
    pub fn getElementPtr(self: TupleAccessor, original_index: usize) !*anyopaque {
        const sorted_index = self.findElementIndexByOriginal(original_index) orelse return error.TupleIndexOutOfBounds;
        std.debug.assert(self.base_value.is_initialized);
        std.debug.assert(self.base_value.ptr != null);
        const element_offset = self.layout_cache.getTupleElementOffset(self.tuple_layout.data.tuple.idx, @intCast(sorted_index));
        const base_ptr = @as([*]u8, @ptrCast(self.base_value.ptr.?));
        return @as(*anyopaque, @ptrCast(base_ptr + element_offset));
    }

    /// Set an element by copying from a source StackValue
    pub fn setElement(self: TupleAccessor, index: usize, source: StackValue, roc_ops: *RocOps) !void {
        const dest_ptr = try self.getElementPtr(index);
        try source.copyToPtr(self.layout_cache, dest_ptr, roc_ops);
    }

    /// Find the sorted element index corresponding to an original tuple position
    pub fn findElementIndexByOriginal(self: TupleAccessor, original_index: usize) ?usize {
        for (0..self.element_layouts.len) |i| {
            const elem = self.element_layouts.get(i);
            if (elem.index == original_index) return i;
        }
        return null;
    }

    /// Get the number of elements in this tuple
    pub fn getElementCount(self: TupleAccessor) usize {
        return self.element_layouts.len;
    }

    /// Get the layout of the element at the given index
    pub fn getElementLayout(self: TupleAccessor, index: usize) !Layout {
        if (index >= self.element_layouts.len) {
            return error.TupleIndexOutOfBounds;
        }
        const element_layout_info = self.element_layouts.get(index);
        return self.layout_cache.getLayout(element_layout_info.layout);
    }
};

/// Create a TagUnionAccessor for safe tag union access
pub fn asTagUnion(self: StackValue, layout_cache: *LayoutStore) !TagUnionAccessor {
    std.debug.assert(self.is_initialized);
    std.debug.assert(self.ptr != null);
    std.debug.assert(self.layout.tag == .tag_union);

    const tu_data = layout_cache.getTagUnionData(self.layout.data.tag_union.idx);

    return TagUnionAccessor{
        .base_value = self,
        .layout_cache = layout_cache,
        .tu_data = tu_data.*,
    };
}

/// Safe accessor for tag union values
pub const TagUnionAccessor = struct {
    base_value: StackValue,
    layout_cache: *LayoutStore,
    tu_data: layout_mod.TagUnionData,

    /// Read the discriminant (tag index) from the tag union
    pub fn getDiscriminant(self: TagUnionAccessor) usize {
        const base_ptr: [*]const u8 = @ptrCast(self.base_value.ptr.?);
        // Use dynamic offset computation to handle recursive types correctly
        return readTagUnionDiscriminant(self.base_value.layout, base_ptr, self.layout_cache);
    }

    /// Get the layout for a specific variant by discriminant
    pub fn getVariantLayout(self: *const TagUnionAccessor, discriminant: usize) Layout {
        const variants = self.layout_cache.getTagUnionVariants(&self.tu_data);
        std.debug.assert(discriminant < variants.len);
        const variant = variants.get(discriminant);
        return self.layout_cache.getLayout(variant.payload_layout);
    }

    /// Get a StackValue for the payload at offset 0
    pub fn getPayload(self: TagUnionAccessor, payload_layout: Layout) StackValue {
        // Payload is always at offset 0 in our tag union layout
        return StackValue{
            .layout = payload_layout,
            .ptr = self.base_value.ptr,
            .is_initialized = true,
        };
    }

    /// Get discriminant and payload layout together
    pub fn getVariant(self: *const TagUnionAccessor) struct { discriminant: usize, payload_layout: Layout } {
        const discriminant = self.getDiscriminant();
        const payload_layout = self.getVariantLayout(discriminant);
        return .{ .discriminant = discriminant, .payload_layout = payload_layout };
    }
};

/// Create a ListAccessor for safe list element access
pub fn asList(self: StackValue, layout_cache: *LayoutStore, element_layout: Layout, roc_ops: *RocOps) !ListAccessor {
    std.debug.assert(self.is_initialized);
    std.debug.assert(self.ptr != null);
    std.debug.assert(self.layout.tag == .list or self.layout.tag == .list_of_zst);

    // Verify alignment before @alignCast (debug builds only for performance)
    if (comptime builtin.mode == .Debug) {
        const ptr_int = @intFromPtr(self.ptr.?);
        if (ptr_int % @alignOf(RocList) != 0) {
            var buf: [64]u8 = undefined;
            const msg = std.fmt.bufPrint(&buf, "[asList] alignment error: ptr=0x{x}", .{ptr_int}) catch "[asList] alignment error";
            roc_ops.crash(msg);
        }
    }
    const header: *const RocList = @ptrCast(@alignCast(self.ptr.?));
    return ListAccessor{
        .base_value = self,
        .layout_cache = layout_cache,
        .element_layout = element_layout,
        .element_size = layout_cache.layoutSize(element_layout),
        .list = header.*,
    };
}

/// Safe accessor for list elements with bounds checking
pub const ListAccessor = struct {
    base_value: StackValue,
    layout_cache: *LayoutStore,
    element_layout: Layout,
    element_size: usize,
    list: RocList,

    pub fn len(self: ListAccessor) usize {
        return self.list.len();
    }

    pub fn getElement(self: ListAccessor, index: usize, elem_rt_var: types.Var) !StackValue {
        if (index >= self.list.len()) return error.ListIndexOutOfBounds;

        if (self.element_size == 0) {
            return StackValue{ .layout = self.element_layout, .ptr = null, .is_initialized = true, .rt_var = elem_rt_var };
        }

        const base_ptr = self.list.bytes orelse return error.NullStackPointer;
        const offset = index * self.element_size;
        return StackValue{
            .layout = self.element_layout,
            .ptr = @ptrCast(base_ptr + offset),
            .is_initialized = true,
            .rt_var = elem_rt_var,
        };
    }

    /// Get just the element pointer without needing type information (for internal operations)
    pub fn getElementPtr(self: ListAccessor, index: usize) !?*anyopaque {
        if (index >= self.list.len()) return error.ListIndexOutOfBounds;
        if (self.element_size == 0) return null;
        const base_ptr = self.list.bytes orelse return error.NullStackPointer;
        const offset = index * self.element_size;
        return @ptrCast(base_ptr + offset);
    }
};

fn storeListElementCount(list: *RocList, elements_refcounted: bool, roc_ops: *RocOps) void {
    if (elements_refcounted and !list.isSeamlessSlice()) {
        if (list.getAllocationDataPtr(roc_ops)) |source| {
            // Verify alignment before @alignCast (debug builds only for performance)
            if (comptime builtin.mode == .Debug) {
                const source_int = @intFromPtr(source);
                if (source_int % @alignOf(usize) != 0) {
                    var buf: [64]u8 = undefined;
                    const msg = std.fmt.bufPrint(&buf, "[storeListElementCount] alignment error: 0x{x}", .{source_int}) catch "[storeListElementCount] alignment error";
                    roc_ops.crash(msg);
                }
            }
            const ptr = @as([*]usize, @ptrCast(@alignCast(source))) - 2;
            ptr[0] = list.length;
        }
    }
}

/// Create a RecordAccessor for safe record field access
pub fn asRecord(self: StackValue, layout_cache: *LayoutStore) !RecordAccessor {
    std.debug.assert(self.is_initialized); // Record must be initialized before accessing
    // Note: ptr can be null for records with all ZST fields
    std.debug.assert(self.layout.tag == .record);

    const record_data = layout_cache.getRecordData(self.layout.data.record.idx);
    const field_layouts = layout_cache.record_fields.sliceRange(record_data.getFields());

    return RecordAccessor{
        .base_value = self,
        .layout_cache = layout_cache,
        .record_layout = self.layout,
        .field_layouts = field_layouts,
    };
}

/// Safe accessor for record fields with bounds checking and proper memory management
pub const RecordAccessor = struct {
    base_value: StackValue,
    layout_cache: *LayoutStore,
    record_layout: Layout,
    field_layouts: layout_mod.RecordField.SafeMultiList.Slice,

    /// Get a StackValue for the field at the given index
    pub fn getFieldByIndex(self: RecordAccessor, index: usize, field_rt_var: types.Var) !StackValue {
        if (index >= self.field_layouts.len) {
            return error.RecordIndexOutOfBounds;
        }

        std.debug.assert(self.base_value.is_initialized);
        std.debug.assert(self.base_value.ptr != null);

        const field_layout_info = self.field_layouts.get(index);
        const field_layout = self.layout_cache.getLayout(field_layout_info.layout);

        // Get the offset for this field within the record
        const field_offset = self.layout_cache.getRecordFieldOffset(self.record_layout.data.record.idx, @intCast(index));

        // Calculate the field pointer with proper alignment
        const base_ptr = @as([*]u8, @ptrCast(self.base_value.ptr.?));
        const field_ptr = @as(*anyopaque, @ptrCast(base_ptr + field_offset));
        const required_alignment = field_layout.alignment(self.layout_cache.targetUsize()).toByteUnits();
        if (required_alignment > 1) {
            const addr = @intFromPtr(field_ptr);
            std.debug.assert(addr % required_alignment == 0);
        }

        return StackValue{
            .layout = field_layout,
            .ptr = field_ptr,
            .is_initialized = true, // Fields in existing records are initialized
            .rt_var = field_rt_var,
        };
    }

    /// Get a StackValue for the field with the given name
    pub fn getFieldByName(self: RecordAccessor, field_name_idx: Ident.Idx, field_rt_var: types.Var) !?StackValue {
        const field_offset = self.layout_cache.getRecordFieldOffsetByName(
            self.record_layout.data.record.idx,
            field_name_idx,
        ) orelse return null;

        // Find the field layout by name
        var field_layout: ?Layout = null;
        for (0..self.field_layouts.len) |i| {
            const field_info = self.field_layouts.get(i);
            // We need to get the field name from the layout cache's identifier store
            // This is a limitation - we'd need access to the env to get the actual name
            // For now, we'll use the offset-based approach
            const this_field_offset = self.layout_cache.getRecordFieldOffset(self.record_layout.data.record.idx, @intCast(i));
            if (this_field_offset == field_offset) {
                field_layout = self.layout_cache.getLayout(field_info.layout);
                break;
            }
        }

        if (field_layout == null) return null;

        const base_ptr = @as([*]u8, @ptrCast(self.base_value.ptr.?));
        const field_ptr = @as(*anyopaque, @ptrCast(base_ptr + field_offset));
        const required_alignment = field_layout.?.alignment(self.layout_cache.targetUsize()).toByteUnits();
        if (required_alignment > 1) {
            const addr = @intFromPtr(field_ptr);
            std.debug.assert(addr % required_alignment == 0);
        }

        return StackValue{
            .layout = field_layout.?,
            .ptr = field_ptr,
            .is_initialized = true,
            .rt_var = field_rt_var,
        };
    }

    /// Set a field by copying from a source StackValue
    pub fn setFieldByIndex(self: RecordAccessor, index: usize, source: StackValue, roc_ops: *RocOps) !void {
        const dest_field = try self.getFieldByIndex(index, source.rt_var);
        try source.copyToPtr(self.layout_cache, dest_field.ptr.?, roc_ops);
    }

    /// Get the number of fields in this record
    pub fn getFieldCount(self: RecordAccessor) usize {
        return self.field_layouts.len;
    }

    /// Get the layout of the field at the given index
    pub fn getFieldLayout(self: RecordAccessor, index: usize) !Layout {
        if (index >= self.field_layouts.len) {
            return error.RecordIndexOutOfBounds;
        }
        const field_layout_info = self.field_layouts.get(index);
        return self.layout_cache.getLayout(field_layout_info.layout);
    }

    /// Find field index by comparing field ident indices
    pub fn findFieldIndex(self: RecordAccessor, field_ident: Ident.Idx) ?usize {
        for (0..self.field_layouts.len) |idx| {
            const field = self.field_layouts.get(idx);
            if (field.name == field_ident) {
                return idx;
            }
        }
        return null;
    }
};

/// Get this value as a string pointer, or null if the pointer is null.
pub fn asRocStr(self: StackValue) ?*RocStr {
    std.debug.assert(self.layout.tag == .scalar and self.layout.data.scalar.tag == .str);
    if (self.ptr) |ptr| {
        return @ptrCast(@alignCast(ptr));
    }
    return null;
}

/// Set this value's contents to a RocStr.
/// Panics if ptr is null or layout is not a string type.
pub fn setRocStr(self: StackValue, value: RocStr) void {
    std.debug.assert(self.layout.tag == .scalar and self.layout.data.scalar.tag == .str);
    const str_ptr: *RocStr = @ptrCast(@alignCast(self.ptr.?));
    str_ptr.* = value;
}

/// Zero-initialize this value's memory based on its layout size.
/// Used for union payloads that need clearing before writing a smaller variant.
/// No-op if ptr is null.
pub fn clearBytes(self: StackValue, layout_cache: *LayoutStore) void {
    if (self.ptr) |ptr| {
        const size = layout_cache.layoutSize(self.layout);
        if (size > 0) {
            @memset(@as([*]u8, @ptrCast(ptr))[0..size], 0);
        }
    }
}

/// Get this value as a list pointer, or null if the pointer is null.
/// Caller can use `.?` to panic on null if they're confident it's non-null.
pub fn asRocList(self: StackValue) ?*RocList {
    std.debug.assert(self.layout.tag == .list or self.layout.tag == .list_of_zst);
    if (self.ptr) |ptr| {
        return @ptrCast(@alignCast(ptr));
    }
    return null;
}

/// Set this value's contents to a RocList.
/// Panics if ptr is null or layout is not a list type.
pub fn setRocList(self: StackValue, value: RocList) void {
    std.debug.assert(self.layout.tag == .list or self.layout.tag == .list_of_zst);
    const list_ptr: *RocList = @ptrCast(@alignCast(self.ptr.?));
    list_ptr.* = value;
}

/// Get this value as a closure header pointer, or null if ptr is null.
/// Caller can use `.?` to panic on null if they're confident it's non-null.
pub fn asClosure(self: StackValue) ?*const Closure {
    std.debug.assert(self.layout.tag == .closure);
    if (self.ptr) |ptr| {
        return @ptrCast(@alignCast(ptr));
    }
    return null;
}

/// Get the box slot pointer (holds address of heap data), or null if ptr is null.
/// Use this for low-level slot manipulation (copy, zero, etc.)
pub fn asBoxSlot(self: StackValue) ?*usize {
    std.debug.assert(self.layout.tag == .box or self.layout.tag == .box_of_zst);
    if (self.ptr) |ptr| {
        return @ptrCast(@alignCast(ptr));
    }
    return null;
}

/// Get the heap data pointer from inside the box, or null if box is empty.
/// This reads the slot and converts to a byte pointer.
pub fn getBoxedData(self: StackValue) ?[*]u8 {
    std.debug.assert(self.layout.tag == .box or self.layout.tag == .box_of_zst);
    if (self.ptr) |ptr| {
        const slot: *const usize = @ptrCast(@alignCast(ptr));
        if (slot.* == 0) return null;
        return @ptrFromInt(slot.*);
    }
    return null;
}

/// Initialize a box slot with a data pointer.
/// Used during box creation after allocation.
pub fn initBoxSlot(self: StackValue, data_ptr: ?*anyopaque) void {
    std.debug.assert(self.layout.tag == .box or self.layout.tag == .box_of_zst);
    const slot: *usize = @ptrCast(@alignCast(self.ptr.?));
    slot.* = if (data_ptr) |p| @intFromPtr(p) else 0;
}

/// Clear a box slot (set to 0/null).
/// Used during destruction after decref.
pub fn clearBoxSlot(self: StackValue) void {
    std.debug.assert(self.layout.tag == .box or self.layout.tag == .box_of_zst);
    const slot: *usize = @ptrCast(@alignCast(self.ptr.?));
    slot.* = 0;
}

/// Move this value to binding (transfers ownership, no refcounts change)
pub fn moveForBinding(self: StackValue) StackValue {
    return self;
}

/// Copy value data to another StackValue (with special string handling)
pub fn copyTo(self: StackValue, dest: StackValue, layout_cache: *LayoutStore, roc_ops: *RocOps) void {
    std.debug.assert(self.is_initialized);
    std.debug.assert(dest.ptr != null);

    // For closures, use getTotalSize to include capture data; for others use layoutSize
    const size = if (self.layout.tag == .closure) self.getTotalSize(layout_cache, roc_ops) else layout_cache.layoutSize(self.layout);
    if (size == 0) return;

    if (self.layout.tag == .scalar and self.layout.data.scalar.tag == .str) {
        // String: use proper struct copy and increment ref count
        const src_str: *const RocStr = @ptrCast(@alignCast(self.ptr.?));
        const dest_str: *RocStr = @ptrCast(@alignCast(dest.ptr.?));
        dest_str.* = src_str.*;
        if (comptime trace_refcount) {
            if (!src_str.isSmallStr()) {
                const alloc_ptr = src_str.getAllocationPtr();
                const rc_before: isize = if (alloc_ptr) |ptr| blk: {
                    if (@intFromPtr(ptr) % @alignOf(usize) != 0) break :blk -999;
                    const isizes: [*]isize = @ptrCast(@alignCast(ptr));
                    break :blk (isizes - 1)[0];
                } else 0;
                traceRefcount("INCREF str (copyTo) ptr=0x{x} len={} rc={} slice={}", .{
                    @intFromPtr(alloc_ptr),
                    src_str.len(),
                    rc_before,
                    @intFromBool(src_str.isSeamlessSlice()),
                });
            }
        }
        dest_str.incref(1, roc_ops);
        return;
    }

    if (self.layout.tag == .list or self.layout.tag == .list_of_zst) {
        const dest_list: *RocList = @ptrCast(@alignCast(dest.ptr.?));
        if (self.ptr == null) {
            dest_list.* = RocList.empty();
            return;
        }

        const src_list = @as(*const RocList, @ptrCast(@alignCast(self.ptr.?))).*;
        dest_list.* = src_list;

        if (self.layout.tag == .list) {
            const elem_layout = layout_cache.getLayout(self.layout.data.list);
            const elements_refcounted = layout_cache.layoutContainsRefcounted(elem_layout);
            dest_list.incref(1, elements_refcounted, roc_ops);
            storeListElementCount(dest_list, elements_refcounted, roc_ops);
        } else {
            dest_list.incref(1, false, roc_ops);
        }
        return;
    }

    if (self.layout.tag == .box) {
        const src_slot = self.asBoxSlot().?;
        const dest_slot = dest.asBoxSlot().?;
        dest_slot.* = src_slot.*;
        if (dest_slot.* != 0) {
            const data_ptr: [*]u8 = @ptrFromInt(dest_slot.*);
            builtins.utils.increfDataPtrC(@as(?[*]u8, data_ptr), 1, roc_ops);
        }
        return;
    }

    if (self.layout.tag == .box_of_zst) {
        dest.clearBoxSlot();
        return;
    }

    // Everything else just copy the bytes
    std.mem.copyForwards(
        u8,
        @as([*]u8, @ptrCast(dest.ptr.?))[0..size],
        @as([*]const u8, @ptrCast(self.ptr.?))[0..size],
    );
}

/// Copy value data to another StackValue WITHOUT incrementing refcounts (move semantics)
pub fn copyWithoutRefcount(self: StackValue, dest: StackValue, layout_cache: *LayoutStore, roc_ops: *RocOps) void {
    std.debug.assert(self.is_initialized);
    std.debug.assert(dest.ptr != null);

    // For closures, use getTotalSize to include capture data; for others use layoutSize
    const size = if (self.layout.tag == .closure) self.getTotalSize(layout_cache, roc_ops) else layout_cache.layoutSize(self.layout);
    if (size == 0) return;

    if (self.layout.tag == .scalar and self.layout.data.scalar.tag == .str) {
        // String: use proper struct copy WITHOUT incrementing ref count (move semantics)
        const src_str: *const RocStr = @ptrCast(@alignCast(self.ptr.?));
        const dest_str: *RocStr = @ptrCast(@alignCast(dest.ptr.?));
        dest_str.* = src_str.*; // Just copy the struct, no refcount change
    } else {
        if (self.layout.tag == .box or self.layout.tag == .box_of_zst) {
            const src_slot = self.asBoxSlot().?;
            const dest_slot = dest.asBoxSlot().?;
            dest_slot.* = src_slot.*;
            return;
        }
        // Everything else just copy the bytes
        std.mem.copyForwards(
            u8,
            @as([*]u8, @ptrCast(dest.ptr.?))[0..size],
            @as([*]const u8, @ptrCast(self.ptr.?))[0..size],
        );
    }
}

/// Increment reference count for refcounted types.
/// Must be symmetric with decref - handles records and tuples by recursively incref'ing fields.
pub fn incref(self: StackValue, layout_cache: *LayoutStore, roc_ops: *RocOps) void {
    if (comptime trace_refcount) {
        traceRefcount("INCREF layout.tag={} ptr=0x{x}", .{ @intFromEnum(self.layout.tag), @intFromPtr(self.ptr) });
    }

    if (self.layout.tag == .scalar and self.layout.data.scalar.tag == .str) {
        const roc_str = self.asRocStr().?;
        if (comptime trace_refcount) {
            // Small strings have no allocation - skip refcount tracing for them
            if (roc_str.isSmallStr()) {
                traceRefcount("INCREF str (small) len={}", .{roc_str.len()});
            } else {
                const alloc_ptr = roc_str.getAllocationPtr();
                const rc_before: isize = if (alloc_ptr) |ptr| blk: {
                    if (@intFromPtr(ptr) % @alignOf(usize) != 0) {
                        traceRefcount("INCREF str ptr=0x{x} MISALIGNED!", .{@intFromPtr(ptr)});
                        break :blk -999;
                    }
                    const isizes: [*]isize = @ptrCast(@alignCast(ptr));
                    break :blk (isizes - 1)[0];
                } else 0;
                traceRefcount("INCREF str ptr=0x{x} len={} cap={} rc={} slice={}", .{
                    @intFromPtr(alloc_ptr),
                    roc_str.len(),
                    roc_str.getCapacity(),
                    rc_before,
                    @intFromBool(roc_str.isSeamlessSlice()),
                });
            }
        }
        roc_str.incref(1, roc_ops);
        return;
    }
    if (self.layout.tag == .list) {
        if (self.ptr == null) return;
        const list_value = @as(*const RocList, @ptrCast(@alignCast(self.ptr.?))).*;
        if (comptime trace_refcount) {
            traceRefcount("INCREF list ptr=0x{x} len={}", .{
                @intFromPtr(list_value.getAllocationDataPtr(roc_ops)),
                list_value.len(),
            });
        }
        // We don't know element layout here to store counts; assume caller already handled
        list_value.incref(1, false, roc_ops);
        return;
    }
    if (self.layout.tag == .box) {
        const slot = self.asBoxSlot() orelse return;
        if (slot.* != 0) {
            if (comptime trace_refcount) {
                traceRefcount("INCREF box ptr=0x{x}", .{slot.*});
            }
            const data_ptr: [*]u8 = @ptrFromInt(slot.*);
            builtins.utils.increfDataPtrC(@as(?[*]u8, data_ptr), 1, roc_ops);
        }
        return;
    }
    // Handle records by recursively incref'ing each field (symmetric with decref)
    if (self.layout.tag == .record) {
        increfLayoutPtr(self.layout, self.ptr, layout_cache, roc_ops);
        return;
    }
    // Handle tuples by recursively incref'ing each element (symmetric with decref)
    if (self.layout.tag == .tuple) {
        increfLayoutPtr(self.layout, self.ptr, layout_cache, roc_ops);
        return;
    }
    // Handle tag unions by reading discriminant and incref'ing only the active variant's payload
    if (self.layout.tag == .tag_union) {
        if (self.ptr == null) return;
        const base_ptr = @as([*]const u8, @ptrCast(self.ptr.?));
        // Use dynamic offset computation to handle recursive types correctly
        const discriminant = readTagUnionDiscriminant(self.layout, base_ptr, layout_cache);

        const tu_data = layout_cache.getTagUnionData(self.layout.data.tag_union.idx);
        const variants = layout_cache.getTagUnionVariants(tu_data);
        std.debug.assert(discriminant < variants.len);
        const variant_layout = layout_cache.getLayout(variants.get(discriminant).payload_layout);

        if (comptime trace_refcount) {
            traceRefcount("INCREF tag_union disc={} variant_layout.tag={}", .{ discriminant, @intFromEnum(variant_layout.tag) });
        }

        increfLayoutPtr(variant_layout, @as(*anyopaque, @ptrCast(@constCast(base_ptr))), layout_cache, roc_ops);
        return;
    }
    // Handle closures by incref'ing their captures (symmetric with decref)
    if (self.layout.tag == .closure) {
        if (self.ptr == null) return;
        const closure_header: *const layout_mod.Closure = @ptrCast(@alignCast(self.ptr.?));

        // Debug assertion: closure layout index must be within bounds.
        // If this trips, it indicates a compiler bug in layout index assignment.
        const idx_as_usize = @intFromEnum(closure_header.captures_layout_idx);
        std.debug.assert(idx_as_usize < layout_cache.layouts.len());

        const captures_layout = layout_cache.getLayout(closure_header.captures_layout_idx);

        // Only incref if there are actual captures (record with fields)
        if (captures_layout.tag == .record) {
            const record_data = layout_cache.getRecordData(captures_layout.data.record.idx);
            if (record_data.fields.count > 0) {
                if (comptime trace_refcount) {
                    traceRefcount("INCREF closure captures ptr=0x{x} fields={}", .{
                        @intFromPtr(self.ptr),
                        record_data.fields.count,
                    });
                }
                const header_size = @sizeOf(layout_mod.Closure);
                const cap_align = captures_layout.alignment(layout_cache.targetUsize());
                const aligned_off = std.mem.alignForward(usize, header_size, @intCast(cap_align.toByteUnits()));
                const base_ptr: [*]u8 = @ptrCast(@alignCast(self.ptr.?));
                const rec_ptr: *anyopaque = @ptrCast(base_ptr + aligned_off);
                increfLayoutPtr(captures_layout, rec_ptr, layout_cache, roc_ops);
            }
        }
        return;
    }
}

/// Trace helper for refcount operations. Only active when built with -Dtrace-refcount=true.
/// Output goes to stderr to avoid interfering with app stdout.
/// Note: Tracing is disabled on freestanding targets (wasm) as they have no stderr.
fn traceRefcount(comptime fmt: []const u8, args: anytype) void {
    if (comptime trace_refcount and builtin.os.tag != .freestanding) {
        const stderr_file: std.fs.File = .stderr();
        var buf: [512]u8 = undefined;
        const msg = std.fmt.bufPrint(&buf, "[REFCOUNT] " ++ fmt ++ "\n", args) catch return;
        stderr_file.writeAll(msg) catch {};
    }
}

/// Trace helper with source location for debugging where decrefs originate
pub fn traceRefcountWithSource(comptime src: std.builtin.SourceLocation, comptime fmt: []const u8, args: anytype) void {
    if (comptime trace_refcount and builtin.os.tag != .freestanding) {
        const stderr_file: std.fs.File = .stderr();
        var buf: [512]u8 = undefined;
        const msg = std.fmt.bufPrint(&buf, "[REFCOUNT @{s}:{d}] " ++ fmt ++ "\n", .{ src.file, src.line } ++ args) catch return;
        stderr_file.writeAll(msg) catch {};
    }
}

/// Decrement reference count for refcounted types
pub fn decref(self: StackValue, layout_cache: *LayoutStore, ops: *RocOps) void {
    if (comptime trace_refcount) {
        traceRefcount("DECREF layout.tag={} ptr=0x{x}", .{ @intFromEnum(self.layout.tag), @intFromPtr(self.ptr) });
    }

    switch (self.layout.tag) {
        .scalar => switch (self.layout.data.scalar.tag) {
            .str => {
                const roc_str = self.asRocStr().?;
                if (comptime trace_refcount) {
                    // Small strings have no allocation - skip refcount tracing for them
                    if (roc_str.isSmallStr()) {
                        traceRefcount("DECREF str (small) len={}", .{roc_str.len()});
                    } else {
                        const alloc_ptr = roc_str.getAllocationPtr();
                        // Only read refcount if pointer is aligned (safety check)
                        const rc_before: isize = if (alloc_ptr) |ptr| blk: {
                            if (@intFromPtr(ptr) % @alignOf(usize) != 0) {
                                traceRefcount("DECREF str ptr=0x{x} MISALIGNED!", .{@intFromPtr(ptr)});
                                break :blk -999;
                            }
                            const isizes: [*]isize = @ptrCast(@alignCast(ptr));
                            break :blk (isizes - 1)[0];
                        } else 0;
                        traceRefcount("DECREF str ptr=0x{x} len={} cap={} rc={} slice={}", .{
                            @intFromPtr(alloc_ptr),
                            roc_str.len(),
                            roc_str.getCapacity(),
                            rc_before,
                            @intFromBool(roc_str.isSeamlessSlice()),
                        });
                    }
                }
                roc_str.decref(ops);
                return;
            },
            else => {},
        },
        .list => {
            const list_header = self.asRocList() orelse return;
            const list_value = list_header.*;
            const elem_layout = layout_cache.getLayout(self.layout.data.list);
            const alignment_u32: u32 = @intCast(elem_layout.alignment(layout_cache.targetUsize()).toByteUnits());
            const element_width: usize = @intCast(layout_cache.layoutSize(elem_layout));
            const elements_refcounted = layout_cache.layoutContainsRefcounted(elem_layout);

            if (comptime trace_refcount) {
                traceRefcount("DECREF list ptr=0x{x} len={} elems_rc={} unique={}", .{
                    @intFromPtr(list_value.getAllocationDataPtr()),
                    list_value.len(),
                    @intFromBool(elements_refcounted),
                    @intFromBool(list_value.isUnique(ops)),
                });
            }

            // Always decref elements when unique, not just when isRefcounted().
            // Records/tuples containing refcounted values also need their fields decreffed.
            // Decref for non-refcounted types (like plain integers) is a no-op.
            if (list_value.isUnique(ops)) {
                if (list_value.getAllocationDataPtr(ops)) |source| {
                    const count = list_value.getAllocationElementCount(elements_refcounted, ops);

                    if (comptime trace_refcount) {
                        traceRefcount("DECREF list decref-ing {} elements", .{count});
                    }

                    var idx: usize = 0;
                    while (idx < count) : (idx += 1) {
                        const elem_ptr = source + idx * element_width;
                        decrefLayoutPtr(elem_layout, @ptrCast(elem_ptr), layout_cache, ops);
                    }
                }
            }
            // We already decreffed all elements above, so pass rcNone to avoid double-decref.
            // But we still need elements_refcounted=true for correct allocation layout.
            list_value.decref(alignment_u32, element_width, elements_refcounted, null, &builtins.list.rcNone, ops);
            return;
        },
        .list_of_zst => {
            const list_header = self.asRocList() orelse return;
            const list_value = list_header.*;

            const alignment_u32: u32 = @intCast(layout_cache.targetUsize().size());
            list_value.decref(alignment_u32, 0, false, null, &builtins.list.rcNone, ops);
            return;
        },
        .box => {
            const slot = self.asBoxSlot() orelse return;
            const raw_ptr = slot.*;
            if (raw_ptr == 0) return;
            const data_ptr: [*]u8 = @ptrFromInt(raw_ptr);
            const target_usize = layout_cache.targetUsize();
            const elem_layout = layout_cache.getLayout(self.layout.data.box);
            const elem_alignment: u32 = @intCast(elem_layout.alignment(target_usize).toByteUnits());

            const ptr_int = @intFromPtr(data_ptr);
            const tag_mask: usize = if (@sizeOf(usize) == 8) 0b111 else 0b11;
            const unmasked_ptr = ptr_int & ~tag_mask;
            const payload_ptr = @as([*]u8, @ptrFromInt(unmasked_ptr));
            const refcount_ptr: *isize = @as(*isize, @ptrFromInt(unmasked_ptr - @sizeOf(isize)));

            if (comptime trace_refcount) {
                traceRefcount("DECREF box ptr=0x{x} rc={} elem_rc={}", .{
                    unmasked_ptr,
                    refcount_ptr.*,
                    @intFromBool(layout_cache.layoutContainsRefcounted(elem_layout)),
                });
            }

            if (builtins.utils.rcUnique(refcount_ptr.*)) {
                if (layout_cache.layoutContainsRefcounted(elem_layout)) {
                    decrefLayoutPtr(elem_layout, @ptrCast(@alignCast(payload_ptr)), layout_cache, ops);
                }
            }

            builtins.utils.decrefDataPtrC(@as(?[*]u8, payload_ptr), elem_alignment, false, ops);
            slot.* = 0;
            return;
        },
        .record => {
            if (self.ptr == null) return;
            const record_data = layout_cache.getRecordData(self.layout.data.record.idx);
            if (record_data.fields.count == 0) return;

            if (comptime trace_refcount) {
                traceRefcount("DECREF record ptr=0x{x} fields={}", .{
                    @intFromPtr(self.ptr),
                    record_data.fields.count,
                });
            }

            decrefLayoutPtr(self.layout, self.ptr, layout_cache, ops);
            return;
        },
        .box_of_zst => {
            if (self.ptr != null) {
                self.clearBoxSlot();
            }
            return;
        },
        .tuple => {
            if (self.ptr == null) return;
            const tuple_data = layout_cache.getTupleData(self.layout.data.tuple.idx);
            if (tuple_data.fields.count == 0) return;

            if (comptime trace_refcount) {
                traceRefcount("DECREF tuple ptr=0x{x} fields={}", .{
                    @intFromPtr(self.ptr),
                    tuple_data.fields.count,
                });
            }

            decrefLayoutPtr(self.layout, self.ptr, layout_cache, ops);
            return;
        },
        .closure => {
            decrefLayoutPtr(self.layout, self.ptr, layout_cache, ops);
            if (comptime trace_refcount) {
                traceRefcount("DECREF closure DONE ptr=0x{x}", .{@intFromPtr(self.ptr)});
            }
            return;
        },
        .tag_union => {
            if (self.ptr == null) return;
            const base_ptr = @as([*]const u8, @ptrCast(self.ptr.?));
            const discriminant = readTagUnionDiscriminant(self.layout, base_ptr, layout_cache);
            const tu_data = layout_cache.getTagUnionData(self.layout.data.tag_union.idx);
            const variants = layout_cache.getTagUnionVariants(tu_data);
            const variant_layout = layout_cache.getLayout(variants.get(discriminant).payload_layout);

            if (comptime trace_refcount) {
                traceRefcount("DECREF tag_union ptr=0x{x} disc={} variant_layout.tag={}", .{
                    @intFromPtr(self.ptr),
                    discriminant,
                    @intFromEnum(variant_layout.tag),
                });
            }

            decrefLayoutPtr(variant_layout, @as(*anyopaque, @ptrCast(@constCast(base_ptr))), layout_cache, ops);
            return;
        },
        else => {},
    }

    // Non-refcounted values require no action
}

/// Calculate total memory footprint for a value.
///
/// - For closures, this includes both the Closure header and captured data
/// - For all other types, this is just the layout size
pub fn getTotalSize(self: StackValue, layout_cache: *LayoutStore, _: *RocOps) u32 {
    if (self.layout.tag == .closure and self.ptr != null) {
        const closure = self.asClosure().?;

        // Debug assertion: closure layout index must be within bounds.
        // If this trips, it indicates a compiler bug in layout index assignment.
        const idx_as_usize = @intFromEnum(closure.captures_layout_idx);
        std.debug.assert(idx_as_usize < layout_cache.layouts.len());

        const captures_layout = layout_cache.getLayout(closure.captures_layout_idx);
        const captures_alignment = captures_layout.alignment(layout_cache.targetUsize());
        const header_size = @sizeOf(Closure);
        const aligned_captures_offset = std.mem.alignForward(u32, header_size, @intCast(captures_alignment.toByteUnits()));
        const captures_size = layout_cache.layoutSize(captures_layout);
        return aligned_captures_offset + captures_size;
    } else {
        return layout_cache.layoutSize(self.layout);
    }
}
