//! Represents a "value" on the Interpreter's stack.
//!
//! This is the public facing interface for interacting with stack values.
//!
//! It provides methods for working with the value safely using the layout.

const std = @import("std");
const builtin = @import("builtin");
const base = @import("base");
const types = @import("types");
const can = @import("can");
const builtins = @import("builtins");
const collections = @import("collections");
const layout_mod = @import("layout");

const CIR = can.CIR;
const ModuleEnv = can.ModuleEnv;
const LayoutStore = layout_mod.Store;
const Layout = layout_mod.Layout;
const StringLiteral = base.StringLiteral;
const RocOps = builtins.host_abi.RocOps;
const RocList = builtins.list.RocList;
const RocStr = builtins.str.RocStr;
const LayoutTag = layout_mod.LayoutTag;
const RocDec = builtins.dec.RocDec;
const SExprTree = base.SExprTree;
const Closure = layout_mod.Closure;
const Expr = CIR.Expr;
const Ident = base.Ident;

// RocList.decref always requires a callback for element teardown. When we've
// already decref'd the elements ourselves (or they don't need refcounting), we
// pass this no-op stub so the list header can clean up without touching the
// payload again.
fn noopDecref(_: ?[*]u8) callconv(.C) void {}

const StackValue = @This();

/// Type and memory layout information for the result value
layout: Layout,
/// Ptr to the actual value in stack memory
ptr: ?*anyopaque,
/// Flag to track whether the memory has been initialized
is_initialized: bool = false,

/// Copy this stack value to a destination pointer with bounds checking
pub fn copyToPtr(self: StackValue, layout_cache: *LayoutStore, dest_ptr: *anyopaque, ops: *RocOps) !void {
    std.debug.assert(self.is_initialized); // Source must be initialized before copying
    if (self.ptr == null) {
        return error.NullStackPointer;
    }

    // For closures, use getTotalSize to include capture data; for others use layoutSize
    const result_size = if (self.layout.tag == .closure) self.getTotalSize(layout_cache) else layout_cache.layoutSize(self.layout);
    if (result_size == 0) {
        return;
    }

    if (self.layout.tag == .scalar) {
        switch (self.layout.data.scalar.tag) {
            .str => {
                // Clone the RocStr into the interpreter's heap
                std.debug.assert(self.ptr != null);
                const src_str: *const RocStr = @ptrCast(@alignCast(self.ptr.?));
                const dest_str: *RocStr = @ptrCast(@alignCast(dest_ptr));
                dest_str.* = src_str.clone(ops);
                return;
            },
            .int => {
                // Use type-specific integer copying with precision
                std.debug.assert(self.ptr != null);
                const precision = self.layout.data.scalar.data.int;
                const value = self.asI128();
                switch (precision) {
                    .u8 => {
                        const typed_ptr: *u8 = @ptrCast(@alignCast(dest_ptr));
                        typed_ptr.* = @intCast(value);
                    },
                    .u16 => {
                        const typed_ptr: *u16 = @ptrCast(@alignCast(dest_ptr));
                        typed_ptr.* = @intCast(value);
                    },
                    .u32 => {
                        const typed_ptr: *u32 = @ptrCast(@alignCast(dest_ptr));
                        typed_ptr.* = @intCast(value);
                    },
                    .u64 => {
                        const typed_ptr: *u64 = @ptrCast(@alignCast(dest_ptr));
                        typed_ptr.* = @intCast(value);
                    },
                    .u128 => {
                        const typed_ptr: *u128 = @ptrCast(@alignCast(dest_ptr));
                        typed_ptr.* = @intCast(value);
                    },
                    .i8 => {
                        const typed_ptr: *i8 = @ptrCast(@alignCast(dest_ptr));
                        typed_ptr.* = @intCast(value);
                    },
                    .i16 => {
                        const typed_ptr: *i16 = @ptrCast(@alignCast(dest_ptr));
                        typed_ptr.* = @intCast(value);
                    },
                    .i32 => {
                        const typed_ptr: *i32 = @ptrCast(@alignCast(dest_ptr));
                        typed_ptr.* = @intCast(value);
                    },
                    .i64 => {
                        const typed_ptr: *i64 = @ptrCast(@alignCast(dest_ptr));
                        typed_ptr.* = @intCast(value);
                    },
                    .i128 => {
                        const typed_ptr: *i128 = @ptrCast(@alignCast(dest_ptr));
                        typed_ptr.* = value;
                    },
                }
                return;
            },
            else => {},
        }
    }

    if (self.layout.tag == .box) {
        const src_slot: *usize = @ptrCast(@alignCast(self.ptr.?));
        const dest_slot: *usize = @ptrCast(@alignCast(dest_ptr));
        dest_slot.* = src_slot.*;
        if (dest_slot.* != 0) {
            const data_ptr: [*]u8 = @as([*]u8, @ptrFromInt(dest_slot.*));
            builtins.utils.increfDataPtrC(@as(?[*]u8, data_ptr), 1);
        }
        return;
    }

    if (self.layout.tag == .box_of_zst) {
        const dest_slot: *usize = @ptrCast(@alignCast(dest_ptr));
        dest_slot.* = 0;
        return;
    }

    std.debug.assert(self.ptr != null);
    const src = @as([*]u8, @ptrCast(self.ptr.?))[0..result_size];
    const dst = @as([*]u8, @ptrCast(dest_ptr))[0..result_size];
    @memcpy(dst, src);
}

/// Read this StackValue's integer value, ensuring it's initialized
pub fn asI128(self: StackValue) i128 {
    std.debug.assert(self.is_initialized); // Ensure initialized before reading
    std.debug.assert(self.ptr != null);
    std.debug.assert(self.layout.tag == .scalar and self.layout.data.scalar.tag == .int);

    const precision = self.layout.data.scalar.data.int;
    return switch (precision) {
        .u8 => blk: {
            const typed_ptr = @as(*const u8, @ptrCast(@alignCast(self.ptr.?)));
            break :blk @as(i128, typed_ptr.*);
        },
        .u16 => blk: {
            const typed_ptr = @as(*const u16, @ptrCast(@alignCast(self.ptr.?)));
            break :blk @as(i128, typed_ptr.*);
        },
        .u32 => blk: {
            const typed_ptr = @as(*const u32, @ptrCast(@alignCast(self.ptr.?)));
            break :blk @as(i128, typed_ptr.*);
        },
        .u64 => blk: {
            const typed_ptr = @as(*const u64, @ptrCast(@alignCast(self.ptr.?)));
            break :blk @as(i128, typed_ptr.*);
        },
        .u128 => blk: {
            const typed_ptr = @as(*const u128, @ptrCast(@alignCast(self.ptr.?)));
            break :blk @as(i128, @intCast(typed_ptr.*));
        },
        .i8 => blk: {
            const typed_ptr = @as(*const i8, @ptrCast(@alignCast(self.ptr.?)));
            break :blk @as(i128, typed_ptr.*);
        },
        .i16 => blk: {
            const typed_ptr = @as(*const i16, @ptrCast(@alignCast(self.ptr.?)));
            break :blk @as(i128, typed_ptr.*);
        },
        .i32 => blk: {
            const typed_ptr = @as(*const i32, @ptrCast(@alignCast(self.ptr.?)));
            break :blk @as(i128, typed_ptr.*);
        },
        .i64 => blk: {
            const typed_ptr = @as(*const i64, @ptrCast(@alignCast(self.ptr.?)));
            break :blk @as(i128, typed_ptr.*);
        },
        .i128 => blk: {
            const typed_ptr = @as(*const i128, @ptrCast(@alignCast(self.ptr.?)));
            break :blk typed_ptr.*;
        },
    };
}

/// Initialise the StackValue integer value
pub fn setInt(self: *StackValue, value: i128) void {

    // Assert this is pointing to a valid memory location
    std.debug.assert(self.ptr != null);

    // Assert this is an integer
    std.debug.assert(self.layout.tag == .scalar and self.layout.data.scalar.tag == .int);

    // Assert this is uninitialised memory
    //
    // Avoid accidental overwrite, manually toggle this if updating an already initialized value
    std.debug.assert(!self.is_initialized);

    const precision = self.layout.data.scalar.data.int;

    // Inline integer writing logic with proper type casting and alignment
    switch (precision) {
        .u8 => {
            const typed_ptr: *u8 = @ptrCast(@alignCast(self.ptr.?));
            typed_ptr.* = @intCast(value);
        },
        .u16 => {
            const typed_ptr: *u16 = @ptrCast(@alignCast(self.ptr.?));
            typed_ptr.* = @intCast(value);
        },
        .u32 => {
            const typed_ptr: *u32 = @ptrCast(@alignCast(self.ptr.?));
            typed_ptr.* = @intCast(value);
        },
        .u64 => {
            const typed_ptr: *u64 = @ptrCast(@alignCast(self.ptr.?));
            typed_ptr.* = @intCast(value);
        },
        .u128 => {
            const typed_ptr: *u128 = @ptrCast(@alignCast(self.ptr.?));
            typed_ptr.* = @intCast(value);
        },
        .i8 => {
            const typed_ptr: *i8 = @ptrCast(@alignCast(self.ptr.?));
            typed_ptr.* = @intCast(value);
        },
        .i16 => {
            const typed_ptr: *i16 = @ptrCast(@alignCast(self.ptr.?));
            typed_ptr.* = @intCast(value);
        },
        .i32 => {
            const typed_ptr: *i32 = @ptrCast(@alignCast(self.ptr.?));
            typed_ptr.* = @intCast(value);
        },
        .i64 => {
            const typed_ptr: *i64 = @ptrCast(@alignCast(self.ptr.?));
            typed_ptr.* = @intCast(value);
        },
        .i128 => {
            const typed_ptr: *i128 = @ptrCast(@alignCast(self.ptr.?));
            typed_ptr.* = value;
        },
    }
}

/// Initialise the StackValue boolean value
pub fn setBool(self: *StackValue, value: u8) void {
    // Assert this is pointing to a valid memory location
    std.debug.assert(self.ptr != null);

    // Assert this is a boolean
    std.debug.assert(self.layout.tag == .scalar and self.layout.data.scalar.tag == .bool);

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
    std.debug.assert(self.layout.tag == .scalar and self.layout.data.scalar.tag == .bool);

    // Read the boolean value as a byte
    const bool_ptr = @as(*const u8, @ptrCast(@alignCast(self.ptr.?)));
    return bool_ptr.* != 0;
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

    /// Get a StackValue for the element at the given index
    pub fn getElement(self: TupleAccessor, index: usize) !StackValue {
        if (index >= self.element_layouts.len) {
            return error.TupleIndexOutOfBounds;
        }

        std.debug.assert(self.base_value.is_initialized);
        std.debug.assert(self.base_value.ptr != null);

        const element_layout_info = self.element_layouts.get(index);
        const element_layout = self.layout_cache.getLayout(element_layout_info.layout);

        // Get the offset for this element within the tuple
        const element_offset = self.layout_cache.getTupleElementOffset(self.tuple_layout.data.tuple.idx, @intCast(index));

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
        };
    }

    /// Set an element by copying from a source StackValue
    pub fn setElement(self: TupleAccessor, index: usize, source: StackValue, ops: *RocOps) !void {
        const dest_element = try self.getElement(index);
        try source.copyToPtr(self.layout_cache, dest_element.ptr.?, ops);
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

/// Create a ListAccessor for safe list element access
pub fn asList(self: StackValue, layout_cache: *LayoutStore, element_layout: Layout) !ListAccessor {
    std.debug.assert(self.is_initialized);
    std.debug.assert(self.ptr != null);
    std.debug.assert(self.layout.tag == .list or self.layout.tag == .list_of_zst);

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

    pub fn getElement(self: ListAccessor, index: usize) !StackValue {
        if (index >= self.list.len()) return error.ListIndexOutOfBounds;

        if (self.element_size == 0) {
            return StackValue{ .layout = self.element_layout, .ptr = null, .is_initialized = true };
        }

        const base_ptr = self.list.bytes orelse return error.NullStackPointer;
        const offset = index * self.element_size;
        return StackValue{
            .layout = self.element_layout,
            .ptr = @ptrCast(base_ptr + offset),
            .is_initialized = true,
        };
    }
};

fn storeListElementCount(list: *RocList, elements_refcounted: bool) void {
    if (elements_refcounted and !list.isSeamlessSlice()) {
        if (list.getAllocationDataPtr()) |source| {
            const ptr = @as([*]usize, @alignCast(@ptrCast(source))) - 2;
            ptr[0] = list.length;
        }
    }
}

fn copyListValueToPtr(
    src: StackValue,
    layout_cache: *LayoutStore,
    dest_ptr: *anyopaque,
    dest_layout: Layout,
) error{ TypeMismatch, NullStackPointer }!void {
    var dest_list: *RocList = @ptrCast(@alignCast(dest_ptr));

    switch (dest_layout.tag) {
        .list_of_zst => {
            if (src.layout.tag != .list_of_zst) return error.TypeMismatch;
            if (src.ptr == null) {
                dest_list.* = RocList.empty();
                return;
            }
            const src_list = @as(*const RocList, @ptrCast(@alignCast(src.ptr.?))).*;
            dest_list.* = src_list;
            dest_list.incref(1, false);
            return;
        },
        .list => {
            if (src.ptr == null) {
                dest_list.* = RocList.empty();
                return;
            }
            if (src.layout.tag != .list) return error.TypeMismatch;
            const src_list = @as(*const RocList, @ptrCast(@alignCast(src.ptr.?))).*;
            dest_list.* = src_list;

            const elem_layout = layout_cache.getLayout(dest_layout.data.list);
            const elements_refcounted = elem_layout.isRefcounted();
            dest_list.incref(1, elements_refcounted);
            storeListElementCount(dest_list, elements_refcounted);
            return;
        },
        else => return error.TypeMismatch,
    }
}

/// Create a RecordAccessor for safe record field access
pub fn asRecord(self: StackValue, layout_cache: *LayoutStore) !RecordAccessor {
    std.debug.assert(self.is_initialized); // Record must be initialized before accessing
    std.debug.assert(self.ptr != null);
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
    pub fn getFieldByIndex(self: RecordAccessor, index: usize) !StackValue {
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
        };
    }

    /// Get a StackValue for the field with the given name
    pub fn getFieldByName(self: RecordAccessor, field_name: []const u8) !?StackValue {
        // Find the field index by name
        const field_offset = self.layout_cache.getRecordFieldOffsetByName(
            self.record_layout.data.record.idx,
            field_name,
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
        };
    }

    /// Set a field by copying from a source StackValue
    pub fn setFieldByIndex(self: RecordAccessor, index: usize, source: StackValue, ops: *RocOps) !void {
        const dest_field = try self.getFieldByIndex(index);
        try source.copyToPtr(self.layout_cache, dest_field.ptr.?, ops);
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

    /// Find field index by comparing field names (requires env access for name comparison)
    pub fn findFieldIndex(self: RecordAccessor, env: anytype, field_name: []const u8) ?usize {
        for (0..self.field_layouts.len) |idx| {
            const field = self.field_layouts.get(idx);
            if (std.mem.eql(u8, env.getIdent(field.name), field_name)) {
                return idx;
            }
        }
        return null;
    }
};

/// Get this value as a string pointer
pub fn asRocStr(self: StackValue) *RocStr {
    std.debug.assert(self.layout.tag == .scalar and self.layout.data.scalar.tag == .str);
    return @ptrCast(@alignCast(self.ptr.?));
}

/// Get this value as a closure pointer
pub fn asClosure(self: StackValue) *const Closure {
    std.debug.assert(self.layout.tag == .closure);
    std.debug.assert(self.ptr != null);
    return @ptrCast(@alignCast(self.ptr.?));
}

/// Get the payload pointer stored inside a Box value (if any)
pub fn boxDataPointer(self: StackValue) ?[*]u8 {
    std.debug.assert(self.layout.tag == .box or self.layout.tag == .box_of_zst);
    if (self.ptr == null) return null;
    const slot: *usize = @ptrCast(@alignCast(self.ptr.?));
    if (slot.* == 0) return null;
    return @as([*]u8, @ptrFromInt(slot.*));
}

/// Move this value to binding (transfers ownership, no refcounts change)
pub fn moveForBinding(self: StackValue) StackValue {
    return self;
}

/// Copy value data to another StackValue (with special string handling)
pub fn copyTo(self: StackValue, dest: StackValue, layout_cache: *LayoutStore) void {
    std.debug.assert(self.is_initialized);
    std.debug.assert(dest.ptr != null);

    // For closures, use getTotalSize to include capture data; for others use layoutSize
    const size = if (self.layout.tag == .closure) self.getTotalSize(layout_cache) else layout_cache.layoutSize(self.layout);
    if (size == 0) return;

    if (self.layout.tag == .scalar and self.layout.data.scalar.tag == .str) {
        // String: use proper struct copy and increment ref count
        const src_str: *const RocStr = @ptrCast(@alignCast(self.ptr.?));
        const dest_str: *RocStr = @ptrCast(@alignCast(dest.ptr.?));
        dest_str.* = src_str.*;
        dest_str.incref(1);
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
            const elements_refcounted = elem_layout.isRefcounted();
            dest_list.incref(1, elements_refcounted);
            storeListElementCount(dest_list, elements_refcounted);
        } else {
            dest_list.incref(1, false);
        }
        return;
    }

    if (self.layout.tag == .box) {
        const src_slot: *usize = @ptrCast(@alignCast(self.ptr.?));
        const dest_slot: *usize = @ptrCast(@alignCast(dest.ptr.?));
        dest_slot.* = src_slot.*;
        if (dest_slot.* != 0) {
            const data_ptr: [*]u8 = @as([*]u8, @ptrFromInt(dest_slot.*));
            builtins.utils.increfDataPtrC(@as(?[*]u8, data_ptr), 1);
        }
        return;
    }

    if (self.layout.tag == .box_of_zst) {
        const dest_slot: *usize = @ptrCast(@alignCast(dest.ptr.?));
        dest_slot.* = 0;
        return;
    }

    // Everything else just copy the bytes
    std.mem.copyForwards(
        u8,
        @as([*]u8, @ptrCast(dest.ptr.?))[0..size],
        @as([*]const u8, @ptrCast(self.ptr.?))[0..size],
    );
}

/// Create a StackValue view of a memory region (no copy)
pub fn fromPtr(layout: Layout, ptr: *anyopaque) StackValue {
    return StackValue{
        .layout = layout,
        .ptr = ptr,
        .is_initialized = true,
    };
}

/// Copy value data to another StackValue WITHOUT incrementing refcounts (move semantics)
pub fn copyWithoutRefcount(self: StackValue, dest: StackValue, layout_cache: *LayoutStore) void {
    std.debug.assert(self.is_initialized);
    std.debug.assert(dest.ptr != null);

    // For closures, use getTotalSize to include capture data; for others use layoutSize
    const size = if (self.layout.tag == .closure) self.getTotalSize(layout_cache) else layout_cache.layoutSize(self.layout);
    if (size == 0) return;

    if (self.layout.tag == .scalar and self.layout.data.scalar.tag == .str) {
        // String: use proper struct copy WITHOUT incrementing ref count (move semantics)
        const src_str: *const RocStr = @ptrCast(@alignCast(self.ptr.?));
        const dest_str: *RocStr = @ptrCast(@alignCast(dest.ptr.?));
        dest_str.* = src_str.*; // Just copy the struct, no refcount change
    } else {
        if (self.layout.tag == .box or self.layout.tag == .box_of_zst) {
            const src_slot: *usize = @ptrCast(@alignCast(self.ptr.?));
            const dest_slot: *usize = @ptrCast(@alignCast(dest.ptr.?));
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

/// Increment reference count for refcounted types
pub fn incref(self: StackValue) void {
    if (self.layout.tag == .scalar and self.layout.data.scalar.tag == .str) {
        const roc_str = self.asRocStr();
        roc_str.incref(1);
        return;
    }
    if (self.layout.tag == .list) {
        if (self.ptr == null) return;
        const list_value = @as(*const RocList, @ptrCast(@alignCast(self.ptr.?))).*;
        // We don't know element layout here to store counts; assume caller already handled
        list_value.incref(1, false);
        return;
    }
    if (self.layout.tag == .box) {
        if (self.ptr == null) return;
        const slot: *usize = @ptrCast(@alignCast(self.ptr.?));
        if (slot.* != 0) {
            const data_ptr: [*]u8 = @as([*]u8, @ptrFromInt(slot.*));
            builtins.utils.increfDataPtrC(@as(?[*]u8, data_ptr), 1);
        }
        return;
    }
}

/// Decrement reference count for refcounted types
pub fn decref(self: StackValue, layout_cache: *LayoutStore, ops: *RocOps) void {
    switch (self.layout.tag) {
        .scalar => switch (self.layout.data.scalar.tag) {
            .str => {
                const roc_str = self.asRocStr();
                roc_str.decref(ops);
                return;
            },
            else => {},
        },
        .list => {
            if (self.ptr == null) return;
            const list_header: *const RocList = @ptrCast(@alignCast(self.ptr.?));
            const list_value = list_header.*;
            const elem_layout = layout_cache.getLayout(self.layout.data.list);
            const alignment_u32: u32 = @intCast(elem_layout.alignment(layout_cache.targetUsize()).toByteUnits());
            const element_width: usize = @intCast(layout_cache.layoutSize(elem_layout));
            const elements_refcounted = elem_layout.isRefcounted();
            if (elements_refcounted) {
                if (list_value.isUnique()) {
                    if (list_value.getAllocationDataPtr()) |source| {
                        const count: usize = if (list_value.isSeamlessSlice()) blk: {
                            const ptr = @as([*]usize, @alignCast(@ptrCast(source))) - 2;
                            break :blk ptr[0];
                        } else list_value.len();

                        var idx: usize = 0;
                        while (idx < count) : (idx += 1) {
                            const elem_ptr = source + idx * element_width;
                            const elem_value = StackValue{
                                .layout = elem_layout,
                                .ptr = @ptrCast(elem_ptr),
                                .is_initialized = true,
                            };
                            elem_value.decref(layout_cache, ops);
                        }
                    }
                }
                list_value.decref(alignment_u32, element_width, false, noopDecref, ops);
                return;
            }
            list_value.decref(alignment_u32, element_width, false, noopDecref, ops);
            return;
        },
        .list_of_zst => {
            if (self.ptr == null) return;
            const list_header: *const RocList = @ptrCast(@alignCast(self.ptr.?));
            const list_value = list_header.*;
            const alignment_u32: u32 = @intCast(layout_cache.targetUsize().size());
            list_value.decref(alignment_u32, 0, false, noopDecref, ops);
            return;
        },
        .box => {
            if (self.ptr == null) return;
            const slot: *usize = @ptrCast(@alignCast(self.ptr.?));
            const raw_ptr = slot.*;
            if (raw_ptr == 0) return;
            const data_ptr = @as([*]u8, @ptrFromInt(raw_ptr));
            const target_usize = layout_cache.targetUsize();
            const elem_layout = layout_cache.getLayout(self.layout.data.box);
            const elem_alignment: u32 = @intCast(elem_layout.alignment(target_usize).toByteUnits());

            const ptr_int = @intFromPtr(data_ptr);
            const tag_mask: usize = if (@sizeOf(usize) == 8) 0b111 else 0b11;
            const unmasked_ptr = ptr_int & ~tag_mask;
            const payload_ptr = @as([*]u8, @ptrFromInt(unmasked_ptr));
            const refcount_ptr: *isize = @as(*isize, @ptrFromInt(unmasked_ptr - @sizeOf(isize)));

            if (builtins.utils.rcUnique(refcount_ptr.*)) {
                if (elem_layout.isRefcounted()) {
                    const payload_value = StackValue{
                        .layout = elem_layout,
                        .ptr = @ptrCast(@alignCast(payload_ptr)),
                        .is_initialized = true,
                    };
                    payload_value.decref(layout_cache, ops);
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

            const field_layouts = layout_cache.record_fields.sliceRange(record_data.getFields());
            const base_ptr = @as([*]u8, @ptrCast(self.ptr.?));

            var field_index: usize = 0;
            while (field_index < field_layouts.len) : (field_index += 1) {
                const field_info = field_layouts.get(field_index);
                const field_layout = layout_cache.getLayout(field_info.layout);

                const field_offset = layout_cache.getRecordFieldOffset(self.layout.data.record.idx, @intCast(field_index));
                const field_ptr = @as(*anyopaque, @ptrCast(base_ptr + field_offset));

                const field_value = StackValue{
                    .layout = field_layout,
                    .ptr = field_ptr,
                    .is_initialized = true,
                };

                field_value.decref(layout_cache, ops);
            }

            return;
        },
        .box_of_zst => {
            if (self.ptr == null) return;
            const slot: *usize = @ptrCast(@alignCast(self.ptr.?));
            slot.* = 0;
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
pub fn getTotalSize(self: StackValue, layout_cache: *LayoutStore) u32 {
    if (self.layout.tag == .closure and self.ptr != null) {
        const closure = self.asClosure();
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
