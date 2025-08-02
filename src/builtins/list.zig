//! Builtin list operations and data structures for the Roc runtime.
//!
//! This module provides the core implementation of Roc's List type, including
//! operations for creation, manipulation, sorting, and memory management.
//! It defines the RocList structure and associated functions that are called
//! from compiled Roc code to handle list operations efficiently.
const std = @import("std");
const builtins = @import("builtins");

const UpdateMode = builtins.utils.UpdateMode;
const RocOps = builtins.host_abi.RocOps;
const RocStr = builtins.str.RocStr;
const increfDataPtrC = builtins.utils.increfDataPtrC;

const Opaque = ?[*]u8;
const EqFn = *const fn (Opaque, Opaque) callconv(.C) bool;
const CompareFn = *const fn (Opaque, Opaque, Opaque) callconv(.C) u8;
const CopyFn = *const fn (Opaque, Opaque) callconv(.C) void;

const Inc = *const fn (?[*]u8) callconv(.C) void;
const IncN = *const fn (?[*]u8, usize) callconv(.C) void;
const Dec = *const fn (?[*]u8) callconv(.C) void;
const HasTagId = *const fn (u16, ?[*]u8) callconv(.C) extern struct { matched: bool, data: ?[*]u8 };

pub const SEAMLESS_SLICE_BIT: usize =
    @as(usize, @bitCast(@as(isize, std.math.minInt(isize))));

/// TODO: Document the RocList struct.
pub const RocList = extern struct {
    bytes: ?[*]u8,
    length: usize,
    // For normal lists, contains the capacity.
    // For seamless slices contains the pointer to the original allocation.
    // This pointer is to the first element of the original list.
    // Note we storing an allocation pointer, the pointer must be right shifted by one.
    capacity_or_alloc_ptr: usize,

    /// Returns the number of elements in the list.
    pub inline fn len(self: RocList) usize {
        return self.length;
    }

    /// Returns the total capacity of the list.
    pub fn getCapacity(self: RocList) usize {
        const list_capacity = self.capacity_or_alloc_ptr;
        const slice_capacity = self.length;
        const slice_mask = self.seamlessSliceMask();
        const capacity = (list_capacity & ~slice_mask) | (slice_capacity & slice_mask);
        return capacity;
    }

    /// Returns true if this list is a seamless slice.
    pub fn isSeamlessSlice(self: RocList) bool {
        return @as(isize, @bitCast(self.capacity_or_alloc_ptr)) < 0;
    }

    // This returns all ones if the list is a seamless slice.
    // Otherwise, it returns all zeros.
    // This is done without branching for optimization purposes.
    pub fn seamlessSliceMask(self: RocList) usize {
        return @as(usize, @bitCast(@as(isize, @bitCast(self.capacity_or_alloc_ptr)) >> (@bitSizeOf(isize) - 1)));
    }

    pub fn isEmpty(self: RocList) bool {
        return self.len() == 0;
    }

    pub fn empty() RocList {
        return RocList{ .bytes = null, .length = 0, .capacity_or_alloc_ptr = 0 };
    }

    pub fn eql(self: RocList, other: RocList) bool {
        if (self.len() != other.len()) {
            return false;
        }

        // Their lengths are the same, and one is empty; they're both empty!
        if (self.isEmpty()) {
            return true;
        }

        var index: usize = 0;
        const self_bytes = self.bytes orelse unreachable;
        const other_bytes = other.bytes orelse unreachable;

        while (index < self.len()) {
            if (self_bytes[index] != other_bytes[index]) {
                return false;
            }

            index += 1;
        }

        return true;
    }

    pub fn fromSlice(
        comptime T: type,
        slice: []const T,
        elements_refcounted: bool,
        roc_ops: *RocOps,
    ) RocList {
        if (slice.len == 0) {
            return RocList.empty();
        }

        const list = list_allocate(@alignOf(T), slice.len, @sizeOf(T), elements_refcounted, roc_ops);

        if (slice.len > 0) {
            const dest = list.bytes orelse unreachable;
            const src = @as([*]const u8, @ptrCast(slice.ptr));
            const num_bytes = slice.len * @sizeOf(T);

            @memcpy(dest[0..num_bytes], src[0..num_bytes]);
        }

        return list;
    }

    // returns a pointer to the original allocation.
    // This pointer points to the first element of the allocation.
    // The pointer is to just after the refcount.
    // For big lists, it just returns their bytes pointer.
    // For seamless slices, it returns the pointer stored in capacity_or_alloc_ptr.
    pub fn getAllocationDataPtr(self: RocList) ?[*]u8 {
        const list_alloc_ptr = @intFromPtr(self.bytes);
        const slice_alloc_ptr = self.capacity_or_alloc_ptr << 1;
        const slice_mask = self.seamlessSliceMask();
        const alloc_ptr = (list_alloc_ptr & ~slice_mask) | (slice_alloc_ptr & slice_mask);
        return @as(?[*]u8, @ptrFromInt(alloc_ptr));
    }

    // This function is only valid if the list has refcounted elements.
    fn getAllocationElementCount(self: RocList) usize {
        if (self.isSeamlessSlice()) {
            // Seamless slices always refer to an underlying allocation.
            const alloc_ptr = self.getAllocationDataPtr() orelse unreachable;
            // - 1 is refcount.
            // - 2 is size on heap.
            const ptr = @as([*]usize, @ptrCast(@alignCast(alloc_ptr))) - 2;
            return ptr[0];
        } else {
            return self.length;
        }
    }

    // This needs to be called when creating seamless slices from unique list.
    // It will put the allocation size on the heap to enable the seamless slice to free the underlying allocation.
    fn setAllocationElementCount(self: RocList, elements_refcounted: bool) void {
        if (elements_refcounted and !self.isSeamlessSlice()) {
            // - 1 is refcount.
            // - 2 is size on heap.
            const ptr = @as([*]usize, @alignCast(@ptrCast(self.getAllocationDataPtr()))) - 2;
            ptr[0] = self.length;
        }
    }

    pub fn incref(self: RocList, amount: isize, elements_refcounted: bool) void {
        // If the list is unique and not a seamless slice, the length needs to be store on the heap if the elements are refcounted.
        if (elements_refcounted and self.isUnique() and !self.isSeamlessSlice()) {
            if (self.getAllocationDataPtr()) |source| {
                // - 1 is refcount.
                // - 2 is size on heap.
                const ptr = @as([*]usize, @alignCast(@ptrCast(source))) - 2;
                ptr[0] = self.length;
            }
        }
        increfDataPtrC(self.getAllocationDataPtr(), amount);
    }

    pub fn decref(
        self: RocList,
        alignment: u32,
        element_width: usize,
        elements_refcounted: bool,
        dec: Dec,
        roc_ops: *RocOps,
    ) void {
        // If unique, decref will free the list. Before that happens, all elements must be decremented.
        if (elements_refcounted and self.isUnique()) {
            if (self.getAllocationDataPtr()) |source| {
                const count = self.getAllocationElementCount();

                var i: usize = 0;
                while (i < count) : (i += 1) {
                    const element = source + i * element_width;
                    dec(element);
                }
            }
        }

        // We use the raw capacity to ensure we always decrement the refcount of seamless slices.
        builtins.utils.decref(
            self.getAllocationDataPtr(),
            self.capacity_or_alloc_ptr,
            alignment,
            elements_refcounted,
            roc_ops,
        );
    }

    pub fn elements(self: RocList, comptime T: type) ?[*]T {
        return @as(?[*]T, @ptrCast(@alignCast(self.bytes)));
    }

    pub fn isUnique(self: RocList) bool {
        return builtins.utils.rcUnique(@bitCast(self.refcount()));
    }

    fn refcount(self: RocList) usize {
        if (self.getCapacity() == 0 and !self.isSeamlessSlice()) {
            // the zero-capacity is Clone, copying it will not leak memory
            return 1;
        }

        const ptr: [*]usize = @as([*]usize, @ptrCast(@alignCast(self.getAllocationDataPtr())));
        return (ptr - 1)[0];
    }

    pub fn makeUnique(
        self: RocList,
        alignment: u32,
        element_width: usize,
        elements_refcounted: bool,
        inc: Inc,
        dec: Dec,
        roc_ops: *RocOps,
    ) RocList {
        if (self.isUnique()) {
            return self;
        }

        if (self.isEmpty()) {
            // Empty is not necessarily unique on it's own.
            // The list could have capacity and be shared.
            self.decref(alignment, element_width, elements_refcounted, dec, roc_ops);
            return RocList.empty();
        }

        // unfortunately, we have to clone
        const new_list = RocList.list_allocate(alignment, self.length, element_width, elements_refcounted, roc_ops);

        var old_bytes: [*]u8 = @as([*]u8, @ptrCast(self.bytes));
        var new_bytes: [*]u8 = @as([*]u8, @ptrCast(new_list.bytes));

        const number_of_bytes = self.len() * element_width;
        @memcpy(new_bytes[0..number_of_bytes], old_bytes[0..number_of_bytes]);

        // Increment refcount of all elements now in a new list.
        if (elements_refcounted) {
            var i: usize = 0;
            while (i < self.len()) : (i += 1) {
                inc(new_bytes + i * element_width);
            }
        }

        self.decref(alignment, element_width, elements_refcounted, dec, roc_ops);

        return new_list;
    }

    pub fn list_allocate(
        alignment: u32,
        length: usize,
        element_width: usize,
        elements_refcounted: bool,
        roc_ops: *RocOps,
    ) RocList {
        if (length == 0) {
            return empty();
        }

        const capacity = builtins.utils.calculateCapacity(0, length, element_width);
        const data_bytes = capacity * element_width;
        return RocList{
            .bytes = builtins.utils.allocateWithRefcount(
                data_bytes,
                alignment,
                elements_refcounted,
                roc_ops,
            ),
            .length = length,
            .capacity_or_alloc_ptr = capacity,
        };
    }

    pub fn allocateExact(
        alignment: u32,
        length: usize,
        element_width: usize,
        elements_refcounted: bool,
        roc_ops: *RocOps,
    ) RocList {
        if (length == 0) {
            return empty();
        }

        const data_bytes = length * element_width;
        return RocList{
            .bytes = builtins.utils.allocateWithRefcount(
                data_bytes,
                alignment,
                elements_refcounted,
                roc_ops,
            ),
            .length = length,
            .capacity_or_alloc_ptr = length,
        };
    }

    pub fn reallocate(
        self: RocList,
        alignment: u32,
        new_length: usize,
        element_width: usize,
        elements_refcounted: bool,
        inc: Inc,
        roc_ops: *RocOps,
    ) RocList {
        if (self.bytes) |source_ptr| {
            if (self.isUnique() and !self.isSeamlessSlice()) {
                const capacity = self.capacity_or_alloc_ptr;
                if (capacity >= new_length) {
                    return RocList{ .bytes = self.bytes, .length = new_length, .capacity_or_alloc_ptr = capacity };
                } else {
                    const new_capacity = builtins.utils.calculateCapacity(capacity, new_length, element_width);
                    const new_source = builtins.utils.unsafeReallocate(source_ptr, alignment, capacity, new_capacity, element_width, elements_refcounted);
                    return RocList{ .bytes = new_source, .length = new_length, .capacity_or_alloc_ptr = new_capacity };
                }
            }
            return self.reallocateFresh(alignment, new_length, element_width, elements_refcounted, inc, roc_ops);
        }
        return RocList.list_allocate(alignment, new_length, element_width, elements_refcounted, roc_ops);
    }

    /// reallocate by explicitly making a new allocation and copying elements over
    fn reallocateFresh(
        self: RocList,
        alignment: u32,
        new_length: usize,
        element_width: usize,
        elements_refcounted: bool,
        inc: Inc,
        roc_ops: *RocOps,
    ) RocList {
        const old_length = self.length;

        const result = RocList.list_allocate(alignment, new_length, element_width, elements_refcounted, roc_ops);

        if (self.bytes) |source_ptr| {
            // transfer the memory
            const dest_ptr = result.bytes orelse unreachable;

            @memcpy(dest_ptr[0..(old_length * element_width)], source_ptr[0..(old_length * element_width)]);
            @memset(dest_ptr[(old_length * element_width)..(new_length * element_width)], 0);

            // Increment refcount of all elements now in a new list.
            if (elements_refcounted) {
                var i: usize = 0;
                while (i < old_length) : (i += 1) {
                    inc(dest_ptr + i * element_width);
                }
            }
        }

        // Calls utils.decref directly to avoid decrementing the refcount of elements.
        builtins.utils.decref(self.getAllocationDataPtr(), self.capacity_or_alloc_ptr, alignment, elements_refcounted, roc_ops);

        return result;
    }
};

/// TODO: Document listIncref.
pub fn listIncref(list: RocList, amount: isize, elements_refcounted: bool) callconv(.C) void {
    list.incref(amount, elements_refcounted);
}

/// TODO: Document listDecref.
pub fn listDecref(
    list: RocList,
    alignment: u32,
    element_width: usize,
    elements_refcounted: bool,
    dec: Dec,
    roc_ops: *RocOps,
) callconv(.C) void {
    list.decref(
        alignment,
        element_width,
        elements_refcounted,
        dec,
        roc_ops,
    );
}

/// TODO: Document listWithCapacity.
pub fn listWithCapacity(
    capacity: u64,
    alignment: u32,
    element_width: usize,
    elements_refcounted: bool,
    inc: Inc,
    roc_ops: *RocOps,
) callconv(.C) RocList {
    return listReserve(
        RocList.empty(),
        alignment,
        capacity,
        element_width,
        elements_refcounted,
        inc,
        .InPlace,
        roc_ops,
    );
}

/// TODO: Document listReserve.
pub fn listReserve(
    list: RocList,
    alignment: u32,
    spare: u64,
    element_width: usize,
    elements_refcounted: bool,
    inc: Inc,
    update_mode: UpdateMode,
    roc_ops: *RocOps,
) callconv(.C) RocList {
    const original_len = list.len();
    const cap = @as(u64, @intCast(list.getCapacity()));
    const desired_cap = @as(u64, @intCast(original_len)) +| spare;

    if ((update_mode == .InPlace or list.isUnique()) and cap >= desired_cap) {
        return list;
    } else {
        // Make sure on 32-bit targets we don't accidentally wrap when we cast our U64 desired capacity to U32.
        const reserve_size: u64 = @min(desired_cap, @as(u64, @intCast(std.math.maxInt(usize))));

        var output = list.reallocate(
            alignment,
            @as(usize, @intCast(reserve_size)),
            element_width,
            elements_refcounted,
            inc,
            roc_ops,
        );
        output.length = original_len;
        return output;
    }
}

/// TODO: Document listReleaseExcessCapacity.
pub fn listReleaseExcessCapacity(
    list: RocList,
    alignment: u32,
    element_width: usize,
    elements_refcounted: bool,
    inc: Inc,
    dec: Dec,
    update_mode: UpdateMode,
    roc_ops: *RocOps,
) callconv(.C) RocList {
    const old_length = list.len();
    // We use the direct list.capacity_or_alloc_ptr to make sure both that there is no extra capacity and that it isn't a seamless slice.
    if ((update_mode == .InPlace or list.isUnique()) and list.capacity_or_alloc_ptr == old_length) {
        return list;
    } else if (old_length == 0) {
        list.decref(alignment, element_width, elements_refcounted, dec, roc_ops);
        return RocList.empty();
    } else {
        // TODO: This can be made more efficient, but has to work around the `decref`.
        // If the list is unique, we can avoid incrementing and decrementing the live items.
        // We can just decrement the dead elements and free the old list.
        // This pattern is also like true in other locations like listConcat and listDropAt.
        const output = RocList.allocateExact(alignment, old_length, element_width, elements_refcounted, roc_ops);
        if (list.bytes) |source_ptr| {
            const dest_ptr = output.bytes orelse unreachable;

            @memcpy(dest_ptr[0..(old_length * element_width)], source_ptr[0..(old_length * element_width)]);
            if (elements_refcounted) {
                var i: usize = 0;
                while (i < old_length) : (i += 1) {
                    const element = source_ptr + i * element_width;
                    inc(element);
                }
            }
        }
        list.decref(alignment, element_width, elements_refcounted, dec, roc_ops);
        return output;
    }
}

/// TODO: Document listAppendUnsafe.
pub fn listAppendUnsafe(
    list: RocList,
    element: Opaque,
    element_width: usize,
    copy: CopyFn,
) callconv(.C) RocList {
    const old_length = list.len();
    var output = list;
    output.length += 1;

    if (output.bytes) |bytes| {
        if (element) |source| {
            const target = bytes + old_length * element_width;
            copy(target, source);
        }
    }

    return output;
}

fn listAppend(
    list: RocList,
    alignment: u32,
    element: Opaque,
    element_width: usize,
    elements_refcounted: bool,
    inc: Inc,
    update_mode: UpdateMode,
    copy: CopyFn,
    roc_ops: *RocOps,
) callconv(.C) RocList {
    const with_capacity = listReserve(
        list,
        alignment,
        1,
        element_width,
        elements_refcounted,
        inc,
        update_mode,
        roc_ops,
    );
    return listAppendUnsafe(with_capacity, element, element_width, copy);
}

/// TODO: Document listPrepend.
pub fn listPrepend(
    list: RocList,
    alignment: u32,
    element: Opaque,
    element_width: usize,
    elements_refcounted: bool,
    inc: Inc,
    copy: CopyFn,
    roc_ops: *RocOps,
) callconv(.C) RocList {
    const old_length = list.len();
    // TODO: properly wire in update mode.
    var with_capacity = listReserve(
        list,
        alignment,
        1,
        element_width,
        elements_refcounted,
        inc,
        .Immutable,
        roc_ops,
    );
    with_capacity.length += 1;

    // can't use one memcpy here because source and target overlap
    if (with_capacity.bytes) |target| {
        const from = target;
        const to = target + element_width;
        const size = element_width * old_length;
        std.mem.copyBackwards(u8, to[0..size], from[0..size]);

        // finally copy in the new first element
        if (element) |source| {
            copy(target, source);
        }
    }

    return with_capacity;
}

/// TODO: Document listSwap.
pub fn listSwap(
    list: RocList,
    alignment: u32,
    element_width: usize,
    index_1: u64,
    index_2: u64,
    elements_refcounted: bool,
    inc: Inc,
    dec: Dec,
    update_mode: UpdateMode,
    copy: CopyFn,
    roc_ops: *RocOps,
) callconv(.C) RocList {
    // Early exit to avoid swapping the same element.
    if (index_1 == index_2)
        return list;

    const size = @as(u64, @intCast(list.len()));
    if (index_1 == index_2 or index_1 >= size or index_2 >= size) {
        // Either one index was out of bounds, or both indices were the same; just return
        return list;
    }

    const newList = blk: {
        if (update_mode == .InPlace) {
            break :blk list;
        } else {
            break :blk list.makeUnique(
                alignment,
                element_width,
                elements_refcounted,
                inc,
                dec,
                roc_ops,
            );
        }
    };

    const source_ptr = @as([*]u8, @ptrCast(newList.bytes));

    swapElements(source_ptr, element_width, @as(usize,
        // We already verified that both indices are less than the stored list length,
        // which is usize, so casting them to usize will definitely be lossless.
        @intCast(index_1)), @as(usize, @intCast(index_2)), copy);

    return newList;
}

/// Returns a sublist of the given list
pub fn listSublist(
    list: RocList,
    alignment: u32,
    element_width: usize,
    elements_refcounted: bool,
    start_u64: u64,
    len_u64: u64,
    dec: Dec,
    roc_ops: *RocOps,
) callconv(.C) RocList {
    const size = list.len();
    if (size == 0 or len_u64 == 0 or start_u64 >= @as(u64, @intCast(size))) {
        if (list.isUnique()) {
            // Decrement the reference counts of all elements.
            if (list.bytes) |source_ptr| {
                if (elements_refcounted) {
                    var i: usize = 0;
                    while (i < size) : (i += 1) {
                        const element = source_ptr + i * element_width;
                        dec(element);
                    }
                }
            }

            var output = list;
            output.length = 0;
            return output;
        }
        list.decref(alignment, element_width, elements_refcounted, dec, roc_ops);
        return RocList.empty();
    }

    if (list.bytes) |source_ptr| {
        // This cast is lossless because we would have early-returned already
        // if `start_u64` were greater than `size`, and `size` fits in usize.
        const start: usize = @intCast(start_u64);

        // (size - start) can't overflow because we would have early-returned already
        // if `start` were greater than `size`.
        const size_minus_start = size - start;

        // This outer cast to usize is lossless. size, start, and size_minus_start all fit in usize,
        // and @min guarantees that if `len_u64` gets returned, it's because it was smaller
        // than something that fit in usize.
        const keep_len = @as(usize, @intCast(@min(len_u64, @as(u64, @intCast(size_minus_start)))));

        if (start == 0 and list.isUnique()) {
            // The list is unique, we actually have to decrement refcounts to elements we aren't keeping around.
            // Decrement the reference counts of elements after `start + keep_len`.
            if (elements_refcounted) {
                const drop_end_len = size_minus_start - keep_len;
                var i: usize = 0;
                while (i < drop_end_len) : (i += 1) {
                    const element = source_ptr + (start + keep_len + i) * element_width;
                    dec(element);
                }
            }

            var output = list;
            output.length = keep_len;
            return output;
        } else {
            if (list.isUnique()) {
                list.setAllocationElementCount(elements_refcounted);
            }
            const list_alloc_ptr = (@intFromPtr(source_ptr) >> 1) | SEAMLESS_SLICE_BIT;
            const slice_alloc_ptr = list.capacity_or_alloc_ptr;
            const slice_mask = list.seamlessSliceMask();
            const alloc_ptr = (list_alloc_ptr & ~slice_mask) | (slice_alloc_ptr & slice_mask);
            return RocList{
                .bytes = source_ptr + start * element_width,
                .length = keep_len,
                .capacity_or_alloc_ptr = alloc_ptr,
            };
        }
    }

    return RocList.empty();
}

/// TODO: Document listDropAt.
pub fn listDropAt(
    list: RocList,
    alignment: u32,
    element_width: usize,
    elements_refcounted: bool,
    drop_index_u64: u64,
    inc: Inc,
    dec: Dec,
    roc_ops: *RocOps,
) callconv(.C) RocList {
    const size = list.len();
    const size_u64 = @as(u64, @intCast(size));

    // NOTE
    // we need to return an empty list explicitly,
    // because we rely on the pointer field being null if the list is empty
    // which also requires duplicating the utils.decref call to spend the RC token
    if (size <= 1) {
        list.decref(alignment, element_width, elements_refcounted, dec, roc_ops);
        return RocList.empty();
    }

    // If dropping the first or last element, return a seamless slice.
    // For simplicity, do this by calling listSublist.
    // In the future, we can test if it is faster to manually inline the important parts here.
    if (drop_index_u64 == 0) {
        return listSublist(
            list,
            alignment,
            element_width,
            elements_refcounted,
            1,
            size -| 1,
            dec,
            roc_ops,
        );
    } else if (drop_index_u64 == size_u64 - 1) { // It's fine if (size - 1) wraps on size == 0 here,
        // because if size is 0 then it's always fine for this branch to be taken; no
        // matter what drop_index was, we're size == 0, so empty list will always be returned.
        return listSublist(
            list,
            alignment,
            element_width,
            elements_refcounted,
            0,
            size -| 1,
            dec,
            roc_ops,
        );
    }

    if (list.bytes) |source_ptr| {
        if (drop_index_u64 >= size_u64) {
            return list;
        }

        // This cast must be lossless, because we would have just early-returned if drop_index
        // were >= than `size`, and we know `size` fits in usize.
        const drop_index: usize = @intCast(drop_index_u64);

        if (list.isUnique()) {
            if (elements_refcounted) {
                const element = source_ptr + drop_index * element_width;
                dec(element);
            }

            const copy_target = source_ptr + (drop_index * element_width);
            const copy_source = copy_target + element_width;
            const copy_size = (size - drop_index - 1) * element_width;
            std.mem.copyForwards(u8, copy_target[0..copy_size], copy_source[0..copy_size]);

            var new_list = list;

            new_list.length -= 1;
            return new_list;
        }

        const output = RocList.list_allocate(
            alignment,
            size - 1,
            element_width,
            elements_refcounted,
            roc_ops,
        );
        const target_ptr = output.bytes orelse unreachable;

        const head_size = drop_index * element_width;
        @memcpy(target_ptr[0..head_size], source_ptr[0..head_size]);

        const tail_target = target_ptr + drop_index * element_width;
        const tail_source = source_ptr + (drop_index + 1) * element_width;
        const tail_size = (size - drop_index - 1) * element_width;
        @memcpy(tail_target[0..tail_size], tail_source[0..tail_size]);

        if (elements_refcounted) {
            var i: usize = 0;
            while (i < output.len()) : (i += 1) {
                const cloned_elem = target_ptr + i * element_width;
                inc(cloned_elem);
            }
        }

        list.decref(alignment, element_width, elements_refcounted, dec, roc_ops);

        return output;
    } else {
        return RocList.empty();
    }
}

/// TODO: Document listSortWith.
pub fn listSortWith(
    input: RocList,
    cmp: CompareFn,
    cmp_data: Opaque,
    inc_n_data: IncN,
    data_is_owned: bool,
    alignment: u32,
    element_width: usize,
    elements_refcounted: bool,
    inc: Inc,
    dec: Dec,
    copy: CopyFn,
    roc_ops: *RocOps,
) callconv(.C) RocList {
    if (input.len() < 2) {
        return input;
    }
    var list = input.makeUnique(
        alignment,
        element_width,
        elements_refcounted,
        inc,
        dec,
        roc_ops,
    );

    if (list.bytes) |source_ptr| {
        builtins.sort.fluxsort(
            source_ptr,
            list.len(),
            cmp,
            cmp_data,
            data_is_owned,
            inc_n_data,
            element_width,
            alignment,
            copy,
            roc_ops,
        );
    }

    return list;
}

// SWAP ELEMENTS

fn swap(
    element_width: usize,
    p1: [*]u8,
    p2: [*]u8,
    copy: CopyFn,
) void {
    const threshold: usize = 64;

    var buffer_actual: [threshold]u8 = undefined;
    const buffer: [*]u8 = buffer_actual[0..];

    if (element_width <= threshold) {
        copy(buffer, p1);
        copy(p1, p2);
        copy(p2, buffer);
        return;
    }

    var width = element_width;

    var ptr1 = p1;
    var ptr2 = p2;
    while (true) {
        if (width < threshold) {
            @memcpy(buffer[0..width], ptr1[0..width]);
            @memcpy(ptr1[0..width], ptr2[0..width]);
            @memcpy(ptr2[0..width], buffer[0..width]);
            return;
        } else {
            @memcpy(buffer[0..threshold], ptr1[0..threshold]);
            @memcpy(ptr1[0..threshold], ptr2[0..threshold]);
            @memcpy(ptr2[0..threshold], buffer[0..threshold]);

            ptr1 += threshold;
            ptr2 += threshold;

            width -= threshold;
        }
    }
}

fn swapElements(
    source_ptr: [*]u8,
    element_width: usize,
    index_1: usize,
    index_2: usize,
    copy: CopyFn,
) void {
    const element_at_i = source_ptr + (index_1 * element_width);
    const element_at_j = source_ptr + (index_2 * element_width);

    return swap(element_width, element_at_i, element_at_j, copy);
}

/// Concatenates two lists into a new list containing all elements from both lists.
///
/// ## Ownership and Memory Management
/// **IMPORTANT**: This function CONSUMES both input lists (`list_a` and `list_b`).
/// The caller must NOT call `decref` on either input list after calling this function,
/// as this function handles their cleanup internally.
pub fn listConcat(
    list_a: RocList,
    list_b: RocList,
    alignment: u32,
    element_width: usize,
    elements_refcounted: bool,
    inc: Inc,
    dec: Dec,
    roc_ops: *RocOps,
) callconv(.C) RocList {
    // NOTE we always use list_a! because it is owned, we must consume it, and it may have unused capacity
    if (list_b.isEmpty()) {
        if (list_a.getCapacity() == 0) {
            // a could be a seamless slice, so we still need to decref.
            list_a.decref(alignment, element_width, elements_refcounted, dec, roc_ops);
            return list_b;
        } else {
            // we must consume this list. Even though it has no elements, it could still have capacity
            list_b.decref(alignment, element_width, elements_refcounted, dec, roc_ops);

            return list_a;
        }
    } else if (list_a.isUnique()) {
        const total_length: usize = list_a.len() + list_b.len();

        const resized_list_a = list_a.reallocate(
            alignment,
            total_length,
            element_width,
            elements_refcounted,
            inc,
            roc_ops,
        );

        // These must exist, otherwise, the lists would have been empty.
        const source_a = resized_list_a.bytes orelse unreachable;
        const source_b = list_b.bytes orelse unreachable;
        @memcpy(source_a[(list_a.len() * element_width)..(total_length * element_width)], source_b[0..(list_b.len() * element_width)]);

        // Increment refcount of all cloned elements.
        if (elements_refcounted) {
            var i: usize = 0;
            while (i < list_b.len()) : (i += 1) {
                const cloned_elem = source_b + i * element_width;
                inc(cloned_elem);
            }
        }

        // decrement list b.
        list_b.decref(alignment, element_width, elements_refcounted, dec, roc_ops);

        return resized_list_a;
    } else if (list_b.isUnique()) {
        const total_length: usize = list_a.len() + list_b.len();

        const resized_list_b = list_b.reallocate(
            alignment,
            total_length,
            element_width,
            elements_refcounted,
            inc,
            roc_ops,
        );

        // These must exist, otherwise, the lists would have been empty.
        const source_a = list_a.bytes orelse unreachable;
        const source_b = resized_list_b.bytes orelse unreachable;

        // This is a bit special, we need to first copy the elements of list_b to the end,
        // then copy the elements of list_a to the beginning.
        // This first call must use mem.copy because the slices might overlap.
        const byte_count_a = list_a.len() * element_width;
        const byte_count_b = list_b.len() * element_width;
        std.mem.copyBackwards(u8, source_b[byte_count_a .. byte_count_a + byte_count_b], source_b[0..byte_count_b]);
        @memcpy(source_b[0..byte_count_a], source_a[0..byte_count_a]);

        // Increment refcount of all cloned elements.
        if (elements_refcounted) {
            var i: usize = 0;
            while (i < list_a.len()) : (i += 1) {
                const cloned_elem = source_a + i * element_width;
                inc(cloned_elem);
            }
        }

        // decrement list a.
        list_a.decref(alignment, element_width, elements_refcounted, dec, roc_ops);

        return resized_list_b;
    }
    const total_length: usize = list_a.len() + list_b.len();

    const output = RocList.list_allocate(alignment, total_length, element_width, elements_refcounted, roc_ops);

    // These must exist, otherwise, the lists would have been empty.
    const target = output.bytes orelse unreachable;
    const source_a = list_a.bytes orelse unreachable;
    const source_b = list_b.bytes orelse unreachable;

    @memcpy(target[0..(list_a.len() * element_width)], source_a[0..(list_a.len() * element_width)]);
    @memcpy(target[(list_a.len() * element_width)..(total_length * element_width)], source_b[0..(list_b.len() * element_width)]);

    // Increment refcount of all cloned elements.
    if (elements_refcounted) {
        var i: usize = 0;
        while (i < list_a.len()) : (i += 1) {
            const cloned_elem = source_a + i * element_width;
            inc(cloned_elem);
        }
        i = 0;
        while (i < list_b.len()) : (i += 1) {
            const cloned_elem = source_b + i * element_width;
            inc(cloned_elem);
        }
    }

    // decrement list a and b.
    list_a.decref(alignment, element_width, elements_refcounted, dec, roc_ops);
    list_b.decref(alignment, element_width, elements_refcounted, dec, roc_ops);

    return output;
}

/// TODO: Document listReplaceInPlace.
pub fn listReplaceInPlace(
    list: RocList,
    index: u64,
    element: Opaque,
    element_width: usize,
    out_element: ?[*]u8,
    copy: CopyFn,
) callconv(.C) RocList {
    // INVARIANT: bounds checking happens on the roc side
    //
    // at the time of writing, the function is implemented roughly as
    // `if inBounds then LowLevelListReplace input index item else input`
    // so we don't do a bounds check here. Hence, the list is also non-empty,
    // because inserting into an empty list is always out of bounds,
    // and it's always safe to cast index to usize.
    return listReplaceInPlaceHelp(list, @as(usize, @intCast(index)), element, element_width, out_element, copy);
}

/// TODO: Document listReplace.
pub fn listReplace(
    list: RocList,
    alignment: u32,
    index: u64,
    element: Opaque,
    element_width: usize,
    elements_refcounted: bool,
    inc: Inc,
    dec: Dec,
    out_element: ?[*]u8,
    copy: CopyFn,
    roc_ops: *RocOps,
) callconv(.C) RocList {
    // INVARIANT: bounds checking happens on the roc side
    //
    // at the time of writing, the function is implemented roughly as
    // `if inBounds then LowLevelListReplace input index item else input`
    // so we don't do a bounds check here. Hence, the list is also non-empty,
    // because inserting into an empty list is always out of bounds,
    // and it's always safe to cast index to usize.
    // because inserting into an empty list is always out of bounds
    return listReplaceInPlaceHelp(
        list.makeUnique(alignment, element_width, elements_refcounted, inc, dec, roc_ops),
        @as(usize, @intCast(index)),
        element,
        element_width,
        out_element,
        copy,
    );
}

inline fn listReplaceInPlaceHelp(
    list: RocList,
    index: usize,
    element: Opaque,
    element_width: usize,
    out_element: ?[*]u8,
    copy: CopyFn,
) RocList {
    // the element we will replace
    const element_at_index = (list.bytes orelse unreachable) + (index * element_width);

    // copy out the old element
    copy((out_element orelse unreachable), element_at_index);

    // copy in the new element
    copy(element_at_index, (element orelse unreachable));

    return list;
}

/// TODO: Document listIsUnique.
pub fn listIsUnique(
    list: RocList,
) callconv(.C) bool {
    return list.isEmpty() or list.isUnique();
}

/// TODO: Document listClone.
pub fn listClone(
    list: RocList,
    alignment: u32,
    element_width: usize,
    elements_refcounted: bool,
    inc: Inc,
    dec: Dec,
    roc_ops: *RocOps,
) callconv(.C) RocList {
    return list.makeUnique(alignment, element_width, elements_refcounted, inc, dec, roc_ops);
}

/// TODO: Document listCapacity.
pub fn listCapacity(
    list: RocList,
) callconv(.C) usize {
    return list.getCapacity();
}

/// TODO: Document listAllocationPtr.
pub fn listAllocationPtr(
    list: RocList,
) callconv(.C) ?[*]u8 {
    return list.getAllocationDataPtr();
}

fn rcNone(_: ?[*]u8) callconv(.C) void {}

/// TODO: Document listConcatUtf8.
pub fn listConcatUtf8(
    list: RocList,
    string: RocStr,
    roc_ops: *RocOps,
) callconv(.C) RocList {
    if (string.len() == 0) {
        return list;
    } else {
        const combined_length = list.len() + string.len();

        // List U8 has alignment 1 and element_width 1
        const result = list.reallocate(1, combined_length, 1, false, &rcNone, roc_ops);
        // We just allocated combined_length, which is > 0 because string.len() > 0
        var bytes = result.bytes orelse unreachable;
        @memcpy(bytes[list.len()..combined_length], string.asU8ptr()[0..string.len()]);

        return result;
    }
}
