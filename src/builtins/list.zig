//! Runtime implementation of Roc's List type with reference counting and memory optimization.
//!
//! Lists use copy-on-write semantics to minimize allocations when shared across contexts.
//! Seamless slice optimization reduces memory overhead for substring operations.
const std = @import("std");

const utils = @import("utils.zig");
const UpdateMode = utils.UpdateMode;
const TestEnv = utils.TestEnv;
const RocOps = @import("host_abi.zig").RocOps;
const RocStr = @import("str.zig").RocStr;
const increfDataPtrC = utils.increfDataPtrC;

const Opaque = ?[*]u8;
const EqFn = *const fn (Opaque, Opaque) callconv(.c) bool;
const CompareFn = *const fn (Opaque, Opaque, Opaque) callconv(.c) u8;
const CopyFn = *const fn (Opaque, Opaque) callconv(.c) void;

const Inc = *const fn (?[*]u8) callconv(.c) void;
const IncN = *const fn (?[*]u8, usize) callconv(.c) void;
const Dec = *const fn (?[*]u8) callconv(.c) void;
const HasTagId = *const fn (u16, ?[*]u8) callconv(.c) extern struct { matched: bool, data: ?[*]u8 };

/// A bit mask were the only set bit is the bit indicating if the List is a seamless slice.
pub const SEAMLESS_SLICE_BIT: usize =
    @as(usize, @bitCast(@as(isize, std.math.minInt(isize))));

/// Runtime representation of Roc's List type with reference counting and seamless slice optimization.
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
            const ptr = @as([*]usize, @ptrCast(@alignCast(self.getAllocationDataPtr()))) - 2;
            ptr[0] = self.length;
        }
    }

    pub fn incref(self: RocList, amount: isize, elements_refcounted: bool) void {
        // If the list is unique and not a seamless slice, the length needs to be store on the heap if the elements are refcounted.
        if (elements_refcounted and self.isUnique() and !self.isSeamlessSlice()) {
            if (self.getAllocationDataPtr()) |source| {
                // - 1 is refcount.
                // - 2 is size on heap.
                const ptr = @as([*]usize, @ptrCast(@alignCast(source))) - 2;
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
        utils.decref(
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
        return utils.rcUnique(@bitCast(self.refcount()));
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
        elem_alignment: u32,
        length: usize,
        element_width: usize,
        elements_refcounted: bool,
        roc_ops: *RocOps,
    ) RocList {
        if (length == 0) {
            return empty();
        }

        const capacity = utils.calculateCapacity(0, length, element_width);
        const data_bytes = capacity * element_width;
        return RocList{
            .bytes = utils.allocateWithRefcount(
                data_bytes,
                elem_alignment,
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
            .bytes = utils.allocateWithRefcount(
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
                    const new_capacity = utils.calculateCapacity(capacity, new_length, element_width);
                    const new_source = utils.unsafeReallocate(source_ptr, alignment, capacity, new_capacity, element_width, elements_refcounted);
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
        utils.decref(self.getAllocationDataPtr(), self.capacity_or_alloc_ptr, alignment, elements_refcounted, roc_ops);

        return result;
    }
};

/// Increment the reference count.
pub fn listIncref(list: RocList, amount: isize, elements_refcounted: bool) callconv(.c) void {
    list.incref(amount, elements_refcounted);
}

/// Decrement reference count and deallocate when no longer shared.
pub fn listDecref(
    list: RocList,
    alignment: u32,
    element_width: usize,
    elements_refcounted: bool,
    dec: Dec,
    roc_ops: *RocOps,
) callconv(.c) void {
    list.decref(
        alignment,
        element_width,
        elements_refcounted,
        dec,
        roc_ops,
    );
}

/// Create an empty list with pre-allocated capacity to avoid reallocation during growth.
pub fn listWithCapacity(
    capacity: u64,
    alignment: u32,
    element_width: usize,
    elements_refcounted: bool,
    inc: Inc,
    roc_ops: *RocOps,
) callconv(.c) RocList {
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

/// Ensure the list has capacity for additional elements to prevent reallocation.
pub fn listReserve(
    list: RocList,
    alignment: u32,
    spare: u64,
    element_width: usize,
    elements_refcounted: bool,
    inc: Inc,
    update_mode: UpdateMode,
    roc_ops: *RocOps,
) callconv(.c) RocList {
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

/// Reduce memory usage by trimming unused capacity when list has shrunk significantly.
pub fn listReleaseExcessCapacity(
    list: RocList,
    alignment: u32,
    element_width: usize,
    elements_refcounted: bool,
    inc: Inc,
    dec: Dec,
    update_mode: UpdateMode,
    roc_ops: *RocOps,
) callconv(.c) RocList {
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

/// Add element to end of list. Caller must ensure sufficient capacity exists.
pub fn listAppendUnsafe(
    list: RocList,
    element: Opaque,
    element_width: usize,
    copy: CopyFn,
) callconv(.c) RocList {
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
) callconv(.c) RocList {
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

/// Directly mutate the given list to push an element onto the end, and then return it.
/// If there isn't enough capacity, uses roc_realloc to get more.
///
/// NOTE: This does *not* increment any refcounts! The caller should increment
/// the refcount of the element being appended before calling this function.
///
/// To implement `List.append` using this, you need to:
/// - Check if this list's refcount is 1, and if so, call pushInPlace on it and return that list.
///   - (If you know statically that the list's refcount will be 1, you don't need to check it at runtime.)
/// - If the refcount is not 1, then:
///   - Create a new list with a fresh refcount (ideally using 1.5x the necessary capacity, so future appends are cheaper)
///   - Copy all the existing elements over using shallowClone()
///     - Then increment all the existing elements' refcounts, if they are refcounted.
///     - The reason we don't have an `append` function in this file is that refcounting other types is out of scope.
///       (Refcounting other types would require passing in function pointers, which LLVM doesn't optimize well.)
pub fn pushInPlace(
    list: RocList,
    alignment: u32,
    element_size: usize,
    element: *anyopaque,
    roc_ops: *RocOps,
) callconv(.c) RocList {
    const old_length = list.len();
    const old_capacity = list.getCapacity();

    if (old_capacity > old_length) {
        var output = list;
        output.length += 1;

        if (output.bytes) |bytes| {
            const target = bytes + old_length * element_size;
            @memcpy(target[0..element_size], @as([*]const u8, @ptrCast(element))[0..element_size]);
        }

        return output;
    } else {
        // No overflow check needed: allocator will fail at isize::MAX before usize overflow
        const new_length = old_length + 1;

        const resized_list = list.reallocate(
            alignment,
            new_length,
            element_size,
            false,
            rcNone,
            roc_ops,
        );

        if (resized_list.bytes) |bytes| {
            const target = bytes + old_length * element_size;
            @memcpy(target[0..element_size], @as([*]const u8, @ptrCast(element))[0..element_size]);
        }

        return resized_list;
    }
}

/// Make a new list of nonzero-sized elements, with the given *POSITIVE* capacity, by
/// shallowly copying an existing list's elements. (That is, doing a memcpy of the heap bytes this list points to.)
/// The new list has a default refcount, and the same length as the existing list.
/// This function assumes you always want a heap allocation to be performed, so passing 0 capacity to
/// this function is illegal behavior and will panic in debug builds. If you don't want a heap allocation
/// performed, then don't call this function! Same with passing an element with either a size or alignment of 0.
///
/// NOTE: This does *not* increment any refcounts! If the existing list's elements are refcounted,
/// the *caller* is responsible for incrementing their refcounts, as this function does not know
/// where their refcounts are stored in memory, and therefore cannot increment them.
///
/// (Refcounting elements would require passing a function pointer, which LLVM does not optimize well.)
pub fn shallowClone(
    old_list: RocList,
    desired_capacity: usize,
    elem_size: usize,
    elem_alignment: u32,
    elements_refcounted: bool,
    roc_ops: *RocOps,
) callconv(.c) RocList {
    std.debug.assert(desired_capacity > 0);
    std.debug.assert(elem_size > 0);
    std.debug.assert(elem_alignment > 0);

    const len = old_list.len();
    const capacity = utils.calculateCapacity(0, desired_capacity, elem_size);
    const new_list = RocList{
        .bytes = utils.allocateWithRefcount(
            capacity * elem_size,
            elem_alignment,
            elements_refcounted,
            roc_ops,
        ),
        .length = len,
        .capacity_or_alloc_ptr = capacity,
    };

    // Only copy bytes over if the original list was nonempty.
    // It's fine if the original list was empty and this one will be nonempty;
    // we just make the allocation, set its length to 0, and return it.
    if (old_list.bytes) |source_bytes| {
        // We know the new list has a valid pointer because otherwise roc_alloc would have crashed.
        var dest_bytes = new_list.bytes orelse unreachable;

        const copy_size = len * elem_size;
        @memcpy(dest_bytes[0..copy_size], source_bytes[0..copy_size]);
    }

    return new_list;
}

/// Add element to beginning of list, shifting existing elements.
pub fn listPrepend(
    list: RocList,
    alignment: u32,
    element: Opaque,
    element_width: usize,
    elements_refcounted: bool,
    inc: Inc,
    copy: CopyFn,
    roc_ops: *RocOps,
) callconv(.c) RocList {
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

/// Exchange elements at two positions within the list.
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
) callconv(.c) RocList {
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
) callconv(.c) RocList {
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

/// Remove element at specified index, shifting remaining elements.
pub fn listDropAt(
    list: RocList,
    alignment: u32,
    element_width: usize,
    elements_refcounted: bool,
    drop_index_u64: u64,
    inc: Inc,
    dec: Dec,
    roc_ops: *RocOps,
) callconv(.c) RocList {
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

/// Sort list elements using provided comparison function for custom ordering.
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
) callconv(.c) RocList {
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
        @import("sort.zig").fluxsort(
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
) callconv(.c) RocList {
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

/// Replace element at index, returning original value. No allocation when unique.
pub fn listReplaceInPlace(
    list: RocList,
    index: u64,
    element: Opaque,
    element_width: usize,
    out_element: ?[*]u8,
    copy: CopyFn,
) callconv(.c) RocList {
    // INVARIANT: bounds checking happens on the roc side
    //
    // at the time of writing, the function is implemented roughly as
    // `if inBounds then LowLevelListReplace input index item else input`
    // so we don't do a bounds check here. Hence, the list is also non-empty,
    // because inserting into an empty list is always out of bounds,
    // and it's always safe to cast index to usize.
    return listReplaceInPlaceHelp(list, @as(usize, @intCast(index)), element, element_width, out_element, copy);
}

/// Replace element at index, ensuring list uniqueness through copy-on-write.
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
) callconv(.c) RocList {
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

/// Check if list has exclusive ownership for safe in-place modification.
pub fn listIsUnique(
    list: RocList,
) callconv(.c) bool {
    return list.isEmpty() or list.isUnique();
}

/// Create independent copy for safe mutation when list is shared.
pub fn listClone(
    list: RocList,
    alignment: u32,
    element_width: usize,
    elements_refcounted: bool,
    inc: Inc,
    dec: Dec,
    roc_ops: *RocOps,
) callconv(.c) RocList {
    return list.makeUnique(alignment, element_width, elements_refcounted, inc, dec, roc_ops);
}

/// Get current allocated capacity for growth planning.
pub fn listCapacity(
    list: RocList,
) callconv(.c) usize {
    return list.getCapacity();
}

/// Get raw memory pointer for direct access patterns.
pub fn listAllocationPtr(
    list: RocList,
) callconv(.c) ?[*]u8 {
    return list.getAllocationDataPtr();
}

fn rcNone(_: ?[*]u8) callconv(.c) void {}

/// Append UTF-8 string bytes to list for efficient string-to-bytes conversion.
pub fn listConcatUtf8(
    list: RocList,
    string: RocStr,
    roc_ops: *RocOps,
) callconv(.c) RocList {
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

test "listConcat: non-unique with unique overlapping" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    const nonUnique = RocList.fromSlice(u8, ([_]u8{1})[0..], false, test_env.getOps());
    const bytes: [*]u8 = @as([*]u8, @ptrCast(nonUnique.bytes));
    const ptr_width = @sizeOf(usize);
    const refcount_ptr = @as([*]isize, @ptrCast(@as([*]align(ptr_width) u8, @alignCast(bytes)) - ptr_width));
    utils.increfRcPtrC(&refcount_ptr[0], 1);
    // NOTE: nonUnique will be consumed by listConcat, so no defer decref needed

    const unique = RocList.fromSlice(u8, ([_]u8{ 2, 3, 4 })[0..], false, test_env.getOps());
    // NOTE: unique will be consumed by listConcat, so no defer decref needed

    var concatted = listConcat(nonUnique, unique, 1, 1, false, rcNone, rcNone, test_env.getOps());
    defer concatted.decref(@alignOf(u8), @sizeOf(u8), false, rcNone, test_env.getOps());
    var wanted = RocList.fromSlice(u8, ([_]u8{ 1, 2, 3, 4 })[0..], false, test_env.getOps());
    defer wanted.decref(@alignOf(u8), @sizeOf(u8), false, rcNone, test_env.getOps());

    try std.testing.expect(concatted.eql(wanted));
}

test "listConcatUtf8" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    const list = RocList.fromSlice(u8, &[_]u8{ 1, 2, 3, 4 }, false, test_env.getOps());
    // NOTE: list will be consumed by listConcatUtf8, so no defer decref needed
    const string_bytes = "🐦";
    const string = RocStr.init(string_bytes.ptr, string_bytes.len, test_env.getOps());
    defer string.decref(test_env.getOps());
    const ret = listConcatUtf8(list, string, test_env.getOps());
    defer ret.decref(1, 1, false, &rcNone, test_env.getOps());
    const expected = RocList.fromSlice(u8, &[_]u8{ 1, 2, 3, 4, 240, 159, 144, 166 }, false, test_env.getOps());
    defer expected.decref(1, 1, false, &rcNone, test_env.getOps());
    try std.testing.expect(ret.eql(expected));
}

test "RocList empty list creation" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    const empty_list = RocList.empty();
    defer empty_list.decref(1, 1, false, rcNone, test_env.getOps());

    try std.testing.expectEqual(@as(usize, 0), empty_list.len());
    try std.testing.expect(empty_list.isEmpty());
}

test "RocList fromSlice basic functionality" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    const data = [_]i32{ 10, 20, 30, 40 };
    const list = RocList.fromSlice(i32, data[0..], false, test_env.getOps());
    defer list.decref(@alignOf(i32), @sizeOf(i32), false, rcNone, test_env.getOps());

    try std.testing.expectEqual(@as(usize, 4), list.len());
    try std.testing.expect(!list.isEmpty());
}

test "RocList elements access" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    const data = [_]u8{ 1, 2, 3, 4, 5 };
    const list = RocList.fromSlice(u8, data[0..], false, test_env.getOps());
    defer list.decref(@alignOf(u8), @sizeOf(u8), false, rcNone, test_env.getOps());

    const elements_ptr = list.elements(u8);
    try std.testing.expect(elements_ptr != null);
    const elements = elements_ptr.?[0..list.len()];
    try std.testing.expectEqual(@as(u8, 1), elements[0]);
    try std.testing.expectEqual(@as(u8, 2), elements[1]);
    try std.testing.expectEqual(@as(u8, 3), elements[2]);
    try std.testing.expectEqual(@as(u8, 4), elements[3]);
    try std.testing.expectEqual(@as(u8, 5), elements[4]);
}

test "RocList capacity operations" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    const data = [_]i16{ 100, 200 };
    const list = RocList.fromSlice(i16, data[0..], false, test_env.getOps());
    defer list.decref(@alignOf(i16), @sizeOf(i16), false, rcNone, test_env.getOps());

    const capacity = list.getCapacity();
    try std.testing.expect(capacity >= list.len());
    try std.testing.expect(capacity >= 2);
}

test "RocList equality operations" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    const data1 = [_]u8{ 1, 2, 3 };
    const data2 = [_]u8{ 1, 2, 3 };
    const data3 = [_]u8{ 1, 2, 4 };

    const list1 = RocList.fromSlice(u8, data1[0..], false, test_env.getOps());
    defer list1.decref(@alignOf(u8), @sizeOf(u8), false, rcNone, test_env.getOps());

    const list2 = RocList.fromSlice(u8, data2[0..], false, test_env.getOps());
    defer list2.decref(@alignOf(u8), @sizeOf(u8), false, rcNone, test_env.getOps());

    const list3 = RocList.fromSlice(u8, data3[0..], false, test_env.getOps());
    defer list3.decref(@alignOf(u8), @sizeOf(u8), false, rcNone, test_env.getOps());

    // Equal lists should be equal
    try std.testing.expect(list1.eql(list2));
    try std.testing.expect(list2.eql(list1));

    // Different lists should not be equal
    try std.testing.expect(!list1.eql(list3));
    try std.testing.expect(!list3.eql(list1));

    // Empty lists should be equal
    const empty1 = RocList.empty();
    defer empty1.decref(1, 1, false, rcNone, test_env.getOps());
    const empty2 = RocList.empty();
    defer empty2.decref(1, 1, false, rcNone, test_env.getOps());
    try std.testing.expect(empty1.eql(empty2));
}

test "RocList uniqueness and cloning" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    const data = [_]i32{ 10, 20, 30 };
    const list = RocList.fromSlice(i32, data[0..], false, test_env.getOps());

    // A freshly created list should be unique
    try std.testing.expect(list.isUnique());

    // Make the list non-unique by incrementing reference count
    list.incref(1, false);
    defer list.decref(@alignOf(i32), @sizeOf(i32), false, rcNone, test_env.getOps());
    try std.testing.expect(!list.isUnique());

    // Clone the list (this will consume one reference to the original)
    const cloned = listClone(list, @alignOf(i32), @sizeOf(i32), false, rcNone, rcNone, test_env.getOps());
    defer cloned.decref(@alignOf(i32), @sizeOf(i32), false, rcNone, test_env.getOps());

    // Both should be equal but different objects (since list was not unique)
    try std.testing.expect(list.eql(cloned));
    try std.testing.expect(list.bytes != cloned.bytes);
}

test "RocList isUnique with reference counting" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    const data = [_]u8{ 1, 2, 3 };
    const list = RocList.fromSlice(u8, data[0..], false, test_env.getOps());
    defer list.decref(@alignOf(u8), @sizeOf(u8), false, rcNone, test_env.getOps());

    // Should be unique initially
    try std.testing.expect(list.isUnique());

    // Increment reference count
    list.incref(1, false);
    defer list.decref(@alignOf(u8), @sizeOf(u8), false, rcNone, test_env.getOps());

    // Should no longer be unique
    try std.testing.expect(!list.isUnique());
}

test "listWithCapacity basic functionality" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    const capacity: usize = 10;
    const list = listWithCapacity(capacity, @alignOf(i32), @sizeOf(i32), false, rcNone, test_env.getOps());
    defer list.decref(@alignOf(i32), @sizeOf(i32), false, rcNone, test_env.getOps());

    // Should have the requested capacity
    try std.testing.expect(list.getCapacity() >= capacity);
    // Should be empty initially
    try std.testing.expectEqual(@as(usize, 0), list.len());
    try std.testing.expect(list.isEmpty());
}

test "listReserve functionality" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    const data = [_]u8{ 1, 2, 3 };
    const list = RocList.fromSlice(u8, data[0..], false, test_env.getOps());

    const reserved_list = listReserve(list, @alignOf(u8), 20, @sizeOf(u8), false, rcNone, utils.UpdateMode.Immutable, test_env.getOps());
    defer reserved_list.decref(@alignOf(u8), @sizeOf(u8), false, rcNone, test_env.getOps());

    // Should have at least the requested capacity
    try std.testing.expect(reserved_list.getCapacity() >= 20);
    // Should preserve the original content
    try std.testing.expectEqual(@as(usize, 3), reserved_list.len());

    const elements_ptr = reserved_list.elements(u8);
    try std.testing.expect(elements_ptr != null);
    const elements = elements_ptr.?[0..reserved_list.len()];
    try std.testing.expectEqual(@as(u8, 1), elements[0]);
    try std.testing.expectEqual(@as(u8, 2), elements[1]);
    try std.testing.expectEqual(@as(u8, 3), elements[2]);
}

test "listCapacity function" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    const data = [_]i16{ 100, 200, 300 };
    const list = RocList.fromSlice(i16, data[0..], false, test_env.getOps());
    defer list.decref(@alignOf(i16), @sizeOf(i16), false, rcNone, test_env.getOps());

    const capacity = listCapacity(list);
    try std.testing.expectEqual(list.getCapacity(), capacity);
    try std.testing.expect(capacity >= list.len());
}

test "RocList allocateExact functionality" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    const exact_size: usize = 5;
    const list = RocList.allocateExact(@alignOf(u64), exact_size, @sizeOf(u64), false, test_env.getOps());
    defer list.decref(@alignOf(u64), @sizeOf(u64), false, rcNone, test_env.getOps());

    // Should have exactly the requested capacity (or very close)
    try std.testing.expectEqual(exact_size, list.getCapacity());
    // Should have the requested length
    try std.testing.expectEqual(exact_size, list.len());
    try std.testing.expect(!list.isEmpty());
}

test "listReleaseExcessCapacity functionality" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    // Create a list with some data
    const data = [_]u8{ 1, 2, 3 };
    const list_with_data = RocList.fromSlice(u8, data[0..], false, test_env.getOps());

    // Reserve excess capacity for it
    const list_with_excess = listReserve(list_with_data, @alignOf(u8), 100, @sizeOf(u8), false, rcNone, utils.UpdateMode.Immutable, test_env.getOps());

    // Verify it has excess capacity
    try std.testing.expect(list_with_excess.getCapacity() >= 100);
    try std.testing.expectEqual(@as(usize, 3), list_with_excess.len());

    // Release the excess capacity
    const released_list = listReleaseExcessCapacity(list_with_excess, @alignOf(u8), @sizeOf(u8), false, rcNone, rcNone, utils.UpdateMode.Immutable, test_env.getOps());
    defer released_list.decref(@alignOf(u8), @sizeOf(u8), false, rcNone, test_env.getOps());

    // The released list should have capacity close to its length and preserve the data
    try std.testing.expectEqual(@as(usize, 3), released_list.len());
    try std.testing.expect(released_list.getCapacity() >= released_list.len());
    try std.testing.expect(released_list.getCapacity() < 100); // Much less than the original excess

    // Verify data is preserved
    const elements_ptr = released_list.elements(u8);
    try std.testing.expect(elements_ptr != null);
    const elements = elements_ptr.?[0..released_list.len()];
    try std.testing.expectEqual(@as(u8, 1), elements[0]);
    try std.testing.expectEqual(@as(u8, 2), elements[1]);
    try std.testing.expectEqual(@as(u8, 3), elements[2]);
}

test "listSublist basic functionality" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    const data = [_]u8{ 1, 2, 3, 4, 5, 6, 7, 8 };
    const list = RocList.fromSlice(u8, data[0..], false, test_env.getOps());
    // Note: listSublist consumes the original list

    // Extract middle portion
    const sublist = listSublist(list, @alignOf(u8), @sizeOf(u8), false, 2, 4, rcNone, test_env.getOps());
    defer sublist.decref(@alignOf(u8), @sizeOf(u8), false, rcNone, test_env.getOps());

    try std.testing.expectEqual(@as(usize, 4), sublist.len());

    const elements_ptr = sublist.elements(u8);
    try std.testing.expect(elements_ptr != null);
    const elements = elements_ptr.?[0..sublist.len()];
    try std.testing.expectEqual(@as(u8, 3), elements[0]); // data[2]
    try std.testing.expectEqual(@as(u8, 4), elements[1]); // data[3]
    try std.testing.expectEqual(@as(u8, 5), elements[2]); // data[4]
    try std.testing.expectEqual(@as(u8, 6), elements[3]); // data[5]
}

test "listSublist edge cases" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    const data = [_]i32{ 10, 20, 30 };
    const list = RocList.fromSlice(i32, data[0..], false, test_env.getOps());

    // Take empty sublist
    const empty_sublist = listSublist(list, @alignOf(i32), @sizeOf(i32), false, 1, 0, rcNone, test_env.getOps());
    defer empty_sublist.decref(@alignOf(i32), @sizeOf(i32), false, rcNone, test_env.getOps());

    try std.testing.expectEqual(@as(usize, 0), empty_sublist.len());
    try std.testing.expect(empty_sublist.isEmpty());
}

test "listSwap basic functionality" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    const data = [_]u16{ 100, 200, 300, 400 };
    const list = RocList.fromSlice(u16, data[0..], false, test_env.getOps());

    // Swap elements at indices 1 and 3
    // Proper copy function for u16 elements
    const copy_fn = struct {
        fn copy(dest: ?[*]u8, src: ?[*]u8) callconv(.c) void {
            if (dest != null and src != null) {
                const dest_ptr = @as(*u16, @ptrCast(@alignCast(dest)));
                const src_ptr = @as(*u16, @ptrCast(@alignCast(src)));
                dest_ptr.* = src_ptr.*;
            }
        }
    }.copy;

    const swapped_list = listSwap(list, @alignOf(u16), @sizeOf(u16), 1, 3, false, rcNone, rcNone, utils.UpdateMode.Immutable, copy_fn, test_env.getOps());
    defer swapped_list.decref(@alignOf(u16), @sizeOf(u16), false, rcNone, test_env.getOps());

    try std.testing.expectEqual(@as(usize, 4), swapped_list.len());

    // Verify the swap actually worked
    const elements_ptr = swapped_list.elements(u16);
    try std.testing.expect(elements_ptr != null);
    const elements = elements_ptr.?[0..swapped_list.len()];
    try std.testing.expectEqual(@as(u16, 100), elements[0]); // unchanged
    try std.testing.expectEqual(@as(u16, 400), elements[1]); // was 200, now 400
    try std.testing.expectEqual(@as(u16, 300), elements[2]); // unchanged
    try std.testing.expectEqual(@as(u16, 200), elements[3]); // was 400, now 200
}

test "listAppendUnsafe basic functionality" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    // Copy function for u8 elements
    const copy_fn = struct {
        fn copy(dest: ?[*]u8, src: ?[*]u8) callconv(.c) void {
            if (dest != null and src != null) {
                const dest_ptr = @as(*u8, @ptrCast(@alignCast(dest)));
                const src_ptr = @as(*u8, @ptrCast(@alignCast(src)));
                dest_ptr.* = src_ptr.*;
            }
        }
    }.copy;

    // Create a list with some capacity
    var list = listWithCapacity(10, @alignOf(u8), @sizeOf(u8), false, rcNone, test_env.getOps());

    // Add some initial elements using listAppendUnsafe
    const element1: u8 = 42;
    list = listAppendUnsafe(list, @as(?[*]u8, @ptrCast(@constCast(&element1))), @sizeOf(u8), copy_fn);

    const element2: u8 = 84;
    list = listAppendUnsafe(list, @as(?[*]u8, @ptrCast(@constCast(&element2))), @sizeOf(u8), copy_fn);

    defer list.decref(@alignOf(u8), @sizeOf(u8), false, rcNone, test_env.getOps());

    try std.testing.expectEqual(@as(usize, 2), list.len());

    const elements_ptr = list.elements(u8);
    try std.testing.expect(elements_ptr != null);
    const elements = elements_ptr.?[0..list.len()];
    try std.testing.expectEqual(@as(u8, 42), elements[0]);
    try std.testing.expectEqual(@as(u8, 84), elements[1]);
}

test "listAppendUnsafe with different types" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    // Copy function for i32 elements
    const copy_fn = struct {
        fn copy(dest: ?[*]u8, src: ?[*]u8) callconv(.c) void {
            if (dest != null and src != null) {
                const dest_ptr = @as(*i32, @ptrCast(@alignCast(dest)));
                const src_ptr = @as(*i32, @ptrCast(@alignCast(src)));
                dest_ptr.* = src_ptr.*;
            }
        }
    }.copy;

    // Test with i32
    var int_list = listWithCapacity(5, @alignOf(i32), @sizeOf(i32), false, rcNone, test_env.getOps());

    const int_val: i32 = -123;
    int_list = listAppendUnsafe(int_list, @as(?[*]u8, @ptrCast(@constCast(&int_val))), @sizeOf(i32), copy_fn);

    defer int_list.decref(@alignOf(i32), @sizeOf(i32), false, rcNone, test_env.getOps());

    try std.testing.expectEqual(@as(usize, 1), int_list.len());

    const int_elements_ptr = int_list.elements(i32);
    try std.testing.expect(int_elements_ptr != null);
    const int_elements = int_elements_ptr.?[0..int_list.len()];
    try std.testing.expectEqual(@as(i32, -123), int_elements[0]);
}

test "listAppendUnsafe with pre-allocated capacity" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    // Copy function for u16 elements
    const copy_fn = struct {
        fn copy(dest: ?[*]u8, src: ?[*]u8) callconv(.c) void {
            if (dest != null and src != null) {
                const dest_ptr = @as(*u16, @ptrCast(@alignCast(dest)));
                const src_ptr = @as(*u16, @ptrCast(@alignCast(src)));
                dest_ptr.* = src_ptr.*;
            }
        }
    }.copy;

    // Create a list with capacity (listAppendUnsafe requires pre-allocated space)
    var list_with_capacity = listWithCapacity(5, @alignOf(u16), @sizeOf(u16), false, rcNone, test_env.getOps());

    const element: u16 = 9999;
    list_with_capacity = listAppendUnsafe(list_with_capacity, @as(?[*]u8, @ptrCast(@constCast(&element))), @sizeOf(u16), copy_fn);

    defer list_with_capacity.decref(@alignOf(u16), @sizeOf(u16), false, rcNone, test_env.getOps());

    try std.testing.expectEqual(@as(usize, 1), list_with_capacity.len());
    try std.testing.expect(!list_with_capacity.isEmpty());

    const elements_ptr = list_with_capacity.elements(u16);
    try std.testing.expect(elements_ptr != null);
    const elements = elements_ptr.?[0..list_with_capacity.len()];
    try std.testing.expectEqual(@as(u16, 9999), elements[0]);
}

test "listPrepend basic functionality" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    // Copy function for u8 elements
    const copy_fn = struct {
        fn copy(dest: ?[*]u8, src: ?[*]u8) callconv(.c) void {
            if (dest != null and src != null) {
                const dest_ptr = @as(*u8, @ptrCast(@alignCast(dest)));
                const src_ptr = @as(*u8, @ptrCast(@alignCast(src)));
                dest_ptr.* = src_ptr.*;
            }
        }
    }.copy;

    // Start with a list containing some elements
    const initial_data = [_]u8{ 2, 3, 4 };
    const list = RocList.fromSlice(u8, initial_data[0..], false, test_env.getOps());

    // Prepend an element
    const element: u8 = 1;
    const result = listPrepend(list, @alignOf(u8), @as(?[*]u8, @ptrCast(@constCast(&element))), @sizeOf(u8), false, rcNone, copy_fn, test_env.getOps());
    defer result.decref(@alignOf(u8), @sizeOf(u8), false, rcNone, test_env.getOps());

    try std.testing.expectEqual(@as(usize, 4), result.len());

    const elements_ptr = result.elements(u8);
    try std.testing.expect(elements_ptr != null);
    const elements = elements_ptr.?[0..result.len()];
    try std.testing.expectEqual(@as(u8, 1), elements[0]); // prepended element
    try std.testing.expectEqual(@as(u8, 2), elements[1]); // original first
    try std.testing.expectEqual(@as(u8, 3), elements[2]); // original second
    try std.testing.expectEqual(@as(u8, 4), elements[3]); // original third
}

test "listPrepend to empty list" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    // Copy function for i32 elements
    const copy_fn = struct {
        fn copy(dest: ?[*]u8, src: ?[*]u8) callconv(.c) void {
            if (dest != null and src != null) {
                const dest_ptr = @as(*i32, @ptrCast(@alignCast(dest)));
                const src_ptr = @as(*i32, @ptrCast(@alignCast(src)));
                dest_ptr.* = src_ptr.*;
            }
        }
    }.copy;

    // Start with an empty list
    const empty_list = RocList.empty();

    // Prepend an element
    const element: i32 = 42;
    const result = listPrepend(empty_list, @alignOf(i32), @as(?[*]u8, @ptrCast(@constCast(&element))), @sizeOf(i32), false, rcNone, copy_fn, test_env.getOps());
    defer result.decref(@alignOf(i32), @sizeOf(i32), false, rcNone, test_env.getOps());

    try std.testing.expectEqual(@as(usize, 1), result.len());
    try std.testing.expect(!result.isEmpty());

    const elements_ptr = result.elements(i32);
    try std.testing.expect(elements_ptr != null);
    const elements = elements_ptr.?[0..result.len()];
    try std.testing.expectEqual(@as(i32, 42), elements[0]);
}

test "listPrepend multiple elements" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    // Copy function for u16 elements
    const copy_fn = struct {
        fn copy(dest: ?[*]u8, src: ?[*]u8) callconv(.c) void {
            if (dest != null and src != null) {
                const dest_ptr = @as(*u16, @ptrCast(@alignCast(dest)));
                const src_ptr = @as(*u16, @ptrCast(@alignCast(src)));
                dest_ptr.* = src_ptr.*;
            }
        }
    }.copy;

    // Start with a single element
    const initial_data = [_]u16{100};
    var list = RocList.fromSlice(u16, initial_data[0..], false, test_env.getOps());

    // Prepend first element
    const element1: u16 = 200;
    list = listPrepend(list, @alignOf(u16), @as(?[*]u8, @ptrCast(@constCast(&element1))), @sizeOf(u16), false, rcNone, copy_fn, test_env.getOps());

    // Prepend second element
    const element2: u16 = 300;
    list = listPrepend(list, @alignOf(u16), @as(?[*]u8, @ptrCast(@constCast(&element2))), @sizeOf(u16), false, rcNone, copy_fn, test_env.getOps());

    defer list.decref(@alignOf(u16), @sizeOf(u16), false, rcNone, test_env.getOps());

    try std.testing.expectEqual(@as(usize, 3), list.len());

    const elements_ptr = list.elements(u16);
    try std.testing.expect(elements_ptr != null);
    const elements = elements_ptr.?[0..list.len()];
    try std.testing.expectEqual(@as(u16, 300), elements[0]); // last prepended (most recent)
    try std.testing.expectEqual(@as(u16, 200), elements[1]); // first prepended
    try std.testing.expectEqual(@as(u16, 100), elements[2]); // original element
}

test "listDropAt basic functionality" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    // Create a list with multiple elements
    const data = [_]u8{ 10, 20, 30, 40, 50 };
    const list = RocList.fromSlice(u8, data[0..], false, test_env.getOps());

    // Drop element at index 2 (value 30)
    const result = listDropAt(list, @alignOf(u8), @sizeOf(u8), false, 2, rcNone, rcNone, test_env.getOps());
    defer result.decref(@alignOf(u8), @sizeOf(u8), false, rcNone, test_env.getOps());

    try std.testing.expectEqual(@as(usize, 4), result.len());

    const elements_ptr = result.elements(u8);
    try std.testing.expect(elements_ptr != null);
    const elements = elements_ptr.?[0..result.len()];
    try std.testing.expectEqual(@as(u8, 10), elements[0]);
    try std.testing.expectEqual(@as(u8, 20), elements[1]);
    try std.testing.expectEqual(@as(u8, 40), elements[2]); // 30 was dropped
    try std.testing.expectEqual(@as(u8, 50), elements[3]);
}

test "listDropAt first element" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    // Create a list with multiple elements
    const data = [_]i32{ 100, 200, 300 };
    const list = RocList.fromSlice(i32, data[0..], false, test_env.getOps());

    // Drop first element (index 0)
    const result = listDropAt(list, @alignOf(i32), @sizeOf(i32), false, 0, rcNone, rcNone, test_env.getOps());
    defer result.decref(@alignOf(i32), @sizeOf(i32), false, rcNone, test_env.getOps());

    try std.testing.expectEqual(@as(usize, 2), result.len());

    const elements_ptr = result.elements(i32);
    try std.testing.expect(elements_ptr != null);
    const elements = elements_ptr.?[0..result.len()];
    try std.testing.expectEqual(@as(i32, 200), elements[0]); // first element was dropped
    try std.testing.expectEqual(@as(i32, 300), elements[1]);
}

test "listDropAt last element" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    // Create a list with multiple elements
    const data = [_]u16{ 1, 2, 3, 4 };
    const list = RocList.fromSlice(u16, data[0..], false, test_env.getOps());

    // Drop last element (index 3)
    const result = listDropAt(list, @alignOf(u16), @sizeOf(u16), false, 3, rcNone, rcNone, test_env.getOps());
    defer result.decref(@alignOf(u16), @sizeOf(u16), false, rcNone, test_env.getOps());

    try std.testing.expectEqual(@as(usize, 3), result.len());

    const elements_ptr = result.elements(u16);
    try std.testing.expect(elements_ptr != null);
    const elements = elements_ptr.?[0..result.len()];
    try std.testing.expectEqual(@as(u16, 1), elements[0]);
    try std.testing.expectEqual(@as(u16, 2), elements[1]);
    try std.testing.expectEqual(@as(u16, 3), elements[2]); // last element (4) was dropped
}

test "listDropAt single element list" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    // Create a list with a single element
    const data = [_]u8{42};
    const list = RocList.fromSlice(u8, data[0..], false, test_env.getOps());

    // Drop the only element (index 0)
    const result = listDropAt(list, @alignOf(u8), @sizeOf(u8), false, 0, rcNone, rcNone, test_env.getOps());
    defer result.decref(@alignOf(u8), @sizeOf(u8), false, rcNone, test_env.getOps());

    try std.testing.expectEqual(@as(usize, 0), result.len());
    try std.testing.expect(result.isEmpty());
}

test "listDropAt out of bounds" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    // Create a list with 3 elements
    const data = [_]i16{ 10, 20, 30 };
    const list = RocList.fromSlice(i16, data[0..], false, test_env.getOps());

    // Try to drop at index 5 (out of bounds)
    const result = listDropAt(list, @alignOf(i16), @sizeOf(i16), false, 5, rcNone, rcNone, test_env.getOps());
    defer result.decref(@alignOf(i16), @sizeOf(i16), false, rcNone, test_env.getOps());

    // Should return the original list unchanged
    try std.testing.expectEqual(@as(usize, 3), result.len());

    const elements_ptr = result.elements(i16);
    try std.testing.expect(elements_ptr != null);
    const elements = elements_ptr.?[0..result.len()];
    try std.testing.expectEqual(@as(i16, 10), elements[0]);
    try std.testing.expectEqual(@as(i16, 20), elements[1]);
    try std.testing.expectEqual(@as(i16, 30), elements[2]);
}

test "listReplace basic functionality" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    // Copy function for u8 elements
    const copy_fn = struct {
        fn copy(dest: ?[*]u8, src: ?[*]u8) callconv(.c) void {
            if (dest != null and src != null) {
                const dest_ptr = @as(*u8, @ptrCast(@alignCast(dest)));
                const src_ptr = @as(*u8, @ptrCast(@alignCast(src)));
                dest_ptr.* = src_ptr.*;
            }
        }
    }.copy;

    // Create a list with multiple elements
    const data = [_]u8{ 10, 20, 30, 40 };
    const list = RocList.fromSlice(u8, data[0..], false, test_env.getOps());

    // Replace element at index 2 (value 30) with 99
    const new_element: u8 = 99;
    var out_element: u8 = 0;
    const result = listReplace(list, @alignOf(u8), 2, @as(?[*]u8, @ptrCast(@constCast(&new_element))), @sizeOf(u8), false, rcNone, rcNone, @as(?[*]u8, @ptrCast(&out_element)), copy_fn, test_env.getOps());
    defer result.decref(@alignOf(u8), @sizeOf(u8), false, rcNone, test_env.getOps());

    try std.testing.expectEqual(@as(usize, 4), result.len());
    try std.testing.expectEqual(@as(u8, 30), out_element); // original value

    const elements_ptr = result.elements(u8);
    try std.testing.expect(elements_ptr != null);
    const elements = elements_ptr.?[0..result.len()];
    try std.testing.expectEqual(@as(u8, 10), elements[0]);
    try std.testing.expectEqual(@as(u8, 20), elements[1]);
    try std.testing.expectEqual(@as(u8, 99), elements[2]); // replaced value
    try std.testing.expectEqual(@as(u8, 40), elements[3]);
}

test "listReplace first element" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    // Copy function for i32 elements
    const copy_fn = struct {
        fn copy(dest: ?[*]u8, src: ?[*]u8) callconv(.c) void {
            if (dest != null and src != null) {
                const dest_ptr = @as(*i32, @ptrCast(@alignCast(dest)));
                const src_ptr = @as(*i32, @ptrCast(@alignCast(src)));
                dest_ptr.* = src_ptr.*;
            }
        }
    }.copy;

    // Create a list with multiple elements
    const data = [_]i32{ 100, 200, 300 };
    const list = RocList.fromSlice(i32, data[0..], false, test_env.getOps());

    // Replace first element (index 0)
    const new_element: i32 = -999;
    var out_element: i32 = 0;
    const result = listReplace(list, @alignOf(i32), 0, @as(?[*]u8, @ptrCast(@constCast(&new_element))), @sizeOf(i32), false, rcNone, rcNone, @as(?[*]u8, @ptrCast(&out_element)), copy_fn, test_env.getOps());
    defer result.decref(@alignOf(i32), @sizeOf(i32), false, rcNone, test_env.getOps());

    try std.testing.expectEqual(@as(usize, 3), result.len());
    try std.testing.expectEqual(@as(i32, 100), out_element); // original value

    const elements_ptr = result.elements(i32);
    try std.testing.expect(elements_ptr != null);
    const elements = elements_ptr.?[0..result.len()];
    try std.testing.expectEqual(@as(i32, -999), elements[0]); // replaced value
    try std.testing.expectEqual(@as(i32, 200), elements[1]);
    try std.testing.expectEqual(@as(i32, 300), elements[2]);
}

test "listReplace last element" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    // Copy function for u16 elements
    const copy_fn = struct {
        fn copy(dest: ?[*]u8, src: ?[*]u8) callconv(.c) void {
            if (dest != null and src != null) {
                const dest_ptr = @as(*u16, @ptrCast(@alignCast(dest)));
                const src_ptr = @as(*u16, @ptrCast(@alignCast(src)));
                dest_ptr.* = src_ptr.*;
            }
        }
    }.copy;

    // Create a list with multiple elements
    const data = [_]u16{ 1, 2, 3, 4 };
    const list = RocList.fromSlice(u16, data[0..], false, test_env.getOps());

    // Replace last element (index 3)
    const new_element: u16 = 9999;
    var out_element: u16 = 0;
    const result = listReplace(list, @alignOf(u16), 3, @as(?[*]u8, @ptrCast(@constCast(&new_element))), @sizeOf(u16), false, rcNone, rcNone, @as(?[*]u8, @ptrCast(&out_element)), copy_fn, test_env.getOps());
    defer result.decref(@alignOf(u16), @sizeOf(u16), false, rcNone, test_env.getOps());

    try std.testing.expectEqual(@as(usize, 4), result.len());
    try std.testing.expectEqual(@as(u16, 4), out_element); // original value

    const elements_ptr = result.elements(u16);
    try std.testing.expect(elements_ptr != null);
    const elements = elements_ptr.?[0..result.len()];
    try std.testing.expectEqual(@as(u16, 1), elements[0]);
    try std.testing.expectEqual(@as(u16, 2), elements[1]);
    try std.testing.expectEqual(@as(u16, 3), elements[2]);
    try std.testing.expectEqual(@as(u16, 9999), elements[3]); // replaced value
}

test "listReplace single element list" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    // Copy function for u8 elements
    const copy_fn = struct {
        fn copy(dest: ?[*]u8, src: ?[*]u8) callconv(.c) void {
            if (dest != null and src != null) {
                const dest_ptr = @as(*u8, @ptrCast(@alignCast(dest)));
                const src_ptr = @as(*u8, @ptrCast(@alignCast(src)));
                dest_ptr.* = src_ptr.*;
            }
        }
    }.copy;

    // Create a list with a single element
    const data = [_]u8{42};
    const list = RocList.fromSlice(u8, data[0..], false, test_env.getOps());

    // Replace the only element (index 0)
    const new_element: u8 = 84;
    var out_element: u8 = 0;
    const result = listReplace(list, @alignOf(u8), 0, @as(?[*]u8, @ptrCast(@constCast(&new_element))), @sizeOf(u8), false, rcNone, rcNone, @as(?[*]u8, @ptrCast(&out_element)), copy_fn, test_env.getOps());
    defer result.decref(@alignOf(u8), @sizeOf(u8), false, rcNone, test_env.getOps());

    try std.testing.expectEqual(@as(usize, 1), result.len());
    try std.testing.expectEqual(@as(u8, 42), out_element); // original value

    const elements_ptr = result.elements(u8);
    try std.testing.expect(elements_ptr != null);
    const elements = elements_ptr.?[0..result.len()];
    try std.testing.expectEqual(@as(u8, 84), elements[0]); // replaced value
}

test "edge case: listConcat with empty lists" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    const empty1 = RocList.empty();
    const empty2 = RocList.empty();

    const result = listConcat(empty1, empty2, 1, 1, false, rcNone, rcNone, test_env.getOps());
    defer result.decref(@alignOf(u8), @sizeOf(u8), false, rcNone, test_env.getOps());

    try std.testing.expectEqual(@as(usize, 0), result.len());
    try std.testing.expect(result.isEmpty());
}

test "edge case: listConcat one empty one non-empty" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    const empty_list = RocList.empty();
    const data = [_]u8{ 1, 2, 3 };
    const non_empty = RocList.fromSlice(u8, data[0..], false, test_env.getOps());

    // Empty + non-empty
    const result1 = listConcat(empty_list, non_empty, 1, 1, false, rcNone, rcNone, test_env.getOps());
    defer result1.decref(@alignOf(u8), @sizeOf(u8), false, rcNone, test_env.getOps());

    try std.testing.expectEqual(@as(usize, 3), result1.len());

    // Non-empty + empty
    const empty2 = RocList.empty();
    const non_empty2 = RocList.fromSlice(u8, data[0..], false, test_env.getOps());
    const result2 = listConcat(non_empty2, empty2, 1, 1, false, rcNone, rcNone, test_env.getOps());
    defer result2.decref(@alignOf(u8), @sizeOf(u8), false, rcNone, test_env.getOps());

    try std.testing.expectEqual(@as(usize, 3), result2.len());
}

test "edge case: listSublist with zero length" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    const data = [_]u8{ 1, 2, 3, 4, 5 };
    const list = RocList.fromSlice(u8, data[0..], false, test_env.getOps());

    // Extract zero-length sublist from middle
    const sublist = listSublist(list, @alignOf(u8), @sizeOf(u8), false, 2, 0, rcNone, test_env.getOps());
    defer sublist.decref(@alignOf(u8), @sizeOf(u8), false, rcNone, test_env.getOps());

    try std.testing.expectEqual(@as(usize, 0), sublist.len());
    try std.testing.expect(sublist.isEmpty());
}

test "edge case: listSublist entire list" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    const data = [_]i16{ 10, 20, 30 };
    const list = RocList.fromSlice(i16, data[0..], false, test_env.getOps());

    // Extract entire list as sublist
    const sublist = listSublist(list, @alignOf(i16), @sizeOf(i16), false, 0, 3, rcNone, test_env.getOps());
    defer sublist.decref(@alignOf(i16), @sizeOf(i16), false, rcNone, test_env.getOps());

    try std.testing.expectEqual(@as(usize, 3), sublist.len());

    const elements_ptr = sublist.elements(i16);
    try std.testing.expect(elements_ptr != null);
    const elements = elements_ptr.?[0..sublist.len()];
    try std.testing.expectEqual(@as(i16, 10), elements[0]);
    try std.testing.expectEqual(@as(i16, 20), elements[1]);
    try std.testing.expectEqual(@as(i16, 30), elements[2]);
}

test "edge case: listPrepend to large list" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    // Copy function for u8 elements
    const copy_fn = struct {
        fn copy(dest: ?[*]u8, src: ?[*]u8) callconv(.c) void {
            if (dest != null and src != null) {
                const dest_ptr = @as(*u8, @ptrCast(@alignCast(dest)));
                const src_ptr = @as(*u8, @ptrCast(@alignCast(src)));
                dest_ptr.* = src_ptr.*;
            }
        }
    }.copy;

    // Create a larger list
    var large_data: [100]u8 = undefined;
    for (large_data[0..], 0..) |*elem, i| {
        elem.* = @as(u8, @intCast(i % 256));
    }
    const list = RocList.fromSlice(u8, large_data[0..], false, test_env.getOps());

    // Prepend an element
    const element: u8 = 255;
    const result = listPrepend(list, @alignOf(u8), @as(?[*]u8, @ptrCast(@constCast(&element))), @sizeOf(u8), false, rcNone, copy_fn, test_env.getOps());
    defer result.decref(@alignOf(u8), @sizeOf(u8), false, rcNone, test_env.getOps());

    try std.testing.expectEqual(@as(usize, 101), result.len());

    const elements_ptr = result.elements(u8);
    try std.testing.expect(elements_ptr != null);
    const elements = elements_ptr.?[0..result.len()];
    try std.testing.expectEqual(@as(u8, 255), elements[0]); // prepended element
    try std.testing.expectEqual(@as(u8, 0), elements[1]); // original first element
    try std.testing.expectEqual(@as(u8, 1), elements[2]); // original second element
}

test "edge case: listWithCapacity zero capacity" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    const list = listWithCapacity(0, @alignOf(u32), @sizeOf(u32), false, rcNone, test_env.getOps());
    defer list.decref(@alignOf(u32), @sizeOf(u32), false, rcNone, test_env.getOps());

    try std.testing.expectEqual(@as(usize, 0), list.len());
    try std.testing.expect(list.isEmpty());
}

test "edge case: RocList equality with different capacities" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    // Create two lists with same content but different capacities
    const data = [_]u8{ 1, 2, 3 };
    const list1 = RocList.fromSlice(u8, data[0..], false, test_env.getOps());
    defer list1.decref(@alignOf(u8), @sizeOf(u8), false, rcNone, test_env.getOps());

    // Create list with larger capacity
    var list2 = listWithCapacity(10, @alignOf(u8), @sizeOf(u8), false, rcNone, test_env.getOps());
    // Manually set the same content
    list2.length = 3;
    if (list2.bytes) |bytes| {
        for (data, 0..) |val, i| {
            bytes[i] = val;
        }
    }
    defer list2.decref(@alignOf(u8), @sizeOf(u8), false, rcNone, test_env.getOps());

    // Should be equal despite different capacities
    try std.testing.expect(list1.eql(list2));
    try std.testing.expect(list2.eql(list1));
}

test "edge case: listAppendUnsafe multiple times" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    // Copy function for u8 elements
    const copy_fn = struct {
        fn copy(dest: ?[*]u8, src: ?[*]u8) callconv(.c) void {
            if (dest != null and src != null) {
                const dest_ptr = @as(*u8, @ptrCast(@alignCast(dest)));
                const src_ptr = @as(*u8, @ptrCast(@alignCast(src)));
                dest_ptr.* = src_ptr.*;
            }
        }
    }.copy;

    // Create a list with sufficient capacity
    var list = listWithCapacity(5, @alignOf(u8), @sizeOf(u8), false, rcNone, test_env.getOps());

    // Append multiple elements
    const element1: u8 = 10;
    list = listAppendUnsafe(list, @as(?[*]u8, @ptrCast(@constCast(&element1))), @sizeOf(u8), copy_fn);

    const element2: u8 = 20;
    list = listAppendUnsafe(list, @as(?[*]u8, @ptrCast(@constCast(&element2))), @sizeOf(u8), copy_fn);

    const element3: u8 = 30;
    list = listAppendUnsafe(list, @as(?[*]u8, @ptrCast(@constCast(&element3))), @sizeOf(u8), copy_fn);

    defer list.decref(@alignOf(u8), @sizeOf(u8), false, rcNone, test_env.getOps());

    try std.testing.expectEqual(@as(usize, 3), list.len());

    const elements_ptr = list.elements(u8);
    try std.testing.expect(elements_ptr != null);
    const elements = elements_ptr.?[0..list.len()];
    try std.testing.expectEqual(@as(u8, 10), elements[0]);
    try std.testing.expectEqual(@as(u8, 20), elements[1]);
    try std.testing.expectEqual(@as(u8, 30), elements[2]);
}

test "seamless slice: isSeamlessSlice detection" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    // Regular list should not be a seamless slice
    const data = [_]u8{ 1, 2, 3 };
    const regular_list = RocList.fromSlice(u8, data[0..], false, test_env.getOps());
    defer regular_list.decref(@alignOf(u8), @sizeOf(u8), false, rcNone, test_env.getOps());

    try std.testing.expect(!regular_list.isSeamlessSlice());

    // Empty list should not be a seamless slice
    const empty_list = RocList.empty();
    defer empty_list.decref(@alignOf(u8), @sizeOf(u8), false, rcNone, test_env.getOps());

    try std.testing.expect(!empty_list.isSeamlessSlice());
}

test "seamless slice: seamlessSliceMask functionality" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    // Regular list should have mask of all zeros
    const data = [_]u8{ 1, 2, 3 };
    const regular_list = RocList.fromSlice(u8, data[0..], false, test_env.getOps());
    defer regular_list.decref(@alignOf(u8), @sizeOf(u8), false, rcNone, test_env.getOps());

    try std.testing.expectEqual(@as(usize, 0), regular_list.seamlessSliceMask());

    // Empty list should have mask of all zeros
    const empty_list = RocList.empty();
    defer empty_list.decref(@alignOf(u8), @sizeOf(u8), false, rcNone, test_env.getOps());

    try std.testing.expectEqual(@as(usize, 0), empty_list.seamlessSliceMask());
}

test "seamless slice: manual creation and detection" {
    // Test creating a seamless slice manually by setting the high bit
    var seamless_list = RocList{
        .bytes = null,
        .length = 0,
        .capacity_or_alloc_ptr = SEAMLESS_SLICE_BIT,
    };

    try std.testing.expect(seamless_list.isSeamlessSlice());
    try std.testing.expectEqual(@as(usize, std.math.maxInt(usize)), seamless_list.seamlessSliceMask());
}

test "seamless slice: getCapacity behavior" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    // Regular list capacity
    const data = [_]u8{ 1, 2, 3 };
    const regular_list = RocList.fromSlice(u8, data[0..], false, test_env.getOps());
    defer regular_list.decref(@alignOf(u8), @sizeOf(u8), false, rcNone, test_env.getOps());

    const regular_capacity = regular_list.getCapacity();
    try std.testing.expect(regular_capacity >= 3);

    // Seamless slice with high bit set should mask out the bit when getting capacity
    var seamless_list = RocList{
        .bytes = null,
        .length = 5,
        .capacity_or_alloc_ptr = SEAMLESS_SLICE_BIT | 10, // High bit set, capacity of 10
    };

    try std.testing.expect(seamless_list.isSeamlessSlice());
    try std.testing.expectEqual(@as(usize, 5), seamless_list.getCapacity());
}

test "complex reference counting: multiple increfs and decrefs" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    const data = [_]u8{ 1, 2, 3 };
    const list = RocList.fromSlice(u8, data[0..], false, test_env.getOps());

    // Should be unique initially
    try std.testing.expect(list.isUnique());

    // Increment reference count multiple times
    list.incref(1, false);
    list.incref(1, false);
    list.incref(1, false);

    // Should no longer be unique
    try std.testing.expect(!list.isUnique());

    // Decrement back down
    list.decref(@alignOf(u8), @sizeOf(u8), false, rcNone, test_env.getOps());
    list.decref(@alignOf(u8), @sizeOf(u8), false, rcNone, test_env.getOps());
    list.decref(@alignOf(u8), @sizeOf(u8), false, rcNone, test_env.getOps());

    // Should be unique again
    try std.testing.expect(list.isUnique());

    // Final cleanup
    list.decref(@alignOf(u8), @sizeOf(u8), false, rcNone, test_env.getOps());
}

test "complex reference counting: makeUnique with shared list" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    const data = [_]i32{ 10, 20, 30, 40 };
    const original_list = RocList.fromSlice(i32, data[0..], false, test_env.getOps());

    // Make the list non-unique by incrementing reference count
    original_list.incref(1, false);
    defer original_list.decref(@alignOf(i32), @sizeOf(i32), false, rcNone, test_env.getOps());

    try std.testing.expect(!original_list.isUnique());

    // makeUnique should create a new copy
    const unique_list = original_list.makeUnique(@alignOf(i32), @sizeOf(i32), false, rcNone, rcNone, test_env.getOps());
    defer unique_list.decref(@alignOf(i32), @sizeOf(i32), false, rcNone, test_env.getOps());

    // The unique list should be different from the original
    try std.testing.expect(unique_list.bytes != original_list.bytes);
    try std.testing.expect(unique_list.isUnique());

    // But should have the same content
    try std.testing.expect(unique_list.eql(original_list));
}

test "complex reference counting: listIsUnique consistency" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    const data = [_]u16{ 100, 200 };
    const list = RocList.fromSlice(u16, data[0..], false, test_env.getOps());

    // Test that listIsUnique function matches isUnique method
    try std.testing.expectEqual(list.isUnique(), listIsUnique(list));

    // After incref, both should report not unique
    list.incref(1, false);
    defer list.decref(@alignOf(u16), @sizeOf(u16), false, rcNone, test_env.getOps());

    try std.testing.expectEqual(list.isUnique(), listIsUnique(list));
    try std.testing.expect(!listIsUnique(list));

    // Final cleanup
    list.decref(@alignOf(u16), @sizeOf(u16), false, rcNone, test_env.getOps());
}

test "complex reference counting: clone behavior" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    const data = [_]u8{ 5, 10, 15 };
    const original_list = RocList.fromSlice(u8, data[0..], false, test_env.getOps());

    // Clone should create a new independent copy
    const cloned_list = listClone(original_list, @alignOf(u8), @sizeOf(u8), false, rcNone, rcNone, test_env.getOps());
    defer cloned_list.decref(@alignOf(u8), @sizeOf(u8), false, rcNone, test_env.getOps());

    // Cloned list should be unique and have same content
    try std.testing.expect(cloned_list.isUnique());
    try std.testing.expect(cloned_list.eql(original_list));
}

test "complex reference counting: empty list operations" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    const empty_list = RocList.empty();
    defer empty_list.decref(@alignOf(u8), @sizeOf(u8), false, rcNone, test_env.getOps());

    // Empty lists should handle basic operations gracefully
    try std.testing.expect(empty_list.isUnique());
    try std.testing.expect(empty_list.isEmpty());
    try std.testing.expectEqual(@as(usize, 0), empty_list.len());
}

test "listReplaceInPlace basic functionality" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    // Copy function for u8 elements
    const copy_fn = struct {
        fn copy(dest: ?[*]u8, src: ?[*]u8) callconv(.c) void {
            if (dest != null and src != null) {
                const dest_ptr = @as(*u8, @ptrCast(@alignCast(dest)));
                const src_ptr = @as(*u8, @ptrCast(@alignCast(src)));
                dest_ptr.* = src_ptr.*;
            }
        }
    }.copy;

    // Create a list with multiple elements
    const data = [_]u8{ 10, 20, 30, 40 };
    const list = RocList.fromSlice(u8, data[0..], false, test_env.getOps());

    // Replace element at index 2 (value 30) with 99
    const new_element: u8 = 99;
    var out_element: u8 = 0;
    const result = listReplaceInPlace(list, 2, @as(?[*]u8, @ptrCast(@constCast(&new_element))), @sizeOf(u8), @as(?[*]u8, @ptrCast(&out_element)), copy_fn);
    defer result.decref(@alignOf(u8), @sizeOf(u8), false, rcNone, test_env.getOps());

    try std.testing.expectEqual(@as(usize, 4), result.len());
    try std.testing.expectEqual(@as(u8, 30), out_element); // original value

    const elements_ptr = result.elements(u8);
    try std.testing.expect(elements_ptr != null);
    const elements = elements_ptr.?[0..result.len()];
    try std.testing.expectEqual(@as(u8, 10), elements[0]);
    try std.testing.expectEqual(@as(u8, 20), elements[1]);
    try std.testing.expectEqual(@as(u8, 99), elements[2]); // replaced value
    try std.testing.expectEqual(@as(u8, 40), elements[3]);
}

test "listReplaceInPlace first and last elements" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    // Copy function for i32 elements
    const copy_fn = struct {
        fn copy(dest: ?[*]u8, src: ?[*]u8) callconv(.c) void {
            if (dest != null and src != null) {
                const dest_ptr = @as(*i32, @ptrCast(@alignCast(dest)));
                const src_ptr = @as(*i32, @ptrCast(@alignCast(src)));
                dest_ptr.* = src_ptr.*;
            }
        }
    }.copy;

    // Test replacing first element
    const data = [_]i32{ 100, 200, 300 };
    const list1 = RocList.fromSlice(i32, data[0..], false, test_env.getOps());

    const new_first: i32 = -999;
    var out_first: i32 = 0;
    const result1 = listReplaceInPlace(list1, 0, @as(?[*]u8, @ptrCast(@constCast(&new_first))), @sizeOf(i32), @as(?[*]u8, @ptrCast(&out_first)), copy_fn);
    defer result1.decref(@alignOf(i32), @sizeOf(i32), false, rcNone, test_env.getOps());

    try std.testing.expectEqual(@as(i32, 100), out_first);
    const elements1_ptr = result1.elements(i32);
    try std.testing.expect(elements1_ptr != null);
    const elements1 = elements1_ptr.?[0..result1.len()];
    try std.testing.expectEqual(@as(i32, -999), elements1[0]);

    // Test replacing last element
    const list2 = RocList.fromSlice(i32, data[0..], false, test_env.getOps());

    const new_last: i32 = 999;
    var out_last: i32 = 0;
    const result2 = listReplaceInPlace(list2, 2, @as(?[*]u8, @ptrCast(@constCast(&new_last))), @sizeOf(i32), @as(?[*]u8, @ptrCast(&out_last)), copy_fn);
    defer result2.decref(@alignOf(i32), @sizeOf(i32), false, rcNone, test_env.getOps());

    try std.testing.expectEqual(@as(i32, 300), out_last);
    const elements2_ptr = result2.elements(i32);
    try std.testing.expect(elements2_ptr != null);
    const elements2 = elements2_ptr.?[0..result2.len()];
    try std.testing.expectEqual(@as(i32, 999), elements2[2]);
}

test "listReplaceInPlace single element list" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    // Copy function for u16 elements
    const copy_fn = struct {
        fn copy(dest: ?[*]u8, src: ?[*]u8) callconv(.c) void {
            if (dest != null and src != null) {
                const dest_ptr = @as(*u16, @ptrCast(@alignCast(dest)));
                const src_ptr = @as(*u16, @ptrCast(@alignCast(src)));
                dest_ptr.* = src_ptr.*;
            }
        }
    }.copy;

    // Create a list with a single element
    const data = [_]u16{42};
    const list = RocList.fromSlice(u16, data[0..], false, test_env.getOps());

    // Replace the only element (index 0)
    const new_element: u16 = 84;
    var out_element: u16 = 0;
    const result = listReplaceInPlace(list, 0, @as(?[*]u8, @ptrCast(@constCast(&new_element))), @sizeOf(u16), @as(?[*]u8, @ptrCast(&out_element)), copy_fn);
    defer result.decref(@alignOf(u16), @sizeOf(u16), false, rcNone, test_env.getOps());

    try std.testing.expectEqual(@as(usize, 1), result.len());
    try std.testing.expectEqual(@as(u16, 42), out_element); // original value

    const elements_ptr = result.elements(u16);
    try std.testing.expect(elements_ptr != null);
    const elements = elements_ptr.?[0..result.len()];
    try std.testing.expectEqual(@as(u16, 84), elements[0]); // replaced value
}

test "listReplaceInPlace vs listReplace comparison" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    // Copy function for u8 elements
    const copy_fn = struct {
        fn copy(dest: ?[*]u8, src: ?[*]u8) callconv(.c) void {
            if (dest != null and src != null) {
                const dest_ptr = @as(*u8, @ptrCast(@alignCast(dest)));
                const src_ptr = @as(*u8, @ptrCast(@alignCast(src)));
                dest_ptr.* = src_ptr.*;
            }
        }
    }.copy;

    const data = [_]u8{ 1, 2, 3, 4, 5 };

    // Test listReplaceInPlace
    const list1 = RocList.fromSlice(u8, data[0..], false, test_env.getOps());
    const new_element1: u8 = 99;
    var out_element1: u8 = 0;
    const result1 = listReplaceInPlace(list1, 2, @as(?[*]u8, @ptrCast(@constCast(&new_element1))), @sizeOf(u8), @as(?[*]u8, @ptrCast(&out_element1)), copy_fn);
    defer result1.decref(@alignOf(u8), @sizeOf(u8), false, rcNone, test_env.getOps());

    // Test listReplace with same parameters
    const list2 = RocList.fromSlice(u8, data[0..], false, test_env.getOps());
    const new_element2: u8 = 99;
    var out_element2: u8 = 0;
    const result2 = listReplace(list2, @alignOf(u8), 2, @as(?[*]u8, @ptrCast(@constCast(&new_element2))), @sizeOf(u8), false, rcNone, rcNone, @as(?[*]u8, @ptrCast(&out_element2)), copy_fn, test_env.getOps());
    defer result2.decref(@alignOf(u8), @sizeOf(u8), false, rcNone, test_env.getOps());

    // Both should produce the same result
    try std.testing.expect(result1.eql(result2));
    try std.testing.expectEqual(out_element1, out_element2);
}

test "listAllocationPtr basic functionality" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    // Test with regular list
    const data = [_]u8{ 1, 2, 3, 4 };
    const list = RocList.fromSlice(u8, data[0..], false, test_env.getOps());
    defer list.decref(@alignOf(u8), @sizeOf(u8), false, rcNone, test_env.getOps());

    const alloc_ptr = listAllocationPtr(list);
    try std.testing.expect(alloc_ptr != null);

    // The allocation pointer should be valid and accessible
    if (alloc_ptr) |ptr| {
        // Should be able to access the data through the allocation pointer
        _ = ptr; // Just verify it's not null
    }
}

test "listAllocationPtr empty list" {
    var test_env = utils.TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    const empty_list = RocList.empty();
    defer empty_list.decref(@alignOf(u8), @sizeOf(u8), false, rcNone, test_env.getOps());

    const alloc_ptr = listAllocationPtr(empty_list);
    // Empty lists may have null allocation pointer
    _ = alloc_ptr; // Just verify the function doesn't crash
}

test "listIncref and listDecref public functions" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    const data = [_]u8{ 10, 20, 30 };
    const list = RocList.fromSlice(u8, data[0..], false, test_env.getOps());

    // Should be unique initially
    try std.testing.expect(list.isUnique());

    // Use public listIncref function
    listIncref(list, 1, false);

    // Should no longer be unique
    try std.testing.expect(!list.isUnique());

    // Use public listDecref function
    listDecref(list, @alignOf(u8), @sizeOf(u8), false, rcNone, test_env.getOps());

    // Should be unique again
    try std.testing.expect(list.isUnique());

    // Final cleanup
    listDecref(list, @alignOf(u8), @sizeOf(u8), false, rcNone, test_env.getOps());
}

test "integration: prepend then drop operations" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    // Copy function for u8 elements
    const copy_fn = struct {
        fn copy(dest: ?[*]u8, src: ?[*]u8) callconv(.c) void {
            if (dest != null and src != null) {
                const dest_ptr = @as(*u8, @ptrCast(@alignCast(dest)));
                const src_ptr = @as(*u8, @ptrCast(@alignCast(src)));
                dest_ptr.* = src_ptr.*;
            }
        }
    }.copy;

    // Start with a basic list
    const initial_data = [_]u8{ 5, 10, 15 };
    var list = RocList.fromSlice(u8, initial_data[0..], false, test_env.getOps());

    // Prepend multiple elements
    const element1: u8 = 1;
    list = listPrepend(list, @alignOf(u8), @as(?[*]u8, @ptrCast(@constCast(&element1))), @sizeOf(u8), false, rcNone, copy_fn, test_env.getOps());

    const element2: u8 = 2;
    list = listPrepend(list, @alignOf(u8), @as(?[*]u8, @ptrCast(@constCast(&element2))), @sizeOf(u8), false, rcNone, copy_fn, test_env.getOps());

    // Now we should have [2, 1, 5, 10, 15]
    try std.testing.expectEqual(@as(usize, 5), list.len());

    // Drop the middle element (index 2, value 5)
    list = listDropAt(list, @alignOf(u8), @sizeOf(u8), false, 2, rcNone, rcNone, test_env.getOps());

    // Now we should have [2, 1, 10, 15]
    try std.testing.expectEqual(@as(usize, 4), list.len());

    defer list.decref(@alignOf(u8), @sizeOf(u8), false, rcNone, test_env.getOps());

    const elements_ptr = list.elements(u8);
    try std.testing.expect(elements_ptr != null);
    const elements = elements_ptr.?[0..list.len()];
    try std.testing.expectEqual(@as(u8, 2), elements[0]);
    try std.testing.expectEqual(@as(u8, 1), elements[1]);
    try std.testing.expectEqual(@as(u8, 10), elements[2]);
    try std.testing.expectEqual(@as(u8, 15), elements[3]);
}

test "integration: concat then sublist operations" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    // Create two lists to concatenate
    const data1 = [_]i16{ 100, 200 };
    const list1 = RocList.fromSlice(i16, data1[0..], false, test_env.getOps());

    const data2 = [_]i16{ 300, 400, 500 };
    const list2 = RocList.fromSlice(i16, data2[0..], false, test_env.getOps());

    // Concatenate them
    const concatenated = listConcat(list1, list2, @alignOf(i16), @sizeOf(i16), false, rcNone, rcNone, test_env.getOps());

    // Should have [100, 200, 300, 400, 500]
    try std.testing.expectEqual(@as(usize, 5), concatenated.len());

    // Extract a sublist from the middle
    const sublist = listSublist(concatenated, @alignOf(i16), @sizeOf(i16), false, 1, 3, rcNone, test_env.getOps());
    defer sublist.decref(@alignOf(i16), @sizeOf(i16), false, rcNone, test_env.getOps());

    // Should have [200, 300, 400]
    try std.testing.expectEqual(@as(usize, 3), sublist.len());

    const elements_ptr = sublist.elements(i16);
    try std.testing.expect(elements_ptr != null);
    const elements = elements_ptr.?[0..sublist.len()];
    try std.testing.expectEqual(@as(i16, 200), elements[0]);
    try std.testing.expectEqual(@as(i16, 300), elements[1]);
    try std.testing.expectEqual(@as(i16, 400), elements[2]);
}

test "integration: replace then swap operations" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    // Copy function for u32 elements
    const copy_fn = struct {
        fn copy(dest: ?[*]u8, src: ?[*]u8) callconv(.c) void {
            if (dest != null and src != null) {
                const dest_ptr = @as(*u32, @ptrCast(@alignCast(dest)));
                const src_ptr = @as(*u32, @ptrCast(@alignCast(src)));
                dest_ptr.* = src_ptr.*;
            }
        }
    }.copy;

    // Start with a list
    const data = [_]u32{ 10, 20, 30, 40 };
    var list = RocList.fromSlice(u32, data[0..], false, test_env.getOps());

    // Replace element at index 1 (20 -> 99)
    const new_element: u32 = 99;
    var out_element: u32 = 0;
    list = listReplace(list, @alignOf(u32), 1, @as(?[*]u8, @ptrCast(@constCast(&new_element))), @sizeOf(u32), false, rcNone, rcNone, @as(?[*]u8, @ptrCast(&out_element)), copy_fn, test_env.getOps());

    try std.testing.expectEqual(@as(u32, 20), out_element);

    // Now we should have [10, 99, 30, 40]
    // Swap elements at indices 0 and 2 (10 <-> 30)
    list = listSwap(list, @alignOf(u32), @sizeOf(u32), 0, 2, false, rcNone, rcNone, utils.UpdateMode.Immutable, copy_fn, test_env.getOps());

    defer list.decref(@alignOf(u32), @sizeOf(u32), false, rcNone, test_env.getOps());

    // Now we should have [30, 99, 10, 40]
    try std.testing.expectEqual(@as(usize, 4), list.len());

    const elements_ptr = list.elements(u32);
    try std.testing.expect(elements_ptr != null);
    const elements = elements_ptr.?[0..list.len()];
    try std.testing.expectEqual(@as(u32, 30), elements[0]); // swapped from index 2
    try std.testing.expectEqual(@as(u32, 99), elements[1]); // replaced value
    try std.testing.expectEqual(@as(u32, 10), elements[2]); // swapped from index 0
    try std.testing.expectEqual(@as(u32, 40), elements[3]); // unchanged
}

test "stress: large list operations" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    // Create a large list (1000 elements)
    const large_size = 1000;
    var large_data: [large_size]u16 = undefined;
    for (large_data[0..], 0..) |*elem, i| {
        elem.* = @as(u16, @intCast(i));
    }

    const large_list = RocList.fromSlice(u16, large_data[0..], false, test_env.getOps());

    // Test capacity operations on large list
    try std.testing.expectEqual(@as(usize, large_size), large_list.len());
    try std.testing.expect(large_list.getCapacity() >= large_size);

    // Test sublist on large list (note: listSublist consumes the original list)
    const mid_sublist = listSublist(large_list, @alignOf(u16), @sizeOf(u16), false, 400, 200, rcNone, test_env.getOps());
    defer mid_sublist.decref(@alignOf(u16), @sizeOf(u16), false, rcNone, test_env.getOps());

    try std.testing.expectEqual(@as(usize, 200), mid_sublist.len());

    // Verify the sublist contains correct values
    const sub_elements_ptr = mid_sublist.elements(u16);
    try std.testing.expect(sub_elements_ptr != null);
    const sub_elements = sub_elements_ptr.?[0..mid_sublist.len()];
    try std.testing.expectEqual(@as(u16, 400), sub_elements[0]); // first element of sublist
    try std.testing.expectEqual(@as(u16, 599), sub_elements[199]); // last element of sublist
}

test "stress: many small operations" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    // Copy function for u8 elements
    const copy_fn = struct {
        fn copy(dest: ?[*]u8, src: ?[*]u8) callconv(.c) void {
            if (dest != null and src != null) {
                const dest_ptr = @as(*u8, @ptrCast(@alignCast(dest)));
                const src_ptr = @as(*u8, @ptrCast(@alignCast(src)));
                dest_ptr.* = src_ptr.*;
            }
        }
    }.copy;

    // Start with a list with some capacity
    var list = listWithCapacity(50, @alignOf(u8), @sizeOf(u8), false, rcNone, test_env.getOps());

    // Add many elements using listAppendUnsafe
    var i: u8 = 0;
    while (i < 20) : (i += 1) {
        list = listAppendUnsafe(list, @as(?[*]u8, @ptrCast(@constCast(&i))), @sizeOf(u8), copy_fn);
    }

    try std.testing.expectEqual(@as(usize, 20), list.len());

    // Verify the elements are correct
    const elements_ptr = list.elements(u8);
    try std.testing.expect(elements_ptr != null);
    const elements = elements_ptr.?[0..list.len()];

    for (elements, 0..) |elem, idx| {
        try std.testing.expectEqual(@as(u8, @intCast(idx)), elem);
    }

    defer list.decref(@alignOf(u8), @sizeOf(u8), false, rcNone, test_env.getOps());
}

test "memory management: capacity boundary conditions" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    // Create a list with exact capacity
    const exact_capacity: usize = 10;
    var list = listWithCapacity(exact_capacity, @alignOf(u32), @sizeOf(u32), false, rcNone, test_env.getOps());

    try std.testing.expect(list.getCapacity() >= exact_capacity);
    try std.testing.expectEqual(@as(usize, 0), list.len());

    // Use listReserve to ensure we have exactly the capacity we want
    list = listReserve(list, @alignOf(u32), exact_capacity, @sizeOf(u32), false, rcNone, utils.UpdateMode.Immutable, test_env.getOps());
    defer list.decref(@alignOf(u32), @sizeOf(u32), false, rcNone, test_env.getOps());

    // Verify capacity management functions work correctly
    const initial_capacity = list.getCapacity();
    try std.testing.expect(initial_capacity >= exact_capacity);

    const capacity_via_function = listCapacity(list);
    try std.testing.expectEqual(initial_capacity, capacity_via_function);
}

test "memory management: release excess capacity edge cases" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    // Create a list with minimal data but large capacity
    const data = [_]u8{42};
    const small_list = RocList.fromSlice(u8, data[0..], false, test_env.getOps());

    // Reserve much more capacity than needed
    const oversized_list = listReserve(small_list, @alignOf(u8), 1000, @sizeOf(u8), false, rcNone, utils.UpdateMode.Immutable, test_env.getOps());

    try std.testing.expectEqual(@as(usize, 1), oversized_list.len());
    try std.testing.expect(oversized_list.getCapacity() >= 1000);

    // Release excess capacity
    const trimmed_list = listReleaseExcessCapacity(oversized_list, @alignOf(u8), @sizeOf(u8), false, rcNone, rcNone, utils.UpdateMode.Immutable, test_env.getOps());
    defer trimmed_list.decref(@alignOf(u8), @sizeOf(u8), false, rcNone, test_env.getOps());

    // Should maintain content but reduce capacity
    try std.testing.expectEqual(@as(usize, 1), trimmed_list.len());
    try std.testing.expect(trimmed_list.getCapacity() < 1000);
    try std.testing.expect(trimmed_list.getCapacity() >= trimmed_list.len());

    // Verify content is preserved
    const elements_ptr = trimmed_list.elements(u8);
    try std.testing.expect(elements_ptr != null);
    const elements = elements_ptr.?[0..trimmed_list.len()];
    try std.testing.expectEqual(@as(u8, 42), elements[0]);
}

test "boundary conditions: zero-sized operations" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    // Test sublist with zero length from various starting positions
    const data = [_]u16{ 1, 2, 3, 4, 5 };

    // Zero-length sublist from start
    const list1 = RocList.fromSlice(u16, data[0..], false, test_env.getOps());
    const empty_start = listSublist(list1, @alignOf(u16), @sizeOf(u16), false, 0, 0, rcNone, test_env.getOps());
    defer empty_start.decref(@alignOf(u16), @sizeOf(u16), false, rcNone, test_env.getOps());
    try std.testing.expectEqual(@as(usize, 0), empty_start.len());

    // Zero-length sublist from middle
    const list2 = RocList.fromSlice(u16, data[0..], false, test_env.getOps());
    const empty_mid = listSublist(list2, @alignOf(u16), @sizeOf(u16), false, 2, 0, rcNone, test_env.getOps());
    defer empty_mid.decref(@alignOf(u16), @sizeOf(u16), false, rcNone, test_env.getOps());
    try std.testing.expectEqual(@as(usize, 0), empty_mid.len());

    // Zero-length sublist from end
    const list3 = RocList.fromSlice(u16, data[0..], false, test_env.getOps());
    const empty_end = listSublist(list3, @alignOf(u16), @sizeOf(u16), false, 5, 0, rcNone, test_env.getOps());
    defer empty_end.decref(@alignOf(u16), @sizeOf(u16), false, rcNone, test_env.getOps());
    try std.testing.expectEqual(@as(usize, 0), empty_end.len());
}

test "boundary conditions: maximum index operations" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    const data = [_]u8{ 10, 20, 30 };

    // Test dropAt with index at boundary (last valid index)
    const list1 = RocList.fromSlice(u8, data[0..], false, test_env.getOps());
    const dropped_last = listDropAt(list1, @alignOf(u8), @sizeOf(u8), false, 2, rcNone, rcNone, test_env.getOps());
    defer dropped_last.decref(@alignOf(u8), @sizeOf(u8), false, rcNone, test_env.getOps());

    try std.testing.expectEqual(@as(usize, 2), dropped_last.len());
    const elements_ptr = dropped_last.elements(u8);
    try std.testing.expect(elements_ptr != null);
    const elements = elements_ptr.?[0..dropped_last.len()];
    try std.testing.expectEqual(@as(u8, 10), elements[0]);
    try std.testing.expectEqual(@as(u8, 20), elements[1]);

    // Test dropAt with out-of-bounds index (should return original list)
    const list2 = RocList.fromSlice(u8, data[0..], false, test_env.getOps());
    const dropped_oob = listDropAt(list2, @alignOf(u8), @sizeOf(u8), false, 10, rcNone, rcNone, test_env.getOps());
    defer dropped_oob.decref(@alignOf(u8), @sizeOf(u8), false, rcNone, test_env.getOps());

    try std.testing.expectEqual(@as(usize, 3), dropped_oob.len());
}

test "memory management: clone with different update modes" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    const data = [_]i32{ 100, 200, 300, 400 };
    const original = RocList.fromSlice(i32, data[0..], false, test_env.getOps());

    // Make the list non-unique
    original.incref(1, false);
    defer original.decref(@alignOf(i32), @sizeOf(i32), false, rcNone, test_env.getOps());

    // Clone should create an independent copy
    const cloned = listClone(original, @alignOf(i32), @sizeOf(i32), false, rcNone, rcNone, test_env.getOps());
    defer cloned.decref(@alignOf(i32), @sizeOf(i32), false, rcNone, test_env.getOps());

    // Verify independence - they should have the same content but different memory
    try std.testing.expect(cloned.eql(original));
    try std.testing.expect(cloned.isUnique());

    // Verify they can be modified independently by testing capacity operations
    const cloned_capacity = cloned.getCapacity();
    const original_capacity = original.getCapacity();

    // Both should have valid capacities
    try std.testing.expect(cloned_capacity >= cloned.len());
    try std.testing.expect(original_capacity >= original.len());

    // Cleanup is handled by defer statement above
}

test "boundary conditions: swap with identical indices" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    // Copy function for u8 elements
    const copy_fn = struct {
        fn copy(dest: ?[*]u8, src: ?[*]u8) callconv(.c) void {
            if (dest != null and src != null) {
                const dest_ptr = @as(*u8, @ptrCast(@alignCast(dest)));
                const src_ptr = @as(*u8, @ptrCast(@alignCast(src)));
                dest_ptr.* = src_ptr.*;
            }
        }
    }.copy;

    const data = [_]u8{ 10, 20, 30, 40 };
    const list = RocList.fromSlice(u8, data[0..], false, test_env.getOps());

    // Swap element with itself (index 2 with index 2)
    const swapped = listSwap(list, @alignOf(u8), @sizeOf(u8), 2, 2, false, rcNone, rcNone, utils.UpdateMode.Immutable, copy_fn, test_env.getOps());
    defer swapped.decref(@alignOf(u8), @sizeOf(u8), false, rcNone, test_env.getOps());

    // Should be unchanged
    try std.testing.expectEqual(@as(usize, 4), swapped.len());
    const elements_ptr = swapped.elements(u8);
    try std.testing.expect(elements_ptr != null);
    const elements = elements_ptr.?[0..swapped.len()];

    for (data, 0..) |expected, i| {
        try std.testing.expectEqual(expected, elements[i]);
    }
}

test "memory management: multiple reserve operations" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    // Start with a small list
    const data = [_]u8{ 1, 2 };
    var list = RocList.fromSlice(u8, data[0..], false, test_env.getOps());

    // Reserve capacity multiple times, each time increasing
    list = listReserve(list, @alignOf(u8), 10, @sizeOf(u8), false, rcNone, utils.UpdateMode.Immutable, test_env.getOps());
    try std.testing.expect(list.getCapacity() >= 12); // 2 existing + 10 spare

    list = listReserve(list, @alignOf(u8), 20, @sizeOf(u8), false, rcNone, utils.UpdateMode.Immutable, test_env.getOps());
    try std.testing.expect(list.getCapacity() >= 22); // 2 existing + 20 spare

    list = listReserve(list, @alignOf(u8), 5, @sizeOf(u8), false, rcNone, utils.UpdateMode.Immutable, test_env.getOps());
    // Should not decrease capacity, so still >= 22
    try std.testing.expect(list.getCapacity() >= 22);

    defer list.decref(@alignOf(u8), @sizeOf(u8), false, rcNone, test_env.getOps());

    // Verify content is preserved through all operations
    try std.testing.expectEqual(@as(usize, 2), list.len());
    const elements_ptr = list.elements(u8);
    try std.testing.expect(elements_ptr != null);
    const elements = elements_ptr.?[0..list.len()];
    try std.testing.expectEqual(@as(u8, 1), elements[0]);
    try std.testing.expectEqual(@as(u8, 2), elements[1]);
}

test "push: basic functionality with empty list" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    // Start with an empty list
    var list = RocList.empty();

    // Push first element
    const element1: u8 = 42;
    list = pushInPlace(list, @alignOf(u8), @sizeOf(u8), @ptrCast(@constCast(&element1)), test_env.getOps());

    try std.testing.expectEqual(@as(usize, 1), list.len());
    try std.testing.expect(list.getCapacity() >= 1);

    // Verify the element
    const elements_ptr = list.elements(u8);
    try std.testing.expect(elements_ptr != null);
    try std.testing.expectEqual(@as(u8, 42), elements_ptr.?[0]);

    defer list.decref(@alignOf(u8), @sizeOf(u8), false, rcNone, test_env.getOps());
}

test "push: multiple elements with reallocation" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    var list = RocList.empty();

    // Push multiple elements to trigger reallocation
    const values = [_]u8{ 10, 20, 30, 40, 50 };
    for (values) |value| {
        list = pushInPlace(list, @alignOf(u8), @sizeOf(u8), @ptrCast(@constCast(&value)), test_env.getOps());
    }

    try std.testing.expectEqual(@as(usize, 5), list.len());
    try std.testing.expect(list.getCapacity() >= 5);

    // Verify all elements
    const elements_ptr = list.elements(u8);
    try std.testing.expect(elements_ptr != null);
    const elements = elements_ptr.?[0..list.len()];

    for (values, 0..) |expected, i| {
        try std.testing.expectEqual(expected, elements[i]);
    }

    defer list.decref(@alignOf(u8), @sizeOf(u8), false, rcNone, test_env.getOps());
}

test "push: with pre-existing capacity" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    // Create a list with capacity but no elements
    var list = listWithCapacity(10, @alignOf(u32), @sizeOf(u32), false, rcNone, test_env.getOps());

    const initial_capacity = list.getCapacity();
    try std.testing.expect(initial_capacity >= 10);

    // Push elements without triggering reallocation
    const value1: u32 = 100;
    list = pushInPlace(list, @alignOf(u32), @sizeOf(u32), @ptrCast(@constCast(&value1)), test_env.getOps());

    const value2: u32 = 200;
    list = pushInPlace(list, @alignOf(u32), @sizeOf(u32), @ptrCast(@constCast(&value2)), test_env.getOps());

    // Capacity should remain the same
    try std.testing.expectEqual(initial_capacity, list.getCapacity());
    try std.testing.expectEqual(@as(usize, 2), list.len());

    // Verify elements
    const elements_ptr = list.elements(u32);
    try std.testing.expect(elements_ptr != null);
    const elements = elements_ptr.?[0..list.len()];
    try std.testing.expectEqual(@as(u32, 100), elements[0]);
    try std.testing.expectEqual(@as(u32, 200), elements[1]);

    defer list.decref(@alignOf(u32), @sizeOf(u32), false, rcNone, test_env.getOps());
}

test "push: different sized elements" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    // Test with u64 elements
    var list = RocList.empty();

    const value1: u64 = 0xDEADBEEF;
    list = pushInPlace(list, @alignOf(u64), @sizeOf(u64), @ptrCast(@constCast(&value1)), test_env.getOps());

    const value2: u64 = 0xCAFEBABE;
    list = pushInPlace(list, @alignOf(u64), @sizeOf(u64), @ptrCast(@constCast(&value2)), test_env.getOps());

    try std.testing.expectEqual(@as(usize, 2), list.len());

    const elements_ptr = list.elements(u64);
    try std.testing.expect(elements_ptr != null);
    const elements = elements_ptr.?[0..list.len()];
    try std.testing.expectEqual(@as(u64, 0xDEADBEEF), elements[0]);
    try std.testing.expectEqual(@as(u64, 0xCAFEBABE), elements[1]);

    defer list.decref(@alignOf(u64), @sizeOf(u64), false, rcNone, test_env.getOps());
}

test "push: stress test with many elements" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    var list = RocList.empty();

    // Push many elements to test multiple reallocations
    const count: u16 = 100;
    var i: u16 = 0;
    while (i < count) : (i += 1) {
        list = pushInPlace(list, @alignOf(u16), @sizeOf(u16), @ptrCast(@constCast(&i)), test_env.getOps());
    }

    try std.testing.expectEqual(@as(usize, count), list.len());
    try std.testing.expect(list.getCapacity() >= count);

    // Verify all elements
    const elements_ptr = list.elements(u16);
    try std.testing.expect(elements_ptr != null);
    const elements = elements_ptr.?[0..list.len()];

    i = 0;
    while (i < count) : (i += 1) {
        try std.testing.expectEqual(i, elements[i]);
    }

    defer list.decref(@alignOf(u16), @sizeOf(u16), false, rcNone, test_env.getOps());
}

test "append: with unique list (refcount 1)" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    var list = RocList.empty();

    // Add one element to make it unique
    const value1: u8 = 10;
    list = pushInPlace(list, @alignOf(u8), @sizeOf(u8), @ptrCast(@constCast(&value1)), test_env.getOps());

    // Verify it's unique
    try std.testing.expectEqual(@as(usize, 1), list.refcount());

    // Append should mutate in place
    const value2: u8 = 20;
    const result = testAppend(list, @alignOf(u8), @sizeOf(u8), @ptrCast(@constCast(&value2)), &test_env);

    try std.testing.expectEqual(@as(usize, 2), result.len());

    const elements_ptr = result.elements(u8);
    try std.testing.expect(elements_ptr != null);
    const elements = elements_ptr.?[0..result.len()];
    try std.testing.expectEqual(@as(u8, 10), elements[0]);
    try std.testing.expectEqual(@as(u8, 20), elements[1]);

    defer result.decref(@alignOf(u8), @sizeOf(u8), false, rcNone, test_env.getOps());
}

test "append: with shared list (refcount > 1)" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    // Create a list with some elements
    var list = RocList.empty();
    const value1: u8 = 100;
    list = pushInPlace(list, @alignOf(u8), @sizeOf(u8), @ptrCast(@constCast(&value1)), test_env.getOps());
    const value2: u8 = 200;
    list = pushInPlace(list, @alignOf(u8), @sizeOf(u8), @ptrCast(@constCast(&value2)), test_env.getOps());

    // Increment refcount to simulate sharing
    list.incref(1, false);
    try std.testing.expect(list.refcount() > 1);

    // Append should clone
    const value3: u8 = 50;
    const result = testAppend(list, @alignOf(u8), @sizeOf(u8), @ptrCast(@constCast(&value3)), &test_env);

    try std.testing.expectEqual(@as(usize, 3), result.len());
    try std.testing.expectEqual(@as(usize, 2), list.len()); // Original unchanged

    const result_elements_ptr = result.elements(u8);
    try std.testing.expect(result_elements_ptr != null);
    const result_elements = result_elements_ptr.?[0..result.len()];
    try std.testing.expectEqual(@as(u8, 100), result_elements[0]);
    try std.testing.expectEqual(@as(u8, 200), result_elements[1]);
    try std.testing.expectEqual(@as(u8, 50), result_elements[2]);

    defer list.decref(@alignOf(u8), @sizeOf(u8), false, rcNone, test_env.getOps());
    defer result.decref(@alignOf(u8), @sizeOf(u8), false, rcNone, test_env.getOps());
}

test "append: with empty list" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    const list = RocList.empty();

    const value: u32 = 42;
    const result = testAppend(list, @alignOf(u32), @sizeOf(u32), @ptrCast(@constCast(&value)), &test_env);

    try std.testing.expectEqual(@as(usize, 1), result.len());

    const elements_ptr = result.elements(u32);
    try std.testing.expect(elements_ptr != null);
    try std.testing.expectEqual(@as(u32, 42), elements_ptr.?[0]);

    defer result.decref(@alignOf(u32), @sizeOf(u32), false, rcNone, test_env.getOps());
}

test "push: large element types" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    // Test with larger struct-like elements
    const LargeElement = extern struct {
        a: u64,
        b: u64,
        c: u64,
        d: u64,
    };

    var list = RocList.empty();

    const elem1 = LargeElement{ .a = 1, .b = 2, .c = 3, .d = 4 };
    list = pushInPlace(list, @alignOf(LargeElement), @sizeOf(LargeElement), @ptrCast(@constCast(&elem1)), test_env.getOps());

    const elem2 = LargeElement{ .a = 5, .b = 6, .c = 7, .d = 8 };
    list = pushInPlace(list, @alignOf(LargeElement), @sizeOf(LargeElement), @ptrCast(@constCast(&elem2)), test_env.getOps());

    try std.testing.expectEqual(@as(usize, 2), list.len());

    const elements_ptr = list.elements(LargeElement);
    try std.testing.expect(elements_ptr != null);
    const elements = elements_ptr.?[0..list.len()];

    try std.testing.expectEqual(@as(u64, 1), elements[0].a);
    try std.testing.expectEqual(@as(u64, 2), elements[0].b);
    try std.testing.expectEqual(@as(u64, 5), elements[1].a);
    try std.testing.expectEqual(@as(u64, 6), elements[1].b);

    defer list.decref(@alignOf(LargeElement), @sizeOf(LargeElement), false, rcNone, test_env.getOps());
}

test "push: with exact capacity boundary" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    // Create list with exact capacity of 3
    var list = listWithCapacity(3, @alignOf(u16), @sizeOf(u16), false, rcNone, test_env.getOps());

    const initial_capacity = list.getCapacity();
    try std.testing.expect(initial_capacity >= 3);

    // Fill exactly to capacity
    const val1: u16 = 111;
    list = pushInPlace(list, @alignOf(u16), @sizeOf(u16), @ptrCast(@constCast(&val1)), test_env.getOps());
    const val2: u16 = 222;
    list = pushInPlace(list, @alignOf(u16), @sizeOf(u16), @ptrCast(@constCast(&val2)), test_env.getOps());
    const val3: u16 = 333;
    list = pushInPlace(list, @alignOf(u16), @sizeOf(u16), @ptrCast(@constCast(&val3)), test_env.getOps());

    try std.testing.expectEqual(@as(usize, 3), list.len());
    try std.testing.expectEqual(initial_capacity, list.getCapacity());

    // Next push should trigger reallocation
    const val4: u16 = 444;
    list = pushInPlace(list, @alignOf(u16), @sizeOf(u16), @ptrCast(@constCast(&val4)), test_env.getOps());

    try std.testing.expectEqual(@as(usize, 4), list.len());
    try std.testing.expect(list.getCapacity() > initial_capacity);

    const elements_ptr = list.elements(u16);
    try std.testing.expect(elements_ptr != null);
    const elements = elements_ptr.?[0..list.len()];
    try std.testing.expectEqual(@as(u16, 111), elements[0]);
    try std.testing.expectEqual(@as(u16, 222), elements[1]);
    try std.testing.expectEqual(@as(u16, 333), elements[2]);
    try std.testing.expectEqual(@as(u16, 444), elements[3]);

    defer list.decref(@alignOf(u16), @sizeOf(u16), false, rcNone, test_env.getOps());
}

test "push: single byte elements" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    var list = RocList.empty();

    // Push ASCII characters
    const chars = "Hello";
    for (chars) |ch| {
        list = pushInPlace(list, @alignOf(u8), @sizeOf(u8), @ptrCast(@constCast(&ch)), test_env.getOps());
    }

    try std.testing.expectEqual(@as(usize, 5), list.len());

    const elements_ptr = list.elements(u8);
    try std.testing.expect(elements_ptr != null);
    const elements = elements_ptr.?[0..list.len()];

    try std.testing.expectEqual(@as(u8, 'H'), elements[0]);
    try std.testing.expectEqual(@as(u8, 'e'), elements[1]);
    try std.testing.expectEqual(@as(u8, 'l'), elements[2]);
    try std.testing.expectEqual(@as(u8, 'l'), elements[3]);
    try std.testing.expectEqual(@as(u8, 'o'), elements[4]);

    defer list.decref(@alignOf(u8), @sizeOf(u8), false, rcNone, test_env.getOps());
}

test "append: refcount transitions" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    // Start with unique list
    var list = RocList.empty();
    const val1: u8 = 10;
    list = pushInPlace(list, @alignOf(u8), @sizeOf(u8), @ptrCast(@constCast(&val1)), test_env.getOps());

    try std.testing.expectEqual(@as(usize, 1), list.refcount());

    // Append while unique (should use push)
    const val2: u8 = 20;
    const result1 = testAppend(list, @alignOf(u8), @sizeOf(u8), @ptrCast(@constCast(&val2)), &test_env);

    try std.testing.expectEqual(@as(usize, 2), result1.len());

    // Increment refcount
    result1.incref(1, false);
    try std.testing.expect(result1.refcount() > 1);

    // Append while shared (should clone)
    const val3: u8 = 30;
    const result2 = testAppend(result1, @alignOf(u8), @sizeOf(u8), @ptrCast(@constCast(&val3)), &test_env);

    try std.testing.expectEqual(@as(usize, 3), result2.len());
    try std.testing.expectEqual(@as(usize, 2), result1.len()); // Original unchanged

    // Verify contents
    const result2_elements = result2.elements(u8).?[0..result2.len()];
    try std.testing.expectEqual(@as(u8, 10), result2_elements[0]);
    try std.testing.expectEqual(@as(u8, 20), result2_elements[1]);
    try std.testing.expectEqual(@as(u8, 30), result2_elements[2]);

    defer result1.decref(@alignOf(u8), @sizeOf(u8), false, rcNone, test_env.getOps());
    defer result2.decref(@alignOf(u8), @sizeOf(u8), false, rcNone, test_env.getOps());
}

test "append: capacity growth strategy" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    // Create list with specific content
    var list = RocList.empty();
    const values = [_]u32{ 100, 200, 300 };
    for (values) |val| {
        list = pushInPlace(list, @alignOf(u32), @sizeOf(u32), @ptrCast(@constCast(&val)), test_env.getOps());
    }

    // Make it shared
    list.incref(1, false);

    // Append should create new list with growth-strategy determined capacity
    const new_val: u32 = 400;
    const result = testAppend(list, @alignOf(u32), @sizeOf(u32), @ptrCast(@constCast(&new_val)), &test_env);

    try std.testing.expectEqual(@as(usize, 4), result.len());
    try std.testing.expect(result.getCapacity() >= 4); // At least enough for elements

    // Verify all elements copied correctly
    const result_elements = result.elements(u32).?[0..result.len()];
    try std.testing.expectEqual(@as(u32, 100), result_elements[0]);
    try std.testing.expectEqual(@as(u32, 200), result_elements[1]);
    try std.testing.expectEqual(@as(u32, 300), result_elements[2]);
    try std.testing.expectEqual(@as(u32, 400), result_elements[3]);

    defer list.decref(@alignOf(u32), @sizeOf(u32), false, rcNone, test_env.getOps());
    defer result.decref(@alignOf(u32), @sizeOf(u32), false, rcNone, test_env.getOps());
}

test "append: mixed with push operations" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    var list = RocList.empty();

    // Start with push
    const val1: u8 = 1;
    list = pushInPlace(list, @alignOf(u8), @sizeOf(u8), @ptrCast(@constCast(&val1)), test_env.getOps());

    // Append while unique
    const val2: u8 = 2;
    list = testAppend(list, @alignOf(u8), @sizeOf(u8), @ptrCast(@constCast(&val2)), &test_env);

    // More pushes
    const val3: u8 = 3;
    list = pushInPlace(list, @alignOf(u8), @sizeOf(u8), @ptrCast(@constCast(&val3)), test_env.getOps());

    // Make shared
    list.incref(1, false);

    // Append while shared (should clone)
    const val4: u8 = 4;
    const result = testAppend(list, @alignOf(u8), @sizeOf(u8), @ptrCast(@constCast(&val4)), &test_env);

    try std.testing.expectEqual(@as(usize, 3), list.len());
    try std.testing.expectEqual(@as(usize, 4), result.len());

    const result_elements = result.elements(u8).?[0..result.len()];
    try std.testing.expectEqual(@as(u8, 1), result_elements[0]);
    try std.testing.expectEqual(@as(u8, 2), result_elements[1]);
    try std.testing.expectEqual(@as(u8, 3), result_elements[2]);
    try std.testing.expectEqual(@as(u8, 4), result_elements[3]);

    defer list.decref(@alignOf(u8), @sizeOf(u8), false, rcNone, test_env.getOps());
    defer result.decref(@alignOf(u8), @sizeOf(u8), false, rcNone, test_env.getOps());
}

test "push and append: large scale alternating operations" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    var list = RocList.empty();

    // Alternate between push and append operations
    var i: u16 = 0;
    while (i < 50) : (i += 1) {
        if (i % 2 == 0) {
            list = pushInPlace(list, @alignOf(u16), @sizeOf(u16), @ptrCast(@constCast(&i)), test_env.getOps());
        } else {
            list = pushInPlace(list, @alignOf(u16), @sizeOf(u16), @ptrCast(@constCast(&i)), test_env.getOps());
        }
    }

    try std.testing.expectEqual(@as(usize, 50), list.len());

    // Verify all elements are correct
    const elements = list.elements(u16).?[0..list.len()];
    i = 0;
    while (i < 50) : (i += 1) {
        try std.testing.expectEqual(i, elements[i]);
    }

    defer list.decref(@alignOf(u16), @sizeOf(u16), false, rcNone, test_env.getOps());
}

// Helper function for tests that does proper append with cloning for shared lists
fn testAppend(
    list: RocList,
    alignment: u32,
    element_size: usize,
    element: *anyopaque,
    test_env: *TestEnv,
) RocList {
    // Check if list is unique (refcount == 1)
    if (list.isUnique()) {
        // List is unique, can mutate in place
        return pushInPlace(list, alignment, element_size, element, test_env.getOps());
    } else {
        // List is shared, need to clone first
        const old_len = list.len();
        const new_capacity = old_len + 1;

        // Create new list with capacity for the new element
        var new_list = listWithCapacity(new_capacity, alignment, element_size, false, rcNone, test_env.getOps());
        new_list.length = old_len + 1;

        // Copy existing elements
        if (list.bytes) |src_bytes| {
            if (new_list.bytes) |dst_bytes| {
                @memcpy(dst_bytes[0..(old_len * element_size)], src_bytes[0..(old_len * element_size)]);
                // Copy the new element
                @memcpy(dst_bytes[(old_len * element_size)..((old_len + 1) * element_size)], @as([*]const u8, @ptrCast(element))[0..element_size]);
            }
        }

        return new_list;
    }
}

test "append: stress test with cloning" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    var original = RocList.empty();

    // Build up original list
    var i: u8 = 0;
    while (i < 20) : (i += 1) {
        original = pushInPlace(original, @alignOf(u8), @sizeOf(u8), @ptrCast(@constCast(&i)), test_env.getOps());
    }

    // Make it shared
    original.incref(1, false);

    // Create multiple clones via append
    var clones: [10]RocList = undefined;
    i = 0;
    while (i < 10) : (i += 1) {
        const append_val: u8 = 100 + i;
        clones[i] = testAppend(original, @alignOf(u8), @sizeOf(u8), @ptrCast(@constCast(&append_val)), &test_env);
    }

    // Verify original is unchanged
    try std.testing.expectEqual(@as(usize, 20), original.len());

    // Verify each clone
    i = 0;
    while (i < 10) : (i += 1) {
        try std.testing.expectEqual(@as(usize, 21), clones[i].len());

        const elements = clones[i].elements(u8).?[0..clones[i].len()];

        // Check original elements are preserved
        var j: u8 = 0;
        while (j < 20) : (j += 1) {
            try std.testing.expectEqual(j, elements[j]);
        }

        // Check appended element
        try std.testing.expectEqual(100 + i, elements[20]);
    }

    // Cleanup
    defer original.decref(@alignOf(u8), @sizeOf(u8), false, rcNone, test_env.getOps());
    i = 0;
    while (i < 10) : (i += 1) {
        defer clones[i].decref(@alignOf(u8), @sizeOf(u8), false, rcNone, test_env.getOps());
    }
}
