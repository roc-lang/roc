const std = @import("std");
const utils = @import("utils.zig");
const UpdateMode = utils.UpdateMode;
const mem = std.mem;
const math = std.math;

const expect = std.testing.expect;

const EqFn = *const fn (?[*]u8, ?[*]u8) callconv(.C) bool;
const CompareFn = *const fn (?[*]u8, ?[*]u8, ?[*]u8) callconv(.C) u8;
const Opaque = ?[*]u8;

const Inc = *const fn (?[*]u8) callconv(.C) void;
const IncN = *const fn (?[*]u8, usize) callconv(.C) void;
const Dec = *const fn (?[*]u8) callconv(.C) void;
const HasTagId = *const fn (u16, ?[*]u8) callconv(.C) extern struct { matched: bool, data: ?[*]u8 };

const SEAMLESS_SLICE_BIT: usize =
    @as(usize, @bitCast(@as(isize, std.math.minInt(isize))));

pub const RocList = extern struct {
    bytes: ?[*]u8,
    length: usize,
    // This technically points to directly after the refcount.
    // This is an optimization that enables use one code path for regular lists and slices for geting the refcount ptr.
    capacity_or_ref_ptr: usize,

    pub inline fn len(self: RocList) usize {
        return self.length;
    }

    pub fn getCapacity(self: RocList) usize {
        const list_capacity = self.capacity_or_ref_ptr;
        const slice_capacity = self.length;
        const slice_mask = self.seamlessSliceMask();
        const capacity = (list_capacity & ~slice_mask) | (slice_capacity & slice_mask);
        return capacity;
    }

    pub fn isSeamlessSlice(self: RocList) bool {
        return @as(isize, @bitCast(self.capacity_or_ref_ptr)) < 0;
    }

    // This returns all ones if the list is a seamless slice.
    // Otherwise, it returns all zeros.
    // This is done without branching for optimization purposes.
    pub fn seamlessSliceMask(self: RocList) usize {
        return @as(usize, @bitCast(@as(isize, @bitCast(self.capacity_or_ref_ptr)) >> (@bitSizeOf(isize) - 1)));
    }

    pub fn isEmpty(self: RocList) bool {
        return self.len() == 0;
    }

    pub fn empty() RocList {
        return RocList{ .bytes = null, .length = 0, .capacity_or_ref_ptr = 0 };
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

    pub fn fromSlice(comptime T: type, slice: []const T) RocList {
        if (slice.len == 0) {
            return RocList.empty();
        }

        var list = allocate(@alignOf(T), slice.len, @sizeOf(T));

        if (slice.len > 0) {
            const dest = list.bytes orelse unreachable;
            const src = @as([*]const u8, @ptrCast(slice.ptr));
            const num_bytes = slice.len * @sizeOf(T);

            @memcpy(dest[0..num_bytes], src[0..num_bytes]);
        }

        return list;
    }

    // returns a pointer to just after the refcount.
    // It is just after the refcount as an optimization for other shared code paths.
    // For regular list, it just returns their bytes pointer.
    // For seamless slices, it returns the pointer stored in capacity_or_ref_ptr.
    pub fn getRefcountPtr(self: RocList) ?[*]u8 {
        const list_ref_ptr = @intFromPtr(self.bytes);
        const slice_ref_ptr = self.capacity_or_ref_ptr << 1;
        const slice_mask = self.seamlessSliceMask();
        const ref_ptr = (list_ref_ptr & ~slice_mask) | (slice_ref_ptr & slice_mask);
        return @as(?[*]u8, @ptrFromInt(ref_ptr));
    }

    pub fn decref(self: RocList, alignment: u32) void {
        // We use the raw capacity to ensure we always decrement the refcount of seamless slices.
        utils.decref(self.getRefcountPtr(), self.capacity_or_ref_ptr, alignment);
    }

    pub fn elements(self: RocList, comptime T: type) ?[*]T {
        return @as(?[*]T, @ptrCast(@alignCast(self.bytes)));
    }

    pub fn isUnique(self: RocList) bool {
        return self.refcountMachine() == utils.REFCOUNT_ONE;
    }

    fn refcountMachine(self: RocList) usize {
        if (self.getCapacity() == 0 and !self.isSeamlessSlice()) {
            // the zero-capacity is Clone, copying it will not leak memory
            return utils.REFCOUNT_ONE;
        }

        const ptr: [*]usize = @as([*]usize, @ptrCast(@alignCast(self.bytes)));
        return (ptr - 1)[0];
    }

    fn refcountHuman(self: RocList) usize {
        return self.refcountMachine() - utils.REFCOUNT_ONE + 1;
    }

    pub fn makeUniqueExtra(self: RocList, alignment: u32, element_width: usize, update_mode: UpdateMode) RocList {
        if (update_mode == .InPlace) {
            return self;
        } else {
            return self.makeUnique(alignment, element_width);
        }
    }

    pub fn makeUnique(self: RocList, alignment: u32, element_width: usize) RocList {
        if (self.isUnique()) {
            return self;
        }

        if (self.isEmpty()) {
            // Empty is not necessarily unique on it's own.
            // The list could have capacity and be shared.
            self.decref(alignment);
            return RocList.empty();
        }

        // unfortunately, we have to clone
        var new_list = RocList.allocate(alignment, self.length, element_width);

        var old_bytes: [*]u8 = @as([*]u8, @ptrCast(self.bytes));
        var new_bytes: [*]u8 = @as([*]u8, @ptrCast(new_list.bytes));

        const number_of_bytes = self.len() * element_width;
        @memcpy(new_bytes[0..number_of_bytes], old_bytes[0..number_of_bytes]);

        // NOTE we fuse an increment of all keys/values with a decrement of the input list.
        self.decref(alignment);

        return new_list;
    }

    pub fn allocate(
        alignment: u32,
        length: usize,
        element_width: usize,
    ) RocList {
        if (length == 0) {
            return empty();
        }

        const capacity = utils.calculateCapacity(0, length, element_width);
        const data_bytes = capacity * element_width;
        return RocList{
            .bytes = utils.allocateWithRefcount(data_bytes, alignment),
            .length = length,
            .capacity_or_ref_ptr = capacity,
        };
    }

    pub fn allocateExact(
        alignment: u32,
        length: usize,
        element_width: usize,
    ) RocList {
        if (length == 0) {
            return empty();
        }

        const data_bytes = length * element_width;
        return RocList{
            .bytes = utils.allocateWithRefcount(data_bytes, alignment),
            .length = length,
            .capacity_or_ref_ptr = length,
        };
    }

    pub fn reallocate(
        self: RocList,
        alignment: u32,
        new_length: usize,
        element_width: usize,
    ) RocList {
        if (self.bytes) |source_ptr| {
            if (self.isUnique() and !self.isSeamlessSlice()) {
                const capacity = self.capacity_or_ref_ptr;
                if (capacity >= new_length) {
                    return RocList{ .bytes = self.bytes, .length = new_length, .capacity_or_ref_ptr = capacity };
                } else {
                    const new_capacity = utils.calculateCapacity(capacity, new_length, element_width);
                    const new_source = utils.unsafeReallocate(source_ptr, alignment, capacity, new_capacity, element_width);
                    return RocList{ .bytes = new_source, .length = new_length, .capacity_or_ref_ptr = new_capacity };
                }
            }
            return self.reallocateFresh(alignment, new_length, element_width);
        }
        return RocList.allocate(alignment, new_length, element_width);
    }

    /// reallocate by explicitly making a new allocation and copying elements over
    fn reallocateFresh(
        self: RocList,
        alignment: u32,
        new_length: usize,
        element_width: usize,
    ) RocList {
        const old_length = self.length;

        const result = RocList.allocate(alignment, new_length, element_width);

        // transfer the memory
        if (self.bytes) |source_ptr| {
            const dest_ptr = result.bytes orelse unreachable;

            @memcpy(dest_ptr[0..(old_length * element_width)], source_ptr[0..(old_length * element_width)]);
            @memset(dest_ptr[(old_length * element_width)..(new_length * element_width)], 0);
        }

        self.decref(alignment);

        return result;
    }
};

const Caller0 = *const fn (?[*]u8, ?[*]u8) callconv(.C) void;
const Caller1 = *const fn (?[*]u8, ?[*]u8, ?[*]u8) callconv(.C) void;
const Caller2 = *const fn (?[*]u8, ?[*]u8, ?[*]u8, ?[*]u8) callconv(.C) void;
const Caller3 = *const fn (?[*]u8, ?[*]u8, ?[*]u8, ?[*]u8, ?[*]u8) callconv(.C) void;
const Caller4 = *const fn (?[*]u8, ?[*]u8, ?[*]u8, ?[*]u8, ?[*]u8, ?[*]u8) callconv(.C) void;

pub fn listMap(
    list: RocList,
    caller: Caller1,
    data: Opaque,
    inc_n_data: IncN,
    data_is_owned: bool,
    alignment: u32,
    old_element_width: usize,
    new_element_width: usize,
) callconv(.C) RocList {
    if (list.bytes) |source_ptr| {
        const size = list.len();
        var i: usize = 0;
        const output = RocList.allocate(alignment, size, new_element_width);
        const target_ptr = output.bytes orelse unreachable;

        if (data_is_owned) {
            inc_n_data(data, size);
        }

        while (i < size) : (i += 1) {
            caller(data, source_ptr + (i * old_element_width), target_ptr + (i * new_element_width));
        }

        return output;
    } else {
        return RocList.empty();
    }
}

fn decrementTail(list: RocList, start_index: usize, element_width: usize, dec: Dec) void {
    if (list.bytes) |source| {
        var i = start_index;
        while (i < list.len()) : (i += 1) {
            const element = source + i * element_width;
            dec(element);
        }
    }
}

pub fn listMap2(
    list1: RocList,
    list2: RocList,
    caller: Caller2,
    data: Opaque,
    inc_n_data: IncN,
    data_is_owned: bool,
    alignment: u32,
    a_width: usize,
    b_width: usize,
    c_width: usize,
    dec_a: Dec,
    dec_b: Dec,
) callconv(.C) RocList {
    const output_length = @min(list1.len(), list2.len());

    // if the lists don't have equal length, we must consume the remaining elements
    // In this case we consume by (recursively) decrementing the elements
    decrementTail(list1, output_length, a_width, dec_a);
    decrementTail(list2, output_length, b_width, dec_b);

    if (data_is_owned) {
        inc_n_data(data, output_length);
    }

    if (list1.bytes) |source_a| {
        if (list2.bytes) |source_b| {
            const output = RocList.allocate(alignment, output_length, c_width);
            const target_ptr = output.bytes orelse unreachable;

            var i: usize = 0;
            while (i < output_length) : (i += 1) {
                const element_a = source_a + i * a_width;
                const element_b = source_b + i * b_width;
                const target = target_ptr + i * c_width;
                caller(data, element_a, element_b, target);
            }

            return output;
        } else {
            return RocList.empty();
        }
    } else {
        return RocList.empty();
    }
}

pub fn listMap3(
    list1: RocList,
    list2: RocList,
    list3: RocList,
    caller: Caller3,
    data: Opaque,
    inc_n_data: IncN,
    data_is_owned: bool,
    alignment: u32,
    a_width: usize,
    b_width: usize,
    c_width: usize,
    d_width: usize,
    dec_a: Dec,
    dec_b: Dec,
    dec_c: Dec,
) callconv(.C) RocList {
    const smaller_length = @min(list1.len(), list2.len());
    const output_length = @min(smaller_length, list3.len());

    decrementTail(list1, output_length, a_width, dec_a);
    decrementTail(list2, output_length, b_width, dec_b);
    decrementTail(list3, output_length, c_width, dec_c);

    if (data_is_owned) {
        inc_n_data(data, output_length);
    }

    if (list1.bytes) |source_a| {
        if (list2.bytes) |source_b| {
            if (list3.bytes) |source_c| {
                const output = RocList.allocate(alignment, output_length, d_width);
                const target_ptr = output.bytes orelse unreachable;

                var i: usize = 0;
                while (i < output_length) : (i += 1) {
                    const element_a = source_a + i * a_width;
                    const element_b = source_b + i * b_width;
                    const element_c = source_c + i * c_width;
                    const target = target_ptr + i * d_width;

                    caller(data, element_a, element_b, element_c, target);
                }

                return output;
            } else {
                return RocList.empty();
            }
        } else {
            return RocList.empty();
        }
    } else {
        return RocList.empty();
    }
}

pub fn listMap4(
    list1: RocList,
    list2: RocList,
    list3: RocList,
    list4: RocList,
    caller: Caller4,
    data: Opaque,
    inc_n_data: IncN,
    data_is_owned: bool,
    alignment: u32,
    a_width: usize,
    b_width: usize,
    c_width: usize,
    d_width: usize,
    e_width: usize,
    dec_a: Dec,
    dec_b: Dec,
    dec_c: Dec,
    dec_d: Dec,
) callconv(.C) RocList {
    const output_length = @min(@min(list1.len(), list2.len()), @min(list3.len(), list4.len()));

    decrementTail(list1, output_length, a_width, dec_a);
    decrementTail(list2, output_length, b_width, dec_b);
    decrementTail(list3, output_length, c_width, dec_c);
    decrementTail(list4, output_length, d_width, dec_d);

    if (data_is_owned) {
        inc_n_data(data, output_length);
    }

    if (list1.bytes) |source_a| {
        if (list2.bytes) |source_b| {
            if (list3.bytes) |source_c| {
                if (list4.bytes) |source_d| {
                    const output = RocList.allocate(alignment, output_length, e_width);
                    const target_ptr = output.bytes orelse unreachable;

                    var i: usize = 0;
                    while (i < output_length) : (i += 1) {
                        const element_a = source_a + i * a_width;
                        const element_b = source_b + i * b_width;
                        const element_c = source_c + i * c_width;
                        const element_d = source_d + i * d_width;
                        const target = target_ptr + i * e_width;

                        caller(data, element_a, element_b, element_c, element_d, target);
                    }

                    return output;
                } else {
                    return RocList.empty();
                }
            } else {
                return RocList.empty();
            }
        } else {
            return RocList.empty();
        }
    } else {
        return RocList.empty();
    }
}

pub fn listWithCapacity(
    capacity: usize,
    alignment: u32,
    element_width: usize,
) callconv(.C) RocList {
    return listReserve(RocList.empty(), alignment, capacity, element_width, .InPlace);
}

pub fn listReserve(
    list: RocList,
    alignment: u32,
    spare: usize,
    element_width: usize,
    update_mode: UpdateMode,
) callconv(.C) RocList {
    const old_length = list.len();
    if ((update_mode == .InPlace or list.isUnique()) and list.getCapacity() >= list.len() + spare) {
        return list;
    } else {
        var output = list.reallocate(alignment, old_length + spare, element_width);
        output.length = old_length;
        return output;
    }
}

pub fn listReleaseExcessCapacity(
    list: RocList,
    alignment: u32,
    element_width: usize,
    update_mode: UpdateMode,
) callconv(.C) RocList {
    const old_length = list.len();
    // We use the direct list.capacity_or_ref_ptr to make sure both that there is no extra capacity and that it isn't a seamless slice.
    if ((update_mode == .InPlace or list.isUnique()) and list.capacity_or_ref_ptr == old_length) {
        return list;
    } else if (old_length == 0) {
        list.decref(alignment);
        return RocList.empty();
    } else {
        var output = RocList.allocateExact(alignment, old_length, element_width);
        if (list.bytes) |source_ptr| {
            const dest_ptr = output.bytes orelse unreachable;

            @memcpy(dest_ptr[0..(old_length * element_width)], source_ptr[0..(old_length * element_width)]);
        }
        list.decref(alignment);
        return output;
    }
}

pub fn listAppendUnsafe(
    list: RocList,
    element: Opaque,
    element_width: usize,
) callconv(.C) RocList {
    const old_length = list.len();
    var output = list;
    output.length += 1;

    if (output.bytes) |bytes| {
        if (element) |source| {
            const target = bytes + old_length * element_width;
            @memcpy(target[0..element_width], source[0..element_width]);
        }
    }

    return output;
}

fn listAppend(list: RocList, alignment: u32, element: Opaque, element_width: usize, update_mode: UpdateMode) callconv(.C) RocList {
    const with_capacity = listReserve(list, alignment, 1, element_width, update_mode);
    return listAppendUnsafe(with_capacity, element, element_width);
}

pub fn listPrepend(list: RocList, alignment: u32, element: Opaque, element_width: usize) callconv(.C) RocList {
    const old_length = list.len();
    // TODO: properly wire in update mode.
    var with_capacity = listReserve(list, alignment, 1, element_width, .Immutable);
    with_capacity.length += 1;

    // can't use one memcpy here because source and target overlap
    if (with_capacity.bytes) |target| {
        var i: usize = old_length;

        while (i > 0) {
            i -= 1;

            // move the ith element to the (i + 1)th position
            const to = target + (i + 1) * element_width;
            const from = target + i * element_width;
            @memcpy(to[0..element_width], from[0..element_width]);
        }

        // finally copy in the new first element
        if (element) |source| {
            @memcpy(target[0..element_width], source[0..element_width]);
        }
    }

    return with_capacity;
}

pub fn listSwap(
    list: RocList,
    alignment: u32,
    element_width: usize,
    index_1: usize,
    index_2: usize,
    update_mode: UpdateMode,
) callconv(.C) RocList {
    const size = list.len();
    if (index_1 == index_2 or index_1 >= size or index_2 >= size) {
        // Either index out of bounds so we just return
        return list;
    }

    const newList = blk: {
        if (update_mode == .InPlace) {
            break :blk list;
        } else {
            break :blk list.makeUnique(alignment, element_width);
        }
    };

    const source_ptr = @as([*]u8, @ptrCast(newList.bytes));
    swapElements(source_ptr, element_width, index_1, index_2);

    return newList;
}

pub fn listSublist(
    list: RocList,
    alignment: u32,
    element_width: usize,
    start: usize,
    len: usize,
    dec: Dec,
) callconv(.C) RocList {
    const size = list.len();
    if (len == 0 or start >= size) {
        // Decrement the reference counts of all elements.
        if (list.bytes) |source_ptr| {
            var i: usize = 0;
            while (i < size) : (i += 1) {
                const element = source_ptr + i * element_width;
                dec(element);
            }
        }
        if (list.isUnique()) {
            var output = list;
            output.length = 0;
            return output;
        }
        list.decref(alignment);
        return RocList.empty();
    }

    if (list.bytes) |source_ptr| {
        const keep_len = @min(len, size - start);
        const drop_start_len = start;
        const drop_end_len = size - (start + keep_len);

        // Decrement the reference counts of elements before `start`.
        var i: usize = 0;
        while (i < drop_start_len) : (i += 1) {
            const element = source_ptr + i * element_width;
            dec(element);
        }

        // Decrement the reference counts of elements after `start + keep_len`.
        i = 0;
        while (i < drop_end_len) : (i += 1) {
            const element = source_ptr + (start + keep_len + i) * element_width;
            dec(element);
        }

        if (start == 0 and list.isUnique()) {
            var output = list;
            output.length = keep_len;
            return output;
        } else {
            const list_ref_ptr = (@intFromPtr(source_ptr) >> 1) | SEAMLESS_SLICE_BIT;
            const slice_ref_ptr = list.capacity_or_ref_ptr;
            const slice_mask = list.seamlessSliceMask();
            const ref_ptr = (list_ref_ptr & ~slice_mask) | (slice_ref_ptr & slice_mask);
            return RocList{
                .bytes = source_ptr + start * element_width,
                .length = keep_len,
                .capacity_or_ref_ptr = ref_ptr,
            };
        }
    }

    return RocList.empty();
}

pub fn listDropAt(
    list: RocList,
    alignment: u32,
    element_width: usize,
    drop_index: usize,
    dec: Dec,
) callconv(.C) RocList {
    const size = list.len();
    // If droping the first or last element, return a seamless slice.
    // For simplicity, do this by calling listSublist.
    // In the future, we can test if it is faster to manually inline the important parts here.
    if (drop_index == 0) {
        return listSublist(list, alignment, element_width, 1, size -| 1, dec);
    } else if (drop_index == size -| 1) {
        return listSublist(list, alignment, element_width, 0, size -| 1, dec);
    }

    if (list.bytes) |source_ptr| {
        if (drop_index >= size) {
            return list;
        }

        if (drop_index < size) {
            const element = source_ptr + drop_index * element_width;
            dec(element);
        }

        // NOTE
        // we need to return an empty list explicitly,
        // because we rely on the pointer field being null if the list is empty
        // which also requires duplicating the utils.decref call to spend the RC token
        if (size < 2) {
            list.decref(alignment);
            return RocList.empty();
        }

        if (list.isUnique()) {
            var i = drop_index;
            while (i < size - 1) : (i += 1) {
                const copy_target = source_ptr + i * element_width;
                const copy_source = copy_target + element_width;

                @memcpy(copy_target[0..element_width], copy_source[0..element_width]);
            }

            var new_list = list;

            new_list.length -= 1;
            return new_list;
        }

        const output = RocList.allocate(alignment, size - 1, element_width);
        const target_ptr = output.bytes orelse unreachable;

        const head_size = drop_index * element_width;
        @memcpy(target_ptr[0..head_size], source_ptr[0..head_size]);

        const tail_target = target_ptr + drop_index * element_width;
        const tail_source = source_ptr + (drop_index + 1) * element_width;
        const tail_size = (size - drop_index - 1) * element_width;
        @memcpy(tail_target[0..tail_size], tail_source[0..tail_size]);

        list.decref(alignment);

        return output;
    } else {
        return RocList.empty();
    }
}

fn partition(source_ptr: [*]u8, transform: Opaque, wrapper: CompareFn, element_width: usize, low: isize, high: isize) isize {
    const pivot = source_ptr + (@as(usize, @intCast(high)) * element_width);
    var i = (low - 1); // Index of smaller element and indicates the right position of pivot found so far
    var j = low;

    while (j <= high - 1) : (j += 1) {
        const current_elem = source_ptr + (@as(usize, @intCast(j)) * element_width);

        const ordering = wrapper(transform, current_elem, pivot);
        const order = @as(utils.Ordering, @enumFromInt(ordering));

        switch (order) {
            utils.Ordering.LT => {
                // the current element is smaller than the pivot; swap it
                i += 1;
                swapElements(source_ptr, element_width, @as(usize, @intCast(i)), @as(usize, @intCast(j)));
            },
            utils.Ordering.EQ, utils.Ordering.GT => {},
        }
    }
    swapElements(source_ptr, element_width, @as(usize, @intCast(i + 1)), @as(usize, @intCast(high)));
    return (i + 1);
}

fn quicksort(source_ptr: [*]u8, transform: Opaque, wrapper: CompareFn, element_width: usize, low: isize, high: isize) void {
    if (low < high) {
        // partition index
        const pi = partition(source_ptr, transform, wrapper, element_width, low, high);

        _ = quicksort(source_ptr, transform, wrapper, element_width, low, pi - 1); // before pi
        _ = quicksort(source_ptr, transform, wrapper, element_width, pi + 1, high); // after pi
    }
}

pub fn listSortWith(
    input: RocList,
    caller: CompareFn,
    data: Opaque,
    inc_n_data: IncN,
    data_is_owned: bool,
    alignment: u32,
    element_width: usize,
) callconv(.C) RocList {
    var list = input.makeUnique(alignment, element_width);

    if (data_is_owned) {
        inc_n_data(data, list.len());
    }

    if (list.bytes) |source_ptr| {
        const low = 0;
        const high: isize = @as(isize, @intCast(list.len())) - 1;
        quicksort(source_ptr, data, caller, element_width, low, high);
    }

    return list;
}

// SWAP ELEMENTS

inline fn swapHelp(width: usize, temporary: [*]u8, ptr1: [*]u8, ptr2: [*]u8) void {
    @memcpy(temporary[0..width], ptr1[0..width]);
    @memcpy(ptr1[0..width], ptr2[0..width]);
    @memcpy(ptr2[0..width], temporary[0..width]);
}

fn swap(width_initial: usize, p1: [*]u8, p2: [*]u8) void {
    const threshold: usize = 64;

    var width = width_initial;

    var ptr1 = p1;
    var ptr2 = p2;

    var buffer_actual: [threshold]u8 = undefined;
    var buffer: [*]u8 = buffer_actual[0..];

    while (true) {
        if (width < threshold) {
            swapHelp(width, buffer, ptr1, ptr2);
            return;
        } else {
            swapHelp(threshold, buffer, ptr1, ptr2);

            ptr1 += threshold;
            ptr2 += threshold;

            width -= threshold;
        }
    }
}

fn swapElements(source_ptr: [*]u8, element_width: usize, index_1: usize, index_2: usize) void {
    var element_at_i = source_ptr + (index_1 * element_width);
    var element_at_j = source_ptr + (index_2 * element_width);

    return swap(element_width, element_at_i, element_at_j);
}

pub fn listConcat(list_a: RocList, list_b: RocList, alignment: u32, element_width: usize) callconv(.C) RocList {
    // NOTE we always use list_a! because it is owned, we must consume it, and it may have unused capacity
    if (list_b.isEmpty()) {
        if (list_a.getCapacity() == 0) {
            // a could be a seamless slice, so we still need to decref.
            list_a.decref(alignment);
            return list_b;
        } else {
            // we must consume this list. Even though it has no elements, it could still have capacity
            list_b.decref(alignment);

            return list_a;
        }
    } else if (list_a.isUnique()) {
        const total_length: usize = list_a.len() + list_b.len();

        const resized_list_a = list_a.reallocate(alignment, total_length, element_width);

        // These must exist, otherwise, the lists would have been empty.
        const source_a = resized_list_a.bytes orelse unreachable;
        const source_b = list_b.bytes orelse unreachable;
        @memcpy(source_a[(list_a.len() * element_width)..(total_length * element_width)], source_b[0..(list_b.len() * element_width)]);

        // decrement list b.
        list_b.decref(alignment);

        return resized_list_a;
    } else if (list_b.isUnique()) {
        const total_length: usize = list_a.len() + list_b.len();

        const resized_list_b = list_b.reallocate(alignment, total_length, element_width);

        // These must exist, otherwise, the lists would have been empty.
        const source_a = list_a.bytes orelse unreachable;
        const source_b = resized_list_b.bytes orelse unreachable;

        // This is a bit special, we need to first copy the elements of list_b to the end,
        // then copy the elements of list_a to the beginning.
        // This first call must use mem.copy because the slices might overlap.
        const byte_count_a = list_a.len() * element_width;
        const byte_count_b = list_b.len() * element_width;
        mem.copyBackwards(u8, source_b[byte_count_a .. byte_count_a + byte_count_b], source_b[0..byte_count_b]);
        @memcpy(source_b[0..byte_count_a], source_a[0..byte_count_a]);

        // decrement list a.
        list_a.decref(alignment);

        return resized_list_b;
    }
    const total_length: usize = list_a.len() + list_b.len();

    const output = RocList.allocate(alignment, total_length, element_width);

    // These must exist, otherwise, the lists would have been empty.
    const target = output.bytes orelse unreachable;
    const source_a = list_a.bytes orelse unreachable;
    const source_b = list_b.bytes orelse unreachable;

    @memcpy(target[0..(list_a.len() * element_width)], source_a[0..(list_a.len() * element_width)]);
    @memcpy(target[(list_a.len() * element_width)..(total_length * element_width)], source_b[0..(list_b.len() * element_width)]);

    // decrement list a and b.
    list_a.decref(alignment);
    list_b.decref(alignment);

    return output;
}

pub fn listReplaceInPlace(
    list: RocList,
    index: usize,
    element: Opaque,
    element_width: usize,
    out_element: ?[*]u8,
) callconv(.C) RocList {
    // INVARIANT: bounds checking happens on the roc side
    //
    // at the time of writing, the function is implemented roughly as
    // `if inBounds then LowLevelListReplace input index item else input`
    // so we don't do a bounds check here. Hence, the list is also non-empty,
    // because inserting into an empty list is always out of bounds
    return listReplaceInPlaceHelp(list, index, element, element_width, out_element);
}

pub fn listReplace(
    list: RocList,
    alignment: u32,
    index: usize,
    element: Opaque,
    element_width: usize,
    out_element: ?[*]u8,
) callconv(.C) RocList {
    // INVARIANT: bounds checking happens on the roc side
    //
    // at the time of writing, the function is implemented roughly as
    // `if inBounds then LowLevelListReplace input index item else input`
    // so we don't do a bounds check here. Hence, the list is also non-empty,
    // because inserting into an empty list is always out of bounds
    return listReplaceInPlaceHelp(list.makeUnique(alignment, element_width), index, element, element_width, out_element);
}

inline fn listReplaceInPlaceHelp(
    list: RocList,
    index: usize,
    element: Opaque,
    element_width: usize,
    out_element: ?[*]u8,
) RocList {
    // the element we will replace
    var element_at_index = (list.bytes orelse unreachable) + (index * element_width);

    // copy out the old element
    @memcpy((out_element orelse unreachable)[0..element_width], element_at_index[0..element_width]);

    // copy in the new element
    @memcpy(element_at_index[0..element_width], (element orelse unreachable)[0..element_width]);

    return list;
}

pub fn listIsUnique(
    list: RocList,
) callconv(.C) bool {
    return list.isEmpty() or list.isUnique();
}

pub fn listCapacity(
    list: RocList,
) callconv(.C) usize {
    return list.getCapacity();
}

pub fn listRefcountPtr(
    list: RocList,
) callconv(.C) ?[*]u8 {
    return list.getRefcountPtr();
}

test "listConcat: non-unique with unique overlapping" {
    var nonUnique = RocList.fromSlice(u8, ([_]u8{1})[0..]);
    var bytes: [*]u8 = @as([*]u8, @ptrCast(nonUnique.bytes));
    const ptr_width = @sizeOf(usize);
    const refcount_ptr = @as([*]isize, @ptrCast(@as([*]align(ptr_width) u8, @alignCast(bytes)) - ptr_width));
    utils.increfRcPtrC(&refcount_ptr[0], 1);
    defer nonUnique.decref(@sizeOf(u8)); // listConcat will dec the other refcount

    var unique = RocList.fromSlice(u8, ([_]u8{ 2, 3, 4 })[0..]);
    defer unique.decref(@sizeOf(u8));

    var concatted = listConcat(nonUnique, unique, 1, 1);
    var wanted = RocList.fromSlice(u8, ([_]u8{ 1, 2, 3, 4 })[0..]);
    defer wanted.decref(@sizeOf(u8));

    try expect(concatted.eql(wanted));
}
