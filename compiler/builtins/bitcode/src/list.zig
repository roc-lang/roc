const std = @import("std");
const utils = @import("utils.zig");
const RocResult = utils.RocResult;
const UpdateMode = utils.UpdateMode;
const mem = std.mem;

const EqFn = fn (?[*]u8, ?[*]u8) callconv(.C) bool;
const CompareFn = fn (?[*]u8, ?[*]u8, ?[*]u8) callconv(.C) u8;
const Opaque = ?[*]u8;

const Inc = fn (?[*]u8) callconv(.C) void;
const IncN = fn (?[*]u8, usize) callconv(.C) void;
const Dec = fn (?[*]u8) callconv(.C) void;
const HasTagId = fn (u16, ?[*]u8) callconv(.C) extern struct { matched: bool, data: ?[*]u8 };

pub const RocList = extern struct {
    bytes: ?[*]u8,
    length: usize,

    pub fn len(self: RocList) usize {
        return self.length;
    }

    pub fn isEmpty(self: RocList) bool {
        return self.len() == 0;
    }

    pub fn empty() RocList {
        return RocList{ .bytes = null, .length = 0 };
    }

    pub fn isUnique(self: RocList) bool {
        // the empty list is unique (in the sense that copying it will not leak memory)
        if (self.isEmpty()) {
            return true;
        }

        // otherwise, check if the refcount is one
        const ptr: [*]usize = @ptrCast([*]usize, @alignCast(8, self.bytes));
        return (ptr - 1)[0] == utils.REFCOUNT_ONE;
    }

    pub fn allocate(
        alignment: u32,
        length: usize,
        element_size: usize,
    ) RocList {
        const data_bytes = length * element_size;

        return RocList{
            .bytes = utils.allocateWithRefcount(data_bytes, alignment),
            .length = length,
        };
    }

    pub fn makeUniqueExtra(self: RocList, alignment: u32, element_width: usize, update_mode: UpdateMode) RocList {
        if (update_mode == .InPlace) {
            return self;
        } else {
            return self.makeUnique(alignment, element_width);
        }
    }

    pub fn makeUnique(self: RocList, alignment: u32, element_width: usize) RocList {
        if (self.isEmpty()) {
            return self;
        }

        if (self.isUnique()) {
            return self;
        }

        // unfortunately, we have to clone
        var new_list = RocList.allocate(alignment, self.length, element_width);

        var old_bytes: [*]u8 = @ptrCast([*]u8, self.bytes);
        var new_bytes: [*]u8 = @ptrCast([*]u8, new_list.bytes);

        const number_of_bytes = self.len() * element_width;
        @memcpy(new_bytes, old_bytes, number_of_bytes);

        // NOTE we fuse an increment of all keys/values with a decrement of the input dict
        const data_bytes = self.len() * element_width;
        utils.decref(self.bytes, data_bytes, alignment);

        return new_list;
    }

    pub fn reallocate(
        self: RocList,
        alignment: u32,
        new_length: usize,
        element_width: usize,
    ) RocList {
        if (self.bytes) |source_ptr| {
            if (self.isUnique()) {
                const new_source = utils.unsafeReallocate(source_ptr, alignment, self.len(), new_length, element_width);

                return RocList{ .bytes = new_source, .length = new_length };
            }
        }

        return self.reallocateFresh(alignment, new_length, element_width);
    }

    /// reallocate by explicitly making a new allocation and copying elements over
    fn reallocateFresh(
        self: RocList,
        alignment: u32,
        new_length: usize,
        element_width: usize,
    ) RocList {
        const old_length = self.length;
        const delta_length = new_length - old_length;

        const data_bytes = new_length * element_width;
        const first_slot = utils.allocateWithRefcount(data_bytes, alignment);

        // transfer the memory

        if (self.bytes) |source_ptr| {
            const dest_ptr = first_slot;

            @memcpy(dest_ptr, source_ptr, old_length * element_width);
            @memset(dest_ptr + old_length * element_width, 0, delta_length * element_width);
        }

        const result = RocList{
            .bytes = first_slot,
            .length = new_length,
        };

        utils.decref(self.bytes, old_length * element_width, alignment);

        return result;
    }
};

const Caller0 = fn (?[*]u8, ?[*]u8) callconv(.C) void;
const Caller1 = fn (?[*]u8, ?[*]u8, ?[*]u8) callconv(.C) void;
const Caller2 = fn (?[*]u8, ?[*]u8, ?[*]u8, ?[*]u8) callconv(.C) void;
const Caller3 = fn (?[*]u8, ?[*]u8, ?[*]u8, ?[*]u8, ?[*]u8) callconv(.C) void;
const Caller4 = fn (?[*]u8, ?[*]u8, ?[*]u8, ?[*]u8, ?[*]u8, ?[*]u8) callconv(.C) void;

pub fn listReverse(list: RocList, alignment: u32, element_width: usize, update_mode: UpdateMode) callconv(.C) RocList {
    if (list.bytes) |source_ptr| {
        const size = list.len();

        var i: usize = 0;
        const end: usize = size - 1;

        if (update_mode == .InPlace or list.isUnique()) {

            // Working from the front and back so
            // we only need to go ~(n / 2) iterations.
            // If the length is an odd number the middle
            // element stays in the same place anyways.
            while (i < (end - i)) : (i += 1) {
                swapElements(source_ptr, element_width, i, end - i);
            }

            return list;
        } else {
            const output = RocList.allocate(alignment, size, element_width);

            const target_ptr = output.bytes orelse unreachable;

            while (i < size) : (i += 1) {
                const last_position = end - i;

                @memcpy(target_ptr + (i * element_width), source_ptr + (last_position * element_width), element_width);
            }

            utils.decref(list.bytes, size * element_width, alignment);

            return output;
        }
    } else {
        return RocList.empty();
    }
}

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

pub fn listMapWithIndex(
    list: RocList,
    caller: Caller2,
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
            caller(data, @ptrCast(?[*]u8, &i), source_ptr + (i * old_element_width), target_ptr + (i * new_element_width));
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
    const output_length = std.math.min(list1.len(), list2.len());

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
    const smaller_length = std.math.min(list1.len(), list2.len());
    const output_length = std.math.min(smaller_length, list3.len());

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
    const output_length = std.math.min(std.math.min(list1.len(), list2.len()), std.math.min(list3.len(), list4.len()));

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

pub fn listKeepIf(
    list: RocList,
    caller: Caller1,
    data: Opaque,
    inc_n_data: IncN,
    data_is_owned: bool,
    alignment: u32,
    element_width: usize,
    inc: Inc,
    dec: Dec,
) callconv(.C) RocList {
    if (list.bytes) |source_ptr| {
        const size = list.len();
        var i: usize = 0;
        var output = RocList.allocate(alignment, list.len(), list.len() * element_width);
        const target_ptr = output.bytes orelse unreachable;

        if (data_is_owned) {
            inc_n_data(data, size);
        }

        var kept: usize = 0;
        while (i < size) : (i += 1) {
            var keep = false;
            const element = source_ptr + (i * element_width);
            inc(element);
            caller(data, element, @ptrCast(?[*]u8, &keep));

            if (keep) {
                @memcpy(target_ptr + (kept * element_width), element, element_width);

                kept += 1;
            } else {
                dec(element);
            }
        }

        if (kept == 0) {
            // if the output is empty, deallocate the space we made for the result
            utils.decref(output.bytes, size * element_width, alignment);
            return RocList.empty();
        } else {
            output.length = kept;

            return output;
        }
    } else {
        return RocList.empty();
    }
}

pub fn listKeepOks(
    list: RocList,
    caller: Caller1,
    data: Opaque,
    inc_n_data: IncN,
    data_is_owned: bool,
    alignment: u32,
    before_width: usize,
    result_width: usize,
    after_width: usize,
    has_tag_id: HasTagId,
    dec_result: Dec,
) callconv(.C) RocList {
    const good_constructor: u16 = 1;

    return listKeepResult(
        list,
        good_constructor,
        caller,
        data,
        inc_n_data,
        data_is_owned,
        alignment,
        before_width,
        result_width,
        after_width,
        has_tag_id,
        dec_result,
    );
}

pub fn listKeepErrs(
    list: RocList,
    caller: Caller1,
    data: Opaque,
    inc_n_data: IncN,
    data_is_owned: bool,
    alignment: u32,
    before_width: usize,
    result_width: usize,
    after_width: usize,
    has_tag_id: HasTagId,
    dec_result: Dec,
) callconv(.C) RocList {
    const good_constructor: u16 = 0;

    return listKeepResult(
        list,
        good_constructor,
        caller,
        data,
        inc_n_data,
        data_is_owned,
        alignment,
        before_width,
        result_width,
        after_width,
        has_tag_id,
        dec_result,
    );
}

pub fn listKeepResult(
    list: RocList,
    good_constructor: u16,
    caller: Caller1,
    data: Opaque,
    inc_n_data: IncN,
    data_is_owned: bool,
    alignment: u32,
    before_width: usize,
    result_width: usize,
    after_width: usize,
    has_tag_id: HasTagId,
    dec_result: Dec,
) RocList {
    if (list.bytes) |source_ptr| {
        const size = list.len();
        var i: usize = 0;
        var output = RocList.allocate(alignment, list.len(), list.len() * after_width);
        const target_ptr = output.bytes orelse unreachable;

        var temporary = @ptrCast([*]u8, utils.alloc(result_width, alignment));

        if (data_is_owned) {
            inc_n_data(data, size);
        }

        var kept: usize = 0;
        while (i < size) : (i += 1) {
            const before_element = source_ptr + (i * before_width);
            caller(data, before_element, temporary);

            // a record { matched: bool, data: ?[*]u8 }
            // for now, that data pointer is just the input `temporary` pointer
            // this will change in the future to only return a pointer to the
            // payload of the tag
            const answer = has_tag_id(good_constructor, temporary);
            if (answer.matched) {
                const contents = (answer.data orelse unreachable);
                @memcpy(target_ptr + (kept * after_width), contents, after_width);
                kept += 1;
            } else {
                dec_result(temporary);
            }
        }

        utils.dealloc(temporary, alignment);

        if (kept == 0) {
            utils.decref(output.bytes, size * after_width, alignment);
            return RocList.empty();
        } else {
            output.length = kept;
            return output;
        }
    } else {
        return RocList.empty();
    }
}

pub fn listWalk(
    list: RocList,
    caller: Caller2,
    data: Opaque,
    inc_n_data: IncN,
    data_is_owned: bool,
    accum: Opaque,
    alignment: u32,
    element_width: usize,
    accum_width: usize,
    output: Opaque,
) callconv(.C) void {
    if (accum_width == 0) {
        return;
    }

    if (list.isEmpty()) {
        @memcpy(output orelse unreachable, accum orelse unreachable, accum_width);
        return;
    }

    if (data_is_owned) {
        inc_n_data(data, list.len());
    }

    const bytes_ptr: [*]u8 = utils.alloc(accum_width, alignment);
    var b1 = output orelse unreachable;
    var b2 = bytes_ptr;

    @memcpy(b2, accum orelse unreachable, accum_width);

    if (list.bytes) |source_ptr| {
        var i: usize = 0;
        const size = list.len();
        while (i < size) : (i += 1) {
            const element = source_ptr + i * element_width;
            caller(data, b2, element, b1);

            std.mem.swap([*]u8, &b1, &b2);
        }
    }

    @memcpy(output orelse unreachable, b2, accum_width);
    utils.dealloc(bytes_ptr, alignment);
}

pub fn listWalkBackwards(
    list: RocList,
    caller: Caller2,
    data: Opaque,
    inc_n_data: IncN,
    data_is_owned: bool,
    accum: Opaque,
    alignment: u32,
    element_width: usize,
    accum_width: usize,
    output: Opaque,
) callconv(.C) void {
    if (accum_width == 0) {
        return;
    }

    if (list.isEmpty()) {
        @memcpy(output orelse unreachable, accum orelse unreachable, accum_width);
        return;
    }

    if (data_is_owned) {
        inc_n_data(data, list.len());
    }

    const bytes_ptr: [*]u8 = utils.alloc(accum_width, alignment);
    var b1 = output orelse unreachable;
    var b2 = bytes_ptr;

    @memcpy(b2, accum orelse unreachable, accum_width);

    if (list.bytes) |source_ptr| {
        const size = list.len();
        var i: usize = size;
        while (i > 0) {
            i -= 1;
            const element = source_ptr + i * element_width;
            caller(data, b2, element, b1);

            std.mem.swap([*]u8, &b1, &b2);
        }
    }

    @memcpy(output orelse unreachable, b2, accum_width);
    utils.dealloc(bytes_ptr, alignment);
}

pub fn listWalkUntil(
    list: RocList,
    caller: Caller2,
    data: Opaque,
    inc_n_data: IncN,
    data_is_owned: bool,
    accum: Opaque,
    alignment: u32,
    element_width: usize,
    continue_stop_width: usize,
    accum_width: usize,
    has_tag_id: HasTagId,
    dec: Dec,
    output: Opaque,
) callconv(.C) void {
    // [ Continue a, Stop a ]

    if (accum_width == 0) {
        return;
    }

    if (list.isEmpty()) {
        @memcpy(output orelse unreachable, accum orelse unreachable, accum_width);
        return;
    }

    const bytes_ptr: [*]u8 = utils.alloc(continue_stop_width, alignment);

    // NOTE: assumes data bytes are the first bytes in a tag
    @memcpy(bytes_ptr, accum orelse unreachable, accum_width);

    if (list.bytes) |source_ptr| {
        var i: usize = 0;
        const size = list.len();
        while (i < size) : (i += 1) {
            const element = source_ptr + i * element_width;

            if (data_is_owned) {
                inc_n_data(data, 1);
            }

            caller(data, bytes_ptr, element, bytes_ptr);

            // [ Continue ..., Stop ]
            const tag_id = has_tag_id(0, bytes_ptr);

            if (!tag_id.matched) {
                // decrement refcount of the remaining items
                i += 1;
                while (i < size) : (i += 1) {
                    dec(source_ptr + i * element_width);
                }
                break;
            }
        }
    }

    @memcpy(output orelse unreachable, bytes_ptr, accum_width);
    utils.dealloc(bytes_ptr, alignment);
}

// List.contains : List k, k -> Bool
pub fn listContains(list: RocList, key: Opaque, key_width: usize, is_eq: EqFn) callconv(.C) bool {
    if (list.bytes) |source_ptr| {
        const size = list.len();
        var i: usize = 0;
        while (i < size) : (i += 1) {
            const element = source_ptr + i * key_width;
            if (is_eq(element, key)) {
                return true;
            }
        }
    }

    return false;
}

pub fn listRepeat(count: usize, alignment: u32, element: Opaque, element_width: usize, inc_n_element: IncN) callconv(.C) RocList {
    if (count == 0) {
        return RocList.empty();
    }

    var output = RocList.allocate(alignment, count, element_width);

    if (output.bytes) |target_ptr| {
        // increment the element's RC N times
        inc_n_element(element, count);

        var i: usize = 0;
        const source = element orelse unreachable;
        while (i < count) : (i += 1) {
            @memcpy(target_ptr + i * element_width, source, element_width);
        }

        return output;
    } else {
        unreachable;
    }
}

pub fn listSingle(alignment: u32, element: Opaque, element_width: usize) callconv(.C) RocList {
    var output = RocList.allocate(alignment, 1, element_width);

    if (output.bytes) |target| {
        if (element) |source| {
            @memcpy(target, source, element_width);
        }
    }

    return output;
}

pub fn listAppend(list: RocList, alignment: u32, element: Opaque, element_width: usize, update_mode: UpdateMode) callconv(.C) RocList {
    const old_length = list.len();
    var output = list.reallocate(alignment, old_length + 1, element_width);

    // we'd need capacity to use update_mode here
    _ = update_mode;

    if (output.bytes) |target| {
        if (element) |source| {
            @memcpy(target + old_length * element_width, source, element_width);
        }
    }

    return output;
}

pub fn listPrepend(list: RocList, alignment: u32, element: Opaque, element_width: usize) callconv(.C) RocList {
    const old_length = list.len();
    var output = list.reallocate(alignment, old_length + 1, element_width);

    // can't use one memcpy here because source and target overlap
    if (output.bytes) |target| {
        var i: usize = old_length;

        while (i > 0) {
            i -= 1;

            // move the ith element to the (i + 1)th position
            @memcpy(target + (i + 1) * element_width, target + i * element_width, element_width);
        }

        // finally copy in the new first element
        if (element) |source| {
            @memcpy(target, source, element_width);
        }
    }

    return output;
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

    const source_ptr = @ptrCast([*]u8, newList.bytes);
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
    if (len == 0) {
        return RocList.empty();
    }
    if (list.bytes) |source_ptr| {
        const size = list.len();

        if (start >= size) {
            return RocList.empty();
        }

        const keep_len = std.math.min(len, size - start);
        const drop_len = std.math.max(start, 0);

        var i: usize = 0;
        while (i < drop_len) : (i += 1) {
            const element = source_ptr + i * element_width;
            dec(element);
        }

        const output = RocList.allocate(alignment, keep_len, element_width);
        const target_ptr = output.bytes orelse unreachable;

        @memcpy(target_ptr, source_ptr + start * element_width, keep_len * element_width);

        utils.decref(list.bytes, size * element_width, alignment);

        return output;
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
    if (list.bytes) |source_ptr| {
        const size = list.len();

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
            utils.decref(list.bytes, size * element_width, alignment);
            return RocList.empty();
        }

        if (list.isUnique()) {
            var i = drop_index;
            while (i < size) : (i += 1) {
                const copy_target = source_ptr + i * element_width;
                const copy_source = copy_target + element_width;
                @memcpy(copy_target, copy_source, element_width);
            }

            var new_list = list;

            new_list.length -= 1;
            return new_list;
        }

        const output = RocList.allocate(alignment, size - 1, element_width);
        const target_ptr = output.bytes orelse unreachable;

        const head_size = drop_index * element_width;
        @memcpy(target_ptr, source_ptr, head_size);

        const tail_target = target_ptr + drop_index * element_width;
        const tail_source = source_ptr + (drop_index + 1) * element_width;
        const tail_size = (size - drop_index - 1) * element_width;
        @memcpy(tail_target, tail_source, tail_size);

        utils.decref(list.bytes, size * element_width, alignment);

        return output;
    } else {
        return RocList.empty();
    }
}

pub fn listRange(width: utils.IntWidth, low: Opaque, high: Opaque) callconv(.C) RocList {
    return switch (width) {
        .U8 => helper1(u8, low, high),
        .U16 => helper1(u16, low, high),
        .U32 => helper1(u32, low, high),
        .U64 => helper1(u64, low, high),
        .U128 => helper1(u128, low, high),
        .I8 => helper1(i8, low, high),
        .I16 => helper1(i16, low, high),
        .I32 => helper1(i32, low, high),
        .I64 => helper1(i64, low, high),
        .I128 => helper1(i128, low, high),
        .Usize => helper1(usize, low, high),
    };
}

fn helper1(comptime T: type, low: Opaque, high: Opaque) RocList {
    const ptr1 = @ptrCast(*T, @alignCast(@alignOf(T), low));
    const ptr2 = @ptrCast(*T, @alignCast(@alignOf(T), high));

    return listRangeHelp(T, ptr1.*, ptr2.*);
}

fn listRangeHelp(comptime T: type, low: T, high: T) RocList {
    const Order = std.math.Order;

    switch (std.math.order(low, high)) {
        Order.gt => {
            return RocList.empty();
        },

        Order.eq => {
            const list = RocList.allocate(@alignOf(usize), 1, @sizeOf(T));
            const buffer = @ptrCast([*]T, @alignCast(@alignOf(T), list.bytes orelse unreachable));

            buffer[0] = low;

            return list;
        },

        Order.lt => {
            const length: usize = @intCast(usize, high - low);
            const list = RocList.allocate(@alignOf(usize), length, @sizeOf(T));

            const buffer = @ptrCast([*]T, @alignCast(@alignOf(T), list.bytes orelse unreachable));

            var i: usize = 0;
            var current = low;

            while (i < length) {
                buffer[i] = current;

                i += 1;
                current += 1;
            }

            return list;
        },
    }
}

fn partition(source_ptr: [*]u8, transform: Opaque, wrapper: CompareFn, element_width: usize, low: isize, high: isize) isize {
    const pivot = source_ptr + (@intCast(usize, high) * element_width);
    var i = (low - 1); // Index of smaller element and indicates the right position of pivot found so far
    var j = low;

    while (j <= high - 1) : (j += 1) {
        const current_elem = source_ptr + (@intCast(usize, j) * element_width);

        const ordering = wrapper(transform, current_elem, pivot);
        const order = @intToEnum(utils.Ordering, ordering);

        switch (order) {
            utils.Ordering.LT => {
                // the current element is smaller than the pivot; swap it
                i += 1;
                swapElements(source_ptr, element_width, @intCast(usize, i), @intCast(usize, j));
            },
            utils.Ordering.EQ, utils.Ordering.GT => {},
        }
    }
    swapElements(source_ptr, element_width, @intCast(usize, i + 1), @intCast(usize, high));
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
        const high: isize = @intCast(isize, list.len()) - 1;
        quicksort(source_ptr, data, caller, element_width, low, high);
    }

    return list;
}

pub fn listAny(
    list: RocList,
    caller: Caller1,
    data: Opaque,
    inc_n_data: IncN,
    data_is_owned: bool,
    element_width: usize,
) callconv(.C) bool {
    if (list.bytes) |source_ptr| {
        const size = list.len();

        if (data_is_owned) {
            inc_n_data(data, size);
        }

        var i: usize = 0;
        var satisfied = false;
        while (i < size) : (i += 1) {
            const element = source_ptr + i * element_width;
            caller(data, element, @ptrCast(?[*]u8, &satisfied));

            if (satisfied) {
                return satisfied;
            }
        }
    }

    return false;
}

// SWAP ELEMENTS

inline fn swapHelp(width: usize, temporary: [*]u8, ptr1: [*]u8, ptr2: [*]u8) void {
    @memcpy(temporary, ptr1, width);
    @memcpy(ptr1, ptr2, width);
    @memcpy(ptr2, temporary, width);
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

pub fn listJoin(list_of_lists: RocList, alignment: u32, element_width: usize) callconv(.C) RocList {
    var total_length: usize = 0;

    const slice_of_lists = @ptrCast([*]RocList, @alignCast(@alignOf(RocList), list_of_lists.bytes));

    var i: usize = 0;
    while (i < list_of_lists.len()) : (i += 1) {
        total_length += slice_of_lists[i].len();
    }

    const output = RocList.allocate(alignment, total_length, element_width);

    if (output.bytes) |target| {
        var elements_copied: usize = 0;

        i = 0;
        while (i < list_of_lists.len()) : (i += 1) {
            const list = slice_of_lists[i];
            if (list.bytes) |source| {
                @memcpy(target + elements_copied * element_width, source, list.len() * element_width);
                elements_copied += list.len();
            }
        }
    }

    return output;
}

pub fn listConcat(list_a: RocList, list_b: RocList, alignment: u32, element_width: usize) callconv(.C) RocList {
    if (list_a.isEmpty()) {
        return list_b;
    } else if (list_b.isEmpty()) {
        return list_a;
    } else if (!list_a.isEmpty() and list_a.isUnique()) {
        const total_length: usize = list_a.len() + list_b.len();

        if (list_a.bytes) |source| {
            const new_source = utils.unsafeReallocate(
                source,
                alignment,
                list_a.len(),
                total_length,
                element_width,
            );

            if (list_b.bytes) |source_b| {
                @memcpy(new_source + list_a.len() * element_width, source_b, list_b.len() * element_width);
            }

            return RocList{ .bytes = new_source, .length = total_length };
        }
    }
    const total_length: usize = list_a.len() + list_b.len();

    const output = RocList.allocate(alignment, total_length, element_width);

    if (output.bytes) |target| {
        if (list_a.bytes) |source| {
            @memcpy(target, source, list_a.len() * element_width);
        }
        if (list_b.bytes) |source| {
            @memcpy(target + list_a.len() * element_width, source, list_b.len() * element_width);
        }
    }

    return output;
}

pub fn listSetInPlace(
    bytes: ?[*]u8,
    index: usize,
    element: Opaque,
    element_width: usize,
    dec: Dec,
) callconv(.C) ?[*]u8 {
    // INVARIANT: bounds checking happens on the roc side
    //
    // at the time of writing, the function is implemented roughly as
    // `if inBounds then LowLevelListGet input index item else input`
    // so we don't do a bounds check here. Hence, the list is also non-empty,
    // because inserting into an empty list is always out of bounds

    return listSetInPlaceHelp(bytes, index, element, element_width, dec);
}

pub fn listSet(
    bytes: ?[*]u8,
    length: usize,
    alignment: u32,
    index: usize,
    element: Opaque,
    element_width: usize,
    dec: Dec,
) callconv(.C) ?[*]u8 {
    // INVARIANT: bounds checking happens on the roc side
    //
    // at the time of writing, the function is implemented roughly as
    // `if inBounds then LowLevelListGet input index item else input`
    // so we don't do a bounds check here. Hence, the list is also non-empty,
    // because inserting into an empty list is always out of bounds
    const ptr: [*]usize = @ptrCast([*]usize, @alignCast(8, bytes));

    if ((ptr - 1)[0] == utils.REFCOUNT_ONE) {
        return listSetInPlaceHelp(bytes, index, element, element_width, dec);
    } else {
        return listSetImmutable(bytes, length, alignment, index, element, element_width, dec);
    }
}

inline fn listSetInPlaceHelp(
    bytes: ?[*]u8,
    index: usize,
    element: Opaque,
    element_width: usize,
    dec: Dec,
) ?[*]u8 {
    // the element we will replace
    var element_at_index = (bytes orelse undefined) + (index * element_width);

    // decrement its refcount
    dec(element_at_index);

    // copy in the new element
    @memcpy(element_at_index, element orelse undefined, element_width);

    return bytes;
}

inline fn listSetImmutable(
    old_bytes: ?[*]u8,
    length: usize,
    alignment: u32,
    index: usize,
    element: Opaque,
    element_width: usize,
    dec: Dec,
) ?[*]u8 {
    const data_bytes = length * element_width;

    var new_bytes = utils.allocateWithRefcount(data_bytes, alignment);

    @memcpy(new_bytes, old_bytes orelse undefined, data_bytes);

    // the element we will replace
    var element_at_index = new_bytes + (index * element_width);

    // decrement its refcount
    dec(element_at_index);

    // copy in the new element
    @memcpy(element_at_index, element orelse undefined, element_width);

    // consume RC token of original
    utils.decref(old_bytes, data_bytes, alignment);

    //return list;
    return new_bytes;
}

pub fn listFindUnsafe(
    list: RocList,
    caller: Caller1,
    data: Opaque,
    inc_n_data: IncN,
    data_is_owned: bool,
    alignment: u32,
    element_width: usize,
    inc: Inc,
    dec: Dec,
) callconv(.C) extern struct { value: Opaque, found: bool } {
    if (list.bytes) |source_ptr| {
        const size = list.len();
        if (data_is_owned) {
            inc_n_data(data, size);
        }

        var i: usize = 0;
        while (i < size) : (i += 1) {
            var theOne = false;
            const element = source_ptr + (i * element_width);
            inc(element);
            caller(data, element, @ptrCast(?[*]u8, &theOne));

            if (theOne) {
                return .{ .value = element, .found = true };
            } else {
                dec(element);
            }
        }
        return .{ .value = null, .found = false };
    } else {
        return .{ .value = null, .found = false };
    }
}
