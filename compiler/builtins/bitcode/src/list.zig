const std = @import("std");
const utils = @import("utils.zig");
const RocResult = utils.RocResult;
const mem = std.mem;
const Allocator = mem.Allocator;

const TAG_WIDTH = 8;

const EqFn = fn (?[*]u8, ?[*]u8) callconv(.C) bool;
const CompareFn = fn (?[*]u8, ?[*]u8, ?[*]u8) callconv(.C) u8;
const Opaque = ?[*]u8;

const Inc = fn (?[*]u8) callconv(.C) void;
const Dec = fn (?[*]u8) callconv(.C) void;

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
        allocator: *Allocator,
        alignment: usize,
        length: usize,
        element_size: usize,
    ) RocList {
        const data_bytes = length * element_size;

        return RocList{
            .bytes = utils.allocateWithRefcount(allocator, alignment, data_bytes),
            .length = length,
        };
    }

    pub fn makeUnique(self: RocList, allocator: *Allocator, alignment: usize, element_width: usize) RocList {
        if (self.isEmpty()) {
            return self;
        }

        if (self.isUnique()) {
            return self;
        }

        // unfortunately, we have to clone
        var new_list = RocList.allocate(allocator, alignment, self.length, element_width);

        var old_bytes: [*]u8 = @ptrCast([*]u8, self.bytes);
        var new_bytes: [*]u8 = @ptrCast([*]u8, new_list.bytes);

        const number_of_bytes = self.len() * element_width;
        @memcpy(new_bytes, old_bytes, number_of_bytes);

        // NOTE we fuse an increment of all keys/values with a decrement of the input dict
        const data_bytes = self.len() * element_width;
        utils.decref(allocator, alignment, self.bytes, data_bytes);

        return new_list;
    }

    pub fn reallocate(
        self: RocList,
        allocator: *Allocator,
        alignment: usize,
        new_length: usize,
        element_width: usize,
    ) RocList {
        const old_length = self.length;
        const delta_length = new_length - old_length;

        const data_bytes = new_length * element_width;
        const first_slot = utils.allocateWithRefcount(allocator, alignment, data_bytes);

        // transfer the memory

        if (self.bytes) |source_ptr| {
            const dest_ptr = first_slot;

            @memcpy(dest_ptr, source_ptr, old_length * element_width);
            @memset(dest_ptr + old_length * element_width, 0, delta_length * element_width);
        }

        // NOTE the newly added elements are left uninitialized

        const result = RocList{
            .bytes = first_slot,
            .length = new_length,
        };

        // NOTE we fuse an increment of all keys/values with a decrement of the input dict
        utils.decref(allocator, alignment, self.bytes, old_length * element_width);

        return result;
    }
};

const Caller1 = fn (?[*]u8, ?[*]u8, ?[*]u8) callconv(.C) void;
const Caller2 = fn (?[*]u8, ?[*]u8, ?[*]u8, ?[*]u8) callconv(.C) void;
const Caller3 = fn (?[*]u8, ?[*]u8, ?[*]u8, ?[*]u8, ?[*]u8) callconv(.C) void;

pub fn listMap(list: RocList, transform: Opaque, caller: Caller1, alignment: usize, old_element_width: usize, new_element_width: usize) callconv(.C) RocList {
    if (list.bytes) |source_ptr| {
        const size = list.len();
        var i: usize = 0;
        const output = RocList.allocate(std.heap.c_allocator, alignment, size, new_element_width);
        const target_ptr = output.bytes orelse unreachable;

        while (i < size) : (i += 1) {
            caller(transform, source_ptr + (i * old_element_width), target_ptr + (i * new_element_width));
        }

        utils.decref(std.heap.c_allocator, alignment, list.bytes, size * old_element_width);

        return output;
    } else {
        return RocList.empty();
    }
}

pub fn listMapWithIndex(list: RocList, transform: Opaque, caller: Caller2, alignment: usize, old_element_width: usize, new_element_width: usize) callconv(.C) RocList {
    if (list.bytes) |source_ptr| {
        const size = list.len();
        var i: usize = 0;
        const output = RocList.allocate(std.heap.c_allocator, alignment, size, new_element_width);
        const target_ptr = output.bytes orelse unreachable;

        while (i < size) : (i += 1) {
            caller(transform, @ptrCast(?[*]u8, &i), source_ptr + (i * old_element_width), target_ptr + (i * new_element_width));
        }

        utils.decref(std.heap.c_allocator, alignment, list.bytes, size * old_element_width);

        return output;
    } else {
        return RocList.empty();
    }
}

pub fn listMap2(list1: RocList, list2: RocList, transform: Opaque, caller: Caller2, alignment: usize, a_width: usize, b_width: usize, c_width: usize, dec_a: Dec, dec_b: Dec) callconv(.C) RocList {
    const output_length = std.math.min(list1.len(), list2.len());

    if (list1.bytes) |source_a| {
        if (list2.bytes) |source_b| {
            const output = RocList.allocate(std.heap.c_allocator, alignment, output_length, c_width);
            const target_ptr = output.bytes orelse unreachable;

            var i: usize = 0;
            while (i < output_length) : (i += 1) {
                const element_a = source_a + i * a_width;
                const element_b = source_b + i * b_width;
                const target = target_ptr + i * c_width;
                caller(transform, element_a, element_b, target);
            }

            // if the lists don't have equal length, we must consume the remaining elements
            // In this case we consume by (recursively) decrementing the elements
            if (list1.len() > output_length) {
                while (i < list1.len()) : (i += 1) {
                    const element_a = source_a + i * a_width;
                    dec_a(element_a);
                }
            } else if (list2.len() > output_length) {
                while (i < list2.len()) : (i += 1) {
                    const element_b = source_b + i * b_width;
                    dec_b(element_b);
                }
            }

            utils.decref(std.heap.c_allocator, alignment, list1.bytes, list1.len() * a_width);
            utils.decref(std.heap.c_allocator, alignment, list2.bytes, list2.len() * b_width);

            return output;
        } else {
            // consume list1 elements (we know there is at least one because the list1.bytes pointer is non-null
            var i: usize = 0;
            while (i < list1.len()) : (i += 1) {
                const element_a = source_a + i * a_width;
                dec_a(element_a);
            }
            utils.decref(std.heap.c_allocator, alignment, list1.bytes, list1.len() * a_width);

            return RocList.empty();
        }
    } else {
        // consume list2 elements (if any)
        if (list2.bytes) |source_b| {
            var i: usize = 0;
            while (i < list2.len()) : (i += 1) {
                const element_b = source_b + i * b_width;
                dec_b(element_b);
            }
            utils.decref(std.heap.c_allocator, alignment, list2.bytes, list2.len() * b_width);
        }

        return RocList.empty();
    }
}

pub fn listMap3(list1: RocList, list2: RocList, list3: RocList, transform: Opaque, caller: Caller3, alignment: usize, a_width: usize, b_width: usize, c_width: usize, d_width: usize, dec_a: Dec, dec_b: Dec, dec_c: Dec) callconv(.C) RocList {
    const smaller_length = std.math.min(list1.len(), list2.len());
    const output_length = std.math.min(smaller_length, list3.len());

    if (list1.bytes) |source_a| {
        if (list2.bytes) |source_b| {
            if (list3.bytes) |source_c| {
                const output = RocList.allocate(std.heap.c_allocator, alignment, output_length, d_width);
                const target_ptr = output.bytes orelse unreachable;

                var i: usize = 0;
                while (i < output_length) : (i += 1) {
                    const element_a = source_a + i * a_width;
                    const element_b = source_b + i * b_width;
                    const element_c = source_c + i * c_width;
                    const target = target_ptr + i * d_width;

                    caller(transform, element_a, element_b, element_c, target);
                }

                // if the lists don't have equal length, we must consume the remaining elements
                // In this case we consume by (recursively) decrementing the elements
                if (list1.len() > output_length) {
                    i = output_length;
                    while (i < list1.len()) : (i += 1) {
                        const element_a = source_a + i * a_width;
                        dec_a(element_a);
                    }
                }

                if (list2.len() > output_length) {
                    i = output_length;
                    while (i < list2.len()) : (i += 1) {
                        const element_b = source_b + i * b_width;
                        dec_b(element_b);
                    }
                }

                if (list3.len() > output_length) {
                    i = output_length;
                    while (i < list3.len()) : (i += 1) {
                        const element_c = source_c + i * c_width;
                        dec_c(element_c);
                    }
                }

                utils.decref(std.heap.c_allocator, alignment, list1.bytes, list1.len() * a_width);
                utils.decref(std.heap.c_allocator, alignment, list2.bytes, list2.len() * b_width);
                utils.decref(std.heap.c_allocator, alignment, list3.bytes, list3.len() * c_width);

                return output;
            } else {
                // consume list1 elements (we know there is at least one because the list1.bytes pointer is non-null
                var i: usize = 0;
                while (i < list1.len()) : (i += 1) {
                    const element_a = source_a + i * a_width;
                    dec_a(element_a);
                }
                utils.decref(std.heap.c_allocator, alignment, list1.bytes, list1.len() * a_width);

                // consume list2 elements (we know there is at least one because the list1.bytes pointer is non-null
                i = 0;
                while (i < list2.len()) : (i += 1) {
                    const element_b = source_b + i * b_width;
                    dec_b(element_b);
                }
                utils.decref(std.heap.c_allocator, alignment, list2.bytes, list2.len() * b_width);

                return RocList.empty();
            }
        } else {
            // consume list1 elements (we know there is at least one because the list1.bytes pointer is non-null
            var i: usize = 0;
            while (i < list1.len()) : (i += 1) {
                const element_a = source_a + i * a_width;
                dec_a(element_a);
            }

            utils.decref(std.heap.c_allocator, alignment, list1.bytes, list1.len() * a_width);

            // consume list3 elements (if any)
            if (list3.bytes) |source_c| {
                i = 0;

                while (i < list2.len()) : (i += 1) {
                    const element_c = source_c + i * c_width;
                    dec_c(element_c);
                }

                utils.decref(std.heap.c_allocator, alignment, list3.bytes, list3.len() * c_width);
            }

            return RocList.empty();
        }
    } else {
        // consume list2 elements (if any)
        if (list2.bytes) |source_b| {
            var i: usize = 0;

            while (i < list2.len()) : (i += 1) {
                const element_b = source_b + i * b_width;
                dec_b(element_b);
            }

            utils.decref(std.heap.c_allocator, alignment, list2.bytes, list2.len() * b_width);
        }

        // consume list3 elements (if any)
        if (list3.bytes) |source_c| {
            var i: usize = 0;

            while (i < list2.len()) : (i += 1) {
                const element_c = source_c + i * c_width;
                dec_c(element_c);
            }

            utils.decref(std.heap.c_allocator, alignment, list3.bytes, list3.len() * c_width);
        }

        return RocList.empty();
    }
}

pub fn listKeepIf(list: RocList, transform: Opaque, caller: Caller1, alignment: usize, element_width: usize, inc: Inc, dec: Dec) callconv(.C) RocList {
    if (list.bytes) |source_ptr| {
        const size = list.len();
        var i: usize = 0;
        var output = RocList.allocate(std.heap.c_allocator, alignment, list.len(), list.len() * element_width);
        const target_ptr = output.bytes orelse unreachable;

        var kept: usize = 0;
        while (i < size) : (i += 1) {
            var keep = false;
            const element = source_ptr + (i * element_width);
            inc(element);
            caller(transform, element, @ptrCast(?[*]u8, &keep));

            if (keep) {
                @memcpy(target_ptr + (kept * element_width), element, element_width);

                kept += 1;
            } else {
                dec(element);
            }
        }

        // consume the input list
        utils.decref(std.heap.c_allocator, alignment, list.bytes, size * element_width);

        if (kept == 0) {
            // if the output is empty, deallocate the space we made for the result
            utils.decref(std.heap.c_allocator, alignment, output.bytes, size * element_width);
            return RocList.empty();
        } else {
            output.length = kept;

            return output;
        }
    } else {
        return RocList.empty();
    }
}

pub fn listKeepOks(list: RocList, transform: Opaque, caller: Caller1, alignment: usize, before_width: usize, result_width: usize, after_width: usize, inc_closure: Inc, dec_result: Dec) callconv(.C) RocList {
    return listKeepResult(list, RocResult.isOk, transform, caller, alignment, before_width, result_width, after_width, inc_closure, dec_result);
}

pub fn listKeepErrs(list: RocList, transform: Opaque, caller: Caller1, alignment: usize, before_width: usize, result_width: usize, after_width: usize, inc_closure: Inc, dec_result: Dec) callconv(.C) RocList {
    return listKeepResult(list, RocResult.isErr, transform, caller, alignment, before_width, result_width, after_width, inc_closure, dec_result);
}

pub fn listKeepResult(list: RocList, is_good_constructor: fn (RocResult) bool, transform: Opaque, caller: Caller1, alignment: usize, before_width: usize, result_width: usize, after_width: usize, inc_closure: Inc, dec_result: Dec) RocList {
    if (list.bytes) |source_ptr| {
        const size = list.len();
        var i: usize = 0;
        var output = RocList.allocate(std.heap.c_allocator, alignment, list.len(), list.len() * after_width);
        const target_ptr = output.bytes orelse unreachable;

        var temporary = @ptrCast([*]u8, std.heap.c_allocator.alloc(u8, result_width) catch unreachable);

        var kept: usize = 0;
        while (i < size) : (i += 1) {
            const before_element = source_ptr + (i * before_width);
            inc_closure(transform);
            caller(transform, before_element, temporary);

            const result = utils.RocResult{ .bytes = temporary };

            const after_element = temporary + @sizeOf(i64);
            if (is_good_constructor(result)) {
                @memcpy(target_ptr + (kept * after_width), after_element, after_width);
                kept += 1;
            } else {
                dec_result(temporary);
            }
        }

        utils.decref(std.heap.c_allocator, alignment, list.bytes, size * before_width);
        std.heap.c_allocator.free(temporary[0..result_width]);

        if (kept == 0) {
            utils.decref(std.heap.c_allocator, alignment, output.bytes, size * after_width);
            return RocList.empty();
        } else {
            output.length = kept;
            return output;
        }
    } else {
        return RocList.empty();
    }
}

pub fn listWalk(list: RocList, stepper: Opaque, stepper_caller: Caller2, accum: Opaque, alignment: usize, element_width: usize, accum_width: usize, output: Opaque) callconv(.C) void {
    if (accum_width == 0) {
        return;
    }

    if (list.isEmpty()) {
        @memcpy(output orelse unreachable, accum orelse unreachable, accum_width);
        return;
    }

    const alloc: [*]u8 = @ptrCast([*]u8, std.heap.c_allocator.alloc(u8, accum_width) catch unreachable);
    var b1 = output orelse unreachable;
    var b2 = alloc;

    @memcpy(b2, accum orelse unreachable, accum_width);

    if (list.bytes) |source_ptr| {
        var i: usize = 0;
        const size = list.len();
        while (i < size) : (i += 1) {
            const element = source_ptr + i * element_width;
            stepper_caller(stepper, element, b2, b1);

            const temp = b1;
            b2 = b1;
            b1 = temp;
        }
    }

    @memcpy(output orelse unreachable, b2, accum_width);
    std.heap.c_allocator.free(alloc[0..accum_width]);

    const data_bytes = list.len() * element_width;
    utils.decref(std.heap.c_allocator, alignment, list.bytes, data_bytes);
}

pub fn listWalkBackwards(list: RocList, stepper: Opaque, stepper_caller: Caller2, accum: Opaque, alignment: usize, element_width: usize, accum_width: usize, output: Opaque) callconv(.C) void {
    if (accum_width == 0) {
        return;
    }

    if (list.isEmpty()) {
        @memcpy(output orelse unreachable, accum orelse unreachable, accum_width);
        return;
    }

    const alloc: [*]u8 = @ptrCast([*]u8, std.heap.c_allocator.alloc(u8, accum_width) catch unreachable);
    var b1 = output orelse unreachable;
    var b2 = alloc;

    @memcpy(b2, accum orelse unreachable, accum_width);

    if (list.bytes) |source_ptr| {
        const size = list.len();
        var i: usize = size;
        while (i > 0) {
            i -= 1;
            const element = source_ptr + i * element_width;
            stepper_caller(stepper, element, b2, b1);

            const temp = b1;
            b2 = b1;
            b1 = temp;
        }

        const data_bytes = list.len() * element_width;
        utils.decref(std.heap.c_allocator, alignment, list.bytes, data_bytes);
    }

    @memcpy(output orelse unreachable, b2, accum_width);
    std.heap.c_allocator.free(alloc[0..accum_width]);

    const data_bytes = list.len() * element_width;
    utils.decref(std.heap.c_allocator, alignment, list.bytes, data_bytes);
}

pub fn listWalkUntil(list: RocList, stepper: Opaque, stepper_caller: Caller2, accum: Opaque, alignment: usize, element_width: usize, accum_width: usize, dec: Dec, output: Opaque) callconv(.C) void {
    // [ Continue a, Stop a ]
    const CONTINUE: usize = 0;

    if (accum_width == 0) {
        return;
    }

    if (list.isEmpty()) {
        @memcpy(output orelse unreachable, accum orelse unreachable, accum_width);
        return;
    }

    const alloc: [*]u8 = @ptrCast([*]u8, std.heap.c_allocator.alloc(u8, TAG_WIDTH + accum_width) catch unreachable);

    @memcpy(alloc + TAG_WIDTH, accum orelse unreachable, accum_width);

    if (list.bytes) |source_ptr| {
        var i: usize = 0;
        const size = list.len();
        while (i < size) : (i += 1) {
            const element = source_ptr + i * element_width;
            stepper_caller(stepper, element, alloc + TAG_WIDTH, alloc);

            const usizes: [*]usize = @ptrCast([*]usize, @alignCast(8, alloc));
            if (usizes[0] != 0) {
                // decrement refcount of the remaining items
                i += 1;
                while (i < size) : (i += 1) {
                    dec(source_ptr + i * element_width);
                }
                break;
            }
        }
    }

    @memcpy(output orelse unreachable, alloc + TAG_WIDTH, accum_width);
    std.heap.c_allocator.free(alloc[0 .. TAG_WIDTH + accum_width]);

    const data_bytes = list.len() * element_width;
    utils.decref(std.heap.c_allocator, alignment, list.bytes, data_bytes);
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

pub fn listRepeat(count: usize, alignment: usize, element: Opaque, element_width: usize, inc_n_element: Inc) callconv(.C) RocList {
    if (count == 0) {
        return RocList.empty();
    }

    const allocator = std.heap.c_allocator;
    var output = RocList.allocate(allocator, alignment, count, element_width);

    if (output.bytes) |target_ptr| {
        var i: usize = 0;
        const source = element orelse unreachable;
        while (i < count) : (i += 1) {
            @memcpy(target_ptr + i * element_width, source, element_width);
        }

        // TODO do all increments at once!
        i = 0;
        while (i < count) : (i += 1) {
            inc_n_element(element);
        }

        return output;
    } else {
        unreachable;
    }
}

pub fn listAppend(list: RocList, alignment: usize, element: Opaque, element_width: usize) callconv(.C) RocList {
    const old_length = list.len();
    var output = list.reallocate(std.heap.c_allocator, alignment, old_length + 1, element_width);

    if (output.bytes) |target| {
        if (element) |source| {
            @memcpy(target + old_length * element_width, source, element_width);
        }
    }

    return output;
}

pub fn listRange(width: utils.IntWidth, low: Opaque, high: Opaque) callconv(.C) RocList {
    const allocator = std.heap.c_allocator;
    const IntWidth = utils.IntWidth;

    switch (width) {
        IntWidth.U8 => {
            return helper1(allocator, u8, low, high);
        },
        IntWidth.U16 => {
            return helper1(allocator, u16, low, high);
        },
        IntWidth.U32 => {
            return helper1(allocator, u32, low, high);
        },
        IntWidth.U64 => {
            return helper1(allocator, u64, low, high);
        },
        IntWidth.U128 => {
            return helper1(allocator, u128, low, high);
        },
        IntWidth.I8 => {
            return helper1(allocator, i8, low, high);
        },
        IntWidth.I16 => {
            return helper1(allocator, i16, low, high);
        },
        IntWidth.I32 => {
            return helper1(allocator, i32, low, high);
        },
        IntWidth.I64 => {
            return helper1(allocator, i64, low, high);
        },
        IntWidth.I128 => {
            return helper1(allocator, i128, low, high);
        },
        IntWidth.Usize => {
            return helper1(allocator, usize, low, high);
        },
    }
}

fn helper1(allocator: *Allocator, comptime T: type, low: Opaque, high: Opaque) RocList {
    const ptr1 = @ptrCast(*T, @alignCast(@alignOf(T), low));
    const ptr2 = @ptrCast(*T, @alignCast(@alignOf(T), high));

    return listRangeHelp(allocator, T, ptr1.*, ptr2.*);
}

fn listRangeHelp(allocator: *Allocator, comptime T: type, low: T, high: T) RocList {
    const Order = std.math.Order;

    switch (std.math.order(low, high)) {
        Order.gt => {
            return RocList.empty();
        },

        Order.eq => {
            const list = RocList.allocate(allocator, @alignOf(usize), 1, @sizeOf(T));
            const buffer = @ptrCast([*]T, @alignCast(@alignOf(T), list.bytes orelse unreachable));

            buffer[0] = low;

            return list;
        },

        Order.lt => {
            const length: usize = @intCast(usize, high - low);
            const list = RocList.allocate(allocator, @alignOf(usize), length, @sizeOf(T));

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

fn swap(source_ptr: [*]u8, element_width: usize, index_1: usize, index_2: usize) void {
    const temp: [*]u8 = @ptrCast([*]u8, std.heap.c_allocator.alloc(u8, element_width) catch unreachable);

    const element_at_i = source_ptr + (index_1 * element_width);
    const element_at_j = source_ptr + (index_2 * element_width);
    // Store I Element in temp
    @memcpy(temp, element_at_i, element_width);

    // Swap I Element with J Element
    @memcpy(element_at_i, element_at_j, element_width);

    // Swap J Element with temp
    @memcpy(element_at_j, temp, element_width);

    std.heap.c_allocator.free(temp[0..element_width]);
}

fn partition(source_ptr: [*]u8, transform: Opaque, wrapper: CompareFn, element_width: usize, low: isize, high: isize) isize {
    const pivot = source_ptr + (@intCast(usize, high) * element_width);
    var i = (low - 1); // Index of smaller element and indicates the right position of pivot found so far
    var j = low;

    while (j <= high - 1) : (j += 1) {
        const current_elem = source_ptr + (@intCast(usize, j) * element_width);

        const ordering = wrapper(transform, current_elem, pivot);
        const order = @intToEnum(utils.Ordering, ordering);

        // If current element is smaller than the pivot
        if (order == utils.Ordering.LT or order == utils.Ordering.EQ) {
            i += 1; // increment index of smaller element
            swap(source_ptr, element_width, @intCast(usize, i), @intCast(usize, j));
        }
    }
    swap(source_ptr, element_width, @intCast(usize, i + 1), @intCast(usize, high));
    return (i + 1);
}

fn quicksort(list: RocList, transform: Opaque, wrapper: CompareFn, element_width: usize, low: isize, high: isize) RocList {
    if (list.bytes) |source_ptr| {
        if (low < high) {
            const pi = partition(source_ptr, transform, wrapper, element_width, low, high);

            const _unused1 = quicksort(list, transform, wrapper, element_width, low, pi - 1); // before pi
            const _unused2 = quicksort(list, transform, wrapper, element_width, pi + 1, high); // after pi
        }
    }
    return list;
}

pub fn listSortWith(list: RocList, transform: Opaque, wrapper: CompareFn, alignment: usize, element_width: usize) callconv(.C) RocList {
    const low = 0;
    const high: isize = @intCast(isize, list.len()) - 1;
    return quicksort(list, transform, wrapper, element_width, low, high);
}
