const std = @import("std");
const utils = @import("utils.zig");

pub const RocList = extern struct {
    bytes: ?[*]u8,
    length: usize,

    pub fn len(self: RocDict) usize {
        return self.length;
    }

    pub fn isEmpty(self: RocDict) bool {
        return self.len() == 0;
    }

    pub fn empty() RocList {
        return RocList{ .bytes = null, .length = 0 };
    }

    pub fn isUnique(self: RocDict) bool {
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

    pub fn makeUnique(self: RocDict, allocator: *Allocator, alignment: Alignment, key_width: usize, value_width: usize) RocDict {
        if (self.isEmpty()) {
            return self;
        }

        if (self.isUnique()) {
            return self;
        }

        // unfortunately, we have to clone
        var new_dict = RocDict.allocate(allocator, self.number_of_levels, self.dict_entries_len, alignment, key_width, value_width);

        var old_bytes: [*]u8 = @ptrCast([*]u8, self.dict_bytes);
        var new_bytes: [*]u8 = @ptrCast([*]u8, new_dict.dict_bytes);

        const number_of_bytes = self.capacity() * (@sizeOf(Slot) + key_width + value_width);
        @memcpy(new_bytes, old_bytes, number_of_bytes);

        // NOTE we fuse an increment of all keys/values with a decrement of the input dict
        const data_bytes = self.capacity() * slotSize(key_width, value_width);
        decref(allocator, alignment, self.dict_bytes, data_bytes);

        return new_dict;
    }

    pub fn reallocate(
        self: RocDict,
        allocator: *Allocator,
        alignment: Alignment,
        new_length: usize,
        element_width: usize,
    ) RocDict {
        const old_length = self.length;
        const delta_length = new_length - old_length;

        const data_bytes = new_capacity * slot_size;
        const first_slot = allocateWithRefcount(allocator, alignment, data_bytes);

        // transfer the memory

        if (self.bytes) |source_ptr| {
            const dest_ptr = first_slot;

            @memcpy(dest_ptr, source_ptr, old_length);
        }

        // NOTE the newly added elements are left uninitialized

        const result = RocList{
            .dict_bytes = first_slot,
            .length = new_length,
        };

        // NOTE we fuse an increment of all keys/values with a decrement of the input dict
        utils.decref(allocator, alignment, self.bytes, old_length * element_width);

        return result;
    }
};
