//! Unit tests for list builtin functions

const std = @import("std");
const builtins = @import("builtins");

const TestEnv = builtins.utils.TestEnv;
const RocList = builtins.list.RocList;
const RocStr = builtins.str.RocStr;
const listConcatUtf8 = builtins.list.listConcatUtf8;
const listConcat = builtins.list.listConcat;
const rcNone = builtins.utils.rcNone;

test "listConcat: non-unique with unique overlapping" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    const nonUnique = RocList.fromSlice(u8, ([_]u8{1})[0..], false, test_env.getOps());
    const bytes: [*]u8 = @as([*]u8, @ptrCast(nonUnique.bytes));
    const ptr_width = @sizeOf(usize);
    const refcount_ptr = @as([*]isize, @ptrCast(@as([*]align(ptr_width) u8, @alignCast(bytes)) - ptr_width));
    builtins.utils.increfRcPtrC(&refcount_ptr[0], 1);
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
    const cloned = builtins.list.listClone(list, @alignOf(i32), @sizeOf(i32), false, rcNone, rcNone, test_env.getOps());
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
    const list = builtins.list.listWithCapacity(capacity, @alignOf(i32), @sizeOf(i32), false, rcNone, test_env.getOps());
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

    const reserved_list = builtins.list.listReserve(list, @alignOf(u8), 20, @sizeOf(u8), false, rcNone, builtins.utils.UpdateMode.Immutable, test_env.getOps());
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

    const capacity = builtins.list.listCapacity(list);
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
    const list_with_excess = builtins.list.listReserve(list_with_data, @alignOf(u8), 100, @sizeOf(u8), false, rcNone, builtins.utils.UpdateMode.Immutable, test_env.getOps());

    // Verify it has excess capacity
    try std.testing.expect(list_with_excess.getCapacity() >= 100);
    try std.testing.expectEqual(@as(usize, 3), list_with_excess.len());

    // Release the excess capacity
    const released_list = builtins.list.listReleaseExcessCapacity(list_with_excess, @alignOf(u8), @sizeOf(u8), false, rcNone, rcNone, builtins.utils.UpdateMode.Immutable, test_env.getOps());
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
    const sublist = builtins.list.listSublist(list, @alignOf(u8), @sizeOf(u8), false, 2, 4, rcNone, test_env.getOps());
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
    const empty_sublist = builtins.list.listSublist(list, @alignOf(i32), @sizeOf(i32), false, 1, 0, rcNone, test_env.getOps());
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
        fn copy(dest: ?[*]u8, src: ?[*]u8) callconv(.C) void {
            if (dest != null and src != null) {
                const dest_ptr = @as(*u16, @ptrCast(@alignCast(dest)));
                const src_ptr = @as(*u16, @ptrCast(@alignCast(src)));
                dest_ptr.* = src_ptr.*;
            }
        }
    }.copy;

    const swapped_list = builtins.list.listSwap(list, @alignOf(u16), @sizeOf(u16), 1, 3, false, rcNone, rcNone, builtins.utils.UpdateMode.Immutable, copy_fn, test_env.getOps());
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
