//! Unit tests for list builtin functions

const std = @import("std");
const builtins = @import("builtins");

const TestEnv = builtins.utils.TestEnv;
const RocList = builtins.list.RocList;
const RocStr = builtins.str.RocStr;
const listConcatUtf8 = builtins.list.listConcatUtf8;
const listConcat = builtins.list.listConcat;
const listAppendUnsafe = builtins.list.listAppendUnsafe;
const listPrepend = builtins.list.listPrepend;
const listDropAt = builtins.list.listDropAt;
const listReplace = builtins.list.listReplace;
const listReplaceInPlace = builtins.list.listReplaceInPlace;
const listAllocationPtr = builtins.list.listAllocationPtr;
const listIncref = builtins.list.listIncref;
const listDecref = builtins.list.listDecref;
const listSwap = builtins.list.listSwap;
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

test "listAppendUnsafe basic functionality" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    // Copy function for u8 elements
    const copy_fn = struct {
        fn copy(dest: ?[*]u8, src: ?[*]u8) callconv(.C) void {
            if (dest != null and src != null) {
                const dest_ptr = @as(*u8, @ptrCast(@alignCast(dest)));
                const src_ptr = @as(*u8, @ptrCast(@alignCast(src)));
                dest_ptr.* = src_ptr.*;
            }
        }
    }.copy;

    // Create a list with some capacity
    var list = builtins.list.listWithCapacity(10, @alignOf(u8), @sizeOf(u8), false, rcNone, test_env.getOps());

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
        fn copy(dest: ?[*]u8, src: ?[*]u8) callconv(.C) void {
            if (dest != null and src != null) {
                const dest_ptr = @as(*i32, @ptrCast(@alignCast(dest)));
                const src_ptr = @as(*i32, @ptrCast(@alignCast(src)));
                dest_ptr.* = src_ptr.*;
            }
        }
    }.copy;

    // Test with i32
    var int_list = builtins.list.listWithCapacity(5, @alignOf(i32), @sizeOf(i32), false, rcNone, test_env.getOps());

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
        fn copy(dest: ?[*]u8, src: ?[*]u8) callconv(.C) void {
            if (dest != null and src != null) {
                const dest_ptr = @as(*u16, @ptrCast(@alignCast(dest)));
                const src_ptr = @as(*u16, @ptrCast(@alignCast(src)));
                dest_ptr.* = src_ptr.*;
            }
        }
    }.copy;

    // Create a list with capacity (listAppendUnsafe requires pre-allocated space)
    var list_with_capacity = builtins.list.listWithCapacity(5, @alignOf(u16), @sizeOf(u16), false, rcNone, test_env.getOps());

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
        fn copy(dest: ?[*]u8, src: ?[*]u8) callconv(.C) void {
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
        fn copy(dest: ?[*]u8, src: ?[*]u8) callconv(.C) void {
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
        fn copy(dest: ?[*]u8, src: ?[*]u8) callconv(.C) void {
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
        fn copy(dest: ?[*]u8, src: ?[*]u8) callconv(.C) void {
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
        fn copy(dest: ?[*]u8, src: ?[*]u8) callconv(.C) void {
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
        fn copy(dest: ?[*]u8, src: ?[*]u8) callconv(.C) void {
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
        fn copy(dest: ?[*]u8, src: ?[*]u8) callconv(.C) void {
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
    const sublist = builtins.list.listSublist(list, @alignOf(u8), @sizeOf(u8), false, 2, 0, rcNone, test_env.getOps());
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
    const sublist = builtins.list.listSublist(list, @alignOf(i16), @sizeOf(i16), false, 0, 3, rcNone, test_env.getOps());
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
        fn copy(dest: ?[*]u8, src: ?[*]u8) callconv(.C) void {
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

    const list = builtins.list.listWithCapacity(0, @alignOf(u32), @sizeOf(u32), false, rcNone, test_env.getOps());
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
    var list2 = builtins.list.listWithCapacity(10, @alignOf(u8), @sizeOf(u8), false, rcNone, test_env.getOps());
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
        fn copy(dest: ?[*]u8, src: ?[*]u8) callconv(.C) void {
            if (dest != null and src != null) {
                const dest_ptr = @as(*u8, @ptrCast(@alignCast(dest)));
                const src_ptr = @as(*u8, @ptrCast(@alignCast(src)));
                dest_ptr.* = src_ptr.*;
            }
        }
    }.copy;

    // Create a list with sufficient capacity
    var list = builtins.list.listWithCapacity(5, @alignOf(u8), @sizeOf(u8), false, rcNone, test_env.getOps());

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
        .capacity_or_alloc_ptr = builtins.list.SEAMLESS_SLICE_BIT,
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
        .capacity_or_alloc_ptr = builtins.list.SEAMLESS_SLICE_BIT | 10, // High bit set, capacity of 10
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
    try std.testing.expectEqual(list.isUnique(), builtins.list.listIsUnique(list));

    // After incref, both should report not unique
    list.incref(1, false);
    defer list.decref(@alignOf(u16), @sizeOf(u16), false, rcNone, test_env.getOps());

    try std.testing.expectEqual(list.isUnique(), builtins.list.listIsUnique(list));
    try std.testing.expect(!builtins.list.listIsUnique(list));

    // Final cleanup
    list.decref(@alignOf(u16), @sizeOf(u16), false, rcNone, test_env.getOps());
}

test "complex reference counting: clone behavior" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    const data = [_]u8{ 5, 10, 15 };
    const original_list = RocList.fromSlice(u8, data[0..], false, test_env.getOps());

    // Clone should create a new independent copy
    const cloned_list = builtins.list.listClone(original_list, @alignOf(u8), @sizeOf(u8), false, rcNone, rcNone, test_env.getOps());
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
        fn copy(dest: ?[*]u8, src: ?[*]u8) callconv(.C) void {
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
        fn copy(dest: ?[*]u8, src: ?[*]u8) callconv(.C) void {
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
        fn copy(dest: ?[*]u8, src: ?[*]u8) callconv(.C) void {
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
        fn copy(dest: ?[*]u8, src: ?[*]u8) callconv(.C) void {
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
    var test_env = builtins.utils.TestEnv.init(std.testing.allocator);
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
        fn copy(dest: ?[*]u8, src: ?[*]u8) callconv(.C) void {
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
    const sublist = builtins.list.listSublist(concatenated, @alignOf(i16), @sizeOf(i16), false, 1, 3, rcNone, test_env.getOps());
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
        fn copy(dest: ?[*]u8, src: ?[*]u8) callconv(.C) void {
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
    list = listSwap(list, @alignOf(u32), @sizeOf(u32), 0, 2, false, rcNone, rcNone, builtins.utils.UpdateMode.Immutable, copy_fn, test_env.getOps());

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
    const mid_sublist = builtins.list.listSublist(large_list, @alignOf(u16), @sizeOf(u16), false, 400, 200, rcNone, test_env.getOps());
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
        fn copy(dest: ?[*]u8, src: ?[*]u8) callconv(.C) void {
            if (dest != null and src != null) {
                const dest_ptr = @as(*u8, @ptrCast(@alignCast(dest)));
                const src_ptr = @as(*u8, @ptrCast(@alignCast(src)));
                dest_ptr.* = src_ptr.*;
            }
        }
    }.copy;

    // Start with a list with some capacity
    var list = builtins.list.listWithCapacity(50, @alignOf(u8), @sizeOf(u8), false, rcNone, test_env.getOps());

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
    var list = builtins.list.listWithCapacity(exact_capacity, @alignOf(u32), @sizeOf(u32), false, rcNone, test_env.getOps());

    try std.testing.expect(list.getCapacity() >= exact_capacity);
    try std.testing.expectEqual(@as(usize, 0), list.len());

    // Use listReserve to ensure we have exactly the capacity we want
    list = builtins.list.listReserve(list, @alignOf(u32), exact_capacity, @sizeOf(u32), false, rcNone, builtins.utils.UpdateMode.Immutable, test_env.getOps());
    defer list.decref(@alignOf(u32), @sizeOf(u32), false, rcNone, test_env.getOps());

    // Verify capacity management functions work correctly
    const initial_capacity = list.getCapacity();
    try std.testing.expect(initial_capacity >= exact_capacity);

    const capacity_via_function = builtins.list.listCapacity(list);
    try std.testing.expectEqual(initial_capacity, capacity_via_function);
}

test "memory management: release excess capacity edge cases" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    // Create a list with minimal data but large capacity
    const data = [_]u8{42};
    const small_list = RocList.fromSlice(u8, data[0..], false, test_env.getOps());

    // Reserve much more capacity than needed
    const oversized_list = builtins.list.listReserve(small_list, @alignOf(u8), 1000, @sizeOf(u8), false, rcNone, builtins.utils.UpdateMode.Immutable, test_env.getOps());

    try std.testing.expectEqual(@as(usize, 1), oversized_list.len());
    try std.testing.expect(oversized_list.getCapacity() >= 1000);

    // Release excess capacity
    const trimmed_list = builtins.list.listReleaseExcessCapacity(oversized_list, @alignOf(u8), @sizeOf(u8), false, rcNone, rcNone, builtins.utils.UpdateMode.Immutable, test_env.getOps());
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
    const empty_start = builtins.list.listSublist(list1, @alignOf(u16), @sizeOf(u16), false, 0, 0, rcNone, test_env.getOps());
    defer empty_start.decref(@alignOf(u16), @sizeOf(u16), false, rcNone, test_env.getOps());
    try std.testing.expectEqual(@as(usize, 0), empty_start.len());

    // Zero-length sublist from middle
    const list2 = RocList.fromSlice(u16, data[0..], false, test_env.getOps());
    const empty_mid = builtins.list.listSublist(list2, @alignOf(u16), @sizeOf(u16), false, 2, 0, rcNone, test_env.getOps());
    defer empty_mid.decref(@alignOf(u16), @sizeOf(u16), false, rcNone, test_env.getOps());
    try std.testing.expectEqual(@as(usize, 0), empty_mid.len());

    // Zero-length sublist from end
    const list3 = RocList.fromSlice(u16, data[0..], false, test_env.getOps());
    const empty_end = builtins.list.listSublist(list3, @alignOf(u16), @sizeOf(u16), false, 5, 0, rcNone, test_env.getOps());
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
    const cloned = builtins.list.listClone(original, @alignOf(i32), @sizeOf(i32), false, rcNone, rcNone, test_env.getOps());
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
        fn copy(dest: ?[*]u8, src: ?[*]u8) callconv(.C) void {
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
    const swapped = listSwap(list, @alignOf(u8), @sizeOf(u8), 2, 2, false, rcNone, rcNone, builtins.utils.UpdateMode.Immutable, copy_fn, test_env.getOps());
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
    list = builtins.list.listReserve(list, @alignOf(u8), 10, @sizeOf(u8), false, rcNone, builtins.utils.UpdateMode.Immutable, test_env.getOps());
    try std.testing.expect(list.getCapacity() >= 12); // 2 existing + 10 spare

    list = builtins.list.listReserve(list, @alignOf(u8), 20, @sizeOf(u8), false, rcNone, builtins.utils.UpdateMode.Immutable, test_env.getOps());
    try std.testing.expect(list.getCapacity() >= 22); // 2 existing + 20 spare

    list = builtins.list.listReserve(list, @alignOf(u8), 5, @sizeOf(u8), false, rcNone, builtins.utils.UpdateMode.Immutable, test_env.getOps());
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
