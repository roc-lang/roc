//! TODO

const std = @import("std");
const builtins = @import("builtins");

const TestEnv = builtins.utils.TestEnv;
const RocList = builtins.list.RocList;
const RocStr = builtins.str.RocStr;
const listConcatUtf8 = builtins.list.listConcatUtf8;
const listConcat = builtins.list.listConcat;
const rcNone = builtins.utils.rcNone;

test "listConcat: non-unique with unique overlapping" {
    // TODO segfaulting ... :sad face:
    return error.SkipZigTest;
    // var test_env = TestEnv.init(std.testing.allocator);
    // defer test_env.deinit();

    // var nonUnique = RocList.fromSlice(u8, ([_]u8{1})[0..], false, test_env.getOps());
    // const bytes: [*]u8 = @as([*]u8, @ptrCast(nonUnique.bytes));
    // const ptr_width = @sizeOf(usize);
    // const refcount_ptr = @as([*]isize, @ptrCast(@as([*]align(ptr_width) u8, @alignCast(bytes)) - ptr_width));
    // builtins.utils.increfRcPtrC(&refcount_ptr[0], 1);
    // defer nonUnique.decref(@alignOf(u8), @sizeOf(u8), false, rcNone, test_env.getOps()); // listConcat will dec the other refcount

    // var unique = RocList.fromSlice(u8, ([_]u8{ 2, 3, 4 })[0..], false, test_env.getOps());
    // defer unique.decref(@alignOf(u8), @sizeOf(u8), false, rcNone, test_env.getOps());

    // var concatted = listConcat(nonUnique, unique, 1, 1, false, rcNone, rcNone, test_env.getOps());
    // defer concatted.decref(@alignOf(u8), @sizeOf(u8), false, rcNone, test_env.getOps());
    // var wanted = RocList.fromSlice(u8, ([_]u8{ 1, 2, 3, 4 })[0..], false, test_env.getOps());
    // defer wanted.decref(@alignOf(u8), @sizeOf(u8), false, rcNone, test_env.getOps());

    // try std.testing.expect(concatted.eql(wanted));
}

test "listConcatUtf8" {
    // TODO segfaulting ... :sad face:
    return error.SkipZigTest;
    // var test_env = TestEnv.init(std.testing.allocator);
    // defer test_env.deinit();

    // const list = RocList.fromSlice(u8, &[_]u8{ 1, 2, 3, 4 }, false, test_env.getOps());
    // defer list.decref(1, 1, false, &rcNone, test_env.getOps());
    // const string_bytes = "🐦";
    // const string = RocStr.init(string_bytes, string_bytes.len, test_env.getOps());
    // defer string.decref(test_env.getOps());
    // const ret = listConcatUtf8(list, string, test_env.getOps());
    // defer ret.decref(1, 1, false, &rcNone, test_env.getOps());
    // const expected = RocList.fromSlice(u8, &[_]u8{ 1, 2, 3, 4, 240, 159, 144, 166 }, false, test_env.getOps());
    // defer expected.decref(1, 1, false, &rcNone, test_env.getOps());
    // try std.testing.expect(ret.eql(expected));
}
