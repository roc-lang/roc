//! TODO

const std = @import("std");
const builtins = @import("builtins");

test "increfC, refcounted data" {
    var mock_rc: isize = 17;
    const ptr_to_refcount: *isize = &mock_rc;
    builtins.utils.increfRcPtrC(ptr_to_refcount, 2);
    try std.testing.expectEqual(mock_rc, 19);
}

test "increfC, static data" {
    var mock_rc: isize = builtins.utils.REFCOUNT_MAX_ISIZE;
    const ptr_to_refcount: *isize = &mock_rc;
    builtins.utils.increfRcPtrC(ptr_to_refcount, 2);
    try std.testing.expectEqual(mock_rc, builtins.utils.REFCOUNT_MAX_ISIZE);
}
