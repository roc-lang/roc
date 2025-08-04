const std = @import("std");
const base = @import("base");
const Ident = base.Ident;

// Test 1: nominal_import_wildcard.md regression
// The diff shows "red" corrupted to "aed"
test "red identifier should not be corrupted" {
    const testing = std.testing;

    const idx_opt = Ident.Idx.try_inline("red");
    try testing.expect(idx_opt != null);

    const idx = idx_opt.?;
    const inner = idx.toInner();
    try testing.expect(inner.is_small);

    // Create a copy to avoid alignment issues with packed struct
    const small_copy = inner.data.small;
    var buffer: [7]u8 = undefined;
    const result = small_copy.writeTextToBuffer(&buffer);

    // This should be "red", not "aed"
    try testing.expectEqualStrings("red", result);
}

// Test 2: underscore_in_regular_annotations.md regression
// The diff shows "_field2" corrupted to "_field"
test "_field2 identifier should not be corrupted" {
    const testing = std.testing;
    const gpa = testing.allocator;

    var store = try Ident.Store.initCapacity(gpa, 10);
    defer store.deinit(gpa);

    const ident = Ident.for_text("_field2");
    const idx = try store.insert(gpa, ident);
    const result = store.getText(idx);

    // This should be "_field2", not "_field"
    try testing.expectEqualStrings("_field2", result);
}

// Test 3: plume_package/Color.md regression
// The diff shows "U8" corrupted to "a8"
test "U8 identifier should not be corrupted" {
    const testing = std.testing;

    const idx_opt = Ident.Idx.try_inline("U8");
    try testing.expect(idx_opt != null);

    const idx = idx_opt.?;
    const inner = idx.toInner();
    try testing.expect(inner.is_small);

    // Create a copy to avoid alignment issues with packed struct
    const small_copy = inner.data.small;
    var buffer: [7]u8 = undefined;
    const result = small_copy.writeTextToBuffer(&buffer);

    // This should be "U8", not "a8"
    try testing.expectEqualStrings("U8", result);
}

// Test 4: test_exact_pattern_crash.md regression
// The diff shows "_e" corrupted to "_a"
test "_e identifier should not be corrupted" {
    const testing = std.testing;

    const idx_opt = Ident.Idx.try_inline("_e");
    try testing.expect(idx_opt != null);

    const idx = idx_opt.?;
    const inner = idx.toInner();
    try testing.expect(inner.is_small);

    // Create a copy to avoid alignment issues with packed struct
    const small_copy = inner.data.small;
    var buffer: [7]u8 = undefined;
    const result = small_copy.writeTextToBuffer(&buffer);

    // This should be "_e", not "_a"
    try testing.expectEqualStrings("_e", result);
}
