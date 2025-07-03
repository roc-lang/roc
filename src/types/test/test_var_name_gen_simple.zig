const std = @import("std");
const expect = std.testing.expect;
const expectEqual = std.testing.expectEqual;
const expectEqualStrings = std.testing.expectEqualStrings;
const var_name_gen = @import("../../types/var_name_gen.zig");

const TypeVarNameGenerator = var_name_gen.TypeVarNameGenerator;

test "generateName single letters" {
    const allocator = std.testing.allocator;

    var gen = try TypeVarNameGenerator.init(allocator);
    defer gen.deinit();

    // First call should return "a"
    const name1 = try gen.generateName();
    try expectEqualStrings("a", name1);

    // Second call should return "b"
    const name2 = try gen.generateName();
    try expectEqualStrings("b", name2);

    // Mark "c" as taken
    try gen.taken_names.put("c", {});

    // Third call should skip "c" and return "d"
    const name3 = try gen.generateName();
    defer allocator.free(name3);
    try expectEqualStrings("d", name3);
}

test "generateName with suffixes" {
    const allocator = std.testing.allocator;

    var gen = try TypeVarNameGenerator.init(allocator);
    defer gen.deinit();

    // Mark all single letters as taken
    var letter: u8 = 'a';
    while (letter <= 'z') : (letter += 1) {
        var buf: [1]u8 = .{letter};
        try gen.taken_names.put(&buf, {});
    }

    // Next call should return "a2"
    const name1 = try gen.generateName();
    defer allocator.free(name1);
    try expectEqualStrings("a2", name1);

    // Next call should return "b2"
    const name2 = try gen.generateName();
    defer allocator.free(name2);
    try expectEqualStrings("b2", name2);
}

test "generateName skips taken names with suffixes" {
    const allocator = std.testing.allocator;

    var gen = try TypeVarNameGenerator.init(allocator);
    defer gen.deinit();

    // Mark all single letters and "a2" as taken
    var letter: u8 = 'a';
    while (letter <= 'z') : (letter += 1) {
        var buf: [1]u8 = .{letter};
        try gen.taken_names.put(&buf, {});
    }
    try gen.taken_names.put("a2", {});

    // First call should skip "a2" and return "b2"
    const name1 = try gen.generateName();
    defer allocator.free(name1);
    try expectEqualStrings("b2", name1);
}

test "taken_names tracking" {
    const allocator = std.testing.allocator;

    var gen = try TypeVarNameGenerator.init(allocator);
    defer gen.deinit();

    // Initially no names are taken
    try expect(!gen.taken_names.contains("a"));
    try expect(!gen.taken_names.contains("b"));

    // Generate a name
    _ = try gen.generateName();

    // Now "a" should be taken
    try expect(gen.taken_names.contains("a"));
    try expect(!gen.taken_names.contains("b"));

    // Generate another name
    const name2 = try gen.generateName();
    defer allocator.free(name2);

    // Now both "a" and "b" should be taken
    try expect(gen.taken_names.contains("a"));
    try expect(gen.taken_names.contains("b"));
}

test "respects pre-existing names" {
    const allocator = std.testing.allocator;

    var gen = try TypeVarNameGenerator.init(allocator);
    defer gen.deinit();

    // Pre-mark some names as taken
    try gen.taken_names.put("a", {});
    try gen.taken_names.put("c", {});
    try gen.taken_names.put("e", {});

    // Should get "b"
    const name1 = try gen.generateName();
    try expectEqualStrings("b", name1);

    // Should get "d"
    const name2 = try gen.generateName();
    try expectEqualStrings("d", name2);

    // Should get "f"
    const name3 = try gen.generateName();
    try expectEqualStrings("f", name3);
}

test "suffix progression" {
    const allocator = std.testing.allocator;

    var gen = try TypeVarNameGenerator.init(allocator);
    defer gen.deinit();

    // Mark all single letters and all "2" suffix names as taken
    var letter: u8 = 'a';
    while (letter <= 'z') : (letter += 1) {
        var buf: [1]u8 = .{letter};
        try gen.taken_names.put(&buf, {});

        var buf2: [16]u8 = undefined;
        const name2 = try std.fmt.bufPrint(&buf2, "{c}2", .{letter});
        const allocated_name2 = try allocator.dupe(u8, name2);
        defer allocator.free(allocated_name2);
        try gen.taken_names.put(allocated_name2, {});
    }

    // Should get "a3" since all single letters and "2" suffixes are taken
    const name = try gen.generateName();
    try expectEqualStrings("a3", name);

    // Should get "b3"
    const name2 = try gen.generateName();
    try expectEqualStrings("b3", name2);
}
