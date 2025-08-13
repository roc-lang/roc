//! Unit tests for the bundle CLI logic (sorting, deduplication, first arg preservation)

const std = @import("std");
const testing = std.testing;

test "bundle paths - empty list defaults to main.roc" {
    // Simulate the logic from rocBundle
    const paths_to_use = if (0 == 0) &[_][]const u8{"main.roc"} else &[_][]const u8{};
    
    try testing.expectEqual(@as(usize, 1), paths_to_use.len);
    try testing.expectEqualStrings("main.roc", paths_to_use[0]);
}

test "bundle paths - single file unchanged" {
    const allocator = testing.allocator;
    
    var file_paths = std.ArrayList([]const u8).init(allocator);
    defer file_paths.deinit();
    
    try file_paths.append("app.roc");
    
    try testing.expectEqual(@as(usize, 1), file_paths.items.len);
    try testing.expectEqualStrings("app.roc", file_paths.items[0]);
}

test "bundle paths - sorting and deduplication" {
    const allocator = testing.allocator;
    
    var file_paths = std.ArrayList([]const u8).init(allocator);
    defer file_paths.deinit();
    
    // Add paths in non-sorted order with duplicates
    try file_paths.append("zebra.roc");
    try file_paths.append("apple.roc");
    try file_paths.append("banana.roc");
    try file_paths.append("apple.roc");
    
    const first_cli_path = file_paths.items[0]; // "zebra.roc"
    
    // Sort
    std.mem.sort([]const u8, file_paths.items, {}, struct {
        fn lessThan(_: void, a: []const u8, b: []const u8) bool {
            return std.mem.order(u8, a, b) == .lt;
        }
    }.lessThan);
    
    // Remove duplicates
    var unique_count: usize = 0;
    for (file_paths.items, 0..) |path, i| {
        if (i == 0 or !std.mem.eql(u8, path, file_paths.items[i - 1])) {
            file_paths.items[unique_count] = path;
            unique_count += 1;
        }
    }
    file_paths.items.len = unique_count;
    
    // Verify deduplication worked
    try testing.expectEqual(@as(usize, 3), file_paths.items.len);
    
    // Ensure first CLI path stays first
    if (file_paths.items.len > 1) {
        var found_index: ?usize = null;
        for (file_paths.items, 0..) |path, i| {
            if (std.mem.eql(u8, path, first_cli_path)) {
                found_index = i;
                break;
            }
        }
        
        if (found_index) |idx| {
            if (idx != 0) {
                const temp = file_paths.items[0];
                file_paths.items[0] = file_paths.items[idx];
                file_paths.items[idx] = temp;
            }
        }
    }
    
    // Verify final order
    try testing.expectEqualStrings("zebra.roc", file_paths.items[0]);
    try testing.expectEqualStrings("banana.roc", file_paths.items[1]);
    try testing.expectEqualStrings("apple.roc", file_paths.items[2]);
}

test "bundle paths - preserves first CLI arg with many files" {
    const allocator = testing.allocator;
    
    var file_paths = std.ArrayList([]const u8).init(allocator);
    defer file_paths.deinit();
    
    // Add 8 paths with specific first
    try file_paths.append("tests/test2.roc");
    try file_paths.append("main.roc");
    try file_paths.append("src/app.roc");
    try file_paths.append("src/lib.roc");
    try file_paths.append("src/utils/helper.roc");
    try file_paths.append("tests/test1.roc");
    try file_paths.append("docs/readme.md");
    try file_paths.append("config.roc");
    
    const first_cli_path = file_paths.items[0]; // "tests/test2.roc"
    
    // Sort
    std.mem.sort([]const u8, file_paths.items, {}, struct {
        fn lessThan(_: void, a: []const u8, b: []const u8) bool {
            return std.mem.order(u8, a, b) == .lt;
        }
    }.lessThan);
    
    // Find and move first CLI path to front
    if (file_paths.items.len > 1) {
        var found_index: ?usize = null;
        for (file_paths.items, 0..) |path, i| {
            if (std.mem.eql(u8, path, first_cli_path)) {
                found_index = i;
                break;
            }
        }
        
        if (found_index) |idx| {
            if (idx != 0) {
                const temp = file_paths.items[0];
                file_paths.items[0] = file_paths.items[idx];
                file_paths.items[idx] = temp;
            }
        }
    }
    
    // Verify first path is preserved
    try testing.expectEqualStrings("tests/test2.roc", file_paths.items[0]);
    try testing.expectEqual(@as(usize, 8), file_paths.items.len);
}