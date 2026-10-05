//! Topological sorting for module dependencies using Kahn's algorithm.
//!
//! This module provides a generalized topological sort that can be used by both
//! the IPC path (default `roc` command/`roc build`) and BuildEnv path (`roc check`) to ensure modules
//! are compiled in dependency order (dependencies first, dependents last).

const std = @import("std");
const Allocator = std.mem.Allocator;

/// Context for import extraction callback.
pub const ImportContext = struct {
    /// User-provided context pointer
    ctx: *anyopaque,
    /// Allocator for temporary allocations
    gpa: Allocator,
    /// Available module names (for filtering)
    available_modules: []const []const u8,
};

/// Simpler version that takes pre-computed imports for each module.
/// Useful when imports have already been extracted (e.g., during parsing phase).
pub fn sortByPrecomputedDependency(
    gpa: Allocator,
    module_names: []const []const u8,
    module_imports: []const []const []const u8,
) (Allocator.Error || error{CyclicDependency})![][]const u8 {
    std.debug.assert(module_names.len == module_imports.len);

    const n = module_names.len;

    // Early return for trivial cases
    if (n <= 1) {
        var result = try gpa.alloc([]const u8, n);
        for (module_names, 0..) |name, i| {
            result[i] = name;
        }
        return result;
    }

    // Build a name -> index map for O(1) lookups
    var name_to_idx = std.StringHashMap(usize).init(gpa);
    defer name_to_idx.deinit();
    for (module_names, 0..) |name, i| {
        try name_to_idx.put(name, i);
    }

    // Build adjacency list and in-degree
    var adjacency = try gpa.alloc(std.ArrayList(usize), n);
    defer {
        for (adjacency) |*list| list.deinit(gpa);
        gpa.free(adjacency);
    }
    for (adjacency) |*list| {
        list.* = std.ArrayList(usize).empty;
    }

    var in_degree = try gpa.alloc(usize, n);
    defer gpa.free(in_degree);
    @memset(in_degree, 0);

    // Build graph from pre-computed imports
    for (module_imports, 0..) |imports, i| {
        for (imports) |imp| {
            if (name_to_idx.get(imp)) |dep_idx| {
                try adjacency[dep_idx].append(gpa, i);
                in_degree[i] += 1;
            }
        }
    }

    // Kahn's algorithm
    var queue = std.ArrayList(usize).empty;
    defer queue.deinit(gpa);

    for (0..n) |i| {
        if (in_degree[i] == 0) {
            try queue.append(gpa, i);
        }
    }

    const result = try gpa.alloc([]const u8, n);
    var result_count: usize = 0;

    while (queue.items.len > 0) {
        const current = queue.orderedRemove(0);
        result[result_count] = module_names[current];
        result_count += 1;

        for (adjacency[current].items) |dependent| {
            in_degree[dependent] -= 1;
            if (in_degree[dependent] == 0) {
                try queue.append(gpa, dependent);
            }
        }
    }

    if (result_count != n) {
        gpa.free(result);
        return error.CyclicDependency;
    }

    return result;
}

test "sortByPrecomputedDependency - no dependencies" {
    const allocator = std.testing.allocator;
    const names = &[_][]const u8{ "A", "B", "C" };
    const imports = &[_][]const []const u8{
        &[_][]const u8{},
        &[_][]const u8{},
        &[_][]const u8{},
    };

    const result = try sortByPrecomputedDependency(allocator, names, imports);
    defer allocator.free(result);

    try std.testing.expectEqual(@as(usize, 3), result.len);
}

test "sortByPrecomputedDependency - linear chain" {
    const allocator = std.testing.allocator;
    // C -> B -> A (C depends on B, B depends on A)
    const names = &[_][]const u8{ "A", "B", "C" };
    const imports = &[_][]const []const u8{
        &[_][]const u8{}, // A has no deps
        &[_][]const u8{"A"}, // B depends on A
        &[_][]const u8{"B"}, // C depends on B
    };

    const result = try sortByPrecomputedDependency(allocator, names, imports);
    defer allocator.free(result);

    // A must come first, then B, then C
    try std.testing.expectEqual(@as(usize, 3), result.len);
    try std.testing.expectEqualStrings("A", result[0]);
    try std.testing.expectEqualStrings("B", result[1]);
    try std.testing.expectEqualStrings("C", result[2]);
}

test "sortByPrecomputedDependency - diamond" {
    const allocator = std.testing.allocator;
    // D depends on B and C, B and C both depend on A
    //     A
    //    / \
    //   B   C
    //    \ /
    //     D
    const names = &[_][]const u8{ "A", "B", "C", "D" };
    const imports = &[_][]const []const u8{
        &[_][]const u8{}, // A
        &[_][]const u8{"A"}, // B -> A
        &[_][]const u8{"A"}, // C -> A
        &[_][]const u8{ "B", "C" }, // D -> B, C
    };

    const result = try sortByPrecomputedDependency(allocator, names, imports);
    defer allocator.free(result);

    // A must come first, D must come last
    try std.testing.expectEqual(@as(usize, 4), result.len);
    try std.testing.expectEqualStrings("A", result[0]);
    try std.testing.expectEqualStrings("D", result[3]);
    // B and C can be in either order (both at index 1 or 2)
}

test "sortByPrecomputedDependency - cycle detection" {
    const allocator = std.testing.allocator;
    // A -> B -> A (cycle)
    const names = &[_][]const u8{ "A", "B" };
    const imports = &[_][]const []const u8{
        &[_][]const u8{"B"}, // A -> B
        &[_][]const u8{"A"}, // B -> A
    };

    const result = sortByPrecomputedDependency(allocator, names, imports);
    try std.testing.expectError(error.CyclicDependency, result);
}
