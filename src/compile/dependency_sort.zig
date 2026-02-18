//! Topological sorting for module dependencies using Kahn's algorithm.
//!
//! This module provides a generalized topological sort that can be used by both
//! the IPC path (roc run/build) and BuildEnv path (roc check) to ensure modules
//! are compiled in dependency order (dependencies first, dependents last).

const std = @import("std");
const Allocator = std.mem.Allocator;

/// Error returned when a circular dependency is detected.
pub const CyclicDependencyError = error{CyclicDependency};

/// Context for import extraction callback.
pub const ImportContext = struct {
    /// User-provided context pointer
    ctx: *anyopaque,
    /// Allocator for temporary allocations
    gpa: Allocator,
    /// Available module names (for filtering)
    available_modules: []const []const u8,
};

/// Callback type for extracting imports from a module.
/// Returns a slice of imported module names (caller owns memory).
pub const ImportExtractor = *const fn (context: ImportContext, module_name: []const u8) anyerror![][]const u8;

/// Sort modules by their import dependencies using Kahn's algorithm.
/// Returns modules in compilation order (dependencies first, dependents last).
///
/// Parameters:
///   gpa: Allocator for result and temporary allocations
///   module_names: List of module names to sort
///   module_dir: Directory containing the modules
///   extractor: Function to extract imports from a module
///   extractor_ctx: Context pointer passed to extractor
///
/// Returns: Sorted list of module names (caller owns memory)
/// Returns error.CyclicDependency if modules have circular imports.
pub fn sortByDependency(
    gpa: Allocator,
    module_names: []const []const u8,
    module_dir: []const u8,
    extractor: ImportExtractor,
    extractor_ctx: *anyopaque,
) ![][]const u8 {
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

    // Build adjacency list: adj[i] = list of modules that depend on module i
    // And compute in-degree: how many modules each module depends on
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

    // For each module, extract its imports and build the graph
    const context = ImportContext{
        .ctx = extractor_ctx,
        .gpa = gpa,
        .available_modules = module_names,
    };
    _ = module_dir; // Will be used when we integrate with file-based extraction

    for (module_names, 0..) |name, i| {
        const imports = try extractor(context, name);
        defer {
            for (imports) |imp| gpa.free(imp);
            gpa.free(imports);
        }

        // For each import, add an edge: this module depends on the imported module
        for (imports) |imp| {
            if (name_to_idx.get(imp)) |dep_idx| {
                // Module i imports module dep_idx, so dep_idx must come before i
                // Edge: dep_idx -> i (dep_idx is depended upon by i)
                try adjacency[dep_idx].append(gpa, i);
                in_degree[i] += 1;
            }
        }
    }

    // Kahn's algorithm: start with modules that have no dependencies (in_degree == 0)
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

        // For each module that depends on current, decrement its in-degree
        for (adjacency[current].items) |dependent| {
            in_degree[dependent] -= 1;
            if (in_degree[dependent] == 0) {
                try queue.append(gpa, dependent);
            }
        }
    }

    // If we didn't process all modules, there's a cycle
    if (result_count != n) {
        gpa.free(result);
        return error.CyclicDependency;
    }

    return result;
}

/// Simpler version that takes pre-computed imports for each module.
/// Useful when imports have already been extracted (e.g., during parsing phase).
pub fn sortByPrecomputedDependency(
    gpa: Allocator,
    module_names: []const []const u8,
    module_imports: []const []const []const u8,
) ![][]const u8 {
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
