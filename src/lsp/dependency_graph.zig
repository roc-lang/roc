//! Dependency graph for tracking module relationships in the LSP.
//!
//! This module provides a graph structure that tracks:
//! - Which modules depend on which other modules (imports)
//! - Which modules are affected when a module changes (dependents)
//! - Content hashes for change detection
//!
//! The graph is built from successful builds and used for:
//! - Determining which modules need rebuilding when a file changes
//! - Efficient incremental invalidation

const std = @import("std");
const compile = @import("compile");
const Allocator = std.mem.Allocator;

/// A node in the dependency graph representing a single module.
pub const ModuleNode = struct {
    /// Absolute file path of the module
    path: []const u8,
    /// Module name (e.g., "Str", "List", "MyModule")
    name: []const u8,
    /// Blake3 hash of the module's source content
    content_hash: [32]u8,
    /// Blake3 hash of the module's exported symbols (for dependency-aware invalidation)
    /// When exports change, dependents need to be rebuilt. When only implementation
    /// changes (same exports), dependents can keep their cache.
    exports_hash: [32]u8,
    /// Modules that this module imports (dependencies)
    imports: std.ArrayList([]const u8),
    /// Modules that import this module (reverse dependencies)
    dependents: std.ArrayList([]const u8),
    /// Build depth from root (lower = closer to root)
    depth: u32,

    pub fn init(allocator: Allocator, path: []const u8, name: []const u8) Allocator.Error!ModuleNode {
        return .{
            .path = try allocator.dupe(u8, path),
            .name = try allocator.dupe(u8, name),
            .content_hash = std.mem.zeroes([32]u8),
            .exports_hash = std.mem.zeroes([32]u8),
            .imports = .empty,
            .dependents = .empty,
            .depth = std.math.maxInt(u32),
        };
    }

    pub fn deinit(self: *ModuleNode, allocator: Allocator) void {
        allocator.free(self.path);
        allocator.free(self.name);
        for (self.imports.items) |imp| {
            allocator.free(imp);
        }
        self.imports.deinit(allocator);
        for (self.dependents.items) |dep| {
            allocator.free(dep);
        }
        self.dependents.deinit(allocator);
    }

    pub fn addImport(self: *ModuleNode, allocator: Allocator, import_path: []const u8) Allocator.Error!void {
        try self.imports.append(allocator, try allocator.dupe(u8, import_path));
    }

    pub fn addDependent(self: *ModuleNode, allocator: Allocator, dependent_path: []const u8) Allocator.Error!void {
        try self.dependents.append(allocator, try allocator.dupe(u8, dependent_path));
    }
};

/// Dependency graph tracking relationships between modules.
pub const DependencyGraph = struct {
    allocator: Allocator,
    /// Maps absolute file path to ModuleNode
    modules: std.StringHashMap(ModuleNode),

    pub fn init(allocator: Allocator) DependencyGraph {
        return .{
            .allocator = allocator,
            .modules = std.StringHashMap(ModuleNode).init(allocator),
        };
    }

    pub fn deinit(self: *DependencyGraph) void {
        var it = self.modules.iterator();
        while (it.next()) |entry| {
            var node = entry.value_ptr;
            node.deinit(self.allocator);
        }
        self.modules.deinit();
    }

    /// Clear the graph completely (e.g., before rebuilding from scratch)
    pub fn clear(self: *DependencyGraph) void {
        var it = self.modules.iterator();
        while (it.next()) |entry| {
            var node = entry.value_ptr;
            node.deinit(self.allocator);
        }
        self.modules.clearRetainingCapacity();
    }

    /// Clear only the dependency relationships (imports/dependents) while preserving
    /// content and exports hashes for incremental change detection.
    pub fn clearRelationships(self: *DependencyGraph) void {
        var it = self.modules.iterator();
        while (it.next()) |entry| {
            var node = entry.value_ptr;
            // Clear imports
            for (node.imports.items) |imp| {
                self.allocator.free(imp);
            }
            node.imports.clearRetainingCapacity();
            // Clear dependents
            for (node.dependents.items) |dep| {
                self.allocator.free(dep);
            }
            node.dependents.clearRetainingCapacity();
            // Reset depth but preserve hashes
            node.depth = std.math.maxInt(u32);
        }
    }

    /// Get or create a module node for the given path
    pub fn getOrCreateModule(self: *DependencyGraph, path: []const u8, name: []const u8) Allocator.Error!*ModuleNode {
        const gop = try self.modules.getOrPut(path);
        if (!gop.found_existing) {
            gop.value_ptr.* = try ModuleNode.init(self.allocator, path, name);
            // Need to update the key to point to owned memory
            gop.key_ptr.* = gop.value_ptr.path;
        }
        return gop.value_ptr;
    }

    /// Get a module node by path
    pub fn getModule(self: *const DependencyGraph, path: []const u8) ?*const ModuleNode {
        return self.modules.getPtr(path);
    }

    /// Update the content hash for a module (creates module if it doesn't exist)
    pub fn setContentHash(self: *DependencyGraph, path: []const u8, hash: [32]u8) Allocator.Error!void {
        const node = try self.getOrCreateModule(path, std.fs.path.basename(path));
        node.content_hash = hash;
    }

    /// Get the content hash for a module
    pub fn getContentHash(self: *const DependencyGraph, path: []const u8) ?[32]u8 {
        if (self.modules.get(path)) |node| {
            // Check if hash is non-zero (has been set)
            const zeroes = std.mem.zeroes([32]u8);
            if (!std.mem.eql(u8, &node.content_hash, &zeroes)) {
                return node.content_hash;
            }
        }
        return null;
    }

    /// Update the exports hash for a module
    pub fn setExportsHash(self: *DependencyGraph, path: []const u8, hash: [32]u8) void {
        if (self.modules.getPtr(path)) |node| {
            node.exports_hash = hash;
        }
    }

    /// Get the exports hash for a module
    pub fn getExportsHash(self: *const DependencyGraph, path: []const u8) ?[32]u8 {
        if (self.modules.get(path)) |node| {
            // Check if hash is non-zero (has been set)
            const zeroes = std.mem.zeroes([32]u8);
            if (!std.mem.eql(u8, &node.exports_hash, &zeroes)) {
                return node.exports_hash;
            }
        }
        return null;
    }

    /// Build the dependency graph from a Coordinator PackageState after a successful build.
    /// This extracts module relationships from the compiler's internal state.
    pub fn buildFromPackageState(self: *DependencyGraph, pkg: *compile.coordinator.PackageState) Allocator.Error!void {
        // First pass: create all module nodes
        for (pkg.modules.items) |*module_state| {
            const node = try self.getOrCreateModule(module_state.path, module_state.name);
            node.depth = module_state.depth;
        }

        // Second pass: build import/dependent relationships
        for (pkg.modules.items) |*module_state| {
            const node = self.modules.getPtr(module_state.path) orelse continue;

            // Add imports (local modules within same package)
            for (module_state.imports.items) |edge| {
                if (edge.module_id < pkg.modules.items.len) {
                    const imported_module = &pkg.modules.items[edge.module_id];
                    try node.addImport(self.allocator, imported_module.path);

                    // Add reverse dependency
                    if (self.modules.getPtr(imported_module.path)) |imported_node| {
                        try imported_node.addDependent(self.allocator, module_state.path);
                    }
                }
            }
        }
    }

    /// Compute a Blake3 hash of the given content.
    pub fn computeContentHash(content: []const u8) [32]u8 {
        var hasher = std.crypto.hash.Blake3.init(.{});
        hasher.update(content);
        var hash: [32]u8 = undefined;
        hasher.final(&hash);
        return hash;
    }

    /// Compute a Blake3 hash of a module's exported symbols.
    /// This hash changes when the module's public interface changes,
    /// but not when only internal implementation changes.
    ///
    /// The hash is computed from sorted export names for stability.
    pub fn computeExportsHash(allocator: Allocator, module_env: *const @import("can").ModuleEnv) Allocator.Error![32]u8 {
        // Collect all exported symbol names
        var export_names: std.ArrayList([]const u8) = .empty;
        defer export_names.deinit(allocator);

        // Iterate through module's exports (definitions)
        const defs_slice = module_env.store.sliceDefs(module_env.exports);
        for (defs_slice) |def_idx| {
            const def = module_env.store.getDef(def_idx);
            // Get the definition name from its pattern
            const pattern = module_env.store.getSourcePattern(def.pattern);
            if (std.meta.activeTag(pattern) != .assign) continue;
            const name = module_env.common.idents.getText(pattern.assign.ident);
            try export_names.append(allocator, name);
        }

        // Sort names for stable hash (same exports in different order = same hash)
        std.mem.sort([]const u8, export_names.items, {}, struct {
            fn lessThan(_: void, a: []const u8, b: []const u8) bool {
                return std.mem.order(u8, a, b) == .lt;
            }
        }.lessThan);

        // Hash all export names
        var hasher = std.crypto.hash.Blake3.init(.{});
        for (export_names.items) |name| {
            hasher.update(name);
            hasher.update("\x00"); // Separator to avoid collisions
        }
        var hash: [32]u8 = undefined;
        hasher.final(&hash);
        return hash;
    }

    /// Get the number of modules in the graph
    pub fn count(self: *const DependencyGraph) usize {
        return self.modules.count();
    }
};

test "DependencyGraph basic operations" {
    const allocator = std.testing.allocator;

    var graph = DependencyGraph.init(allocator);
    defer graph.deinit();

    // Create some modules
    const node_a = try graph.getOrCreateModule("/path/to/A.roc", "A");
    const node_b = try graph.getOrCreateModule("/path/to/B.roc", "B");

    // A imports B
    try node_a.addImport(allocator, "/path/to/B.roc");
    try node_b.addDependent(allocator, "/path/to/A.roc");

    // Set content hash
    const hash = DependencyGraph.computeContentHash("module A\n");
    try graph.setContentHash("/path/to/A.roc", hash);

    // Check hash retrieval
    const retrieved_hash = graph.getContentHash("/path/to/A.roc");
    try std.testing.expect(retrieved_hash != null);
    try std.testing.expectEqualSlices(u8, &hash, &retrieved_hash.?);
}

test "clearRelationships preserves content and exports hashes" {
    const allocator = std.testing.allocator;

    var graph = DependencyGraph.init(allocator);
    defer graph.deinit();

    const node_a = try graph.getOrCreateModule("/path/to/A.roc", "A");
    const node_b = try graph.getOrCreateModule("/path/to/B.roc", "B");

    // A imports B
    try node_a.addImport(allocator, "/path/to/B.roc");
    try node_b.addDependent(allocator, "/path/to/A.roc");

    // Set content and exports hashes
    const content_hash = DependencyGraph.computeContentHash("module content");
    const exports_hash = DependencyGraph.computeContentHash("export foo");

    try graph.setContentHash("/path/to/A.roc", content_hash);
    graph.setExportsHash("/path/to/A.roc", exports_hash);

    // Verify hashes are set
    try std.testing.expect(graph.getContentHash("/path/to/A.roc") != null);
    try std.testing.expect(graph.getExportsHash("/path/to/A.roc") != null);

    // Clear relationships
    graph.clearRelationships();

    // Hashes should still be present
    const retrieved_content = graph.getContentHash("/path/to/A.roc");
    const retrieved_exports = graph.getExportsHash("/path/to/A.roc");

    try std.testing.expect(retrieved_content != null);
    try std.testing.expect(retrieved_exports != null);
    try std.testing.expectEqualSlices(u8, &content_hash, &retrieved_content.?);
    try std.testing.expectEqualSlices(u8, &exports_hash, &retrieved_exports.?);

    // But imports should be cleared
    const node_a_after = graph.modules.getPtr("/path/to/A.roc").?;
    try std.testing.expectEqual(@as(usize, 0), node_a_after.imports.items.len);
}

test "computeContentHash produces consistent results" {
    // Same content should always produce same hash
    const content = "this is some test content\nwith multiple lines\n";

    const hash1 = DependencyGraph.computeContentHash(content);
    const hash2 = DependencyGraph.computeContentHash(content);

    try std.testing.expectEqualSlices(u8, &hash1, &hash2);
}

test "computeContentHash produces different results for different content" {
    const content1 = "version 1";
    const content2 = "version 2";

    const hash1 = DependencyGraph.computeContentHash(content1);
    const hash2 = DependencyGraph.computeContentHash(content2);

    try std.testing.expect(!std.mem.eql(u8, &hash1, &hash2));
}

test "clear removes all modules" {
    const allocator = std.testing.allocator;

    var graph = DependencyGraph.init(allocator);
    defer graph.deinit();

    _ = try graph.getOrCreateModule("/path/to/A.roc", "A");
    _ = try graph.getOrCreateModule("/path/to/B.roc", "B");

    try std.testing.expectEqual(@as(usize, 2), graph.count());

    graph.clear();

    try std.testing.expectEqual(@as(usize, 0), graph.count());
}

test "getModule returns null for unknown path" {
    const allocator = std.testing.allocator;

    var graph = DependencyGraph.init(allocator);
    defer graph.deinit();

    const result = graph.getModule("/path/to/nonexistent.roc");
    try std.testing.expect(result == null);
}

test "getOrCreateModule returns existing module" {
    const allocator = std.testing.allocator;

    var graph = DependencyGraph.init(allocator);
    defer graph.deinit();

    const node1 = try graph.getOrCreateModule("/path/to/A.roc", "A");
    node1.depth = 5;

    const node2 = try graph.getOrCreateModule("/path/to/A.roc", "A");

    // Should be the same node
    try std.testing.expectEqual(@as(u32, 5), node2.depth);
    try std.testing.expectEqual(@as(usize, 1), graph.count());
}
