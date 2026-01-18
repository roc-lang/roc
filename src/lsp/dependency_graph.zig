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
    /// Modules that this module imports (dependencies)
    imports: std.ArrayList([]const u8),
    /// Modules that import this module (reverse dependencies)
    dependents: std.ArrayList([]const u8),
    /// Build depth from root (lower = closer to root)
    depth: u32,

    pub fn init(allocator: Allocator, path: []const u8, name: []const u8) !ModuleNode {
        return .{
            .path = try allocator.dupe(u8, path),
            .name = try allocator.dupe(u8, name),
            .content_hash = std.mem.zeroes([32]u8),
            .imports = std.ArrayList([]const u8){},
            .dependents = std.ArrayList([]const u8){},
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

    pub fn addImport(self: *ModuleNode, allocator: Allocator, import_path: []const u8) !void {
        try self.imports.append(allocator, try allocator.dupe(u8, import_path));
    }

    pub fn addDependent(self: *ModuleNode, allocator: Allocator, dependent_path: []const u8) !void {
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

    /// Clear the graph (e.g., before rebuilding from scratch)
    pub fn clear(self: *DependencyGraph) void {
        var it = self.modules.iterator();
        while (it.next()) |entry| {
            var node = entry.value_ptr;
            node.deinit(self.allocator);
        }
        self.modules.clearRetainingCapacity();
    }

    /// Get or create a module node for the given path
    pub fn getOrCreateModule(self: *DependencyGraph, path: []const u8, name: []const u8) !*ModuleNode {
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

    /// Update the content hash for a module
    pub fn setContentHash(self: *DependencyGraph, path: []const u8, hash: [32]u8) void {
        if (self.modules.getPtr(path)) |node| {
            node.content_hash = hash;
        }
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

    /// Build the dependency graph from a PackageEnv after a successful build.
    /// This extracts module relationships from the compiler's internal state.
    pub fn buildFromPackageEnv(self: *DependencyGraph, pkg_env: *compile.package.PackageEnv) !void {
        // First pass: create all module nodes
        for (pkg_env.modules.items, 0..) |*module_state, idx| {
            const node = try self.getOrCreateModule(module_state.path, module_state.name);
            node.depth = module_state.depth;
            _ = idx;
        }

        // Second pass: build import/dependent relationships
        for (pkg_env.modules.items) |*module_state| {
            const node = self.modules.getPtr(module_state.path) orelse continue;

            // Add imports (local modules within same package)
            for (module_state.imports.items) |import_id| {
                if (import_id < pkg_env.modules.items.len) {
                    const imported_module = &pkg_env.modules.items[import_id];
                    try node.addImport(self.allocator, imported_module.path);

                    // Add reverse dependency
                    if (self.modules.getPtr(imported_module.path)) |imported_node| {
                        try imported_node.addDependent(self.allocator, module_state.path);
                    }
                }
            }
        }
    }

    /// Get all modules that would be affected if the given module changes.
    /// Returns a list of paths that need to be rebuilt (transitively).
    pub fn getStaleModules(self: *const DependencyGraph, changed_path: []const u8) ![]const []const u8 {
        var stale = std.ArrayList([]const u8){};
        errdefer stale.deinit(self.allocator);

        var visited = std.StringHashMap(void).init(self.allocator);
        defer visited.deinit();

        // Use a worklist for BFS traversal of dependents
        var worklist = std.ArrayList([]const u8){};
        defer worklist.deinit(self.allocator);

        try worklist.append(self.allocator, changed_path);
        try visited.put(changed_path, {});

        while (worklist.items.len > 0) {
            const current_path = worklist.pop().?;
            try stale.append(self.allocator, current_path);

            // Add all dependents to the worklist
            if (self.modules.get(current_path)) |node| {
                for (node.dependents.items) |dep_path| {
                    if (!visited.contains(dep_path)) {
                        try visited.put(dep_path, {});
                        try worklist.append(self.allocator, dep_path);
                    }
                }
            }
        }

        return stale.toOwnedSlice(self.allocator);
    }

    /// Check if a module's content has changed by comparing hashes.
    pub fn hasContentChanged(self: *const DependencyGraph, path: []const u8, new_hash: [32]u8) bool {
        if (self.getContentHash(path)) |old_hash| {
            return !std.mem.eql(u8, &old_hash, &new_hash);
        }
        // No previous hash means it's new or changed
        return true;
    }

    /// Compute a Blake3 hash of the given content.
    pub fn computeContentHash(content: []const u8) [32]u8 {
        var hasher = std.crypto.hash.Blake3.init(.{});
        hasher.update(content);
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
    graph.setContentHash("/path/to/A.roc", hash);

    // Check hash retrieval
    const retrieved_hash = graph.getContentHash("/path/to/A.roc");
    try std.testing.expect(retrieved_hash != null);
    try std.testing.expectEqualSlices(u8, &hash, &retrieved_hash.?);

    // Check stale modules when B changes
    const stale = try graph.getStaleModules("/path/to/B.roc");
    defer allocator.free(stale);

    // Both B and A should be stale (A depends on B)
    try std.testing.expectEqual(@as(usize, 2), stale.len);
}
