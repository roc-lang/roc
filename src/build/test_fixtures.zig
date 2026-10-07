//! Generated test hosts belong to the build cache. Fixture runners get private
//! mutable copies so their output files never alter source or cached inputs.
const std = @import("std");
const Step = std.Build.Step;
const LazyPath = std.Build.LazyPath;

const Output = struct {
    step: *Step,
    path: []const u8,
    source: LazyPath,
};

/// Stage declared fixture producers and provide isolated writable runner trees.
pub const Plan = struct {
    /// Fixture platforms that reuse another platform's host targets declare the
    /// reuse here, so prepared trees contain the copies without touching the checkout.
    pub const Alias = struct {
        source: []const u8,
        destination: []const u8,
        /// False when the source directory holds only generated hosts and is absent from the checkout.
        has_checked_in_inputs: bool = true,
    };

    b: *std.Build,
    outputs: std.ArrayList(Output) = .empty,
    static_root: ?LazyPath = null,
    aliases: []const Alias = &.{},

    pub fn copy(self: *Plan, files: *Step.WriteFile, source: LazyPath, path: []const u8) void {
        const generated = files.addCopyFile(source, path);
        self.outputs.append(self.b.allocator, .{
            .step = &files.step,
            .path = self.b.dupe(path),
            .source = generated,
        }) catch @panic("OOM");
    }

    pub fn output(self: *Plan, step: *Step, path: []const u8) LazyPath {
        for (self.outputs.items) |item| {
            if (item.step == step and std.mem.eql(u8, item.path, path)) return item.source;
        }
        std.debug.panic("missing declared test fixture: {s}", .{path});
    }

    /// Includes only generated files reachable from the caller's existing
    /// host dependencies. A small suite never inherits another suite's hosts.
    pub fn cachedRoot(self: *Plan, deps: []const *Step) LazyPath {
        const b = self.b;
        var visited: std.AutoHashMapUnmanaged(*Step, void) = .empty;
        for (deps) |dep| collectDependencies(b, dep, &visited);

        const files = b.addWriteFiles();
        _ = files.addCopyDirectory(self.staticRoot(), ".", .{});
        var paths: std.StringHashMapUnmanaged(LazyPath) = .empty;
        for (self.outputs.items) |item| {
            if (!visited.contains(item.step)) continue;
            const entry = paths.getOrPut(b.allocator, item.path) catch @panic("OOM");
            if (entry.found_existing) {
                // Multiple compilations of a host are allowed in the graph,
                // but each fixture tree must select one unambiguous producer.
                std.debug.panic("duplicate test fixture producers: {s}", .{item.path});
            }
            entry.value_ptr.* = item.source;
            _ = files.addCopyFile(item.source, item.path);
            for (self.aliases) |alias| {
                if (item.path.len > alias.source.len and
                    std.mem.startsWith(u8, item.path, alias.source) and
                    item.path[alias.source.len] == '/')
                {
                    _ = files.addCopyFile(item.source, b.pathJoin(&.{ alias.destination, item.path[alias.source.len + 1 ..] }));
                }
            }
        }
        return files.getDirectory();
    }

    pub fn mutableRoot(self: *Plan, deps: []const *Step) LazyPath {
        const files = self.b.addWriteFiles();
        files.mode = .tmp;
        _ = files.addCopyDirectory(self.cachedRoot(deps), ".", .{});
        return files.getDirectory();
    }

    /// Publication into the checkout is an explicit maintenance operation.
    pub fn addUpdateStep(self: *Plan, deps: []const *Step) void {
        var visited: std.AutoHashMapUnmanaged(*Step, void) = .empty;
        for (deps) |dep| collectDependencies(self.b, dep, &visited);
        const publication = self.b.addUpdateSourceFiles();
        var paths: std.StringHashMapUnmanaged(LazyPath) = .empty;
        for (self.outputs.items) |item| {
            if (visited.contains(item.step)) paths.put(self.b.allocator, item.path, item.source) catch @panic("OOM");
        }
        var iter = paths.iterator();
        while (iter.next()) |entry| publication.addCopyFileToSource(entry.value_ptr.*, entry.key_ptr.*);
        self.b.step("update-test-fixtures", "Copy generated test hosts into the checkout for manual use").dependOn(&publication.step);
    }

    fn staticRoot(self: *Plan) LazyPath {
        if (self.static_root) |root| return root;
        const files = self.b.addWriteFiles();
        _ = files.addCopyDirectory(self.b.path("test"), "test", .{
            // These are generated host/stub names. Checked-in CRT objects and
            // import libraries are fixture inputs and must remain included.
            .exclude_extensions = &.{ "libhost.a", "host.lib", "host.wasm", "libc.so", "libc.so.6", "libc_stub.s" },
        });
        for (self.aliases) |alias| {
            if (!alias.has_checked_in_inputs) continue;
            _ = files.addCopyDirectory(self.b.path(alias.source), alias.destination, .{
                .exclude_extensions = &.{ "libhost.a", "host.lib", "host.wasm", "libc.so", "libc.so.6", "libc_stub.s" },
            });
        }
        inline for (.{ "src", "vendor" }) |dir| {
            _ = files.addCopyDirectory(self.b.path(dir), dir, .{});
        }
        _ = files.addCopyDirectory(self.b.path("ci"), "ci", .{ .exclude_extensions = &.{ ".pyc", ".pyo" } });
        // Roc's string-import fixtures reference these files relative to their
        // original test directories. Declare the external inputs at the same
        // paths so prepared trees preserve those imports and hash their bytes.
        inline for (.{ "build.zig", "build.zig.zon", "design.md", "legal_details", "README.md", "CONTRIBUTING/profiling/bench_repeated_check_ORIGINAL.roc" }) |path| {
            _ = files.addCopyFile(self.b.path(path), path);
        }
        const root = files.getDirectory();
        self.static_root = root;
        return root;
    }
};

fn collectDependencies(b: *std.Build, step: *Step, visited: *std.AutoHashMapUnmanaged(*Step, void)) void {
    const entry = visited.getOrPut(b.allocator, step) catch @panic("OOM");
    if (entry.found_existing) return;
    for (step.dependencies.items) |dep| collectDependencies(b, dep, visited);
}
