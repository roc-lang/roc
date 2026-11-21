//! Generated test hosts belong to the build cache. Fixture consumers read the
//! cached tree itself and write only into a private scratch directory, so
//! their output files never alter source or cached inputs.
const std = @import("std");
const Step = std.Build.Step;
const LazyPath = std.Build.LazyPath;

const Output = struct {
    step: *Step,
    path: []const u8,
    source: LazyPath,
};

/// Stage declared fixture producers and provide the trees runners read.
pub const Plan = struct {
    /// Fixture platforms that reuse another platform's host targets declare the
    /// reuse here, so prepared trees contain the copies without touching the checkout.
    pub const Alias = struct {
        source: []const u8,
        destination: []const u8,
        /// False when the source directory holds only generated hosts and is absent from the checkout.
        has_checked_in_inputs: bool = true,
        /// The `targets/<name>` directories the destination receives, or null
        /// for all of them. Consumers that only build for the machine running
        /// the tests name those targets, so the tree holds no copies of cross
        /// targets' hosts and C runtimes that nothing reads.
        targets: ?[]const []const u8 = null,

        /// Whether `relative`, a path under `source`, lies in a target directory this alias copies.
        fn covers(self: Alias, relative: []const u8) bool {
            const names = self.targets orelse return true;
            const end = std.mem.findScalar(u8, relative, '/') orelse relative.len;
            for (names) |name| {
                if (std.mem.eql(u8, name, relative[0..end])) return true;
            }
            return false;
        }
    };

    b: *std.Build,
    outputs: std.ArrayList(Output) = .empty,
    aliases: []const Alias = &.{},

    /// Declare that prepared trees hold `source` at `path`. `files` is the
    /// step callers depend on to have the file produced; prepared trees copy
    /// `source` itself, so no intermediate copy is made.
    pub fn copy(self: *Plan, files: *Step.WriteFile, source: LazyPath, path: []const u8) void {
        source.addStepDependencies(&files.step);
        self.outputs.append(self.b.allocator, .{
            .step = &files.step,
            .path = self.b.dupe(path),
            .source = source,
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
        const files = self.b.addWriteFiles();
        self.addStaticInputs(files);
        self.addGenerated(files, deps, "");
        return files.getDirectory();
    }

    /// A prepared tree holding only the checkout directory `dir` and the
    /// generated files under it, at their usual paths, for consumers that
    /// read nothing outside `dir`.
    pub fn cachedDirectory(self: *Plan, dir: []const u8, deps: []const *Step) LazyPath {
        const files = self.b.addWriteFiles();
        _ = files.addCopyDirectory(self.b.path(dir), dir, .{ .exclude_extensions = &generated_extensions });
        for (self.aliases) |alias| {
            if (!isUnder(alias.destination, dir)) continue;
            self.addCheckedInAliasInputs(files, alias);
        }
        self.addGenerated(files, deps, dir);
        return files.getDirectory();
    }

    /// Copies the generated files reachable from `deps` whose paths lie under
    /// `dir` (every one, when `dir` is empty), plus their alias copies.
    fn addGenerated(self: *Plan, files: *Step.WriteFile, deps: []const *Step, dir: []const u8) void {
        const b = self.b;
        var visited: std.AutoHashMapUnmanaged(*Step, void) = .empty;
        for (deps) |dep| collectDependencies(b, dep, &visited);

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
            // Declared paths use the host separator, but aliases and `dir` are
            // written with '/', so compare in normalized form.
            const normalized = b.allocator.dupe(u8, item.path) catch @panic("OOM");
            std.mem.replaceScalar(u8, normalized, '\\', '/');
            if (isUnder(normalized, dir)) _ = files.addCopyFile(item.source, item.path);
            for (self.aliases) |alias| {
                if (!isUnder(alias.destination, dir)) continue;
                if (normalized.len > alias.source.len and
                    std.mem.startsWith(u8, normalized, alias.source) and
                    normalized[alias.source.len] == '/' and
                    alias.covers(normalized[alias.source.len + 1 ..]))
                {
                    _ = files.addCopyFile(item.source, b.fmt("{s}/{s}", .{ alias.destination, normalized[alias.source.len + 1 ..] }));
                }
            }
        }
    }

    /// An empty private directory, removed when the build finishes, for the
    /// files a fixture consumer writes; the fixture tree itself is only read.
    pub fn scratchDirectory(self: *Plan) LazyPath {
        const files = self.b.addWriteFiles();
        files.mode = .tmp;
        return files.getDirectory();
    }

    /// Publication into the checkout is an explicit maintenance operation.
    pub fn addUpdateStep(self: *Plan, deps: []const *Step) void {
        self.b.step("update-test-fixtures", "Copy generated test hosts into the checkout for manual use").dependOn(self.publication(deps));
    }

    /// A step that copies the generated files reachable from `deps` into the
    /// checkout at their fixture paths.
    pub fn publication(self: *Plan, deps: []const *Step) *Step {
        var visited: std.AutoHashMapUnmanaged(*Step, void) = .empty;
        for (deps) |dep| collectDependencies(self.b, dep, &visited);
        const update = self.b.addUpdateSourceFiles();
        var paths: std.StringHashMapUnmanaged(LazyPath) = .empty;
        for (self.outputs.items) |item| {
            if (visited.contains(item.step)) paths.put(self.b.allocator, item.path, item.source) catch @panic("OOM");
        }
        var iter = paths.iterator();
        while (iter.next()) |entry| update.addCopyFileToSource(entry.value_ptr.*, entry.key_ptr.*);
        return &update.step;
    }

    /// Copies the checked-in inputs of the target directories `alias` covers.
    fn addCheckedInAliasInputs(self: *Plan, files: *Step.WriteFile, alias: Alias) void {
        if (!alias.has_checked_in_inputs) return;
        const b = self.b;
        const names = alias.targets orelse {
            _ = files.addCopyDirectory(b.path(alias.source), alias.destination, .{ .exclude_extensions = &generated_extensions });
            return;
        };
        for (names) |name| {
            const source = b.pathJoin(&.{ alias.source, name });
            // A target whose inputs are all generated has no checked-in directory.
            const absolute = b.root.joinString(b.allocator, source) catch @panic("OOM");
            std.Io.Dir.cwd().access(b.graph.io, absolute, .{}) catch |err| switch (err) {
                error.FileNotFound => continue,
                else => std.debug.panic("unable to read {s}: {t}", .{ source, err }),
            };
            _ = files.addCopyDirectory(b.path(source), b.fmt("{s}/{s}", .{ alias.destination, name }), .{ .exclude_extensions = &generated_extensions });
        }
    }

    fn addStaticInputs(self: *Plan, files: *Step.WriteFile) void {
        _ = files.addCopyDirectory(self.b.path("test"), "test", .{ .exclude_extensions = &generated_extensions });
        for (self.aliases) |alias| self.addCheckedInAliasInputs(files, alias);
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
    }
};

/// Generated host and stub names. Checked-in CRT objects and import libraries
/// are fixture inputs and stay included.
const generated_extensions = [_][]const u8{ "libhost.a", "host.lib", "host.wasm", "libc.so", "libc.so.6", "libc_stub.s" };

fn isUnder(path: []const u8, dir: []const u8) bool {
    if (dir.len == 0) return true;
    return path.len > dir.len and std.mem.startsWith(u8, path, dir) and path[dir.len] == '/';
}

fn collectDependencies(b: *std.Build, step: *Step, visited: *std.AutoHashMapUnmanaged(*Step, void)) void {
    const entry = visited.getOrPut(b.allocator, step) catch @panic("OOM");
    if (entry.found_existing) return;
    for (step.dependencies.items) |dep| collectDependencies(b, dep, visited);
}
