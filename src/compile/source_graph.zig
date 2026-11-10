//! Import-graph line counting without canonicalization, checking or evaluation.
const std = @import("std");
const base = @import("base");
const collections = @import("collections");
const parse = @import("parse");
const CoreCtx = @import("ctx").CoreCtx;
const discovery = @import("module_discovery.zig");
const inventory = @import("source_inventory.zig");
const Resolved = @import("package_resolution.zig").Resolved;
const CacheManager = @import("cache_manager.zig").CacheManager;
const Allocator = std.mem.Allocator;

/// A position in `Graph.files`.
const FileIndex = enum(u32) { _ };

/// Physical lines and the subset containing code.
pub const Counts = struct {
    lines: u64 = 0,
    code: u64 = 0,

    /// Accumulate another source file or category.
    pub fn add(self: *Counts, other: Counts) void {
        self.lines += other.lines;
        self.code += other.code;
    }
};

/// Count physical lines. A final newline terminates the preceding line; it
/// does not add an empty line. Only the first non-whitespace byte determines
/// whether a nonblank line is comment-only; inline comments still count.
pub fn count(source: []const u8) Counts {
    var result: Counts = .{};
    var start: usize = 0;
    while (start < source.len) {
        const end = std.mem.findScalarPos(u8, source, start, '\n') orelse source.len;
        result.lines += 1;
        var first = start;
        while (first < end and std.ascii.isWhitespace(source[first])) : (first += 1) {}
        if (first < end and source[first] != '#') result.code += 1;
        start = end + 1;
    }
    return result;
}

/// Ownership of a counted source file, in attribution priority order.
pub const Category = enum { root, local, https };

/// One canonical source path and its source-only facts.
pub const File = struct {
    path: []const u8,
    counts: Counts,
    imports: inventory.Inventory,
    category: ?Category = null,
};

/// The source and import being processed when traversal fails.
pub const Failure = struct {
    path: []const u8 = "",
    import_name: ?[]const u8 = null,
};

/// Source I/O, allocation, syntax, and import-resolution failures.
pub const Error = Allocator.Error || CoreCtx.ReadError || CoreCtx.CanonicalizeError || error{
    InvalidSyntax,
    ImportEscapesPackageRoot,
    UnknownPackage,
    PrivateModule,
    ImportSourceAlias,
};

/// Owns a traversal's source metadata. File bytes and ASTs are released after
/// each cache lookup/parse. Physical files are counted once even when visited
/// in several package contexts, whose aliases can resolve differently.
pub const Graph = struct {
    arena: base.SingleThreadArena,
    fs: CoreCtx,
    manager: ?*CacheManager,
    resolved: *const Resolved,
    files: std.ArrayList(File) = .empty,
    file_indices: std.StringHashMapUnmanaged(usize) = .empty,
    visited: std.StringHashMapUnmanaged(void) = .empty,
    physical_modules: std.StringHashMapUnmanaged([]const u8) = .empty,
    package_roots: []const []const u8 = &.{},
    queue: std.ArrayList(Module) = .empty,
    roots: collections.OptionalIndexColumn(FileIndex) = .{},
    failure: Failure = .{},
    cache_hits: usize = 0,
    parses: usize = 0,

    const Module = struct { package: usize, logical: []const u8, path: []const u8 };

    /// Start a graph traversal borrowing the resolved dependency graph.
    pub fn init(gpa: Allocator, fs: CoreCtx, manager: ?*CacheManager, resolved: *const Resolved) Graph {
        return .{ .arena = base.SingleThreadArena.init(gpa), .fs = fs, .manager = manager, .resolved = resolved };
    }

    /// Release all retained source metadata.
    pub fn deinit(self: *Graph) void {
        self.arena.deinit();
    }

    /// Classify the entry, including headerless default-platform applications.
    pub fn rootKind(self: *const Graph) @import("package_resolution.zig").HeaderKind {
        const root = self.roots.get(Resolved.root_index) orelse base.invariant("completed source graph has a root inventory", .{});
        return if (self.files.items[@backingInt(root)].imports.is_application) .app else self.resolved.packages[0].kind;
    }

    /// Sum each counted file once in its selected ownership category.
    pub fn totals(self: *const Graph) [3]Counts {
        var result: [3]Counts = @splat(.{});
        for (self.files.items) |file| if (file.category) |category| {
            result[@backingInt(category)].add(file.counts);
        };
        return result;
    }

    /// Visit the exact import closure of the entry and selected platform.
    pub fn run(self: *Graph) Error!void {
        const a = self.arena.allocator();
        self.roots = try collections.OptionalIndexColumn(FileIndex).initFilled(a, self.resolved.packages.len);
        const package_roots = try a.alloc([]const u8, self.resolved.packages.len);
        for (self.resolved.packages, package_roots) |package, *out| out.* = try self.fs.canonicalize(package.root_dir, a);
        self.package_roots = package_roots;
        _ = try self.loadRoot(Resolved.root_index);
        try self.enqueueRoot(Resolved.root_index);
        if (self.resolved.selected_platform_index) |platform| try self.enqueueRoot(platform);
        var next: usize = 0;
        while (next < self.queue.items.len) : (next += 1) {
            const module = self.queue.items[next];
            self.failure = .{ .path = module.path };
            const file_index = try self.loadFile(module.path);
            const physical = self.files.items[file_index].path;
            const package_root = self.package_roots[module.package];
            if (!std.mem.startsWith(u8, physical, package_root) or
                (physical.len > package_root.len and !std.fs.path.isSep(package_root[package_root.len - 1]) and !std.fs.path.isSep(physical[package_root.len])))
                return error.ImportEscapesPackageRoot;
            const physical_key = try std.fmt.allocPrint(a, "{d}:{s}", .{ module.package, physical });
            const physical_entry = try self.physical_modules.getOrPut(a, physical_key);
            if (physical_entry.found_existing and !std.mem.eql(u8, physical_entry.value_ptr.*, module.logical)) return error.ImportSourceAlias;
            physical_entry.value_ptr.* = module.logical;
            self.markCounted(file_index, module.package);
            const imports = self.files.items[file_index].imports;
            for (imports.local) |local| {
                self.failure = .{ .path = module.path, .import_name = local.import_name };
                const logical = (try discovery.resolveLocalImportLogicalPath(a, module.logical, local)) orelse return error.ImportEscapesPackageRoot;
                try self.enqueue(module.package, logical);
            }
            for (imports.external) |external| {
                self.failure = .{ .path = module.path, .import_name = external };
                const qualified = base.module_path.parseQualifiedImport(external) orelse
                    base.invariant("parser inventory contains a non-qualified external import", .{});
                const package = self.resolved.packages[module.package];
                const target: usize = target: {
                    for (package.deps) |dep| {
                        if (std.mem.eql(u8, dep.alias, qualified.qualifier)) break :target dep.target;
                    }
                    return error.UnknownPackage;
                };
                const root_index = try self.loadRoot(target);
                self.markCounted(root_index, target);
                const public = self.files.items[root_index].imports.public;
                const logical = target: {
                    for (public) |item| {
                        if (std.mem.eql(u8, item.name, qualified.module)) break :target item.target;
                    }
                    return error.PrivateModule;
                };
                try self.enqueue(target, logical);
            }
        }
    }

    fn markCounted(self: *Graph, index: usize, package_index: usize) void {
        const package = self.resolved.packages[package_index];
        // Compiler-owned sources are not application or user dependency code.
        if (package.compiler_owned_platform != null) return;
        const category: Category = if (package_index == 0) .root else if (package.url != null) .https else .local;
        const file = &self.files.items[index];
        // If two package contexts share a source file, root ownership wins,
        // then local ownership. Attribution never depends on visit order.
        if (file.category == null or @backingInt(category) < @backingInt(file.category.?)) file.category = category;
    }

    fn loadRoot(self: *Graph, package_index: usize) Error!usize {
        if (self.roots.get(package_index)) |index| return @backingInt(index);
        const index = try self.loadFile(self.resolved.packages[package_index].root_file);
        self.roots.set(package_index, @fromBackingInt(@as(u32, @intCast(index))));
        return index;
    }

    fn enqueueRoot(self: *Graph, package_index: usize) Error!void {
        const package = self.resolved.packages[package_index];
        try self.enqueuePath(package_index, base.module_path.getModuleName(package.root_file), package.root_file);
    }

    fn enqueue(self: *Graph, package_index: usize, logical: []const u8) Error!void {
        const a = self.arena.allocator();
        const relative = try std.fmt.allocPrint(a, "{s}.roc", .{logical});
        const path = try std.fs.path.join(a, &.{ self.resolved.packages[package_index].root_dir, relative });
        try self.enqueuePath(package_index, logical, path);
    }

    fn enqueuePath(self: *Graph, package_index: usize, logical: []const u8, path: []const u8) Allocator.Error!void {
        const a = self.arena.allocator();
        const visit_key = try std.fmt.allocPrint(a, "{d}:{s}", .{ package_index, logical });
        const entry = try self.visited.getOrPut(a, visit_key);
        if (entry.found_existing) return;
        try self.queue.append(a, .{ .package = package_index, .logical = logical, .path = path });
    }

    fn loadFile(self: *Graph, path: []const u8) Error!usize {
        const a = self.arena.allocator();
        self.failure.path = path;
        const physical_path = try self.fs.canonicalize(path, a);
        if (self.file_indices.get(physical_path)) |index| return index;
        const gpa = self.arena.child_allocator;
        var source = try self.fs.readFile(physical_path, gpa);
        defer gpa.free(source);
        const counts = count(source);
        source = try base.source_utils.normalizeLineEndingsRealloc(gpa, source);
        const source_key = inventory.key(source);
        const cached = if (self.manager) |manager| try inventory.load(a, manager, source_key) else null;
        const imports = if (cached) |facts| facts else fresh: {
            var env = try base.CommonEnv.init(gpa, source);
            defer env.deinit(gpa);
            const ast = try parse.file(gpa, &env);
            defer ast.deinit();
            self.parses += 1;
            if (ast.tokenize_diagnostics.items.len != 0 or ast.parse_diagnostics.items.len != 0) return error.InvalidSyntax;
            const facts = try inventory.fromAst(a, ast);
            if (self.manager) |manager| try inventory.store(gpa, manager, source_key, facts);
            break :fresh facts;
        };
        if (cached != null) self.cache_hits += 1;
        const index = self.files.items.len;
        try self.files.append(a, .{ .path = physical_path, .counts = counts, .imports = imports });
        try self.file_indices.put(a, physical_path, index);
        return index;
    }
};

test "loc counts blank and indented comment lines, inline comments, CRLF and final lines" {
    const cases = [_]struct { source: []const u8, lines: u64, code: u64 }{
        .{ .source = "", .lines = 0, .code = 0 },
        .{ .source = "\n", .lines = 1, .code = 0 },
        .{ .source = "\n\n", .lines = 2, .code = 0 },
        .{ .source = "x", .lines = 1, .code = 1 },
        .{ .source = "x\n", .lines = 1, .code = 1 },
        .{ .source = "  \t\r\n# comment\n \t# indented\nx # inline\n\"# string\"\n", .lines = 5, .code = 2 },
        .{ .source = "a\r\n\r\n\t# comment\r\nb", .lines = 4, .code = 2 },
        .{ .source = " \t\x0b\x0c\r", .lines = 1, .code = 0 },
    };
    for (cases) |case| try std.testing.expectEqual(Counts{ .lines = case.lines, .code = case.code }, count(case.source));
}

test "loc graph follows transitive imports and aliases, deduplicates diamonds and ignores unused files" {
    const gpa = std.testing.allocator;
    const io = std.testing.io;
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    const root = try tmp.dir.realPathFileAlloc(io, ".", gpa);
    defer gpa.free(root);
    const fs = CoreCtx.default(gpa, gpa, io);
    const fixtures = [_]struct { path: []const u8, source: []const u8 }{
        .{ .path = "app/main.roc", .source = "app [main!] { dep: \"../dep/main.roc\" }\nimport A\nimport B\nimport dep.Public\nmain! = |_| {}\n" },
        .{ .path = "app/A.roc", .source = "module []\nimport Shared\nimport Sub/Child\n# comment\n" },
        .{ .path = "app/B.roc", .source = "module []\nimport Shared\n" },
        .{ .path = "app/Shared.roc", .source = "module []\n\n \t# comment\n" },
        .{ .path = "app/Sub/Child.roc", .source = "module []\nimport ../Shared\nimport /B\n" },
        .{ .path = "app/Unused.roc", .source = "this is deliberately invalid and must never be parsed\n" },
        .{ .path = "dep/main.roc", .source = "package [Public, Unused] { trans: \"../trans/main.roc\" }\nimport Internal/Impl as Public\n" },
        .{ .path = "dep/Internal/Impl.roc", .source = "module []\nimport trans.Value\nimport /Shared\n" },
        .{ .path = "dep/Shared.roc", .source = "module []\nimport trans.Value\n" },
        .{ .path = "dep/Unused.roc", .source = "this unimported public module must not be parsed\n" },
        .{ .path = "trans/main.roc", .source = "package [Value] {}\n" },
        .{ .path = "trans/Value.roc", .source = "module []\n# final comment" },
    };
    var expected: [3]Counts = @splat(.{});
    for (fixtures) |fixture| {
        if (std.fs.path.dirname(fixture.path)) |dir| try tmp.dir.createDirPath(io, dir);
        try tmp.dir.writeFile(io, .{ .sub_path = fixture.path, .data = fixture.source });
        if (std.mem.endsWith(u8, fixture.path, "Unused.roc")) continue;
        expected[if (std.mem.startsWith(u8, fixture.path, "app/")) @as(usize, 0) else 1].add(count(fixture.source));
    }
    var fetcher: @import("package_resolution.zig").CtxFetcher = .{ .fs = fs, .gpa = gpa, .cache_packages_dir = null };
    var resolver = @import("package_resolution.zig").Resolver.init(gpa, fetcher.fetcher(), .{});
    defer resolver.deinit();
    const entry = try std.fs.path.join(gpa, &.{ root, "app/main.roc" });
    defer gpa.free(entry);
    var resolved = try resolver.resolve(entry);
    defer resolved.deinit();
    const cache_dir = try std.fs.path.join(gpa, &.{ root, "cache" });
    defer gpa.free(cache_dir);
    var manager = CacheManager.init(gpa, .{ .cache_dir = cache_dir }, fs);
    defer manager.deinit();
    for (0..3) |run| {
        manager.config.enabled = run != 2;
        var graph = Graph.init(gpa, fs, &manager, &resolved);
        defer graph.deinit();
        try graph.run();
        try std.testing.expectEqualDeep(expected, graph.totals());
        try std.testing.expectEqual(@as(usize, 10), graph.files.items.len);
        if (run == 1) {
            try std.testing.expectEqual(@as(usize, 0), graph.parses);
            try std.testing.expectEqual(@as(usize, 10), graph.cache_hits);
        }
    }
    // An edit adds an import without changing the resolved package graph.
    // The source-byte key must reject stale inventories and report its absence.
    try tmp.dir.writeFile(io, .{ .sub_path = "app/Shared.roc", .data = "module []\nimport Missing\n" });
    manager.config.enabled = true;
    var changed = Graph.init(gpa, fs, &manager, &resolved);
    defer changed.deinit();
    try std.testing.expectError(error.FileNotFound, changed.run());
    try std.testing.expect(std.mem.endsWith(u8, changed.failure.path, "Missing.roc"));
}

test "loc graph classifies HTTPS by resolved provenance and counts shared sources once" {
    const gpa = std.testing.allocator;
    const io = std.testing.io;
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    const root = try tmp.dir.realPathFileAlloc(io, ".", gpa);
    defer gpa.free(root);
    const fs = CoreCtx.default(gpa, gpa, io);
    try tmp.dir.createDirPath(io, "dep");
    try tmp.dir.writeFile(io, .{ .sub_path = "main.roc", .data = "app [main!] { a: \"dep/main.roc\", b: \"dep/main.roc\" }\nimport a.Foo\nimport b.Foo\nmain! = |_| {}\n" });
    try tmp.dir.writeFile(io, .{ .sub_path = "dep/main.roc", .data = "package [Foo] {}\n" });
    try tmp.dir.writeFile(io, .{ .sub_path = "dep/Foo.roc", .data = "module []\n# comment\n" });
    var fetcher: @import("package_resolution.zig").CtxFetcher = .{ .fs = fs, .gpa = gpa, .cache_packages_dir = null };
    var resolver = @import("package_resolution.zig").Resolver.init(gpa, fetcher.fetcher(), .{});
    defer resolver.deinit();
    const entry = try std.fs.path.join(gpa, &.{ root, "main.roc" });
    defer gpa.free(entry);
    var resolved = try resolver.resolve(entry);
    defer resolved.deinit();
    try std.testing.expectEqual(@as(usize, 2), resolved.packages.len);
    // Source classification consumes resolver provenance, never cache paths.
    resolved.packages[1].url = .{ .url = "https://example.com/pkg", .url_id = undefined, .version = undefined, .hash = "hash" };
    var graph = Graph.init(gpa, fs, null, &resolved);
    defer graph.deinit();
    try graph.run();
    try std.testing.expectEqual(@as(usize, 3), graph.files.items.len);
    try std.testing.expectEqual(Counts{}, graph.totals()[1]);
    try std.testing.expectEqual(Counts{ .lines = 3, .code = 2 }, graph.totals()[2]);
}

test "loc graph rejects incomplete syntax instead of printing partial counts" {
    const gpa = std.testing.allocator;
    const io = std.testing.io;
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    try tmp.dir.writeFile(io, .{ .sub_path = "main.roc", .data = "package [Broken] {}\n" });
    try tmp.dir.writeFile(io, .{ .sub_path = "Broken.roc", .data = "module []\nimport ../\n" });
    const entry = try tmp.dir.realPathFileAlloc(io, "main.roc", gpa);
    defer gpa.free(entry);
    const fs = CoreCtx.default(gpa, gpa, io);
    var fetcher: @import("package_resolution.zig").CtxFetcher = .{ .fs = fs, .gpa = gpa, .cache_packages_dir = null };
    var resolver = @import("package_resolution.zig").Resolver.init(gpa, fetcher.fetcher(), .{});
    defer resolver.deinit();
    var resolved = try resolver.resolve(entry);
    defer resolved.deinit();
    var graph = Graph.init(gpa, fs, null, &resolved);
    defer graph.deinit();
    try std.testing.expectError(error.InvalidSyntax, graph.run());
}

test "loc graph traverses a shared file in each package alias context but counts it once" {
    const gpa = std.testing.allocator;
    const io = std.testing.io;
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    const fs = CoreCtx.default(gpa, gpa, io);
    const fixtures = [_]struct { path: []const u8, source: []const u8 }{
        .{ .path = "main.roc", .source = "app [main!] { a: \"shared/a.roc\", b: \"shared/b.roc\" }\nimport a.Foo\nimport b.Foo\nmain! = |_| {}\n" },
        .{ .path = "shared/a.roc", .source = "package [Foo] { dep: \"../one/main.roc\" }\n" },
        .{ .path = "shared/b.roc", .source = "package [Foo] { dep: \"../two/main.roc\" }\n" },
        .{ .path = "shared/Foo.roc", .source = "module []\nimport dep.Value\n" },
        .{ .path = "one/main.roc", .source = "package [Value] {}\n" },
        .{ .path = "one/Value.roc", .source = "module []\n" },
        .{ .path = "two/main.roc", .source = "package [Value] {}\n" },
        .{ .path = "two/Value.roc", .source = "module []\n\n# separate file, identical module name\n" },
    };
    var expected: Counts = .{};
    for (fixtures, 0..) |fixture, i| {
        if (std.fs.path.dirname(fixture.path)) |dir| try tmp.dir.createDirPath(io, dir);
        try tmp.dir.writeFile(io, .{ .sub_path = fixture.path, .data = fixture.source });
        if (i != 0) expected.add(count(fixture.source));
    }
    const entry = try tmp.dir.realPathFileAlloc(io, "main.roc", gpa);
    defer gpa.free(entry);
    var fetcher: @import("package_resolution.zig").CtxFetcher = .{ .fs = fs, .gpa = gpa, .cache_packages_dir = null };
    var resolver = @import("package_resolution.zig").Resolver.init(gpa, fetcher.fetcher(), .{});
    defer resolver.deinit();
    var resolved = try resolver.resolve(entry);
    defer resolved.deinit();
    var graph = Graph.init(gpa, fs, null, &resolved);
    defer graph.deinit();
    try graph.run();
    try std.testing.expectEqual(@as(usize, 8), graph.files.items.len);
    try std.testing.expectEqual(expected, graph.totals()[1]);
}

test "loc graph includes platform hosted targets and recognizes default applications" {
    const gpa = std.testing.allocator;
    const io = std.testing.io;
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    const fs = CoreCtx.default(gpa, gpa, io);
    try tmp.dir.writeFile(io, .{
        .sub_path = "main.roc",
        .data = "platform \"test\"\n" ++
            "    requires {}\n    exposes []\n    packages {}\n    provides {}\n" ++
            "    hosted { \"roc_effect_run\": Effect.run! }\n    targets: {}\n",
    });
    try tmp.dir.writeFile(io, .{ .sub_path = "Effect.roc", .data = "module [run!]\nrun! : {} => {}\n" });
    const entry = try tmp.dir.realPathFileAlloc(io, "main.roc", gpa);
    defer gpa.free(entry);
    var fetcher: @import("package_resolution.zig").CtxFetcher = .{ .fs = fs, .gpa = gpa, .cache_packages_dir = null };
    var resolver = @import("package_resolution.zig").Resolver.init(gpa, fetcher.fetcher(), .{});
    defer resolver.deinit();
    var resolved = try resolver.resolve(entry);
    defer resolved.deinit();
    var graph = Graph.init(gpa, fs, null, &resolved);
    defer graph.deinit();
    try graph.run();
    try std.testing.expectEqual(.platform, graph.rootKind());
    try std.testing.expectEqual(@as(usize, 2), graph.files.items.len);
    try std.testing.expectEqual(@as(u64, 9), graph.totals()[0].lines);

    try tmp.dir.writeFile(io, .{ .sub_path = "main.roc", .data = "main! = |_| {}\n" });
    var app = Graph.init(gpa, fs, null, &resolved);
    defer app.deinit();
    try app.run();
    try std.testing.expectEqual(.app, app.rootKind());
}
