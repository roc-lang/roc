//! Verifies that every `test` declaration in src/ actually runs in CI.
//!
//! The check has two layers:
//!
//! 1. An import-level wiring check: every file containing a `test` decl must
//!    be `@import`ed (directly or transitively) from a mod.zig aggregator or a
//!    build root registered in build.zig / src/build/modules.zig. This layer
//!    produces the friendliest error messages for the common mistake of adding
//!    a test file without wiring it up, but it cannot prove the tests run:
//!    Zig's lazy test collection means a cross-module
//!    `refAllDecls(@import("module"))` collects nothing, `refAllDecls(@This())`
//!    is one-level and pub-only, and an unreferenced import contributes
//!    nothing to the test binary.
//!
//! 2. A semantic check, which is the actual gate: build.zig passes the path of
//!    every host-runnable Zig test binary as a command-line argument. Each
//!    binary is run with `--listen=-` and asked for its full test list via the
//!    std.zig.Server `query_test_metadata` message (this only queries
//!    metadata; no tests execute). Every test decl found in src/ must appear
//!    in at least one binary's list, otherwise it can never run and the check
//!    fails.
//!
//! The semantic layer attributes each reported test to the file it was
//! declared in rather than comparing bare names, so two files that declare the
//! same test name cannot stand in for one another. A binary reports every test
//! as "<namespace>.test.<name>", and the namespace identifies the file; see
//! `namespaceBelongsToFile`. Unnamed `test { ... }` blocks carry no name at
//! all (they are reported as "<namespace>.test_N"), so they are checked per
//! file: a file holding one must show up under a namespace of its own.

const std = @import("std");

const Allocator = std.mem.Allocator;
const Ast = std.zig.Ast;
const PathList = std.ArrayList([]u8);

const max_file_bytes: usize = 16 * 1024 * 1024;

/// Files under src/ whose test decls intentionally never run in any test
/// binary. Every entry must have a comment justifying why the tests are dead.
/// (Vendored code lives under vendor/, which is not walked at all, so it never
/// needs an entry here.)
const test_file_exclusions = [_][]const u8{};

/// Files whose named tests are exempt from the semantic must-run gate; they
/// are still covered by the import-level check. Every entry must have a
/// comment justifying the exemption. Note that tests which merely skip
/// themselves at runtime (returning error.SkipZigTest) still appear in the
/// binary's test list, so they never need an entry here; only tests that are
/// comptime-excluded from every binary built on some host do.
const semantic_test_exclusions = [_][]const u8{
    // The sljmp implementation file is comptime-selected by builtin.os.tag in
    // src/sljmp/mod.zig, so a host's test binaries only ever contain the
    // implementation tests for the host's own OS; each of the other files'
    // tests run on that OS's CI instead.
    "src/sljmp/linux.zig",
    "src/sljmp/unix.zig",
    "src/sljmp/windows.zig",
    "src/sljmp/windows_aarch64.zig",
};

/// A named test declaration found in a source file under src/.
const SourceTest = struct {
    file: []const u8,
    /// Decoded name: string-literal escapes are already resolved, so this
    /// compares byte-for-byte against names reported by test binaries.
    name: []const u8,
    kind: Kind,

    const Kind = enum {
        /// `test "name" { ... }`; runs as "<namespace>.test.<name>".
        named,
        /// `test decl { ... }`; runs as "<namespace>.decltest.<decl>".
        doctest,
    };
};

/// Every namespace some binary reported one test name under.
const NamespaceList = std.ArrayList([]const u8);

/// Maps a test name to the namespaces it was reported under, which is what
/// lets a source test be checked against the file it was declared in.
const NameIndex = struct {
    arena: Allocator,
    map: std.StringHashMap(NamespaceList),

    fn init(arena: Allocator) NameIndex {
        return .{ .arena = arena, .map = std.StringHashMap(NamespaceList).init(arena) };
    }

    fn record(self: *NameIndex, name: []const u8, namespace: []const u8) !void {
        const entry = try self.map.getOrPut(name);
        if (!entry.found_existing) entry.value_ptr.* = .empty;
        for (entry.value_ptr.items) |seen| {
            if (std.mem.eql(u8, seen, namespace)) return;
        }
        try entry.value_ptr.append(self.arena, namespace);
    }

    /// Whether `name` runs under a namespace belonging to `file`.
    fn runsInFile(self: NameIndex, name: []const u8, file: []const u8) bool {
        const namespaces = self.map.get(name) orelse return false;
        for (namespaces.items) |namespace| {
            if (namespaceBelongsToFile(namespace, file)) return true;
        }
        return false;
    }
};

/// Everything the walk over src/ gathers.
const Collected = struct {
    /// Files with test decls that the import-level layer must find wiring for.
    /// Paths are owned by the general-purpose allocator.
    test_files: PathList,
    /// Aggregators whose imports count as wiring. Paths are owned by the
    /// general-purpose allocator.
    mod_files: PathList,
    /// Named test decls; strings live in the arena.
    source_tests: std.ArrayList(SourceTest),
    /// Files declaring at least one unnamed `test { ... }` block; strings live
    /// in the arena.
    unnamed_test_files: std.ArrayList([]const u8),
    /// How many files hold a test decl of any kind, aggregators included.
    files_with_tests: usize,
};

const TermColor = struct {
    pub const red = "\x1b[0;31m";
    pub const green = "\x1b[0;32m";
    pub const yellow = "\x1b[1;33m";
    pub const reset = "\x1b[0m";
};

pub fn main(init: std.process.Init) !void {
    const io = init.io;
    // This tool stays standalone (no build_options wiring), so unlike the
    // first-party DebugAllocators behind -Ddebug-gpa-traces it keeps std's
    // default allocation-site traces; it allocates too little to matter.
    var gpa_impl = std.heap.DebugAllocator(.{}){};
    defer _ = gpa_impl.deinit();
    const gpa = gpa_impl.allocator();

    // The semantic phase builds large short-lived string tables; batch-free
    // them through an arena instead of tracking every allocation.
    var arena_impl = std.heap.ArenaAllocator.init(gpa);
    defer arena_impl.deinit();
    const arena = arena_impl.allocator();

    const raw_args = try init.minimal.args.toSlice(arena);
    const test_binaries: []const []const u8 = @ptrCast(raw_args[1..]);

    var stdout_buffer: [4096]u8 = undefined;
    var stdout_state = std.Io.File.stdout().writer(io, &stdout_buffer);
    const stdout = &stdout_state.interface;

    try stdout.print("Checking test wiring in src/ directory...\n\n", .{});

    try stdout.print("Step 1: Finding all test declarations in source files...\n", .{});
    var collected: Collected = .{
        .test_files = .empty,
        .mod_files = .empty,
        .source_tests = .empty,
        .unnamed_test_files = .empty,
        .files_with_tests = 0,
    };
    defer freePathList(&collected.test_files, gpa);
    defer freePathList(&collected.mod_files, gpa);

    try walkTree(gpa, arena, io, "src", &collected);
    try stdout.print(
        "Found {d} named test decl(s) and {d} unnamed test block file(s) across {d} file(s)\n\n",
        .{
            collected.source_tests.items.len,
            collected.unnamed_test_files.items.len,
            collected.files_with_tests,
        },
    );

    // Some tests are wired through build.zig rather than mod.zig files.
    // For example, the CLI tests are driven via src/cli/main.zig and
    // src/cli/test/roc_subcommands.zig test roots.
    //
    // To avoid false positives, we:
    // - Treat src/cli/main.zig as an additional aggregator when scanning @import()
    //   statements for wired test files.
    // - Treat src/cli/test/fx_platform_test.zig as an aggregator since it imports
    //   fx_test_specs.zig which contains shared test specifications.
    if (fileExists(io, "src/cli/main.zig")) {
        try collected.mod_files.append(gpa, try gpa.dupe(u8, "src/cli/main.zig"));
    }
    if (fileExists(io, "src/cli/test/fx_platform_test.zig")) {
        try collected.mod_files.append(gpa, try gpa.dupe(u8, "src/cli/test/fx_platform_test.zig"));
    }
    if (fileExists(io, "src/cli/test/test_runner.zig")) {
        try collected.mod_files.append(gpa, try gpa.dupe(u8, "src/cli/test/test_runner.zig"));
    }
    if (fileExists(io, "src/cli/cli_error.zig")) {
        try collected.mod_files.append(gpa, try gpa.dupe(u8, "src/cli/cli_error.zig"));
    }
    if (fileExists(io, "src/snapshot_tool/main.zig")) {
        try collected.mod_files.append(gpa, try gpa.dupe(u8, "src/snapshot_tool/main.zig"));
    }

    try stdout.print("Step 2: Extracting test references from mod.zig files and build roots...\n", .{});
    var referenced = std.StringHashMap(void).init(gpa);
    defer {
        var it = referenced.keyIterator();
        while (it.next()) |key| {
            gpa.free(@constCast(key.*));
        }
        referenced.deinit();
    }

    // Imports are followed transitively: aggregator roots frequently wire
    // test files through intermediate aggregators (e.g. src/lsp/unit_tests.zig
    // imports test/unit.zig, which imports the individual test files). This
    // makes the import-level layer generous—it only proves a file is
    // reachable, not that its tests run—which is fine because the semantic
    // layer below is the actual gate.
    var scan_queue: PathList = .empty;
    defer freePathList(&scan_queue, gpa);
    var scanned = std.StringHashMap(void).init(gpa);
    defer {
        var scanned_it = scanned.keyIterator();
        while (scanned_it.next()) |key| {
            gpa.free(@constCast(key.*));
        }
        scanned.deinit();
    }

    for (collected.mod_files.items) |mod_path| {
        try enqueueForScan(gpa, &scanned, &scan_queue, mod_path);
    }
    // Also treat build-registered Zig roots (build.zig + src/build/modules.zig)
    // as valid wiring for the corresponding files, and scan their imports as
    // aggregators.
    try markBuildRootsAsReferenced(gpa, io, &referenced, &scanned, &scan_queue);

    while (scan_queue.pop()) |scan_path| {
        defer gpa.free(scan_path);
        try collectFileImports(gpa, io, scan_path, &referenced, &scanned, &scan_queue);
    }

    try stdout.print(
        "Found {d} file references in mod.zig files and build roots\n\n",
        .{referenced.count()},
    );

    try stdout.print("Step 3: Checking that all test files are import-wired...\n\n", .{});
    var unwired: PathList = .empty;
    defer freePathList(&unwired, gpa);

    for (collected.test_files.items) |test_path| {
        const key: []const u8 = test_path;
        if (!referenced.contains(key)) {
            try unwired.append(gpa, try gpa.dupe(u8, key));
        }
    }

    var failed = false;

    if (unwired.items.len > 0) {
        failed = true;
        std.mem.sort([]u8, unwired.items, {}, lessThanPath);
        try stdout.print(
            "{s}[ERR]{s} Found {d} test file(s) that are NOT wired through mod.zig:\n\n",
            .{ TermColor.red, TermColor.reset, unwired.items.len },
        );

        for (unwired.items) |path| {
            const path_text: []const u8 = path;
            try stdout.print("  {s}[MISSING]{s} {s}\n", .{ TermColor.red, TermColor.reset, path_text });
            try printSuggestion(gpa, io, stdout, path_text);
            try stdout.print("\n", .{});
        }
    } else {
        try stdout.print("{s}[OK]{s} All test files are import-wired\n\n", .{ TermColor.green, TermColor.reset });
    }

    if (test_binaries.len == 0) {
        // build.zig omits the binary args when the configured target cannot
        // run on this host or when a --test-filter trimmed the test set; only
        // the import-level check is possible then.
        try stdout.print(
            "{s}[WARN]{s} No test binaries were supplied; skipping the semantic test enumeration.\n",
            .{ TermColor.yellow, TermColor.reset },
        );
    } else {
        try runSemanticCheck(arena, io, stdout, test_binaries, collected, &failed);
    }

    if (failed) {
        try stdout.print("{s}[ERR]{s} Test wiring issues found. Please fix the issues above.\n\n", .{
            TermColor.red,
            TermColor.reset,
        });
        try stdout.print("To fix:\n", .{});
        try stdout.print("1. Add missing std.testing.refAllDecls() calls to the appropriate mod.zig files\n", .{});
        try stdout.print("2. Ensure all modules with tests are listed in src/build/modules.zig test_configs\n", .{});
        try stdout.print("3. Remember that refAllDecls(@import(\"module\")) across module boundaries collects\n", .{});
        try stdout.print("   nothing; test decls only run when reachable inside the test binary's root module\n\n", .{});
        try stdout.flush();
        std.process.exit(1);
    }

    try stdout.flush();
}

/// The semantic gate: enumerate the tests each supplied binary actually
/// contains (via the std.zig.Server protocol) and require every source test
/// decl to appear, under a namespace of its own file, in at least one binary.
fn runSemanticCheck(
    arena: Allocator,
    io: std.Io,
    stdout: anytype,
    test_binaries: []const []const u8,
    collected: Collected,
    failed: *bool,
) !void {
    try stdout.print(
        "Step 4: Enumerating tests from {d} test binaries...\n",
        .{test_binaries.len},
    );
    // Every "<name>" tail that follows a ".test." / ".decltest." component in
    // some binary's fully-qualified test names, indexed against the namespaces
    // it appeared under. A source test passes if some binary reports it under
    // its own file's namespace (the same file is often compiled into several
    // binaries, and each binary names it relative to that binary's own root).
    var named = NameIndex.init(arena);
    var doctests = NameIndex.init(arena);
    // Namespaces that reported an unnamed `test { ... }` block.
    var unnamed_namespaces = std.StringHashMap(void).init(arena);

    var enumerated_total: u64 = 0;
    for (test_binaries) |bin_path| {
        const count = enumerateBinaryTests(
            arena,
            io,
            bin_path,
            &named,
            &doctests,
            &unnamed_namespaces,
        ) catch |err| {
            try stdout.print(
                "{s}[ERR]{s} Failed to enumerate tests from {s}: {t}\n",
                .{ TermColor.red, TermColor.reset, bin_path, err },
            );
            failed.* = true;
            continue;
        };
        enumerated_total += count;
    }
    try stdout.print("Enumerated {d} test(s) total\n\n", .{enumerated_total});

    try stdout.print("Step 5: Checking that every source test decl runs in some binary...\n\n", .{});
    var missing_count: usize = 0;
    for (collected.source_tests.items) |source_test| {
        if (isSemanticallyExcluded(source_test.file)) continue;
        const index = switch (source_test.kind) {
            .named => named,
            .doctest => doctests,
        };
        if (index.runsInFile(source_test.name, source_test.file)) continue;
        missing_count += 1;
        try stdout.print(
            "  {s}[NEVER RUNS]{s} {s}: test \"{s}\"\n",
            .{ TermColor.red, TermColor.reset, source_test.file, source_test.name },
        );
    }
    for (collected.unnamed_test_files.items) |file| {
        if (isSemanticallyExcluded(file)) continue;
        if (namespaceSetCoversFile(unnamed_namespaces, file)) continue;
        missing_count += 1;
        try stdout.print(
            "  {s}[NEVER RUNS]{s} {s}: unnamed test block\n",
            .{ TermColor.red, TermColor.reset, file },
        );
    }

    if (missing_count > 0) {
        failed.* = true;
        try stdout.print(
            "\n{s}[ERR]{s} {d} test decl(s) exist in source but run in no test binary\n\n",
            .{ TermColor.red, TermColor.reset, missing_count },
        );
    } else {
        try stdout.print(
            "{s}[OK]{s} All {d} named source test decls, and every file's unnamed test blocks, run in at least one test binary\n\n",
            .{ TermColor.green, TermColor.reset, collected.source_tests.items.len },
        );
    }
}

/// Whether any namespace in `set` belongs to `file`.
fn namespaceSetCoversFile(set: std.StringHashMap(void), file: []const u8) bool {
    var it = set.keyIterator();
    while (it.next()) |namespace| {
        if (namespaceBelongsToFile(namespace.*, file)) return true;
    }
    return false;
}

/// Runs one test binary with `--listen=-` and records every test name it
/// reports. Only metadata is queried; no tests are executed. Returns the
/// number of tests the binary contains.
fn enumerateBinaryTests(
    arena: Allocator,
    io: std.Io,
    bin_path: []const u8,
    named: *NameIndex,
    doctests: *NameIndex,
    unnamed_namespaces: *std.StringHashMap(void),
) !u32 {
    var child = try std.process.spawn(io, .{
        .argv = &.{ bin_path, "--listen=-" },
        .stdin = .pipe,
        .stdout = .pipe,
        .stderr = .inherit,
    });
    errdefer child.kill(io);

    try sendClientMessage(io, child.stdin.?, .query_test_metadata);

    var read_buffer: [8192]u8 = undefined;
    var stdout_reader: std.Io.File.Reader = .initStreaming(child.stdout.?, io, &read_buffer);
    const reader = &stdout_reader.interface;

    const tests_len = while (true) {
        const header = try reader.takeStruct(std.zig.Server.Message.Header, .little);
        if (header.tag != .test_metadata) {
            // e.g. the zig_version handshake the server sends on startup
            try reader.discardAll(header.bytes_len);
            continue;
        }
        const metadata = try reader.takeStruct(std.zig.Server.Message.TestMetadata, .little);
        const name_offsets = try arena.alloc(u32, metadata.tests_len);
        for (name_offsets) |*offset| {
            offset.* = try reader.takeInt(u32, .little);
        }
        // expected_panic_msg entries are irrelevant here
        try reader.discardAll(@sizeOf(u32) * metadata.tests_len);
        const string_bytes = try arena.alloc(u8, metadata.string_bytes_len);
        try reader.readSliceAll(string_bytes);

        for (name_offsets) |offset| {
            const name = std.mem.sliceTo(string_bytes[offset..], 0);
            try registerBinaryTestName(name, named, doctests, unnamed_namespaces);
        }
        break metadata.tests_len;
    };

    try sendClientMessage(io, child.stdin.?, .exit);
    child.stdin.?.close(io);
    child.stdin = null;
    _ = try child.wait(io);
    return tests_len;
}

fn sendClientMessage(io: std.Io, file: std.Io.File, tag: std.zig.Client.Message.Tag) !void {
    var message: [8]u8 = undefined;
    std.mem.writeInt(u32, message[0..4], @intFromEnum(tag), .little);
    std.mem.writeInt(u32, message[4..8], 0, .little);
    try file.writeStreamingAll(io, &message);
}

/// A fully-qualified test name looks like "<namespace>.test.<name>" (or
/// ".decltest.<decl name>" for doctests; unnamed blocks show up as
/// "<namespace>.test_N"). Index the tail after every marker occurrence, paired
/// with the namespace preceding it, so lookups don't have to guess how deep
/// the namespace path is: a test name can itself contain ".test.", and a
/// directory named `test` puts that component in the namespace too.
fn registerBinaryTestName(
    name: []const u8,
    named: *NameIndex,
    doctests: *NameIndex,
    unnamed_namespaces: *std.StringHashMap(void),
) !void {
    try recordMarkerTails(name, ".test.", named);
    try recordMarkerTails(name, ".decltest.", doctests);
    if (std.mem.startsWith(u8, name, "test.")) {
        try named.record(name["test.".len..], name[0..0]);
    }
    if (std.mem.startsWith(u8, name, "decltest.")) {
        try doctests.record(name["decltest.".len..], name[0..0]);
    }
    if (unnamedTestNamespace(name)) |namespace| {
        try unnamed_namespaces.put(namespace, {});
    }
}

fn recordMarkerTails(
    name: []const u8,
    comptime marker: []const u8,
    index: *NameIndex,
) !void {
    var search_index: usize = 0;
    while (std.mem.findPos(u8, name, search_index, marker)) |pos| {
        try index.record(name[pos + marker.len ..], name[0..pos]);
        search_index = pos + 1;
    }
}

/// The namespace of an unnamed `test { ... }` block, which the test runner
/// reports as "<namespace>.test_N" where N counts the unnamed blocks in that
/// container. Null when `name` has some other shape.
fn unnamedTestNamespace(name: []const u8) ?[]const u8 {
    const marker = ".test_";
    if (std.mem.findLast(u8, name, marker)) |pos| {
        if (!isDecimal(name[pos + marker.len ..])) return null;
        return name[0..pos];
    }
    const bare_marker = "test_";
    if (std.mem.startsWith(u8, name, bare_marker) and isDecimal(name[bare_marker.len..])) {
        return name[0..0];
    }
    return null;
}

fn isDecimal(text: []const u8) bool {
    if (text.len == 0) return false;
    for (text) |char| {
        if (!std.ascii.isDigit(char)) return false;
    }
    return true;
}

/// Whether a namespace reported by a test binary names `file`.
///
/// Zig names a test's container by the source file's path relative to the
/// directory holding the root source file of the binary it was compiled into,
/// with separators turned into dots and the `.zig` suffix dropped, followed by
/// the names of any containers the test is nested inside. Which part of a
/// file's path that leaves depends on where each binary is rooted. The same
/// src/types/store.zig is `store` in the types binary and `types.store` in one
/// rooted a directory higher, so a namespace names the file when it starts
/// with any trailing run of that file's own path components.
fn namespaceBelongsToFile(namespace: []const u8, file: []const u8) bool {
    // A namespace with nothing before the marker has no component to attribute
    // at all, so accept it for any file rather than report a test as dead.
    if (namespace.len == 0) return true;

    const stem = if (std.mem.endsWith(u8, file, ".zig"))
        file[0 .. file.len - ".zig".len]
    else
        file;

    var tail_start: usize = 0;
    while (true) {
        if (namespaceStartsWithPath(namespace, stem[tail_start..])) return true;
        const separator = std.mem.findScalarPos(u8, stem, tail_start, '/') orelse return false;
        tail_start = separator + 1;
    }
}

/// Whether every component of the path `tail` equals the namespace component
/// in the same position, i.e. the namespace begins with that whole path.
fn namespaceStartsWithPath(namespace: []const u8, tail: []const u8) bool {
    var namespace_components = std.mem.splitScalar(u8, namespace, '.');
    var tail_components = std.mem.splitScalar(u8, tail, '/');
    while (tail_components.next()) |tail_component| {
        const namespace_component = namespace_components.next() orelse return false;
        if (!std.mem.eql(u8, namespace_component, tail_component)) return false;
    }
    return true;
}

/// Normalize path separators to forward slashes for consistent cross-platform comparison.
/// This is important because:
/// 1. Zig @import paths always use forward slashes
/// 2. We need consistent path comparison between walked files and mod.zig imports
fn normalizePath(allocator: Allocator, path: []u8) ![]u8 {
    if (comptime @import("builtin").os.tag == .windows) {
        const normalized = try allocator.dupe(u8, path);
        for (normalized) |*c| {
            if (c.* == '\\') c.* = '/';
        }
        allocator.free(path);
        return normalized;
    }
    return path;
}

fn walkTree(
    allocator: Allocator,
    arena: Allocator,
    io: std.Io,
    dir_path: []const u8,
    collected: *Collected,
) !void {
    var dir = try std.Io.Dir.cwd().openDir(io, dir_path, .{ .iterate = true });
    defer dir.close(io);

    var it = dir.iterate();
    while (try it.next(io)) |entry| {
        if (entry.kind == .sym_link) continue;

        const joined_path = try std.fs.path.join(allocator, &.{ dir_path, entry.name });
        const next_path = try normalizePath(allocator, joined_path);

        if (entry.kind == .directory) {
            defer allocator.free(next_path);
            try walkTree(allocator, arena, io, next_path, collected);
        } else if (entry.kind == .file) {
            try handleFile(allocator, arena, io, next_path, entry.name, collected);
        } else {
            allocator.free(next_path);
        }
    }
}

fn handleFile(
    allocator: Allocator,
    arena: Allocator,
    std_io: std.Io,
    path: []u8,
    file_name: []const u8,
    collected: *Collected,
) !void {
    if (!std.mem.endsWith(u8, file_name, ".zig")) {
        allocator.free(path);
        return;
    }

    // A mod.zig is an aggregator whose imports wire other files up, so it is
    // never itself reported as unwired, but its own test decls still have to
    // run somewhere, so they are collected like any other file's.
    const is_aggregator = std.mem.eql(u8, file_name, "mod.zig");
    if (is_aggregator) {
        try collected.mod_files.append(allocator, path);
    } else if (shouldSkipTestFile(path)) {
        allocator.free(path);
        return;
    }

    const has_test_decl = try collectFileTests(allocator, arena, std_io, path, collected);
    if (has_test_decl) collected.files_with_tests += 1;

    // mod_files owns an aggregator's path.
    if (is_aggregator) return;

    if (has_test_decl) {
        try collected.test_files.append(allocator, path);
        return;
    }

    allocator.free(path);
}

fn shouldSkipTestFile(path: []const u8) bool {
    for (test_file_exclusions) |excluded| {
        if (std.mem.eql(u8, path, excluded)) return true;
    }
    return false;
}

fn isSemanticallyExcluded(path: []const u8) bool {
    for (semantic_test_exclusions) |excluded| {
        if (std.mem.eql(u8, path, excluded)) return true;
    }
    return false;
}

/// Parses one source file, appending each named test decl to
/// `collected.source_tests` and, if the file has any unnamed `test { ... }`
/// block, the file itself to `collected.unnamed_test_files` (both allocated in
/// `arena`, which must outlive the returned data). Returns whether the file
/// contains any test decl at all.
fn collectFileTests(
    allocator: Allocator,
    arena: Allocator,
    std_io: std.Io,
    path: []const u8,
    collected: *Collected,
) !bool {
    const source = try readSourceFile(allocator, std_io, path);
    defer allocator.free(source);
    var tree = try Ast.parse(allocator, source, .zig);
    defer tree.deinit(allocator);

    var file_copy: ?[]const u8 = null;
    var has_test_decl = false;
    var has_unnamed_test = false;
    for (0..tree.nodes.len) |node_index| {
        const node: Ast.Node.Index = @enumFromInt(node_index);
        if (tree.nodeTag(node) != .test_decl) continue;
        has_test_decl = true;

        const file = file_copy orelse blk: {
            const copy = try arena.dupe(u8, path);
            file_copy = copy;
            break :blk copy;
        };

        const opt_name_token, _ = tree.nodeData(node).opt_token_and_node;
        // Unnamed `test { ... }` blocks have no name to look up, so the file
        // that declares them is checked as a whole instead; one entry per file
        // is enough, because a file's test decls are collected all together or
        // not at all.
        const name_token = opt_name_token.unwrap() orelse {
            if (!has_unnamed_test) {
                has_unnamed_test = true;
                try collected.unnamed_test_files.append(arena, file);
            }
            continue;
        };

        const token_slice = tree.tokenSlice(name_token);
        const name_tag = tree.tokenTag(name_token);
        const decoded: []const u8, const kind: SourceTest.Kind = if (name_tag == .string_literal)
            // The name may contain escapes; decode so it matches the raw
            // bytes reported by test binaries.
            .{ try std.zig.string_literal.parseAlloc(arena, token_slice), .named }
        else if (name_tag == .identifier)
            // Doctest: `test declName { ... }`, where the identifier itself
            // may be @"..."-quoted.
            if (token_slice[0] == '@')
                .{ try std.zig.string_literal.parseAlloc(arena, token_slice[1..]), .doctest }
            else
                .{ try arena.dupe(u8, token_slice), .doctest }
        else
            unreachable;

        try collected.source_tests.append(arena, .{ .file = file, .name = decoded, .kind = kind });
    }

    return has_test_decl;
}

fn readSourceFile(allocator: Allocator, std_io: std.Io, path: []const u8) ![:0]u8 {
    return try std.Io.Dir.cwd().readFileAllocOptions(
        std_io,
        path,
        allocator,
        .limited(max_file_bytes),
        std.mem.Alignment.of(u8),
        0,
    );
}

/// Adds `path` to the import-scan queue unless it was already scheduled.
fn enqueueForScan(
    allocator: Allocator,
    scanned: *std.StringHashMap(void),
    scan_queue: *PathList,
    path: []const u8,
) !void {
    if (scanned.contains(path)) return;
    try scanned.put(try allocator.dupe(u8, path), {});
    try scan_queue.append(allocator, try allocator.dupe(u8, path));
}

/// Marks every .zig file import in `file_path` as referenced and queues it
/// for its own import scan, so wiring is followed transitively.
fn collectFileImports(
    allocator: Allocator,
    std_io: std.Io,
    file_path: []const u8,
    referenced: *std.StringHashMap(void),
    scanned: *std.StringHashMap(void),
    scan_queue: *PathList,
) !void {
    const source = readSourceFile(allocator, std_io, file_path) catch |err| switch (err) {
        error.AccessDenied,
        error.AntivirusInterference,
        error.BadPathName,
        error.Canceled,
        error.ConnectionResetByPeer,
        error.DeviceBusy,
        error.FileBusy,
        error.FileLocksUnsupported,
        error.FileTooBig,
        error.InputOutput,
        error.IsDir,
        error.LockViolation,
        error.NameTooLong,
        error.NetworkNotFound,
        error.NoDevice,
        error.NoSpaceLeft,
        error.NotDir,
        error.NotOpenForReading,
        error.OutOfMemory,
        error.PathAlreadyExists,
        error.PermissionDenied,
        error.PipeBusy,
        error.ProcessFdQuotaExceeded,
        error.ReadOnlyFileSystem,
        error.SocketUnconnected,
        error.StreamTooLong,
        error.SymLinkLoop,
        error.SystemFdQuotaExceeded,
        error.SystemResources,
        error.Unexpected,
        error.WouldBlock,
        => return err,
        // Imports can name generated files that don't exist in the source
        // tree (e.g. compiled_builtins.zig); they can't wire anything.
        error.FileNotFound => return,
    };
    defer allocator.free(source);

    var tree = try Ast.parse(allocator, source, .zig);
    defer tree.deinit(allocator);

    const tags = tree.tokens.items(.tag);
    var idx: usize = 0;
    while (idx < tree.tokens.len) : (idx += 1) {
        if (tags[idx] != .builtin) continue;
        const token_index = @as(Ast.TokenIndex, @intCast(idx));
        if (!std.mem.eql(u8, tree.tokenSlice(token_index), "@import")) continue;

        const import_path = try extractImportPath(allocator, &tree, idx) orelse continue;
        defer allocator.free(import_path);

        if (!std.mem.endsWith(u8, import_path, ".zig")) continue;

        const resolved = try resolveImportPath(allocator, file_path, import_path);
        try enqueueForScan(allocator, scanned, scan_queue, resolved);
        if (referenced.contains(resolved)) {
            allocator.free(resolved);
        } else {
            try referenced.put(resolved, {});
        }
    }
}

fn extractImportPath(
    allocator: Allocator,
    tree: *const Ast,
    builtin_token_index: usize,
) !?[]u8 {
    var cursor = builtin_token_index + 1;
    if (cursor >= tree.tokens.len) return null;
    if (tree.tokenTag(@intCast(cursor)) != .l_paren) return null;

    cursor += 1;
    if (cursor >= tree.tokens.len) return null;
    const str_token_index = @as(Ast.TokenIndex, @intCast(cursor));
    const tag = tree.tokenTag(str_token_index);
    if (tag != .string_literal) return null;

    const literal = tree.tokenSlice(str_token_index);
    if (literal.len < 2) return null;
    return try allocator.dupe(u8, literal[1 .. literal.len - 1]);
}

fn resolveImportPath(
    allocator: Allocator,
    mod_path: []const u8,
    import_path: []const u8,
) ![]u8 {
    const mod_dir = std.fs.path.dirname(mod_path) orelse ".";
    return std.fs.path.resolvePosix(allocator, &.{ mod_dir, import_path });
}

/// Mark files that are used as build roots as "wired".
///
/// In addition to mod.zig imports, some tests are hooked up via explicit
/// build roots. Any Zig file that is used as a
/// `root_source_file = b.path("...")` should not be reported as missing
/// wiring. These roots also act as aggregators for their own imports.
fn markBuildRootsAsReferenced(
    allocator: Allocator,
    std_io: std.Io,
    referenced: *std.StringHashMap(void),
    scanned: *std.StringHashMap(void),
    scan_queue: *PathList,
) !void {
    const build_sources = [_][]const u8{
        "build.zig",
        "src/build/modules.zig",
    };
    for (build_sources) |build_path| {
        if (!fileExists(std_io, build_path)) continue;
        try markBuildRootsFromFile(allocator, std_io, build_path, referenced, scanned, scan_queue);
    }
}

fn markBuildRootsFromFile(
    allocator: Allocator,
    std_io: std.Io,
    build_path: []const u8,
    referenced: *std.StringHashMap(void),
    scanned: *std.StringHashMap(void),
    scan_queue: *PathList,
) !void {
    const source = try readSourceFile(allocator, std_io, build_path);
    defer allocator.free(source);

    const pattern = ".root_source_file = b.path(\"";
    var search_index: usize = 0;

    while (std.mem.findPos(u8, source, search_index, pattern)) |match_pos| {
        const literal_start = match_pos + pattern.len;
        const literal_end = std.mem.findScalarPos(u8, source, literal_start, '"') orelse break;
        const rel_path = source[literal_start..literal_end];
        search_index = literal_end + 1;

        if (!std.mem.endsWith(u8, rel_path, ".zig")) continue;
        if (!std.mem.startsWith(u8, rel_path, "src/")) continue;
        if (!fileExists(std_io, rel_path)) continue;

        try markReferenced(allocator, referenced, rel_path);
        try enqueueForScan(allocator, scanned, scan_queue, rel_path);
    }
}

fn markReferenced(
    allocator: Allocator,
    referenced: *std.StringHashMap(void),
    path: []const u8,
) !void {
    const key = try allocator.dupe(u8, path);
    if (referenced.contains(key)) {
        allocator.free(key);
    } else {
        try referenced.put(key, {});
    }
}

fn lessThanPath(_: void, lhs: []u8, rhs: []u8) bool {
    const l: []const u8 = lhs;
    const r: []const u8 = rhs;
    return std.mem.lessThan(u8, l, r);
}

fn printSuggestion(
    allocator: Allocator,
    std_io: std.Io,
    writer: anytype,
    test_path: []const u8,
) !void {
    const maybe_mod = try findNearestMod(allocator, std_io, test_path);
    if (maybe_mod) |mod_path| {
        defer allocator.free(mod_path);

        const mod_dir = std.fs.path.dirname(mod_path) orelse ".";
        const relative = try std.fs.path.relativePosix(allocator, ".", mod_dir, test_path);
        defer allocator.free(relative);

        try writer.print("    {s}[HINT]{s} Should be added to {s}\n", .{
            TermColor.yellow,
            TermColor.reset,
            mod_path,
        });
        try writer.print(
            "    {s}[HINT]{s} Add: std.testing.refAllDecls(@import(\"{s}\"));\n",
            .{ TermColor.yellow, TermColor.reset, relative },
        );
    } else {
        try writer.print(
            "    {s}[HINT]{s} No nearby mod.zig found for this test file\n",
            .{ TermColor.yellow, TermColor.reset },
        );
    }
}

fn findNearestMod(allocator: Allocator, std_io: std.Io, file_path: []const u8) !?[]u8 {
    var current_dir_opt = std.fs.path.dirname(file_path);
    while (current_dir_opt) |current_dir| {
        const joined = try std.fs.path.join(allocator, &.{ current_dir, "mod.zig" });
        const candidate = try normalizePath(allocator, joined);
        if (fileExists(std_io, candidate)) {
            return candidate;
        }
        allocator.free(candidate);
        current_dir_opt = std.fs.path.dirname(current_dir);
    }
    return null;
}

fn fileExists(std_io: std.Io, path: []const u8) bool {
    _ = std.Io.Dir.cwd().statFile(std_io, path, .{}) catch return false;
    return true;
}

fn freePathList(list: *PathList, allocator: Allocator) void {
    for (list.items) |path| {
        allocator.free(path);
    }
    list.deinit(allocator);
}
