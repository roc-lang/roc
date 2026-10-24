//! Regression test for https://github.com/roc-lang/roc/issues/12407
//!
//! A checked document's diagnostics must reach the client under the URI the
//! client used for that document, even when the client spells the URI
//! differently from the server's own encoding of the resolved path (a
//! percent-encoded `+`, or a path through a symlink). The client keeps the
//! last publish it received for a URI, so the last publish for the client's
//! URI must carry the document's problems. Locations the server returns for an
//! open document use the client's URI too.

const std = @import("std");
const lsp = @import("lsp");
const SyntaxChecker = lsp.syntax.SyntaxChecker;
const Diagnostics = lsp.diagnostics;
const uri_util = lsp.uri;
const integration_spec = @import("integration_spec.zig");
const test_env = @import("integration_env.zig");

/// Diagnostics publishing specs for client URIs spelled differently from the server's.
pub const specs = [_]integration_spec.Spec{
    .{ .name = "issue 12407: diagnostics are published under a percent-encoded client URI", .run = diagnosticsReachPercentEncodedClientUri },
    .{ .name = "issue 12407: diagnostics are published under a client URI through a symlink", .run = diagnosticsReachSymlinkedClientUri },
    .{ .name = "issue 12407: a document open under two URIs gets diagnostics under both", .run = diagnosticsReachEveryClientUriOfADocument },
    .{ .name = "issue 12407: reopening a closed document republishes its diagnostics", .run = reopenedDocumentRepublishesDiagnostics },
    .{ .name = "issue 12407: goto definition into a document open through a symlink returns its client URI", .run = definitionReturnsSymlinkedClientUri },
};

const broken_source =
    \\Broken := [].{
    \\    greeting : Str
    \\    greeting = 42 + "hello"
    \\}
    \\
;

const PublishSets = struct {
    sets: []Diagnostics.PublishDiagnostics,

    fn deinit(self: PublishSets) void {
        for (self.sets) |*set| set.deinit(test_env.allocator);
        test_env.allocator.free(self.sets);
    }

    /// Requires the last publish for exactly `client_uri` to contain diagnostics.
    fn expectDiagnosticsFor(self: PublishSets, client_uri: []const u8) integration_spec.SpecError!void {
        var client_diagnostic_count: ?usize = null;
        for (self.sets) |set| {
            if (std.mem.eql(u8, set.uri, client_uri)) client_diagnostic_count = set.diagnostics.len;
        }

        const count = client_diagnostic_count orelse {
            self.print();
            std.debug.print("no publish for client URI {s}\n", .{client_uri});
            return error.TestUnexpectedResult;
        };
        if (count == 0) {
            self.print();
            std.debug.print("last publish for client URI {s} has 0 diagnostics\n", .{client_uri});
            return error.TestUnexpectedResult;
        }
    }

    fn print(self: PublishSets) void {
        for (self.sets) |set| {
            std.debug.print("\npublishDiagnostics {s} {d} diagnostics", .{ set.uri, set.diagnostics.len });
        }
        std.debug.print("\n", .{});
    }
};

fn checkDocument(checker: *SyntaxChecker, client_uri: []const u8, source: []const u8) integration_spec.SpecError!PublishSets {
    return .{ .sets = try checker.check(client_uri, source, null) };
}

/// Checks `client_uri` and requires the last publish for exactly that URI to
/// contain diagnostics.
fn expectDiagnosticsForClientUri(cache_root: []const u8, client_uri: []const u8) integration_spec.SpecError!void {
    var checker = SyntaxChecker.init(test_env.allocator, test_env.io, .{}, null);
    defer checker.deinit();
    test_env.configureChecker(&checker, cache_root);

    const publishes = try checkDocument(&checker, client_uri, broken_source);
    defer publishes.deinit();
    try publishes.expectDiagnosticsFor(client_uri);
}

/// A temporary directory holding `real/<file_name>` and a symlink `linked` to `real`.
const SymlinkedFixture = struct {
    tmp: test_env.TmpDir,
    tmp_path: [:0]u8,
    real_uri: []u8,
    linked_uri: []u8,

    fn init(fixture: *SymlinkedFixture, file_name: []const u8, source: []const u8) integration_spec.SpecError!void {
        const allocator = test_env.allocator;
        fixture.tmp = test_env.tmpDir(.{});
        errdefer fixture.tmp.cleanup();

        try fixture.tmp.dir.createDirPath(test_env.io, "real");
        const sub_path = try std.fmt.allocPrint(allocator, "real/{s}", .{file_name});
        defer allocator.free(sub_path);
        try fixture.tmp.dir.writeFile(test_env.io, .{ .sub_path = sub_path, .data = source });
        try fixture.tmp.dir.symLink(test_env.io, "real", "linked", .{ .is_directory = true });

        fixture.tmp_path = try fixture.tmp.dir.realPathFileAlloc(test_env.io, ".", allocator);
        errdefer allocator.free(fixture.tmp_path);
        const dir_uri = try uri_util.pathToUri(allocator, fixture.tmp_path);
        defer allocator.free(dir_uri);

        fixture.real_uri = try std.fmt.allocPrint(allocator, "{s}/real/{s}", .{ dir_uri, file_name });
        errdefer allocator.free(fixture.real_uri);
        fixture.linked_uri = try std.fmt.allocPrint(allocator, "{s}/linked/{s}", .{ dir_uri, file_name });
    }

    fn deinit(fixture: *SymlinkedFixture) void {
        const allocator = test_env.allocator;
        allocator.free(fixture.linked_uri);
        allocator.free(fixture.real_uri);
        allocator.free(fixture.tmp_path);
        fixture.tmp.cleanup();
    }
};

fn diagnosticsReachPercentEncodedClientUri() integration_spec.SpecError!void {
    const allocator = test_env.allocator;
    var tmp = test_env.tmpDir(.{});
    defer tmp.cleanup();

    try tmp.dir.createDirPath(test_env.io, "a+b");
    try tmp.dir.writeFile(test_env.io, .{ .sub_path = "a+b/Broken.roc", .data = broken_source });
    const tmp_path = try tmp.dir.realPathFileAlloc(test_env.io, ".", allocator);
    defer allocator.free(tmp_path);
    const dir_uri = try uri_util.pathToUri(allocator, tmp_path);
    defer allocator.free(dir_uri);

    // How VS Code spells the URI of `<tmp>/a+b/Broken.roc`.
    const client_uri = try std.fmt.allocPrint(allocator, "{s}/a%2Bb/Broken.roc", .{dir_uri});
    defer allocator.free(client_uri);

    try expectDiagnosticsForClientUri(tmp_path, client_uri);
}

fn diagnosticsReachSymlinkedClientUri() integration_spec.SpecError!void {
    var fixture: SymlinkedFixture = undefined;
    try fixture.init("Broken.roc", broken_source);
    defer fixture.deinit();

    // The editor opened the file through the symlinked directory.
    try expectDiagnosticsForClientUri(fixture.tmp_path, fixture.linked_uri);
}

fn diagnosticsReachEveryClientUriOfADocument() integration_spec.SpecError!void {
    var fixture: SymlinkedFixture = undefined;
    try fixture.init("Broken.roc", broken_source);
    defer fixture.deinit();

    var checker = SyntaxChecker.init(test_env.allocator, test_env.io, .{}, null);
    defer checker.deinit();
    test_env.configureChecker(&checker, fixture.tmp_path);

    const first = try checkDocument(&checker, fixture.real_uri, broken_source);
    defer first.deinit();
    try first.expectDiagnosticsFor(fixture.real_uri);

    // The same file opened again through the symlink, with the same text.
    const second = try checkDocument(&checker, fixture.linked_uri, broken_source);
    defer second.deinit();
    try second.expectDiagnosticsFor(fixture.linked_uri);
    try second.expectDiagnosticsFor(fixture.real_uri);
}

fn reopenedDocumentRepublishesDiagnostics() integration_spec.SpecError!void {
    var fixture: SymlinkedFixture = undefined;
    try fixture.init("Broken.roc", broken_source);
    defer fixture.deinit();

    var checker = SyntaxChecker.init(test_env.allocator, test_env.io, .{}, null);
    defer checker.deinit();
    test_env.configureChecker(&checker, fixture.tmp_path);

    const opened = try checkDocument(&checker, fixture.linked_uri, broken_source);
    defer opened.deinit();
    try opened.expectDiagnosticsFor(fixture.linked_uri);

    checker.closeDocument(fixture.linked_uri);

    const reopened = try checkDocument(&checker, fixture.linked_uri, broken_source);
    defer reopened.deinit();
    try reopened.expectDiagnosticsFor(fixture.linked_uri);
}

const helper_source =
    \\Helper :: [].{
    \\    shout : Str -> Str
    \\    shout = |text| text
    \\}
    \\
;

fn definitionReturnsSymlinkedClientUri() integration_spec.SpecError!void {
    const allocator = test_env.allocator;
    var fixture: SymlinkedFixture = undefined;
    try fixture.init("Helper.roc", helper_source);
    defer fixture.deinit();

    // `real/` sits one directory below the fixture root, so the platform is one
    // more `../` away than from a `TmpDir` root.
    const main_source = try std.fmt.allocPrint(allocator,
        \\app [process_string] {{ pf: platform "../{s}" }}
        \\
        \\import Helper exposing [shout]
        \\
        \\process_string : Str -> Str
        \\process_string = |text| shout(text)
        \\
    , .{test_env.tmp_dir_platform_path});
    defer allocator.free(main_source);
    try fixture.tmp.dir.writeFile(test_env.io, .{ .sub_path = "real/main.roc", .data = main_source });
    const main_path = try fixture.tmp.dir.realPathFileAlloc(test_env.io, "real/main.roc", allocator);
    defer allocator.free(main_path);
    const main_uri = try uri_util.pathToUri(allocator, main_path);
    defer allocator.free(main_uri);

    var checker = SyntaxChecker.init(allocator, test_env.io, .{}, null);
    defer checker.deinit();
    test_env.configureChecker(&checker, fixture.tmp_path);

    // The editor has `Helper.roc` open through the symlinked directory.
    const helper_publishes = try checkDocument(&checker, fixture.linked_uri, helper_source);
    defer helper_publishes.deinit();

    const main_publishes = try checkDocument(&checker, main_uri, main_source);
    defer main_publishes.deinit();
    for (main_publishes.sets) |set| {
        if (set.diagnostics.len == 0) continue;
        main_publishes.print();
        return error.TestUnexpectedResult;
    }

    // `shout` in `import Helper exposing [shout]`.
    var result = try checker.getDefinitionAtPosition(main_uri, main_source, 2, 25) orelse {
        std.debug.print("\ngoto definition returned null\n", .{});
        return error.TestUnexpectedResult;
    };
    defer result.deinit(allocator);
    try std.testing.expectEqualStrings(fixture.linked_uri, result.uri);
}
