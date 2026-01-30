//! Tests for the LSP syntax checker integration.

const std = @import("std");
const SyntaxChecker = @import("../syntax.zig").SyntaxChecker;
const uri_util = @import("../uri.zig");
const completion_handler = @import("../handlers/completion.zig");
const CompletionItem = completion_handler.CompletionItem;
const completion_context = @import("../completion/context.zig");

fn platformPath(allocator: std.mem.Allocator) ![]u8 {
    // Resolve from repo root to ensure absolute path
    const repo_root = try std.fs.cwd().realpathAlloc(allocator, ".");
    defer allocator.free(repo_root);
    const path = try std.fs.path.join(allocator, &.{ repo_root, "test", "str", "platform", "main.roc" });
    // Convert backslashes to forward slashes for cross-platform Roc source compatibility
    // Roc interprets backslashes as escape sequences in string literals
    for (path) |*c| {
        if (c.* == '\\') c.* = '/';
    }
    return path;
}

// =========================================================================
// Test Harness
// =========================================================================

/// Shared setup for syntax checker tests. Manages allocator, SyntaxChecker,
/// temporary directory, platform path, and file URI — eliminating the
/// boilerplate that every test otherwise repeats.
const TestHarness = struct {
    allocator: std.mem.Allocator,
    checker: SyntaxChecker,
    tmp: std.testing.TmpDir,
    platform_path: []u8,
    file_path: ?[]u8 = null,
    uri: ?[]u8 = null,

    fn init() !TestHarness {
        const allocator = std.testing.allocator;
        return .{
            .allocator = allocator,
            .checker = SyntaxChecker.init(allocator, .{}, null),
            .tmp = std.testing.tmpDir(.{}),
            .platform_path = try platformPath(allocator),
        };
    }

    fn deinit(self: *TestHarness) void {
        if (self.uri) |u| self.allocator.free(u);
        if (self.file_path) |f| self.allocator.free(f);
        self.allocator.free(self.platform_path);
        self.tmp.cleanup();
        self.checker.deinit();
    }

    /// Format a Roc source template, substituting the platform path for `{s}`.
    fn formatSource(self: *TestHarness, comptime fmt: []const u8) ![]u8 {
        return std.fmt.allocPrint(self.allocator, fmt, .{self.platform_path});
    }

    /// Write a file to the tmp directory and register its path and URI.
    fn writeFile(self: *TestHarness, filename: []const u8, data: []const u8) !void {
        try self.tmp.dir.writeFile(.{ .sub_path = filename, .data = data });
        if (self.file_path) |f| self.allocator.free(f);
        if (self.uri) |u| self.allocator.free(u);
        self.file_path = try self.tmp.dir.realpathAlloc(self.allocator, filename);
        self.uri = try uri_util.pathToUri(self.allocator, self.file_path.?);
    }

    /// Build the file to populate the snapshot env. Discards diagnostics.
    fn check(self: *TestHarness, override_text: ?[]const u8) !void {
        const publish_sets = try self.checker.check(self.uri.?, override_text, null);
        for (publish_sets) |*set| set.deinit(self.allocator);
        self.allocator.free(publish_sets);
    }

    /// Get completion items at a line/character position.
    /// Fails the test if no completions are returned.
    fn getCompletions(self: *TestHarness, source: []const u8, line: u32, character: u32) ![]const CompletionItem {
        const result = try self.checker.getCompletionsAtPosition(self.uri.?, source, line, character);
        if (result) |r| return r.items;
        return error.TestUnexpectedResult;
    }

    /// Free completion items returned by `getCompletions`.
    fn freeCompletions(self: *TestHarness, items: []const CompletionItem) void {
        for (items) |item| {
            if (item.detail) |d| self.allocator.free(d);
            if (item.documentation) |doc| self.allocator.free(doc);
            if (item.sortText) |s| self.allocator.free(s);
            if (item.insertText) |t| self.allocator.free(t);
        }
        self.allocator.free(items);
    }

    /// Assert that every label in `expected` appears in `items`.
    fn expectHasLabels(items: []const CompletionItem, expected: []const []const u8) !void {
        for (expected) |label| {
            var found = false;
            for (items) |item| {
                if (std.mem.eql(u8, item.label, label)) {
                    found = true;
                    break;
                }
            }
            if (!found) {
                std.debug.print("Expected label '{s}' not found. Available:\n", .{label});
                for (items) |item| {
                    std.debug.print("  - {s}\n", .{item.label});
                }
                return error.TestUnexpectedResult;
            }
        }
    }
};

// =========================================================================
// Syntax Checker Tests
// =========================================================================

test "syntax checker skips rebuild when content unchanged" {
    var h = try TestHarness.init();
    defer h.deinit();
    h.checker.cache_config.enabled = false;

    const contents = try h.formatSource(
        \\app [main] {{ pf: platform "{s}" }}
        \\
        \\main = "hello"
        \\
    );
    defer h.allocator.free(contents);

    try h.writeFile("test.roc", contents);

    const result1 = try h.checker.check(h.uri.?, contents, null);
    defer {
        for (result1) |*set| set.deinit(h.allocator);
        h.allocator.free(result1);
    }

    const hash1 = h.checker.dependency_graph.getContentHash(h.file_path.?);
    try std.testing.expect(hash1 != null);

    // Same content should skip rebuild (returns empty)
    const result2 = try h.checker.check(h.uri.?, contents, null);
    defer {
        for (result2) |*set| set.deinit(h.allocator);
        h.allocator.free(result2);
    }

    try std.testing.expectEqual(@as(usize, 0), result2.len);
    const hash2 = h.checker.dependency_graph.getContentHash(h.file_path.?);
    try std.testing.expect(hash2 != null);
    try std.testing.expectEqualSlices(u8, &hash1.?, &hash2.?);
}

test "syntax checker rebuilds when content changes" {
    var h = try TestHarness.init();
    defer h.deinit();
    h.checker.cache_config.enabled = false;

    const contents1 = try h.formatSource(
        \\app [main] {{ pf: platform "{s}" }}
        \\
        \\main = "hello"
        \\
    );
    defer h.allocator.free(contents1);

    const contents2 = try h.formatSource(
        \\app [main] {{ pf: platform "{s}" }}
        \\
        \\main = "world"
        \\
    );
    defer h.allocator.free(contents2);

    try h.writeFile("test2.roc", contents1);

    const result1 = try h.checker.check(h.uri.?, contents1, null);
    defer {
        for (result1) |*set| set.deinit(h.allocator);
        h.allocator.free(result1);
    }

    const hash1 = h.checker.dependency_graph.getContentHash(h.file_path.?);
    try std.testing.expect(hash1 != null);

    const result2 = try h.checker.check(h.uri.?, contents2, null);
    defer {
        for (result2) |*set| set.deinit(h.allocator);
        h.allocator.free(result2);
    }

    const hash2 = h.checker.dependency_graph.getContentHash(h.file_path.?);
    try std.testing.expect(hash2 != null);
    try std.testing.expect(!std.mem.eql(u8, &hash1.?, &hash2.?));
}

test "syntax checker reports diagnostics for invalid source" {
    var h = try TestHarness.init();
    defer h.deinit();
    h.checker.cache_config.enabled = false;

    const contents = try h.formatSource(
        \\app [main] {{ pf: platform "{s}" }}
        \\
        \\main =
        \\
    );
    defer h.allocator.free(contents);

    try h.writeFile("bad.roc", contents);

    const publish_sets = try h.checker.check(h.uri.?, null, null);
    defer {
        for (publish_sets) |*set| set.deinit(h.allocator);
        h.allocator.free(publish_sets);
    }

    try std.testing.expect(publish_sets.len > 0);
    var total_diags: usize = 0;
    for (publish_sets) |set| {
        total_diags += set.diagnostics.len;
    }
    try std.testing.expect(total_diags > 0);
}

test "getDocumentSymbols returns symbols for valid app file" {
    var h = try TestHarness.init();
    defer h.deinit();
    h.checker.cache_config.enabled = false;

    const contents = try h.formatSource(
        \\app [main, helper] {{ pf: platform "{s}" }}
        \\
        \\helper = "test"
        \\
        \\main = helper
        \\
    );
    defer h.allocator.free(contents);

    try h.writeFile("symbols.roc", contents);

    const symbols = try h.checker.getDocumentSymbols(h.allocator, h.uri.?, contents);
    defer {
        for (symbols) |*sym| {
            h.allocator.free(sym.name);
        }
        h.allocator.free(symbols);
    }

    try std.testing.expectEqual(@as(usize, 2), symbols.len);
    var found_main = false;
    var found_helper = false;
    for (symbols) |sym| {
        if (std.mem.eql(u8, sym.name, "main")) found_main = true;
        if (std.mem.eql(u8, sym.name, "helper")) found_helper = true;
    }
    try std.testing.expect(found_main);
    try std.testing.expect(found_helper);
}

// =========================================================================
// Completion Context Detection Tests
// =========================================================================

test "completion context detects after_record_dot for lowercase identifier" {
    const source = "main = my_var.";
    const context = completion_context.detectCompletionContext(source, 0, 14);
    switch (context) {
        .after_value_dot => |access| {
            try std.testing.expectEqualStrings("my_var", access.access_chain);
        },
        else => return error.TestUnexpectedResult,
    }
}

test "completion context detects after_module_dot for uppercase identifier" {
    const source = "main = Str.";
    const context = completion_context.detectCompletionContext(source, 0, 11);
    switch (context) {
        .after_module_dot => |module_name| {
            try std.testing.expectEqualStrings("Str", module_name);
        },
        else => return error.TestUnexpectedResult,
    }
}

test "completion context detects after_receiver_dot for chained call" {
    const source = "main = val.func().";
    const context = completion_context.detectCompletionContext(source, 0, 18);
    switch (context) {
        .after_receiver_dot => |info| {
            try std.testing.expectEqual(@as(u32, 17), info.dot_offset);
        },
        else => return error.TestUnexpectedResult,
    }
}

test "completion context detects expression context" {
    const source = "main = ";
    const context = completion_context.detectCompletionContext(source, 0, 7);
    switch (context) {
        .expression => {},
        else => return error.TestUnexpectedResult,
    }
}

test "completion context detects after_colon for type annotation" {
    const source = "foo : ";
    const context = completion_context.detectCompletionContext(source, 0, 6);
    switch (context) {
        .after_colon => {},
        else => return error.TestUnexpectedResult,
    }
}

// =========================================================================
// Completion Tests
// =========================================================================

test "getCompletionsAtPosition returns basic completions" {
    var h = try TestHarness.init();
    defer h.deinit();

    const source = try h.formatSource(
        \\app [main] {{ pf: platform "{s}" }}
        \\
        \\my_value = "hello"
        \\
        \\main = my_
        \\
    );
    defer h.allocator.free(source);

    try h.writeFile("completion.roc", source);

    const items = try h.getCompletions(source, 4, 10);
    defer h.freeCompletions(items);

    try std.testing.expect(items.len > 0);
}

test "record field completion works for modules" {
    var h = try TestHarness.init();
    defer h.deinit();

    const clean = try h.formatSource(
        \\app [main, get_foo, get_bar] {{ pf: platform "{s}" }}
        \\
        \\MyRecord := {{ foo : Str, bar : I64 }}.{{}}
        \\my_record : MyRecord
        \\my_record = {{ foo: "hello", bar: 42 }}
        \\
        \\get_foo = my_record.foo
        \\get_bar = my_record.bar
        \\
    );
    defer h.allocator.free(clean);

    try h.writeFile("record_completion.roc", clean);
    try h.check(clean);

    const incomplete = try h.formatSource(
        \\app [main, get_foo] {{ pf: platform "{s}" }}
        \\
        \\MyRecord := {{ foo : Str, bar : I64 }}.{{}}
        \\my_record : MyRecord
        \\my_record = {{ foo: "hello", bar: 42 }}
        \\
        \\get_foo = my_record.
        \\
    );
    defer h.allocator.free(incomplete);

    // Line 6: "get_foo = my_record." — character 20 is right after the dot
    const items = try h.getCompletions(incomplete, 6, 20);
    defer h.freeCompletions(items);

    try TestHarness.expectHasLabels(items, &.{ "foo", "bar" });
}

test "record field completion in sub module" {
    var h = try TestHarness.init();
    defer h.deinit();

    const clean = try h.formatSource(
        \\app [main] {{ pf: platform "{s}" }}
        \\
        \\Basic := [Val(Str)].{{
        \\  rec = {{ foo: "hello", bar: 42 }}
        \\}}
        \\
        \\
    );
    defer h.allocator.free(clean);

    try h.writeFile("record_completion.roc", clean);
    try h.check(clean);

    const incomplete = try h.formatSource(
        \\app [main] {{ pf: platform "{s}" }}
        \\
        \\Basic := [Val(Str)].{{
        \\  rec = {{ foo: "hello", bar: 42 }}
        \\}}
        \\
        \\main = Basic.rec.
        \\
    );
    defer h.allocator.free(incomplete);

    // Line 6: "get_foo = my_record." — character 20 is right after the dot
    const items = try h.getCompletions(incomplete, 6, 17);
    defer h.freeCompletions(items);

    try TestHarness.expectHasLabels(items, &.{ "foo", "bar" });
}

test "record field completion works" {
    var h = try TestHarness.init();
    defer h.deinit();

    const clean = try h.formatSource(
        \\app [main, get_foo, get_bar] {{ pf: platform "{s}" }}
        \\
        \\my_record = {{ foo: "hello", bar: 42 }}
        \\
    );
    defer h.allocator.free(clean);

    try h.writeFile("record_completion.roc", clean);
    try h.check(clean);

    const incomplete = try h.formatSource(
        \\app [main, get_foo] {{ pf: platform "{s}" }}
        \\
        \\my_record = {{ foo: "hello", bar: 42 }}
        \\
        \\get_foo = my_record.
        \\
    );
    defer h.allocator.free(incomplete);

    // Line 4: "get_foo = my_record." — character 20 is right after the dot
    const items = try h.getCompletions(incomplete, 4, 20);
    defer h.freeCompletions(items);

    try TestHarness.expectHasLabels(items, &.{ "foo", "bar" });
}

test "record field completion with partial field name" {
    var h = try TestHarness.init();
    defer h.deinit();

    const clean = try h.formatSource(
        \\app [main, get_foo, get_bar] {{ pf: platform "{s}" }}
        \\
        \\MyRecord := {{ foo : Str, bar : I64 }}.{{}}
        \\my_record : MyRecord
        \\my_record = {{ foo: "hello", bar: 42 }}
        \\
        \\get_foo = my_record.foo
        \\get_bar = my_record.bar
        \\
    );
    defer h.allocator.free(clean);

    try h.writeFile("partial_field.roc", clean);
    try h.check(clean);

    const partial = try h.formatSource(
        \\app [main, get_foo] {{ pf: platform "{s}" }}
        \\
        \\MyRecord := {{ foo : Str, bar : I64 }}.{{}}
        \\my_record : MyRecord
        \\my_record = {{ foo: "hello", bar: 42 }}
        \\
        \\get_foo = my_record.fo
        \\
    );
    defer h.allocator.free(partial);

    // Both fields returned — client-side filtering narrows to "fo" prefix
    const items = try h.getCompletions(partial, 5, 23);
    defer h.freeCompletions(items);

    try TestHarness.expectHasLabels(items, &.{ "foo", "bar" });
}

test "static dispatch completion for nominal type methods" {
    var h = try TestHarness.init();
    defer h.deinit();

    const clean = try h.formatSource(
        \\app [main] {{ pf: platform "{s}" }}
        \\
        \\Basic := [Val(Str)].{{
        \\  to_str : Basic -> Str
        \\  to_str = |Basic.Val(s)| s
        \\}}
        \\
        \\val : Basic
        \\val = Basic.Val("hello")
        \\
        \\main = val.to_str()
        \\
    );
    defer h.allocator.free(clean);

    try h.writeFile("static_dispatch.roc", clean);
    try h.check(clean);

    const incomplete = try h.formatSource(
        \\app [main] {{ pf: platform "{s}" }}
        \\
        \\Basic := [Val(Str)].{{
        \\  to_str : Basic -> Str
        \\  to_str = |Basic.Val(s)| s
        \\}}
        \\
        \\val : Basic
        \\val = Basic.Val("hello")
        \\
        \\main = val.to_str()
        \\
        \\result = val.
        \\
    );
    defer h.allocator.free(incomplete);

    // Line 12: "result = val." — character 14 is right after the dot
    const items = try h.getCompletions(incomplete, 12, 14);
    defer h.freeCompletions(items);

    try TestHarness.expectHasLabels(items, &.{"to_str"});
}

test "static dispatch completion for chained call" {
    var h = try TestHarness.init();
    defer h.deinit();

    const clean = try h.formatSource(
        \\app [main] {{ pf: platform "{s}" }}
        \\
        \\Basic := [Val(Str)].{{
        \\  step : Basic -> Basic
        \\  step = |b| b
        \\}}
        \\
        \\val : Basic
        \\val = Basic.Val("hello")
        \\
        \\main = val.step()
        \\
    );
    defer h.allocator.free(clean);

    try h.writeFile("static_dispatch_chain.roc", clean);
    try h.check(clean);

    const incomplete = try h.formatSource(
        \\app [main] {{ pf: platform "{s}" }}
        \\
        \\Basic := [Val(Str)].{{
        \\  step : Basic -> Basic
        \\  step = |b| b
        \\}}
        \\
        \\val : Basic
        \\val = Basic.Val("hello")
        \\
        \\main = val.step()
        \\
        \\result = val.step().
        \\
    );
    defer h.allocator.free(incomplete);

    // Line 12: "result = val.step()." — character 21 is right after the dot
    const items = try h.getCompletions(incomplete, 12, 21);
    defer h.freeCompletions(items);

    try TestHarness.expectHasLabels(items, &.{"step"});
}
