//! Tests for the LSP syntax checker integration.

const std = @import("std");
const SyntaxChecker = @import("lsp").syntax.SyntaxChecker;
const uri_util = @import("lsp").uri;
const completion_handler = @import("lsp").handlers.completion;
const CompletionItem = completion_handler.CompletionItem;
const document_symbol_handler = @import("lsp").handlers.document_symbol;
const integration_spec = @import("integration_spec.zig");
const test_env = @import("integration_env.zig");

fn platformPath(allocator: std.mem.Allocator) integration_spec.SpecError![]u8 {
    // Resolve from repo root to ensure absolute path
    const repo_root = try std.Io.Dir.cwd().realPathFileAlloc(test_env.io, ".", allocator);
    defer allocator.free(repo_root);
    const path = try std.fs.path.join(allocator, &.{ repo_root, "test", "str", "platform", "main.roc" });
    // Convert backslashes to forward slashes for cross-platform Roc source compatibility
    // Roc interprets backslashes as escape sequences in string literals
    for (path) |*c| {
        if (c.* == '\\') c.* = '/';
    }
    return path;
}

// Test Harness

/// Shared setup for syntax checker tests. Manages allocator, SyntaxChecker,
/// temporary directory, platform path, and file URI — eliminating the
/// boilerplate that every test otherwise repeats.
const TestHarness = struct {
    allocator: std.mem.Allocator,
    checker: SyntaxChecker,
    tmp: test_env.TmpDir,
    platform_path: []u8,
    file_path: ?[:0]u8 = null,
    uri: ?[]u8 = null,

    fn init() integration_spec.SpecError!TestHarness {
        const allocator = test_env.allocator;
        var tmp = test_env.tmpDir(.{});
        errdefer tmp.cleanup();
        var checker = SyntaxChecker.init(allocator, test_env.io, .{}, null);
        errdefer checker.deinit();
        test_env.configureChecker(&checker, ".zig-cache/tmp");
        const platform_path = try platformPath(allocator);
        return .{
            .allocator = allocator,
            .checker = checker,
            .tmp = tmp,
            .platform_path = platform_path,
        };
    }

    fn deinit(self: *TestHarness) void {
        self.checker.deinit();
        if (self.uri) |u| self.allocator.free(u);
        if (self.file_path) |f| self.allocator.free(f);
        self.allocator.free(self.platform_path);
        self.tmp.cleanup();
    }

    /// Format a Roc source template, substituting the platform path for `{s}`.
    fn formatSource(self: *TestHarness, comptime fmt: []const u8) std.mem.Allocator.Error![]u8 {
        return std.fmt.allocPrint(self.allocator, fmt, .{self.platform_path});
    }

    /// Write a file to the tmp directory and register its path and URI.
    fn writeFile(self: *TestHarness, filename: []const u8, data: []const u8) integration_spec.SpecError!void {
        try self.tmp.dir.writeFile(test_env.io, .{ .sub_path = filename, .data = data });
        if (self.file_path) |f| self.allocator.free(f);
        if (self.uri) |u| self.allocator.free(u);
        self.file_path = try self.tmp.dir.realPathFileAlloc(test_env.io, filename, self.allocator);
        self.checker.cache_config.cache_dir = std.fs.path.dirname(self.file_path.?) orelse self.file_path.?;
        self.uri = try uri_util.pathToUri(self.allocator, self.file_path.?);
    }

    /// Build the file to populate the snapshot env. Discards diagnostics.
    fn check(self: *TestHarness, override_text: ?[]const u8) integration_spec.SpecError!void {
        const publish_sets = try self.checker.check(self.uri.?, override_text, null);
        for (publish_sets) |*set| set.deinit(self.allocator);
        self.allocator.free(publish_sets);
    }

    /// Get completion items at a line/character position.
    /// Fails the test if no completions are returned.
    fn getCompletions(self: *TestHarness, source: []const u8, line: u32, character: u32) integration_spec.SpecError![]const CompletionItem {
        const result = try self.checker.getCompletionsAtPosition(self.uri.?, source, line, character);
        if (result) |r| return r.items;
        return error.TestUnexpectedResult;
    }

    /// Get hover information at a line/character position.
    fn getHover(self: *TestHarness, source: []const u8, line: u32, character: u32) integration_spec.SpecError!?[]const u8 {
        const result = try self.checker.getTypeAtPosition(self.uri.?, source, line, character);
        if (result) |r| return r.type_str;
        return null;
    }

    /// Free completion items returned by `getCompletions`.
    fn freeCompletions(self: *TestHarness, items: []const CompletionItem) void {
        for (items) |item| {
            self.allocator.free(item.label);
            if (item.detail) |d| self.allocator.free(d);
            if (item.documentation) |doc| self.allocator.free(doc);
            if (item.sortText) |s| self.allocator.free(s);
            if (item.insertText) |t| self.allocator.free(t);
        }
        self.allocator.free(items);
    }

    /// Assert that every label in `expected` appears in `items`.
    fn expectHasLabels(items: []const CompletionItem, expected: []const []const u8) integration_spec.SpecError!void {
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

/// Syntax integration specs exported to the LSP harness.
pub const specs = [_]integration_spec.Spec{
    .{ .name = "syntax checker skips rebuild when content unchanged", .run = syntaxCheckerSkipsRebuildWhenContentUnchanged },
    .{ .name = "syntax checker rebuilds when content changes", .run = syntaxCheckerRebuildsWhenContentChanges },
    .{ .name = "syntax checker reports diagnostics for invalid source", .run = syntaxCheckerReportsDiagnosticsForInvalidSource },
    .{ .name = "getDocumentSymbols returns symbols for valid app file", .run = getDocumentSymbolsReturnsSymbolsForValidAppFile },
    .{ .name = "getDocumentSymbols returns type declarations for type-only module", .run = getDocumentSymbolsReturnsTypeDeclarationsForTypeOnlyModule },
    .{ .name = "getCompletionsAtPosition returns basic completions", .run = getCompletionsAtPositionReturnsBasicCompletions },
    .{ .name = "record field completion works for modules", .run = recordFieldCompletionWorksForModules },
    .{ .name = "record field completion in sub module", .run = recordFieldCompletionInSubModule },
    .{ .name = "record field completion works for nested nominal submodule", .run = recordFieldCompletionWorksForNestedNominalSubmodule },
    .{ .name = "record field completion works", .run = recordFieldCompletionWorks },
    .{ .name = "tuple index completion works", .run = tupleIndexCompletionWorks },
    .{ .name = "record field completion with partial field name", .run = recordFieldCompletionWithPartialFieldName },
    .{ .name = "static dispatch completion for nominal type methods", .run = staticDispatchCompletionForNominalTypeMethods },
    .{ .name = "static dispatch completion for chained call", .run = staticDispatchCompletionForChainedCall },
    .{ .name = "completion includes doc comments from source", .run = completionIncludesDocCommentsFromSource },
    .{ .name = "hover shows documentation for function definition", .run = hoverShowsDocumentationForFunctionDefinition },
    .{ .name = "hover shows documentation for local function call", .run = hoverShowsDocumentationForLocalFunctionCall },
    .{ .name = "hover shows documentation for external function call", .run = hoverShowsDocumentationForExternalFunctionCall },
    .{ .name = "hover shows documentation for function without type annotation", .run = hoverShowsDocumentationForFunctionWithoutTypeAnnotation },
    .{ .name = "hover shows documentation for local variable", .run = hoverShowsDocumentationForLocalVariable },
    .{ .name = "hover shows documentation for method call via static dispatch", .run = hoverShowsDocumentationForMethodCallViaStaticDispatch },
    .{ .name = "hover without documentation shows only type", .run = hoverWithoutDocumentationShowsOnlyType },
};

// Syntax Checker Tests

/// Verifies unchanged source content reuses the existing syntax-check result.
pub fn syntaxCheckerSkipsRebuildWhenContentUnchanged() integration_spec.SpecError!void {
    var h = try TestHarness.init();
    defer h.deinit();

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

/// Verifies changed source content updates the syntax checker content hash.
pub fn syntaxCheckerRebuildsWhenContentChanges() integration_spec.SpecError!void {
    var h = try TestHarness.init();
    defer h.deinit();

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

/// Verifies invalid source produces one or more LSP diagnostics.
pub fn syntaxCheckerReportsDiagnosticsForInvalidSource() integration_spec.SpecError!void {
    var h = try TestHarness.init();
    defer h.deinit();

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

/// Verifies document symbols are returned for a valid app file.
pub fn getDocumentSymbolsReturnsSymbolsForValidAppFile() integration_spec.SpecError!void {
    var h = try TestHarness.init();
    defer h.deinit();

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

/// Verifies document symbols include type declarations in modules without values.
pub fn getDocumentSymbolsReturnsTypeDeclarationsForTypeOnlyModule() integration_spec.SpecError!void {
    var h = try TestHarness.init();
    defer h.deinit();

    const contents =
        \\Color := [Red, Green]
        \\
        \\User : { name : Str }
        \\
    ;

    try h.writeFile("Color.roc", contents);

    const symbols = try h.checker.getDocumentSymbols(h.allocator, h.uri.?, contents);
    defer {
        for (symbols) |*sym| {
            h.allocator.free(sym.name);
        }
        h.allocator.free(symbols);
    }

    try std.testing.expectEqual(@as(usize, 2), symbols.len);
    var found_color = false;
    var found_user = false;
    for (symbols) |sym| {
        if (std.mem.eql(u8, sym.name, "Color")) {
            found_color = true;
            try std.testing.expectEqual(document_symbol_handler.SymbolKind.@"struct", sym.kind);
        }
        if (std.mem.eql(u8, sym.name, "User")) {
            found_user = true;
            try std.testing.expectEqual(document_symbol_handler.SymbolKind.class, sym.kind);
        }
    }
    try std.testing.expect(found_color);
    try std.testing.expect(found_user);
}

// Completion Tests

/// Verifies basic expression completions return at least one item.
pub fn getCompletionsAtPositionReturnsBasicCompletions() integration_spec.SpecError!void {
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

/// Verifies record fields complete for values with nominal record types.
pub fn recordFieldCompletionWorksForModules() integration_spec.SpecError!void {
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

/// Verifies record fields complete through a nominal submodule value.
pub fn recordFieldCompletionInSubModule() integration_spec.SpecError!void {
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

/// Verifies nested nominal submodule members complete after a dot.
pub fn recordFieldCompletionWorksForNestedNominalSubmodule() integration_spec.SpecError!void {
    var h = try TestHarness.init();
    defer h.deinit();

    const clean = try h.formatSource(
        \\app [main, test] {{ pf: platform "{s}" }}
        \\
        \\MyType := [MyTag(Str)].{{
        \\    t = 10
        \\    Sub := [SubTag].{{
        \\        ta = 10
        \\    }}
        \\}}
        \\
        \\test = MyType.Sub.ta
        \\main = "ok"
        \\
    );
    defer h.allocator.free(clean);

    try h.writeFile("nested_nominal_completion.roc", clean);
    try h.check(clean);

    const incomplete = try h.formatSource(
        \\app [main, test] {{ pf: platform "{s}" }}
        \\
        \\MyType := [MyTag(Str)].{{
        \\    t = 10
        \\    Sub := [SubTag].{{
        \\        ta = 10
        \\    }}
        \\}}
        \\
        \\test = MyType.Sub.
        \\main = "ok"
        \\
    );
    defer h.allocator.free(incomplete);

    // Line 9: "test = MyType.Sub." — character 18 is right after the dot.
    const items = try h.getCompletions(incomplete, 9, 18);
    defer h.freeCompletions(items);

    try TestHarness.expectHasLabels(items, &.{"ta"});
}

/// Verifies record fields complete for inferred record values.
pub fn recordFieldCompletionWorks() integration_spec.SpecError!void {
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

/// Verifies tuple index completions appear after a tuple dot.
pub fn tupleIndexCompletionWorks() integration_spec.SpecError!void {
    var h = try TestHarness.init();
    defer h.deinit();

    const clean = try h.formatSource(
        \\app [main, get_first] {{ pf: platform "{s}" }}
        \\
        \\my_tuple = ("hello", 42, Bool.true)
        \\
        \\get_first = my_tuple.0
        \\
    );
    defer h.allocator.free(clean);

    try h.writeFile("tuple_completion.roc", clean);
    try h.check(clean);

    const incomplete = try h.formatSource(
        \\app [main, get_first] {{ pf: platform "{s}" }}
        \\
        \\my_tuple = ("hello", 42, Bool.true)
        \\
        \\get_first = my_tuple.
        \\
    );
    defer h.allocator.free(incomplete);

    // Line 4: "get_first = my_tuple." — character 21 is right after the dot
    const items = try h.getCompletions(incomplete, 4, 21);
    defer h.freeCompletions(items);

    try TestHarness.expectHasLabels(items, &.{ "0", "1", "2" });
}

/// Verifies partial field names still expose record field completions.
pub fn recordFieldCompletionWithPartialFieldName() integration_spec.SpecError!void {
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
        \\get_foo = my_record.of
        \\
    );
    defer h.allocator.free(partial);

    // Both fields returned — client-side filtering narrows to "of" prefix
    const items = try h.getCompletions(partial, 5, 23);
    defer h.freeCompletions(items);

    try TestHarness.expectHasLabels(items, &.{ "foo", "bar" });
}

/// Verifies static-dispatch methods complete for nominal values.
pub fn staticDispatchCompletionForNominalTypeMethods() integration_spec.SpecError!void {
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

/// Verifies static-dispatch methods complete after chained method calls.
pub fn staticDispatchCompletionForChainedCall() integration_spec.SpecError!void {
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

// Doc Comment Tests

/// Verifies completion items include doc comments from Roc source.
pub fn completionIncludesDocCommentsFromSource() integration_spec.SpecError!void {
    var h = try TestHarness.init();
    defer h.deinit();

    // Create a file with a documented function
    const clean = try h.formatSource(
        \\app [main, add] {{ pf: platform "{s}" }}
        \\
        \\## Adds two numbers together.
        \\## Returns the sum.
        \\add : I64, I64 -> I64
        \\add = |a, b| a + b
        \\
        \\main = add(1, 2)
        \\
    );
    defer h.allocator.free(clean);

    try h.writeFile("doc_comment_completion.roc", clean);
    try h.check(clean);

    // Request completions at a position where "add" should appear
    const incomplete = try h.formatSource(
        \\app [main, add] {{ pf: platform "{s}" }}
        \\
        \\## Adds two numbers together.
        \\## Returns the sum.
        \\add : I64, I64 -> I64
        \\add = |a, b| a + b
        \\
        \\main = a
        \\
    );
    defer h.allocator.free(incomplete);

    // Line 7: "main = a" — character 8 is at the end after 'a'
    const items = try h.getCompletions(incomplete, 7, 8);
    defer h.freeCompletions(items);

    // Find the "add" completion and verify it has documentation
    var found_add = false;
    for (items) |item| {
        if (std.mem.eql(u8, item.label, "add")) {
            found_add = true;
            // The documentation should contain our doc comment
            if (item.documentation) |doc| {
                try std.testing.expect(std.mem.find(u8, doc, "Adds two numbers together") != null);
                try std.testing.expect(std.mem.find(u8, doc, "Returns the sum") != null);
            } else {
                // Documentation should be present
                std.debug.print("Expected documentation for 'add' but got null\n", .{});
                return error.TestUnexpectedResult;
            }
            break;
        }
    }

    if (!found_add) {
        std.debug.print("Expected 'add' in completion items\n", .{});
        return error.TestUnexpectedResult;
    }
}
// Hover Documentation Tests

/// Verifies hovering a function definition includes documentation and type text.
pub fn hoverShowsDocumentationForFunctionDefinition() integration_spec.SpecError!void {
    var h = try TestHarness.init();
    defer h.deinit();

    const source = try h.formatSource(
        \\app [main, add] {{ pf: platform "{s}" }}
        \\
        \\## Adds two numbers together.
        \\## Returns the sum.
        \\add : I64, I64 -> I64
        \\add = |a, b| a + b
        \\
        \\main = add(1, 2)
        \\
    );
    defer h.allocator.free(source);

    try h.writeFile("hover_def.roc", source);
    try h.check(source);

    // Hover on "add" in the definition line (line 5, character 0-2)
    const hover = try h.getHover(source, 5, 0);
    if (hover) |text| {
        defer h.allocator.free(text);
        // Should contain the doc comment
        try std.testing.expect(std.mem.find(u8, text, "Adds two numbers together") != null);
        try std.testing.expect(std.mem.find(u8, text, "Returns the sum") != null);
        // Should also contain the type signature
        try std.testing.expect(std.mem.find(u8, text, "I64, I64 -> I64") != null);
    } else {
        return error.TestUnexpectedResult;
    }
}

/// Verifies hovering a local function call shows documentation from its definition.
pub fn hoverShowsDocumentationForLocalFunctionCall() integration_spec.SpecError!void {
    var h = try TestHarness.init();
    defer h.deinit();

    const source = try h.formatSource(
        \\app [main] {{ pf: platform "{s}" }}
        \\
        \\## Multiplies two numbers.
        \\multiply : I64, I64 -> I64
        \\multiply = |a, b| a * b
        \\
        \\main = multiply(3, 4)
        \\
    );
    defer h.allocator.free(source);

    try h.writeFile("hover_call.roc", source);
    try h.check(source);

    // Hover on "multiply" in the call (line 6, character 7-15)
    const hover = try h.getHover(source, 6, 10);
    if (hover) |text| {
        defer h.allocator.free(text);
        // Should contain the doc comment from the definition
        try std.testing.expect(std.mem.find(u8, text, "Multiplies two numbers") != null);
        // Should contain the type signature
        try std.testing.expect(std.mem.find(u8, text, "I64, I64 -> I64") != null);
    } else {
        std.debug.print("\n=== HOVER RETURNED NULL ===\n", .{});
        return error.TestUnexpectedResult;
    }
}

/// Verifies hovering a builtin function call returns type information.
pub fn hoverShowsDocumentationForExternalFunctionCall() integration_spec.SpecError!void {
    var h = try TestHarness.init();
    defer h.deinit();

    const source = try h.formatSource(
        \\app [main] {{ pf: platform "{s}" }}
        \\
        \\main = Str.concat("hello", " world")
        \\
    );
    defer h.allocator.free(source);

    try h.writeFile("hover_external.roc", source);
    try h.check(source);

    // Hover on "concat" in Str.concat (line 2, character 11-17)
    const hover = try h.getHover(source, 2, 13);
    if (hover) |text| {
        defer h.allocator.free(text);
        // Should at least contain a type signature (documentation may or may not be available)
        try std.testing.expect(text.len > 0);
        try std.testing.expect(std.mem.find(u8, text, "Str") != null);
        // Note: We don't strictly check for documentation here as builtin docs
        // may not always be available, but the type should always be present
    } else {
        // Hover on builtin functions should work
        return error.TestUnexpectedResult;
    }
}

/// Verifies hovering an unannotated function still shows its documentation.
pub fn hoverShowsDocumentationForFunctionWithoutTypeAnnotation() integration_spec.SpecError!void {
    var h = try TestHarness.init();
    defer h.deinit();

    const source = try h.formatSource(
        \\app [main, helper] {{ pf: platform "{s}" }}
        \\
        \\## A simple helper function.
        \\helper = |x| x + 1
        \\
        \\main = helper(5)
        \\
    );
    defer h.allocator.free(source);

    try h.writeFile("hover_no_anno.roc", source);
    try h.check(source);

    // Hover on "helper" in the call
    const hover = try h.getHover(source, 5, 9);
    if (hover) |text| {
        defer h.allocator.free(text);
        // Should contain the doc comment
        try std.testing.expect(std.mem.find(u8, text, "A simple helper function") != null);
    } else {
        return error.TestUnexpectedResult;
    }
}

/// Verifies hovering a local value shows its source documentation.
pub fn hoverShowsDocumentationForLocalVariable() integration_spec.SpecError!void {
    var h = try TestHarness.init();
    defer h.deinit();

    const source = try h.formatSource(
        \\app [main] {{ pf: platform "{s}" }}
        \\
        \\## The magic number.
        \\number = 42
        \\
        \\main =
        \\    number + 1
        \\
    );
    defer h.allocator.free(source);

    try h.writeFile("hover_local.roc", source);
    try h.check(source);

    // Hover on "number" in the usage (line 6)
    const hover = try h.getHover(source, 6, 6);
    if (hover) |text| {
        defer h.allocator.free(text);
        // Should contain the doc comment
        try std.testing.expect(std.mem.find(u8, text, "The magic number") != null);
    } else {
        return error.TestUnexpectedResult;
    }
}

/// Verifies hovering a static-dispatch method call shows method documentation.
pub fn hoverShowsDocumentationForMethodCallViaStaticDispatch() integration_spec.SpecError!void {
    var h = try TestHarness.init();
    defer h.deinit();

    const source = try h.formatSource(
        \\app [main] {{ pf: platform "{s}" }}
        \\
        \\Basic := [Val(I64)].{{
        \\  ## Doubles the value.
        \\  double : Basic -> Basic
        \\  double = |Basic.Val(n)| Basic.Val(n * 2)
        \\}}
        \\
        \\val = Basic.Val(21)
        \\
        \\main = val.double()
        \\
    );
    defer h.allocator.free(source);

    try h.writeFile("hover_method.roc", source);
    try h.check(source);

    // Hover on "double" in the method call (line 10)
    const hover = try h.getHover(source, 10, 13);
    if (hover) |text| {
        defer h.allocator.free(text);
        // Should contain the doc comment from the method definition
        try std.testing.expect(std.mem.find(u8, text, "Doubles the value") != null);
    } else {
        return error.TestUnexpectedResult;
    }
}

/// Verifies hover text without documentation contains only type information.
pub fn hoverWithoutDocumentationShowsOnlyType() integration_spec.SpecError!void {
    var h = try TestHarness.init();
    defer h.deinit();

    const source = try h.formatSource(
        \\app [main, add] {{ pf: platform "{s}" }}
        \\
        \\add : I64, I64 -> I64
        \\add = |a, b| a + b
        \\
        \\main = add(1, 2)
        \\
    );
    defer h.allocator.free(source);

    try h.writeFile("hover_no_doc.roc", source);
    try h.check(source);

    // Hover on "add" in the call
    const hover = try h.getHover(source, 5, 9);
    if (hover) |text| {
        defer h.allocator.free(text);
        // Should contain the type but no doc text
        try std.testing.expect(std.mem.find(u8, text, "I64, I64 -> I64") != null);
        // Should not have doc comment-specific text from a previous line
        try std.testing.expect(std.mem.find(u8, text, "##") == null);
    } else {
        return error.TestUnexpectedResult;
    }
}
