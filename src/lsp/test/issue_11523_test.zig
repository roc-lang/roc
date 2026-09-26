//! Regression test for https://github.com/roc-lang/roc/issues/11523
//!
//! Checking replaces an erroneous expression with a runtime error, which cuts
//! the source subtree beneath it out of the checked tree. Hover, goto
//! definition, and rename on names inside that subtree must still answer from
//! the source code, and an unused statement value must not destroy the type of
//! the function it calls.

const std = @import("std");
const lsp = @import("lsp");
const Diagnostics = lsp.diagnostics;
const SyntaxChecker = lsp.syntax.SyntaxChecker;
const uri_util = lsp.uri;
const integration_spec = @import("integration_spec.zig");
const test_env = @import("integration_env.zig");

/// Hover, goto definition, and rename specs over erroneous source subtrees.
pub const specs = [_]integration_spec.Spec{
    .{ .name = "issue 11523: hover on a call-site function name shows the signature even when the call has a type error", .run = hoverOnCallSiteNameShowsSignatureDespiteTypeError },
    .{ .name = "issue 11523: hover on a name in an unused-value statement shows the signature", .run = hoverOnUnusedValueStatementNameShowsSignature },
    .{ .name = "issue 11523: goto definition resolves a call-site function name even when the call has a type error", .run = gotoDefinitionResolvesCallSiteNameDespiteTypeError },
    .{ .name = "issue 11523: hover and goto definition see the callee of a call retired for an erroneous argument", .run = calleeOfCallRetiredForErroneousArgument },
    .{ .name = "issue 11523: rename updates a name inside an unused-value statement", .run = renameUpdatesNameInsideUnusedValueStatement },
};

/// `greet!` is defined on line 3 and called as an unused statement value on
/// line 6, character 4.
const unused_value_source =
    \\app [main!] {{ pf: platform "{s}" }}
    \\
    \\greet! : Str => Try(Str, [Exit(I32)])
    \\greet! = |text| Ok(text)
    \\
    \\main! = |_| {{
    \\    greet!("hello")
    \\    Ok({{}})
    \\}}
    \\
;

/// One checked document whose check reported at least one problem.
const Fixture = struct {
    tmp: test_env.TmpDir,
    source: []u8,
    file_path: [:0]u8,
    file_uri: []u8,
    checker: SyntaxChecker,

    fn init(fixture: *Fixture, comptime template: []const u8, file_name: []const u8) integration_spec.SpecError!void {
        const allocator = test_env.allocator;
        fixture.tmp = test_env.tmpDir(.{});
        errdefer fixture.tmp.cleanup();

        fixture.source = try std.fmt.allocPrint(allocator, template, .{test_env.tmp_dir_platform_path});
        errdefer allocator.free(fixture.source);

        try fixture.tmp.dir.writeFile(test_env.io, .{ .sub_path = file_name, .data = fixture.source });
        fixture.file_path = try fixture.tmp.dir.realPathFileAlloc(test_env.io, file_name, allocator);
        errdefer allocator.free(fixture.file_path);
        fixture.file_uri = try uri_util.pathToUri(allocator, fixture.file_path);
        errdefer allocator.free(fixture.file_uri);

        fixture.checker = SyntaxChecker.init(allocator, test_env.io, .{}, null);
        errdefer fixture.checker.deinit();
        fixture.checker.cache_config.cache_dir = std.fs.path.dirname(fixture.file_path) orelse fixture.file_path;

        const publish_sets = try fixture.checker.check(fixture.file_uri, fixture.source, null);
        defer {
            for (publish_sets) |*set| set.deinit(allocator);
            allocator.free(publish_sets);
        }
        var problem_count: usize = 0;
        for (publish_sets) |set| problem_count += set.diagnostics.len;
        try std.testing.expect(problem_count > 0);
    }

    fn deinit(fixture: *Fixture) void {
        const allocator = test_env.allocator;
        fixture.checker.deinit();
        allocator.free(fixture.file_uri);
        allocator.free(fixture.file_path);
        allocator.free(fixture.source);
        fixture.tmp.cleanup();
    }

    fn expectHoverContains(fixture: *Fixture, line: u32, character: u32, expected: []const u8) integration_spec.SpecError!void {
        const result = try fixture.checker.getTypeAtPosition(fixture.file_uri, fixture.source, line, character) orelse {
            std.debug.print("\nhover returned null at {d}:{d}\n", .{ line, character });
            return error.TestUnexpectedResult;
        };
        defer test_env.allocator.free(result.type_str);
        if (std.mem.find(u8, result.type_str, "Error") != null or
            std.mem.find(u8, result.type_str, expected) == null)
        {
            std.debug.print("\nhover at {d}:{d} was {s}, expected it to contain {s}\n", .{ line, character, result.type_str, expected });
            return error.TestUnexpectedResult;
        }
    }

    fn expectDefinitionLine(fixture: *Fixture, line: u32, character: u32, definition_line: u32) integration_spec.SpecError!void {
        var result = try fixture.checker.getDefinitionAtPosition(fixture.file_uri, fixture.source, line, character) orelse {
            std.debug.print("\ngoto definition returned null at {d}:{d}\n", .{ line, character });
            return error.TestUnexpectedResult;
        };
        defer result.deinit(test_env.allocator);
        try std.testing.expectEqualStrings(fixture.file_uri, result.uri);
        try std.testing.expectEqual(definition_line, result.range.start_line);
    }
};

/// Hover on `greet` in `main = greet(42)`, where the argument type error must
/// not hide the function's signature.
fn hoverOnCallSiteNameShowsSignatureDespiteTypeError() integration_spec.SpecError!void {
    var fixture: Fixture = undefined;
    try fixture.init(
        \\app [main, greet] {{ pf: platform "{s}" }}
        \\
        \\greet : Str -> Str
        \\greet = |text| text
        \\
        \\main = greet(42)
        \\
    , "hover_11523.roc");
    defer fixture.deinit();

    try fixture.expectHoverContains(5, 7, "Str -> Str");
}

/// Hover on `greet!` in the issue's unused-value statement inside `main!`'s
/// body.
fn hoverOnUnusedValueStatementNameShowsSignature() integration_spec.SpecError!void {
    var fixture: Fixture = undefined;
    try fixture.init(unused_value_source, "hover_unused_11523.roc");
    defer fixture.deinit();

    try fixture.expectHoverContains(6, 4, "Str => Try(Str, [Exit(I32)])");
}

/// Goto definition on the same call-site name resolves to the local definition.
fn gotoDefinitionResolvesCallSiteNameDespiteTypeError() integration_spec.SpecError!void {
    var fixture: Fixture = undefined;
    try fixture.init(unused_value_source, "def_11523.roc");
    defer fixture.deinit();

    try fixture.expectDefinitionLine(6, 4, 3);
}

/// A call whose argument is itself erroneous is retired as a whole; its callee
/// is still source the editor must see.
fn calleeOfCallRetiredForErroneousArgument() integration_spec.SpecError!void {
    var fixture: Fixture = undefined;
    try fixture.init(
        \\app [main] {{ pf: platform "{s}" }}
        \\
        \\greet : Str -> Str
        \\greet = |text| text
        \\
        \\main = greet(missing)
        \\
    , "retired_call_11523.roc");
    defer fixture.deinit();

    try fixture.expectHoverContains(5, 7, "Str -> Str");
    try fixture.expectDefinitionLine(5, 7, 3);
}

/// Renaming a local binding must rewrite its use inside the retired statement,
/// or the rename would leave a stale reference behind.
fn renameUpdatesNameInsideUnusedValueStatement() integration_spec.SpecError!void {
    var fixture: Fixture = undefined;
    try fixture.init(
        \\app [main!] {{ pf: platform "{s}" }}
        \\
        \\main! = |_| {{
        \\    shout = |text| Ok(text)
        \\    shout("hello")
        \\    Ok({{}})
        \\}}
        \\
    , "rename_11523.roc");
    defer fixture.deinit();

    const outcome = try fixture.checker.getRenameEditsAtPosition(fixture.file_uri, fixture.source, 3, 4, "yell") orelse {
        std.debug.print("\nrename returned null\n", .{});
        return error.TestUnexpectedResult;
    };
    switch (outcome) {
        .rejected => |rejection| {
            std.debug.print("\nrename was rejected: {}\n", .{rejection});
            return error.TestUnexpectedResult;
        },
        .edits => |edits| {
            defer edits.deinit(test_env.allocator);
            var renamed_use = false;
            for (edits.regions) |region| {
                if (region.start_line == 4 and region.start_col == 4) renamed_use = true;
            }
            if (!renamed_use) {
                std.debug.print("\nrename did not edit the use on line 4: {any}\n", .{edits.regions});
                return error.TestUnexpectedResult;
            }
        },
    }
}
