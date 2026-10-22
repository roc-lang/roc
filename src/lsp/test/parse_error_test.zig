//! Regression tests for surfacing parser failures as LSP diagnostics.

const std = @import("std");
const lsp = @import("lsp");
const SyntaxChecker = lsp.syntax.SyntaxChecker;
const uri_util = lsp.uri;
const integration_spec = @import("integration_spec.zig");
const test_env = @import("integration_env.zig");

/// Parse-error integration specs exported to the LSP harness.
pub const specs = [_]integration_spec.Spec{
    .{ .name = "parse errors are reported as diagnostics", .run = parseErrorsAreReportedAsDiagnostics },
};

/// An app whose body has an unclosed string literal; its header is intact.
const app_source_template =
    \\app [process_string] {{ pf: platform "{s}" }}
    \\
    \\process_string = |input|
    \\    Str.concat(input, "hello world
    \\
;

/// A broken `main.roc` beside the app. The app carries its own packages, so
/// checking it must report the app's own errors and never build this file.
const sibling_main_source =
    \\main! = |_args| {
    \\    echo!(no_such_name)
    \\    Ok({})
    \\}
    \\
;

/// Verifies parser failures are returned as concrete LSP diagnostics for the
/// checked file, regardless of a `main.roc` in the same directory.
pub fn parseErrorsAreReportedAsDiagnostics() integration_spec.SpecError!void {
    const allocator = test_env.allocator;
    var tmp = test_env.tmpDir(.{});
    defer tmp.cleanup();

    const source = try std.fmt.allocPrint(allocator, app_source_template, .{test_env.tmp_dir_platform_path});
    defer allocator.free(source);

    try tmp.dir.writeFile(test_env.io, .{ .sub_path = "main.roc", .data = sibling_main_source });
    try tmp.dir.writeFile(test_env.io, .{ .sub_path = "app.roc", .data = source });
    const file_path = try tmp.dir.realPathFileAlloc(test_env.io, "app.roc", allocator);
    defer allocator.free(file_path);
    const uri = try uri_util.pathToUri(allocator, file_path);
    defer allocator.free(uri);

    var checker = SyntaxChecker.init(allocator, test_env.io, .{}, null);
    test_env.configureChecker(&checker, std.fs.path.dirname(file_path) orelse file_path);
    defer checker.deinit();

    const result = try checker.check(uri, source, null);
    defer {
        for (result) |*pub_diag| {
            pub_diag.deinit(allocator);
        }
        allocator.free(result);
    }

    try std.testing.expectEqual(@as(usize, 1), result.len);
    try std.testing.expectEqualStrings(uri, result[0].uri);
    try std.testing.expect(result[0].diagnostics.len > 0);
    const first_message = result[0].diagnostics[0].message;
    const first_line_end = std.mem.findScalar(u8, first_message, '\n') orelse first_message.len;
    try std.testing.expectEqualStrings("This string is missing a closing quote.", first_message[0..first_line_end]);

    for (result[0].diagnostics) |diag| {
        try std.testing.expect(std.mem.find(u8, diag.message, "no_such_name") == null);
    }
}
