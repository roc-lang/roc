//! Regression tests for https://github.com/roc-lang/roc/issues/13366.
//! A tokenizer diagnostic must be published at the source it reports,
//! never at the start of the file.

const std = @import("std");
const lsp = @import("lsp");
const SyntaxChecker = lsp.syntax.SyntaxChecker;
const Range = lsp.diagnostics.Range;
const integration_spec = @import("integration_spec.zig");
const test_env = @import("integration_env.zig");

/// Issue 13366 integration specs exported to the LSP harness.
pub const specs = [_]integration_spec.Spec{
    .{ .name = "issue 13366: a leading zero is reported at its number literal", .run = leadingZero },
    .{ .name = "issue 13366: an uppercase base is reported at its prefix letter", .run = uppercaseBase },
    .{ .name = "issue 13366: a control character in a string is reported at the character", .run = controlInString },
    .{ .name = "issue 13366: a stray control character is reported at the character", .run = strayControl },
};

fn leadingZero() integration_spec.SpecError!void {
    try expectDiagnostic(
        "A := [].{\n    n = 0000\n}\n",
        "Numbers cannot have leading zeros.",
        .{ .start = .{ .line = 1, .character = 8 }, .end = .{ .line = 1, .character = 12 } },
    );
}

fn uppercaseBase() integration_spec.SpecError!void {
    try expectDiagnostic(
        "A := [].{\n    n = 0X1F\n}\n",
        "Number base prefixes must be lowercase (0x, 0o, 0b).",
        .{ .start = .{ .line = 1, .character = 9 }, .end = .{ .line = 1, .character = 10 } },
    );
}

fn controlInString() integration_spec.SpecError!void {
    try expectDiagnostic(
        "A := [].{\n    s = \"a\x01b\"\n}\n",
        "Non-printable Unicode characters are not allowed in string-like literals.",
        .{ .start = .{ .line = 1, .character = 10 }, .end = .{ .line = 1, .character = 11 } },
    );
}

fn strayControl() integration_spec.SpecError!void {
    try expectDiagnostic(
        "A := [].{\n    n = 1 \x02\n}\n",
        "ASCII control characters are not allowed in Roc source code.",
        .{ .start = .{ .line = 1, .character = 10 }, .end = .{ .line = 1, .character = 11 } },
    );
}

/// Check `source` as the type module `A.roc` and expect a diagnostic whose
/// message starts with `message_line` to be published at `range`.
fn expectDiagnostic(source: []const u8, message_line: []const u8, range: Range) integration_spec.SpecError!void {
    const allocator = test_env.allocator;
    var tmp = test_env.tmpDir(.{});
    defer tmp.cleanup();

    try tmp.dir.writeFile(test_env.io, .{ .sub_path = "A.roc", .data = source });
    const file_path = try tmp.dir.realPathFileAlloc(test_env.io, "A.roc", allocator);
    defer allocator.free(file_path);
    const uri = try lsp.uri.pathToUri(allocator, file_path);
    defer allocator.free(uri);

    var checker = SyntaxChecker.init(allocator, test_env.io, .{}, null);
    test_env.configureChecker(&checker, std.fs.path.dirname(file_path) orelse file_path);
    defer checker.deinit();

    const result = try checker.check(uri, source, null, null);
    defer {
        for (result) |*publish| publish.deinit(allocator);
        allocator.free(result);
    }

    for (result) |publish| {
        for (publish.diagnostics) |diagnostic| {
            if (!std.mem.startsWith(u8, diagnostic.message, message_line)) continue;
            try std.testing.expectEqualStrings(uri, publish.uri);
            try std.testing.expectEqual(range, diagnostic.range);
            return;
        }
    }
    return error.TestUnexpectedResult;
}
