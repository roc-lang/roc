//! Regression tests for surfacing parser failures as LSP diagnostics.

const std = @import("std");
const SyntaxChecker = @import("../syntax.zig").SyntaxChecker;

test "parse errors are reported as diagnostics" {
    const allocator = std.testing.allocator;

    var checker = SyntaxChecker.init(allocator, .{}, null);
    defer checker.deinit();

    // File content with parse error (unclosed string)
    const source =
        \\app [main] { pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.17.0/lZFLstMUCUvd5bjnnpYromZJXkQUrdhbva4xdBInicE.tar.br" }
        \\
        \\import pf.Stdout
        \\
        \\main =
        \\    Stdout.line! "hello world
    ;

    const uri = "file:///tmp/test_parse_error.roc";

    // Check should return diagnostics (not the "Failed to retrieve diagnostics" error)
    const result = try checker.check(uri, source, null);
    defer {
        for (result) |*pub_diag| {
            pub_diag.deinit(allocator);
        }
        allocator.free(result);
    }

    // We should get at least one diagnostic
    try std.testing.expect(result.len > 0);

    // The diagnostic should NOT be the fallback "Failed to retrieve diagnostics" error
    // Parse errors should have actual error messages
    if (result[0].diagnostics.len > 0) {
        const first_diag = result[0].diagnostics[0];
        try std.testing.expect(!std.mem.startsWith(u8, first_diag.message, "Failed to retrieve diagnostics"));

        // Parse errors should mention the actual issue (unclosed string, parse error, etc.)
        const mentions_parse_issue =
            std.mem.indexOf(u8, first_diag.message, "UNCLOSED") != null or
            std.mem.indexOf(u8, first_diag.message, "PARSE") != null or
            std.mem.indexOf(u8, first_diag.message, "string") != null;
        try std.testing.expect(mentions_parse_issue);
    }
}
