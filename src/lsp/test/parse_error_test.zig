//! Regression tests for surfacing parser failures as LSP diagnostics.

const std = @import("std");
const SyntaxChecker = @import("lsp").syntax.SyntaxChecker;
const integration_spec = @import("integration_spec.zig");
const test_env = @import("integration_env.zig");

/// Parse-error integration specs exported to the LSP harness.
pub const specs = [_]integration_spec.Spec{
    .{ .name = "parse errors are reported as diagnostics", .run = parseErrorsAreReportedAsDiagnostics },
};

/// Verifies parser failures are returned as concrete LSP diagnostics.
pub fn parseErrorsAreReportedAsDiagnostics() integration_spec.SpecError!void {
    const allocator = test_env.allocator;
    var tmp = test_env.tmpDir(.{});
    defer tmp.cleanup();
    const cache_path = try tmp.dir.realPathFileAlloc(test_env.io, ".", allocator);
    defer allocator.free(cache_path);

    var checker = SyntaxChecker.init(allocator, test_env.io, .{}, null);
    test_env.configureChecker(&checker, cache_path);
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
            std.mem.find(u8, first_diag.message, "UNCLOSED") != null or
            std.mem.find(u8, first_diag.message, "PARSE") != null or
            std.mem.find(u8, first_diag.message, "string") != null;
        try std.testing.expect(mentions_parse_issue);
    }
}
