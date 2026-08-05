//! Tests for the `roc: "<version>"` compiler-version pin a header may carry.

const std = @import("std");
const parse = @import("parse");
const Can = @import("../Can.zig");
const CIR = @import("../CIR.zig");
const ModuleEnv = @import("../ModuleEnv.zig");
const BuiltinTestContext = @import("BuiltinTestContext.zig").BuiltinTestContext;
const CoreCtx = @import("ctx").CoreCtx;

/// Canonicalize `source` as a compiler reporting itself as `compiler_version`,
/// and return the version-mismatch diagnostic it produced, if any.
fn mismatchDiagnostic(
    allocator: std.mem.Allocator,
    source: []const u8,
    compiler_version: ?[]const u8,
) (std.mem.Allocator.Error || error{TestUnexpectedResult})!?CIR.Diagnostic {
    var builtin_ctx = try BuiltinTestContext.init(allocator);
    defer builtin_ctx.deinit();

    var env = try ModuleEnv.init(allocator, source);
    defer env.deinit();
    try env.initCIRFields("Test");

    const ast = try parse.file(allocator, &env.common);
    defer ast.deinit();

    var context = builtin_ctx.canInitContext();
    context.compiler_version = compiler_version;

    const roc_ctx = CoreCtx.testing(allocator, allocator);
    var can = try Can.initModule(roc_ctx, &env, ast, context);
    defer can.deinit();
    try can.canonicalizeFile();

    const diagnostics = try env.getDiagnostics();
    defer allocator.free(diagnostics);

    var found: ?CIR.Diagnostic = null;
    for (diagnostics) |diagnostic| {
        if (diagnostic == .roc_version_mismatch) {
            try std.testing.expect(found == null);
            found = diagnostic;
        }
    }
    return found;
}

const pinned_app =
    \\app [main!] { pf: platform "../platform/main.roc", roc: "nightly-2026-July-30-aaaaaaa" }
    \\
    \\main! = |_| {}
;

test "a pin that names another compiler is reported" {
    const allocator = std.testing.allocator;
    const found = try mismatchDiagnostic(allocator, pinned_app, "nightly-2026-August-1-bbbbbbb");

    const diagnostic = found orelse return error.TestUnexpectedResult;
    try std.testing.expect(diagnostic == .roc_version_mismatch);
}

test "a pin that names the running compiler is not reported" {
    const allocator = std.testing.allocator;
    const found = try mismatchDiagnostic(allocator, pinned_app, "nightly-2026-July-30-aaaaaaa");
    try std.testing.expect(found == null);
}

test "a pin is not reported when the running compiler was built from source" {
    const allocator = std.testing.allocator;
    try std.testing.expect(try mismatchDiagnostic(allocator, pinned_app, "debug-c6dfe61b") == null);
    try std.testing.expect(try mismatchDiagnostic(allocator, pinned_app, "release-fast-7fdb318d") == null);
}

test "a pin is not checked when the caller does not say which compiler is running" {
    const allocator = std.testing.allocator;
    const found = try mismatchDiagnostic(allocator, pinned_app, null);
    try std.testing.expect(found == null);
}

test "a header without a pin is never reported" {
    const allocator = std.testing.allocator;
    const found = try mismatchDiagnostic(allocator,
        \\app [main!] { pf: platform "../platform/main.roc" }
        \\
        \\main! = |_| {}
    , "nightly-2026-August-1-bbbbbbb");
    try std.testing.expect(found == null);
}

test "an unreadable pin is left to the parser to report" {
    const allocator = std.testing.allocator;
    const found = try mismatchDiagnostic(allocator,
        \\app [main!] { pf: platform "../platform/main.roc", roc: "yesterday's build" }
        \\
        \\main! = |_| {}
    , "nightly-2026-August-1-bbbbbbb");
    try std.testing.expect(found == null);
}

test "package and platform headers are checked too" {
    const allocator = std.testing.allocator;

    const package = try mismatchDiagnostic(allocator,
        \\package [Foo] { roc: "0.1.0" }
    , "nightly-2026-August-1-bbbbbbb");
    try std.testing.expect(package != null);

    const platform = try mismatchDiagnostic(allocator,
        \\platform "test-platform"
        \\    requires {}
        \\    exposes []
        \\    packages { roc: "0.1.0" }
        \\    provides {}
    , "nightly-2026-August-1-bbbbbbb");
    try std.testing.expect(platform != null);
}
