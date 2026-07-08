//! Tests for string interpolation patterns during canonicalization.

const std = @import("std");
const parse = @import("parse");

const Can = @import("../Can.zig");
const ModuleEnv = @import("../ModuleEnv.zig");
const BuiltinTestContext = @import("./BuiltinTestContext.zig").BuiltinTestContext;

const CoreCtx = @import("ctx").CoreCtx;
const testing = std.testing;

test "adjacent string pattern captures emit warnings" {
    const allocator = testing.allocator;
    var builtin_ctx = try BuiltinTestContext.init(allocator);
    defer builtin_ctx.deinit();

    const source =
        \\classify : Str -> Str
        \\classify = |s| match s {
        \\    "${foo}${bar}" => foo
        \\    "${_}${_}" => "discard"
        \\    "ok${value}done" => value
        \\    _ => "miss"
        \\}
    ;

    var env = try ModuleEnv.init(allocator, source);
    defer env.deinit();
    try env.initCIRFields("Test");

    const roc_ctx = CoreCtx.testing(allocator, allocator);
    const ast = try parse.file(allocator, &env.common);
    defer ast.deinit();

    var czer = try Can.initModule(roc_ctx, &env, ast, builtin_ctx.canInitContext());
    defer czer.deinit();

    try czer.canonicalizeFile();

    var warning_count: usize = 0;
    const diag_indices = env.store.sliceDiagnostics(env.diagnostics);
    for (diag_indices) |diag_idx| {
        const diag = env.store.getDiagnostic(diag_idx);
        switch (diag) {
            .unreachable_string_pattern_capture => warning_count += 1,
            else => {},
        }
    }

    try testing.expectEqual(@as(usize, 2), warning_count);
}
