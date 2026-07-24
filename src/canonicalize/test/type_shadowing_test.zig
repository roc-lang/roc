//! Tests for type redeclaration and type shadowing diagnostics.

const std = @import("std");
const TestEnv = @import("./TestEnv.zig");

const testing = std.testing;

test "type declaration shadowing a builtin has a dedicated warning" {
    const source =
        \\{
        \\    U64 := {}
        \\    123.U64
        \\}
    ;

    var test_env = try TestEnv.init(source);
    defer test_env.deinit();

    _ = try test_env.canonicalizeExpr();

    const diagnostics = try test_env.getDiagnostics();
    defer testing.allocator.free(diagnostics);

    var diag_count: usize = 0;
    for (diagnostics) |diag| {
        switch (diag) {
            .builtin_type_shadowed_warning => {
                diag_count += 1;
                var report = try test_env.module_env.diagnosticToReport(
                    diag,
                    testing.allocator,
                    test_env.module_env.module_name,
                );
                defer report.deinit();
                try testing.expectEqualStrings("Builtin Type Shadowed", report.title);
            },
            else => {},
        }
    }
    try testing.expectEqual(@as(usize, 1), diag_count);
}

test "nested type declaration has an outer-scope shadowing warning" {
    const source =
        \\{
        \\    Item := [Outer]
        \\    {
        \\        Item := [Inner]
        \\        {}
        \\    }
        \\}
    ;

    var test_env = try TestEnv.init(source);
    defer test_env.deinit();

    _ = try test_env.canonicalizeExpr();

    const diagnostics = try test_env.getDiagnostics();
    defer testing.allocator.free(diagnostics);

    var diag_count: usize = 0;
    for (diagnostics) |diag| {
        switch (diag) {
            .type_shadowed_warning => |data| {
                diag_count += 1;
                try testing.expect(data.original_region.start.offset < data.region.start.offset);

                var report = try test_env.module_env.diagnosticToReport(
                    diag,
                    testing.allocator,
                    test_env.module_env.module_name,
                );
                defer report.deinit();
                try testing.expectEqualStrings("Type Shadowed", report.title);
            },
            else => {},
        }
    }
    try testing.expectEqual(@as(usize, 1), diag_count);
}

test "type redeclaration in the same scope is an error" {
    const source =
        \\{
        \\    Item := [First]
        \\    Item := [Second]
        \\    {}
        \\}
    ;

    var test_env = try TestEnv.init(source);
    defer test_env.deinit();

    _ = try test_env.canonicalizeExpr();

    const diagnostics = try test_env.getDiagnostics();
    defer testing.allocator.free(diagnostics);

    var redeclaration_count: usize = 0;
    for (diagnostics) |diag| {
        switch (diag) {
            .type_redeclared => {
                redeclaration_count += 1;
                var report = try test_env.module_env.diagnosticToReport(
                    diag,
                    testing.allocator,
                    test_env.module_env.module_name,
                );
                defer report.deinit();
                try testing.expectEqualStrings("Type Redeclared", report.title);
            },
            .type_shadowed_warning, .builtin_type_shadowed_warning => return error.UnexpectedShadowingWarning,
            else => {},
        }
    }
    try testing.expectEqual(@as(usize, 1), redeclaration_count);
}
