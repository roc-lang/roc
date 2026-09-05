//! Regression coverage for issue #10875.

const std = @import("std");
const can = @import("can");
const TestEnv = @import("./TestEnv.zig");

fn expectSingleNamingWarning(
    test_env: *TestEnv,
    expected_ident: []const u8,
    expected_mutability: can.CIR.Diagnostic.BindingMutability,
) TestEnv.TestEnvError!void {
    const diagnostics = try test_env.module_env.getDiagnostics();
    defer test_env.gpa.free(diagnostics);

    try std.testing.expectEqual(@as(usize, 1), diagnostics.len);
    try std.testing.expectEqual(.binding_name_does_not_match_mutability, std.meta.activeTag(diagnostics[0]));
    const mismatch = diagnostics[0].binding_name_does_not_match_mutability;
    try std.testing.expectEqual(expected_mutability, mismatch.mutability);
    try std.testing.expectEqualStrings(expected_ident, test_env.module_env.getIdent(mismatch.ident));
    try std.testing.expectEqualStrings(
        expected_ident,
        test_env.module_env.getSourceAll()[mismatch.region.start.offset..mismatch.region.end.offset],
    );
    try std.testing.expectEqual(@as(usize, 0), test_env.checker.problems.problems.items.len);

    var report = try test_env.module_env.diagnosticToReport(diagnostics[0], test_env.gpa, "Test");
    defer report.deinit();
    try std.testing.expectEqual(.warning, report.severity);
    try std.testing.expectEqualStrings(switch (expected_mutability) {
        .mutable => "Var Name Missing `$`",
        .immutable => "Dollar Prefix Without `var`",
    }, report.title);
}

fn expectVarPattern(test_env: *TestEnv, expected_ident: []const u8) TestEnv.TestEnvError!void {
    var raw_node_idx: u32 = 0;
    while (raw_node_idx < test_env.module_env.store.nodes.len()) : (raw_node_idx += 1) {
        const node_idx: can.CIR.Node.Idx = @enumFromInt(raw_node_idx);
        if (test_env.module_env.store.nodes.get(node_idx).tag != .statement_var) continue;

        const statement: can.CIR.Statement.Idx = @enumFromInt(raw_node_idx);
        const var_stmt = test_env.module_env.store.getStatement(statement).s_var;
        const pattern = test_env.module_env.store.getPattern(var_stmt.pattern_idx);
        try std.testing.expectEqual(.var_assign, std.meta.activeTag(pattern));
        try std.testing.expectEqualStrings(expected_ident, test_env.module_env.getIdent(pattern.var_assign.ident));
        return;
    }
    return error.TestUnexpectedResult;
}

test "check - repro - issue 10875 - var without dollar warns but remains mutable" {
    const source =
        \\main = |_| {
        \\    var total = 3
        \\    total = total + 1
        \\    total
        \\}
    ;

    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try expectSingleNamingWarning(&test_env, "total", .mutable);
    try expectVarPattern(&test_env, "total");
}

test "check - issue 10875 - dollar-prefixed var has explicit mutable CIR" {
    const source =
        \\main = |_| {
        \\    var $total = 3
        \\    $total = $total + 1
        \\    $total
        \\}
    ;

    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    const diagnostics = try test_env.module_env.getDiagnostics();
    defer test_env.gpa.free(diagnostics);
    try std.testing.expectEqual(@as(usize, 0), diagnostics.len);
    try std.testing.expectEqual(@as(usize, 0), test_env.checker.problems.problems.items.len);
    try expectVarPattern(&test_env, "$total");
}

test "check - issue 10875 - dollar spelling does not make an immutable binding mutable" {
    const source =
        \\main = |_| {
        \\    $value = 3
        \\    $value
        \\}
    ;

    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try expectSingleNamingWarning(&test_env, "$value", .immutable);
}

test "check - issue 10875 - annotation and definition report one declaration warning" {
    const source =
        \\main = |_| {
        \\    $value : U64
        \\    $value = 3
        \\    $value
        \\}
    ;

    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try expectSingleNamingWarning(&test_env, "$value", .immutable);
}

test "check - issue 10875 - associated annotation and definition warn at the source name once" {
    const source =
        \\Thing := U64.{
        \\    $identity : _
        \\    $identity = |value| value
        \\}
    ;

    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try expectSingleNamingWarning(&test_env, "$identity", .immutable);
}

test "check - issue 10875 - dollar and bare identifiers remain distinct" {
    const source =
        \\main = |_| {
        \\    var $value = 3
        \\    value = 10
        \\    $value = $value + 1
        \\    value + $value
        \\}
    ;

    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    const diagnostics = try test_env.module_env.getDiagnostics();
    defer test_env.gpa.free(diagnostics);
    try std.testing.expectEqual(@as(usize, 0), diagnostics.len);
    try std.testing.expectEqual(@as(usize, 0), test_env.checker.problems.problems.items.len);
    try expectVarPattern(&test_env, "$value");
}
