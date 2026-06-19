//! Tests for checker selection of top-level-equivalent compile-time roots.

const std = @import("std");
const CIR = @import("can").CIR;
const TestEnv = @import("./TestEnv.zig");

test "hoist roots selected for referenced closed local binding chain" {
    var test_env = try TestEnv.init("Test",
        \\main = |arg| {
        \\    x = 41.I64
        \\    y = x + 1.I64
        \\    y + arg
        \\}
    );
    defer test_env.deinit();

    try test_env.assertNoErrors();
    const roots = test_env.checker.selectedHoistedRoots();
    try std.testing.expectEqual(@as(usize, 2), roots.len);
    try std.testing.expect(roots[0].pattern != null);
    try std.testing.expect(roots[1].pattern != null);
}

test "hoist roots are not selected for local values depending on function arguments" {
    var test_env = try TestEnv.init("Test",
        \\main = |arg| {
        \\    x = arg + 1.I64
        \\    x
        \\}
    );
    defer test_env.deinit();

    try test_env.assertNoErrors();
    try std.testing.expectEqual(@as(usize, 0), test_env.checker.selectedHoistedRoots().len);
}

test "hoist roots are not selected for local values indirectly depending on function arguments" {
    var test_env = try TestEnv.init("Test",
        \\main = |arg| {
        \\    x = arg + 1.I64
        \\    y = x + 1.I64
        \\    y
        \\}
    );
    defer test_env.deinit();

    try test_env.assertNoErrors();
    try std.testing.expectEqual(@as(usize, 0), test_env.checker.selectedHoistedRoots().len);
}

test "hoist roots are not selected for mutable local dependencies" {
    var test_env = try TestEnv.init("Test",
        \\main = |_| {
        \\    var x = 41.I64
        \\    y = x + 1.I64
        \\    y
        \\}
    );
    defer test_env.deinit();

    try test_env.assertNoErrors();
    try std.testing.expectEqual(@as(usize, 0), test_env.checker.selectedHoistedRoots().len);
}

test "hoist roots are not selected for observable debug expressions" {
    var test_env = try TestEnv.init("Test",
        \\main = |_| {
        \\    dbg 1.I64
        \\    0.I64
        \\}
    );
    defer test_env.deinit();

    try test_env.assertNoErrors();
    try std.testing.expectEqual(@as(usize, 0), test_env.checker.selectedHoistedRoots().len);
}

test "hoist roots with non-concrete compile-time types are pruned" {
    var test_env = try TestEnv.init("Test",
        \\main = |arg| {
        \\    x = []
        \\    _y = x
        \\    arg
        \\}
    );
    defer test_env.deinit();

    try test_env.assertNoErrors();
    try std.testing.expectEqual(@as(usize, 0), test_env.checker.selectedHoistedRoots().len);
}

test "hoist roots are not selected inside ordinary top-level constants" {
    var test_env = try TestEnv.init("Test",
        \\value = {
        \\    x = 41.I64
        \\    y = x + 1.I64
        \\    y
        \\}
    );
    defer test_env.deinit();

    try test_env.assertNoErrors();
    try std.testing.expectEqual(@as(usize, 0), test_env.checker.selectedHoistedRoots().len);
}

test "hoist roots selected for arbitrary closed child expressions" {
    var test_env = try TestEnv.init("Test",
        \\main = |arg| {
        \\    if arg == 0.I64 {
        \\        1.I64 + 2.I64
        \\    } else {
        \\        arg
        \\    }
        \\}
    );
    defer test_env.deinit();

    try test_env.assertNoErrors();
    const roots = test_env.checker.selectedHoistedRoots();
    try std.testing.expectEqual(@as(usize, 1), roots.len);
    try std.testing.expectEqual(@as(?CIR.Pattern.Idx, null), roots[0].pattern);
}

test "hoist roots are not selected for custom from_numeral conversion roots" {
    var test_env = try TestEnv.init("Test",
        \\Picky := [Picky].{
        \\    from_numeral : Numeral -> Try(Picky, [InvalidNumeral(Str)])
        \\    from_numeral = |_numeral| Ok(Picky)
        \\}
        \\
        \\main = |_| {
        \\    x : Picky
        \\    x = 42
        \\    _ = x
        \\    0.I64
        \\}
    );
    defer test_env.deinit();

    try test_env.assertNoErrors();
    try std.testing.expectEqual(@as(usize, 0), test_env.checker.selectedHoistedRoots().len);
}

test "hoist roots are not selected for custom from_quote conversion roots" {
    var test_env = try TestEnv.init("Test",
        \\Tag := [Tag(Str)].{
        \\    from_quote : Str -> Try(Tag, [BadQuotedBytes(Str)])
        \\    from_quote = |str| Ok(Tag(str))
        \\}
        \\
        \\main = |_| {
        \\    x : Tag
        \\    x = "Roc"
        \\    _ = x
        \\    0.I64
        \\}
    );
    defer test_env.deinit();

    try test_env.assertNoErrors();
    try std.testing.expectEqual(@as(usize, 0), test_env.checker.selectedHoistedRoots().len);
}

test "hoist roots are not selected for effectful static dispatch calls" {
    var test_env = try TestEnv.init("Test",
        \\Foo := { x: I64 }.{
        \\    show! : Foo => I64
        \\    show! = |_self| 41.I64
        \\}
        \\
        \\main! = |_| {
        \\    foo : Foo
        \\    foo = { x: 42.I64 }
        \\    y = foo.show!()
        \\    _ = y
        \\    {}
        \\}
    );
    defer test_env.deinit();

    try test_env.assertNoErrors();
    try std.testing.expectEqual(@as(usize, 1), test_env.checker.selectedHoistedRoots().len);
}
