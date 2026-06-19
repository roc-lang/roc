//! Tests for checker selection of top-level-equivalent compile-time roots.

const std = @import("std");
const CIR = @import("can").CIR;
const TestEnv = @import("./TestEnv.zig");
const hoist_roots = @import("../hoist_roots.zig");

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

test "hoist roots selected for closed ordinary call binding RHS" {
    var test_env = try TestEnv.init("Test",
        \\add_one = |n| n + 1.I64
        \\
        \\main = |arg| {
        \\    x = add_one(41.I64)
        \\    x + arg
        \\}
    );
    defer test_env.deinit();

    try test_env.assertNoErrors();
    const roots = test_env.checker.selectedHoistedRoots();
    try std.testing.expectEqual(@as(usize, 1), roots.len);
    try std.testing.expect(roots[0].pattern != null);
    try expectExprTag(&test_env, roots[0].expr, .e_call);
}

test "hoist roots selected for direct closed ordinary call function body" {
    var test_env = try TestEnv.init("Test",
        \\add_one = |n| n + 1.I64
        \\
        \\main = |_| add_one(41.I64)
    );
    defer test_env.deinit();

    try test_env.assertNoErrors();
    const roots = test_env.checker.selectedHoistedRoots();
    try std.testing.expectEqual(@as(usize, 1), roots.len);
    try std.testing.expectEqual(@as(?CIR.Pattern.Idx, null), roots[0].pattern);
    try expectExprTag(&test_env, roots[0].expr, .e_call);
}

test "hoist roots selected for direct closed arithmetic function body" {
    var test_env = try TestEnv.init("Test",
        \\main = |_| 1.I64 + 2.I64
    );
    defer test_env.deinit();

    try test_env.assertNoErrors();
    const roots = test_env.checker.selectedHoistedRoots();
    try std.testing.expectEqual(@as(usize, 1), roots.len);
    try std.testing.expectEqual(@as(?CIR.Pattern.Idx, null), roots[0].pattern);
    try expectExprTag(&test_env, roots[0].expr, .e_dispatch_call);
}

test "hoist roots select closed ordinary call dependencies first" {
    var test_env = try TestEnv.init("Test",
        \\add_one = |n| n + 1.I64
        \\
        \\main = |arg| {
        \\    base = 41.I64
        \\    x = add_one(base)
        \\    x + arg
        \\}
    );
    defer test_env.deinit();

    try test_env.assertNoErrors();
    const roots = test_env.checker.selectedHoistedRoots();
    try std.testing.expectEqual(@as(usize, 2), roots.len);
    try std.testing.expect(roots[0].pattern != null);
    try std.testing.expect(roots[1].pattern != null);
    try expectExprTag(&test_env, roots[1].expr, .e_call);
}

test "hoist roots select closed ordinary call child expressions" {
    var test_env = try TestEnv.init("Test",
        \\add_one = |n| n + 1.I64
        \\
        \\main = |arg| {
        \\    _ = [add_one(41.I64), arg]
        \\    arg
        \\}
    );
    defer test_env.deinit();

    try test_env.assertNoErrors();
    const roots = test_env.checker.selectedHoistedRoots();
    try std.testing.expectEqual(@as(usize, 1), roots.len);
    try std.testing.expectEqual(@as(?CIR.Pattern.Idx, null), roots[0].pattern);
    try expectExprTag(&test_env, roots[0].expr, .e_call);
}

test "hoist roots selected for closed pure static dispatch call binding RHS" {
    var test_env = try TestEnv.init("Test",
        \\DispatchBox := [Val(I64)].{
        \\    inc = |DispatchBox.Val(n)| n + 1.I64
        \\}
        \\
        \\main = |arg| {
        \\    x = DispatchBox.Val(41.I64).inc()
        \\    x + arg
        \\}
    );
    defer test_env.deinit();

    try test_env.assertNoErrors();
    const roots = test_env.checker.selectedHoistedRoots();
    try std.testing.expectEqual(@as(usize, 1), roots.len);
    try std.testing.expect(roots[0].pattern != null);
    try expectExprTag(&test_env, roots[0].expr, .e_dispatch_call);
}

test "hoist roots selected for direct closed static dispatch function body" {
    var test_env = try TestEnv.init("Test",
        \\DispatchBox := [Val(I64)].{
        \\    inc = |DispatchBox.Val(n)| n + 1.I64
        \\}
        \\
        \\main = |_| DispatchBox.Val(41.I64).inc()
    );
    defer test_env.deinit();

    try test_env.assertNoErrors();
    const roots = test_env.checker.selectedHoistedRoots();
    try std.testing.expectEqual(@as(usize, 1), roots.len);
    try std.testing.expectEqual(@as(?CIR.Pattern.Idx, null), roots[0].pattern);
    try expectExprTag(&test_env, roots[0].expr, .e_dispatch_call);
}

test "hoist roots are not selected for ordinary call with runtime argument" {
    var test_env = try TestEnv.init("Test",
        \\add_one = |n| n + 1.I64
        \\
        \\main = |arg| {
        \\    x = add_one(arg)
        \\    x
        \\}
    );
    defer test_env.deinit();

    try test_env.assertNoErrors();
    try std.testing.expectEqual(@as(usize, 0), test_env.checker.selectedHoistedRoots().len);
}

test "hoist roots are not selected for ordinary call with runtime callee" {
    var test_env = try TestEnv.init("Test",
        \\main = |arg, f| {
        \\    x = f(41.I64)
        \\    x + arg
        \\}
    );
    defer test_env.deinit();

    try test_env.assertNoErrors();
    try std.testing.expectEqual(@as(usize, 0), test_env.checker.selectedHoistedRoots().len);
}

test "hoist roots are not selected for direct runtime-dependent function body" {
    var test_env = try TestEnv.init("Test",
        \\add_one = |n| n + 1.I64
        \\
        \\main = |arg| add_one(arg)
    );
    defer test_env.deinit();

    try test_env.assertNoErrors();
    try std.testing.expectEqual(@as(usize, 0), test_env.checker.selectedHoistedRoots().len);
}

test "hoist roots selected for closed nested lambda body child" {
    var test_env = try TestEnv.init("Test",
        \\main = |arg| {
        \\    f = |_| 1.I64 + 2.I64
        \\    f(arg)
        \\}
    );
    defer test_env.deinit();

    try test_env.assertNoErrors();
    const roots = test_env.checker.selectedHoistedRoots();
    try std.testing.expectEqual(@as(usize, 1), roots.len);
    try std.testing.expectEqual(@as(?CIR.Pattern.Idx, null), roots[0].pattern);
    try expectExprTag(&test_env, roots[0].expr, .e_dispatch_call);
}

test "hoist roots are not selected for nested lambda body depending on its argument" {
    var test_env = try TestEnv.init("Test",
        \\main = |arg| {
        \\    f = |n| n + 2.I64
        \\    f(arg)
        \\}
    );
    defer test_env.deinit();

    try test_env.assertNoErrors();
    try std.testing.expectEqual(@as(usize, 0), test_env.checker.selectedHoistedRoots().len);
}

test "hoist roots selected for record destructure extraction binders" {
    var test_env = try TestEnv.init("Test",
        \\main = |arg| {
        \\    { x } = { x: 41.I64 }
        \\    y = x + 1.I64
        \\    y + arg
        \\}
    );
    defer test_env.deinit();

    try test_env.assertNoErrors();
    const roots = test_env.checker.selectedHoistedRoots();
    try std.testing.expectEqual(@as(usize, 2), roots.len);
    try expectPatternExtractionRoot(roots[0]);
    try std.testing.expect(roots[1].pattern != null);
}

test "hoist roots selected for tuple destructure extraction binders" {
    var test_env = try TestEnv.init("Test",
        \\main = |arg| {
        \\    pair = (40.I64, 2.I64)
        \\    (left, right) = pair
        \\    total = left + right
        \\    total + arg
        \\}
    );
    defer test_env.deinit();

    try test_env.assertNoErrors();
    const roots = test_env.checker.selectedHoistedRoots();
    try std.testing.expectEqual(@as(usize, 4), roots.len);
    try std.testing.expect(roots[0].pattern != null);
    try expectPatternExtractionRoot(roots[1]);
    try expectPatternExtractionRoot(roots[2]);
    try std.testing.expect(roots[3].pattern != null);
}

test "hoist roots selected for tag payload extraction binders" {
    var test_env = try TestEnv.init("Test",
        \\main = |arg| {
        \\    Ok(n) = Ok(41.I64)
        \\    y = n + 1.I64
        \\    y + arg
        \\}
    );
    defer test_env.deinit();

    try test_env.assertNoErrors();
    const roots = test_env.checker.selectedHoistedRoots();
    try std.testing.expectEqual(@as(usize, 2), roots.len);
    try expectPatternExtractionRoot(roots[0]);
    try std.testing.expect(roots[1].pattern != null);
}

test "hoist roots selected for nested tag payload extraction binders" {
    var test_env = try TestEnv.init("Test",
        \\main = |arg| {
        \\    Ok((left, right)) = Ok((40.I64, 2.I64))
        \\    total = left + right
        \\    total + arg
        \\}
    );
    defer test_env.deinit();

    try test_env.assertNoErrors();
    const roots = test_env.checker.selectedHoistedRoots();
    try std.testing.expectEqual(@as(usize, 3), roots.len);
    try expectPatternExtractionRoot(roots[0]);
    try expectPatternExtractionRoot(roots[1]);
    try std.testing.expect(roots[2].pattern != null);
}

test "hoist roots selected for nested record and tuple destructure extraction binders" {
    var test_env = try TestEnv.init("Test",
        \\main = |arg| {
        \\    { outer: { pair: (left, right) } } = { outer: { pair: (40.I64, 2.I64) } }
        \\    total = left + right
        \\    total + arg
        \\}
    );
    defer test_env.deinit();

    try test_env.assertNoErrors();
    const roots = test_env.checker.selectedHoistedRoots();
    try std.testing.expectEqual(@as(usize, 3), roots.len);
    try expectPatternExtractionRoot(roots[0]);
    try expectPatternExtractionRoot(roots[1]);
    try std.testing.expect(roots[2].pattern != null);
}

test "hoist roots selected for record rest extraction binders" {
    var test_env = try TestEnv.init("Test",
        \\main = |arg| {
        \\    { name: _, ..rest } = { name: "Roc", count: 41.I64 }
        \\    total = rest.count + 1.I64
        \\    total + arg
        \\}
    );
    defer test_env.deinit();

    try test_env.assertNoErrors();
    const roots = test_env.checker.selectedHoistedRoots();
    try std.testing.expectEqual(@as(usize, 2), roots.len);
    try expectPatternExtractionRoot(roots[0]);
    try std.testing.expect(roots[1].pattern != null);
}

test "hoist roots selected for single-branch match tuple binders" {
    var test_env = try TestEnv.init("Test",
        \\main = |arg| {
        \\    match (40.I64, 2.I64) {
        \\        (left, right) => left + right + arg
        \\    }
        \\}
    );
    defer test_env.deinit();

    try test_env.assertNoErrors();
    try std.testing.expectEqual(@as(usize, 2), countPatternExtractionRoots(test_env.checker.selectedHoistedRoots()));
}

test "hoist roots selected for single-branch match alias binders" {
    var test_env = try TestEnv.init("Test",
        \\main = |arg| {
        \\    match 41.I64 {
        \\        _n as whole => whole + arg
        \\    }
        \\}
    );
    defer test_env.deinit();

    try test_env.assertNoErrors();
    try std.testing.expectEqual(@as(usize, 1), countPatternExtractionRoots(test_env.checker.selectedHoistedRoots()));
}

test "hoist roots selected for single-branch match list rest binders" {
    var test_env = try TestEnv.init("Test",
        \\main = |arg| {
        \\    match [40.I64, 2.I64] {
        \\        [.. as rest] => List.len(rest).to_i64_wrap() + List.len(arg).to_i64_wrap()
        \\    }
        \\}
    );
    defer test_env.deinit();

    try test_env.assertNoErrors();
    try std.testing.expectEqual(@as(usize, 1), countPatternExtractionRoots(test_env.checker.selectedHoistedRoots()));
}

test "hoist roots selected for closed multi-branch match with branch binders" {
    var test_env = try TestEnv.init("Test",
        \\main = |arg| {
        \\    input : Try(I64, I64)
        \\    input = Ok(40.I64)
        \\    value = match input {
        \\        Ok(n) => n + 2.I64
        \\        Err(code) => code
        \\    }
        \\    value + List.len(arg).to_i64_wrap()
        \\}
    );
    defer test_env.deinit();

    try test_env.assertNoErrors();
    const roots = test_env.checker.selectedHoistedRoots();
    try std.testing.expectEqual(@as(usize, 2), roots.len);
    try std.testing.expectEqual(@as(usize, 1), countMatchExprRoots(&test_env));
    try std.testing.expectEqual(@as(usize, 0), countPatternExtractionRoots(roots));
}

test "hoist roots are not selected for runtime-dependent multi-branch match" {
    var test_env = try TestEnv.init("Test",
        \\main = |arg| {
        \\    input : Try(I64, I64)
        \\    input = if List.len(arg) == 0 {
        \\        Ok(40.I64)
        \\    } else {
        \\        Err(0.I64)
        \\    }
        \\    value = match input {
        \\        Ok(n) => n + 2.I64
        \\        Err(code) => code
        \\    }
        \\    value
        \\}
    );
    defer test_env.deinit();

    try test_env.assertNoErrors();
    try std.testing.expectEqual(@as(usize, 0), countMatchExprRoots(&test_env));
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

test "hoist roots are not selected for many unused closed locals" {
    var test_env = try TestEnv.init("Test",
        \\main = |arg| {
        \\    a00 = 0.I64
        \\    a01 = a00 + 1.I64
        \\    a02 = a01 + 1.I64
        \\    a03 = a02 + 1.I64
        \\    a04 = a03 + 1.I64
        \\    a05 = a04 + 1.I64
        \\    a06 = a05 + 1.I64
        \\    a07 = a06 + 1.I64
        \\    a08 = a07 + 1.I64
        \\    a09 = a08 + 1.I64
        \\    a10 = a09 + 1.I64
        \\    a11 = a10 + 1.I64
        \\    a12 = a11 + 1.I64
        \\    a13 = a12 + 1.I64
        \\    a14 = a13 + 1.I64
        \\    a15 = a14 + 1.I64
        \\    a16 = a15 + 1.I64
        \\    a17 = a16 + 1.I64
        \\    a18 = a17 + 1.I64
        \\    _a19 = a18 + 1.I64
        \\    arg
        \\}
    );
    defer test_env.deinit();

    try test_env.assertNoErrors();
    try std.testing.expectEqual(@as(usize, 0), test_env.checker.selectedHoistedRoots().len);
}

fn expectPatternExtractionRoot(root: hoist_roots.SelectedHoistedRoot) !void {
    try std.testing.expect(root.pattern != null);
    const extraction = switch (root.body) {
        .pattern_extraction => |extraction| extraction,
        .expr => return error.ExpectedPatternExtractionRoot,
    };
    try std.testing.expectEqual(root.expr, extraction.base_expr);
    try std.testing.expectEqual(root.pattern.?, extraction.result_pattern);
    try std.testing.expect(extraction.scrutinee_pattern != extraction.result_pattern);
}

fn expectExprTag(test_env: *const TestEnv, expr: CIR.Expr.Idx, expected: std.meta.Tag(CIR.Expr)) !void {
    try std.testing.expectEqual(expected, std.meta.activeTag(test_env.checker.cir.store.getExpr(expr)));
}

fn countPatternExtractionRoots(roots: []const hoist_roots.SelectedHoistedRoot) usize {
    var count: usize = 0;
    for (roots) |root| {
        switch (root.body) {
            .pattern_extraction => count += 1,
            .expr => {},
        }
    }
    return count;
}

fn countMatchExprRoots(test_env: *const TestEnv) usize {
    var count: usize = 0;
    for (test_env.checker.selectedHoistedRoots()) |root| {
        switch (test_env.checker.cir.store.getExpr(root.expr)) {
            .e_match => count += 1,
            .e_lookup_local,
            .e_lookup_external,
            .e_lookup_required,
            .e_str_segment,
            .e_bytes_literal,
            .e_num,
            .e_num_from_numeral,
            .e_frac_f32,
            .e_frac_f64,
            .e_dec,
            .e_dec_small,
            .e_typed_int,
            .e_typed_frac,
            .e_typed_num_from_numeral,
            .e_empty_list,
            .e_empty_record,
            .e_zero_argument_tag,
            .e_runtime_error,
            .e_ellipsis,
            .e_anno_only,
            .e_crash,
            .e_closure,
            .e_lambda,
            .e_hosted_lambda,
            .e_str,
            .e_list,
            .e_tuple,
            .e_block,
            .e_if,
            .e_call,
            .e_method_call,
            .e_dispatch_call,
            .e_record,
            .e_tag,
            .e_nominal,
            .e_nominal_external,
            .e_binop,
            .e_unary_minus,
            .e_unary_not,
            .e_field_access,
            .e_interpolation,
            .e_structural_eq,
            .e_method_eq,
            .e_type_method_call,
            .e_type_dispatch_call,
            .e_tuple_access,
            .e_dbg,
            .e_expect_err,
            .e_expect,
            .e_for,
            .e_return,
            .e_run_low_level,
            => {},
        }
    }
    return count;
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

test "hoist roots are not selected for branch-local binding dependencies" {
    var test_env = try TestEnv.init("Test",
        \\main = |arg| {
        \\    input : [A(I64), B]
        \\    input = if arg == 0.I64 {
        \\        A(41.I64)
        \\    } else {
        \\        B
        \\    }
        \\    value = match input {
        \\        A(n) => {
        \\            x = n + 1.I64
        \\            x
        \\        }
        \\        B => 0.I64
        \\    }
        \\    value
        \\}
    );
    defer test_env.deinit();

    try test_env.assertNoErrors();
    for (test_env.checker.selectedHoistedRoots()) |root| {
        try std.testing.expectEqual(@as(?CIR.Pattern.Idx, null), root.pattern);
    }
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

test "hoist roots depending on pruned non-concrete roots are pruned" {
    var test_env = try TestEnv.init("Test",
        \\main = |_| {
        \\    x = []
        \\    y = List.len(x).to_i64_wrap()
        \\    y
        \\}
    );
    defer test_env.deinit();

    try test_env.assertNoErrors();
    try std.testing.expectEqual(@as(usize, 0), test_env.checker.selectedHoistedRoots().len);
}

test "hoist roots depending on pruned custom literal roots are pruned" {
    var test_env = try TestEnv.init("Test",
        \\Picky := [Picky(Numeral)].{
        \\    from_numeral : Numeral -> Try(Picky, [InvalidNumeral(Str)])
        \\    from_numeral = |numeral| Ok(Picky(numeral))
        \\}
        \\
        \\main = |_| {
        \\    x : Picky
        \\    x = 42
        \\    y = match x {
        \\        Picky(_) => 7.I64
        \\    }
        \\    y
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
        \\Picky := [Picky(Numeral)].{
        \\    from_numeral : Numeral -> Try(Picky, [InvalidNumeral(Str)])
        \\    from_numeral = |numeral| Ok(Picky(numeral))
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
    try std.testing.expectEqual(@as(usize, 0), test_env.checker.selectedHoistedRoots().len);
}

test "hoist root validator accepts checker-selected roots" {
    var test_env = try TestEnv.init("Test",
        \\main = |arg| {
        \\    x = 41.I64
        \\    y = x + 1.I64
        \\    y + arg
        \\}
    );
    defer test_env.deinit();

    try test_env.assertNoErrors();
    try hoist_roots.validateSelectedRootSetForPublication(
        std.testing.allocator,
        test_env.module_env,
        test_env.checker.selectedHoistedRoots(),
    );
}

test "hoist root validator rejects runtime argument dependency" {
    var test_env = try TestEnv.init("Test",
        \\main = |arg| arg + 1.I64
    );
    defer test_env.deinit();

    try test_env.assertNoErrors();
    const invalid_roots = [_]hoist_roots.SelectedHoistedRoot{.{
        .expr = try firstExprWithTag(&test_env, .e_dispatch_call),
        .body = .expr,
    }};
    try std.testing.expectError(
        error.InvalidSelectedHoistedRootSet,
        hoist_roots.validateSelectedRootSetForPublication(std.testing.allocator, test_env.module_env, &invalid_roots),
    );
}

test "hoist root validator rejects effectful root" {
    var test_env = try TestEnv.init("Test",
        \\Foo := { x: I64 }.{
        \\    show! : Foo => I64
        \\    show! = |self| self.x
        \\}
        \\
        \\main! = |_| {
        \\    foo : Foo
        \\    foo = { x: 42.I64 }
        \\    foo.show!()
        \\}
    );
    defer test_env.deinit();

    try test_env.assertNoErrors();
    const invalid_roots = [_]hoist_roots.SelectedHoistedRoot{.{
        .expr = try firstExprWithTag(&test_env, .e_dispatch_call),
        .body = .expr,
    }};
    try std.testing.expectError(
        error.InvalidSelectedHoistedRootSet,
        hoist_roots.validateSelectedRootSetForPublication(std.testing.allocator, test_env.module_env, &invalid_roots),
    );
}

test "hoist root validator rejects function-typed root" {
    var test_env = try TestEnv.init("Test",
        \\main = |_| {
        \\    f = |n| n
        \\    f
        \\}
    );
    defer test_env.deinit();

    try test_env.assertNoErrors();
    const invalid_roots = [_]hoist_roots.SelectedHoistedRoot{.{
        .expr = try firstExprWithTag(&test_env, .e_lambda),
        .body = .expr,
    }};
    try std.testing.expectError(
        error.InvalidSelectedHoistedRootSet,
        hoist_roots.validateSelectedRootSetForPublication(std.testing.allocator, test_env.module_env, &invalid_roots),
    );
}

test "hoist root validator rejects dedicated literal conversion root" {
    var test_env = try TestEnv.init("Test",
        \\Picky := [Picky(Numeral)].{
        \\    from_numeral : Numeral -> Try(Picky, [InvalidNumeral(Str)])
        \\    from_numeral = |numeral| Ok(Picky(numeral))
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
    const invalid_roots = [_]hoist_roots.SelectedHoistedRoot{.{
        .expr = try firstExprWithLiteralConversion(&test_env),
        .body = .expr,
    }};
    try std.testing.expectError(
        error.InvalidSelectedHoistedRootSet,
        hoist_roots.validateSelectedRootSetForPublication(std.testing.allocator, test_env.module_env, &invalid_roots),
    );
}

test "hoist root validator rejects later selected dependency" {
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
    const reversed_roots = [_]hoist_roots.SelectedHoistedRoot{ roots[1], roots[0] };
    try std.testing.expectError(
        error.InvalidSelectedHoistedRootSet,
        hoist_roots.validateSelectedRootSetForPublication(std.testing.allocator, test_env.module_env, &reversed_roots),
    );
}

test "hoist root validator rejects unselected local dependency" {
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
    const missing_dependency_roots = [_]hoist_roots.SelectedHoistedRoot{roots[1]};
    try std.testing.expectError(
        error.InvalidSelectedHoistedRootSet,
        hoist_roots.validateSelectedRootSetForPublication(std.testing.allocator, test_env.module_env, &missing_dependency_roots),
    );
}

test "hoist root validator rejects mutable local dependency" {
    var test_env = try TestEnv.init("Test",
        \\main = |_| {
        \\    var x = 41.I64
        \\    y = x + 1.I64
        \\    y
        \\}
    );
    defer test_env.deinit();

    try test_env.assertNoErrors();
    const invalid_roots = [_]hoist_roots.SelectedHoistedRoot{.{
        .expr = try firstExprWithTag(&test_env, .e_dispatch_call),
        .body = .expr,
    }};
    try std.testing.expectError(
        error.InvalidSelectedHoistedRootSet,
        hoist_roots.validateSelectedRootSetForPublication(std.testing.allocator, test_env.module_env, &invalid_roots),
    );
}

fn firstExprWithTag(test_env: *const TestEnv, tag: std.meta.Tag(CIR.Expr)) !CIR.Expr.Idx {
    var raw_node_idx: u32 = 0;
    while (raw_node_idx < test_env.checker.cir.store.nodes.len()) : (raw_node_idx += 1) {
        const node_idx: CIR.Node.Idx = @enumFromInt(raw_node_idx);
        if (!exprNodeTag(test_env.checker.cir.store.nodes.get(node_idx).tag)) continue;
        const expr: CIR.Expr.Idx = @enumFromInt(raw_node_idx);
        if (std.meta.activeTag(test_env.checker.cir.store.getExpr(expr)) == tag) return expr;
    }
    return error.ExpectedExprTag;
}

fn firstExprWithLiteralConversion(test_env: *const TestEnv) !CIR.Expr.Idx {
    var raw_node_idx: u32 = 0;
    while (raw_node_idx < test_env.checker.cir.store.nodes.len()) : (raw_node_idx += 1) {
        const node_idx: CIR.Node.Idx = @enumFromInt(raw_node_idx);
        if (!exprNodeTag(test_env.checker.cir.store.nodes.get(node_idx).tag)) continue;
        if (test_env.module_env.numeralDispatchPlanForNode(node_idx) != null or
            test_env.module_env.quoteDispatchPlanForNode(node_idx) != null)
        {
            return @enumFromInt(raw_node_idx);
        }
    }
    return error.ExpectedLiteralConversionExpr;
}

fn exprNodeTag(tag: CIR.Node.Tag) bool {
    return switch (tag) {
        .expr_var,
        .expr_tuple,
        .expr_tuple_access,
        .expr_list,
        .expr_empty_list,
        .expr_call,
        .expr_record,
        .expr_empty_record,
        .expr_field_access,
        .expr_method_call,
        .expr_dispatch_call,
        .expr_interpolation,
        .expr_structural_eq,
        .expr_method_eq,
        .expr_type_method_call,
        .expr_type_dispatch_call,
        .expr_static_dispatch,
        .expr_external_lookup,
        .expr_required_lookup,
        .expr_apply,
        .expr_string,
        .expr_string_segment,
        .expr_bytes_literal,
        .expr_num,
        .expr_frac_f32,
        .expr_frac_f64,
        .expr_dec,
        .expr_dec_small,
        .expr_num_from_numeral,
        .expr_typed_int,
        .expr_typed_frac,
        .expr_typed_num_from_numeral,
        .expr_tag,
        .expr_nominal,
        .expr_nominal_external,
        .expr_zero_argument_tag,
        .expr_closure,
        .expr_lambda,
        .expr_record_update,
        .expr_bin_op,
        .expr_unary_minus,
        .expr_unary_not,
        .expr_suffix_single_question,
        .expr_if_then_else,
        .expr_match,
        .expr_dbg,
        .expr_crash,
        .expr_expect_err,
        .expr_block,
        .expr_ellipsis,
        .expr_anno_only,
        .expr_hosted_lambda,
        .expr_low_level,
        .expr_run_low_level,
        .expr_expect,
        .expr_for,
        .expr_record_builder,
        .expr_return,
        => true,
        .statement_decl,
        .statement_var,
        .statement_var_uninitialized,
        .statement_reassign,
        .statement_crash,
        .statement_dbg,
        .statement_expr,
        .statement_expect,
        .statement_for,
        .statement_while,
        .statement_infinite_loop,
        .statement_breakable_loop,
        .statement_break,
        .statement_return,
        .statement_import,
        .statement_alias_decl,
        .statement_nominal_decl,
        .statement_type_anno,
        .statement_type_var_alias,
        .record_field,
        .record_destruct,
        .match_branch,
        .match_branch_pattern,
        .type_header,
        .annotation,
        .ty_apply,
        .ty_apply_external,
        .ty_rigid_var,
        .ty_rigid_var_lookup,
        .ty_lookup,
        .ty_underscore,
        .ty_tag_union,
        .ty_tag,
        .ty_tuple,
        .ty_record,
        .ty_record_field,
        .ty_fn,
        .ty_parens,
        .ty_lookup_external,
        .ty_malformed,
        .where_method,
        .where_alias,
        .where_malformed,
        .pattern_identifier,
        .pattern_as,
        .pattern_applied_tag,
        .pattern_nominal,
        .pattern_nominal_external,
        .pattern_record_destructure,
        .pattern_list,
        .pattern_tuple,
        .pattern_num_literal,
        .pattern_dec_literal,
        .pattern_f32_literal,
        .pattern_f64_literal,
        .pattern_small_dec_literal,
        .pattern_str_literal,
        .pattern_str_interpolation,
        .pattern_underscore,
        .lambda_capture,
        .def,
        .exposed_item,
        .if_branch,
        .type_var_slot,
        .malformed,
        .diag_not_implemented,
        .diag_invalid_num_literal,
        .diag_empty_single_quote,
        .diag_empty_tuple,
        .diag_ident_already_in_scope,
        .diag_ident_not_in_scope,
        .diag_read_uninitialized_var,
        .diag_self_referential_definition,
        .diag_circular_value_definition,
        .diag_local_reference_before_definition,
        .diag_mutually_recursive_local_definitions,
        .diag_erroneous_value_use,
        .diag_erroneous_value_expr,
        .diag_qualified_ident_does_not_exist,
        .diag_invalid_top_level_statement,
        .diag_expr_not_canonicalized,
        .diag_invalid_string_interpolation,
        .diag_unreachable_string_pattern_capture,
        .diag_pattern_arg_invalid,
        .diag_pattern_not_canonicalized,
        .diag_can_lambda_not_implemented,
        .diag_lambda_body_not_canonicalized,
        .diag_if_condition_not_canonicalized,
        .diag_if_then_not_canonicalized,
        .diag_if_else_not_canonicalized,
        .diag_malformed_type_annotation,
        .diag_malformed_where_clause,
        .diag_where_clause_not_allowed_in_type_decl,
        .diag_open_ext_not_allowed_in_type_decl,
        .diag_type_module_missing_matching_type,
        .diag_type_module_has_alias_not_nominal,
        .diag_default_app_missing_main,
        .diag_default_app_wrong_arity,
        .diag_cannot_import_default_app,
        .diag_execution_requires_app_or_default_app,
        .diag_type_name_case_mismatch,
        .diag_module_header_deprecated,
        .diag_redundant_expose_main_type,
        .diag_invalid_main_type_rename_in_exposing,
        .diag_var_across_function_boundary,
        .diag_shadowing_warning,
        .diag_type_redeclared,
        .diag_undeclared_type,
        .diag_undeclared_type_var,
        .diag_type_alias_but_needed_nominal,
        .diag_type_alias_redeclared,
        .diag_tuple_elem_not_canonicalized,
        .diag_file_import_not_found,
        .diag_file_import_io_error,
        .diag_file_import_not_utf8,
        .diag_module_not_found,
        .diag_value_not_exposed,
        .diag_type_not_exposed,
        .diag_private_type_in_exposed_type,
        .diag_private_type_in_exposed_field,
        .diag_type_from_missing_module,
        .diag_module_not_imported,
        .diag_nested_type_not_found,
        .diag_nested_value_not_found,
        .diag_record_builder_map2_not_found,
        .diag_too_many_exports,
        .diag_nominal_type_redeclared,
        .diag_type_shadowed_warning,
        .diag_type_parameter_conflict,
        .diag_unused_variable,
        .diag_used_underscore_variable,
        .diag_duplicate_record_field,
        .diag_crash_expects_string,
        .diag_f64_pattern_literal,
        .diag_unused_type_var_name,
        .diag_type_var_marked_unused,
        .diag_type_var_starting_with_dollar,
        .diag_underscore_in_type_declaration,
        .diagnostic_exposed_but_not_implemented,
        .diag_redundant_exposed,
        .diag_if_expr_without_else,
        .diag_break_outside_loop,
        .diag_infinite_loop_never_exits,
        .diag_return_outside_fn,
        .diag_mutually_recursive_type_aliases,
        .diag_deprecated_number_suffix,
        .diag_range_op_chained,
        => false,
    };
}
