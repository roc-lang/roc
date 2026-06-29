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

test "hoist roots selected for closed local block with internal locals" {
    var test_env = try TestEnv.init("Test",
        \\main = |arg| {
        \\    z = {
        \\        x = 41.I64
        \\        y = x + 1.I64
        \\        y
        \\    }
        \\    z + arg
        \\}
    );
    defer test_env.deinit();

    try test_env.assertNoErrors();
    const roots = test_env.checker.selectedHoistedRoots();
    try std.testing.expectEqual(@as(usize, 1), roots.len);
    try std.testing.expect(roots[0].pattern != null);
    try expectExprTag(&test_env, roots[0].expr, .e_block);
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

test "hoist roots are not selected for ordinary call with dbg in reachable body" {
    var test_env = try TestEnv.init("Test",
        \\dbg_value = || {
        \\    x = 42.I64
        \\    dbg x
        \\    x
        \\}
        \\
        \\main = |arg| {
        \\    x = dbg_value()
        \\    x + arg
        \\}
    );
    defer test_env.deinit();

    try test_env.assertNoErrors();
    try std.testing.expectEqual(@as(usize, 0), countExprRootsByTag(&test_env, .e_call));
}

test "hoist roots are not selected for ordinary call with expect in reachable body" {
    var test_env = try TestEnv.init("Test",
        \\expect_value = || {
        \\    expect 1.I64 == 1.I64
        \\    42.I64
        \\}
        \\
        \\main = |arg| {
        \\    x = expect_value()
        \\    x + arg
        \\}
    );
    defer test_env.deinit();

    try test_env.assertNoErrors();
    try std.testing.expectEqual(@as(usize, 0), countExprRootsByTag(&test_env, .e_call));
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

    try expectOnlyComptimeConditionWarnings(&test_env, 1);
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

    try expectOnlyComptimeConditionWarnings(&test_env, 1);
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

    try expectOnlyComptimeConditionWarnings(&test_env, 1);
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

    try expectOnlyComptimeConditionWarnings(&test_env, 1);
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

test "hoist roots are not selected for runtime-controlled match branch bodies" {
    var test_env = try TestEnv.init("Test",
        \\main = |arg| {
        \\    input : Try(I64, I64)
        \\    input = if arg == 0.I64 {
        \\        Ok(40.I64)
        \\    } else {
        \\        Err(0.I64)
        \\    }
        \\    match input {
        \\        Ok(_) => 1.I64 + 2.I64
        \\        Err(_) => arg
        \\    }
        \\}
    );
    defer test_env.deinit();

    try test_env.assertNoErrors();
    try std.testing.expectEqual(@as(usize, 0), test_env.checker.selectedHoistedRoots().len);
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

fn expectPatternExtractionRoot(root: hoist_roots.SelectedHoistedRoot) error{ TestUnexpectedResult, TestExpectedEqual, ExpectedPatternExtractionRoot }!void {
    try std.testing.expect(root.pattern != null);
    const extraction = switch (root.body) {
        .pattern_extraction => |extraction| extraction,
        .expr => return error.ExpectedPatternExtractionRoot,
    };
    try std.testing.expectEqual(root.expr, extraction.base_expr);
    try std.testing.expectEqual(root.pattern.?, extraction.result_pattern);
    try std.testing.expect(extraction.scrutinee_pattern != extraction.result_pattern);
}

fn expectExprTag(test_env: *const TestEnv, expr: CIR.Expr.Idx, expected: std.meta.Tag(CIR.Expr)) error{TestExpectedEqual}!void {
    try std.testing.expectEqual(expected, std.meta.activeTag(test_env.checker.cir.store.getExpr(expr)));
}

fn expectOnlyComptimeConditionWarnings(test_env: *TestEnv, expected_count: usize) error{ OutOfMemory, TestExpectedEqual, TestUnexpectedResult }!void {
    try std.testing.expect(!test_env.parse_ast.hasErrors());

    const diagnostics = try test_env.module_env.getDiagnostics();
    defer test_env.gpa.free(diagnostics);
    try std.testing.expectEqual(@as(usize, 0), diagnostics.len);

    try std.testing.expectEqual(expected_count, test_env.checker.problems.problems.items.len);
    for (test_env.checker.problems.problems.items) |problem| {
        switch (problem) {
            .comptime_condition => {},
            else => return error.TestUnexpectedResult,
        }
    }
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
            .e_structural_hash,
            .e_method_eq,
            .e_type_method_call,
            .e_type_dispatch_call,
            .e_tuple_access,
            .e_dbg,
            .e_expect_err,
            .e_expect,
            .e_for,
            .e_return,
            .e_break,
            .e_run_low_level,
            => {},
        }
    }
    return count;
}

fn countExprRootsByTag(test_env: *const TestEnv, tag: std.meta.Tag(CIR.Expr)) usize {
    var count: usize = 0;
    for (test_env.checker.selectedHoistedRoots()) |root| {
        if (std.meta.activeTag(test_env.checker.cir.store.getExpr(root.expr)) == tag) count += 1;
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

test "hoist roots are not selected after observable effects in blocks" {
    var test_env = try TestEnv.init("Test",
        \\main = |_| {
        \\    before = 1.I64 + 2.I64
        \\    dbg 0.I64
        \\    after = 3.I64 + 4.I64
        \\    before + after
        \\}
    );
    defer test_env.deinit();

    try test_env.assertNoErrors();
    try std.testing.expectEqual(@as(usize, 1), countExprRootsByTag(&test_env, .e_dispatch_call));
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

test "hoist arbitrary block roots with non-concrete internal locals are pruned" {
    var test_env = try TestEnv.init("Test",
        \\main = |arg| {
        \\    _ = [
        \\        {
        \\            x = []
        \\            List.len(x).to_i64_wrap()
        \\        },
        \\        arg,
        \\    ]
        \\    arg
        \\}
    );
    defer test_env.deinit();

    try test_env.assertNoErrors();
    try std.testing.expectEqual(@as(usize, 0), test_env.checker.selectedHoistedRoots().len);
}

test "hoist exact non-concrete internal local repro roots are pruned" {
    var test_env = try TestEnv.init("Test",
        \\main = |arg| {
        \\    _ = [
        \\        {
        \\            x = []
        \\            List.len(x).to_i64_wrap()
        \\        },
        \\        arg,
        \\    ]
        \\    arg
        \\}
    );
    defer test_env.deinit();

    try test_env.assertNoErrors();
    try std.testing.expectEqual(@as(usize, 0), test_env.checker.selectedHoistedRoots().len);
}

test "hoist nested block roots with non-concrete destructured internal binders are pruned" {
    var test_env = try TestEnv.init("Test",
        \\main = |arg| {
        \\    _ = [
        \\        {
        \\            { xs } = { xs: [] }
        \\            List.len(xs).to_i64_wrap()
        \\        },
        \\        arg,
        \\    ]
        \\    arg
        \\}
    );
    defer test_env.deinit();

    try test_env.assertNoErrors();
    try std.testing.expectEqual(@as(usize, 0), test_env.checker.selectedHoistedRoots().len);
}

test "hoist match roots with non-concrete contextual binders are pruned" {
    var test_env = try TestEnv.init("Test",
        \\main = |arg| {
        \\    _ = [
        \\        match [] {
        \\            xs => List.len(xs).to_i64_wrap()
        \\        },
        \\        arg,
        \\    ]
        \\    arg
        \\}
    );
    defer test_env.deinit();

    try expectOnlyComptimeConditionWarnings(&test_env, 1);
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

test "hoist roots are not selected inside top-level expects" {
    var test_env = try TestEnv.init("Test",
        \\expensive = |n| n + 1.I64
        \\
        \\expect {
        \\    result = expensive(41.I64)
        \\    result == 42.I64
        \\}
    );
    defer test_env.deinit();

    try test_env.assertNoErrors();
    // repro for https://github.com/roc-lang/roc/issues/9747
    try std.testing.expectEqual(@as(usize, 0), test_env.checker.selectedHoistedRoots().len);
}

test "hoist roots are not selected for runtime-controlled branch bodies" {
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
    try std.testing.expectEqual(@as(usize, 0), test_env.checker.selectedHoistedRoots().len);
}

test "hoist roots selected for whole closed conditional expressions" {
    var test_env = try TestEnv.init("Test",
        \\main = |_| if 1.I64 == 1.I64 {
        \\    1.I64 + 2.I64
        \\} else {
        \\    3.I64 + 4.I64
        \\}
    );
    defer test_env.deinit();

    try expectOnlyComptimeConditionWarnings(&test_env, 1);
    const roots = test_env.checker.selectedHoistedRoots();
    try std.testing.expectEqual(@as(usize, 1), roots.len);
    try std.testing.expectEqual(@as(?CIR.Pattern.Idx, null), roots[0].pattern);
    try expectExprTag(&test_env, roots[0].expr, .e_if);
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

test "hoist roots are not selected for dict pseudo-seed dependent values" {
    var test_env = try TestEnv.init("Test",
        \\main! = || {
        \\    dict = Dict.single("a", "b")
        \\    _ = Dict.get(dict, "a")
        \\    {}
        \\}
    );
    defer test_env.deinit();

    try test_env.assertNoErrors();
    try std.testing.expectEqual(@as(usize, 0), test_env.checker.selectedHoistedRoots().len);
}
