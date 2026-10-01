//! Tests that `expect` bodies cannot move control flow outside the `expect`.

const std = @import("std");
const testing = std.testing;

const base = @import("base");
const parse = @import("parse");
const CIR = @import("../CIR.zig");
const Can = @import("../Can.zig");
const ModuleEnv = @import("../ModuleEnv.zig");
const BuiltinTestContext = @import("BuiltinTestContext.zig").BuiltinTestContext;
const CoreCtx = @import("ctx").CoreCtx;

const ControlFlowKind = @FieldType(CIR.Diagnostic, "control_flow_in_expect").Kind;

const ExpectTestError = std.mem.Allocator.Error || error{
    TestExpectedEqual,
};

const Outcome = struct {
    try_suffix: usize = 0,
    return_keyword: usize = 0,
    break_keyword: usize = 0,
    return_outside_fn: usize = 0,
    break_outside_loop: usize = 0,
    var_reassigned_in_expect: usize = 0,
    var_across_function_boundary: usize = 0,
    expect_err_nodes: usize = 0,
};

fn canonicalizeModule(source: []const u8) ExpectTestError!Outcome {
    const allocator = testing.allocator;

    var builtin_ctx = try BuiltinTestContext.init(allocator);
    defer builtin_ctx.deinit();

    var env = try ModuleEnv.init(allocator, source);
    defer env.deinit();
    try env.initCIRFields("Test");

    const ast = try parse.file(allocator, &env.common);
    defer ast.deinit();
    try testing.expectEqual(@as(usize, 0), ast.parse_diagnostics.items.len);

    const roc_ctx = CoreCtx.testing(allocator, allocator);
    var can = try Can.initModule(roc_ctx, &env, ast, builtin_ctx.canInitContext());
    defer can.deinit();

    try can.canonicalizeFile();

    var outcome = Outcome{};
    const diagnostics = try env.getDiagnostics();
    defer allocator.free(diagnostics);
    // Unrelated warnings (e.g. shadowing) don't affect what these tests check.
    for (diagnostics) |diag| {
        if (diag == .control_flow_in_expect) {
            switch (diag.control_flow_in_expect.kind) {
                .try_suffix => outcome.try_suffix += 1,
                .return_keyword => outcome.return_keyword += 1,
                .break_keyword => outcome.break_keyword += 1,
            }
        } else if (diag == .return_outside_fn) {
            outcome.return_outside_fn += 1;
        } else if (diag == .var_reassigned_in_expect) {
            outcome.var_reassigned_in_expect += 1;
        } else if (diag == .var_across_function_boundary) {
            outcome.var_across_function_boundary += 1;
        } else if (diag == .break_outside_loop) {
            outcome.break_outside_loop += 1;
        }
    }

    var raw_node_idx: u32 = 0;
    while (raw_node_idx < env.store.nodes.len()) : (raw_node_idx += 1) {
        const node_idx: CIR.Node.Idx = @enumFromInt(raw_node_idx);
        if (env.store.nodes.get(node_idx).tag == .expr_expect_err) outcome.expect_err_nodes += 1;
    }

    return outcome;
}

test "try suffix in an inline expect statement is a compile error" {
    const outcome = try canonicalizeModule(
        \\main! = |_args| {
        \\    expect {
        \\        n = I64.from_str("not a number")?
        \\        n == 42
        \\    }
        \\    Ok({})
        \\}
    );
    try testing.expectEqual(Outcome{ .try_suffix = 1 }, outcome);
}

test "try suffix in an inline expect as the block's final expression is a compile error" {
    const outcome = try canonicalizeModule(
        \\f = |x| {
        \\    _y = x
        \\    expect I64.from_str("1")? == 1
        \\}
    );
    try testing.expectEqual(Outcome{ .try_suffix = 1 }, outcome);
}

test "binary try operator in an inline expect is a compile error" {
    const outcome = try canonicalizeModule(
        \\f = |x| {
        \\    expect (I64.from_str(x) ? BadNum) == 1
        \\    Ok(x)
        \\}
    );
    try testing.expectEqual(Outcome{ .try_suffix = 1 }, outcome);
}

test "try suffix in a top-level expect fails the expect" {
    const outcome = try canonicalizeModule(
        \\expect I64.from_str("1")? == 1
        \\
        \\main = {}
    );
    try testing.expectEqual(Outcome{ .expect_err_nodes = 1 }, outcome);
}

test "return in an inline expect is a compile error" {
    const outcome = try canonicalizeModule(
        \\f = |x| {
        \\    expect {
        \\        return x
        \\    }
        \\    x
        \\}
    );
    try testing.expectEqual(Outcome{ .return_keyword = 1 }, outcome);
}

test "return statement in an inline expect is a compile error" {
    const outcome = try canonicalizeModule(
        \\f = |x| {
        \\    expect {
        \\        if x == 1 {
        \\            return x
        \\        }
        \\        x == 2
        \\    }
        \\    x
        \\}
    );
    try testing.expectEqual(Outcome{ .return_keyword = 1 }, outcome);
}

test "return in a top-level expect is a compile error" {
    const outcome = try canonicalizeModule(
        \\expect {
        \\    return 1
        \\}
        \\
        \\main = {}
    );
    try testing.expectEqual(Outcome{ .return_keyword = 1 }, outcome);
}

test "return and try suffix inside a lambda inside an expect are allowed" {
    const outcome = try canonicalizeModule(
        \\f = |x| {
        \\    expect {
        \\        g = |s| {
        \\            if s == "" {
        \\                return Ok(0)
        \\            }
        \\            n = I64.from_str(s)?
        \\            Ok(n)
        \\        }
        \\        g(x) == Ok(1)
        \\    }
        \\    x
        \\}
        \\
        \\expect {
        \\    g = |s| Ok(I64.from_str(s)?)
        \\    g("1") == Ok(1)
        \\}
    );
    try testing.expectEqual(Outcome{}, outcome);
}

test "break in an expect cannot exit an enclosing loop" {
    const outcome = try canonicalizeModule(
        \\f = |xs| {
        \\    var $count = 0
        \\    for x in xs {
        \\        expect {
        \\            if x == 1 {
        \\                break
        \\            }
        \\            x != 2
        \\        }
        \\        $count = $count + 1
        \\    }
        \\    $count
        \\}
    );
    try testing.expectEqual(Outcome{ .break_keyword = 1 }, outcome);
}

test "break in a loop inside an expect is allowed" {
    const outcome = try canonicalizeModule(
        \\f = |xs| {
        \\    expect {
        \\        var $count = 0
        \\        for x in xs {
        \\            if x == 1 {
        \\                break
        \\            }
        \\            $count = $count + 1
        \\        }
        \\        $count < 10
        \\    }
        \\    xs
        \\}
    );
    try testing.expectEqual(Outcome{}, outcome);
}

test "try suffix in an expect nested inside a lambda inside a top-level expect is a compile error" {
    const outcome = try canonicalizeModule(
        \\expect {
        \\    g = |s| {
        \\        expect I64.from_str(s)? == 1
        \\        s
        \\    }
        \\    g("1") == "1"
        \\}
        \\
        \\main = {}
    );
    try testing.expectEqual(Outcome{ .try_suffix = 1 }, outcome);
}

test "inline expect cannot reassign a var declared outside it" {
    const outcome = try canonicalizeModule(
        \\main! = |_args| {
        \\    var $count = 0
        \\    expect {
        \\        $count = $count + 1
        \\        $count == 1
        \\    }
        \\    Ok($count)
        \\}
    );
    try testing.expectEqual(Outcome{ .var_reassigned_in_expect = 1 }, outcome);
}

test "expect whose body is only a reassignment cannot reassign a var declared outside it" {
    const outcome = try canonicalizeModule(
        \\f = |x| {
        \\    var $count = x
        \\    expect {
        \\        $count = 1
        \\    }
        \\    $count
        \\}
    );
    try testing.expectEqual(Outcome{ .var_reassigned_in_expect = 1 }, outcome);
}

test "inline expect at the end of a block cannot reassign a var declared outside it" {
    const outcome = try canonicalizeModule(
        \\f = |x| {
        \\    var $count = x
        \\    $count = $count + 1
        \\    expect {
        \\        $count = 2
        \\        $count == 2
        \\    }
        \\}
    );
    try testing.expectEqual(Outcome{ .var_reassigned_in_expect = 1 }, outcome);
}

test "expect can read a var declared outside it" {
    const outcome = try canonicalizeModule(
        \\f = |x| {
        \\    var $count = x
        \\    $count = $count + 1
        \\    expect $count > x
        \\    $count
        \\}
    );
    try testing.expectEqual(Outcome{}, outcome);
}

test "var reassigned after an expect is allowed" {
    const outcome = try canonicalizeModule(
        \\f = |x| {
        \\    var $count = x
        \\    expect $count == x
        \\    $count = $count + 1
        \\    $count
        \\}
    );
    try testing.expectEqual(Outcome{}, outcome);
}

test "expect can reassign a var declared inside it, at any nesting depth" {
    const outcome = try canonicalizeModule(
        \\f = |xs| {
        \\    expect {
        \\        var $total = 0
        \\        $total = $total + 1
        \\        if $total > 0 {
        \\            $total = $total + 1
        \\        }
        \\        for x in xs {
        \\            $total = $total + x
        \\        }
        \\        $total > 0
        \\    }
        \\    expect {
        \\        var $last = 0
        \\        $last = 1
        \\    }
        \\    xs
        \\}
        \\
        \\expect {
        \\    var $n = 0
        \\    $n = $n + 1
        \\    $n == 1
        \\}
    );
    try testing.expectEqual(Outcome{}, outcome);
}

test "loop inside an expect cannot reassign a var declared outside the expect" {
    const outcome = try canonicalizeModule(
        \\f = |xs| {
        \\    var $total = 0
        \\    expect {
        \\        for x in xs {
        \\            $total = $total + x
        \\        }
        \\        True
        \\    }
        \\    $total
        \\}
    );
    try testing.expectEqual(Outcome{ .var_reassigned_in_expect = 1 }, outcome);
}

test "expect inside a loop cannot reassign a var declared in the loop body" {
    const outcome = try canonicalizeModule(
        \\f = |xs| {
        \\    for x in xs {
        \\        var $seen = x
        \\        expect {
        \\            $seen = 0
        \\        }
        \\        $seen = $seen + 1
        \\    }
        \\    xs
        \\}
    );
    try testing.expectEqual(Outcome{ .var_reassigned_in_expect = 1 }, outcome);
}

test "var declared inside an expect shadowing an outer var can be reassigned" {
    const outcome = try canonicalizeModule(
        \\f = |x| {
        \\    var $count = x
        \\    expect {
        \\        var $count = 0
        \\        $count = $count + 1
        \\        $count == 1
        \\    }
        \\    $count
        \\}
    );
    try testing.expectEqual(Outcome{}, outcome);
}

test "for and match binders named like an outer var are not reassignments" {
    const outcome = try canonicalizeModule(
        \\f = |xs| {
        \\    var $x = 0
        \\    expect {
        \\        for $x in xs {
        \\            _y = $x
        \\        }
        \\        match xs {
        \\            [$x] => $x > 0
        \\            _ => True
        \\        }
        \\    }
        \\    $x
        \\}
    );
    try testing.expectEqual(Outcome{}, outcome);
}

test "destructuring in an expect cannot reassign a var declared outside it" {
    const outcome = try canonicalizeModule(
        \\f = |x| {
        \\    var $a = x
        \\    expect {
        \\        ($a, b) = (1, 2)
        \\        $a + b == 3
        \\    }
        \\    $a
        \\}
    );
    try testing.expectEqual(Outcome{ .var_reassigned_in_expect = 1 }, outcome);
}

test "destructuring in an expect can reassign a var declared inside it" {
    const outcome = try canonicalizeModule(
        \\f = |x| {
        \\    expect {
        \\        var $a = x
        \\        ($a, b) = (1, 2)
        \\        $a + b == 3
        \\    }
        \\    x
        \\}
    );
    try testing.expectEqual(Outcome{}, outcome);
}

test "lambda inside an expect reports the function boundary, not the expect" {
    const outcome = try canonicalizeModule(
        \\f = |x| {
        \\    var $count = x
        \\    expect {
        \\        bump = |_| {
        \\            $count = $count + 1
        \\            $count
        \\        }
        \\        bump({}) > 0
        \\    }
        \\    $count
        \\}
    );
    try testing.expectEqual(Outcome{ .var_across_function_boundary = 1 }, outcome);
}

test "lambda inside an expect can reassign its own var" {
    const outcome = try canonicalizeModule(
        \\f = |x| {
        \\    expect {
        \\        count_up = |n| {
        \\            var $i = 0
        \\            $i = $i + n
        \\            $i
        \\        }
        \\        count_up(x) == x
        \\    }
        \\    x
        \\}
    );
    try testing.expectEqual(Outcome{}, outcome);
}

test "expect inside a lambda inside an expect uses its own boundary" {
    const outcome = try canonicalizeModule(
        \\expect {
        \\    g = |s| {
        \\        var $outer = s
        \\        expect {
        \\            var $inner = 0
        \\            $inner = 1
        \\            $outer = 2
        \\            $inner == 1
        \\        }
        \\        $outer = $outer + 1
        \\        $outer
        \\    }
        \\    g(1) == 2
        \\}
        \\
        \\main = {}
    );
    try testing.expectEqual(Outcome{ .var_reassigned_in_expect = 1 }, outcome);
}

test "expect nested in an expect cannot reassign a var declared in the outer one" {
    const outcome = try canonicalizeModule(
        \\f = |x| {
        \\    expect {
        \\        var $a = x
        \\        expect {
        \\            $a = 1
        \\        }
        \\        $a = $a + 1
        \\        $a > x
        \\    }
        \\    x
        \\}
    );
    try testing.expectEqual(Outcome{ .var_reassigned_in_expect = 1 }, outcome);
}

test "expect in a local associated block cannot reassign a var of the enclosing function" {
    const outcome = try canonicalizeModule(
        \\f = |x| {
        \\    var $count = x
        \\    Counter := [Counter].{
        \\        expect {
        \\            $count = 1
        \\        }
        \\    }
        \\    $count
        \\}
    );
    try testing.expectEqual(Outcome{ .var_reassigned_in_expect = 1 }, outcome);
}

test "control flow in expect diagnostics have reports" {
    const kinds = [_]ControlFlowKind{ .try_suffix, .return_keyword, .break_keyword };
    const titles = [_][]const u8{ "Try Operator In Expect", "Return In Expect", "Break In Expect" };
    var env = try ModuleEnv.init(testing.allocator, "x = 1");
    defer env.deinit();
    try env.initCIRFields("Test");
    for (kinds, titles) |kind, title| {
        var report = try env.diagnosticToReport(
            .{ .control_flow_in_expect = .{ .region = .{ .start = .{ .offset = 0 }, .end = .{ .offset = 1 } }, .kind = kind } },
            testing.allocator,
            "Test.roc",
        );
        defer report.deinit();
        try testing.expectEqualStrings(title, report.title);
    }
}

test "var reassigned in expect diagnostic has a report" {
    var env = try ModuleEnv.init(testing.allocator, "x = 1");
    defer env.deinit();
    try env.initCIRFields("Test");
    const region = base.Region{ .start = .{ .offset = 0 }, .end = .{ .offset = 1 } };
    var report = try env.diagnosticToReport(
        .{ .var_reassigned_in_expect = .{ .ident = try env.insertIdent(base.Ident.for_text("$count")), .region = region, .declaration_region = region } },
        testing.allocator,
        "Test.roc",
    );
    defer report.deinit();
    try testing.expectEqualStrings("Var Reassigned In Expect", report.title);
}
