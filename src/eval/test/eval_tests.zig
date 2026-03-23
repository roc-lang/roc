//! Data-driven eval test definitions for the parallel test runner.
//! Each entry corresponds to one `runExpect*` call from the original test files.
//! The parallel runner exercises every backend (interpreter, dev, wasm, llvm)
//! on each test and compares results.

const TestCase = @import("parallel_runner.zig").TestCase;
const RocDec = @import("builtins").dec.RocDec;

pub const tests = [_]TestCase{
    // --- proof of concept tests ---
    .{ .name = "dec: simple number", .source = "1", .expected = .{ .dec_val = 1 * RocDec.one_point_zero_i128 } },
    .{ .name = "dec: if-else true branch", .source = "if (1 == 1) 42 else 99", .expected = .{ .dec_val = 42 * RocDec.one_point_zero_i128 } },
    .{ .name = "dec: arithmetic", .source = "2 + 3 * 4", .expected = .{ .dec_val = 14 * RocDec.one_point_zero_i128 } },
    .{ .name = "bool: true literal", .source = "True", .expected = .{ .bool_val = true } },
    .{ .name = "bool: comparison", .source = "5 > 3", .expected = .{ .bool_val = true } },
    .{ .name = "str: hello", .source = "\"hello\"", .expected = .{ .str_val = "hello" } },
    .{ .name = "dec: 1.5", .source = "1.5", .expected = .{ .dec_val = 1500000000000000000 } },
    .{ .name = "f32: literal", .source = "1.5.F32", .expected = .{ .f32_val = 1.5 } },
    .{ .name = "f64: literal", .source = "2.5.F64", .expected = .{ .f64_val = 2.5 } },
    .{ .name = "err: crash", .source = "{ crash \"test feature\" 0 }", .expected = .{ .err_val = error.Crash } },
    .{ .name = "problem: undefined variable", .source = "undefinedVar", .expected = .{ .problem = {} } },

    // --- from eval_test.zig: eval simple number ---
    .{ .name = "eval simple number: 1", .source = "1", .expected = .{ .dec_val = 1 * RocDec.one_point_zero_i128 } },
    .{ .name = "eval simple number: 42", .source = "42", .expected = .{ .dec_val = 42 * RocDec.one_point_zero_i128 } },
    .{ .name = "eval simple number: -1234", .source = "-1234", .expected = .{ .dec_val = -1234 * RocDec.one_point_zero_i128 } },

    // --- from eval_test.zig: if-else ---
    .{ .name = "if-else: true branch", .source = "if (1 == 1) 42 else 99", .expected = .{ .dec_val = 42 * RocDec.one_point_zero_i128 } },
    .{ .name = "if-else: false branch", .source = "if (1 == 2) 42 else 99", .expected = .{ .dec_val = 99 * RocDec.one_point_zero_i128 } },
    .{ .name = "if-else: greater true", .source = "if (5 > 3) 100 else 200", .expected = .{ .dec_val = 100 * RocDec.one_point_zero_i128 } },
    .{ .name = "if-else: greater false", .source = "if (3 > 5) 100 else 200", .expected = .{ .dec_val = 200 * RocDec.one_point_zero_i128 } },

    // --- from eval_test.zig: nested if-else ---
    .{ .name = "nested if-else: both true", .source = "if (1 == 1) (if (2 == 2) 100 else 200) else 300", .expected = .{ .dec_val = 100 * RocDec.one_point_zero_i128 } },
    .{ .name = "nested if-else: outer true inner false", .source = "if (1 == 1) (if (2 == 3) 100 else 200) else 300", .expected = .{ .dec_val = 200 * RocDec.one_point_zero_i128 } },
    .{ .name = "nested if-else: outer false", .source = "if (1 == 2) (if (2 == 2) 100 else 200) else 300", .expected = .{ .dec_val = 300 * RocDec.one_point_zero_i128 } },

    // --- from eval_test.zig: eval single element record ---
    .{ .name = "eval single element record: x", .source = "{x: 42}.x", .expected = .{ .dec_val = 42 * RocDec.one_point_zero_i128 } },
    .{ .name = "eval single element record: foo", .source = "{foo: 100}.foo", .expected = .{ .dec_val = 100 * RocDec.one_point_zero_i128 } },
    .{ .name = "eval single element record: bar expr", .source = "{bar: 1 + 2}.bar", .expected = .{ .dec_val = 3 * RocDec.one_point_zero_i128 } },

    // --- from eval_test.zig: eval multi-field record ---
    .{ .name = "eval multi-field record: x", .source = "{x: 10, y: 20}.x", .expected = .{ .dec_val = 10 * RocDec.one_point_zero_i128 } },
    .{ .name = "eval multi-field record: y", .source = "{x: 10, y: 20}.y", .expected = .{ .dec_val = 20 * RocDec.one_point_zero_i128 } },
    .{ .name = "eval multi-field record: a", .source = "{a: 1, b: 2, c: 3}.a", .expected = .{ .dec_val = 1 * RocDec.one_point_zero_i128 } },
    .{ .name = "eval multi-field record: b", .source = "{a: 1, b: 2, c: 3}.b", .expected = .{ .dec_val = 2 * RocDec.one_point_zero_i128 } },
    .{ .name = "eval multi-field record: c", .source = "{a: 1, b: 2, c: 3}.c", .expected = .{ .dec_val = 3 * RocDec.one_point_zero_i128 } },

    // --- from eval_test.zig: nested record access ---
    .{ .name = "nested record access: outer.inner", .source = "{outer: {inner: 42}}.outer.inner", .expected = .{ .dec_val = 42 * RocDec.one_point_zero_i128 } },
    .{ .name = "nested record access: a.b.c", .source = "{a: {b: {c: 100}}}.a.b.c", .expected = .{ .dec_val = 100 * RocDec.one_point_zero_i128 } },

    // --- from eval_test.zig: record field order independence ---
    .{ .name = "record field order independence: sum", .source = "{x: 1, y: 2}.x + {y: 2, x: 1}.x", .expected = .{ .dec_val = 2 * RocDec.one_point_zero_i128 } },
    .{ .name = "record field order independence: b forward", .source = "{a: 10, b: 20, c: 30}.b", .expected = .{ .dec_val = 20 * RocDec.one_point_zero_i128 } },
    .{ .name = "record field order independence: b reordered", .source = "{c: 30, a: 10, b: 20}.b", .expected = .{ .dec_val = 20 * RocDec.one_point_zero_i128 } },

    // --- from eval_test.zig: arithmetic binops ---
    .{ .name = "arithmetic binops: add", .source = "1 + 2", .expected = .{ .dec_val = 3 * RocDec.one_point_zero_i128 } },
    .{ .name = "arithmetic binops: sub", .source = "5 - 3", .expected = .{ .dec_val = 2 * RocDec.one_point_zero_i128 } },
    .{ .name = "arithmetic binops: mul", .source = "4 * 5", .expected = .{ .dec_val = 20 * RocDec.one_point_zero_i128 } },
    .{ .name = "arithmetic binops: div", .source = "10 // 2", .expected = .{ .dec_val = 5 * RocDec.one_point_zero_i128 } },
    .{ .name = "arithmetic binops: mod", .source = "7 % 3", .expected = .{ .dec_val = 1 * RocDec.one_point_zero_i128 } },

    // --- from eval_test.zig: Dec division/modulo ---
    .{ .name = "simple Dec division - larger numbers", .source = "100 // 20", .expected = .{ .dec_val = 5 * RocDec.one_point_zero_i128 } },
    .{ .name = "simple Dec modulo - larger numbers", .source = "100 % 30", .expected = .{ .dec_val = 10 * RocDec.one_point_zero_i128 } },
    .{ .name = "Dec division result used in arithmetic", .source = "(100 // 20) + 1", .expected = .{ .dec_val = 6 * RocDec.one_point_zero_i128 } },

    // --- from eval_test.zig: comparison binops ---
    .{ .name = "comparison binops: less true", .source = "if 1 < 2 100 else 200", .expected = .{ .dec_val = 100 * RocDec.one_point_zero_i128 } },
    .{ .name = "comparison binops: less false", .source = "if 2 < 1 100 else 200", .expected = .{ .dec_val = 200 * RocDec.one_point_zero_i128 } },
    .{ .name = "comparison binops: greater true", .source = "if 5 > 3 100 else 200", .expected = .{ .dec_val = 100 * RocDec.one_point_zero_i128 } },
    .{ .name = "comparison binops: greater false", .source = "if 3 > 5 100 else 200", .expected = .{ .dec_val = 200 * RocDec.one_point_zero_i128 } },
    .{ .name = "comparison binops: leq true", .source = "if 10 <= 10 100 else 200", .expected = .{ .dec_val = 100 * RocDec.one_point_zero_i128 } },
    .{ .name = "comparison binops: leq false", .source = "if 10 <= 9 100 else 200", .expected = .{ .dec_val = 200 * RocDec.one_point_zero_i128 } },
    .{ .name = "comparison binops: geq true", .source = "if 10 >= 10 100 else 200", .expected = .{ .dec_val = 100 * RocDec.one_point_zero_i128 } },
    .{ .name = "comparison binops: geq false", .source = "if 9 >= 10 100 else 200", .expected = .{ .dec_val = 200 * RocDec.one_point_zero_i128 } },
    .{ .name = "comparison binops: eq true", .source = "if 5 == 5 100 else 200", .expected = .{ .dec_val = 100 * RocDec.one_point_zero_i128 } },
    .{ .name = "comparison binops: eq false", .source = "if 5 == 6 100 else 200", .expected = .{ .dec_val = 200 * RocDec.one_point_zero_i128 } },
    .{ .name = "comparison binops: neq true", .source = "if 5 != 6 100 else 200", .expected = .{ .dec_val = 100 * RocDec.one_point_zero_i128 } },
    .{ .name = "comparison binops: neq false", .source = "if 5 != 5 100 else 200", .expected = .{ .dec_val = 200 * RocDec.one_point_zero_i128 } },

    // --- from eval_test.zig: unary minus ---
    .{ .name = "unary minus: -5", .source = "-5", .expected = .{ .dec_val = -5 * RocDec.one_point_zero_i128 } },
    .{ .name = "unary minus: double neg", .source = "-(-10)", .expected = .{ .dec_val = 10 * RocDec.one_point_zero_i128 } },
    .{ .name = "unary minus: neg expr", .source = "-(3 + 4)", .expected = .{ .dec_val = -7 * RocDec.one_point_zero_i128 } },
    .{ .name = "unary minus: neg zero", .source = "-0", .expected = .{ .dec_val = 0 * RocDec.one_point_zero_i128 } },

    // --- from eval_test.zig: parentheses and precedence ---
    .{ .name = "precedence: mul before add", .source = "2 + 3 * 4", .expected = .{ .dec_val = 14 * RocDec.one_point_zero_i128 } },
    .{ .name = "precedence: parens override", .source = "(2 + 3) * 4", .expected = .{ .dec_val = 20 * RocDec.one_point_zero_i128 } },
    .{ .name = "precedence: left assoc sub", .source = "100 - 20 - 10", .expected = .{ .dec_val = 70 * RocDec.one_point_zero_i128 } },
    .{ .name = "precedence: parens sub", .source = "100 - (20 - 10)", .expected = .{ .dec_val = 90 * RocDec.one_point_zero_i128 } },

    // --- from eval_test.zig: operator associativity - addition ---
    .{ .name = "assoc addition: a+b+c", .source = "100 + 20 + 10", .expected = .{ .dec_val = 130 * RocDec.one_point_zero_i128 } },
    .{ .name = "assoc addition: a+(b+c)", .source = "100 + (20 + 10)", .expected = .{ .dec_val = 130 * RocDec.one_point_zero_i128 } },
    .{ .name = "assoc addition: chain", .source = "10 + 20 + 30 + 40", .expected = .{ .dec_val = 100 * RocDec.one_point_zero_i128 } },

    // --- from eval_test.zig: operator associativity - subtraction ---
    .{ .name = "assoc subtraction: a-b-c", .source = "100 - 20 - 10", .expected = .{ .dec_val = 70 * RocDec.one_point_zero_i128 } },
    .{ .name = "assoc subtraction: a-(b-c)", .source = "100 - (20 - 10)", .expected = .{ .dec_val = 90 * RocDec.one_point_zero_i128 } },
    .{ .name = "assoc subtraction: chain", .source = "100 - 50 - 25 - 5", .expected = .{ .dec_val = 20 * RocDec.one_point_zero_i128 } },
    .{ .name = "assoc subtraction: right grouped", .source = "100 - (50 - (25 - 5))", .expected = .{ .dec_val = 70 * RocDec.one_point_zero_i128 } },

    // --- from eval_test.zig: operator associativity - mixed add/sub ---
    .{ .name = "assoc mixed add/sub: 1-2+3", .source = "1 - 2 + 3", .expected = .{ .dec_val = 2 * RocDec.one_point_zero_i128 } },
    .{ .name = "assoc mixed add/sub: 5+3-2", .source = "5 + 3 - 2", .expected = .{ .dec_val = 6 * RocDec.one_point_zero_i128 } },
    .{ .name = "assoc mixed add/sub: chain", .source = "10 - 5 + 3 - 2", .expected = .{ .dec_val = 6 * RocDec.one_point_zero_i128 } },
    .{ .name = "assoc mixed add/sub: long chain", .source = "1 + 2 - 3 + 4 - 5", .expected = .{ .dec_val = -1 * RocDec.one_point_zero_i128 } },

    // --- from eval_test.zig: operator associativity - multiplication ---
    .{ .name = "assoc multiplication: a*b*c", .source = "2 * 3 * 4", .expected = .{ .dec_val = 24 * RocDec.one_point_zero_i128 } },
    .{ .name = "assoc multiplication: a*(b*c)", .source = "2 * (3 * 4)", .expected = .{ .dec_val = 24 * RocDec.one_point_zero_i128 } },
    .{ .name = "assoc multiplication: chain", .source = "2 * 3 * 4 * 5", .expected = .{ .dec_val = 120 * RocDec.one_point_zero_i128 } },

    // --- from eval_test.zig: operator associativity - division ---
    .{ .name = "assoc division: a//b//c", .source = "100 // 20 // 2", .expected = .{ .dec_val = 2 * RocDec.one_point_zero_i128 } },
    .{ .name = "assoc division: a//(b//c)", .source = "100 // (20 // 2)", .expected = .{ .dec_val = 10 * RocDec.one_point_zero_i128 } },
    .{ .name = "assoc division: chain left", .source = "80 // 8 // 2", .expected = .{ .dec_val = 5 * RocDec.one_point_zero_i128 } },
    .{ .name = "assoc division: chain right grouped", .source = "80 // (8 // 2)", .expected = .{ .dec_val = 20 * RocDec.one_point_zero_i128 } },

    // --- from eval_test.zig: operator associativity - modulo ---
    .{ .name = "assoc modulo: a%b%c", .source = "100 % 30 % 7", .expected = .{ .dec_val = 3 * RocDec.one_point_zero_i128 } },
    .{ .name = "assoc modulo: a%(b%c)", .source = "100 % (30 % 7)", .expected = .{ .dec_val = 0 * RocDec.one_point_zero_i128 } },
    .{ .name = "assoc modulo: chain left", .source = "50 % 20 % 6", .expected = .{ .dec_val = 4 * RocDec.one_point_zero_i128 } },
    .{ .name = "assoc modulo: chain right grouped", .source = "50 % (20 % 6)", .expected = .{ .dec_val = 0 * RocDec.one_point_zero_i128 } },

    // --- from eval_test.zig: operator associativity - mixed precedence ---
    .{ .name = "assoc mixed prec: add mul", .source = "2 + 3 * 4", .expected = .{ .dec_val = 14 * RocDec.one_point_zero_i128 } },
    .{ .name = "assoc mixed prec: mul add", .source = "2 * 3 + 4", .expected = .{ .dec_val = 10 * RocDec.one_point_zero_i128 } },
    .{ .name = "assoc mixed prec: sub mul", .source = "10 - 2 * 3", .expected = .{ .dec_val = 4 * RocDec.one_point_zero_i128 } },
    .{ .name = "assoc mixed prec: div add", .source = "100 // 5 + 10", .expected = .{ .dec_val = 30 * RocDec.one_point_zero_i128 } },
    .{ .name = "assoc mixed prec: div mod", .source = "100 // 5 % 3", .expected = .{ .dec_val = 2 * RocDec.one_point_zero_i128 } },

    // --- from eval_test.zig: operator associativity - edge cases ---
    .{ .name = "assoc edge: long sub chain", .source = "1000 - 100 - 50 - 25 - 10 - 5", .expected = .{ .dec_val = 810 * RocDec.one_point_zero_i128 } },
    .{ .name = "assoc edge: parens sub 50", .source = "(100 - 50)", .expected = .{ .dec_val = 50 * RocDec.one_point_zero_i128 } },
    .{ .name = "assoc edge: parens sub 20", .source = "(30 - 10)", .expected = .{ .dec_val = 20 * RocDec.one_point_zero_i128 } },
    .{ .name = "assoc edge: simple sub", .source = "50 - 20", .expected = .{ .dec_val = 30 * RocDec.one_point_zero_i128 } },
    .{ .name = "assoc edge: nested parens sub", .source = "100 - (50 - 30) - 10", .expected = .{ .dec_val = 70 * RocDec.one_point_zero_i128 } },
    .{ .name = "assoc edge: both sides parens sub", .source = "(100 - 50) - (30 - 10)", .expected = .{ .dec_val = 30 * RocDec.one_point_zero_i128 } },
    .{ .name = "assoc edge: div chain", .source = "80 // 4 // 2", .expected = .{ .dec_val = 10 * RocDec.one_point_zero_i128 } },
    .{ .name = "assoc edge: mod chain", .source = "1000 % 300 % 40 % 7", .expected = .{ .dec_val = 6 * RocDec.one_point_zero_i128 } },

    // --- from eval_test.zig: comparison operators - non-associative ---
    .{ .name = "comparison non-assoc: gt true", .source = "(5 > 3)", .expected = .{ .bool_val = true } },
    .{ .name = "comparison non-assoc: lt true", .source = "(10 < 20)", .expected = .{ .bool_val = true } },
    .{ .name = "comparison non-assoc: geq true", .source = "(5 >= 5)", .expected = .{ .bool_val = true } },
    .{ .name = "comparison non-assoc: leq false", .source = "(10 <= 9)", .expected = .{ .bool_val = false } },

    // --- from eval_test.zig: operator associativity - documentation ---
    .{ .name = "assoc doc: left sub", .source = "8 - 4 - 2", .expected = .{ .dec_val = 2 * RocDec.one_point_zero_i128 } },
    .{ .name = "assoc doc: left div", .source = "16 // 4 // 2", .expected = .{ .dec_val = 2 * RocDec.one_point_zero_i128 } },
    .{ .name = "assoc doc: bool and", .source = "(5 > 3) and (3 > 1)", .expected = .{ .bool_val = true } },

    // --- from eval_test.zig: error test - divide by zero ---
    .{ .name = "error: divide by zero", .source = "5 // 0", .expected = .{ .err_val = error.DivisionByZero } },
    .{ .name = "error: modulo by zero", .source = "10 % 0", .expected = .{ .err_val = error.DivisionByZero } },

    // --- from eval_test.zig: simple lambda with if-else ---
    .{ .name = "simple lambda with if-else: positive", .source = "(|x| if x > 0.I64 x else 0.I64)(5.I64)", .expected = .{ .i64_val = 5 } },
    .{ .name = "simple lambda with if-else: negative", .source = "(|x| if x > 0.I64 x else 0.I64)(-3.I64)", .expected = .{ .i64_val = 0 } },

    // --- from eval_test.zig: crash in else branch inside lambda ---
    .{ .name = "crash in else branch inside lambda",
        .source =
            \\(|x| if x > 0.I64 x else {
            \\    crash "crash in else!"
            \\    0.I64
            \\})(-5.I64)
        ,
        .expected = .{ .err_val = error.Crash },
    },

    // --- from eval_test.zig: crash NOT taken when condition true ---
    .{ .name = "crash NOT taken when condition true",
        .source =
            \\(|x| if x > 0.I64 x else {
            \\    crash "this should not execute"
            \\    0.I64
            \\})(10.I64)
        ,
        .expected = .{ .i64_val = 10 },
    },

    // --- from eval_test.zig: error test - crash statement ---
    .{ .name = "error test - crash statement: basic",
        .source =
            \\{
            \\    crash "test"
            \\    0
            \\}
        ,
        .expected = .{ .err_val = error.Crash },
    },
    .{ .name = "error test - crash statement: with message",
        .source =
            \\{
            \\    crash "This is a crash statement"
            \\    42
            \\}
        ,
        .expected = .{ .err_val = error.Crash },
    },

    // --- from eval_test.zig: inline expect statement fails ---
    .{ .name = "inline expect statement fails",
        .source =
            \\{
            \\    expect 1 == 2
            \\    {}
            \\}
        ,
        .expected = .{ .err_val = error.Crash },
    },

    // --- from eval_test.zig: inline expect statement passes ---
    .{ .name = "inline expect statement passes",
        .source =
            \\{
            \\    expect 1 == 1
            \\    42
            \\}
        ,
        .expected = .{ .dec_val = 42 * RocDec.one_point_zero_i128 },
    },

    // --- from eval_test.zig: simple lambdas ---
    .{ .name = "simple lambdas: x+1", .source = "(|x| x + 1.I64)(5.I64)", .expected = .{ .i64_val = 6 } },
    .{ .name = "simple lambdas: x*2+1", .source = "(|x| x * 2.I64 + 1.I64)(10.I64)", .expected = .{ .i64_val = 21 } },
    .{ .name = "simple lambdas: x-3", .source = "(|x| x - 3.I64)(8.I64)", .expected = .{ .i64_val = 5 } },
    .{ .name = "simple lambdas: 100-x", .source = "(|x| 100.I64 - x)(25.I64)", .expected = .{ .i64_val = 75 } },
    .{ .name = "simple lambdas: ignore arg", .source = "(|_x| 5.I64)(99.I64)", .expected = .{ .i64_val = 5 } },
    .{ .name = "simple lambdas: x+x", .source = "(|x| x + x)(7.I64)", .expected = .{ .i64_val = 14 } },

    // --- from eval_test.zig: multi-parameter lambdas ---
    .{ .name = "multi-param lambdas: x+y", .source = "(|x, y| x + y)(3.I64, 4.I64)", .expected = .{ .i64_val = 7 } },
    .{ .name = "multi-param lambdas: x*y", .source = "(|x, y| x * y)(5.I64, 6.I64)", .expected = .{ .i64_val = 30 } },
    .{ .name = "multi-param lambdas: a+b+c", .source = "(|a, b, c| a + b + c)(1.I64, 2.I64, 3.I64)", .expected = .{ .i64_val = 6 } },

    // --- from eval_test.zig: lambdas with if-then bodies ---
    .{ .name = "lambdas with if-then: positive", .source = "(|x| if x > 0.I64 x else 0.I64)(5.I64)", .expected = .{ .i64_val = 5 } },
    .{ .name = "lambdas with if-then: negative", .source = "(|x| if x > 0.I64 x else 0.I64)(-3.I64)", .expected = .{ .i64_val = 0 } },
    .{ .name = "lambdas with if-then: zero to one", .source = "(|x| if x == 0.I64 1.I64 else x)(0.I64)", .expected = .{ .i64_val = 1 } },
    .{ .name = "lambdas with if-then: non-zero", .source = "(|x| if x == 0.I64 1.I64 else x)(42.I64)", .expected = .{ .i64_val = 42 } },

    // --- from eval_test.zig: lambdas with unary minus ---
    .{ .name = "lambdas unary minus: negate positive", .source = "(|x| -x)(5.I64)", .expected = .{ .i64_val = -5 } },
    .{ .name = "lambdas unary minus: negate zero", .source = "(|x| -x)(0.I64)", .expected = .{ .i64_val = 0 } },
    .{ .name = "lambdas unary minus: negate negative", .source = "(|x| -x)(-3.I64)", .expected = .{ .i64_val = 3 } },
    .{ .name = "lambdas unary minus: constant neg", .source = "(|_x| -5.I64)(999.I64)", .expected = .{ .i64_val = -5 } },
    .{ .name = "lambdas unary minus: if true neg", .source = "(|x| if True -x else 0.I64)(5.I64)", .expected = .{ .i64_val = -5 } },
    .{ .name = "lambdas unary minus: if true neg const", .source = "(|x| if True -10.I64 else x)(999.I64)", .expected = .{ .i64_val = -10 } },

    // --- from eval_test.zig: lambdas closures ---
    .{ .name = "lambdas closures: curried mul", .source = "(|a| |b| a * b)(5.I64)(10.I64)", .expected = .{ .i64_val = 50 } },
    .{ .name = "lambdas closures: triple curried", .source = "(((|a| |b| |c| a + b + c)(100.I64))(20.I64))(3.I64)", .expected = .{ .i64_val = 123 } },
    .{ .name = "lambdas closures: multi-param returning lambda", .source = "(|a, b, c| |d| a + b + c + d)(10.I64, 20.I64, 5.I64)(7.I64)", .expected = .{ .i64_val = 42 } },
    .{ .name = "lambdas closures: nested captures", .source = "(|y| (|x| (|z| x + y + z)(3.I64))(2.I64))(1.I64)", .expected = .{ .i64_val = 6 } },

    // --- from eval_test.zig: lambdas with capture ---
    .{ .name = "lambdas with capture: x+y",
        .source =
            \\{
            \\    x = 10.I64
            \\    f = |y| x + y
            \\    f(5.I64)
            \\}
        ,
        .expected = .{ .i64_val = 15 },
    },
    .{ .name = "lambdas with capture: x+y+z",
        .source =
            \\{
            \\    x = 20.I64
            \\    y = 30.I64
            \\    f = |z| x + y + z
            \\    f(10.I64)
            \\}
        ,
        .expected = .{ .i64_val = 60 },
    },

    // --- from eval_test.zig: closure with many captures (struct_captures) ---
    .{ .name = "closure with many captures (struct_captures)",
        .source =
            \\{
            \\    a = 100.I64
            \\    b = 200.I64
            \\    c = 300.I64
            \\    d = 400.I64
            \\    f = |n| a + b + c + d + n
            \\    f(5.I64)
            \\}
        ,
        .expected = .{ .i64_val = 1005 },
    },

    // --- from eval_test.zig: lambdas nested closures ---
    .{ .name = "lambdas nested closures",
        .source =
            \\(((|a| {
            \\    a_loc = a * 2.I64
            \\    |b| {
            \\        b_loc = a_loc + b
            \\        |c| b_loc + c
            \\    }
            \\})(100.I64))(20.I64))(3.I64)
        ,
        .expected = .{ .i64_val = 223 },
    },

    // --- from eval_test.zig: integer type evaluation ---
    .{ .name = "integer type evaluation: U8", .source = "255.U8", .expected = .{ .i64_val = 255 } },
    .{ .name = "integer type evaluation: I32", .source = "42.I32", .expected = .{ .i64_val = 42 } },
    .{ .name = "integer type evaluation: I64", .source = "123.I64", .expected = .{ .i64_val = 123 } },

    // --- from eval_test.zig: runtime eval helper auto-imports ---
    .{ .name = "runtime eval helper: I64 add", .source = "0.I64 + 42.I64", .expected = .{ .i64_val = 42 } },
    .{ .name = "runtime eval helper: Dec", .source = "3.14.Dec", .expected = .{ .dec_val = 3_140_000_000_000_000_000 } },

    // --- from eval_test.zig: decimal arithmetic with negative values ---
    .{ .name = "dec arithmetic: -1.5", .source = "-1.5.Dec", .expected = .{ .dec_val = -1_500_000_000_000_000_000 } },
    .{ .name = "dec arithmetic: 1.5", .source = "1.5.Dec", .expected = .{ .dec_val = 1_500_000_000_000_000_000 } },
    .{ .name = "dec arithmetic: -1.5 + 2.5", .source = "-1.5.Dec + 2.5.Dec", .expected = .{ .dec_val = 1_000_000_000_000_000_000 } },
    .{ .name = "dec arithmetic: 0.0 - 1.0", .source = "0.0.Dec - 1.0.Dec", .expected = .{ .dec_val = -1_000_000_000_000_000_000 } },

    // --- from eval_test.zig: comprehensive integer literal formats ---
    .{ .name = "int formats: 0.U8", .source = "0.U8", .expected = .{ .i64_val = 0 } },
    .{ .name = "int formats: 255.U8", .source = "255.U8", .expected = .{ .i64_val = 255 } },
    .{ .name = "int formats: 1000.U16", .source = "1000.U16", .expected = .{ .i64_val = 1000 } },
    .{ .name = "int formats: 65535.U16", .source = "65535.U16", .expected = .{ .i64_val = 65535 } },
    .{ .name = "int formats: 100000.U32", .source = "100000.U32", .expected = .{ .i64_val = 100000 } },
    .{ .name = "int formats: 999999999.U64", .source = "999999999.U64", .expected = .{ .i64_val = 999999999 } },
    .{ .name = "int formats: -128.I8", .source = "-128.I8", .expected = .{ .i64_val = -128 } },
    .{ .name = "int formats: 127.I8", .source = "127.I8", .expected = .{ .i64_val = 127 } },
    .{ .name = "int formats: -32768.I16", .source = "-32768.I16", .expected = .{ .i64_val = -32768 } },
    .{ .name = "int formats: 32767.I16", .source = "32767.I16", .expected = .{ .i64_val = 32767 } },
    .{ .name = "int formats: -2147483648.I32", .source = "-2147483648.I32", .expected = .{ .i64_val = -2147483648 } },
    .{ .name = "int formats: 2147483647.I32", .source = "2147483647.I32", .expected = .{ .i64_val = 2147483647 } },
    .{ .name = "int formats: -999999999.I64", .source = "-999999999.I64", .expected = .{ .i64_val = -999999999 } },
    .{ .name = "int formats: 999999999.I64", .source = "999999999.I64", .expected = .{ .i64_val = 999999999 } },
    .{ .name = "int formats: default 42", .source = "42", .expected = .{ .dec_val = 42 * RocDec.one_point_zero_i128 } },
    .{ .name = "int formats: default -1234", .source = "-1234", .expected = .{ .dec_val = -1234 * RocDec.one_point_zero_i128 } },
    .{ .name = "int formats: default 0", .source = "0", .expected = .{ .dec_val = 0 * RocDec.one_point_zero_i128 } },

    // --- from eval_test.zig: hexadecimal and binary integer literals ---
    .{ .name = "hex/bin: 0xFF", .source = "0xFF", .expected = .{ .dec_val = 255 * RocDec.one_point_zero_i128 } },
    .{ .name = "hex/bin: 0x10", .source = "0x10", .expected = .{ .dec_val = 16 * RocDec.one_point_zero_i128 } },
    .{ .name = "hex/bin: 0xDEADBEEF", .source = "0xDEADBEEF", .expected = .{ .dec_val = 3735928559 * RocDec.one_point_zero_i128 } },
    .{ .name = "hex/bin: 0b1010", .source = "0b1010", .expected = .{ .dec_val = 10 * RocDec.one_point_zero_i128 } },
    .{ .name = "hex/bin: 0b11111111", .source = "0b11111111", .expected = .{ .dec_val = 255 * RocDec.one_point_zero_i128 } },
    .{ .name = "hex/bin: 0b0", .source = "0b0", .expected = .{ .dec_val = 0 * RocDec.one_point_zero_i128 } },

    // --- from eval_test.zig: string refcount tests ---
    .{ .name = "string refcount - basic literal", .source = "\"Hello, World!\"", .expected = .{ .str_val = "Hello, World!" } },
    .{ .name = "polymorphic identity function",
        .source =
            \\{
            \\    identity = |val| val
            \\    num = identity(5)
            \\    str = identity("Hello")
            \\    if (num > 0) str else ""
            \\}
        ,
        .expected = .{ .str_val = "Hello" },
    },
    .{ .name = "direct polymorphic function usage",
        .source =
            \\{
            \\    id = |x| x
            \\
            \\    # Direct calls to identity with different types
            \\    num1 = id(10)
            \\    str1 = id("Test")
            \\    num2 = id(20)
            \\
            \\    # Verify all values are correct
            \\    if (num1 == 10)
            \\        if (num2 == 20)
            \\            str1
            \\        else
            \\            "Failed2"
            \\    else
            \\        "Failed1"
            \\}
        ,
        .expected = .{ .str_val = "Test" },
    },
    .{ .name = "multiple polymorphic instantiations",
        .source =
            \\{
            \\    id = |x| x
            \\
            \\    # Test polymorphic identity with different types
            \\    num1 = id(42)
            \\    str1 = id("Hello")
            \\    num2 = id(100)
            \\
            \\    # Verify all results
            \\    if (num1 == 42)
            \\        if (num2 == 100)
            \\            str1
            \\        else
            \\            "Failed2"
            \\    else
            \\        "Failed1"
            \\}
        ,
        .expected = .{ .str_val = "Hello" },
    },
    .{ .name = "string refcount - large string literal", .source = "\"This is a very long string that definitely exceeds the small string optimization limit in RocStr and will require heap allocation with reference counting\"", .expected = .{ .str_val = "This is a very long string that definitely exceeds the small string optimization limit in RocStr and will require heap allocation with reference counting" } },
    .{ .name = "string refcount - heap allocated string", .source = "\"This is a very long string that definitely exceeds the small string optimization limit and requires heap allocation\"", .expected = .{ .str_val = "This is a very long string that definitely exceeds the small string optimization limit and requires heap allocation" } },
    .{ .name = "string refcount - small string optimization", .source = "\"Small string test\"", .expected = .{ .str_val = "Small string test" } },
    .{ .name = "string refcount - empty string", .source = "\"\"", .expected = .{ .str_val = "" } },
    .{ .name = "string refcount - boundary case 25 bytes", .source = "\"1234567890123456789012345\"", .expected = .{ .str_val = "1234567890123456789012345" } },
    .{ .name = "string refcount - max small string 23 bytes", .source = "\"12345678901234567890123\"", .expected = .{ .str_val = "12345678901234567890123" } },
    .{ .name = "string refcount - conditional strings", .source = "if True \"This is a large string that exceeds small string optimization\" else \"Short\"", .expected = .{ .str_val = "This is a large string that exceeds small string optimization" } },
    .{ .name = "string refcount - simpler record test", .source = "{foo: 42}.foo", .expected = .{ .dec_val = 42 * RocDec.one_point_zero_i128 } },
    .{ .name = "string refcount - mixed string sizes", .source = "if False \"Small\" else \"This is a very long string that definitely exceeds the small string optimization limit and requires heap allocation\"", .expected = .{ .str_val = "This is a very long string that definitely exceeds the small string optimization limit and requires heap allocation" } },
    .{ .name = "string refcount - nested conditionals with strings", .source = "if True (if False \"Inner small\" else \"Inner large string that exceeds small string optimization\") else \"Outer\"", .expected = .{ .str_val = "Inner large string that exceeds small string optimization" } },
    .{ .name = "string refcount - record field access small string", .source = "{foo: \"Hello\"}.foo", .expected = .{ .str_val = "Hello" } },
    .{ .name = "string refcount - record field access large string", .source = "{foo: \"This is a very long string that definitely exceeds the small string optimization limit\"}.foo", .expected = .{ .str_val = "This is a very long string that definitely exceeds the small string optimization limit" } },
    .{ .name = "string refcount - record with empty string", .source = "{empty: \"\"}.empty", .expected = .{ .str_val = "" } },
    .{ .name = "string refcount - simple integer closure", .source = "(|x| x)(42)", .expected = .{ .dec_val = 42 * RocDec.one_point_zero_i128 } },
    .{ .name = "string refcount - simple string closure", .source = "(|s| s)(\"Test\")", .expected = .{ .str_val = "Test" } },

    // --- from eval_test.zig: recursive factorial function ---
    .{ .name = "recursive factorial function",
        .source =
            \\{
            \\    factorial = |n|
            \\        if n <= 1
            \\            1
            \\        else
            \\            n * factorial(n - 1)
            \\    factorial(5)
            \\}
        ,
        .expected = .{ .dec_val = 120 * RocDec.one_point_zero_i128 },
    },

    // --- from eval_test.zig: anonymous record equality ---
    .{ .name = "anonymous record equality: same", .source = "{ x: 1, y: 2 } == { x: 1, y: 2 }", .expected = .{ .bool_val = true } },
    .{ .name = "anonymous record equality: different", .source = "{ x: 1, y: 2 } == { x: 1, y: 3 }", .expected = .{ .bool_val = false } },
    .{ .name = "anonymous record equality: field order", .source = "{ x: 1, y: 2 } == { y: 2, x: 1 }", .expected = .{ .bool_val = true } },

    // --- from eval_test.zig: anonymous tuple equality ---
    .{ .name = "anonymous tuple equality: same", .source = "(1, 2) == (1, 2)", .expected = .{ .bool_val = true } },
    .{ .name = "anonymous tuple equality: different", .source = "(1, 2) == (1, 3)", .expected = .{ .bool_val = false } },

    // --- from eval_test.zig: empty record equality ---
    .{ .name = "empty record equality", .source = "{} == {}", .expected = .{ .bool_val = true } },

    // --- from eval_test.zig: mutable record equality ---
    .{ .name = "mutable record equality",
        .source =
            \\{
            \\    var $x = { sum: 6 }
            \\    $x == { sum: 6 }
            \\}
        ,
        .expected = .{ .bool_val = true },
    },

    // --- from eval_test.zig: mutable record with rebind equality ---
    .{ .name = "mutable record with rebind equality",
        .source =
            \\{
            \\    var $x = { sum: 0 }
            \\    $x = { sum: 6 }
            \\    $x == { sum: 6 }
            \\}
        ,
        .expected = .{ .bool_val = true },
    },

    // --- from eval_test.zig: mutable record loop accumulator equality ---
    .{ .name = "mutable record loop accumulator equality",
        .source =
            \\{
            \\    var $acc = { sum: 0 }
            \\    for item in [1, 2, 3] {
            \\        $acc = { sum: $acc.sum + item }
            \\    }
            \\    $acc == { sum: 6 }
            \\}
        ,
        .expected = .{ .bool_val = true },
    },

    // --- from eval_test.zig: string field equality ---
    .{ .name = "string field equality: same", .source = "{ name: \"hello\" } == { name: \"hello\" }", .expected = .{ .bool_val = true } },
    .{ .name = "string field equality: different", .source = "{ name: \"hello\" } == { name: \"world\" }", .expected = .{ .bool_val = false } },

    // --- from eval_test.zig: nested record equality ---
    .{ .name = "nested record equality: same", .source = "{ a: { x: 1 }, b: 2 } == { a: { x: 1 }, b: 2 }", .expected = .{ .bool_val = true } },
    .{ .name = "nested record equality: inner diff", .source = "{ a: { x: 1 }, b: 2 } == { a: { x: 2 }, b: 2 }", .expected = .{ .bool_val = false } },
    .{ .name = "nested record equality: deep same", .source = "{ outer: { inner: { deep: 42 } } } == { outer: { inner: { deep: 42 } } }", .expected = .{ .bool_val = true } },
    .{ .name = "nested record equality: deep diff", .source = "{ outer: { inner: { deep: 42 } } } == { outer: { inner: { deep: 99 } } }", .expected = .{ .bool_val = false } },

    // --- from eval_test.zig: bool field equality ---
    .{ .name = "bool field equality: same true", .source = "{ flag: (1 == 1) } == { flag: (1 == 1) }", .expected = .{ .bool_val = true } },
    .{ .name = "bool field equality: diff", .source = "{ flag: (1 == 1) } == { flag: (1 != 1) }", .expected = .{ .bool_val = false } },

    // --- from eval_test.zig: nested tuple equality ---
    .{ .name = "nested tuple equality: same left", .source = "((1, 2), 3) == ((1, 2), 3)", .expected = .{ .bool_val = true } },
    .{ .name = "nested tuple equality: diff left", .source = "((1, 2), 3) == ((1, 9), 3)", .expected = .{ .bool_val = false } },
    .{ .name = "nested tuple equality: same right", .source = "(1, (2, 3)) == (1, (2, 3))", .expected = .{ .bool_val = true } },
    .{ .name = "nested tuple equality: diff right", .source = "(1, (2, 3)) == (1, (2, 9))", .expected = .{ .bool_val = false } },

    // --- from eval_test.zig: nominal type equality - Bool ---
    .{ .name = "nominal Bool eq: True True", .source = "Bool.True == Bool.True", .expected = .{ .bool_val = true } },
    .{ .name = "nominal Bool eq: False False", .source = "Bool.False == Bool.False", .expected = .{ .bool_val = true } },
    .{ .name = "nominal Bool eq: True False", .source = "Bool.True == Bool.False", .expected = .{ .bool_val = false } },
    .{ .name = "nominal Bool eq: False True", .source = "Bool.False == Bool.True", .expected = .{ .bool_val = false } },

    // --- from eval_test.zig: nominal type equality - Bool in expressions ---
    .{ .name = "nominal Bool in expr: eq eq", .source = "(1 == 1) == (2 == 2)", .expected = .{ .bool_val = true } },
    .{ .name = "nominal Bool in expr: eq neq", .source = "(1 == 1) == (1 == 2)", .expected = .{ .bool_val = false } },
    .{ .name = "nominal Bool in expr: neq neq", .source = "(1 != 2) == (3 != 4)", .expected = .{ .bool_val = true } },

    // --- from eval_test.zig: nominal type equality - records containing Bool ---
    .{ .name = "records containing Bool: same true", .source = "{ flag: Bool.True } == { flag: Bool.True }", .expected = .{ .bool_val = true } },
    .{ .name = "records containing Bool: diff", .source = "{ flag: Bool.True } == { flag: Bool.False }", .expected = .{ .bool_val = false } },
    .{ .name = "records containing Bool: multi same", .source = "{ a: Bool.True, b: Bool.False } == { a: Bool.True, b: Bool.False }", .expected = .{ .bool_val = true } },
    .{ .name = "records containing Bool: multi diff", .source = "{ a: Bool.True, b: Bool.False } == { a: Bool.False, b: Bool.True }", .expected = .{ .bool_val = false } },

    // --- from eval_test.zig: nominal type equality - tuples containing Bool ---
    .{ .name = "tuples containing Bool: same", .source = "(Bool.True, Bool.False) == (Bool.True, Bool.False)", .expected = .{ .bool_val = true } },
    .{ .name = "tuples containing Bool: diff", .source = "(Bool.True, Bool.False) == (Bool.False, Bool.True)", .expected = .{ .bool_val = false } },
    .{ .name = "tuples containing Bool: mixed", .source = "(1, Bool.True, 2) == (1, Bool.True, 2)", .expected = .{ .bool_val = true } },

    // --- from eval_test.zig: nominal type equality - nested structures with Bool ---
    .{ .name = "nested Bool: record same", .source = "{ outer: { inner: Bool.True } } == { outer: { inner: Bool.True } }", .expected = .{ .bool_val = true } },
    .{ .name = "nested Bool: record diff", .source = "{ outer: { inner: Bool.True } } == { outer: { inner: Bool.False } }", .expected = .{ .bool_val = false } },
    .{ .name = "nested Bool: tuple same", .source = "((Bool.True, Bool.False), Bool.True) == ((Bool.True, Bool.False), Bool.True)", .expected = .{ .bool_val = true } },

    // --- from eval_test.zig: tag union equality ---
    .{ .name = "tag union eq: same no payload Ok", .source = "Ok == Ok", .expected = .{ .bool_val = true } },
    .{ .name = "tag union eq: same no payload Err", .source = "Err == Err", .expected = .{ .bool_val = true } },
    .{ .name = "tag union eq: diff no payload", .source = "Ok == Err", .expected = .{ .bool_val = false } },
    .{ .name = "tag union eq: diff no payload reverse", .source = "Err == Ok", .expected = .{ .bool_val = false } },
    .{ .name = "tag union eq: same payload same val", .source = "Ok(1) == Ok(1)", .expected = .{ .bool_val = true } },
    .{ .name = "tag union eq: same payload diff val", .source = "Ok(1) == Ok(2)", .expected = .{ .bool_val = false } },
    .{ .name = "tag union eq: Err same", .source = "Err(1) == Err(1)", .expected = .{ .bool_val = true } },
    .{ .name = "tag union eq: different tags with payload",
        .source =
            \\{
            \\    x = Ok(1)
            \\    y = if Bool.False Ok(1) else Err(1)
            \\    x == y
            \\}
        ,
        .expected = .{ .bool_val = false },
    },

    // --- from eval_test.zig: tag union match ---
    .{ .name = "tag union match - direct numeric payload", .source = "match Ok(10) { Ok(n) => n + 5, Err(_) => 0 }", .expected = .{ .dec_val = 15 * RocDec.one_point_zero_i128 } },
    .{ .name = "tag union match - direct record payload", .source = "match Ok({ value: 10 }) { Ok({ value }) => value + 5, Err(_) => 0 }", .expected = .{ .dec_val = 15 * RocDec.one_point_zero_i128 } },

    // --- from eval_test.zig: tag union equality - string payloads ---
    .{ .name = "tag union eq: string same", .source = "Ok(\"hello\") == Ok(\"hello\")", .expected = .{ .bool_val = true } },
    .{ .name = "tag union eq: string diff", .source = "Ok(\"hello\") == Ok(\"world\")", .expected = .{ .bool_val = false } },

    // --- from eval_test.zig: tag union equality - three or more tags ---
    .{ .name = "tag union eq: three tags same",
        .source =
            \\{
            \\    x = Red
            \\    y = Red
            \\    x == y
            \\}
        ,
        .expected = .{ .bool_val = true },
    },
    .{ .name = "tag union eq: three tags via if same",
        .source =
            \\{
            \\    x = Red
            \\    y = if Bool.True Red else if Bool.True Green else Blue
            \\    x == y
            \\}
        ,
        .expected = .{ .bool_val = true },
    },
    .{ .name = "tag union eq: three tags diff",
        .source =
            \\{
            \\    x = Red
            \\    y = if Bool.False Red else Green
            \\    x == y
            \\}
        ,
        .expected = .{ .bool_val = false },
    },

    // --- from eval_test.zig: record inequality ---
    .{ .name = "record inequality: same", .source = "{ x: 1, y: 2 } != { x: 1, y: 2 }", .expected = .{ .bool_val = false } },
    .{ .name = "record inequality: diff", .source = "{ x: 1, y: 2 } != { x: 1, y: 3 }", .expected = .{ .bool_val = true } },
    .{ .name = "record inequality: order", .source = "{ x: 1, y: 2 } != { y: 2, x: 1 }", .expected = .{ .bool_val = false } },

    // --- from eval_test.zig: tuple inequality ---
    .{ .name = "tuple inequality: same", .source = "(1, 2) != (1, 2)", .expected = .{ .bool_val = false } },
    .{ .name = "tuple inequality: diff", .source = "(1, 2) != (1, 3)", .expected = .{ .bool_val = true } },

    // --- from eval_test.zig: tag union inequality ---
    .{ .name = "tag union ineq: Ok eq Ok", .source = "Ok == Ok", .expected = .{ .bool_val = true } },
    .{ .name = "tag union ineq: Ok neq Ok", .source = "Ok != Ok", .expected = .{ .bool_val = false } },
    .{ .name = "tag union ineq: Ok neq Err", .source = "Ok != Err", .expected = .{ .bool_val = true } },
    .{ .name = "tag union ineq: Ok(1) neq Ok(1)", .source = "Ok(1) != Ok(1)", .expected = .{ .bool_val = false } },
    .{ .name = "tag union ineq: Ok(1) neq Ok(2)", .source = "Ok(1) != Ok(2)", .expected = .{ .bool_val = true } },

    // --- from eval_test.zig: mixed structural types ---
    .{ .name = "record containing tuple eq: same", .source = "{ pair: (1, 2) } == { pair: (1, 2) }", .expected = .{ .bool_val = true } },
    .{ .name = "record containing tuple eq: diff", .source = "{ pair: (1, 2) } == { pair: (1, 3) }", .expected = .{ .bool_val = false } },
    .{ .name = "tuple containing record eq: same", .source = "({ x: 1 }, 2) == ({ x: 1 }, 2)", .expected = .{ .bool_val = true } },
    .{ .name = "tuple containing record eq: diff", .source = "({ x: 1 }, 2) == ({ x: 9 }, 2)", .expected = .{ .bool_val = false } },
    .{ .name = "record with multiple types: same",
        .source =
            \\{ name: "alice", age: 30 } == { name: "alice", age: 30 }
        ,
        .expected = .{ .bool_val = true },
    },
    .{ .name = "record with multiple types: diff name",
        .source =
            \\{ name: "alice", age: 30 } == { name: "bob", age: 30 }
        ,
        .expected = .{ .bool_val = false },
    },
    .{ .name = "record with multiple types: diff age",
        .source =
            \\{ name: "alice", age: 30 } == { name: "alice", age: 31 }
        ,
        .expected = .{ .bool_val = false },
    },
    .{ .name = "deeply nested mixed structures: same",
        .source =
            \\{ a: (1, { b: 2 }), c: 3 } == { a: (1, { b: 2 }), c: 3 }
        ,
        .expected = .{ .bool_val = true },
    },
    .{ .name = "deeply nested mixed structures: diff",
        .source =
            \\{ a: (1, { b: 2 }), c: 3 } == { a: (1, { b: 9 }), c: 3 }
        ,
        .expected = .{ .bool_val = false },
    },
    .{ .name = "tuple of tuples eq: same", .source = "((1, 2), (3, 4)) == ((1, 2), (3, 4))", .expected = .{ .bool_val = true } },
    .{ .name = "tuple of tuples eq: diff", .source = "((1, 2), (3, 4)) == ((1, 2), (3, 5))", .expected = .{ .bool_val = false } },
    .{ .name = "record with string and bool: same",
        .source =
            \\{ name: "hello", active: Bool.True } == { name: "hello", active: Bool.True }
        ,
        .expected = .{ .bool_val = true },
    },
    .{ .name = "record with string and bool: diff",
        .source =
            \\{ name: "hello", active: Bool.True } == { name: "hello", active: Bool.False }
        ,
        .expected = .{ .bool_val = false },
    },

    // --- from eval_test.zig: tag union inside record/tuple equality ---
    .{ .name = "tag union inside record: same",
        .source =
            \\{
            \\    a = { status: Ok(42) }
            \\    b = { status: Ok(42) }
            \\    a == b
            \\}
        ,
        .expected = .{ .bool_val = true },
    },
    .{ .name = "tag union inside record: diff",
        .source =
            \\{
            \\    a = { status: Ok(42) }
            \\    b = { status: Ok(99) }
            \\    a == b
            \\}
        ,
        .expected = .{ .bool_val = false },
    },
    .{ .name = "record inside tag union eq: same", .source = "Ok({ x: 1, y: 2 }) == Ok({ x: 1, y: 2 })", .expected = .{ .bool_val = true } },
    .{ .name = "record inside tag union eq: diff", .source = "Ok({ x: 1, y: 2 }) == Ok({ x: 1, y: 9 })", .expected = .{ .bool_val = false } },
    .{ .name = "tag union inside tuple eq: same", .source = "(Ok(1), 2) == (Ok(1), 2)", .expected = .{ .bool_val = true } },
    .{ .name = "tag union inside tuple eq: diff", .source = "(Ok(1), 2) == (Ok(9), 2)", .expected = .{ .bool_val = false } },
    .{ .name = "tuple inside tag union eq: same", .source = "Ok((1, 2)) == Ok((1, 2))", .expected = .{ .bool_val = true } },
    .{ .name = "tuple inside tag union eq: diff", .source = "Ok((1, 2)) == Ok((1, 9))", .expected = .{ .bool_val = false } },

    // --- from eval_test.zig: three-deep nested equality ---
    .{ .name = "record inside tag union inside tuple eq: same",
        .source =
            \\(Ok({ x: 1, y: 2 }), 42) == (Ok({ x: 1, y: 2 }), 42)
        ,
        .expected = .{ .bool_val = true },
    },
    .{ .name = "record inside tag union inside tuple eq: diff",
        .source =
            \\(Ok({ x: 1, y: 2 }), 42) == (Ok({ x: 1, y: 9 }), 42)
        ,
        .expected = .{ .bool_val = false },
    },
    .{ .name = "tuple inside record inside tag union eq: same",
        .source =
            \\Ok({ pair: (1, 2), val: 99 }) == Ok({ pair: (1, 2), val: 99 })
        ,
        .expected = .{ .bool_val = true },
    },
    .{ .name = "tuple inside record inside tag union eq: diff",
        .source =
            \\Ok({ pair: (1, 2), val: 99 }) == Ok({ pair: (1, 9), val: 99 })
        ,
        .expected = .{ .bool_val = false },
    },
    .{ .name = "tag union inside record inside tuple eq: same",
        .source =
            \\({ result: Ok(1) }, 99) == ({ result: Ok(1) }, 99)
        ,
        .expected = .{ .bool_val = true },
    },
    .{ .name = "tag union inside record inside tuple eq: diff",
        .source =
            \\({ result: Ok(1) }, 99) == ({ result: Ok(2) }, 99)
        ,
        .expected = .{ .bool_val = false },
    },

    // --- from eval_test.zig: four-deep nested equality ---
    .{ .name = "four-deep nested eq: same",
        .source =
            \\{ data: (Ok({ val: 42 }), 1) } == { data: (Ok({ val: 42 }), 1) }
        ,
        .expected = .{ .bool_val = true },
    },
    .{ .name = "four-deep nested eq: diff",
        .source =
            \\{ data: (Ok({ val: 42 }), 1) } == { data: (Ok({ val: 99 }), 1) }
        ,
        .expected = .{ .bool_val = false },
    },

    // --- from eval_test.zig: long string fields equality ---
    .{ .name = "record long string eq: same",
        .source =
            \\{ name: "this string is long enough to avoid SSO optimization" } == { name: "this string is long enough to avoid SSO optimization" }
        ,
        .expected = .{ .bool_val = true },
    },
    .{ .name = "record long string eq: diff",
        .source =
            \\{ name: "this string is long enough to avoid SSO optimization" } == { name: "different long string that also avoids SSO optimization" }
        ,
        .expected = .{ .bool_val = false },
    },
    .{ .name = "record long string neq: same",
        .source =
            \\{ name: "this string is long enough to avoid SSO optimization" } != { name: "this string is long enough to avoid SSO optimization" }
        ,
        .expected = .{ .bool_val = false },
    },
    .{ .name = "record long string neq: diff",
        .source =
            \\{ name: "this string is long enough to avoid SSO optimization" } != { name: "different long string that also avoids SSO optimization" }
        ,
        .expected = .{ .bool_val = true },
    },
    .{ .name = "tuple long string eq: same",
        .source =
            \\("this string is long enough to avoid SSO optimization", 42) == ("this string is long enough to avoid SSO optimization", 42)
        ,
        .expected = .{ .bool_val = true },
    },
    .{ .name = "tuple long string eq: diff",
        .source =
            \\("this string is long enough to avoid SSO optimization", 42) == ("different long string that also avoids SSO optimization", 42)
        ,
        .expected = .{ .bool_val = false },
    },
    .{ .name = "record multi long string eq: same",
        .source =
            \\{ a: "first long string exceeding SSO limit!!", b: "second long string exceeding SSO limit!" } == { a: "first long string exceeding SSO limit!!", b: "second long string exceeding SSO limit!" }
        ,
        .expected = .{ .bool_val = true },
    },
    .{ .name = "record multi long string eq: diff",
        .source =
            \\{ a: "first long string exceeding SSO limit!!", b: "second long string exceeding SSO limit!" } == { a: "first long string exceeding SSO limit!!", b: "DIFFERENT long string exceeding SSO!!!!" }
        ,
        .expected = .{ .bool_val = false },
    },
    .{ .name = "long string inside record inside tuple eq: same",
        .source =
            \\({ name: "this string is long enough to avoid SSO optimization" }, 1) == ({ name: "this string is long enough to avoid SSO optimization" }, 1)
        ,
        .expected = .{ .bool_val = true },
    },
    .{ .name = "long string inside record inside tuple eq: diff",
        .source =
            \\({ name: "this string is long enough to avoid SSO optimization" }, 1) == ({ name: "different long string that also avoids SSO optimization" }, 1)
        ,
        .expected = .{ .bool_val = false },
    },
    .{ .name = "tag union long string payload eq: same",
        .source =
            \\Ok("this string is long enough to avoid SSO optimization") == Ok("this string is long enough to avoid SSO optimization")
        ,
        .expected = .{ .bool_val = true },
    },
    .{ .name = "tag union long string payload eq: diff",
        .source =
            \\Ok("this string is long enough to avoid SSO optimization") == Ok("different long string that also avoids SSO optimization")
        ,
        .expected = .{ .bool_val = false },
    },
    .{ .name = "tag union long string payload neq: same",
        .source =
            \\Ok("this string is long enough to avoid SSO optimization") != Ok("this string is long enough to avoid SSO optimization")
        ,
        .expected = .{ .bool_val = false },
    },
    .{ .name = "tag union long string payload neq: diff",
        .source =
            \\Ok("this string is long enough to avoid SSO optimization") != Ok("different long string that also avoids SSO optimization")
        ,
        .expected = .{ .bool_val = true },
    },

    // --- from eval_test.zig: equality in control flow ---
    .{ .name = "equality result in if: true",
        .source =
            \\if { x: 1 } == { x: 1 } 42 else 0
        ,
        .expected = .{ .dec_val = 42 * RocDec.one_point_zero_i128 },
    },
    .{ .name = "equality result in if: false",
        .source =
            \\if { x: 1 } == { x: 2 } 42 else 0
        ,
        .expected = .{ .dec_val = 0 * RocDec.one_point_zero_i128 },
    },

    // --- from eval_test.zig: equality with variable bindings ---
    .{ .name = "equality var bindings: same",
        .source =
            \\{
            \\    a = { x: 10, y: 20 }
            \\    b = { x: 10, y: 20 }
            \\    a == b
            \\}
        ,
        .expected = .{ .bool_val = true },
    },
    .{ .name = "equality var bindings: diff",
        .source =
            \\{
            \\    a = { x: 10, y: 20 }
            \\    b = { x: 10, y: 99 }
            \\    a == b
            \\}
        ,
        .expected = .{ .bool_val = false },
    },

    // --- from eval_test.zig: inequality with variable bindings ---
    .{ .name = "inequality var bindings tuples: same",
        .source =
            \\{
            \\    a = (1, 2, 3)
            \\    b = (1, 2, 3)
            \\    a != b
            \\}
        ,
        .expected = .{ .bool_val = false },
    },
    .{ .name = "inequality var bindings tuples: diff",
        .source =
            \\{
            \\    a = (1, 2, 3)
            \\    b = (1, 2, 4)
            \\    a != b
            \\}
        ,
        .expected = .{ .bool_val = true },
    },
    .{ .name = "inequality var bindings records: same",
        .source =
            \\{
            \\    a = { x: 10, y: 20 }
            \\    b = { x: 10, y: 20 }
            \\    a != b
            \\}
        ,
        .expected = .{ .bool_val = false },
    },
    .{ .name = "inequality var bindings records: diff",
        .source =
            \\{
            \\    a = { x: 10, y: 20 }
            \\    b = { x: 10, y: 99 }
            \\    a != b
            \\}
        ,
        .expected = .{ .bool_val = true },
    },

    // --- from eval_test.zig: fold equality tests (non-record) ---
    .{ .name = "simple fold Dec equality", .source = "List.fold([1, 2, 3], 0, |acc, item| acc + item) == 6", .expected = .{ .bool_val = true } },
    .{ .name = "fold record equality comparison", .source = "List.fold([1, 2, 3], {sum: 0}, |acc, item| {sum: acc.sum + item}) == {sum: 6}", .expected = .{ .bool_val = true } },
    .{ .name = "fold multi-field record equality", .source = "List.fold([1, 2, 3], {sum: 0, count: 0}, |acc, item| {sum: acc.sum + item, count: acc.count + 1}) == {sum: 6, count: 3}", .expected = .{ .bool_val = true } },

    // --- from eval_test.zig: list destructuring in fold ---
    .{ .name = "fold list destructuring: first element", .source = "List.fold([[10], [20], [30]], 0, |acc, [x]| acc + x)", .expected = .{ .dec_val = 60 * RocDec.one_point_zero_i128 } },
    .{ .name = "fold list destructuring: two elements", .source = "List.fold([[1, 2], [3, 4]], 0, |acc, [a, b]| acc + a + b)", .expected = .{ .dec_val = 10 * RocDec.one_point_zero_i128 } },
    .{ .name = "match list destructuring: baseline", .source = "match [1, 2, 3] { [a, b, c] => a + b + c, _ => 0 }", .expected = .{ .dec_val = 6 * RocDec.one_point_zero_i128 } },
    .{ .name = "match pattern alternatives", .source = "match Err(42) { Ok(x) | Err(x) => x, _ => 0 }", .expected = .{ .dec_val = 42 * RocDec.one_point_zero_i128 } },

    // --- from eval_test.zig: record update ---
    .{ .name = "record update evaluates extension once",
        .source =
            \\{
            \\    var $calls = 0.I64
            \\    rec = {
            \\        ..({
            \\            $calls = $calls + 1.I64
            \\            { a: 1.I64, b: 2.I64, c: 3.I64 }
            \\        }),
            \\        a: 10.I64,
            \\        b: 20.I64,
            \\        c: 30.I64
            \\    }
            \\    rec.a + rec.b + rec.c + $calls * 100.I64
            \\}
        ,
        .expected = .{ .i64_val = 160 },
    },
    .{ .name = "record update synthesizes missing fields",
        .source =
            \\{
            \\    var $calls = 0.I64
            \\    rec = {
            \\        ..({
            \\            $calls = $calls + 1.I64
            \\            { a: $calls, b: $calls, c: $calls }
            \\        }),
            \\        c: 99.I64
            \\    }
            \\    rec.a * 1000.I64 + rec.b * 100.I64 + rec.c + $calls * 10.I64
            \\}
        ,
        .expected = .{ .i64_val = 1209 },
    },

    // --- from eval_test.zig: regression tests ---
    .{ .name = "list equality - single element list", .source = "[1] == [1]", .expected = .{ .bool_val = true } },
    .{ .name = "list equality - nested lists", .source = "[[1, 2]] == [[1, 2]]", .expected = .{ .bool_val = true } },
    .{ .name = "list equality - single string element list", .source = "[\"hello\"] == [\"hello\"]", .expected = .{ .bool_val = true } },
    .{ .name = "record with list eq: large stack offset 1", .source = "{ a: [1] } == { a: [1, 2] }", .expected = .{ .bool_val = false } },
    .{ .name = "record with list eq: large stack offset 2", .source = "{ a: [1] } == { a: [2] }", .expected = .{ .bool_val = false } },
    .{ .name = "record with list eq: large stack offset 3", .source = "{ a: [] } == { a: [1] }", .expected = .{ .bool_val = false } },
    .{ .name = "record with list eq: large stack offset 4", .source = "{ a: [1] } == { a: [] }", .expected = .{ .bool_val = false } },
    .{ .name = "record with list eq: large stack offset 5", .source = "{ a: [], b: 1 } == { a: [2], b: 1 }", .expected = .{ .bool_val = false } },
    .{ .name = "record with list neq: large stack offset", .source = "{ a: [1] } != { a: [1, 2] }", .expected = .{ .bool_val = true } },
    .{ .name = "record with list eq: same", .source = "{ a: [1] } == { a: [1] }", .expected = .{ .bool_val = true } },
    .{ .name = "record with list eq: empty same", .source = "{ a: [] } == { a: [] }", .expected = .{ .bool_val = true } },
    .{ .name = "if block with local bindings",
        .source =
            \\if True {
            \\    x = 0
            \\    _y = x
            \\    x
            \\}
            \\else 99
        ,
        .expected = .{ .dec_val = 0 * RocDec.one_point_zero_i128 },
    },
    .{ .name = "List.len returns proper U64 nominal type: empty",
        .source =
            \\{
            \\    n = List.len([])
            \\    n.to_str()
            \\}
        ,
        .expected = .{ .str_val = "0" },
    },
    .{ .name = "List.len returns proper U64 nominal type: non-empty",
        .source =
            \\{
            \\    n = List.len([1, 2, 3])
            \\    n.to_str()
            \\}
        ,
        .expected = .{ .str_val = "3" },
    },
    .{ .name = "type annotation on var declaration",
        .source =
            \\{
            \\    var $foo : U8
            \\    var $foo = 42
            \\    $foo
            \\}
        ,
        .expected = .{ .i64_val = 42 },
    },
    .{ .name = "List.get with polymorphic numeric index",
        .source =
            \\{
            \\    list = [10, 20, 30]
            \\    index = 0
            \\    match List.get(list, index) { Ok(v) => v, _ => 0 }
            \\}
        ,
        .expected = .{ .dec_val = 10 * RocDec.one_point_zero_i128 },
    },
    .{ .name = "for loop element type from list runtime type",
        .source =
            \\{
            \\    calc = |list| {
            \\        var $result = ""
            \\        for elem in list {
            \\            $result = elem.to_str()
            \\        }
            \\        $result
            \\    }
            \\    calc([1, 2, 3])
            \\}
        ,
        .expected = .{ .str_val = "3.0" },
    },
    .{ .name = "List.get method dispatch on Try type",
        .source =
            \\{
            \\    list = ["hello"]
            \\    List.get(list, 0).ok_or("fallback")
            \\}
        ,
        .expected = .{ .str_val = "hello" },
    },
    .{ .name = "List.get with list var and when destructure",
        .source =
            \\{
            \\    list = ["hello"]
            \\    match List.get(list, 0) {
            \\        Ok(val) => val
            \\        Err(_) => "error"
            \\    }
            \\}
        ,
        .expected = .{ .str_val = "hello" },
    },
    .{ .name = "record destructuring with assignment",
        .source =
            \\{
            \\    rec = { x: 1, y: 2 }
            \\    { x, y } = rec
            \\    x + y
            \\}
        ,
        .expected = .{ .dec_val = 3 * RocDec.one_point_zero_i128 },
    },
    .{ .name = "record field access - regression 8647",
        .source =
            \\{
            \\    rec = { name: "test" }
            \\    rec.name
            \\}
        ,
        .expected = .{ .str_val = "test" },
    },
    .{ .name = "record field access with multiple string fields",
        .source =
            \\{
            \\    record = { x: "a", y: "b" }
            \\    record.x
            \\}
        ,
        .expected = .{ .str_val = "a" },
    },
    .{ .name = "method calls on numeric variables: float",
        .source =
            \\{
            \\    x = 7.0
            \\    x.to_str()
            \\}
        ,
        .expected = .{ .str_val = "7.0" },
    },
    .{ .name = "method calls on numeric variables: int",
        .source =
            \\{
            \\    x = 42
            \\    x.to_str()
            \\}
        ,
        .expected = .{ .str_val = "42.0" },
    },
    .{ .name = "issue 8710: list len", .source = "[1.I64, 2.I64, 3.I64].len()", .expected = .{ .i64_val = 3 } },
    .{ .name = "issue 8727: make_adder",
        .source =
            \\{
            \\    make_adder = |n| |x| n + x
            \\    add_ten = make_adder(10)
            \\    add_ten(5)
            \\}
        ,
        .expected = .{ .dec_val = 15 * RocDec.one_point_zero_i128 },
    },
    .{ .name = "issue 8727: curried mul", .source = "(|a| |b| a * b)(5)(10)", .expected = .{ .dec_val = 50 * RocDec.one_point_zero_i128 } },
    .{ .name = "issue 8727: triple currying", .source = "(((|a| |b| |c| a + b + c)(100))(20))(3)", .expected = .{ .dec_val = 123 * RocDec.one_point_zero_i128 } },
    .{ .name = "issue 8737: tag union with tuple payload",
        .source =
            \\{
            \\    result = XYZ((QQQ(1.U8), 3.U64))
            \\    match result {
            \\        XYZ(_) => 42
            \\        BBB => 0
            \\    }
            \\}
        ,
        .expected = .{ .dec_val = 42 * RocDec.one_point_zero_i128 },
    },
    .{ .name = "issue 8737: nested tuple pattern destructure",
        .source =
            \\{
            \\    result = XYZ((QQQ(1.U8), 3.U64))
            \\    match result {
            \\        XYZ((QQQ(_), n)) => if n == 3.U64 1 else 0
            \\        BBB => 0
            \\    }
            \\}
        ,
        .expected = .{ .dec_val = 1 * RocDec.one_point_zero_i128 },
    },
    .{ .name = "early return: ? with Ok",
        .source =
            \\{
            \\    compute = |x| Ok(x?)
            \\    match compute(Ok(42.I64)) { Ok(v) => v, _ => 0 }
            \\}
        ,
        .expected = .{ .i64_val = 42 },
    },
    .{ .name = "early return: ? with Err",
        .source =
            \\{
            \\    compute = |x| Ok(x?)
            \\    match compute(Err({})) { Ok(_) => 1, Err(_) => 0 }
            \\}
        ,
        .expected = .{ .dec_val = 0 * RocDec.one_point_zero_i128 },
    },
    .{ .name = "early return: ? in List.map closure",
        .source =
            \\{
            \\    result = [Ok(1), Err({})].map(|x| Ok(x?))
            \\    List.len(result)
            \\}
        ,
        .expected = .{ .i64_val = 2 },
    },
    .{ .name = "early return: ? in second arg",
        .source =
            \\{
            \\    my_func = |_a, b| b
            \\    compute = |x| Ok(x?)
            \\    match my_func(42, compute(Err({}))) { Ok(_) => 1, Err(_) => 0 }
            \\}
        ,
        .expected = .{ .dec_val = 0 * RocDec.one_point_zero_i128 },
    },
    .{ .name = "early return: ? in first arg",
        .source =
            \\{
            \\    my_func = |a, _b| a
            \\    compute = |x| Ok(x?)
            \\    match my_func(compute(Err({})), 42) { Ok(_) => 1, Err(_) => 0 }
            \\}
        ,
        .expected = .{ .dec_val = 0 * RocDec.one_point_zero_i128 },
    },
    .{ .name = "issue 8979: while True with break",
        .source =
            \\{
            \\    var $i = 0.I64
            \\    while (True) {
            \\        if $i >= 5 {
            \\            break
            \\        }
            \\        $i = $i + 1
            \\    }
            \\    $i
            \\}
        ,
        .expected = .{ .i64_val = 5 },
    },
    .{ .name = "list fold_rev i64 dev regression", .source = "List.fold_rev([1.I64, 2.I64, 3.I64], 0.I64, |x, acc| acc * 10 + x)", .expected = .{ .i64_val = 321 } },

    // --- from eval_test.zig: Decoder tests ---
    .{ .name = "Decoder: create ok result - check is Ok",
        .source =
            \\{
            \\    result = { result: Ok(42.I64), rest: [] }
            \\    match result.result {
            \\        Ok(_) => Bool.True
            \\        Err(_) => Bool.False
            \\    }
            \\}
        ,
        .expected = .{ .bool_val = true },
    },
    .{ .name = "Decoder: create ok result - extract value",
        .source =
            \\{
            \\    result = { result: Ok(42.I64), rest: [] }
            \\    match result.result {
            \\        Ok(n) => n
            \\        Err(_) => 0.I64
            \\    }
            \\}
        ,
        .expected = .{ .i64_val = 42 },
    },
    .{ .name = "Decoder: create err result",
        .source =
            \\{
            \\    result = { result: Err(TooShort), rest: [1.U8, 2.U8, 3.U8] }
            \\    match result.result {
            \\        Ok(_) => Bool.True
            \\        Err(_) => Bool.False
            \\    }
            \\}
        ,
        .expected = .{ .bool_val = false },
    },

    // --- from eval_test.zig: decode type mismatch ---
    .{ .name = "decode: I32.decode type mismatch crash",
        .source =
            \\{
            \\    fmt = {
            \\        decode_i32: |_fmt, src| (Ok(42.I32), src),
            \\    }
            \\    (result, _rest) = I32.decode([], fmt)
            \\    match result {
            \\        Ok(n) => n.to_i64()
            \\        Err(_) => 0.I64
            \\    }
            \\}
        ,
        .expected = .{ .type_mismatch_crash = {} },
    },

    // --- from eval_test.zig: debug 8783 series ---
    .{ .name = "debug 8783a: lambda with tag match",
        .source =
            \\{
            \\    f = |child|
            \\        match child {
            \\            Aaa(_, _) => 10.I64
            \\            Bbb(_) => 1.I64
            \\        }
            \\    f(Bbb(42.I64))
            \\}
        ,
        .expected = .{ .i64_val = 1 },
    },
    .{ .name = "debug 8783b: fold with simple addition",
        .source =
            \\{
            \\    items = [1.I64, 2.I64, 3.I64]
            \\    List.fold(items, 0.I64, |acc, x| acc + x)
            \\}
        ,
        .expected = .{ .i64_val = 6 },
    },
    .{ .name = "debug 8783g: match on payload tag without fold",
        .source =
            \\{
            \\    item = A(1.I64)
            \\    match item {
            \\        A(x) => x + 100.I64
            \\        B(x) => x + 200.I64
            \\    }
            \\}
        ,
        .expected = .{ .i64_val = 101 },
    },
    .{ .name = "match on zst-payload tag union",
        .source =
            \\{
            \\    item = A({})
            \\    match item {
            \\        A(_) => 1.I64
            \\        B(_) => 0.I64
            \\    }
            \\}
        ,
        .expected = .{ .i64_val = 1 },
    },
    .{ .name = "proc return of zst-payload tag union",
        .source =
            \\{
            \\    make = || A({})
            \\    match make() {
            \\        A(_) => 1.I64
            \\        _ => 0.I64
            \\    }
            \\}
        ,
        .expected = .{ .i64_val = 1 },
    },
    .{ .name = "debug 8783f: fold with tag match single payload",
        .source =
            \\{
            \\    items = [A(1.I64), B(2.I64)]
            \\    f = |acc, x|
            \\        match x {
            \\            A(_) => acc + 1.I64
            \\            B(_) => acc + 10.I64
            \\        }
            \\    List.fold(items, 0.I64, f)
            \\}
        ,
        .expected = .{ .i64_val = 11 },
    },
    .{ .name = "debug 8783c: fold with tag match",
        .source =
            \\{
            \\    children = [Text("hello")]
            \\    count_child = |acc, child|
            \\        match child {
            \\            Text(_) => acc + 1.I64
            \\            Element(_, _) => acc + 10.I64
            \\        }
            \\    List.fold(children, 0.I64, count_child)
            \\}
        ,
        .expected = .{ .i64_val = 1 },
    },
    .{ .name = "issue 8783: fold match on tag union from pattern match",
        .source =
            \\{
            \\    elem = Element("div", [Text("hello")])
            \\    children = match elem {
            \\        Element(_tag, c) => c
            \\        Text(_) => []
            \\    }
            \\    count_child = |acc, child|
            \\        match child {
            \\            Text(_) => acc + 1.I64
            \\            Element(_, _) => acc + 10.I64
            \\        }
            \\    List.fold(children, 0.I64, count_child)
            \\}
        ,
        .expected = .{ .i64_val = 1 },
    },

    // --- from eval_test.zig: issue 8821 ---
    .{ .name = "issue 8821: List.get with records and match",
        .source =
            \\{
            \\    clients : List({ id : U64, name : Str })
            \\    clients = [{ id: 1, name: "Alice" }]
            \\
            \\    match List.get(clients, 0) {
            \\        Ok(client) => client.name
            \\        Err(_) => "missing"
            \\    }
            \\}
        ,
        .expected = .{ .str_val = "Alice" },
    },
    .{ .name = "issue 8821 reduced: match ignores payload body",
        .source =
            \\{
            \\    clients : List({ id : U64, name : Str })
            \\    clients = [{ id: 1, name: "Alice" }]
            \\
            \\    match List.get(clients, 0) {
            \\        Ok(_client) => 1
            \\        Err(_) => 0
            \\    }
            \\}
        ,
        .expected = .{ .dec_val = 1 * RocDec.one_point_zero_i128 },
    },
    .{ .name = "issue 8821 reduced: without matching result",
        .source =
            \\{
            \\    clients : List({ id : U64, name : Str })
            \\    clients = [{ id: 1, name: "Alice" }]
            \\
            \\    _result = List.get(clients, 0)
            \\    1
            \\}
        ,
        .expected = .{ .dec_val = 1 * RocDec.one_point_zero_i128 },
    },

    // --- from eval_test.zig: encode ---
    .{ .name = "encode: string to utf8 and back",
        .source =
            \\{
            \\    bytes = Str.to_utf8("hello")
            \\    Str.from_utf8_lossy(bytes)
            \\}
        ,
        .expected = .{ .str_val = "hello" },
    },

    // --- from eval_test.zig: static dispatch ---
    .{ .name = "static dispatch: List.sum",
        .source =
            \\{
            \\    list : List(I64)
            \\    list = [1.I64, 2.I64, 3.I64, 4.I64, 5.I64]
            \\    List.sum(list)
            \\}
        ,
        .expected = .{ .i64_val = 15 },
    },

    // --- from eval_test.zig: issue 8814 ---
    .{ .name = "issue 8814: List.get on function parameter",
        .source =
            \\{
            \\    process = |args| {
            \\        match args.get(0) {
            \\            Ok(x) => x
            \\            Err(_) => "error"
            \\        }
            \\    }
            \\    process(["hello", "world"])
            \\}
        ,
        .expected = .{ .str_val = "hello" },
    },

    // --- from eval_test.zig: problems ---
    .{ .name = "issue 8831: self-referential value definition",
        .source =
            \\{
            \\    a = a
            \\    a
            \\}
        ,
        .expected = .{ .problem = {} },
    },
    .{ .name = "issue 8831: nested self-reference in list",
        .source =
            \\{
            \\    a = [a]
            \\    a
            \\}
        ,
        .expected = .{ .problem = {} },
    },
    .{ .name = "issue 9043: self-reference in tuple pattern",
        .source =
            \\{
            \\    next = |idx| (idx, idx + 1)
            \\    (_, var $n) = next($n)
            \\    $n
            \\}
        ,
        .expected = .{ .problem = {} },
    },

    // --- from eval_test.zig: issue 9262 ---
    .{ .name = "issue 9262: opaque function field returning tag union",
        .source =
            \\{
            \\    W(a) := { f : {} -> [V(a)] }.{
            \\        run = |w| (w.f)({})
            \\
            \\        mk = |val| { f: |{}| V(val) }
            \\    }
            \\
            \\    W.run(W.mk("x")) == V("x")
            \\}
        ,
        .expected = .{ .bool_val = true },
    },

    // --- from eval_test.zig: recursive function with record ---
    .{ .name = "recursive function with record - stack memory",
        .source =
            \\{
            \\    f = |n|
            \\        if n <= 0
            \\            0
            \\        else
            \\            { a: n, b: n * 2, c: n * 3, d: n * 4 }.a + f(n - 1)
            \\    f(1000)
            \\}
        ,
        .expected = .{ .dec_val = 500500 * RocDec.one_point_zero_i128 },
    },

    // --- from eval_test.zig: polymorphic tag union payload layout ---
    .{ .name = "issue 8872: polymorphic tag union payload layout",
        .source =
            \\{
            \\    transform_err : [Ok({}), Err(a)], (a -> b) -> [Ok({}), Err(b)]
            \\    transform_err = |try_val, transform| match try_val {
            \\        Err(a) => Err(transform(a))
            \\        Ok(ok) => Ok(ok)
            \\    }
            \\
            \\    err : [Ok({}), Err(I32)]
            \\    err = Err(42.I32)
            \\
            \\    result = transform_err(err, |_e| "hello")
            \\    match result {
            \\        Ok(_) => "got ok"
            \\        Err(msg) => msg
            \\    }
            \\}
        ,
        .expected = .{ .str_val = "hello" },
    },
    .{ .name = "match on tag union with different sizes",
        .source =
            \\{
            \\    transform : [Ok({}), Err(I32)] -> [Ok({}), Err(Str)]
            \\    transform = |try_val| match try_val {
            \\        Err(_) => Err("hello")
            \\        Ok(ok) => Ok(ok)
            \\    }
            \\
            \\    result = transform(Err(42.I32))
            \\    match result {
            \\        Ok(_) => "got ok"
            \\        Err(msg) => msg
            \\    }
            \\}
        ,
        .expected = .{ .str_val = "hello" },
    },
    .{ .name = "polymorphic tag transform with match",
        .source =
            \\{
            \\    transform_err = |try_val| match try_val {
            \\        Err(_) => Err("hello")
            \\        Ok(ok) => Ok(ok)
            \\    }
            \\
            \\    err : [Ok({}), Err(I32)]
            \\    err = Err(42.I32)
            \\
            \\    result = transform_err(err)
            \\    match result {
            \\        Ok(_) => "got ok"
            \\        Err(msg) => msg
            \\    }
            \\}
        ,
        .expected = .{ .str_val = "hello" },
    },
    .{ .name = "proc with tag match returning non-tag type",
        .source =
            \\{
            \\    check : [Ok({}), Err(I32)] -> Str
            \\    check = |try_val| match try_val {
            \\        Err(_) => "was err"
            \\        Ok(_) => "was ok"
            \\    }
            \\
            \\    check(Err(42.I32))
            \\}
        ,
        .expected = .{ .str_val = "was err" },
    },

    // --- from eval_test.zig: lambda with list param tests ---
    .{ .name = "lambda with list param: List.len",
        .source =
            \\{
            \\    get_len = |l| List.len(l)
            \\    get_len([1.I64, 2.I64, 3.I64])
            \\}
        ,
        .expected = .{ .i64_val = 3 },
    },
    .{ .name = "lambda with list param: List.append",
        .source =
            \\{
            \\    add_one = |l| List.len(List.append(l, 99.I64))
            \\    add_one([1.I64, 2.I64, 3.I64])
            \\}
        ,
        .expected = .{ .i64_val = 4 },
    },
    .{ .name = "lambda with list param and var",
        .source =
            \\{
            \\    test_fn = |_l| {
            \\        var $acc = [0.I64]
            \\        List.len($acc)
            \\    }
            \\    test_fn([1.I64, 2.I64])
            \\}
        ,
        .expected = .{ .i64_val = 1 },
    },
    .{ .name = "lambda with list param and list literal",
        .source =
            \\{
            \\    test_fn = |_l| {
            \\        var $acc = [0.I64]
            \\        List.len($acc)
            \\    }
            \\    test_fn([10.I64, 20.I64])
            \\}
        ,
        .expected = .{ .i64_val = 1 },
    },
    .{ .name = "lambda with list param var for loop",
        .source =
            \\{
            \\    test_fn = |l| {
            \\        var $total = 0.I64
            \\        for e in l {
            \\            $total = $total + e
            \\        }
            \\        $total
            \\    }
            \\    test_fn([10.I64, 20.I64, 30.I64])
            \\}
        ,
        .expected = .{ .i64_val = 60 },
    },
    .{ .name = "lambda with list param var List.append no loop",
        .source =
            \\{
            \\    test_fn = |_l| {
            \\        var $acc = [0.I64]
            \\        $acc = List.append($acc, 42.I64)
            \\        List.len($acc)
            \\    }
            \\    test_fn([10.I64, 20.I64])
            \\}
        ,
        .expected = .{ .i64_val = 2 },
    },
    .{ .name = "minimal lambda with list param for loop",
        .source =
            \\{
            \\    test_fn = |l| {
            \\        var $total = 0.I64
            \\        for e in l {
            \\            $total = $total + e
            \\        }
            \\        $total
            \\    }
            \\    test_fn([1.I64, 2.I64])
            \\}
        ,
        .expected = .{ .i64_val = 3 },
    },
    .{ .name = "lambda with list param for loop alloc inside",
        .source =
            \\{
            \\    test_fn = |l| {
            \\        var $total = 0.I64
            \\        for e in l {
            \\            $total = match List.last([e]) { Ok(last) => $total + last, Err(_) => $total }
            \\        }
            \\        $total
            \\    }
            \\    test_fn([1.I64, 2.I64])
            \\}
        ,
        .expected = .{ .i64_val = 3 },
    },
    .{ .name = "lambda for loop over internal list scalar param",
        .source =
            \\{
            \\    test_fn = |_x| {
            \\        var $total = 0.I64
            \\        for e in [1.I64, 2.I64, 3.I64] {
            \\            $total = $total + e
            \\        }
            \\        $total
            \\    }
            \\    test_fn(42.I64)
            \\}
        ,
        .expected = .{ .i64_val = 6 },
    },
    .{ .name = "lambda list param for loop internal list alloc",
        .source =
            \\{
            \\    test_fn = |_l| {
            \\        var $total = 0.I64
            \\        for e in [1.I64, 2.I64] {
            \\            $total = match List.last([e]) { Ok(last) => $total + last, Err(_) => $total }
            \\        }
            \\        $total
            \\    }
            \\    test_fn([10.I64, 20.I64])
            \\}
        ,
        .expected = .{ .i64_val = 3 },
    },
    .{ .name = "lambda list param for loop empty iteration",
        .source =
            \\{
            \\    test_fn = |l| {
            \\        var $acc = [0.I64]
            \\        for e in l {
            \\            $acc = List.append($acc, e)
            \\        }
            \\        List.len($acc)
            \\    }
            \\    test_fn([])
            \\}
        ,
        .expected = .{ .i64_val = 1 },
    },
    .{ .name = "lambda list param for loop append single",
        .source =
            \\{
            \\    test_fn = |l| {
            \\        var $acc = [0.I64]
            \\        for e in l {
            \\            $acc = List.append($acc, e)
            \\        }
            \\        List.len($acc)
            \\    }
            \\    test_fn([10.I64])
            \\}
        ,
        .expected = .{ .i64_val = 2 },
    },
    .{ .name = "lambda list param var for loop List.append",
        .source =
            \\{
            \\    test_fn = |l| {
            \\        var $acc = [0.I64]
            \\        for e in l {
            \\            $acc = List.append($acc, e)
            \\        }
            \\        List.len($acc)
            \\    }
            \\    test_fn([10.I64, 20.I64, 30.I64])
            \\}
        ,
        .expected = .{ .i64_val = 4 },
    },

    // --- from eval_test.zig: issue 8899 ---
    .{ .name = "issue 8899: closure decref in for loop",
        .source =
            \\{
            \\    sum_with_last = |l| {
            \\        var $total = 0.I64
            \\        var $acc = [0.I64]
            \\        for e in l {
            \\            $acc = List.append($acc, e)
            \\            $total = match List.last($acc) { Ok(last) => $total + last, Err(_) => $total }
            \\        }
            \\        $total
            \\    }
            \\    sum_with_last([10.I64, 20.I64, 30.I64])
            \\}
        ,
        .expected = .{ .i64_val = 60 },
    },

    // --- from eval_test.zig: issue 8927 ---
    .{ .name = "issue 8927: early return in method argument",
        .source =
            \\{
            \\    fold_try = |tries| {
            \\        var $ok_list = [""]
            \\        $ok_list = []
            \\        for a_try in tries {
            \\            $ok_list = $ok_list.append(a_try?)
            \\        }
            \\        Ok($ok_list)
            \\    }
            \\
            \\    tries = [Ok("a"), Ok("b"), Err(Oops), Ok("d")]
            \\
            \\    match fold_try(tries) {
            \\        Ok(list) => List.len(list)
            \\        Err(_) => 0
            \\    }
            \\}
        ,
        .expected = .{ .i64_val = 0 },
    },

    // --- from eval_test.zig: issue 8946 ---
    .{ .name = "issue 8946: closure capturing for-loop element",
        .source =
            \\{
            \\    my_any = |lst, pred| {
            \\        for e in lst {
            \\            if pred(e) { return True }
            \\        }
            \\        False
            \\    }
            \\    check = |list| {
            \\        var $built = []
            \\        for item in list {
            \\            _x = my_any($built, |x| x == item)
            \\            $built = $built.append(item)
            \\        }
            \\        $built.len()
            \\    }
            \\    check([1, 2])
            \\}
        ,
        .expected = .{ .i64_val = 2 },
    },

    // --- from eval_test.zig: issue 8978 ---
    .{ .name = "issue 8978: incref alignment recursive tag unions",
        .source =
            \\{
            \\    make_result = || {
            \\        elem = Element("div", [Text("hello"), Element("span", [Text("world")])])
            \\        children = match elem {
            \\            Element(_tag, c) => c
            \\            Text(_) => []
            \\        }
            \\        (children, 42.I64)
            \\    }
            \\    (_, n) = make_result()
            \\    n
            \\}
        ,
        .expected = .{ .i64_val = 42 },
    },

    // --- from eval_test.zig: wildcard cleanup ---
    .{ .name = "owned record wildcard field cleanup",
        .source =
            \\{
            \\    make_record = || { ignored: [1.I64, 2.I64, 3.I64], kept: 7.I64 }
            \\    { ignored: _, kept } = make_record()
            \\    kept
            \\}
        ,
        .expected = .{ .i64_val = 7 },
    },
    .{ .name = "owned tag wildcard payload cleanup", .source = "match Ok([1.I64, 2.I64, 3.I64]) { Ok(_) => 9.I64, Err(_) => 0.I64 }", .expected = .{ .i64_val = 9 } },

    // --- from eval_test.zig: Str.inspect ---
    .{ .name = "str_inspekt - integer", .source = "Str.inspect(42)", .expected = .{ .str_val = "42.0" } },
    .{ .name = "str_inspekt - negative integer", .source = "Str.inspect(-123)", .expected = .{ .str_val = "-123.0" } },
    .{ .name = "str_inspekt - zero", .source = "Str.inspect(0)", .expected = .{ .str_val = "0.0" } },
    .{ .name = "str_inspekt - boolean true", .source = "Str.inspect(Bool.True)", .expected = .{ .str_val = "True" } },
    .{ .name = "str_inspekt - boolean false", .source = "Str.inspect(Bool.False)", .expected = .{ .str_val = "False" } },
    .{ .name = "str_inspekt - simple string", .source = "Str.inspect(\"hello\")", .expected = .{ .str_val = "\"hello\"" } },
    .{ .name = "str_inspekt - string with quotes", .source = "Str.inspect(\"say \\\"hi\\\"\")", .expected = .{ .str_val = "\"say \\\"hi\\\"\"" } },
    .{ .name = "str_inspekt - empty string", .source = "Str.inspect(\"\")", .expected = .{ .str_val = "\"\"" } },
    .{ .name = "str_inspekt - large integer", .source = "Str.inspect(1234567890)", .expected = .{ .str_val = "1234567890.0" } },

    // --- from eval_test.zig: higher-order functions ---
    .{ .name = "higher-order function: simple apply",
        .source =
            \\{
            \\    apply = |f, x| f(x)
            \\    apply(|n| n + 1.I64, 5.I64)
            \\}
        ,
        .expected = .{ .i64_val = 6 },
    },
    .{ .name = "higher-order function: apply with closure",
        .source =
            \\{
            \\    offset = 10.I64
            \\    apply = |f, x| f(x)
            \\    apply(|n| n + offset, 5.I64)
            \\}
        ,
        .expected = .{ .i64_val = 15 },
    },
    .{ .name = "higher-order function: twice",
        .source =
            \\{
            \\    twice = |f, x| f(f(x))
            \\    twice(|n| n * 2.I64, 3.I64)
            \\}
        ,
        .expected = .{ .i64_val = 12 },
    },

    // --- from eval_test.zig: integer conversions ---
    .{ .name = "int conversion: I8.to_i64 positive", .source = "{ 42.I8.to_i64() }", .expected = .{ .i64_val = 42 } },
    .{ .name = "int conversion: I8.to_i64 negative", .source = "{ (-1.I8).to_i64() }", .expected = .{ .i64_val = -1 } },
    .{ .name = "int conversion: I16.to_i64 positive", .source = "{ 1000.I16.to_i64() }", .expected = .{ .i64_val = 1000 } },
    .{ .name = "int conversion: I16.to_i64 negative", .source = "{ (-500.I16).to_i64() }", .expected = .{ .i64_val = -500 } },
    .{ .name = "int conversion: I32.to_i64 positive", .source = "{ 100000.I32.to_i64() }", .expected = .{ .i64_val = 100000 } },
    .{ .name = "int conversion: I32.to_i64 negative", .source = "{ (-100000.I32).to_i64() }", .expected = .{ .i64_val = -100000 } },
    .{ .name = "int conversion: U8.to_i64", .source = "{ 255.U8.to_i64() }", .expected = .{ .i64_val = 255 } },
    .{ .name = "int conversion: U16.to_i64", .source = "{ 65535.U16.to_i64() }", .expected = .{ .i64_val = 65535 } },
    .{ .name = "int conversion: U32.to_i64", .source = "{ 4000000000.U32.to_i64() }", .expected = .{ .i64_val = 4000000000 } },
    .{ .name = "int conversion: I8.to_i32.to_i64", .source = "{ (-10.I8).to_i32().to_i64() }", .expected = .{ .i64_val = -10 } },
    .{ .name = "int conversion: U8.to_u32.to_i64", .source = "{ 200.U8.to_u32().to_i64() }", .expected = .{ .i64_val = 200 } },
    .{ .name = "int conversion: U8.to_i16.to_i64", .source = "{ 128.U8.to_i16().to_i64() }", .expected = .{ .i64_val = 128 } },

    // --- from eval_test.zig: diag tests ---
    .{ .name = "diag: match Ok extract payload", .source = "match Ok(42) { Ok(v) => v, _ => 0 }", .expected = .{ .dec_val = 42 * RocDec.one_point_zero_i128 } },
    .{ .name = "diag: lambda returning tag union",
        .source =
            \\{
            \\    f = |x| Ok(x)
            \\    match f(42) { Ok(v) => v, _ => 0 }
            \\}
        ,
        .expected = .{ .dec_val = 42 * RocDec.one_point_zero_i128 },
    },
    .{ .name = "diag: identity lambda call",
        .source =
            \\{
            \\    f = |x| x
            \\    f(42)
            \\}
        ,
        .expected = .{ .dec_val = 42 * RocDec.one_point_zero_i128 },
    },
    .{ .name = "diag: lambda wrapping try suffix",
        .source =
            \\{
            \\    compute = |x| Ok(x?)
            \\    match compute(Ok(42.I64)) { Ok(v) => v, _ => 0 }
            \\}
        ,
        .expected = .{ .i64_val = 42 },
    },

    // --- from eval_test.zig: Bool raw values ---
    .{ .name = "Bool.True raw value", .source = "Bool.True", .expected = .{ .bool_val = true } },
    .{ .name = "Bool.False raw value", .source = "Bool.False", .expected = .{ .bool_val = false } },
    .{ .name = "Bool in record field: True", .source = "{ flag: Bool.True }.flag", .expected = .{ .bool_val = true } },
    .{ .name = "Bool in record field: False", .source = "{ flag: Bool.False }.flag", .expected = .{ .bool_val = false } },

    // --- from eval_test.zig: polymorphic tag union payload substitution ---
    .{ .name = "polymorphic tag union payload: extract",
        .source =
            \\{
            \\    second : [Left(a), Right(b)], b -> b
            \\    second = |either, fallback| match either {
            \\        Left(_) => fallback
            \\        Right(val) => val
            \\    }
            \\
            \\    input : [Left(I64), Right(I64)]
            \\    input = Right(42.I64)
            \\    second(input, 0.I64)
            \\}
        ,
        .expected = .{ .i64_val = 42 },
    },
    .{ .name = "polymorphic tag union payload: multiple type vars",
        .source =
            \\{
            \\    get_err : [Ok(a), Err(e)], e -> e
            \\    get_err = |result, fallback| match result {
            \\        Ok(_) => fallback
            \\        Err(e) => e
            \\    }
            \\
            \\    val : [Ok(I64), Err(Str)]
            \\    val = Err("hello")
            \\    get_err(val, "")
            \\}
        ,
        .expected = .{ .str_val = "hello" },
    },

    // --- from eval_test.zig: type mismatch crash tests ---
    .{ .name = "polymorphic tag union: erroneous match branch crashes",
        .source =
            \\{
            \\    get_err : [Ok(a), Err(e)] -> e
            \\    get_err = |result| match result {
            \\        Ok(_) => ""
            \\        Err(e) => e
            \\    }
            \\
            \\    val : [Ok(I64), Err(Str)]
            \\    val = Ok(42)
            \\    get_err(val)
            \\}
        ,
        .expected = .{ .type_mismatch_crash = {} },
    },
    .{ .name = "polymorphic: erroneous if-else branch crashes",
        .source =
            \\{
            \\    get_val : Bool, e -> e
            \\    get_val = |flag, val| if (flag) "" else val
            \\
            \\    get_val(Bool.true, 42)
            \\}
        ,
        .expected = .{ .type_mismatch_crash = {} },
    },
    .{ .name = "polymorphic tag union: erroneous match in block crashes",
        .source =
            \\{
            \\    get_err : [Ok(a), Err(e)] -> e
            \\    get_err = |result| {
            \\        unused = 0
            \\        match result {
            \\            Ok(_) => ""
            \\            Err(e) => e
            \\        }
            \\    }
            \\
            \\    val : [Ok(I64), Err(Str)]
            \\    val = Ok(42)
            \\    get_err(val)
            \\}
        ,
        .expected = .{ .type_mismatch_crash = {} },
    },
    .{ .name = "polymorphic tag union payload: wrap and unwrap",
        .source =
            \\{
            \\    wrap : a -> [Val(a)]
            \\    wrap = |x| Val(x)
            \\
            \\    result = wrap(42)
            \\    match result {
            \\        Val(n) => n
            \\    }
            \\}
        ,
        .expected = .{ .dec_val = 42 * RocDec.one_point_zero_i128 },
    },

    // --- from eval_test.zig: Bool in records with mixed alignment ---
    .{ .name = "Bool mixed alignment: U64 True", .source = "{ key: 42.U64, flag: Bool.True }.flag", .expected = .{ .bool_val = true } },
    .{ .name = "Bool mixed alignment: U64 False", .source = "{ key: 42.U64, flag: Bool.False }.flag", .expected = .{ .bool_val = false } },
    .{ .name = "Bool mixed alignment: U64 U32 True", .source = "{ key: 42.U64, count: 1.U32, flag: Bool.True }.flag", .expected = .{ .bool_val = true } },
    .{ .name = "Bool mixed alignment: U64 U32 False", .source = "{ key: 42.U64, count: 1.U32, flag: Bool.False }.flag", .expected = .{ .bool_val = false } },

    // --- from eval_test.zig: Bool.not ---
    .{ .name = "Bool.not(Bool.True) returns False", .source = "Bool.not(Bool.True)", .expected = .{ .bool_val = false } },
    .{ .name = "Bool.not(Bool.False) returns True", .source = "Bool.not(Bool.False)", .expected = .{ .bool_val = true } },
    .{ .name = "Bool.not(True) returns False", .source = "Bool.not(True)", .expected = .{ .bool_val = false } },
    .{ .name = "Bool.not(False) returns True", .source = "Bool.not(False)", .expected = .{ .bool_val = true } },
    .{ .name = "!Bool.True returns False", .source = "!Bool.True", .expected = .{ .bool_val = false } },
    .{ .name = "!Bool.False returns True", .source = "!Bool.False", .expected = .{ .bool_val = true } },

    // --- from eval_test.zig: dev only tests ---
    .{ .name = "dev only: Bool.True formats as True", .source = "Bool.True", .expected = .{ .dev_only_str = "True" } },
    .{ .name = "dev only: Bool.False formats as False", .source = "Bool.False", .expected = .{ .dev_only_str = "False" } },
    .{ .name = "dev only: Bool.not(Bool.True) formats as False", .source = "Bool.not(Bool.True)", .expected = .{ .dev_only_str = "False" } },
    .{ .name = "dev only: Bool.not(Bool.False) formats as True", .source = "Bool.not(Bool.False)", .expected = .{ .dev_only_str = "True" } },
    .{ .name = "dev only: Bool.not(False) formats as True", .source = "Bool.not(False)", .expected = .{ .dev_only_str = "True" } },
    .{ .name = "dev only: !Bool.True formats as False", .source = "!Bool.True", .expected = .{ .dev_only_str = "False" } },
    .{ .name = "dev only: !Bool.False formats as True", .source = "!Bool.False", .expected = .{ .dev_only_str = "True" } },
    .{ .name = "dev only: nested List.append U32", .source = "List.append(List.append([], 1.U32), 2.U32)", .expected = .{ .dev_only_str = "[1, 2]" } },
    .{ .name = "dev only: U32 literal", .source = "15.U32", .expected = .{ .dev_only_str = "15" } },
    .{ .name = "dev only: U32 comparison", .source = "1.U32 <= 5.U32", .expected = .{ .dev_only_str = "True" } },
    .{ .name = "dev only: U32 addition", .source = "1.U32 + 2.U32", .expected = .{ .dev_only_str = "3" } },
    .{ .name = "dev only: while loop increment U32",
        .source =
            \\{
            \\    var current = 1.U32
            \\
            \\    while current <= 5.U32 {
            \\        current = current + 1.U32
            \\    }
            \\
            \\    current
            \\}
        ,
        .expected = .{ .dev_only_str = "6" },
    },
    .{ .name = "dev only: while loop sum U32",
        .source =
            \\{
            \\    var current = 1.U32
            \\    var sum = 0.U32
            \\
            \\    while current <= 5.U32 {
            \\        sum = sum + current
            \\        current = current + 1.U32
            \\    }
            \\
            \\    sum
            \\}
        ,
        .expected = .{ .dev_only_str = "15" },
    },

    // --- from eval_test.zig: Str operations ---
    .{ .name = "Str.trim: spaces", .source = "Str.trim(\"  hello  \")", .expected = .{ .str_val = "hello" } },
    .{ .name = "Str.trim: no spaces", .source = "Str.trim(\"hello\")", .expected = .{ .str_val = "hello" } },
    .{ .name = "Str.trim: only spaces", .source = "Str.trim(\"  \")", .expected = .{ .str_val = "" } },
    .{ .name = "Str.trim_start: spaces", .source = "Str.trim_start(\"  hello  \")", .expected = .{ .str_val = "hello  " } },
    .{ .name = "Str.trim_start: no spaces", .source = "Str.trim_start(\"hello\")", .expected = .{ .str_val = "hello" } },
    .{ .name = "Str.trim_end: spaces", .source = "Str.trim_end(\"  hello  \")", .expected = .{ .str_val = "  hello" } },
    .{ .name = "Str.trim_end: no spaces", .source = "Str.trim_end(\"hello\")", .expected = .{ .str_val = "hello" } },
    .{ .name = "Str.with_ascii_lowercased: upper", .source = "Str.with_ascii_lowercased(\"HELLO\")", .expected = .{ .str_val = "hello" } },
    .{ .name = "Str.with_ascii_lowercased: mixed", .source = "Str.with_ascii_lowercased(\"Hello World\")", .expected = .{ .str_val = "hello world" } },
    .{ .name = "Str.with_ascii_lowercased: already lower", .source = "Str.with_ascii_lowercased(\"abc\")", .expected = .{ .str_val = "abc" } },
    .{ .name = "Str.with_ascii_uppercased: lower", .source = "Str.with_ascii_uppercased(\"hello\")", .expected = .{ .str_val = "HELLO" } },
    .{ .name = "Str.with_ascii_uppercased: mixed", .source = "Str.with_ascii_uppercased(\"Hello World\")", .expected = .{ .str_val = "HELLO WORLD" } },
    .{ .name = "Str.with_ascii_uppercased: already upper", .source = "Str.with_ascii_uppercased(\"ABC\")", .expected = .{ .str_val = "ABC" } },
    .{ .name = "Str.caseless_ascii_equals: diff case", .source = "Str.caseless_ascii_equals(\"hello\", \"HELLO\")", .expected = .{ .bool_val = true } },
    .{ .name = "Str.caseless_ascii_equals: same case", .source = "Str.caseless_ascii_equals(\"abc\", \"abc\")", .expected = .{ .bool_val = true } },
    .{ .name = "Str.caseless_ascii_equals: different", .source = "Str.caseless_ascii_equals(\"abc\", \"def\")", .expected = .{ .bool_val = false } },
    .{ .name = "Str.repeat: 3 times", .source = "Str.repeat(\"ab\", 3)", .expected = .{ .str_val = "ababab" } },
    .{ .name = "Str.repeat: 1 time", .source = "Str.repeat(\"x\", 1)", .expected = .{ .str_val = "x" } },
    .{ .name = "Str.repeat: 0 times", .source = "Str.repeat(\"x\", 0)", .expected = .{ .str_val = "" } },
    .{ .name = "Str.with_prefix: normal", .source = "Str.with_prefix(\"world\", \"hello \")", .expected = .{ .str_val = "hello world" } },
    .{ .name = "Str.with_prefix: empty prefix", .source = "Str.with_prefix(\"bar\", \"\")", .expected = .{ .str_val = "bar" } },

    // --- from eval_test.zig: polymorphic closure capture ---
    .{ .name = "polymorphic closure capture: int",
        .source =
            \\{
            \\    make_getter = |n| |_x| n
            \\    get_num = make_getter(42)
            \\    get_num(0)
            \\}
        ,
        .expected = .{ .dec_val = 42 * RocDec.one_point_zero_i128 },
    },
    .{ .name = "polymorphic closure capture: str",
        .source =
            \\{
            \\    make_getter = |n| |_x| n
            \\    get_str = make_getter("hello")
            \\    get_str(0)
            \\}
        ,
        .expected = .{ .str_val = "hello" },
    },

    // --- from eval_test.zig: large record chained HOF ---
    .{ .name = "large record chained HOF: w",
        .source =
            \\{
            \\    apply2 = |a, b, f| f(a, b)
            \\    step1 = apply2("x_val", "y_val", |x, y| { x, y })
            \\    result = apply2("w_val", step1.y, |w, y| { w, y })
            \\    result.w
            \\}
        ,
        .expected = .{ .str_val = "w_val" },
    },
    .{ .name = "large record chained HOF: y",
        .source =
            \\{
            \\    apply2 = |a, b, f| f(a, b)
            \\    step1 = apply2("x_val", "y_val", |x, y| { x, y })
            \\    result = apply2("w_val", step1.y, |w, y| { w, y })
            \\    result.y
            \\}
        ,
        .expected = .{ .str_val = "y_val" },
    },

    // --- from eval_test.zig: more Str operations ---
    .{ .name = "Str.drop_prefix: match", .source = "Str.drop_prefix(\"foobar\", \"foo\")", .expected = .{ .str_val = "bar" } },
    .{ .name = "Str.drop_prefix: no match", .source = "Str.drop_prefix(\"foobar\", \"baz\")", .expected = .{ .str_val = "foobar" } },
    .{ .name = "Str.drop_suffix: match", .source = "Str.drop_suffix(\"foobar\", \"bar\")", .expected = .{ .str_val = "foo" } },
    .{ .name = "Str.drop_suffix: no match", .source = "Str.drop_suffix(\"foobar\", \"baz\")", .expected = .{ .str_val = "foobar" } },
    .{ .name = "Str.release_excess_capacity", .source = "Str.release_excess_capacity(\"hello\")", .expected = .{ .str_val = "hello" } },
    .{ .name = "Str.split_on and Str.join_with",
        .source =
            \\{
            \\    parts = Str.split_on("a,b,c", ",")
            \\    Str.join_with(parts, "-")
            \\}
        ,
        .expected = .{ .str_val = "a-b-c" },
    },
    .{ .name = "Str.join_with",
        .source =
            \\Str.join_with(["hello", "world"], " ")
        ,
        .expected = .{ .str_val = "hello world" },
    },

    // --- from eval_test.zig: dev only List/Str tests ---
    .{ .name = "dev: List.last returns Ok", .source = "List.last([1, 2, 3])", .expected = .{ .dev_only_str = "Ok(3.0)" } },
    .{ .name = "dev: List.first returns Ok", .source = "List.first([10, 20, 30])", .expected = .{ .dev_only_str = "Ok(10.0)" } },
    .{ .name = "dev: List.first empty returns Err", .source = "List.first([])", .expected = .{ .dev_only_str = "Err(ListWasEmpty)" } },
    .{ .name = "dev: Str.from_utf8 Ok", .source = "Str.from_utf8([72, 105])", .expected = .{ .dev_only_str = "Ok(\"Hi\")" } },
    .{ .name = "dev: polymorphic sum in block U64",
        .source =
            \\{
            \\    sum = |a, b| a + b + 0
            \\    U64.to_str(sum(240, 20))
            \\}
        ,
        .expected = .{ .dev_only_str = "\"260\"" },
    },
    .{ .name = "dev: List.contains int", .source = "List.contains([1, 2, 3, 4, 5], 3)", .expected = .{ .dev_only_str = "True" } },
    .{ .name = "dev: List.any inline true", .source = "List.any([1, 2, 3], |x| x == 2)", .expected = .{ .dev_only_str = "True" } },
    .{ .name = "dev: List.any inline false", .source = "List.any([1, 2, 3], |x| x == 5)", .expected = .{ .dev_only_str = "False" } },
    .{ .name = "dev: List.any always true", .source = "List.any([1, 2, 3], |_x| True)", .expected = .{ .dev_only_str = "True" } },
    .{ .name = "dev: List.any typed elements", .source = "List.any([1.I64, 2.I64, 3.I64], |_x| True)", .expected = .{ .dev_only_str = "True" } },
    .{ .name = "dev: polymorphic predicate comparison",
        .source =
            \\{
            \\    is_positive = |x| x > 0
            \\    List.any([-1, 0, 1], is_positive)
            \\}
        ,
        .expected = .{ .dev_only_str = "True" },
    },
    .{ .name = "dev: polymorphic comparison lambda direct",
        .source =
            \\{
            \\    is_positive = |x| x > 0
            \\    is_positive(5)
            \\}
        ,
        .expected = .{ .dev_only_str = "True" },
    },
    .{ .name = "dev: polymorphic comparison lambda List.any",
        .source =
            \\{
            \\    gt_zero = |x| x > 0
            \\    List.any([1, 2, 3], gt_zero)
            \\}
        ,
        .expected = .{ .dev_only_str = "True" },
    },
    .{ .name = "dev: List.any inline lambda", .source = "List.any([1, 2, 3], |x| x > 0)", .expected = .{ .dev_only_str = "True" } },
    .{ .name = "dev: for loop early return",
        .source =
            \\{
            \\    f = |list| {
            \\        for _item in list {
            \\            if True { return True }
            \\        }
            \\        False
            \\    }
            \\    f([1, 2, 3])
            \\}
        ,
        .expected = .{ .dev_only_str = "True" },
    },
    .{ .name = "dev: for loop closure early return",
        .source =
            \\{
            \\    f = |list, pred| {
            \\        for item in list {
            \\            if pred(item) { return True }
            \\        }
            \\        False
            \\    }
            \\    f([1, 2, 3], |_x| True)
            \\}
        ,
        .expected = .{ .dev_only_str = "True" },
    },
    .{ .name = "dev: local any-style HOF equality predicate",
        .source =
            \\{
            \\    f = |list, pred| {
            \\        for item in list {
            \\            if pred(item) { return True }
            \\        }
            \\        False
            \\    }
            \\    f([1, 2, 3], |x| x == 2)
            \\}
        ,
        .expected = .{ .dev_only_str = "True" },
    },
    .{ .name = "dev: inline any-style HOF always true",
        .source =
            \\(|list, pred| {
            \\    for item in list {
            \\        if pred(item) { return True }
            \\    }
            \\    False
            \\})([1, 2, 3], |_x| True)
        ,
        .expected = .{ .dev_only_str = "True" },
    },

    // --- from eval_test.zig: polymorphic function tests ---
    .{ .name = "polymorphic function: two list types",
        .source =
            \\{
            \\    my_len = |list| list.len()
            \\    a : List(I64)
            \\    a = [1, 2, 3]
            \\    b : List(Str)
            \\    b = ["x", "y"]
            \\    my_len(a) + my_len(b)
            \\}
        ,
        .expected = .{ .i64_val = 5 },
    },
    .{ .name = "direct List.contains I64",
        .source =
            \\{
            \\    a : List(I64)
            \\    a = [1, 2, 3]
            \\    if a.contains(2) { 1 } else { 0 }
            \\}
        ,
        .expected = .{ .dec_val = 1 * RocDec.one_point_zero_i128 },
    },
    .{ .name = "polymorphic function single call I64",
        .source =
            \\{
            \\    contains = |list, item| list.contains(item)
            \\    a : List(I64)
            \\    a = [1, 2, 3]
            \\    r = contains(a, 2)
            \\    if r { 1 } else { 0 }
            \\}
        ,
        .expected = .{ .dec_val = 1 * RocDec.one_point_zero_i128 },
    },
    .{ .name = "polymorphic function single call Str",
        .source =
            \\{
            \\    contains = |list, item| list.contains(item)
            \\    b : List(Str)
            \\    b = ["x", "y"]
            \\    r = contains(b, "x")
            \\    if r { 1 } else { 0 }
            \\}
        ,
        .expected = .{ .dec_val = 1 * RocDec.one_point_zero_i128 },
    },
    .{ .name = "polymorphic function List.contains two types",
        .source =
            \\{
            \\    contains = |list, item| list.contains(item)
            \\    a : List(I64)
            \\    a = [1, 2, 3]
            \\    b : List(Str)
            \\    b = ["x", "y"]
            \\    r1 = contains(a, 2)
            \\    r2 = contains(b, "x")
            \\    if r1 and r2 { 1 } else { 0 }
            \\}
        ,
        .expected = .{ .dec_val = 1 * RocDec.one_point_zero_i128 },
    },
    .{ .name = "polymorphic function List.contains multiple types",
        .source =
            \\{
            \\    dedup = |list| {
            \\        var $out = []
            \\        for item in list {
            \\            if !$out.contains(item) {
            \\                $out = $out.append(item)
            \\            }
            \\        }
            \\        $out
            \\    }
            \\    nums : List(I64)
            \\    nums = [1, 2, 3, 2, 1]
            \\    u1 = dedup(nums)
            \\    strs : List(Str)
            \\    strs = ["a", "b", "a"]
            \\    u2 = dedup(strs)
            \\    u1.len() + u2.len()
            \\}
        ,
        .expected = .{ .i64_val = 5 },
    },

    // --- from eval_test.zig: nested List.any / List.contains ---
    .{ .name = "nested List.any true path captured Str",
        .source =
            \\{
            \\    out = ["a"]
            \\    List.any(["a"], |item| out.contains(item))
            \\}
        ,
        .expected = .{ .bool_val = true },
    },
    .{ .name = "nested List.any false path captured Str",
        .source =
            \\{
            \\    out = ["a"]
            \\    List.any(["b"], |item| out.contains(item))
            \\}
        ,
        .expected = .{ .bool_val = false },
    },
    .{ .name = "direct List.contains captured Str",
        .source =
            \\{
            \\    out = ["a"]
            \\    out.contains("a")
            \\}
        ,
        .expected = .{ .bool_val = true },
    },
    .{ .name = "forwarding tag union Str payload no leak",
        .source =
            \\{
            \\    consume = |value| value == Ok({ x: "x" })
            \\    forward = |value| consume(value)
            \\    value = Ok({ x: "x" })
            \\    forward(value)
            \\}
        ,
        .expected = .{ .bool_val = true },
    },

    // --- from eval_test.zig: focused fold tests (non-record) ---
    .{ .name = "focused: fold multi-field record equality", .source = "List.fold([1, 2, 3], {sum: 0, count: 0}, |acc, item| {sum: acc.sum + item, count: acc.count + 1}) == {sum: 6, count: 3}", .expected = .{ .bool_val = true } },
    .{ .name = "focused: fold multi-field record field checks",
        .source =
            \\{
            \\    rec = List.fold([1, 2, 3], {sum: 0, count: 0}, |acc, item| {sum: acc.sum + item, count: acc.count + 1})
            \\    rec.sum == 6 and rec.count == 3
            \\}
        ,
        .expected = .{ .bool_val = true },
    },
    .{ .name = "focused: fold multi-field record sum check",
        .source =
            \\{
            \\    rec = List.fold([1, 2, 3], {sum: 0, count: 0}, |acc, item| {sum: acc.sum + item, count: acc.count + 1})
            \\    rec.sum == 6
            \\}
        ,
        .expected = .{ .bool_val = true },
    },
    .{ .name = "focused: fold multi-field record count check",
        .source =
            \\{
            \\    rec = List.fold([1, 2, 3], {sum: 0, count: 0}, |acc, item| {sum: acc.sum + item, count: acc.count + 1})
            \\    rec.count == 3
            \\}
        ,
        .expected = .{ .bool_val = true },
    },
    .{ .name = "focused: fold multi-field record sum value",
        .source =
            \\{
            \\    rec = List.fold([1, 2, 3], {sum: 0, count: 0}, |acc, item| {sum: acc.sum + item, count: acc.count + 1})
            \\    rec.sum
            \\}
        ,
        .expected = .{ .dec_val = 6_000_000_000_000_000_000 },
    },
    .{ .name = "focused: fold multi-field record count value",
        .source =
            \\{
            \\    rec = List.fold([1, 2, 3], {sum: 0, count: 0}, |acc, item| {sum: acc.sum + item, count: acc.count + 1})
            \\    rec.count
            \\}
        ,
        .expected = .{ .dec_val = 3_000_000_000_000_000_000 },
    },
    .{ .name = "focused: simple two-field record sum access", .source = "{sum: 6, count: 3}.sum", .expected = .{ .dec_val = 6_000_000_000_000_000_000 } },
    .{ .name = "focused: simple two-field record count access", .source = "{sum: 6, count: 3}.count", .expected = .{ .dec_val = 3_000_000_000_000_000_000 } },
    .{ .name = "focused: nested list equality", .source = "[[1, 2]] == [[1, 2]]", .expected = .{ .bool_val = true } },
    .{ .name = "focused: nested list equality i64", .source = "[[1.I64, 2.I64]] == [[1.I64, 2.I64]]", .expected = .{ .bool_val = true } },
    .{ .name = "focused: nested list equality multiple same", .source = "[[1, 2], [3, 4]] == [[1, 2], [3, 4]]", .expected = .{ .bool_val = true } },
    .{ .name = "focused: nested list equality multiple diff", .source = "[[1, 2], [3, 4]] == [[1, 2], [4, 3]]", .expected = .{ .bool_val = false } },
    .{ .name = "focused: nested list equality single diff", .source = "[[3, 4]] == [[4, 3]]", .expected = .{ .bool_val = false } },
    .{ .name = "focused: list equality order-sensitive", .source = "[3, 4] == [4, 3]", .expected = .{ .bool_val = false } },
    .{ .name = "focused: polymorphic additional specialization via List.append",
        .source =
            \\{
            \\    append_one = |acc, x| List.append(acc, x)
            \\    clone_via_fold = |xs| xs.fold(List.with_capacity(1), append_one)
            \\    _first_len = clone_via_fold([1.I64, 2.I64]).len()
            \\    clone_via_fold([[1.I64, 2.I64], [3.I64, 4.I64]]).len()
            \\}
        ,
        .expected = .{ .i64_val = 2 },
    },

    // --- from closure_test.zig ---

    // TIER 1: Basic closure with captures
    .{ .name = "closure: lambda capturing one local variable",
        .source =
            \\{
            \\    y = 10
            \\    f = |x| x + y
            \\    f(5)
            \\}
        ,
        .expected = .{ .dec_val = 15 * RocDec.one_point_zero_i128 },
    },
    .{ .name = "closure: lambda capturing two local variables",
        .source =
            \\{
            \\    a = 3
            \\    b = 7
            \\    f = |x| x + a + b
            \\    f(10)
            \\}
        ,
        .expected = .{ .dec_val = 20 * RocDec.one_point_zero_i128 },
    },
    .{ .name = "closure: lambda capturing a string",
        .source =
            \\{
            \\    greeting = "Hello"
            \\    f = |name| Str.concat(greeting, name)
            \\    f(" World")
            \\}
        ,
        .expected = .{ .str_val = "Hello World" },
    },
    .{ .name = "closure: lambda capturing multiple strings",
        .source =
            \\{
            \\    prefix = "Hello"
            \\    suffix = "!"
            \\    f = |name| Str.concat(Str.concat(prefix, name), suffix)
            \\    f(" World")
            \\}
        ,
        .expected = .{ .str_val = "Hello World!" },
    },

    // TIER 2: Functions returning functions (closure escaping defining scope)
    .{ .name = "closure: function returning a closure (make_adder)",
        .source =
            \\{
            \\    make_adder = |n| |x| x + n
            \\    add5 = make_adder(5)
            \\    add5(10)
            \\}
        ,
        .expected = .{ .dec_val = 15 * RocDec.one_point_zero_i128 },
    },
    .{ .name = "closure: function returning a closure, called twice",
        .source =
            \\{
            \\    make_adder = |n| |x| x + n
            \\    add5 = make_adder(5)
            \\    a = add5(10)
            \\    b = add5(20)
            \\    a + b
            \\}
        ,
        .expected = .{ .dec_val = 40 * RocDec.one_point_zero_i128 },
    },
    .{ .name = "closure: two different closures from same factory",
        .source =
            \\{
            \\    make_adder = |n| |x| x + n
            \\    add3 = make_adder(3)
            \\    add7 = make_adder(7)
            \\    add3(10) + add7(10)
            \\}
        ,
        .expected = .{ .dec_val = 30 * RocDec.one_point_zero_i128 },
    },
    .{ .name = "closure: function returning a closure over string",
        .source =
            \\{
            \\    make_greeter = |greeting| |name| Str.concat(greeting, name)
            \\    greet = make_greeter("Hi ")
            \\    greet("Alice")
            \\}
        ,
        .expected = .{ .str_val = "Hi Alice" },
    },
    .{ .name = "closure: two-level deep closure (function returning function returning function)",
        .source =
            \\{
            \\    make_op = |a| |b| |x| x + a + b
            \\    add_3_and_4 = make_op(3)(4)
            \\    add_3_and_4(10)
            \\}
        ,
        .expected = .{ .dec_val = 17 * RocDec.one_point_zero_i128 },
    },

    // TIER 3: Higher-order functions with closure arguments
    .{ .name = "closure: passing closure to higher-order function",
        .source =
            \\{
            \\    apply = |f, x| f(x)
            \\    y = 10
            \\    apply(|x| x + y, 5)
            \\}
        ,
        .expected = .{ .dec_val = 15 * RocDec.one_point_zero_i128 },
    },
    .{ .name = "closure: passing two different closures to same HOF",
        .source =
            \\{
            \\    apply = |f, x| f(x)
            \\    a = 10
            \\    b = 20
            \\    r1 = apply(|x| x + a, 5)
            \\    r2 = apply(|x| x + b, 5)
            \\    r1 + r2
            \\}
        ,
        .expected = .{ .dec_val = 40 * RocDec.one_point_zero_i128 },
    },
    .{ .name = "closure: passing two different closures to same HOF returns first result",
        .source =
            \\{
            \\    apply = |f, x| f(x)
            \\    a = 10
            \\    b = 20
            \\    r1 = apply(|x| x + a, 5)
            \\    _r2 = apply(|x| x + b, 5)
            \\    r1
            \\}
        ,
        .expected = .{ .dec_val = 15 * RocDec.one_point_zero_i128 },
    },
    .{ .name = "closure: passing two different closures to same HOF returns second result",
        .source =
            \\{
            \\    apply = |f, x| f(x)
            \\    a = 10
            \\    b = 20
            \\    _r1 = apply(|x| x + a, 5)
            \\    r2 = apply(|x| x + b, 5)
            \\    r2
            \\}
        ,
        .expected = .{ .dec_val = 25 * RocDec.one_point_zero_i128 },
    },
    .{ .name = "closure: HOF calling closure argument twice",
        .source =
            \\{
            \\    apply_twice = |f, x| f(f(x))
            \\    y = 3
            \\    apply_twice(|x| x + y, 10)
            \\}
        ,
        .expected = .{ .dec_val = 16 * RocDec.one_point_zero_i128 },
    },
    .{ .name = "closure: HOF with closure returning string",
        .source =
            \\{
            \\    apply = |f, x| f(x)
            \\    prefix = "Hello "
            \\    apply(|name| Str.concat(prefix, name), "World")
            \\}
        ,
        .expected = .{ .str_val = "Hello World" },
    },

    // TIER 4: Polymorphic functions with closures
    .{ .name = "closure: polymorphic identity applied to closure result",
        .source =
            \\{
            \\    id = |x| x
            \\    y = 10
            \\    f = |x| x + y
            \\    id(f(5))
            \\}
        ,
        .expected = .{ .dec_val = 15 * RocDec.one_point_zero_i128 },
    },
    .{ .name = "closure: polymorphic function used with both int and string closures",
        .source =
            \\{
            \\    apply = |f, x| f(x)
            \\    n = 10
            \\    prefix = "Hi "
            \\    num_result = apply(|x| x + n, 5)
            \\    str_result = apply(|s| Str.concat(prefix, s), "Bob")
            \\    if (num_result > 0) str_result else ""
            \\}
        ,
        .expected = .{ .str_val = "Hi Bob" },
    },

    // TIER 5: Closure over closure (nested captures)
    .{ .name = "closure: closure forwarding to captured closure (no multiply)",
        .source =
            \\{
            \\    y = 5
            \\    inner = |x| x + y
            \\    outer = |x| inner(x)
            \\    outer(10)
            \\}
        ,
        .expected = .{ .dec_val = 15 * RocDec.one_point_zero_i128 },
    },
    .{ .name = "closure: closure capturing another closure",
        .source =
            \\{
            \\    y = 5
            \\    inner = |x| x + y
            \\    outer = |x| inner(x) * 2
            \\    outer(10)
            \\}
        ,
        .expected = .{ .dec_val = 30 * RocDec.one_point_zero_i128 },
    },
    .{ .name = "closure: closure capturing a factory-produced closure",
        .source =
            \\{
            \\    make_adder = |n| |x| x + n
            \\    add5 = make_adder(5)
            \\    double_add5 = |x| add5(x) * 2
            \\    double_add5(10)
            \\}
        ,
        .expected = .{ .dec_val = 30 * RocDec.one_point_zero_i128 },
    },

    // TIER 6: Multiple closures with different captures at same call site (lambda set dispatch)
    .{ .name = "closure: if-else choosing between two closures with different captures",
        .source =
            \\{
            \\    a = 10
            \\    b = 20
            \\    f = if (True) |x| x + a else |x| x + b
            \\    f(5)
            \\}
        ,
        .expected = .{ .dec_val = 15 * RocDec.one_point_zero_i128 },
    },
    .{ .name = "closure: if-else choosing between two closures, false branch",
        .source =
            \\{
            \\    a = 10
            \\    b = 20
            \\    f = if (False) |x| x + a else |x| x + b
            \\    f(5)
            \\}
        ,
        .expected = .{ .dec_val = 25 * RocDec.one_point_zero_i128 },
    },
    .{ .name = "closure: if-else choosing between closures with different capture counts",
        .source =
            \\{
            \\    a = 10
            \\    b = 20
            \\    c = 30
            \\    f = if (True) |x| x + a else |x| x + b + c
            \\    f(5)
            \\}
        ,
        .expected = .{ .dec_val = 15 * RocDec.one_point_zero_i128 },
    },

    // TIER 7: Closure used in data structures
    .{ .name = "closure: closure stored in record field then called",
        .source =
            \\{
            \\    y = 10
            \\    rec = { f: |x| x + y }
            \\    f = rec.f
            \\    f(5)
            \\}
        ,
        .expected = .{ .dec_val = 15 * RocDec.one_point_zero_i128 },
    },
    .{ .name = "closure: two closures in record, each with own captures",
        .source =
            \\{
            \\    a = 10
            \\    b = 20
            \\    rec = { add_a: |x| x + a, add_b: |x| x + b }
            \\    add_a = rec.add_a
            \\    add_b = rec.add_b
            \\    add_a(5) + add_b(5)
            \\}
        ,
        .expected = .{ .dec_val = 40 * RocDec.one_point_zero_i128 },
    },
    .{ .name = "closure: record field closure add_a preserves its capture",
        .source =
            \\{
            \\    a = 10
            \\    b = 20
            \\    rec = { add_a: |x| x + a, add_b: |x| x + b }
            \\    add_a = rec.add_a
            \\    add_a(5)
            \\}
        ,
        .expected = .{ .dec_val = 15 * RocDec.one_point_zero_i128 },
    },
    .{ .name = "closure: parenthesized record field closure add_b preserves its capture",
        .source =
            \\{
            \\    a = 10
            \\    b = 20
            \\    rec = { add_a: |x| x + a, add_b: |x| x + b }
            \\    (rec.add_b)(5)
            \\}
        ,
        .expected = .{ .dec_val = 25 * RocDec.one_point_zero_i128 },
    },
    .{ .name = "closure: record field closure add_b preserves its capture",
        .source =
            \\{
            \\    a = 10
            \\    b = 20
            \\    rec = { add_a: |x| x + a, add_b: |x| x + b }
            \\    add_b = rec.add_b
            \\    add_b(5)
            \\}
        ,
        .expected = .{ .dec_val = 25 * RocDec.one_point_zero_i128 },
    },

    // TIER 8: Composition and chaining
    .{ .name = "closure: compose two functions",
        .source =
            \\{
            \\    compose = |f, g| |x| f(g(x))
            \\    double = |x| x * 2
            \\    add1 = |x| x + 1
            \\    double_then_add1 = compose(add1, double)
            \\    double_then_add1(5)
            \\}
        ,
        .expected = .{ .dec_val = 11 * RocDec.one_point_zero_i128 },
    },
    .{ .name = "closure: compose with captures",
        .source =
            \\{
            \\    compose = |f, g| |x| f(g(x))
            \\    a = 3
            \\    b = 7
            \\    add_a = |x| x + a
            \\    add_b = |x| x + b
            \\    add_both = compose(add_a, add_b)
            \\    add_both(10)
            \\}
        ,
        .expected = .{ .dec_val = 20 * RocDec.one_point_zero_i128 },
    },
    .{ .name = "closure: pipe (flip of compose)",
        .source =
            \\{
            \\    pipe = |x, f| f(x)
            \\    y = 10
            \\    pipe(5, |x| x + y)
            \\}
        ,
        .expected = .{ .dec_val = 15 * RocDec.one_point_zero_i128 },
    },

    // TIER 9: Recursive closures and self-reference
    .{ .name = "closure: recursive function in let binding",
        .source =
            \\{
            \\    factorial = |n| if (n <= 1) 1 else n * factorial(n - 1)
            \\    factorial(5)
            \\}
        ,
        .expected = .{ .dec_val = 120 * RocDec.one_point_zero_i128 },
    },
    .{ .name = "closure: mutual recursion between two closures",
        .source =
            \\{
            \\    is_even = |n| if (n == 0) True else is_odd(n - 1)
            \\    is_odd = |n| if (n == 0) False else is_even(n - 1)
            \\    if (is_even(4)) 1 else 0
            \\}
        ,
        .expected = .{ .dec_val = 1 * RocDec.one_point_zero_i128 },
    },

    // TIER 10: Extremely complex / stress tests
    .{ .name = "closure: triple-nested closure factory",
        .source =
            \\{
            \\    level1 = |a| |b| |c| |x| x + a + b + c
            \\    level2 = level1(1)
            \\    level3 = level2(2)
            \\    level4 = level3(3)
            \\    level4(10)
            \\}
        ,
        .expected = .{ .dec_val = 16 * RocDec.one_point_zero_i128 },
    },
    .{ .name = "closure: closure capturing another closure (2 levels)",
        .source =
            \\{
            \\    a = 1
            \\    f = |x| x + a
            \\    b = 2
            \\    g = |x| f(x) + b
            \\    g(10)
            \\}
        ,
        .expected = .{ .dec_val = 13 * RocDec.one_point_zero_i128 },
    },
    .{ .name = "closure: closure capturing another closure that captures a third",
        .source =
            \\{
            \\    a = 1
            \\    f = |x| x + a
            \\    b = 2
            \\    g = |x| f(x) + b
            \\    c = 3
            \\    h = |x| g(x) + c
            \\    h(10)
            \\}
        ,
        .expected = .{ .dec_val = 16 * RocDec.one_point_zero_i128 },
    },
    .{ .name = "closure: HOF receiving closure, returning closure that captures the argument closure",
        .source =
            \\{
            \\    make_doubler = |f| |x| f(f(x))
            \\    add3 = |x| x + 3
            \\    double_add3 = make_doubler(add3)
            \\    double_add3(10)
            \\}
        ,
        .expected = .{ .dec_val = 16 * RocDec.one_point_zero_i128 },
    },
    .{ .name = "closure: HOF receiving closure with captures, returning closure that captures it",
        .source =
            \\{
            \\    n = 5
            \\    add_n = |x| x + n
            \\    make_doubler = |f| |x| f(f(x))
            \\    double_add_n = make_doubler(add_n)
            \\    double_add_n(10)
            \\}
        ,
        .expected = .{ .dec_val = 20 * RocDec.one_point_zero_i128 },
    },
    .{ .name = "closure: chained closure factories with accumulating captures",
        .source =
            \\{
            \\    step1 = |a| |b| |c| a + b + c
            \\    step2 = step1(100)
            \\    step3 = step2(20)
            \\    step3(3)
            \\}
        ,
        .expected = .{ .dec_val = 123 * RocDec.one_point_zero_i128 },
    },
    .{ .name = "closure: polymorphic HOF with closures capturing different types",
        .source =
            \\{
            \\    apply = |f, x| f(x)
            \\    offset = 100
            \\    prefix = "Result: "
            \\    num = apply(|x| x + offset, 23)
            \\    if (num > 0) apply(|s| Str.concat(prefix, s), "yes") else "no"
            \\}
        ,
        .expected = .{ .str_val = "Result: yes" },
    },
    .{ .name = "closure: closure over bool used in conditional",
        .source =
            \\{
            \\    flag = True
            \\    choose = |a, b| if (flag) a else b
            \\    choose(42, 0)
            \\}
        ,
        .expected = .{ .dec_val = 42 * RocDec.one_point_zero_i128 },
    },
    .{ .name = "closure: deeply nested blocks each adding captures",
        .source =
            \\{
            \\    a = 1
            \\    r1 = {
            \\        b = 2
            \\        r2 = {
            \\            c = 3
            \\            f = |x| x + a + b + c
            \\            f(10)
            \\        }
            \\        r2
            \\    }
            \\    r1
            \\}
        ,
        .expected = .{ .dec_val = 16 * RocDec.one_point_zero_i128 },
    },
    .{ .name = "closure: same variable captured by multiple independent closures",
        .source =
            \\{
            \\    shared = 10
            \\    f = |x| x + shared
            \\    g = |x| x * shared
            \\    f(5) + g(3)
            \\}
        ,
        .expected = .{ .dec_val = 45 * RocDec.one_point_zero_i128 },
    },
    .{ .name = "closure: closure returning a string that includes a captured string",
        .source =
            \\{
            \\    make_greeter = |greeting|
            \\        |name|
            \\            Str.concat(Str.concat(greeting, ", "), name)
            \\    hello = make_greeter("Hello")
            \\    hi = make_greeter("Hi")
            \\    r1 = hello("Alice")
            \\    r2 = hi("Bob")
            \\    Str.concat(Str.concat(r1, " and "), r2)
            \\}
        ,
        .expected = .{ .str_val = "Hello, Alice and Hi, Bob" },
    },
    .{ .name = "closure: applying the same closure to different arguments",
        .source =
            \\{
            \\    base = 100
            \\    f = |x| x + base
            \\    a = f(1)
            \\    b = f(2)
            \\    c = f(3)
            \\    a + b + c
            \\}
        ,
        .expected = .{ .dec_val = 306 * RocDec.one_point_zero_i128 },
    },
    .{ .name = "closure: immediately invoked closure with capture",
        .source =
            \\{
            \\    y = 42
            \\    (|x| x + y)(8)
            \\}
        ,
        .expected = .{ .dec_val = 50 * RocDec.one_point_zero_i128 },
    },
    .{ .name = "closure: closure that ignores its argument but uses capture",
        .source =
            \\{
            \\    val = 99
            \\    f = |_| val
            \\    f(0)
            \\}
        ,
        .expected = .{ .dec_val = 99 * RocDec.one_point_zero_i128 },
    },
    .{ .name = "closure: closure that ignores capture and uses argument",
        .source =
            \\{
            \\    _unused = 999
            \\    f = |x| x + 1
            \\    f(41)
            \\}
        ,
        .expected = .{ .dec_val = 42 * RocDec.one_point_zero_i128 },
    },

    // TIER 11: Monomorphic identity -- isolating polymorphic specialization
    .{ .name = "closure: monomorphic Str identity (no polymorphism)",
        .source =
            \\{
            \\    identity : Str -> Str
            \\    identity = |val| val
            \\    identity("Hello")
            \\}
        ,
        .expected = .{ .str_val = "Hello" },
    },
    .{ .name = "closure: monomorphic Dec identity (no polymorphism)",
        .source =
            \\{
            \\    identity : Dec -> Dec
            \\    identity = |val| val
            \\    num = identity(5)
            \\    num
            \\}
        ,
        .expected = .{ .dec_val = 5 * RocDec.one_point_zero_i128 },
    },
    .{ .name = "closure: monomorphic Str identity with if-else (exact failing scenario but monomorphic)",
        .source =
            \\{
            \\    str_id : Str -> Str
            \\    str_id = |val| val
            \\    num = 5
            \\    str = str_id("Hello")
            \\    if (num > 0) str else ""
            \\}
        ,
        .expected = .{ .str_val = "Hello" },
    },

    // Regression: refcounting closures with heap-allocated captures
    .{ .name = "closure: multi-use closure with captured short string (SSO)",
        .source =
            \\{
            \\    s = "short"
            \\    f = |_x| s
            \\    _a = f(0)
            \\    f(0)
            \\}
        ,
        .expected = .{ .str_val = "short" },
    },
    .{ .name = "closure: multi-use closure with captured heap string needs incref",
        .source =
            \\{
            \\    s = "This string is definitely longer than twenty three bytes"
            \\    f = |_x| s
            \\    _a = f(0)
            \\    f(0)
            \\}
        ,
        .expected = .{ .str_val = "This string is definitely longer than twenty three bytes" },
    },
};
