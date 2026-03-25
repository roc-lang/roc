//! Data-driven eval test definitions for the parallel test runner.
//! Each entry corresponds to one `runExpect*` call from the original test files.
//! The parallel runner exercises every backend (interpreter, dev, wasm)
//! on each test and compares results.

const TestCase = @import("parallel_runner.zig").TestCase;
const RocDec = @import("builtins").dec.RocDec;

/// Skip all backends — used for tests that document bugs (crash/fail).
const SKIP_ALL: TestCase.Skip = .{ .interpreter = true, .dev = true, .wasm = true, .llvm = true };

/// All eval test cases, consumed by the parallel runner.
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
    // err_val tests removed — crash/error tests are no longer supported as a separate category
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

    // error: divide/modulo by zero tests removed (were err_val tests)

    // --- from eval_test.zig: simple lambda with if-else ---
    .{ .name = "simple lambda with if-else: positive", .source = "(|x| if x > 0.I64 x else 0.I64)(5.I64)", .expected = .{ .i64_val = 5 } },
    .{ .name = "simple lambda with if-else: negative", .source = "(|x| if x > 0.I64 x else 0.I64)(-3.I64)", .expected = .{ .i64_val = 0 } },

    // crash in else branch inside lambda test removed (was err_val test)

    // --- from eval_test.zig: crash NOT taken when condition true ---
    .{
        .name = "crash NOT taken when condition true",
        .source =
        \\(|x| if x > 0.I64 x else {
        \\    crash "this should not execute"
        \\    0.I64
        \\})(10.I64)
        ,
        .expected = .{ .i64_val = 10 },
    },

    // crash statement err_val tests removed

    // inline expect statement fails test removed (was err_val test)

    // --- from eval_test.zig: inline expect statement passes ---
    .{
        .name = "inline expect statement passes",
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
    .{
        .name = "lambdas with capture: x+y",
        .source =
        \\{
        \\    x = 10.I64
        \\    f = |y| x + y
        \\    f(5.I64)
        \\}
        ,
        .expected = .{ .i64_val = 15 },
    },
    .{
        .name = "lambdas with capture: x+y+z",
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
    .{
        .name = "closure with many captures (struct_captures)",
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
    .{
        .name = "lambdas nested closures",
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
    .{
        .name = "polymorphic identity function",
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
    .{
        .name = "direct polymorphic function usage",
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
    .{
        .name = "multiple polymorphic instantiations",
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
    .{
        .name = "recursive factorial function",
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
    .{
        .name = "mutable record equality",
        .source =
        \\{
        \\    var $x = { sum: 6 }
        \\    $x == { sum: 6 }
        \\}
        ,
        .expected = .{ .bool_val = true },
    },

    // --- from eval_test.zig: mutable record with rebind equality ---
    .{
        .name = "mutable record with rebind equality",
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
    .{
        .name = "mutable record loop accumulator equality",
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
    .{
        .name = "tag union eq: different tags with payload",
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
    .{
        .name = "tag union eq: three tags same",
        .source =
        \\{
        \\    x = Red
        \\    y = Red
        \\    x == y
        \\}
        ,
        .expected = .{ .bool_val = true },
    },
    .{
        .name = "tag union eq: three tags via if same",
        .source =
        \\{
        \\    x = Red
        \\    y = if Bool.True Red else if Bool.True Green else Blue
        \\    x == y
        \\}
        ,
        .expected = .{ .bool_val = true },
    },
    .{
        .name = "tag union eq: three tags diff",
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
    .{
        .name = "record with multiple types: same",
        .source =
        \\{ name: "alice", age: 30 } == { name: "alice", age: 30 }
        ,
        .expected = .{ .bool_val = true },
    },
    .{
        .name = "record with multiple types: diff name",
        .source =
        \\{ name: "alice", age: 30 } == { name: "bob", age: 30 }
        ,
        .expected = .{ .bool_val = false },
    },
    .{
        .name = "record with multiple types: diff age",
        .source =
        \\{ name: "alice", age: 30 } == { name: "alice", age: 31 }
        ,
        .expected = .{ .bool_val = false },
    },
    .{
        .name = "deeply nested mixed structures: same",
        .source =
        \\{ a: (1, { b: 2 }), c: 3 } == { a: (1, { b: 2 }), c: 3 }
        ,
        .expected = .{ .bool_val = true },
    },
    .{
        .name = "deeply nested mixed structures: diff",
        .source =
        \\{ a: (1, { b: 2 }), c: 3 } == { a: (1, { b: 9 }), c: 3 }
        ,
        .expected = .{ .bool_val = false },
    },
    .{ .name = "tuple of tuples eq: same", .source = "((1, 2), (3, 4)) == ((1, 2), (3, 4))", .expected = .{ .bool_val = true } },
    .{ .name = "tuple of tuples eq: diff", .source = "((1, 2), (3, 4)) == ((1, 2), (3, 5))", .expected = .{ .bool_val = false } },
    .{
        .name = "record with string and bool: same",
        .source =
        \\{ name: "hello", active: Bool.True } == { name: "hello", active: Bool.True }
        ,
        .expected = .{ .bool_val = true },
    },
    .{
        .name = "record with string and bool: diff",
        .source =
        \\{ name: "hello", active: Bool.True } == { name: "hello", active: Bool.False }
        ,
        .expected = .{ .bool_val = false },
    },

    // --- from eval_test.zig: tag union inside record/tuple equality ---
    .{
        .name = "tag union inside record: same",
        .source =
        \\{
        \\    a = { status: Ok(42) }
        \\    b = { status: Ok(42) }
        \\    a == b
        \\}
        ,
        .expected = .{ .bool_val = true },
    },
    .{
        .name = "tag union inside record: diff",
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
    .{
        .name = "record inside tag union inside tuple eq: same",
        .source =
        \\(Ok({ x: 1, y: 2 }), 42) == (Ok({ x: 1, y: 2 }), 42)
        ,
        .expected = .{ .bool_val = true },
    },
    .{
        .name = "record inside tag union inside tuple eq: diff",
        .source =
        \\(Ok({ x: 1, y: 2 }), 42) == (Ok({ x: 1, y: 9 }), 42)
        ,
        .expected = .{ .bool_val = false },
    },
    .{
        .name = "tuple inside record inside tag union eq: same",
        .source =
        \\Ok({ pair: (1, 2), val: 99 }) == Ok({ pair: (1, 2), val: 99 })
        ,
        .expected = .{ .bool_val = true },
    },
    .{
        .name = "tuple inside record inside tag union eq: diff",
        .source =
        \\Ok({ pair: (1, 2), val: 99 }) == Ok({ pair: (1, 9), val: 99 })
        ,
        .expected = .{ .bool_val = false },
    },
    .{
        .name = "tag union inside record inside tuple eq: same",
        .source =
        \\({ result: Ok(1) }, 99) == ({ result: Ok(1) }, 99)
        ,
        .expected = .{ .bool_val = true },
    },
    .{
        .name = "tag union inside record inside tuple eq: diff",
        .source =
        \\({ result: Ok(1) }, 99) == ({ result: Ok(2) }, 99)
        ,
        .expected = .{ .bool_val = false },
    },

    // --- from eval_test.zig: four-deep nested equality ---
    .{
        .name = "four-deep nested eq: same",
        .source =
        \\{ data: (Ok({ val: 42 }), 1) } == { data: (Ok({ val: 42 }), 1) }
        ,
        .expected = .{ .bool_val = true },
    },
    .{
        .name = "four-deep nested eq: diff",
        .source =
        \\{ data: (Ok({ val: 42 }), 1) } == { data: (Ok({ val: 99 }), 1) }
        ,
        .expected = .{ .bool_val = false },
    },

    // --- from eval_test.zig: long string fields equality ---
    .{
        .name = "record long string eq: same",
        .source =
        \\{ name: "this string is long enough to avoid SSO optimization" } == { name: "this string is long enough to avoid SSO optimization" }
        ,
        .expected = .{ .bool_val = true },
    },
    .{
        .name = "record long string eq: diff",
        .source =
        \\{ name: "this string is long enough to avoid SSO optimization" } == { name: "different long string that also avoids SSO optimization" }
        ,
        .expected = .{ .bool_val = false },
    },
    .{
        .name = "record long string neq: same",
        .source =
        \\{ name: "this string is long enough to avoid SSO optimization" } != { name: "this string is long enough to avoid SSO optimization" }
        ,
        .expected = .{ .bool_val = false },
    },
    .{
        .name = "record long string neq: diff",
        .source =
        \\{ name: "this string is long enough to avoid SSO optimization" } != { name: "different long string that also avoids SSO optimization" }
        ,
        .expected = .{ .bool_val = true },
    },
    .{
        .name = "tuple long string eq: same",
        .source =
        \\("this string is long enough to avoid SSO optimization", 42) == ("this string is long enough to avoid SSO optimization", 42)
        ,
        .expected = .{ .bool_val = true },
    },
    .{
        .name = "tuple long string eq: diff",
        .source =
        \\("this string is long enough to avoid SSO optimization", 42) == ("different long string that also avoids SSO optimization", 42)
        ,
        .expected = .{ .bool_val = false },
    },
    .{
        .name = "record multi long string eq: same",
        .source =
        \\{ a: "first long string exceeding SSO limit!!", b: "second long string exceeding SSO limit!" } == { a: "first long string exceeding SSO limit!!", b: "second long string exceeding SSO limit!" }
        ,
        .expected = .{ .bool_val = true },
    },
    .{
        .name = "record multi long string eq: diff",
        .source =
        \\{ a: "first long string exceeding SSO limit!!", b: "second long string exceeding SSO limit!" } == { a: "first long string exceeding SSO limit!!", b: "DIFFERENT long string exceeding SSO!!!!" }
        ,
        .expected = .{ .bool_val = false },
    },
    .{
        .name = "long string inside record inside tuple eq: same",
        .source =
        \\({ name: "this string is long enough to avoid SSO optimization" }, 1) == ({ name: "this string is long enough to avoid SSO optimization" }, 1)
        ,
        .expected = .{ .bool_val = true },
    },
    .{
        .name = "long string inside record inside tuple eq: diff",
        .source =
        \\({ name: "this string is long enough to avoid SSO optimization" }, 1) == ({ name: "different long string that also avoids SSO optimization" }, 1)
        ,
        .expected = .{ .bool_val = false },
    },
    .{
        .name = "tag union long string payload eq: same",
        .source =
        \\Ok("this string is long enough to avoid SSO optimization") == Ok("this string is long enough to avoid SSO optimization")
        ,
        .expected = .{ .bool_val = true },
    },
    .{
        .name = "tag union long string payload eq: diff",
        .source =
        \\Ok("this string is long enough to avoid SSO optimization") == Ok("different long string that also avoids SSO optimization")
        ,
        .expected = .{ .bool_val = false },
    },
    .{
        .name = "tag union long string payload neq: same",
        .source =
        \\Ok("this string is long enough to avoid SSO optimization") != Ok("this string is long enough to avoid SSO optimization")
        ,
        .expected = .{ .bool_val = false },
    },
    .{
        .name = "tag union long string payload neq: diff",
        .source =
        \\Ok("this string is long enough to avoid SSO optimization") != Ok("different long string that also avoids SSO optimization")
        ,
        .expected = .{ .bool_val = true },
    },

    // --- from eval_test.zig: equality in control flow ---
    .{
        .name = "equality result in if: true",
        .source =
        \\if { x: 1 } == { x: 1 } 42 else 0
        ,
        .expected = .{ .dec_val = 42 * RocDec.one_point_zero_i128 },
    },
    .{
        .name = "equality result in if: false",
        .source =
        \\if { x: 1 } == { x: 2 } 42 else 0
        ,
        .expected = .{ .dec_val = 0 * RocDec.one_point_zero_i128 },
    },

    // --- from eval_test.zig: equality with variable bindings ---
    .{
        .name = "equality var bindings: same",
        .source =
        \\{
        \\    a = { x: 10, y: 20 }
        \\    b = { x: 10, y: 20 }
        \\    a == b
        \\}
        ,
        .expected = .{ .bool_val = true },
    },
    .{
        .name = "equality var bindings: diff",
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
    .{
        .name = "inequality var bindings tuples: same",
        .source =
        \\{
        \\    a = (1, 2, 3)
        \\    b = (1, 2, 3)
        \\    a != b
        \\}
        ,
        .expected = .{ .bool_val = false },
    },
    .{
        .name = "inequality var bindings tuples: diff",
        .source =
        \\{
        \\    a = (1, 2, 3)
        \\    b = (1, 2, 4)
        \\    a != b
        \\}
        ,
        .expected = .{ .bool_val = true },
    },
    .{
        .name = "inequality var bindings records: same",
        .source =
        \\{
        \\    a = { x: 10, y: 20 }
        \\    b = { x: 10, y: 20 }
        \\    a != b
        \\}
        ,
        .expected = .{ .bool_val = false },
    },
    .{
        .name = "inequality var bindings records: diff",
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
    .{
        .name = "record update evaluates extension once",
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
    .{
        .name = "record update synthesizes missing fields",
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
    .{
        .name = "if block with local bindings",
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
    .{
        .name = "List.len returns proper U64 nominal type: empty",
        .source =
        \\{
        \\    n = List.len([])
        \\    n.to_str()
        \\}
        ,
        .expected = .{ .str_val = "0" },
    },
    .{
        .name = "List.len returns proper U64 nominal type: non-empty",
        .source =
        \\{
        \\    n = List.len([1, 2, 3])
        \\    n.to_str()
        \\}
        ,
        .expected = .{ .str_val = "3" },
    },
    .{
        .name = "type annotation on var declaration",
        .source =
        \\{
        \\    var $foo : U8
        \\    var $foo = 42
        \\    $foo
        \\}
        ,
        .expected = .{ .i64_val = 42 },
    },
    .{
        .name = "List.get with polymorphic numeric index",
        .source =
        \\{
        \\    list = [10, 20, 30]
        \\    index = 0
        \\    match List.get(list, index) { Ok(v) => v, _ => 0 }
        \\}
        ,
        .expected = .{ .dec_val = 10 * RocDec.one_point_zero_i128 },
    },
    .{
        .name = "for loop element type from list runtime type",
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
    .{
        .name = "List.get method dispatch on Try type",
        .source =
        \\{
        \\    list = ["hello"]
        \\    List.get(list, 0).ok_or("fallback")
        \\}
        ,
        .expected = .{ .str_val = "hello" },
    },
    .{
        .name = "List.get with list var and when destructure",
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
    .{
        .name = "record destructuring with assignment",
        .source =
        \\{
        \\    rec = { x: 1, y: 2 }
        \\    { x, y } = rec
        \\    x + y
        \\}
        ,
        .expected = .{ .dec_val = 3 * RocDec.one_point_zero_i128 },
    },
    .{
        .name = "record field access - regression 8647",
        .source =
        \\{
        \\    rec = { name: "test" }
        \\    rec.name
        \\}
        ,
        .expected = .{ .str_val = "test" },
    },
    .{
        .name = "record field access with multiple string fields",
        .source =
        \\{
        \\    record = { x: "a", y: "b" }
        \\    record.x
        \\}
        ,
        .expected = .{ .str_val = "a" },
    },
    .{
        .name = "method calls on numeric variables: float",
        .source =
        \\{
        \\    x = 7.0
        \\    x.to_str()
        \\}
        ,
        .expected = .{ .str_val = "7.0" },
    },
    .{
        .name = "method calls on numeric variables: int",
        .source =
        \\{
        \\    x = 42
        \\    x.to_str()
        \\}
        ,
        .expected = .{ .str_val = "42.0" },
    },
    .{ .name = "issue 8710: list len", .source = "[1.I64, 2.I64, 3.I64].len()", .expected = .{ .i64_val = 3 } },
    .{
        .name = "issue 8727: make_adder",
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
    .{
        .name = "issue 8737: tag union with tuple payload",
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
    .{
        .name = "issue 8737: nested tuple pattern destructure",
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
    .{
        .name = "early return: ? with Ok",
        .source =
        \\{
        \\    compute = |x| Ok(x?)
        \\    match compute(Ok(42.I64)) { Ok(v) => v, _ => 0 }
        \\}
        ,
        .expected = .{ .i64_val = 42 },
    },
    .{
        .name = "early return: ? with Err",
        .source =
        \\{
        \\    compute = |x| Ok(x?)
        \\    match compute(Err({})) { Ok(_) => 1, Err(_) => 0 }
        \\}
        ,
        .expected = .{ .dec_val = 0 * RocDec.one_point_zero_i128 },
    },
    .{
        .name = "early return: ? in List.map closure",
        .source =
        \\{
        \\    result = [Ok(1), Err({})].map(|x| Ok(x?))
        \\    List.len(result)
        \\}
        ,
        .expected = .{ .i64_val = 2 },
    },
    .{
        .name = "early return: ? in second arg",
        .source =
        \\{
        \\    my_func = |_a, b| b
        \\    compute = |x| Ok(x?)
        \\    match my_func(42, compute(Err({}))) { Ok(_) => 1, Err(_) => 0 }
        \\}
        ,
        .expected = .{ .dec_val = 0 * RocDec.one_point_zero_i128 },
    },
    .{
        .name = "early return: ? in first arg",
        .source =
        \\{
        \\    my_func = |a, _b| a
        \\    compute = |x| Ok(x?)
        \\    match my_func(compute(Err({})), 42) { Ok(_) => 1, Err(_) => 0 }
        \\}
        ,
        .expected = .{ .dec_val = 0 * RocDec.one_point_zero_i128 },
    },
    .{
        .name = "issue 8979: while True with break",
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
    .{
        .name = "Decoder: create ok result - check is Ok",
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
    .{
        .name = "Decoder: create ok result - extract value",
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
    .{
        .name = "Decoder: create err result",
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
    .{
        .name = "decode: I32.decode type mismatch crash",
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
        .expected = .{ .problem = {} },
    },

    // --- from eval_test.zig: debug 8783 series ---
    .{
        .name = "debug 8783a: lambda with tag match",
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
    .{
        .name = "debug 8783b: fold with simple addition",
        .source =
        \\{
        \\    items = [1.I64, 2.I64, 3.I64]
        \\    List.fold(items, 0.I64, |acc, x| acc + x)
        \\}
        ,
        .expected = .{ .i64_val = 6 },
    },
    .{
        .name = "debug 8783g: match on payload tag without fold",
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
    .{
        .name = "match on zst-payload tag union",
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
    .{
        .name = "proc return of zst-payload tag union",
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
    .{
        .name = "debug 8783f: fold with tag match single payload",
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
    .{
        .name = "debug 8783c: fold with tag match",
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
    .{
        .name = "issue 8783: fold match on tag union from pattern match",
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
    .{
        .name = "issue 8821: List.get with records and match",
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
    .{
        .name = "issue 8821 reduced: match ignores payload body",
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
    .{
        .name = "issue 8821 reduced: without matching result",
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
    .{
        .name = "encode: string to utf8 and back",
        .source =
        \\{
        \\    bytes = Str.to_utf8("hello")
        \\    Str.from_utf8_lossy(bytes)
        \\}
        ,
        .expected = .{ .str_val = "hello" },
    },

    // --- from eval_test.zig: static dispatch ---
    .{
        .name = "static dispatch: List.sum",
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
    .{
        .name = "issue 8814: List.get on function parameter",
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
    .{
        .name = "issue 8831: self-referential value definition",
        .source =
        \\{
        \\    a = a
        \\    a
        \\}
        ,
        .expected = .{ .problem = {} },
    },
    .{
        .name = "issue 8831: nested self-reference in list",
        .source =
        \\{
        \\    a = [a]
        \\    a
        \\}
        ,
        .expected = .{ .problem = {} },
    },
    .{
        .name = "issue 9043: self-reference in tuple pattern",
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
    .{
        .name = "issue 9262: opaque function field returning tag union",
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
    .{
        .name = "recursive function with record - stack memory",
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
    .{
        .name = "issue 8872: polymorphic tag union payload layout",
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
    .{
        .name = "match on tag union with different sizes",
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
    .{
        .name = "polymorphic tag transform with match",
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
    .{
        .name = "proc with tag match returning non-tag type",
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
    .{
        .name = "lambda with list param: List.len",
        .source =
        \\{
        \\    get_len = |l| List.len(l)
        \\    get_len([1.I64, 2.I64, 3.I64])
        \\}
        ,
        .expected = .{ .i64_val = 3 },
    },
    .{
        .name = "lambda with list param: List.append",
        .source =
        \\{
        \\    add_one = |l| List.len(List.append(l, 99.I64))
        \\    add_one([1.I64, 2.I64, 3.I64])
        \\}
        ,
        .expected = .{ .i64_val = 4 },
    },
    .{
        .name = "lambda with list param and var",
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
    .{
        .name = "lambda with list param and list literal",
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
    .{
        .name = "lambda with list param var for loop",
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
    .{
        .name = "lambda with list param var List.append no loop",
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
    .{
        .name = "minimal lambda with list param for loop",
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
    .{
        .name = "lambda with list param for loop alloc inside",
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
    .{
        .name = "lambda for loop over internal list scalar param",
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
    .{
        .name = "lambda list param for loop internal list alloc",
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
    .{
        .name = "lambda list param for loop empty iteration",
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
    .{
        .name = "lambda list param for loop append single",
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
    .{
        .name = "lambda list param var for loop List.append",
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
    .{
        .name = "issue 8899: closure decref in for loop",
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
    .{
        .name = "issue 8927: early return in method argument",
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
    .{
        .name = "issue 8946: closure capturing for-loop element",
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
    .{
        .name = "issue 8978: incref alignment recursive tag unions",
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
    .{
        .name = "owned record wildcard field cleanup",
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
    .{
        .name = "higher-order function: simple apply",
        .source =
        \\{
        \\    apply = |f, x| f(x)
        \\    apply(|n| n + 1.I64, 5.I64)
        \\}
        ,
        .expected = .{ .i64_val = 6 },
    },
    .{
        .name = "higher-order function: apply with closure",
        .source =
        \\{
        \\    offset = 10.I64
        \\    apply = |f, x| f(x)
        \\    apply(|n| n + offset, 5.I64)
        \\}
        ,
        .expected = .{ .i64_val = 15 },
    },
    .{
        .name = "higher-order function: twice",
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
    .{
        .name = "diag: lambda returning tag union",
        .source =
        \\{
        \\    f = |x| Ok(x)
        \\    match f(42) { Ok(v) => v, _ => 0 }
        \\}
        ,
        .expected = .{ .dec_val = 42 * RocDec.one_point_zero_i128 },
    },
    .{
        .name = "diag: identity lambda call",
        .source =
        \\{
        \\    f = |x| x
        \\    f(42)
        \\}
        ,
        .expected = .{ .dec_val = 42 * RocDec.one_point_zero_i128 },
    },
    .{
        .name = "diag: lambda wrapping try suffix",
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
    .{
        .name = "polymorphic tag union payload: extract",
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
    .{
        .name = "polymorphic tag union payload: multiple type vars",
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
    .{
        .name = "polymorphic tag union: erroneous match branch crashes",
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
        .expected = .{ .problem = {} },
    },
    .{
        .name = "polymorphic: erroneous if-else branch crashes",
        .source =
        \\{
        \\    get_val : Bool, e -> e
        \\    get_val = |flag, val| if (flag) "" else val
        \\
        \\    get_val(Bool.true, 42)
        \\}
        ,
        .expected = .{ .problem = {} },
    },
    .{
        .name = "polymorphic tag union: erroneous match in block crashes",
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
        .expected = .{ .problem = {} },
    },
    .{
        .name = "polymorphic tag union payload: wrap and unwrap",
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
    .{ .name = "dev only: Bool.True formats as True", .source = "Bool.True", .expected = .{ .inspect_str = "True" }, .skip = .{ .interpreter = true, .wasm = true } },
    .{ .name = "dev only: Bool.False formats as False", .source = "Bool.False", .expected = .{ .inspect_str = "False" }, .skip = .{ .interpreter = true, .wasm = true } },
    .{ .name = "dev only: Bool.not(Bool.True) formats as False", .source = "Bool.not(Bool.True)", .expected = .{ .inspect_str = "False" }, .skip = .{ .interpreter = true, .wasm = true } },
    .{ .name = "dev only: Bool.not(Bool.False) formats as True", .source = "Bool.not(Bool.False)", .expected = .{ .inspect_str = "True" }, .skip = .{ .interpreter = true, .wasm = true } },
    .{ .name = "dev only: Bool.not(False) formats as True", .source = "Bool.not(False)", .expected = .{ .inspect_str = "True" }, .skip = .{ .interpreter = true, .wasm = true } },
    .{ .name = "dev only: !Bool.True formats as False", .source = "!Bool.True", .expected = .{ .inspect_str = "False" }, .skip = .{ .interpreter = true, .wasm = true } },
    .{ .name = "dev only: !Bool.False formats as True", .source = "!Bool.False", .expected = .{ .inspect_str = "True" }, .skip = .{ .interpreter = true, .wasm = true } },
    .{ .name = "dev only: nested List.append U32", .source = "List.append(List.append([], 1.U32), 2.U32)", .expected = .{ .inspect_str = "[1, 2]" }, .skip = .{ .interpreter = true, .wasm = true } },
    .{ .name = "dev only: U32 literal", .source = "15.U32", .expected = .{ .inspect_str = "15" }, .skip = .{ .interpreter = true, .wasm = true } },
    .{ .name = "dev only: U32 comparison", .source = "1.U32 <= 5.U32", .expected = .{ .inspect_str = "True" }, .skip = .{ .interpreter = true, .wasm = true } },
    .{ .name = "dev only: U32 addition", .source = "1.U32 + 2.U32", .expected = .{ .inspect_str = "3" }, .skip = .{ .interpreter = true, .wasm = true } },
    .{
        .name = "dev only: while loop increment U32",
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
        .expected = .{ .inspect_str = "6" },
        .skip = .{ .interpreter = true, .wasm = true },
    },
    .{
        .name = "dev only: while loop sum U32",
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
        .expected = .{ .inspect_str = "15" },
        .skip = .{ .interpreter = true, .wasm = true },
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
    .{
        .name = "polymorphic closure capture: int",
        .source =
        \\{
        \\    make_getter = |n| |_x| n
        \\    get_num = make_getter(42)
        \\    get_num(0)
        \\}
        ,
        .expected = .{ .dec_val = 42 * RocDec.one_point_zero_i128 },
    },
    .{
        .name = "polymorphic closure capture: str",
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
    .{
        .name = "large record chained HOF: w",
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
    .{
        .name = "large record chained HOF: y",
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
    .{
        .name = "Str.split_on and Str.join_with",
        .source =
        \\{
        \\    parts = Str.split_on("a,b,c", ",")
        \\    Str.join_with(parts, "-")
        \\}
        ,
        .expected = .{ .str_val = "a-b-c" },
    },
    .{
        .name = "Str.join_with",
        .source =
        \\Str.join_with(["hello", "world"], " ")
        ,
        .expected = .{ .str_val = "hello world" },
    },

    // --- from eval_test.zig: dev only List/Str tests ---
    .{ .name = "dev: List.last returns Ok", .source = "List.last([1, 2, 3])", .expected = .{ .inspect_str = "Ok(3.0)" }, .skip = .{ .interpreter = true, .wasm = true } },
    .{ .name = "dev: List.first returns Ok", .source = "List.first([10, 20, 30])", .expected = .{ .inspect_str = "Ok(10.0)" }, .skip = .{ .interpreter = true, .wasm = true } },
    .{ .name = "dev: List.first empty returns Err", .source = "List.first([])", .expected = .{ .inspect_str = "Err(ListWasEmpty)" }, .skip = .{ .interpreter = true, .wasm = true } },
    .{ .name = "dev: Str.from_utf8 Ok", .source = "Str.from_utf8([72, 105])", .expected = .{ .inspect_str = "Ok(\"Hi\")" }, .skip = .{ .interpreter = true, .wasm = true } },
    .{
        .name = "dev: polymorphic sum in block U64",
        .source =
        \\{
        \\    sum = |a, b| a + b + 0
        \\    U64.to_str(sum(240, 20))
        \\}
        ,
        .expected = .{ .inspect_str = "\"260\"" },
        .skip = .{ .interpreter = true, .wasm = true },
    },
    .{ .name = "dev: List.contains int", .source = "List.contains([1, 2, 3, 4, 5], 3)", .expected = .{ .inspect_str = "True" }, .skip = .{ .interpreter = true, .wasm = true } },
    .{ .name = "dev: List.any inline true", .source = "List.any([1, 2, 3], |x| x == 2)", .expected = .{ .inspect_str = "True" }, .skip = .{ .interpreter = true, .wasm = true } },
    .{ .name = "dev: List.any inline false", .source = "List.any([1, 2, 3], |x| x == 5)", .expected = .{ .inspect_str = "False" }, .skip = .{ .interpreter = true, .wasm = true } },
    .{ .name = "dev: List.any always true", .source = "List.any([1, 2, 3], |_x| True)", .expected = .{ .inspect_str = "True" }, .skip = .{ .interpreter = true, .wasm = true } },
    .{ .name = "dev: List.any typed elements", .source = "List.any([1.I64, 2.I64, 3.I64], |_x| True)", .expected = .{ .inspect_str = "True" }, .skip = .{ .interpreter = true, .wasm = true } },
    .{
        .name = "dev: polymorphic predicate comparison",
        .source =
        \\{
        \\    is_positive = |x| x > 0
        \\    List.any([-1, 0, 1], is_positive)
        \\}
        ,
        .expected = .{ .inspect_str = "True" },
        .skip = .{ .interpreter = true, .wasm = true },
    },
    .{
        .name = "dev: polymorphic comparison lambda direct",
        .source =
        \\{
        \\    is_positive = |x| x > 0
        \\    is_positive(5)
        \\}
        ,
        .expected = .{ .inspect_str = "True" },
        .skip = .{ .interpreter = true, .wasm = true },
    },
    .{
        .name = "dev: polymorphic comparison lambda List.any",
        .source =
        \\{
        \\    gt_zero = |x| x > 0
        \\    List.any([1, 2, 3], gt_zero)
        \\}
        ,
        .expected = .{ .inspect_str = "True" },
        .skip = .{ .interpreter = true, .wasm = true },
    },
    .{ .name = "dev: List.any inline lambda", .source = "List.any([1, 2, 3], |x| x > 0)", .expected = .{ .inspect_str = "True" }, .skip = .{ .interpreter = true, .wasm = true } },
    .{
        .name = "dev: for loop early return",
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
        .expected = .{ .inspect_str = "True" },
        .skip = .{ .interpreter = true, .wasm = true },
    },
    .{
        .name = "dev: for loop closure early return",
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
        .expected = .{ .inspect_str = "True" },
        .skip = .{ .interpreter = true, .wasm = true },
    },
    .{
        .name = "dev: local any-style HOF equality predicate",
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
        .expected = .{ .inspect_str = "True" },
        .skip = .{ .interpreter = true, .wasm = true },
    },
    .{
        .name = "dev: inline any-style HOF always true",
        .source =
        \\(|list, pred| {
        \\    for item in list {
        \\        if pred(item) { return True }
        \\    }
        \\    False
        \\})([1, 2, 3], |_x| True)
        ,
        .expected = .{ .inspect_str = "True" },
        .skip = .{ .interpreter = true, .wasm = true },
    },

    // --- from eval_test.zig: polymorphic function tests ---
    .{
        .name = "polymorphic function: two list types",
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
    .{
        .name = "direct List.contains I64",
        .source =
        \\{
        \\    a : List(I64)
        \\    a = [1, 2, 3]
        \\    if a.contains(2) { 1 } else { 0 }
        \\}
        ,
        .expected = .{ .dec_val = 1 * RocDec.one_point_zero_i128 },
    },
    .{
        .name = "polymorphic function single call I64",
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
    .{
        .name = "polymorphic function single call Str",
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
    .{
        .name = "polymorphic function List.contains two types",
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
    .{
        .name = "polymorphic function List.contains multiple types",
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
    .{
        .name = "nested List.any true path captured Str",
        .source =
        \\{
        \\    out = ["a"]
        \\    List.any(["a"], |item| out.contains(item))
        \\}
        ,
        .expected = .{ .bool_val = true },
    },
    .{
        .name = "nested List.any false path captured Str",
        .source =
        \\{
        \\    out = ["a"]
        \\    List.any(["b"], |item| out.contains(item))
        \\}
        ,
        .expected = .{ .bool_val = false },
    },
    .{
        .name = "direct List.contains captured Str",
        .source =
        \\{
        \\    out = ["a"]
        \\    out.contains("a")
        \\}
        ,
        .expected = .{ .bool_val = true },
    },
    .{
        .name = "forwarding tag union Str payload no leak",
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
    .{
        .name = "focused: fold multi-field record field checks",
        .source =
        \\{
        \\    rec = List.fold([1, 2, 3], {sum: 0, count: 0}, |acc, item| {sum: acc.sum + item, count: acc.count + 1})
        \\    rec.sum == 6 and rec.count == 3
        \\}
        ,
        .expected = .{ .bool_val = true },
    },
    .{
        .name = "focused: fold multi-field record sum check",
        .source =
        \\{
        \\    rec = List.fold([1, 2, 3], {sum: 0, count: 0}, |acc, item| {sum: acc.sum + item, count: acc.count + 1})
        \\    rec.sum == 6
        \\}
        ,
        .expected = .{ .bool_val = true },
    },
    .{
        .name = "focused: fold multi-field record count check",
        .source =
        \\{
        \\    rec = List.fold([1, 2, 3], {sum: 0, count: 0}, |acc, item| {sum: acc.sum + item, count: acc.count + 1})
        \\    rec.count == 3
        \\}
        ,
        .expected = .{ .bool_val = true },
    },
    .{
        .name = "focused: fold multi-field record sum value",
        .source =
        \\{
        \\    rec = List.fold([1, 2, 3], {sum: 0, count: 0}, |acc, item| {sum: acc.sum + item, count: acc.count + 1})
        \\    rec.sum
        \\}
        ,
        .expected = .{ .dec_val = 6_000_000_000_000_000_000 },
    },
    .{
        .name = "focused: fold multi-field record count value",
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
    .{
        .name = "focused: polymorphic additional specialization via List.append",
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
    .{
        .name = "closure: lambda capturing one local variable",
        .source =
        \\{
        \\    y = 10
        \\    f = |x| x + y
        \\    f(5)
        \\}
        ,
        .expected = .{ .dec_val = 15 * RocDec.one_point_zero_i128 },
    },
    .{
        .name = "closure: lambda capturing two local variables",
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
    .{
        .name = "closure: lambda capturing a string",
        .source =
        \\{
        \\    greeting = "Hello"
        \\    f = |name| Str.concat(greeting, name)
        \\    f(" World")
        \\}
        ,
        .expected = .{ .str_val = "Hello World" },
    },
    .{
        .name = "closure: lambda capturing multiple strings",
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
    .{
        .name = "closure: function returning a closure (make_adder)",
        .source =
        \\{
        \\    make_adder = |n| |x| x + n
        \\    add5 = make_adder(5)
        \\    add5(10)
        \\}
        ,
        .expected = .{ .dec_val = 15 * RocDec.one_point_zero_i128 },
    },
    .{
        .name = "closure: function returning a closure, called twice",
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
    .{
        .name = "closure: two different closures from same factory",
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
    .{
        .name = "closure: function returning a closure over string",
        .source =
        \\{
        \\    make_greeter = |greeting| |name| Str.concat(greeting, name)
        \\    greet = make_greeter("Hi ")
        \\    greet("Alice")
        \\}
        ,
        .expected = .{ .str_val = "Hi Alice" },
    },
    .{
        .name = "closure: two-level deep closure (function returning function returning function)",
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
    .{
        .name = "closure: passing closure to higher-order function",
        .source =
        \\{
        \\    apply = |f, x| f(x)
        \\    y = 10
        \\    apply(|x| x + y, 5)
        \\}
        ,
        .expected = .{ .dec_val = 15 * RocDec.one_point_zero_i128 },
    },
    .{
        .name = "closure: passing two different closures to same HOF",
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
    .{
        .name = "closure: passing two different closures to same HOF returns first result",
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
    .{
        .name = "closure: passing two different closures to same HOF returns second result",
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
    .{
        .name = "closure: HOF calling closure argument twice",
        .source =
        \\{
        \\    apply_twice = |f, x| f(f(x))
        \\    y = 3
        \\    apply_twice(|x| x + y, 10)
        \\}
        ,
        .expected = .{ .dec_val = 16 * RocDec.one_point_zero_i128 },
    },
    .{
        .name = "closure: HOF with closure returning string",
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
    .{
        .name = "closure: polymorphic identity applied to closure result",
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
    .{
        .name = "closure: polymorphic function used with both int and string closures",
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
    .{
        .name = "closure: closure forwarding to captured closure (no multiply)",
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
    .{
        .name = "closure: closure capturing another closure",
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
    .{
        .name = "closure: closure capturing a factory-produced closure",
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
    .{
        .name = "closure: if-else choosing between two closures with different captures",
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
    .{
        .name = "closure: if-else choosing between two closures, false branch",
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
    .{
        .name = "closure: if-else choosing between closures with different capture counts",
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
    .{
        .name = "closure: closure stored in record field then called",
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
    .{
        .name = "closure: two closures in record, each with own captures",
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
    .{
        .name = "closure: record field closure add_a preserves its capture",
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
    .{
        .name = "closure: parenthesized record field closure add_b preserves its capture",
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
    .{
        .name = "closure: record field closure add_b preserves its capture",
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
    .{
        .name = "closure: compose two functions",
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
    .{
        .name = "closure: compose with captures",
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
    .{
        .name = "closure: pipe (flip of compose)",
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
    .{
        .name = "closure: recursive function in let binding",
        .source =
        \\{
        \\    factorial = |n| if (n <= 1) 1 else n * factorial(n - 1)
        \\    factorial(5)
        \\}
        ,
        .expected = .{ .dec_val = 120 * RocDec.one_point_zero_i128 },
    },
    .{
        .name = "closure: mutual recursion between two closures",
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
    .{
        .name = "closure: triple-nested closure factory",
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
    .{
        .name = "closure: closure capturing another closure (2 levels)",
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
    .{
        .name = "closure: closure capturing another closure that captures a third",
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
    .{
        .name = "closure: HOF receiving closure, returning closure that captures the argument closure",
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
    .{
        .name = "closure: HOF receiving closure with captures, returning closure that captures it",
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
    .{
        .name = "closure: chained closure factories with accumulating captures",
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
    .{
        .name = "closure: polymorphic HOF with closures capturing different types",
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
    .{
        .name = "closure: closure over bool used in conditional",
        .source =
        \\{
        \\    flag = True
        \\    choose = |a, b| if (flag) a else b
        \\    choose(42, 0)
        \\}
        ,
        .expected = .{ .dec_val = 42 * RocDec.one_point_zero_i128 },
    },
    .{
        .name = "closure: deeply nested blocks each adding captures",
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
    .{
        .name = "closure: same variable captured by multiple independent closures",
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
    .{
        .name = "closure: closure returning a string that includes a captured string",
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
    .{
        .name = "closure: applying the same closure to different arguments",
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
    .{
        .name = "closure: immediately invoked closure with capture",
        .source =
        \\{
        \\    y = 42
        \\    (|x| x + y)(8)
        \\}
        ,
        .expected = .{ .dec_val = 50 * RocDec.one_point_zero_i128 },
    },
    .{
        .name = "closure: closure that ignores its argument but uses capture",
        .source =
        \\{
        \\    val = 99
        \\    f = |_| val
        \\    f(0)
        \\}
        ,
        .expected = .{ .dec_val = 99 * RocDec.one_point_zero_i128 },
    },
    .{
        .name = "closure: closure that ignores capture and uses argument",
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
    .{
        .name = "closure: monomorphic Str identity (no polymorphism)",
        .source =
        \\{
        \\    identity : Str -> Str
        \\    identity = |val| val
        \\    identity("Hello")
        \\}
        ,
        .expected = .{ .str_val = "Hello" },
    },
    .{
        .name = "closure: monomorphic Dec identity (no polymorphism)",
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
    .{
        .name = "closure: monomorphic Str identity with if-else (exact failing scenario but monomorphic)",
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
    .{
        .name = "closure: multi-use closure with captured short string (SSO)",
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
    .{
        .name = "closure: multi-use closure with captured heap string needs incref",
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

    // --- from arithmetic_comprehensive_test.zig ---
    // TODO: U8 and U16 large-value arithmetic hangs on x86_64-linux CI.
    // All U8/U16 tests are skipped until the interpreter bug is fixed.

    // U8: plus
    .{
        .name = "U8: plus: 200 + 50",
        .source =
        \\{
        \\    a : U8
        \\    a = 200
        \\    b : U8
        \\    b = 50
        \\    a + b
        \\}
        ,
        .expected = .{ .u8_val = 250 },
        .skip = SKIP_ALL,
    },
    .{
        .name = "U8: plus: 255 + 0",
        .source =
        \\{
        \\    a : U8
        \\    a = 255
        \\    b : U8
        \\    b = 0
        \\    a + b
        \\}
        ,
        .expected = .{ .u8_val = 255 },
        .skip = SKIP_ALL,
    },
    .{
        .name = "U8: plus: 128 + 127",
        .source =
        \\{
        \\    a : U8
        \\    a = 128
        \\    b : U8
        \\    b = 127
        \\    a + b
        \\}
        ,
        .expected = .{ .u8_val = 255 },
        .skip = SKIP_ALL,
    },

    // U8: minus
    .{
        .name = "U8: minus: 200 - 50",
        .source =
        \\{
        \\    a : U8
        \\    a = 200
        \\    b : U8
        \\    b = 50
        \\    a - b
        \\}
        ,
        .expected = .{ .u8_val = 150 },
        .skip = SKIP_ALL,
    },
    // TODO: hangs on x86_64-linux CI (U8/U16 large-value arithmetic infinite loop)
    .{
        .name = "U8: minus: 255 - 100",
        .source =
        \\{
        \\    a : U8
        \\    a = 255
        \\    b : U8
        \\    b = 100
        \\    a - b
        \\}
        ,
        .expected = .{ .u8_val = 155 },
        .skip = SKIP_ALL,
    },
    .{
        .name = "U8: minus: 240 - 240",
        .source =
        \\{
        \\    a : U8
        \\    a = 240
        \\    b : U8
        \\    b = 240
        \\    a - b
        \\}
        ,
        .expected = .{ .u8_val = 0 },
        .skip = SKIP_ALL,
    },

    // U8: times
    .{
        .name = "U8: times: 15 * 17",
        .source =
        \\{
        \\    a : U8
        \\    a = 15
        \\    b : U8
        \\    b = 17
        \\    a * b
        \\}
        ,
        .expected = .{ .u8_val = 255 },
        .skip = SKIP_ALL,
    },
    .{
        .name = "U8: times: 128 * 1",
        .source =
        \\{
        \\    a : U8
        \\    a = 128
        \\    b : U8
        \\    b = 1
        \\    a * b
        \\}
        ,
        .expected = .{ .u8_val = 128 },
        .skip = SKIP_ALL,
    },
    .{
        .name = "U8: times: 16 * 15",
        .source =
        \\{
        \\    a : U8
        \\    a = 16
        \\    b : U8
        \\    b = 15
        \\    a * b
        \\}
        ,
        .expected = .{ .u8_val = 240 },
        .skip = SKIP_ALL,
    },

    // U8: div_by
    .{
        .name = "U8: div_by: 240 // 2",
        .source =
        \\{
        \\    a : U8
        \\    a = 240
        \\    b : U8
        \\    b = 2
        \\    a // b
        \\}
        ,
        .expected = .{ .u8_val = 120 },
        .skip = SKIP_ALL,
    },
    .{
        .name = "U8: div_by: 255 // 15",
        .source =
        \\{
        \\    a : U8
        \\    a = 255
        \\    b : U8
        \\    b = 15
        \\    a // b
        \\}
        ,
        .expected = .{ .u8_val = 17 },
        .skip = SKIP_ALL,
    },
    .{
        .name = "U8: div_by: 200 // 10",
        .source =
        \\{
        \\    a : U8
        \\    a = 200
        \\    b : U8
        \\    b = 10
        \\    a // b
        \\}
        ,
        .expected = .{ .u8_val = 20 },
        .skip = SKIP_ALL,
    },

    // U8: rem_by
    .{
        .name = "U8: rem_by: 200 % 13",
        .source =
        \\{
        \\    a : U8
        \\    a = 200
        \\    b : U8
        \\    b = 13
        \\    a % b
        \\}
        ,
        .expected = .{ .u8_val = 5 },
        .skip = SKIP_ALL,
    },
    .{
        .name = "U8: rem_by: 255 % 16",
        .source =
        \\{
        \\    a : U8
        \\    a = 255
        \\    b : U8
        \\    b = 16
        \\    a % b
        \\}
        ,
        .expected = .{ .u8_val = 15 },
        .skip = SKIP_ALL,
    },
    .{
        .name = "U8: rem_by: 128 % 7",
        .source =
        \\{
        \\    a : U8
        \\    a = 128
        \\    b : U8
        \\    b = 7
        \\    a % b
        \\}
        ,
        .expected = .{ .u8_val = 2 },
        .skip = SKIP_ALL,
    },

    // U16: plus
    .{
        .name = "U16: plus: 40000 + 20000",
        .source =
        \\{
        \\    a : U16
        \\    a = 40000
        \\    b : U16
        \\    b = 20000
        \\    a + b
        \\}
        ,
        .expected = .{ .u16_val = 60000 },
        .skip = SKIP_ALL,
    },
    .{
        .name = "U16: plus: 65535 + 0",
        .source =
        \\{
        \\    a : U16
        \\    a = 65535
        \\    b : U16
        \\    b = 0
        \\    a + b
        \\}
        ,
        .expected = .{ .u16_val = 65535 },
        .skip = SKIP_ALL,
    },
    .{
        .name = "U16: plus: 32768 + 32767",
        .source =
        \\{
        \\    a : U16
        \\    a = 32768
        \\    b : U16
        \\    b = 32767
        \\    a + b
        \\}
        ,
        .expected = .{ .u16_val = 65535 },
        .skip = SKIP_ALL,
    },

    // U16: minus
    .{
        .name = "U16: minus: 50000 - 10000",
        .source =
        \\{
        \\    a : U16
        \\    a = 50000
        \\    b : U16
        \\    b = 10000
        \\    a - b
        \\}
        ,
        .expected = .{ .u16_val = 40000 },
        .skip = SKIP_ALL,
    },
    // TODO: hangs on x86_64-linux CI (infinite loop in interpreter)
    .{
        .name = "U16: minus: 65535 - 30000",
        .source =
        \\{
        \\    a : U16
        \\    a = 65535
        \\    b : U16
        \\    b = 30000
        \\    a - b
        \\}
        ,
        .expected = .{ .u16_val = 35535 },
        .skip = SKIP_ALL,
    },
    .{
        .name = "U16: minus: 50000 - 50000",
        .source =
        \\{
        \\    a : U16
        \\    a = 50000
        \\    b : U16
        \\    b = 50000
        \\    a - b
        \\}
        ,
        .expected = .{ .u16_val = 0 },
        .skip = SKIP_ALL,
    },

    // U16: times
    .{
        .name = "U16: times: 256 * 255",
        .source =
        \\{
        \\    a : U16
        \\    a = 256
        \\    b : U16
        \\    b = 255
        \\    a * b
        \\}
        ,
        .expected = .{ .u16_val = 65280 },
        .skip = SKIP_ALL,
    },
    .{
        .name = "U16: times: 32768 * 1",
        .source =
        \\{
        \\    a : U16
        \\    a = 32768
        \\    b : U16
        \\    b = 1
        \\    a * b
        \\}
        ,
        .expected = .{ .u16_val = 32768 },
        .skip = SKIP_ALL,
    },
    .{
        .name = "U16: times: 255 * 256",
        .source =
        \\{
        \\    a : U16
        \\    a = 255
        \\    b : U16
        \\    b = 256
        \\    a * b
        \\}
        ,
        .expected = .{ .u16_val = 65280 },
        .skip = SKIP_ALL,
    },

    // U16: div_by
    .{
        .name = "U16: div_by: 60000 // 3",
        .source =
        \\{
        \\    a : U16
        \\    a = 60000
        \\    b : U16
        \\    b = 3
        \\    a // b
        \\}
        ,
        .expected = .{ .u16_val = 20000 },
        .skip = SKIP_ALL,
    },
    .{
        .name = "U16: div_by: 65535 // 257",
        .source =
        \\{
        \\    a : U16
        \\    a = 65535
        \\    b : U16
        \\    b = 257
        \\    a // b
        \\}
        ,
        .expected = .{ .u16_val = 255 },
        .skip = SKIP_ALL,
    },
    .{
        .name = "U16: div_by: 40000 // 128",
        .source =
        \\{
        \\    a : U16
        \\    a = 40000
        \\    b : U16
        \\    b = 128
        \\    a // b
        \\}
        ,
        .expected = .{ .u16_val = 312 },
        .skip = SKIP_ALL,
    },

    // U16: rem_by
    .{
        .name = "U16: rem_by: 50000 % 128",
        .source =
        \\{
        \\    a : U16
        \\    a = 50000
        \\    b : U16
        \\    b = 128
        \\    a % b
        \\}
        ,
        .expected = .{ .u16_val = 80 },
        .skip = SKIP_ALL,
    },
    .{
        .name = "U16: rem_by: 65535 % 256",
        .source =
        \\{
        \\    a : U16
        \\    a = 65535
        \\    b : U16
        \\    b = 256
        \\    a % b
        \\}
        ,
        .expected = .{ .u16_val = 255 },
        .skip = SKIP_ALL,
    },
    // TODO: hangs on x86_64-linux CI (infinite loop in interpreter)
    .{
        .name = "U16: rem_by: 40000 % 99",
        .source =
        \\{
        \\    a : U16
        \\    a = 40000
        \\    b : U16
        \\    b = 99
        \\    a % b
        \\}
        ,
        .expected = .{ .u16_val = 4 },
        .skip = SKIP_ALL,
    },

    // U32: plus
    .{
        .name = "U32: plus: 3000000000 + 1000000000",
        .source =
        \\{
        \\    a : U32
        \\    a = 3000000000
        \\    b : U32
        \\    b = 1000000000
        \\    a + b
        \\}
        ,
        .expected = .{ .u32_val = 4000000000 },
    },
    .{
        .name = "U32: plus: 2147483648 + 2147483647",
        .source =
        \\{
        \\    a : U32
        \\    a = 2147483648
        \\    b : U32
        \\    b = 2147483647
        \\    a + b
        \\}
        ,
        .expected = .{ .u32_val = 4294967295 },
    },
    .{
        .name = "U32: plus: 4294967295 + 0",
        .source =
        \\{
        \\    a : U32
        \\    a = 4294967295
        \\    b : U32
        \\    b = 0
        \\    a + b
        \\}
        ,
        .expected = .{ .u32_val = 4294967295 },
    },

    // U32: minus
    .{
        .name = "U32: minus: 3000000000 - 1000000000",
        .source =
        \\{
        \\    a : U32
        \\    a = 3000000000
        \\    b : U32
        \\    b = 1000000000
        \\    a - b
        \\}
        ,
        .expected = .{ .u32_val = 2000000000 },
    },
    .{
        .name = "U32: minus: 4294967295 - 2147483648",
        .source =
        \\{
        \\    a : U32
        \\    a = 4294967295
        \\    b : U32
        \\    b = 2147483648
        \\    a - b
        \\}
        ,
        .expected = .{ .u32_val = 2147483647 },
    },
    .{
        .name = "U32: minus: 3000000000 - 3000000000",
        .source =
        \\{
        \\    a : U32
        \\    a = 3000000000
        \\    b : U32
        \\    b = 3000000000
        \\    a - b
        \\}
        ,
        .expected = .{ .u32_val = 0 },
    },

    // U32: times
    .{
        .name = "U32: times: 65536 * 65535",
        .source =
        \\{
        \\    a : U32
        \\    a = 65536
        \\    b : U32
        \\    b = 65535
        \\    a * b
        \\}
        ,
        .expected = .{ .u32_val = 4294901760 },
    },
    .{
        .name = "U32: times: 2147483648 * 1",
        .source =
        \\{
        \\    a : U32
        \\    a = 2147483648
        \\    b : U32
        \\    b = 1
        \\    a * b
        \\}
        ,
        .expected = .{ .u32_val = 2147483648 },
    },
    .{
        .name = "U32: times: 1000000 * 4294",
        .source =
        \\{
        \\    a : U32
        \\    a = 1000000
        \\    b : U32
        \\    b = 4294
        \\    a * b
        \\}
        ,
        .expected = .{ .u32_val = 4294000000 },
    },

    // U32: div_by
    .{
        .name = "U32: div_by: 4000000000 // 1000",
        .source =
        \\{
        \\    a : U32
        \\    a = 4000000000
        \\    b : U32
        \\    b = 1000
        \\    a // b
        \\}
        ,
        .expected = .{ .u32_val = 4000000 },
    },
    .{
        .name = "U32: div_by: 4294967295 // 65536",
        .source =
        \\{
        \\    a : U32
        \\    a = 4294967295
        \\    b : U32
        \\    b = 65536
        \\    a // b
        \\}
        ,
        .expected = .{ .u32_val = 65535 },
    },
    .{
        .name = "U32: div_by: 3000000000 // 128",
        .source =
        \\{
        \\    a : U32
        \\    a = 3000000000
        \\    b : U32
        \\    b = 128
        \\    a // b
        \\}
        ,
        .expected = .{ .u32_val = 23437500 },
    },

    // U32: rem_by
    .{
        .name = "U32: rem_by: 3000000000 % 128",
        .source =
        \\{
        \\    a : U32
        \\    a = 3000000000
        \\    b : U32
        \\    b = 128
        \\    a % b
        \\}
        ,
        .expected = .{ .u32_val = 0 },
    },
    .{
        .name = "U32: rem_by: 4294967295 % 65536",
        .source =
        \\{
        \\    a : U32
        \\    a = 4294967295
        \\    b : U32
        \\    b = 65536
        \\    a % b
        \\}
        ,
        .expected = .{ .u32_val = 65535 },
    },
    .{
        .name = "U32: rem_by: 2147483648 % 99",
        .source =
        \\{
        \\    a : U32
        \\    a = 2147483648
        \\    b : U32
        \\    b = 99
        \\    a % b
        \\}
        ,
        .expected = .{ .u32_val = 2 },
    },

    // U64: plus
    .{
        .name = "U64: plus: 10000000000000000000 + 5000000000000000000",
        .source =
        \\{
        \\    a : U64
        \\    a = 10000000000000000000
        \\    b : U64
        \\    b = 5000000000000000000
        \\    a + b
        \\}
        ,
        .expected = .{ .u64_val = 15000000000000000000 },
    },
    .{
        .name = "U64: plus: 9223372036854775808 + 9223372036854775807",
        .source =
        \\{
        \\    a : U64
        \\    a = 9223372036854775808
        \\    b : U64
        \\    b = 9223372036854775807
        \\    a + b
        \\}
        ,
        .expected = .{ .u64_val = 18446744073709551615 },
    },
    .{
        .name = "U64: plus: 18446744073709551615 + 0",
        .source =
        \\{
        \\    a : U64
        \\    a = 18446744073709551615
        \\    b : U64
        \\    b = 0
        \\    a + b
        \\}
        ,
        .expected = .{ .u64_val = 18446744073709551615 },
    },

    // U64: minus
    .{
        .name = "U64: minus: 15000000000000000000 - 5000000000000000000",
        .source =
        \\{
        \\    a : U64
        \\    a = 15000000000000000000
        \\    b : U64
        \\    b = 5000000000000000000
        \\    a - b
        \\}
        ,
        .expected = .{ .u64_val = 10000000000000000000 },
    },
    .{
        .name = "U64: minus: 18446744073709551615 - 9223372036854775808",
        .source =
        \\{
        \\    a : U64
        \\    a = 18446744073709551615
        \\    b : U64
        \\    b = 9223372036854775808
        \\    a - b
        \\}
        ,
        .expected = .{ .u64_val = 9223372036854775807 },
    },
    .{
        .name = "U64: minus: 12000000000000000000 - 12000000000000000000",
        .source =
        \\{
        \\    a : U64
        \\    a = 12000000000000000000
        \\    b : U64
        \\    b = 12000000000000000000
        \\    a - b
        \\}
        ,
        .expected = .{ .u64_val = 0 },
    },

    // U64: times
    .{
        .name = "U64: times: 4294967296 * 4294967295",
        .source =
        \\{
        \\    a : U64
        \\    a = 4294967296
        \\    b : U64
        \\    b = 4294967295
        \\    a * b
        \\}
        ,
        .expected = .{ .u64_val = 18446744069414584320 },
    },
    .{
        .name = "U64: times: 9223372036854775808 * 1",
        .source =
        \\{
        \\    a : U64
        \\    a = 9223372036854775808
        \\    b : U64
        \\    b = 1
        \\    a * b
        \\}
        ,
        .expected = .{ .u64_val = 9223372036854775808 },
    },
    .{
        .name = "U64: times: 1000000000 * 10000000000",
        .source =
        \\{
        \\    a : U64
        \\    a = 1000000000
        \\    b : U64
        \\    b = 10000000000
        \\    a * b
        \\}
        ,
        .expected = .{ .u64_val = 10000000000000000000 },
    },

    // U64: div_by
    .{
        .name = "U64: div_by: 15000000000000000000 // 1000000",
        .source =
        \\{
        \\    a : U64
        \\    a = 15000000000000000000
        \\    b : U64
        \\    b = 1000000
        \\    a // b
        \\}
        ,
        .expected = .{ .u64_val = 15000000000000 },
    },
    .{
        .name = "U64: div_by: 18446744073709551615 // 4294967296",
        .source =
        \\{
        \\    a : U64
        \\    a = 18446744073709551615
        \\    b : U64
        \\    b = 4294967296
        \\    a // b
        \\}
        ,
        .expected = .{ .u64_val = 4294967295 },
    },
    .{
        .name = "U64: div_by: 10000000000000000000 // 256",
        .source =
        \\{
        \\    a : U64
        \\    a = 10000000000000000000
        \\    b : U64
        \\    b = 256
        \\    a // b
        \\}
        ,
        .expected = .{ .u64_val = 39062500000000000 },
    },

    // U64: rem_by
    .{
        .name = "U64: rem_by: 10000000000000000000 % 256",
        .source =
        \\{
        \\    a : U64
        \\    a = 10000000000000000000
        \\    b : U64
        \\    b = 256
        \\    a % b
        \\}
        ,
        .expected = .{ .u64_val = 0 },
    },
    .{
        .name = "U64: rem_by: 18446744073709551615 % 4294967296",
        .source =
        \\{
        \\    a : U64
        \\    a = 18446744073709551615
        \\    b : U64
        \\    b = 4294967296
        \\    a % b
        \\}
        ,
        .expected = .{ .u64_val = 4294967295 },
    },
    .{
        .name = "U64: rem_by: 9223372036854775808 % 99",
        .source =
        \\{
        \\    a : U64
        \\    a = 9223372036854775808
        \\    b : U64
        \\    b = 99
        \\    a % b
        \\}
        ,
        .expected = .{ .u64_val = 8 },
    },

    // U128: plus
    .{
        .name = "U128: plus: 100000000000000000000000000000 + 50000000000000000000000000000",
        .source =
        \\{
        \\    a : U128
        \\    a = 100000000000000000000000000000
        \\    b : U128
        \\    b = 50000000000000000000000000000
        \\    a + b
        \\}
        ,
        .expected = .{ .u128_val = 150000000000000000000000000000 },
    },
    .{
        .name = "U128: plus: 18446744073709551616 + 18446744073709551615",
        .source =
        \\{
        \\    a : U128
        \\    a = 18446744073709551616
        \\    b : U128
        \\    b = 18446744073709551615
        \\    a + b
        \\}
        ,
        .expected = .{ .u128_val = 36893488147419103231 },
    },
    .{
        .name = "U128: plus: max_i128 + 0",
        .source =
        \\{
        \\    a : U128
        \\    a = 170141183460469231731687303715884105727
        \\    b : U128
        \\    b = 0
        \\    a + b
        \\}
        ,
        .expected = .{ .u128_val = 170141183460469231731687303715884105727 },
    },

    // U128: minus
    .{
        .name = "U128: minus: 150000000000000000000000000000 - 50000000000000000000000000000",
        .source =
        \\{
        \\    a : U128
        \\    a = 150000000000000000000000000000
        \\    b : U128
        \\    b = 50000000000000000000000000000
        \\    a - b
        \\}
        ,
        .expected = .{ .u128_val = 100000000000000000000000000000 },
    },
    .{
        .name = "U128: minus: 36893488147419103231 - 18446744073709551616",
        .source =
        \\{
        \\    a : U128
        \\    a = 36893488147419103231
        \\    b : U128
        \\    b = 18446744073709551616
        \\    a - b
        \\}
        ,
        .expected = .{ .u128_val = 18446744073709551615 },
    },
    // TODO: hangs on x86_64-linux CI (interpreter infinite loop)
    .{
        .name = "U128: minus: 100000000000000000000000000000 - 100000000000000000000000000000",
        .source =
        \\{
        \\    a : U128
        \\    a = 100000000000000000000000000000
        \\    b : U128
        \\    b = 100000000000000000000000000000
        \\    a - b
        \\}
        ,
        .expected = .{ .u128_val = 0 },
        .skip = SKIP_ALL,
    },

    // U128: times
    .{
        .name = "U128: times: 13043817825332782212 * 13043817825332782212",
        .source =
        \\{
        \\    a : U128
        \\    a = 13043817825332782212
        \\    b : U128
        \\    b = 13043817825332782212
        \\    a * b
        \\}
        ,
        .expected = .{ .u128_val = 170141183460469231722567801800623612944 },
    },
    .{
        .name = "U128: times: 10000000000000000000 * 10000000000000000000",
        .source =
        \\{
        \\    a : U128
        \\    a = 10000000000000000000
        \\    b : U128
        \\    b = 10000000000000000000
        \\    a * b
        \\}
        ,
        .expected = .{ .u128_val = 100000000000000000000000000000000000000 },
    },
    .{
        .name = "U128: times: 1000000000000000000000 * 1000000",
        .source =
        \\{
        \\    a : U128
        \\    a = 1000000000000000000000
        \\    b : U128
        \\    b = 1000000
        \\    a * b
        \\}
        ,
        .expected = .{ .u128_val = 1000000000000000000000000000 },
    },

    // U128: div_by
    .{
        .name = "U128: div_by: 100000000000000000000000000000 // 10000000000000000",
        .source =
        \\{
        \\    a : U128
        \\    a = 100000000000000000000000000000
        \\    b : U128
        \\    b = 10000000000000000
        \\    a // b
        \\}
        ,
        .expected = .{ .u128_val = 10000000000000 },
    },
    .{
        .name = "U128: div_by: large square // factor",
        .source =
        \\{
        \\    a : U128
        \\    a = 170141183460469231722567801800623612944
        \\    b : U128
        \\    b = 13043817825332782212
        \\    a // b
        \\}
        ,
        .expected = .{ .u128_val = 13043817825332782212 },
    },
    .{
        .name = "U128: div_by: 36893488147419103231 // 256",
        .source =
        \\{
        \\    a : U128
        \\    a = 36893488147419103231
        \\    b : U128
        \\    b = 256
        \\    a // b
        \\}
        ,
        .expected = .{ .u128_val = 144115188075855871 },
    },

    // U128: rem_by
    .{
        .name = "U128: rem_by: 100000000000000000000000000000 % 99",
        .source =
        \\{
        \\    a : U128
        \\    a = 100000000000000000000000000000
        \\    b : U128
        \\    b = 99
        \\    a % b
        \\}
        ,
        .expected = .{ .u128_val = 10 },
    },
    .{
        .name = "U128: rem_by: large square % factor",
        .source =
        \\{
        \\    a : U128
        \\    a = 170141183460469231722567801800623612944
        \\    b : U128
        \\    b = 13043817825332782212
        \\    a % b
        \\}
        ,
        .expected = .{ .u128_val = 0 },
    },
    .{
        .name = "U128: rem_by: 36893488147419103231 % 256",
        .source =
        \\{
        \\    a : U128
        \\    a = 36893488147419103231
        \\    b : U128
        \\    b = 256
        \\    a % b
        \\}
        ,
        .expected = .{ .u128_val = 255 },
    },

    // I8: negate
    .{
        .name = "I8: negate: -(-127)",
        .source =
        \\{
        \\    a : I8
        \\    a = -127
        \\    -a
        \\}
        ,
        .expected = .{ .i8_val = 127 },
    },
    .{
        .name = "I8: negate: -(127)",
        .source =
        \\{
        \\    a : I8
        \\    a = 127
        \\    -a
        \\}
        ,
        .expected = .{ .i8_val = -127 },
    },
    .{
        .name = "I8: negate: -(-50)",
        .source =
        \\{
        \\    a : I8
        \\    a = -50
        \\    -a
        \\}
        ,
        .expected = .{ .i8_val = 50 },
    },

    // I8: plus
    .{
        .name = "I8: plus: -100 + -20",
        .source =
        \\{
        \\    a : I8
        \\    a = -100
        \\    b : I8
        \\    b = -20
        \\    a + b
        \\}
        ,
        .expected = .{ .i8_val = -120 },
    },
    .{
        .name = "I8: plus: -50 + 70",
        .source =
        \\{
        \\    a : I8
        \\    a = -50
        \\    b : I8
        \\    b = 70
        \\    a + b
        \\}
        ,
        .expected = .{ .i8_val = 20 },
    },
    .{
        .name = "I8: plus: 127 + 0",
        .source =
        \\{
        \\    a : I8
        \\    a = 127
        \\    b : I8
        \\    b = 0
        \\    a + b
        \\}
        ,
        .expected = .{ .i8_val = 127 },
    },

    // I8: minus
    .{
        .name = "I8: minus: -50 - 70",
        .source =
        \\{
        \\    a : I8
        \\    a = -50
        \\    b : I8
        \\    b = 70
        \\    a - b
        \\}
        ,
        .expected = .{ .i8_val = -120 },
    },
    .{
        .name = "I8: minus: 100 - -27",
        .source =
        \\{
        \\    a : I8
        \\    a = 100
        \\    b : I8
        \\    b = -27
        \\    a - b
        \\}
        ,
        .expected = .{ .i8_val = 127 },
    },
    .{
        .name = "I8: minus: -64 - -64",
        .source =
        \\{
        \\    a : I8
        \\    a = -64
        \\    b : I8
        \\    b = -64
        \\    a - b
        \\}
        ,
        .expected = .{ .i8_val = 0 },
    },

    // I8: times
    .{
        .name = "I8: times: -16 * 8",
        .source =
        \\{
        \\    a : I8
        \\    a = -16
        \\    b : I8
        \\    b = 8
        \\    a * b
        \\}
        ,
        .expected = .{ .i8_val = -128 },
    },
    .{
        .name = "I8: times: -10 * -10",
        .source =
        \\{
        \\    a : I8
        \\    a = -10
        \\    b : I8
        \\    b = -10
        \\    a * b
        \\}
        ,
        .expected = .{ .i8_val = 100 },
    },
    .{
        .name = "I8: times: 127 * 1",
        .source =
        \\{
        \\    a : I8
        \\    a = 127
        \\    b : I8
        \\    b = 1
        \\    a * b
        \\}
        ,
        .expected = .{ .i8_val = 127 },
    },

    // I8: div_by
    .{
        .name = "I8: div_by: -128 // 2",
        .source =
        \\{
        \\    a : I8
        \\    a = -128
        \\    b : I8
        \\    b = 2
        \\    a // b
        \\}
        ,
        .expected = .{ .i8_val = -64 },
    },
    .{
        .name = "I8: div_by: 127 // -1",
        .source =
        \\{
        \\    a : I8
        \\    a = 127
        \\    b : I8
        \\    b = -1
        \\    a // b
        \\}
        ,
        .expected = .{ .i8_val = -127 },
    },
    .{
        .name = "I8: div_by: -100 // -10",
        .source =
        \\{
        \\    a : I8
        \\    a = -100
        \\    b : I8
        \\    b = -10
        \\    a // b
        \\}
        ,
        .expected = .{ .i8_val = 10 },
    },

    // I8: rem_by
    .{
        .name = "I8: rem_by: -128 % 7",
        .source =
        \\{
        \\    a : I8
        \\    a = -128
        \\    b : I8
        \\    b = 7
        \\    a % b
        \\}
        ,
        .expected = .{ .i8_val = -2 },
    },
    .{
        .name = "I8: rem_by: 127 % -10",
        .source =
        \\{
        \\    a : I8
        \\    a = 127
        \\    b : I8
        \\    b = -10
        \\    a % b
        \\}
        ,
        .expected = .{ .i8_val = 7 },
    },
    .{
        .name = "I8: rem_by: -100 % -7",
        .source =
        \\{
        \\    a : I8
        \\    a = -100
        \\    b : I8
        \\    b = -7
        \\    a % b
        \\}
        ,
        .expected = .{ .i8_val = -2 },
    },

    // I16: negate
    .{
        .name = "I16: negate: -(-32767)",
        .source =
        \\{
        \\    a : I16
        \\    a = -32767
        \\    -a
        \\}
        ,
        .expected = .{ .i16_val = 32767 },
    },
    .{
        .name = "I16: negate: -(32767)",
        .source =
        \\{
        \\    a : I16
        \\    a = 32767
        \\    -a
        \\}
        ,
        .expected = .{ .i16_val = -32767 },
    },
    .{
        .name = "I16: negate: -(-10000)",
        .source =
        \\{
        \\    a : I16
        \\    a = -10000
        \\    -a
        \\}
        ,
        .expected = .{ .i16_val = 10000 },
    },

    // I16: plus
    .{
        .name = "I16: plus: -20000 + -10000",
        .source =
        \\{
        \\    a : I16
        \\    a = -20000
        \\    b : I16
        \\    b = -10000
        \\    a + b
        \\}
        ,
        .expected = .{ .i16_val = -30000 },
    },
    .{
        .name = "I16: plus: -32768 + 32767",
        .source =
        \\{
        \\    a : I16
        \\    a = -32768
        \\    b : I16
        \\    b = 32767
        \\    a + b
        \\}
        ,
        .expected = .{ .i16_val = -1 },
    },
    .{
        .name = "I16: plus: 32767 + 0",
        .source =
        \\{
        \\    a : I16
        \\    a = 32767
        \\    b : I16
        \\    b = 0
        \\    a + b
        \\}
        ,
        .expected = .{ .i16_val = 32767 },
    },

    // I16: minus
    .{
        .name = "I16: minus: -10000 - 20000",
        .source =
        \\{
        \\    a : I16
        \\    a = -10000
        \\    b : I16
        \\    b = 20000
        \\    a - b
        \\}
        ,
        .expected = .{ .i16_val = -30000 },
    },
    .{
        .name = "I16: minus: 30000 - -2767",
        .source =
        \\{
        \\    a : I16
        \\    a = 30000
        \\    b : I16
        \\    b = -2767
        \\    a - b
        \\}
        ,
        .expected = .{ .i16_val = 32767 },
    },
    .{
        .name = "I16: minus: -16384 - -16384",
        .source =
        \\{
        \\    a : I16
        \\    a = -16384
        \\    b : I16
        \\    b = -16384
        \\    a - b
        \\}
        ,
        .expected = .{ .i16_val = 0 },
    },

    // I16: times
    .{
        .name = "I16: times: -256 * 128",
        .source =
        \\{
        \\    a : I16
        \\    a = -256
        \\    b : I16
        \\    b = 128
        \\    a * b
        \\}
        ,
        .expected = .{ .i16_val = -32768 },
    },
    .{
        .name = "I16: times: -100 * -327",
        .source =
        \\{
        \\    a : I16
        \\    a = -100
        \\    b : I16
        \\    b = -327
        \\    a * b
        \\}
        ,
        .expected = .{ .i16_val = 32700 },
    },
    .{
        .name = "I16: times: 181 * 181",
        .source =
        \\{
        \\    a : I16
        \\    a = 181
        \\    b : I16
        \\    b = 181
        \\    a * b
        \\}
        ,
        .expected = .{ .i16_val = 32761 },
    },

    // I16: div_by
    .{
        .name = "I16: div_by: -32768 // 2",
        .source =
        \\{
        \\    a : I16
        \\    a = -32768
        \\    b : I16
        \\    b = 2
        \\    a // b
        \\}
        ,
        .expected = .{ .i16_val = -16384 },
    },
    .{
        .name = "I16: div_by: 32767 // -1",
        .source =
        \\{
        \\    a : I16
        \\    a = 32767
        \\    b : I16
        \\    b = -1
        \\    a // b
        \\}
        ,
        .expected = .{ .i16_val = -32767 },
    },
    .{
        .name = "I16: div_by: -30000 // -10",
        .source =
        \\{
        \\    a : I16
        \\    a = -30000
        \\    b : I16
        \\    b = -10
        \\    a // b
        \\}
        ,
        .expected = .{ .i16_val = 3000 },
    },

    // I16: rem_by
    .{
        .name = "I16: rem_by: -32768 % 99",
        .source =
        \\{
        \\    a : I16
        \\    a = -32768
        \\    b : I16
        \\    b = 99
        \\    a % b
        \\}
        ,
        .expected = .{ .i16_val = -98 },
    },
    .{
        .name = "I16: rem_by: 32767 % -100",
        .source =
        \\{
        \\    a : I16
        \\    a = 32767
        \\    b : I16
        \\    b = -100
        \\    a % b
        \\}
        ,
        .expected = .{ .i16_val = 67 },
    },
    .{
        .name = "I16: rem_by: -10000 % -128",
        .source =
        \\{
        \\    a : I16
        \\    a = -10000
        \\    b : I16
        \\    b = -128
        \\    a % b
        \\}
        ,
        .expected = .{ .i16_val = -16 },
    },

    // I32: negate
    .{
        .name = "I32: negate: -(-2147483647)",
        .source =
        \\{
        \\    a : I32
        \\    a = -2147483647
        \\    -a
        \\}
        ,
        .expected = .{ .i32_val = 2147483647 },
    },
    .{
        .name = "I32: negate: -(2147483647)",
        .source =
        \\{
        \\    a : I32
        \\    a = 2147483647
        \\    -a
        \\}
        ,
        .expected = .{ .i32_val = -2147483647 },
    },
    .{
        .name = "I32: negate: -(-1000000000)",
        .source =
        \\{
        \\    a : I32
        \\    a = -1000000000
        \\    -a
        \\}
        ,
        .expected = .{ .i32_val = 1000000000 },
    },

    // I32: plus
    .{
        .name = "I32: plus: -1000000000 + -500000000",
        .source =
        \\{
        \\    a : I32
        \\    a = -1000000000
        \\    b : I32
        \\    b = -500000000
        \\    a + b
        \\}
        ,
        .expected = .{ .i32_val = -1500000000 },
    },
    .{
        .name = "I32: plus: -2147483648 + 2147483647",
        .source =
        \\{
        \\    a : I32
        \\    a = -2147483648
        \\    b : I32
        \\    b = 2147483647
        \\    a + b
        \\}
        ,
        .expected = .{ .i32_val = -1 },
    },
    .{
        .name = "I32: plus: 2147483647 + 0",
        .source =
        \\{
        \\    a : I32
        \\    a = 2147483647
        \\    b : I32
        \\    b = 0
        \\    a + b
        \\}
        ,
        .expected = .{ .i32_val = 2147483647 },
    },

    // I32: minus
    .{
        .name = "I32: minus: -1000000000 - 500000000",
        .source =
        \\{
        \\    a : I32
        \\    a = -1000000000
        \\    b : I32
        \\    b = 500000000
        \\    a - b
        \\}
        ,
        .expected = .{ .i32_val = -1500000000 },
    },
    .{
        .name = "I32: minus: 2000000000 - -147483647",
        .source =
        \\{
        \\    a : I32
        \\    a = 2000000000
        \\    b : I32
        \\    b = -147483647
        \\    a - b
        \\}
        ,
        .expected = .{ .i32_val = 2147483647 },
    },
    .{
        .name = "I32: minus: -1073741824 - -1073741824",
        .source =
        \\{
        \\    a : I32
        \\    a = -1073741824
        \\    b : I32
        \\    b = -1073741824
        \\    a - b
        \\}
        ,
        .expected = .{ .i32_val = 0 },
    },

    // I32: times
    .{
        .name = "I32: times: -65536 * 32768",
        .source =
        \\{
        \\    a : I32
        \\    a = -65536
        \\    b : I32
        \\    b = 32768
        \\    a * b
        \\}
        ,
        .expected = .{ .i32_val = -2147483648 },
    },
    .{
        .name = "I32: times: -10000 * -214748",
        .source =
        \\{
        \\    a : I32
        \\    a = -10000
        \\    b : I32
        \\    b = -214748
        \\    a * b
        \\}
        ,
        .expected = .{ .i32_val = 2147480000 },
    },
    .{
        .name = "I32: times: 46340 * 46340",
        .source =
        \\{
        \\    a : I32
        \\    a = 46340
        \\    b : I32
        \\    b = 46340
        \\    a * b
        \\}
        ,
        .expected = .{ .i32_val = 2147395600 },
    },

    // I32: div_by
    .{
        .name = "I32: div_by: -2147483648 // 2",
        .source =
        \\{
        \\    a : I32
        \\    a = -2147483648
        \\    b : I32
        \\    b = 2
        \\    a // b
        \\}
        ,
        .expected = .{ .i32_val = -1073741824 },
    },
    .{
        .name = "I32: div_by: 2147483647 // -1",
        .source =
        \\{
        \\    a : I32
        \\    a = 2147483647
        \\    b : I32
        \\    b = -1
        \\    a // b
        \\}
        ,
        .expected = .{ .i32_val = -2147483647 },
    },
    .{
        .name = "I32: div_by: -1500000000 // -1000",
        .source =
        \\{
        \\    a : I32
        \\    a = -1500000000
        \\    b : I32
        \\    b = -1000
        \\    a // b
        \\}
        ,
        .expected = .{ .i32_val = 1500000 },
    },

    // I32: rem_by
    .{
        .name = "I32: rem_by: -2147483648 % 99",
        .source =
        \\{
        \\    a : I32
        \\    a = -2147483648
        \\    b : I32
        \\    b = 99
        \\    a % b
        \\}
        ,
        .expected = .{ .i32_val = -2 },
    },
    .{
        .name = "I32: rem_by: 2147483647 % -65536",
        .source =
        \\{
        \\    a : I32
        \\    a = 2147483647
        \\    b : I32
        \\    b = -65536
        \\    a % b
        \\}
        ,
        .expected = .{ .i32_val = 65535 },
    },
    .{
        .name = "I32: rem_by: -1000000000 % -32768",
        .source =
        \\{
        \\    a : I32
        \\    a = -1000000000
        \\    b : I32
        \\    b = -32768
        \\    a % b
        \\}
        ,
        .expected = .{ .i32_val = -18944 },
    },

    // I64: negate
    .{
        .name = "I64: negate: -(-9223372036854775807)",
        .source =
        \\{
        \\    a : I64
        \\    a = -9223372036854775807
        \\    -a
        \\}
        ,
        .expected = .{ .i64_val = 9223372036854775807 },
    },
    .{
        .name = "I64: negate: -(9223372036854775807)",
        .source =
        \\{
        \\    a : I64
        \\    a = 9223372036854775807
        \\    -a
        \\}
        ,
        .expected = .{ .i64_val = -9223372036854775807 },
    },
    .{
        .name = "I64: negate: -(-5000000000000)",
        .source =
        \\{
        \\    a : I64
        \\    a = -5000000000000
        \\    -a
        \\}
        ,
        .expected = .{ .i64_val = 5000000000000 },
    },

    // I64: plus
    .{
        .name = "I64: plus: -5000000000000 + -3000000000000",
        .source =
        \\{
        \\    a : I64
        \\    a = -5000000000000
        \\    b : I64
        \\    b = -3000000000000
        \\    a + b
        \\}
        ,
        .expected = .{ .i64_val = -8000000000000 },
    },
    .{
        .name = "I64: plus: -9223372036854775808 + 9223372036854775807",
        .source =
        \\{
        \\    a : I64
        \\    a = -9223372036854775808
        \\    b : I64
        \\    b = 9223372036854775807
        \\    a + b
        \\}
        ,
        .expected = .{ .i64_val = -1 },
    },
    .{
        .name = "I64: plus: 9223372036854775807 + 0",
        .source =
        \\{
        \\    a : I64
        \\    a = 9223372036854775807
        \\    b : I64
        \\    b = 0
        \\    a + b
        \\}
        ,
        .expected = .{ .i64_val = 9223372036854775807 },
    },

    // I64: minus
    .{
        .name = "I64: minus: -5000000000000 - 3000000000000",
        .source =
        \\{
        \\    a : I64
        \\    a = -5000000000000
        \\    b : I64
        \\    b = 3000000000000
        \\    a - b
        \\}
        ,
        .expected = .{ .i64_val = -8000000000000 },
    },
    .{
        .name = "I64: minus: 9000000000000000000 - -223372036854775807",
        .source =
        \\{
        \\    a : I64
        \\    a = 9000000000000000000
        \\    b : I64
        \\    b = -223372036854775807
        \\    a - b
        \\}
        ,
        .expected = .{ .i64_val = 9223372036854775807 },
    },
    .{
        .name = "I64: minus: -4611686018427387904 - -4611686018427387904",
        .source =
        \\{
        \\    a : I64
        \\    a = -4611686018427387904
        \\    b : I64
        \\    b = -4611686018427387904
        \\    a - b
        \\}
        ,
        .expected = .{ .i64_val = 0 },
    },

    // I64: times
    .{
        .name = "I64: times: -4294967296 * 2147483648",
        .source =
        \\{
        \\    a : I64
        \\    a = -4294967296
        \\    b : I64
        \\    b = 2147483648
        \\    a * b
        \\}
        ,
        .expected = .{ .i64_val = -9223372036854775808 },
    },
    .{
        .name = "I64: times: -1000000000 * -9223372",
        .source =
        \\{
        \\    a : I64
        \\    a = -1000000000
        \\    b : I64
        \\    b = -9223372
        \\    a * b
        \\}
        ,
        .expected = .{ .i64_val = 9223372000000000 },
    },
    .{
        .name = "I64: times: 3037000499 * 3037000499",
        .source =
        \\{
        \\    a : I64
        \\    a = 3037000499
        \\    b : I64
        \\    b = 3037000499
        \\    a * b
        \\}
        ,
        .expected = .{ .i64_val = 9223372030926249001 },
    },

    // I64: div_by
    .{
        .name = "I64: div_by: -9223372036854775808 // 2",
        .source =
        \\{
        \\    a : I64
        \\    a = -9223372036854775808
        \\    b : I64
        \\    b = 2
        \\    a // b
        \\}
        ,
        .expected = .{ .i64_val = -4611686018427387904 },
    },
    .{
        .name = "I64: div_by: 9223372036854775807 // -1",
        .source =
        \\{
        \\    a : I64
        \\    a = 9223372036854775807
        \\    b : I64
        \\    b = -1
        \\    a // b
        \\}
        ,
        .expected = .{ .i64_val = -9223372036854775807 },
    },
    .{
        .name = "I64: div_by: -8000000000000 // -1000000",
        .source =
        \\{
        \\    a : I64
        \\    a = -8000000000000
        \\    b : I64
        \\    b = -1000000
        \\    a // b
        \\}
        ,
        .expected = .{ .i64_val = 8000000 },
    },

    // I64: rem_by
    .{
        .name = "I64: rem_by: -9223372036854775808 % 99",
        .source =
        \\{
        \\    a : I64
        \\    a = -9223372036854775808
        \\    b : I64
        \\    b = 99
        \\    a % b
        \\}
        ,
        .expected = .{ .i64_val = -8 },
    },
    .{
        .name = "I64: rem_by: 9223372036854775807 % -4294967296",
        .source =
        \\{
        \\    a : I64
        \\    a = 9223372036854775807
        \\    b : I64
        \\    b = -4294967296
        \\    a % b
        \\}
        ,
        .expected = .{ .i64_val = 4294967295 },
    },
    .{
        .name = "I64: rem_by: -5000000000000 % -2147483648",
        .source =
        \\{
        \\    a : I64
        \\    a = -5000000000000
        \\    b : I64
        \\    b = -2147483648
        \\    a % b
        \\}
        ,
        .expected = .{ .i64_val = -658067456 },
    },

    // I128: negate
    .{
        .name = "I128: negate: -(-85070591730234615865843651857942052864)",
        .source =
        \\{
        \\    a : I128
        \\    a = -85070591730234615865843651857942052864
        \\    -a
        \\}
        ,
        .expected = .{ .i128_val = 85070591730234615865843651857942052864 },
    },
    .{
        .name = "I128: negate: -(170141183460469231731687303715884105727)",
        .source =
        \\{
        \\    a : I128
        \\    a = 170141183460469231731687303715884105727
        \\    -a
        \\}
        ,
        .expected = .{ .i128_val = -170141183460469231731687303715884105727 },
    },
    .{
        .name = "I128: negate: -(-100000000000000000000000)",
        .source =
        \\{
        \\    a : I128
        \\    a = -100000000000000000000000
        \\    -a
        \\}
        ,
        .expected = .{ .i128_val = 100000000000000000000000 },
    },

    // I128: plus
    .{
        .name = "I128: plus: -100000000000000000000000 + -50000000000000000000000",
        .source =
        \\{
        \\    a : I128
        \\    a = -100000000000000000000000
        \\    b : I128
        \\    b = -50000000000000000000000
        \\    a + b
        \\}
        ,
        .expected = .{ .i128_val = -150000000000000000000000 },
    },
    .{
        .name = "I128: plus: min + max",
        .source =
        \\{
        \\    a : I128
        \\    a = -170141183460469231731687303715884105728
        \\    b : I128
        \\    b = 170141183460469231731687303715884105727
        \\    a + b
        \\}
        ,
        .expected = .{ .i128_val = -1 },
    },
    .{
        .name = "I128: plus: max + 0",
        .source =
        \\{
        \\    a : I128
        \\    a = 170141183460469231731687303715884105727
        \\    b : I128
        \\    b = 0
        \\    a + b
        \\}
        ,
        .expected = .{ .i128_val = 170141183460469231731687303715884105727 },
    },

    // I128: minus
    .{
        .name = "I128: minus: -100000000000000000000000 - 50000000000000000000000",
        .source =
        \\{
        \\    a : I128
        \\    a = -100000000000000000000000
        \\    b : I128
        \\    b = 50000000000000000000000
        \\    a - b
        \\}
        ,
        .expected = .{ .i128_val = -150000000000000000000000 },
    },
    .{
        .name = "I128: minus: 85070591730234615865843651857942052863 - -1",
        .source =
        \\{
        \\    a : I128
        \\    a = 85070591730234615865843651857942052863
        \\    b : I128
        \\    b = -1
        \\    a - b
        \\}
        ,
        .expected = .{ .i128_val = 85070591730234615865843651857942052864 },
    },
    .{
        .name = "I128: minus: -85070591730234615865843651857942052864 - -85070591730234615865843651857942052864",
        .source =
        \\{
        \\    a : I128
        \\    a = -85070591730234615865843651857942052864
        \\    b : I128
        \\    b = -85070591730234615865843651857942052864
        \\    a - b
        \\}
        ,
        .expected = .{ .i128_val = 0 },
    },

    // I128: times
    .{
        .name = "I128: times: -18446744073709551616 * 9223372036854775808",
        .source =
        \\{
        \\    a : I128
        \\    a = -18446744073709551616
        \\    b : I128
        \\    b = 9223372036854775808
        \\    a * b
        \\}
        ,
        .expected = .{ .i128_val = -170141183460469231731687303715884105728 },
    },
    .{
        .name = "I128: times: -10000000000000000000 * -17014118346",
        .source =
        \\{
        \\    a : I128
        \\    a = -10000000000000000000
        \\    b : I128
        \\    b = -17014118346
        \\    a * b
        \\}
        ,
        .expected = .{ .i128_val = 170141183460000000000000000000 },
    },
    .{
        .name = "I128: times: 13043817825332782212 * 13043817825332782212",
        .source =
        \\{
        \\    a : I128
        \\    a = 13043817825332782212
        \\    b : I128
        \\    b = 13043817825332782212
        \\    a * b
        \\}
        ,
        .expected = .{ .i128_val = 170141183460469231722567801800623612944 },
    },

    // I128: div_by
    .{
        .name = "I128: div_by: min // 2",
        .source =
        \\{
        \\    a : I128
        \\    a = -170141183460469231731687303715884105728
        \\    b : I128
        \\    b = 2
        \\    a // b
        \\}
        ,
        .expected = .{ .i128_val = -85070591730234615865843651857942052864 },
    },
    .{
        .name = "I128: div_by: max // -1",
        .source =
        \\{
        \\    a : I128
        \\    a = 170141183460469231731687303715884105727
        \\    b : I128
        \\    b = -1
        \\    a // b
        \\}
        ,
        .expected = .{ .i128_val = -170141183460469231731687303715884105727 },
    },
    .{
        .name = "I128: div_by: -100000000000000000000000 // -10000000000",
        .source =
        \\{
        \\    a : I128
        \\    a = -100000000000000000000000
        \\    b : I128
        \\    b = -10000000000
        \\    a // b
        \\}
        ,
        .expected = .{ .i128_val = 10000000000000 },
    },

    // I128: rem_by
    .{
        .name = "I128: rem_by: min % 99",
        .source =
        \\{
        \\    a : I128
        \\    a = -170141183460469231731687303715884105728
        \\    b : I128
        \\    b = 99
        \\    a % b
        \\}
        ,
        .expected = .{ .i128_val = -29 },
    },
    .{
        .name = "I128: rem_by: max % -18446744073709551616",
        .source =
        \\{
        \\    a : I128
        \\    a = 170141183460469231731687303715884105727
        \\    b : I128
        \\    b = -18446744073709551616
        \\    a % b
        \\}
        ,
        .expected = .{ .i128_val = 18446744073709551615 },
    },
    .{
        .name = "I128: rem_by: -100000000000000000000000 % -9223372036854775808",
        .source =
        \\{
        \\    a : I128
        \\    a = -100000000000000000000000
        \\    b : I128
        \\    b = -9223372036854775808
        \\    a % b
        \\}
        ,
        .expected = .{ .i128_val = -200376420520689664 },
    },

    // F32: literal only
    .{ .name = "F32: literal only", .source = "3.14.F32", .expected = .{ .f32_val = 3.14 } },

    // F32: variable assignment
    .{
        .name = "F32: variable assignment",
        .source =
        \\{
        \\    a : F32
        \\    a = 3.14.F32
        \\    a
        \\}
        ,
        .expected = .{ .f32_val = 3.14 },
    },

    // F32: negate
    .{
        .name = "F32: negate",
        .source =
        \\{
        \\    a : F32
        \\    a = 3.14.F32
        \\    -a
        \\}
        ,
        .expected = .{ .f32_val = -3.14 },
    },

    // F32: plus
    .{
        .name = "F32: plus: 1.5 + 2.5",
        .source =
        \\{
        \\    a : F32
        \\    a = 1.5.F32
        \\    b : F32
        \\    b = 2.5.F32
        \\    a + b
        \\}
        ,
        .expected = .{ .f32_val = 4.0 },
    },
    .{
        .name = "F32: plus: 3.14159 + 2.71828",
        .source =
        \\{
        \\    a : F32
        \\    a = 3.14159.F32
        \\    b : F32
        \\    b = 2.71828.F32
        \\    a + b
        \\}
        ,
        .expected = .{ .f32_val = 5.85987 },
    },
    .{
        .name = "F32: plus: -10.5 + 10.5",
        .source =
        \\{
        \\    a : F32
        \\    a = -10.5.F32
        \\    b : F32
        \\    b = 10.5.F32
        \\    a + b
        \\}
        ,
        .expected = .{ .f32_val = 0.0 },
    },

    // F32: minus
    .{
        .name = "F32: minus: 10.0 - 3.5",
        .source =
        \\{
        \\    a : F32
        \\    a = 10.0.F32
        \\    b : F32
        \\    b = 3.5.F32
        \\    a - b
        \\}
        ,
        .expected = .{ .f32_val = 6.5 },
    },
    .{
        .name = "F32: minus: 2.5 - 5.0",
        .source =
        \\{
        \\    a : F32
        \\    a = 2.5.F32
        \\    b : F32
        \\    b = 5.0.F32
        \\    a - b
        \\}
        ,
        .expected = .{ .f32_val = -2.5 },
    },
    .{
        .name = "F32: minus: 100.0 - 100.0",
        .source =
        \\{
        \\    a : F32
        \\    a = 100.0.F32
        \\    b : F32
        \\    b = 100.0.F32
        \\    a - b
        \\}
        ,
        .expected = .{ .f32_val = 0.0 },
    },

    // F32: times
    .{
        .name = "F32: times: 2.5 * 4.0",
        .source =
        \\{
        \\    a : F32
        \\    a = 2.5.F32
        \\    b : F32
        \\    b = 4.0.F32
        \\    a * b
        \\}
        ,
        .expected = .{ .f32_val = 10.0 },
    },
    .{
        .name = "F32: times: -3.0 * 2.5",
        .source =
        \\{
        \\    a : F32
        \\    a = -3.0.F32
        \\    b : F32
        \\    b = 2.5.F32
        \\    a * b
        \\}
        ,
        .expected = .{ .f32_val = -7.5 },
    },
    .{
        .name = "F32: times: 0.5 * 0.5",
        .source =
        \\{
        \\    a : F32
        \\    a = 0.5.F32
        \\    b : F32
        \\    b = 0.5.F32
        \\    a * b
        \\}
        ,
        .expected = .{ .f32_val = 0.25 },
    },

    // F32: div_by
    .{
        .name = "F32: div_by: 10.0 / 2.0",
        .source =
        \\{
        \\    a : F32
        \\    a = 10.0.F32
        \\    b : F32
        \\    b = 2.0.F32
        \\    a / b
        \\}
        ,
        .expected = .{ .f32_val = 5.0 },
    },
    .{
        .name = "F32: div_by: 7.5 / 2.5",
        .source =
        \\{
        \\    a : F32
        \\    a = 7.5.F32
        \\    b : F32
        \\    b = 2.5.F32
        \\    a / b
        \\}
        ,
        .expected = .{ .f32_val = 3.0 },
    },
    .{
        .name = "F32: div_by: 1.0 / 3.0",
        .source =
        \\{
        \\    a : F32
        \\    a = 1.0.F32
        \\    b : F32
        \\    b = 3.0.F32
        \\    a / b
        \\}
        ,
        .expected = .{ .f32_val = 0.3333333 },
    },

    // F64: negate
    .{
        .name = "F64: negate: -(3.141592653589793)",
        .source =
        \\{
        \\    a : F64
        \\    a = 3.141592653589793.F64
        \\    -a
        \\}
        ,
        .expected = .{ .f64_val = -3.141592653589793 },
    },
    .{
        .name = "F64: negate: -(-2.718281828459045)",
        .source =
        \\{
        \\    a : F64
        \\    a = -2.718281828459045.F64
        \\    -a
        \\}
        ,
        .expected = .{ .f64_val = 2.718281828459045 },
    },
    .{
        .name = "F64: negate: -(0.0)",
        .source =
        \\{
        \\    a : F64
        \\    a = 0.0.F64
        \\    -a
        \\}
        ,
        .expected = .{ .f64_val = 0.0 },
    },

    // F64: plus
    .{
        .name = "F64: plus: 1.5 + 2.5",
        .source =
        \\{
        \\    a : F64
        \\    a = 1.5.F64
        \\    b : F64
        \\    b = 2.5.F64
        \\    a + b
        \\}
        ,
        .expected = .{ .f64_val = 4.0 },
    },
    .{
        .name = "F64: plus: pi + e",
        .source =
        \\{
        \\    a : F64
        \\    a = 3.141592653589793.F64
        \\    b : F64
        \\    b = 2.718281828459045.F64
        \\    a + b
        \\}
        ,
        .expected = .{ .f64_val = 5.859874482048838 },
    },
    .{
        .name = "F64: plus: -100.123456789 + 100.123456789",
        .source =
        \\{
        \\    a : F64
        \\    a = -100.123456789.F64
        \\    b : F64
        \\    b = 100.123456789.F64
        \\    a + b
        \\}
        ,
        .expected = .{ .f64_val = 0.0 },
    },

    // F64: minus
    .{
        .name = "F64: minus: 10.5 - 3.25",
        .source =
        \\{
        \\    a : F64
        \\    a = 10.5.F64
        \\    b : F64
        \\    b = 3.25.F64
        \\    a - b
        \\}
        ,
        .expected = .{ .f64_val = 7.25 },
    },
    .{
        .name = "F64: minus: 2.5 - 5.75",
        .source =
        \\{
        \\    a : F64
        \\    a = 2.5.F64
        \\    b : F64
        \\    b = 5.75.F64
        \\    a - b
        \\}
        ,
        .expected = .{ .f64_val = -3.25 },
    },
    .{
        .name = "F64: minus: 1000.0 - 1000.0",
        .source =
        \\{
        \\    a : F64
        \\    a = 1000.0.F64
        \\    b : F64
        \\    b = 1000.0.F64
        \\    a - b
        \\}
        ,
        .expected = .{ .f64_val = 0.0 },
    },

    // F64: times
    .{
        .name = "F64: times: 2.5 * 4.0",
        .source =
        \\{
        \\    a : F64
        \\    a = 2.5.F64
        \\    b : F64
        \\    b = 4.0.F64
        \\    a * b
        \\}
        ,
        .expected = .{ .f64_val = 10.0 },
    },
    .{
        .name = "F64: times: -3.5 * 2.0",
        .source =
        \\{
        \\    a : F64
        \\    a = -3.5.F64
        \\    b : F64
        \\    b = 2.0.F64
        \\    a * b
        \\}
        ,
        .expected = .{ .f64_val = -7.0 },
    },
    .{
        .name = "F64: times: sqrt2 * sqrt2",
        .source =
        \\{
        \\    a : F64
        \\    a = 1.414213562373095.F64
        \\    b : F64
        \\    b = 1.414213562373095.F64
        \\    a * b
        \\}
        ,
        .expected = .{ .f64_val = 2.0 },
    },

    // F64: div_by
    .{
        .name = "F64: div_by: 10.0 / 2.0",
        .source =
        \\{
        \\    a : F64
        \\    a = 10.0.F64
        \\    b : F64
        \\    b = 2.0.F64
        \\    a / b
        \\}
        ,
        .expected = .{ .f64_val = 5.0 },
    },
    .{
        .name = "F64: div_by: 22.0 / 7.0",
        .source =
        \\{
        \\    a : F64
        \\    a = 22.0.F64
        \\    b : F64
        \\    b = 7.0.F64
        \\    a / b
        \\}
        ,
        .expected = .{ .f64_val = 3.142857142857143 },
    },
    .{
        .name = "F64: div_by: 1.0 / 3.0",
        .source =
        \\{
        \\    a : F64
        \\    a = 1.0.F64
        \\    b : F64
        \\    b = 3.0.F64
        \\    a / b
        \\}
        ,
        .expected = .{ .f64_val = 0.3333333333333333 },
    },

    // Dec: negate
    .{
        .name = "Dec: negate: -(3.14)",
        .source =
        \\{
        \\    a : Dec
        \\    a = 3.14.Dec
        \\    -a
        \\}
        ,
        .expected = .{ .dec_val = -3140000000000000000 },
    },
    .{
        .name = "Dec: negate: -(-2.5)",
        .source =
        \\{
        \\    a : Dec
        \\    a = -2.5.Dec
        \\    -a
        \\}
        ,
        .expected = .{ .dec_val = 2500000000000000000 },
    },
    .{
        .name = "Dec: negate: -(0.0)",
        .source =
        \\{
        \\    a : Dec
        \\    a = 0.0.Dec
        \\    -a
        \\}
        ,
        .expected = .{ .dec_val = 0 },
    },

    // Dec: plus
    .{
        .name = "Dec: plus: 1.5 + 2.5",
        .source =
        \\{
        \\    a : Dec
        \\    a = 1.5.Dec
        \\    b : Dec
        \\    b = 2.5.Dec
        \\    a + b
        \\}
        ,
        .expected = .{ .dec_val = 4000000000000000000 },
    },
    .{
        .name = "Dec: plus: 3.14159 + 2.71828",
        .source =
        \\{
        \\    a : Dec
        \\    a = 3.14159.Dec
        \\    b : Dec
        \\    b = 2.71828.Dec
        \\    a + b
        \\}
        ,
        .expected = .{ .dec_val = 5859870000000000000 },
    },
    .{
        .name = "Dec: plus: -10.5 + 10.5",
        .source =
        \\{
        \\    a : Dec
        \\    a = -10.5.Dec
        \\    b : Dec
        \\    b = 10.5.Dec
        \\    a + b
        \\}
        ,
        .expected = .{ .dec_val = 0 },
    },

    // Dec: minus
    .{
        .name = "Dec: minus: 10.0 - 3.5",
        .source =
        \\{
        \\    a : Dec
        \\    a = 10.0.Dec
        \\    b : Dec
        \\    b = 3.5.Dec
        \\    a - b
        \\}
        ,
        .expected = .{ .dec_val = 6500000000000000000 },
    },
    .{
        .name = "Dec: minus: 2.5 - 5.0",
        .source =
        \\{
        \\    a : Dec
        \\    a = 2.5.Dec
        \\    b : Dec
        \\    b = 5.0.Dec
        \\    a - b
        \\}
        ,
        .expected = .{ .dec_val = -2500000000000000000 },
    },
    .{
        .name = "Dec: minus: 100.0 - 100.0",
        .source =
        \\{
        \\    a : Dec
        \\    a = 100.0.Dec
        \\    b : Dec
        \\    b = 100.0.Dec
        \\    a - b
        \\}
        ,
        .expected = .{ .dec_val = 0 },
    },

    // Dec: times
    .{
        .name = "Dec: times: 2.5 * 4.0",
        .source =
        \\{
        \\    a : Dec
        \\    a = 2.5.Dec
        \\    b : Dec
        \\    b = 4.0.Dec
        \\    a * b
        \\}
        ,
        .expected = .{ .dec_val = 10000000000000000000 },
    },
    .{
        .name = "Dec: times: -3.0 * 2.5",
        .source =
        \\{
        \\    a : Dec
        \\    a = -3.0.Dec
        \\    b : Dec
        \\    b = 2.5.Dec
        \\    a * b
        \\}
        ,
        .expected = .{ .dec_val = -7500000000000000000 },
    },
    .{
        .name = "Dec: times: 0.5 * 0.5",
        .source =
        \\{
        \\    a : Dec
        \\    a = 0.5.Dec
        \\    b : Dec
        \\    b = 0.5.Dec
        \\    a * b
        \\}
        ,
        .expected = .{ .dec_val = 250000000000000000 },
    },

    // Dec: div_by
    .{
        .name = "Dec: div_by: 10.0 / 2.0",
        .source =
        \\{
        \\    a : Dec
        \\    a = 10.0.Dec
        \\    b : Dec
        \\    b = 2.0.Dec
        \\    a / b
        \\}
        ,
        .expected = .{ .dec_val = 5000000000000000000 },
    },
    .{
        .name = "Dec: div_by: 7.5 / 2.5",
        .source =
        \\{
        \\    a : Dec
        \\    a = 7.5.Dec
        \\    b : Dec
        \\    b = 2.5.Dec
        \\    a / b
        \\}
        ,
        .expected = .{ .dec_val = 3000000000000000000 },
    },
    .{
        .name = "Dec: div_by: 1.0 / 3.0",
        .source =
        \\{
        \\    a : Dec
        \\    a = 1.0.Dec
        \\    b : Dec
        \\    b = 3.0.Dec
        \\    a / b
        \\}
        ,
        .expected = .{ .dec_val = 333333333333333333 },
    },

    // Dec: to_str
    .{
        .name = "Dec: to_str: 100.0",
        .source =
        \\{
        \\    a : Dec
        \\    a = 100.0.Dec
        \\    Dec.to_str(a)
        \\}
        ,
        .expected = .{ .str_val = "100.0" },
    },
    .{
        .name = "Dec: to_str: 123.45",
        .source =
        \\{
        \\    a : Dec
        \\    a = 123.45.Dec
        \\    Dec.to_str(a)
        \\}
        ,
        .expected = .{ .str_val = "123.45" },
    },
    .{
        .name = "Dec: to_str: -123.45",
        .source =
        \\{
        \\    a : Dec
        \\    a = -123.45.Dec
        \\    Dec.to_str(a)
        \\}
        ,
        .expected = .{ .str_val = "-123.45" },
    },
    .{
        .name = "Dec: to_str: 123.0",
        .source =
        \\{
        \\    a : Dec
        \\    a = 123.0.Dec
        \\    Dec.to_str(a)
        \\}
        ,
        .expected = .{ .str_val = "123.0" },
    },
    .{
        .name = "Dec: to_str: -123.0",
        .source =
        \\{
        \\    a : Dec
        \\    a = -123.0.Dec
        \\    Dec.to_str(a)
        \\}
        ,
        .expected = .{ .str_val = "-123.0" },
    },
    .{
        .name = "Dec: to_str: 0.45",
        .source =
        \\{
        \\    a : Dec
        \\    a = 0.45.Dec
        \\    Dec.to_str(a)
        \\}
        ,
        .expected = .{ .str_val = "0.45" },
    },
    .{
        .name = "Dec: to_str: -0.45",
        .source =
        \\{
        \\    a : Dec
        \\    a = -0.45.Dec
        \\    Dec.to_str(a)
        \\}
        ,
        .expected = .{ .str_val = "-0.45" },
    },
    .{
        .name = "Dec: to_str: 0.0",
        .source =
        \\{
        \\    a : Dec
        \\    a = 0.0.Dec
        \\    Dec.to_str(a)
        \\}
        ,
        .expected = .{ .str_val = "0.0" },
    },

    // Dec + Int: type mismatch
    .{ .name = "Dec + Int: plus - type mismatch", .source = "1.0.Dec + 2.I64", .expected = .{ .problem = {} } },
    .{ .name = "Dec + Int: minus - type mismatch", .source = "1.0.Dec - 2.I64", .expected = .{ .problem = {} } },
    .{ .name = "Dec + Int: times - type mismatch", .source = "1.0.Dec * 2.I64", .expected = .{ .problem = {} } },
    .{ .name = "Dec + Int: div_by - type mismatch", .source = "1.0.Dec / 2.I64", .expected = .{ .problem = {} } },

    // Int + Dec: type mismatch
    .{ .name = "Int + Dec: plus - type mismatch", .source = "1.I64 + 2.0.Dec", .expected = .{ .problem = {} } },
    .{ .name = "Int + Dec: minus - type mismatch", .source = "1.I64 - 2.0.Dec", .expected = .{ .problem = {} } },
    .{ .name = "Int + Dec: times - type mismatch", .source = "1.I64 * 2.0.Dec", .expected = .{ .problem = {} } },
    .{ .name = "Int + Dec: div_by - type mismatch", .source = "1.I64 / 2.0.Dec", .expected = .{ .problem = {} } },

    // --- from list_refcount_simple.zig ---
    .{
        .name = "list_refcount_simple: empty list pattern match",
        .source =
        \\match [] { [] => 42, _ => 0 }
        ,
        .expected = .{ .dec_val = 42 * RocDec.one_point_zero_i128 },
    },
    .{
        .name = "list_refcount_simple: single element list pattern match",
        .source =
        \\match [1] { [x] => x, _ => 0 }
        ,
        .expected = .{ .dec_val = 1 * RocDec.one_point_zero_i128 },
    },
    .{
        .name = "list_refcount_simple: multi-element list pattern match",
        .source =
        \\match [1, 2, 3] { [a, b, c] => a + b + c, _ => 0 }
        ,
        .expected = .{ .dec_val = 6 * RocDec.one_point_zero_i128 },
    },

    // --- from list_refcount_alias.zig ---
    .{
        .name = "list_refcount_alias: variable aliasing",
        .source =
        \\{
        \\    x = [1, 2, 3]
        \\    y = x
        \\    match y { [a, b, c] => a + b + c, _ => 0 }
        \\}
        ,
        .expected = .{ .dec_val = 6 * RocDec.one_point_zero_i128 },
    },
    .{
        .name = "list_refcount_alias: return original after aliasing",
        .source =
        \\{
        \\    x = [1, 2, 3]
        \\    _y = x
        \\    match x { [a, b, c] => a + b + c, _ => 0 }
        \\}
        ,
        .expected = .{ .dec_val = 6 * RocDec.one_point_zero_i128 },
    },
    .{
        .name = "list_refcount_alias: triple aliasing",
        .source =
        \\{
        \\    x = [1, 2]
        \\    y = x
        \\    z = y
        \\    match z { [a, b] => a + b, _ => 0 }
        \\}
        ,
        .expected = .{ .dec_val = 3 * RocDec.one_point_zero_i128 },
    },
    .{
        .name = "list_refcount_alias: mutable reassignment decrefs old list",
        .source =
        \\{
        \\    var $x = [1, 2]
        \\    $x = [3, 4]
        \\    match $x { [a, b] => a + b, _ => 0 }
        \\}
        ,
        .expected = .{ .dec_val = 7 * RocDec.one_point_zero_i128 },
    },
    .{
        .name = "list_refcount_alias: multiple independent lists",
        .source =
        \\{
        \\    x = [1, 2]
        \\    _y = [3, 4]
        \\    match x { [a, b] => a + b, _ => 0 }
        \\}
        ,
        .expected = .{ .dec_val = 3 * RocDec.one_point_zero_i128 },
    },
    .{
        .name = "list_refcount_alias: empty list aliasing",
        .source =
        \\{
        \\    x = []
        \\    y = x
        \\    match y { [] => 42, _ => 0 }
        \\}
        ,
        .expected = .{ .dec_val = 42 * RocDec.one_point_zero_i128 },
    },
    .{
        .name = "list_refcount_alias: alias then shadow",
        .source =
        \\{
        \\    var $x = [1, 2]
        \\    y = $x
        \\    $x = [3, 4]
        \\    match y { [a, b] => a + b, _ => 0 }
        \\}
        ,
        .expected = .{ .dec_val = 3 * RocDec.one_point_zero_i128 },
    },
    .{
        .name = "list_refcount_alias: both references used",
        .source =
        \\{
        \\    x = [1, 2]
        \\    y = x
        \\    a = match x { [first, ..] => first, _ => 0 }
        \\    b = match y { [first, ..] => first, _ => 0 }
        \\    a + b
        \\}
        ,
        .expected = .{ .dec_val = 2 * RocDec.one_point_zero_i128 },
    },

    // --- from list_refcount_basic.zig ---
    .{
        .name = "list_refcount_basic: various small list sizes: single element",
        .source =
        \\match [5] { [x] => x, _ => 0 }
        ,
        .expected = .{ .dec_val = 5 * RocDec.one_point_zero_i128 },
    },
    .{
        .name = "list_refcount_basic: two elements",
        .source =
        \\match [10, 20] { [a, b] => a + b, _ => 0 }
        ,
        .expected = .{ .dec_val = 30 * RocDec.one_point_zero_i128 },
    },
    .{
        .name = "list_refcount_basic: five elements",
        .source =
        \\match [1, 2, 3, 4, 5] { [a, b, c, d, e] => a + b + c + d + e, _ => 0 }
        ,
        .expected = .{ .dec_val = 15 * RocDec.one_point_zero_i128 },
    },
    .{
        .name = "list_refcount_basic: larger list with pattern",
        .source =
        \\match [1, 2, 3, 4, 5, 6, 7, 8, 9, 10] { [first, second, ..] => first + second, _ => 0 }
        ,
        .expected = .{ .dec_val = 3 * RocDec.one_point_zero_i128 },
    },
    .{
        .name = "list_refcount_basic: sequential independent lists",
        .source =
        \\{
        \\    a = [1]
        \\    _b = [2, 3]
        \\    _c = [4, 5, 6]
        \\    match a { [x] => x, _ => 0 }
        \\}
        ,
        .expected = .{ .dec_val = 1 * RocDec.one_point_zero_i128 },
    },
    .{
        .name = "list_refcount_basic: return middle list",
        .source =
        \\{
        \\    _a = [1]
        \\    b = [2, 3]
        \\    _c = [4, 5, 6]
        \\    match b { [x, y] => x + y, _ => 0 }
        \\}
        ,
        .expected = .{ .dec_val = 5 * RocDec.one_point_zero_i128 },
    },
    .{
        .name = "list_refcount_basic: return last list",
        .source =
        \\{
        \\    _a = [1]
        \\    _b = [2, 3]
        \\    c = [4, 5, 6]
        \\    match c { [x, y, z] => x + y + z, _ => 0 }
        \\}
        ,
        .expected = .{ .dec_val = 15 * RocDec.one_point_zero_i128 },
    },
    .{
        .name = "list_refcount_basic: mix of empty and non-empty",
        .source =
        \\{
        \\    _x = []
        \\    y = [1, 2]
        \\    _z = []
        \\    match y { [a, b] => a + b, _ => 0 }
        \\}
        ,
        .expected = .{ .dec_val = 3 * RocDec.one_point_zero_i128 },
    },
    .{
        .name = "list_refcount_basic: return empty from mix",
        .source =
        \\{
        \\    x = []
        \\    _y = [1, 2]
        \\    _z = []
        \\    match x { [] => 42, _ => 0 }
        \\}
        ,
        .expected = .{ .dec_val = 42 * RocDec.one_point_zero_i128 },
    },
    .{
        .name = "list_refcount_basic: nested blocks with lists",
        .source =
        \\{
        \\    outer = [1, 2, 3]
        \\    result = {
        \\        inner = outer
        \\        match inner { [a, b, c] => a + b + c, _ => 0 }
        \\    }
        \\    result
        \\}
        ,
        .expected = .{ .dec_val = 6 * RocDec.one_point_zero_i128 },
    },
    .{
        .name = "list_refcount_basic: list created and used in inner block",
        .source =
        \\{
        \\    result = {
        \\        lst = [10, 20, 30]
        \\        match lst { [a, b, c] => a + b + c, _ => 0 }
        \\    }
        \\    result
        \\}
        ,
        .expected = .{ .dec_val = 60 * RocDec.one_point_zero_i128 },
    },
    .{
        .name = "list_refcount_basic: multiple lists chained",
        .source =
        \\{
        \\    a = [1]
        \\    b = a
        \\    c = [2, 3]
        \\    d = c
        \\    x = match b { [v] => v, _ => 0 }
        \\    y = match d { [v1, v2] => v1 + v2, _ => 0 }
        \\    x + y
        \\}
        ,
        .expected = .{ .dec_val = 6 * RocDec.one_point_zero_i128 },
    },

    // --- from list_refcount_strings.zig ---
    .{
        .name = "list_refcount_strings: single string in list",
        .source =
        \\{
        \\    x = "hi"
        \\    lst = [x]
        \\    match lst { [s] => s, _ => "" }
        \\}
        ,
        .expected = .{ .str_val = "hi" },
    },
    .{
        .name = "list_refcount_strings: multiple strings in list",
        .source =
        \\{
        \\    x = "a"
        \\    y = "b"
        \\    lst = [x, y]
        \\    match lst { [first, ..] => first, _ => "" }
        \\}
        ,
        .expected = .{ .str_val = "a" },
    },
    .{
        .name = "list_refcount_strings: return second string",
        .source =
        \\{
        \\    x = "a"
        \\    y = "b"
        \\    lst = [x, y]
        \\    match lst { [_, second] => second, _ => "" }
        \\}
        ,
        .expected = .{ .str_val = "b" },
    },
    .{
        .name = "list_refcount_strings: same string multiple times",
        .source =
        \\{
        \\    x = "hi"
        \\    lst = [x, x, x]
        \\    match lst { [first, ..] => first, _ => "" }
        \\}
        ,
        .expected = .{ .str_val = "hi" },
    },
    .{
        .name = "list_refcount_strings: empty string in list",
        .source =
        \\{
        \\    x = ""
        \\    lst = [x]
        \\    match lst { [s] => s, _ => "fallback" }
        \\}
        ,
        .expected = .{ .str_val = "" },
    },
    .{
        .name = "list_refcount_strings: small vs large strings in list",
        .source =
        \\{
        \\    small = "hi"
        \\    large = "This is a very long string that will be heap allocated for sure"
        \\    lst = [small, large]
        \\    match lst { [first, ..] => first, _ => "" }
        \\}
        ,
        .expected = .{ .str_val = "hi" },
    },
    .{
        .name = "list_refcount_strings: return large string",
        .source =
        \\{
        \\    small = "hi"
        \\    large = "This is a very long string that will be heap allocated for sure"
        \\    lst = [small, large]
        \\    match lst { [_, second] => second, _ => "" }
        \\}
        ,
        .expected = .{ .str_val = "This is a very long string that will be heap allocated for sure" },
    },
    .{
        .name = "list_refcount_strings: list of string literals",
        .source =
        \\match ["a", "b", "c"] { [first, ..] => first, _ => "" }
        ,
        .expected = .{ .str_val = "a" },
    },
    .{
        .name = "list_refcount_strings: list of string literals return second",
        .source =
        \\match ["a", "b", "c"] { [_, second, ..] => second, _ => "" }
        ,
        .expected = .{ .str_val = "b" },
    },
    .{
        .name = "list_refcount_strings: empty list then string list",
        .source =
        \\{
        \\    _empty = []
        \\    strings = ["x", "y"]
        \\    match strings { [first, ..] => first, _ => "" }
        \\}
        ,
        .expected = .{ .str_val = "x" },
    },
    .{
        .name = "list_refcount_strings: string list aliased",
        .source =
        \\{
        \\    lst1 = ["a", "b"]
        \\    lst2 = lst1
        \\    match lst2 { [first, ..] => first, _ => "" }
        \\}
        ,
        .expected = .{ .str_val = "a" },
    },
    .{
        .name = "list_refcount_strings: string list aliased return from original",
        .source =
        \\{
        \\    lst1 = ["a", "b"]
        \\    _lst2 = lst1
        \\    match lst1 { [first, ..] => first, _ => "" }
        \\}
        ,
        .expected = .{ .str_val = "a" },
    },
    .{
        .name = "list_refcount_strings: string list reassigned",
        .source =
        \\{
        \\    var $lst = ["old1", "old2"]
        \\    $lst = ["new1", "new2"]
        \\    match $lst { [first, ..] => first, _ => "" }
        \\}
        ,
        .expected = .{ .str_val = "new1" },
    },
    .{
        .name = "list_refcount_strings: three string lists",
        .source =
        \\{
        \\    _a = ["a1", "a2"]
        \\    b = ["b1", "b2"]
        \\    _c = ["c1", "c2"]
        \\    match b { [first, ..] => first, _ => "" }
        \\}
        ,
        .expected = .{ .str_val = "b1" },
    },
    .{
        .name = "list_refcount_strings: extract string from nested match",
        .source =
        \\{
        \\    lst = ["x", "y", "z"]
        \\    match lst {
        \\        [_first, .. as rest] => match rest {
        \\            [second, ..] => second,
        \\            _ => ""
        \\        },
        \\        _ => ""
        \\    }
        \\}
        ,
        .expected = .{ .str_val = "y" },
    },

    // --- from list_refcount_containers.zig ---
    .{
        .name = "list_refcount_containers: single list in tuple",
        .source =
        \\{
        \\    x = [1, 2]
        \\    match x { [a, b] => a + b, _ => 0 }
        \\}
        ,
        .expected = .{ .dec_val = 3 * RocDec.one_point_zero_i128 },
    },
    .{
        .name = "list_refcount_containers: multiple lists in tuple",
        .source =
        \\{
        \\    x = [1, 2]
        \\    y = [3, 4]
        \\    t = (x, y)
        \\    match t { (first, _) => match first { [a, b] => a + b, _ => 0 } }
        \\}
        ,
        .expected = .{ .dec_val = 3 * RocDec.one_point_zero_i128 },
    },
    .{
        .name = "list_refcount_containers: same list twice in tuple",
        .source =
        \\{
        \\    x = [1, 2]
        \\    t = (x, x)
        \\    match t { (first, _) => match first { [a, b] => a + b, _ => 0 } }
        \\}
        ,
        .expected = .{ .dec_val = 3 * RocDec.one_point_zero_i128 },
    },
    .{
        .name = "list_refcount_containers: tuple with string list",
        .source =
        \\{
        \\    x = ["a", "b"]
        \\    t = (x, 42)
        \\    match t { (lst, _) => match lst { [first, ..] => first, _ => "" } }
        \\}
        ,
        .expected = .{ .str_val = "a" },
    },
    .{
        .name = "list_refcount_containers: single field record with list",
        .source =
        \\{
        \\    lst = [1, 2, 3]
        \\    r = {items: lst}
        \\    match r.items { [a, b, c] => a + b + c, _ => 0 }
        \\}
        ,
        .expected = .{ .dec_val = 6 * RocDec.one_point_zero_i128 },
    },
    .{
        .name = "list_refcount_containers: multiple fields with lists",
        .source =
        \\{
        \\    x = [1, 2]
        \\    y = [3, 4]
        \\    r = {first: x, second: y}
        \\    match r.first { [a, b] => a + b, _ => 0 }
        \\}
        ,
        .expected = .{ .dec_val = 3 * RocDec.one_point_zero_i128 },
    },
    .{
        .name = "list_refcount_containers: same list in multiple fields",
        .source =
        \\{
        \\    lst = [10, 20]
        \\    r = {a: lst, b: lst}
        \\    match r.a { [x, y] => x + y, _ => 0 }
        \\}
        ,
        .expected = .{ .dec_val = 30 * RocDec.one_point_zero_i128 },
    },
    .{
        .name = "list_refcount_containers: nested record with list",
        .source =
        \\{
        \\    lst = [5, 6]
        \\    inner = {data: lst}
        \\    outer = {nested: inner}
        \\    match outer.nested.data { [a, b] => a + b, _ => 0 }
        \\}
        ,
        .expected = .{ .dec_val = 11 * RocDec.one_point_zero_i128 },
    },
    .{
        .name = "list_refcount_containers: record with string list",
        .source =
        \\{
        \\    lst = ["hello", "world"]
        \\    r = {items: lst}
        \\    match r.items { [first, ..] => first, _ => "" }
        \\}
        ,
        .expected = .{ .str_val = "hello" },
    },
    .{
        .name = "list_refcount_containers: record with mixed types",
        .source =
        \\{
        \\    lst = [1, 2, 3]
        \\    r = {count: 42, items: lst}
        \\    r.count
        \\}
        ,
        .expected = .{ .dec_val = 42 * RocDec.one_point_zero_i128 },
    },
    .{
        .name = "list_refcount_containers: tag with list payload",
        .source =
        \\match Some([1, 2]) { Some(lst) => match lst { [a, b] => a + b, _ => 0 }, None => 0 }
        ,
        .expected = .{ .dec_val = 3 * RocDec.one_point_zero_i128 },
    },
    .{
        .name = "list_refcount_containers: tag with multiple list payloads",
        .source =
        \\{
        \\    x = [1, 2]
        \\    y = [3, 4]
        \\    tag = Pair(x, y)
        \\    match tag { Pair(first, _) => match first { [a, b] => a + b, _ => 0 }, _ => 0 }
        \\}
        ,
        .expected = .{ .dec_val = 3 * RocDec.one_point_zero_i128 },
    },
    .{
        .name = "list_refcount_containers: tag with string list payload",
        .source =
        \\match Some(["tag", "value"]) { Some(lst) => match lst { [first, ..] => first, _ => "" }, None => "" }
        ,
        .expected = .{ .str_val = "tag" },
    },
    .{
        .name = "list_refcount_containers: Ok/Err with lists",
        .source =
        \\match Ok([1, 2, 3]) { Ok(lst) => match lst { [a, b, c] => a + b + c, _ => 0 }, Err(_) => 0 }
        ,
        .expected = .{ .dec_val = 6 * RocDec.one_point_zero_i128 },
    },
    .{
        .name = "list_refcount_containers: tuple of records with lists",
        .source =
        \\{
        \\    lst1 = [1, 2]
        \\    lst2 = [3, 4]
        \\    r1 = {items: lst1}
        \\    r2 = {items: lst2}
        \\    t = (r1, r2)
        \\    match t { (first, _) => match first.items { [a, b] => a + b, _ => 0 } }
        \\}
        ,
        .expected = .{ .dec_val = 3 * RocDec.one_point_zero_i128 },
    },
    .{
        .name = "list_refcount_containers: record of tuples with lists",
        .source =
        \\{
        \\    lst = [5, 6]
        \\    t = (lst, 99)
        \\    r = {data: t}
        \\    match r.data { (items, _) => match items { [a, b] => a + b, _ => 0 } }
        \\}
        ,
        .expected = .{ .dec_val = 11 * RocDec.one_point_zero_i128 },
    },
    .{
        .name = "list_refcount_containers: tag with record containing list",
        .source =
        \\{
        \\    lst = [7, 8]
        \\    r = {items: lst}
        \\    tag = Some(r)
        \\    match tag { Some(rec) => match rec.items { [a, b] => a + b, _ => 0 }, None => 0 }
        \\}
        ,
        .expected = .{ .dec_val = 15 * RocDec.one_point_zero_i128 },
    },
    .{
        .name = "list_refcount_containers: empty list in record",
        .source =
        \\{
        \\    empty = []
        \\    r = {lst: empty}
        \\    match r.lst { [] => 42, _ => 0 }
        \\}
        ,
        .expected = .{ .dec_val = 42 * RocDec.one_point_zero_i128 },
    },

    // --- from list_refcount_conditional.zig ---
    .{
        .name = "list_refcount_conditional: simple if-else with lists",
        .source =
        \\{
        \\    x = [1, 2]
        \\    result = if True {x} else {[3, 4]}
        \\    match result { [a, b] => a + b, _ => 0 }
        \\}
        ,
        .expected = .{ .dec_val = 3 * RocDec.one_point_zero_i128 },
    },
    .{
        .name = "list_refcount_conditional: return else branch",
        .source =
        \\{
        \\    x = [1, 2]
        \\    result = if False {x} else {[3, 4]}
        \\    match result { [a, b] => a + b, _ => 0 }
        \\}
        ,
        .expected = .{ .dec_val = 7 * RocDec.one_point_zero_i128 },
    },
    .{
        .name = "list_refcount_conditional: same list in both branches",
        .source =
        \\{
        \\    x = [1, 2]
        \\    result = if True {x} else {x}
        \\    match result { [a, b] => a + b, _ => 0 }
        \\}
        ,
        .expected = .{ .dec_val = 3 * RocDec.one_point_zero_i128 },
    },
    .{
        .name = "list_refcount_conditional: unused branch decreffed",
        .source =
        \\{
        \\    x = [1, 2]
        \\    y = [3, 4]
        \\    result = if True {x} else {y}
        \\    match result { [a, b] => a + b, _ => 0 }
        \\}
        ,
        .expected = .{ .dec_val = 3 * RocDec.one_point_zero_i128 },
    },
    .{
        .name = "list_refcount_conditional: nested conditionals",
        .source =
        \\{
        \\    x = [1]
        \\    result = if True {if False {x} else {[2]}} else {[3]}
        \\    match result { [a] => a, _ => 0 }
        \\}
        ,
        .expected = .{ .dec_val = 2 * RocDec.one_point_zero_i128 },
    },
    .{
        .name = "list_refcount_conditional: string lists in conditionals",
        .source =
        \\{
        \\    x = ["a", "b"]
        \\    result = if True {x} else {["c"]}
        \\    match result { [first, ..] => first, _ => "" }
        \\}
        ,
        .expected = .{ .str_val = "a" },
    },
    .{
        .name = "list_refcount_conditional: inline list literals",
        .source =
        \\{
        \\    result = if True {[10, 20]} else {[30, 40]}
        \\    match result { [a, b] => a + b, _ => 0 }
        \\}
        ,
        .expected = .{ .dec_val = 30 * RocDec.one_point_zero_i128 },
    },
    .{
        .name = "list_refcount_conditional: empty list in branch",
        .source =
        \\{
        \\    result = if True {[]} else {[1, 2]}
        \\    match result { [] => 42, _ => 0 }
        \\}
        ,
        .expected = .{ .dec_val = 42 * RocDec.one_point_zero_i128 },
    },

    // --- from list_refcount_function.zig ---
    .{
        .name = "list_refcount_function: pass list to identity function",
        .source =
        \\{
        \\    id = |lst| lst
        \\    x = [1, 2]
        \\    result = id(x)
        \\    match result { [a, b] => a + b, _ => 0 }
        \\}
        ,
        .expected = .{ .dec_val = 3 * RocDec.one_point_zero_i128 },
    },
    .{
        .name = "list_refcount_function: list returned from function",
        .source =
        \\{
        \\    f = |_| [1, 2]
        \\    result = f(0)
        \\    match result { [a, b] => a + b, _ => 0 }
        \\}
        ,
        .expected = .{ .dec_val = 3 * RocDec.one_point_zero_i128 },
    },
    .{
        .name = "list_refcount_function: closure captures list",
        .source =
        \\{
        \\    x = [1, 2]
        \\    f = |_| x
        \\    result = f(0)
        \\    match result { [a, b] => a + b, _ => 0 }
        \\}
        ,
        .expected = .{ .dec_val = 3 * RocDec.one_point_zero_i128 },
    },
    .{
        .name = "list_refcount_function: function called multiple times",
        .source =
        \\{
        \\    f = |lst| lst
        \\    x = [1, 2]
        \\    a = f(x)
        \\    _b = f(x)
        \\    match a { [first, ..] => first, _ => 0 }
        \\}
        ,
        .expected = .{ .dec_val = 1 * RocDec.one_point_zero_i128 },
    },
    .{
        .name = "list_refcount_function: string list through function",
        .source =
        \\{
        \\    f = |lst| lst
        \\    x = ["a", "b"]
        \\    result = f(x)
        \\    match result { [first, ..] => first, _ => "" }
        \\}
        ,
        .expected = .{ .str_val = "a" },
    },
    .{
        .name = "list_refcount_function: function extracts from list",
        .source =
        \\{
        \\    x = [10, 20, 30]
        \\    match x { [first, ..] => first, _ => 0 }
        \\}
        ,
        .expected = .{ .dec_val = 10 * RocDec.one_point_zero_i128 },
    },
    .{
        .name = "list_refcount_function: closure captures string list",
        .source =
        \\{
        \\    x = ["captured", "list"]
        \\    f = |_| x
        \\    result = f(0)
        \\    match result { [first, ..] => first, _ => "" }
        \\}
        ,
        .expected = .{ .str_val = "captured" },
    },
    .{
        .name = "list_refcount_function: nested function calls with lists",
        .source =
        \\{
        \\    x = [5, 10]
        \\    match x { [first, ..] => first + first, _ => 0 }
        \\}
        ,
        .expected = .{ .dec_val = 10 * RocDec.one_point_zero_i128 },
    },
    .{
        .name = "list_refcount_function: same list twice in tuple returned from function",
        .source =
        \\{
        \\    make_pair = |lst| (lst, lst)
        \\    x = [1, 2]
        \\    t = make_pair(x)
        \\    match t { (first, _) => match first { [a, b] => a + b, _ => 0 } }
        \\}
        ,
        .expected = .{ .dec_val = 3 * RocDec.one_point_zero_i128 },
    },
    .{
        .name = "list_refcount_function: same list twice passed to function",
        .source =
        \\{
        \\    add_lens = |a, b|
        \\        match a {
        \\            [first, ..] => match b { [second, ..] => first + second, _ => 0 },
        \\            _ => 0
        \\        }
        \\    x = [1, 2]
        \\    add_lens(x, x)
        \\}
        ,
        .expected = .{ .dec_val = 2 * RocDec.one_point_zero_i128 },
    },

    // --- from list_refcount_pattern.zig ---
    .{
        .name = "list_refcount_pattern: destructure list from record",
        .source =
        \\{
        \\    r = {lst: [1, 2]}
        \\    match r { {lst} => match lst { [a, b] => a + b, _ => 0 } }
        \\}
        ,
        .expected = .{ .dec_val = 3 * RocDec.one_point_zero_i128 },
    },
    .{
        .name = "list_refcount_pattern: wildcard discards list",
        .source =
        \\{
        \\    pair = {a: [1, 2], b: [3, 4]}
        \\    match pair { {a, b: _} => match a { [x, y] => x + y, _ => 0 } }
        \\}
        ,
        .expected = .{ .dec_val = 3 * RocDec.one_point_zero_i128 },
    },
    .{
        .name = "list_refcount_pattern: list rest pattern",
        .source =
        \\match [1, 2, 3, 4] { [first, .. as rest] => match rest { [second, ..] => first + second, _ => 0 }, _ => 0 }
        ,
        .expected = .{ .dec_val = 3 * RocDec.one_point_zero_i128 },
    },
    .{
        .name = "list_refcount_pattern: string list rest pattern",
        .source =
        \\match ["a", "b", "c"] { [_first, .. as rest] => match rest { [second, ..] => second, _ => "" }, _ => "" }
        ,
        .expected = .{ .str_val = "b" },
    },
    .{
        .name = "list_refcount_pattern: nested list patterns",
        .source =
        \\{
        \\    data = {values: [10, 20, 30]}
        \\    match data { {values} => match values { [a, b, c] => a + b + c, _ => 0 } }
        \\}
        ,
        .expected = .{ .dec_val = 60 * RocDec.one_point_zero_i128 },
    },
    .{
        .name = "list_refcount_pattern: tag with list extracted",
        .source =
        \\match Some([5, 10]) { Some(lst) => match lst { [a, b] => a + b, _ => 0 }, None => 0 }
        ,
        .expected = .{ .dec_val = 15 * RocDec.one_point_zero_i128 },
    },
    .{
        .name = "list_refcount_pattern: empty list pattern",
        .source =
        \\match {lst: []} { {lst} => match lst { [] => 42, _ => 0 } }
        ,
        .expected = .{ .dec_val = 42 * RocDec.one_point_zero_i128 },
    },

    // --- from list_refcount_nested.zig ---
    .{
        .name = "list_refcount_nested: simple nested list",
        .source =
        \\{
        \\    inner = [1, 2]
        \\    outer = [inner]
        \\    match outer { [lst] => match lst { [a, b] => a + b, _ => 0 }, _ => 0 }
        \\}
        ,
        .expected = .{ .dec_val = 3 * RocDec.one_point_zero_i128 },
    },
    .{
        .name = "list_refcount_nested: multiple inner lists",
        .source =
        \\{
        \\    a = [1, 2]
        \\    b = [3, 4]
        \\    outer = [a, b]
        \\    match outer { [first, ..] => match first { [x, y] => x + y, _ => 0 }, _ => 0 }
        \\}
        ,
        .expected = .{ .dec_val = 3 * RocDec.one_point_zero_i128 },
    },
    .{
        .name = "list_refcount_nested: same inner list multiple times",
        .source =
        \\{
        \\    inner = [1, 2]
        \\    outer = [inner, inner, inner]
        \\    match outer { [first, ..] => match first { [a, b] => a + b, _ => 0 }, _ => 0 }
        \\}
        ,
        .expected = .{ .dec_val = 3 * RocDec.one_point_zero_i128 },
    },
    .{
        .name = "list_refcount_nested: two levels inline",
        .source =
        \\match [[1, 2], [3, 4]] { [first, ..] => match first { [a, b] => a + b, _ => 0 }, _ => 0 }
        ,
        .expected = .{ .dec_val = 3 * RocDec.one_point_zero_i128 },
    },
    .{
        .name = "list_refcount_nested: three levels",
        .source =
        \\{
        \\    a = [1]
        \\    b = [a]
        \\    c = [b]
        \\    match c { [lst] => match lst { [lst2] => match lst2 { [x] => x, _ => 0 }, _ => 0 }, _ => 0 }
        \\}
        ,
        .expected = .{ .dec_val = 1 * RocDec.one_point_zero_i128 },
    },
    .{
        .name = "list_refcount_nested: empty inner list",
        .source =
        \\{
        \\    inner = []
        \\    outer = [inner]
        \\    match outer { [lst] => match lst { [] => 42, _ => 0 }, _ => 0 }
        \\}
        ,
        .expected = .{ .dec_val = 42 * RocDec.one_point_zero_i128 },
    },
    .{
        .name = "list_refcount_nested: list of string lists",
        .source =
        \\{
        \\    a = ["x", "y"]
        \\    b = ["z"]
        \\    outer = [a, b]
        \\    match outer { [first, ..] => match first { [s, ..] => s, _ => "" }, _ => "" }
        \\}
        ,
        .expected = .{ .str_val = "x" },
    },
    .{
        .name = "list_refcount_nested: inline string lists",
        .source =
        \\match [["a", "b"], ["c"]] { [first, ..] => match first { [s, ..] => s, _ => "" }, _ => "" }
        ,
        .expected = .{ .str_val = "a" },
    },
    .{
        .name = "list_refcount_nested: nested then aliased",
        .source =
        \\{
        \\    inner = [1, 2]
        \\    outer = [inner]
        \\    outer2 = outer
        \\    match outer2 { [lst] => match lst { [a, b] => a + b, _ => 0 }, _ => 0 }
        \\}
        ,
        .expected = .{ .dec_val = 3 * RocDec.one_point_zero_i128 },
    },
    .{
        .name = "list_refcount_nested: access second inner list",
        .source =
        \\{
        \\    a = [1, 2]
        \\    b = [3, 4]
        \\    outer = [a, b]
        \\    match outer { [_, second] => match second { [x, y] => x + y, _ => 0 }, _ => 0 }
        \\}
        ,
        .expected = .{ .dec_val = 7 * RocDec.one_point_zero_i128 },
    },
    .{
        .name = "list_refcount_nested: deeply nested inline",
        .source =
        \\match [[[1]]] { [lst] => match lst { [lst2] => match lst2 { [x] => x, _ => 0 }, _ => 0 }, _ => 0 }
        ,
        .expected = .{ .dec_val = 1 * RocDec.one_point_zero_i128 },
    },
    .{
        .name = "list_refcount_nested: mixed nested and flat",
        .source =
        \\match [[1, 2], [3]] { [first, second] => {
        \\    a = match first { [x, ..] => x, _ => 0 }
        \\    b = match second { [y] => y, _ => 0 }
        \\    a + b
        \\}, _ => 0 }
        ,
        .expected = .{ .dec_val = 4 * RocDec.one_point_zero_i128 },
    },

    // --- from list_refcount_complex.zig ---
    .{
        .name = "list_refcount_complex: list of records with strings",
        .source =
        \\{
        \\    r1 = {s: "a"}
        \\    r2 = {s: "b"}
        \\    lst = [r1, r2]
        \\    match lst { [first, ..] => first.s, _ => "" }
        \\}
        ,
        .expected = .{ .str_val = "a" },
    },
    .{
        .name = "list_refcount_complex: list of records with integers",
        .source =
        \\{
        \\    r1 = {val: 10}
        \\    r2 = {val: 20}
        \\    lst = [r1, r2]
        \\    match lst { [first, ..] => first.val, _ => 0 }
        \\}
        ,
        .expected = .{ .dec_val = 10 * RocDec.one_point_zero_i128 },
    },
    .{
        .name = "list_refcount_complex: same record multiple times in list",
        .source =
        \\{
        \\    r = {val: 42}
        \\    lst = [r, r, r]
        \\    match lst { [first, ..] => first.val, _ => 0 }
        \\}
        ,
        .expected = .{ .dec_val = 42 * RocDec.one_point_zero_i128 },
    },
    .{
        .name = "list_refcount_complex: list of records with nested data",
        .source =
        \\{
        \\    r1 = {inner: {val: 10}}
        \\    r2 = {inner: {val: 20}}
        \\    lst = [r1, r2]
        \\    match lst { [first, ..] => first.inner.val, _ => 0 }
        \\}
        ,
        .expected = .{ .dec_val = 10 * RocDec.one_point_zero_i128 },
    },
    .{
        .name = "list_refcount_complex: list of tuples with integers",
        .source =
        \\{
        \\    t1 = (1, 2)
        \\    t2 = (3, 4)
        \\    lst = [t1, t2]
        \\    match lst { [first, ..] => match first { (a, b) => a + b }, _ => 0 }
        \\}
        ,
        .expected = .{ .dec_val = 3 * RocDec.one_point_zero_i128 },
    },
    .{
        .name = "list_refcount_complex: list of tuples with strings",
        .source =
        \\{
        \\    t1 = ("a", "b")
        \\    t2 = ("c", "d")
        \\    lst = [t1, t2]
        \\    match lst { [first, ..] => match first { (s, _) => s }, _ => "" }
        \\}
        ,
        .expected = .{ .str_val = "a" },
    },
    .{
        .name = "list_refcount_complex: list of tags with integers",
        .source =
        \\match Some([10, 20]) { Some(lst) => match lst { [x, ..] => x, _ => 0 }, None => 0 }
        ,
        .expected = .{ .dec_val = 10 * RocDec.one_point_zero_i128 },
    },
    .{
        .name = "list_refcount_complex: list of tags with strings",
        .source =
        \\match Some(["hello", "world"]) { Some(lst) => match lst { [s, ..] => s, _ => "" }, None => "" }
        ,
        .expected = .{ .str_val = "hello" },
    },
    .{
        .name = "list_refcount_complex: list of records of lists of strings",
        .source =
        \\{
        \\    r1 = {items: ["a", "b"]}
        \\    r2 = {items: ["c", "d"]}
        \\    lst = [r1, r2]
        \\    match lst { [first, ..] => match first.items { [s, ..] => s, _ => "" }, _ => "" }
        \\}
        ,
        .expected = .{ .str_val = "a" },
    },
    .{
        .name = "list_refcount_complex: inline complex structure",
        .source =
        \\{
        \\    data = [{val: 1}, {val: 2}]
        \\    match data { [first, ..] => first.val, _ => 0 }
        \\}
        ,
        .expected = .{ .dec_val = 1 * RocDec.one_point_zero_i128 },
    },
    .{
        .name = "list_refcount_complex: deeply nested mixed structures",
        .source =
        \\{
        \\    inner = {x: 42}
        \\    outer = {nested: inner}
        \\    lst = [outer]
        \\    match lst { [first, ..] => first.nested.x, _ => 0 }
        \\}
        ,
        .expected = .{ .dec_val = 42 * RocDec.one_point_zero_i128 },
    },
    .{
        .name = "list_refcount_complex: list of Ok/Err tags",
        .source =
        \\match Ok([1, 2]) { Ok(lst) => match lst { [x, ..] => x, _ => 0 }, Err(_) => 0 }
        ,
        .expected = .{ .dec_val = 1 * RocDec.one_point_zero_i128 },
    },
    // --- inspect_str tests: records, tuples, lists ---
    // Tuples
    .{ .name = "tuple: (10, 20)", .source = "(10, 20)", .expected = .{ .inspect_str = "(10.0, 20.0)" } },
    .{ .name = "tuple: (5 + 1, 5 * 3)", .source = "(5 + 1, 5 * 3)", .expected = .{ .inspect_str = "(6.0, 15.0)" } },
    // Records - fold with record accumulator
    .{
        .name = "record: fold sum and count",
        .source = "List.fold([1, 2, 3], {sum: 0, count: 0}, |acc, item| {sum: acc.sum + item, count: acc.count + 1})",
        .expected = .{ .inspect_str = "{ count: 3.0, sum: 6.0 }" },
    },
    .{
        .name = "record: fold empty list",
        .source = "List.fold([], {sum: 0, count: 0}, |acc, item| {sum: acc.sum + item, count: acc.count + 1})",
        .expected = .{ .inspect_str = "{ count: 0.0, sum: 0.0 }" },
    },
    .{
        .name = "record: fold single field",
        .source = "List.fold([1, 2, 3, 4], {total: 0}, |acc, item| {total: acc.total + item})",
        .expected = .{ .inspect_str = "{ total: 10.0 }" },
    },
    .{
        .name = "record: fold record update syntax",
        .source = "List.fold([1, 2, 3], {sum: 0, count: 0}, |acc, item| {..acc, sum: acc.sum + item, count: acc.count + 1})",
        .expected = .{ .inspect_str = "{ count: 3.0, sum: 6.0 }" },
    },
    .{
        .name = "record: fold partial update",
        .source = "List.fold([1, 2, 3, 4], {sum: 0, multiplier: 2}, |acc, item| {..acc, sum: acc.sum + item})",
        .expected = .{ .inspect_str = "{ multiplier: 2.0, sum: 10.0 }" },
    },
    .{
        .name = "record: fold nested field access",
        .source = "List.fold([1, 2, 3], {value: 0}, |acc, item| {value: acc.value + item})",
        .expected = .{ .inspect_str = "{ value: 6.0 }" },
    },
    .{
        .name = "record: fold three fields",
        .source = "List.fold([1, 2, 3, 4], {sum: 0, count: 0, product: 1}, |acc, item| {sum: acc.sum + item, count: acc.count + 1, product: acc.product * item})",
        .expected = .{ .inspect_str = "{ count: 4.0, product: 24.0, sum: 10.0 }" },
    },
    .{
        .name = "record: fold conditional update",
        .source = "List.fold([1, 2, 3, 4], {evens: 0, odds: 0}, |acc, item| if item % 2 == 0 {evens: acc.evens + item, odds: acc.odds} else {evens: acc.evens, odds: acc.odds + item})",
        .expected = .{ .inspect_str = "{ evens: 6.0, odds: 4.0 }" },
    },
    .{
        .name = "record: fold string list count",
        .source = "List.fold([\"a\", \"bb\", \"ccc\"], {count: 0}, |acc, _| {count: acc.count + 1})",
        .expected = .{ .inspect_str = "{ count: 3.0 }" },
    },
    .{
        .name = "record: fold record destructuring",
        .source = "List.fold([{x: 1, y: 2}, {x: 2, y: 5}, {x: 3, y: 8}], {total_x: 0, total_y: 0}, |acc, {x, y}| {total_x: acc.total_x + x, total_y: acc.total_y + y})",
        .expected = .{ .inspect_str = "{ total_x: 6.0, total_y: 15.0 }" },
    },
    .{
        .name = "record: fold partial record destructuring",
        .source = "List.fold([{a: 1, b: 100}, {a: 2, b: 200}, {a: 3, b: 300}], {sum: 0}, |acc, {a}| {sum: acc.sum + a})",
        .expected = .{ .inspect_str = "{ sum: 6.0 }" },
    },
    .{
        .name = "record: fold single-field record destructuring",
        .source = "List.fold([{val: 1}, {val: 2}, {val: 3}, {val: 4}], {total: 0}, |acc, {val}| {total: acc.total + val})",
        .expected = .{ .inspect_str = "{ total: 10.0 }" },
    },
    .{
        .name = "record: fold list destructuring",
        .source = "List.fold([[1, 2], [3, 4], [5, 6]], {first_sum: 0, count: 0}, |acc, [first, ..]| {first_sum: acc.first_sum + first, count: acc.count + 1})",
        .expected = .{ .inspect_str = "{ count: 3.0, first_sum: 9.0 }" },
    },
    .{
        .name = "record: fold destructure two elements",
        .source = "List.fold([[1, 2, 100], [3, 4, 200], [5, 6, 300]], {sum_firsts: 0, sum_seconds: 0}, |acc, [a, b, ..]| {sum_firsts: acc.sum_firsts + a, sum_seconds: acc.sum_seconds + b})",
        .expected = .{ .inspect_str = "{ sum_firsts: 9.0, sum_seconds: 12.0 }" },
    },
    .{
        .name = "record: fold exact list pattern",
        .source = "List.fold([[1, 2], [3, 4], [5, 6]], {total: 0}, |acc, [a, b]| {total: acc.total + a + b})",
        .expected = .{ .inspect_str = "{ total: 21.0 }" },
    },
    .{
        .name = "record: fold nested list and record",
        .source = "List.fold([[1, 10, 20], [2, 30, 40], [3, 50, 60]], {head_sum: 0, tail_count: 0}, |acc, [head, .. as tail]| {head_sum: acc.head_sum + head, tail_count: acc.tail_count + List.len(tail)})",
        .expected = .{ .inspect_str = "{ head_sum: 6.0, tail_count: 6 }" },
    },
    // Focused record fold tests
    .{
        .name = "focused: fold single-field record",
        .source = "List.fold([1, 2, 3, 4], {total: 0}, |acc, item| {total: acc.total + item})",
        .expected = .{ .inspect_str = "{ total: 10.0 }" },
    },
    .{
        .name = "focused: fold record partial update",
        .source = "List.fold([1, 2, 3, 4], {sum: 0, multiplier: 2}, |acc, item| {..acc, sum: acc.sum + item})",
        .expected = .{ .inspect_str = "{ multiplier: 2.0, sum: 10.0 }" },
    },
    .{
        .name = "focused: fold record nested field access",
        .source = "List.fold([1, 2, 3], {value: 0}, |acc, item| {value: acc.value + item})",
        .expected = .{ .inspect_str = "{ value: 6.0 }" },
    },
    .{
        .name = "focused: fold record over string list",
        .source = "List.fold([\"a\", \"bb\", \"ccc\"], {count: 0}, |acc, _| {count: acc.count + 1})",
        .expected = .{ .inspect_str = "{ count: 3.0 }" },
    },
    .{
        .name = "focused: fold multi-field record binding identity",
        .source =
        \\{
        \\    rec = List.fold([1, 2, 3], {sum: 0, count: 0}, |acc, item| {sum: acc.sum + item, count: acc.count + 1})
        \\    rec
        \\}
        ,
        .expected = .{ .inspect_str = "{ count: 3.0, sum: 6.0 }" },
    },
    .{
        .name = "focused: fold multi-field record binding survives extra alloc",
        .source =
        \\{
        \\    rec = List.fold([1, 2, 3], {sum: 0, count: 0}, |acc, item| {sum: acc.sum + item, count: acc.count + 1})
        \\    _tmp = 999
        \\    rec
        \\}
        ,
        .expected = .{ .inspect_str = "{ count: 3.0, sum: 6.0 }" },
    },
    .{
        .name = "focused: fold partial record destructuring",
        .source = "List.fold([{a: 1, b: 100}, {a: 2, b: 200}, {a: 3, b: 300}], {sum: 0}, |acc, {a}| {sum: acc.sum + a})",
        .expected = .{ .inspect_str = "{ sum: 6.0 }" },
    },
    .{
        .name = "focused: fold single-field record destructuring",
        .source = "List.fold([{val: 1}, {val: 2}, {val: 3}, {val: 4}], {total: 0}, |acc, {val}| {total: acc.total + val})",
        .expected = .{ .inspect_str = "{ total: 10.0 }" },
    },
    .{
        .name = "focused: fold exact list pattern",
        .source = "List.fold([[1, 2], [3, 4], [5, 6]], {total: 0}, |acc, [a, b]| {total: acc.total + a + b})",
        .expected = .{ .inspect_str = "{ total: 21.0 }" },
    },
    .{
        .name = "focused: list append zst",
        .source = "List.append([{}], {})",
        .expected = .{ .inspect_str = "[{}, {}]" },
    },
    // List I64 tests
    .{
        .name = "list: for loop mutable append",
        .source =
        \\{
        \\    list = [1.I64, 2.I64, 3.I64]
        \\    var $result = List.with_capacity(List.len(list))
        \\    for item in list {
        \\        $result = List.append($result, item)
        \\    }
        \\    $result
        \\}
        ,
        .expected = .{ .inspect_str = "[1, 2, 3]" },
    },
    .{
        .name = "list: for loop with closure transform",
        .source =
        \\{
        \\    list = [1.I64, 2.I64, 3.I64]
        \\    identity = |x| x
        \\    var $result = List.with_capacity(List.len(list))
        \\    for item in list {
        \\        $result = List.append($result, identity(item))
        \\    }
        \\    $result
        \\}
        ,
        .expected = .{ .inspect_str = "[1, 2, 3]" },
    },
    .{ .name = "list: map identity", .source = "List.map([1.I64, 2.I64, 3.I64], |x| x)", .expected = .{ .inspect_str = "[1, 2, 3]" } },
    .{ .name = "list: map single element", .source = "List.map([42.I64], |x| x)", .expected = .{ .inspect_str = "[42]" } },
    .{ .name = "list: map squaring", .source = "List.map([1.I64, 2.I64, 3.I64, 4.I64, 5.I64], |x| x * x)", .expected = .{ .inspect_str = "[1, 4, 9, 16, 25]" } },
    .{ .name = "list: map doubling", .source = "List.map([1.I64, 2.I64, 3.I64], |x| x * 2.I64)", .expected = .{ .inspect_str = "[2, 4, 6]" } },
    .{ .name = "list: map adding", .source = "List.map([10.I64, 20.I64], |x| x + 5.I64)", .expected = .{ .inspect_str = "[15, 25]" } },
    // List ZST / empty list tests
    .{ .name = "list: map empty", .source = "List.map([], |x| x)", .expected = .{ .inspect_str = "[]" } },
    .{ .name = "list: empty non-numeric constraint", .source = "[]", .expected = .{ .inspect_str = "[]" } },
    .{ .name = "list: append zst", .source = "List.append([{}], {})", .expected = .{ .inspect_str = "[{}, {}]" } },
    .{ .name = "list: with_capacity unknown", .source = "List.with_capacity(5)", .expected = .{ .inspect_str = "[]" } },
    // List append / repeat
    .{ .name = "list: append basic", .source = "List.append([1.I64, 2.I64], 3.I64)", .expected = .{ .inspect_str = "[1, 2, 3]" } },
    .{ .name = "list: append empty", .source = "List.append([], 42.I64)", .expected = .{ .inspect_str = "[42]" } },
    .{ .name = "list: repeat basic", .source = "List.repeat(7.I64, 4)", .expected = .{ .inspect_str = "[7, 7, 7, 7]" } },
    .{ .name = "list: repeat empty", .source = "List.repeat(7.I64, 0)", .expected = .{ .inspect_str = "[]" } },
    .{ .name = "list: with_capacity append", .source = "List.with_capacity(5).append(10.I64)", .expected = .{ .inspect_str = "[10]" } },
    // Dec fold/sum tests
    .{
        .name = "dec: simple fold sum",
        .source = "List.fold([1, 2, 3], 0, |acc, item| acc + item)",
        .expected = .{ .dec_val = 6 * RocDec.one_point_zero_i128 },
    },
    .{ .name = "dec: List.sum basic", .source = "List.sum([1, 2, 3, 4])", .expected = .{ .dec_val = 10 * RocDec.one_point_zero_i128 } },
    .{ .name = "dec: List.sum single", .source = "List.sum([42])", .expected = .{ .dec_val = 42 * RocDec.one_point_zero_i128 } },
    .{ .name = "dec: List.sum negative", .source = "List.sum([-1, -2, 3, 4])", .expected = .{ .dec_val = 4 * RocDec.one_point_zero_i128 } },
    .{ .name = "dec: List.sum larger", .source = "List.sum([1, 2, 3, 4, 5, 6, 7, 8, 9, 10])", .expected = .{ .dec_val = 55 * RocDec.one_point_zero_i128 } },
    // Decimal literal evaluation (upgrade from runExpectSuccess)
    .{ .name = "dec: literal 0.0", .source = "0.0.Dec", .expected = .{ .dec_val = 0 } },
    .{ .name = "dec: literal 123.456", .source = "123.456.Dec", .expected = .{ .dec_val = 123_456_000_000_000_000_000 } },
    // Float literal evaluation (upgrade from runExpectSuccess)
    .{ .name = "f64: literal 3.14", .source = "3.14.F64", .expected = .{ .f64_val = 3.14 } },
    .{ .name = "f32: literal 2.5", .source = "2.5.F32", .expected = .{ .f32_val = 2.5 } },
    .{ .name = "f64: literal -3.14", .source = "-3.14.F64", .expected = .{ .f64_val = -3.14 } },
    .{ .name = "f32: literal 0.0", .source = "0.0.F32", .expected = .{ .f32_val = 0.0 } },
    // Scientific notation (upgrade from runExpectSuccess — use inspect_str since the i128 values are hard to compute)
    .{ .name = "dec: scientific 1e5", .source = "1e5", .expected = .{ .inspect_str = "99999.999999999991611392" } },
    .{ .name = "dec: scientific 2.5e10", .source = "2.5e10", .expected = .{ .inspect_str = "24999999999.999997858287714304" } },
    .{ .name = "dec: scientific 1.5e-5", .source = "1.5e-5", .expected = .{ .inspect_str = "0.000015" } },
    .{ .name = "dec: scientific -1.5e-5", .source = "-1.5e-5", .expected = .{ .inspect_str = "-0.000015" } },
    // String literal evaluation (upgrade from runExpectSuccess)
    .{ .name = "str: Hello World", .source = "\"Hello, World!\"", .expected = .{ .str_val = "Hello, World!" } },
    .{ .name = "str: empty", .source = "\"\"", .expected = .{ .str_val = "" } },
    .{ .name = "str: Roc", .source = "\"Roc\"", .expected = .{ .str_val = "Roc" } },
    .{
        .name = "str: interpolation",
        .source =
        \\{
        \\    hello = "Hello"
        \\    world = "World"
        \\    "${hello} ${world}"
        \\}
        ,
        .expected = .{ .str_val = "Hello World" },
    },
    // Issue 8667: List.with_capacity type inference
    .{
        .name = "issue 8667: with_capacity append",
        .source = "List.append(List.with_capacity(1), 1.I64)",
        .expected = .{ .inspect_str = "[1]" },
    },
    .{
        .name = "issue 8667: fold with inline append",
        .source = "[1.I64].fold(List.with_capacity(1), |acc, item| acc.append(item))",
        .expected = .{ .inspect_str = "[1]" },
    },
    .{
        .name = "issue 8667: fold with List.append",
        .source = "[1.I64].fold(List.with_capacity(1), List.append)",
        .expected = .{ .inspect_str = "[1]" },
    },
    // Issue 8710: tag union with heap payload in tuple
    .{ .name = "issue 8710: list len", .source = "[1.I64, 2.I64, 3.I64].len()", .expected = .{ .i64_val = 3 } },
    .{
        .name = "issue 8710: tag union in tuple",
        .source =
        \\{
        \\    list = [1.I64, 2.I64, 3.I64]
        \\    _tuple = (Ok(list), 42.I64)
        \\    list
        \\}
        ,
        .expected = .{ .inspect_str = "[1, 2, 3]" },
    },
    // --- from eval_test.zig: tag union regression tests ---
    // These produce tag union results. The interpreter can evaluate them but
    // RocValue.format() can't render tag unions yet (returns TagUnionNotSupported),
    // so inspect_str falls back to compiled-backend-only comparison.
    .{
        .name = "match with tag containing pattern-bound variable - regression",
        .source =
        \\match Some("x") {
        \\    Some(a) => Tagged(a)
        \\    None => Tagged("")
        \\}
        ,
        .expected = .{ .inspect_str = "Tagged(\"x\")" },
        .skip = .{ .wasm = true, .llvm = true },
    },
    .{
        .name = "nested match with Result type - regression",
        .source =
        \\match ["x"] {
        \\    [a] => {
        \\        match Ok(a) {
        \\            Ok(val) => Ok(val),
        \\            _ => Err(Oops)
        \\        }
        \\    }
        \\    _ => Err(Oops)
        \\}
        ,
        .expected = .{ .inspect_str = "Ok(\"x\")" },
        .skip = .{ .wasm = true, .llvm = true },
    },
    .{
        .name = "issue 8892: nominal type wrapping tag union with match expression",
        .source =
        \\{
        \\    parse_value = || {
        \\        combination_method = match ModuloToken {
        \\            ModuloToken => Modulo
        \\        }
        \\        combination_method
        \\    }
        \\    parse_value()
        \\}
        ,
        .expected = .{ .inspect_str = "Modulo" },
        .skip = .{ .wasm = true, .llvm = true },
    },

    // --- known bugs (skipped on all backends) ---
    .{
        .name = "early return: ? in closure passed to List.fold",
        .source =
        \\{
        \\    compute = |x| Ok(x?)
        \\    result = List.fold([Ok(1), Err({})], [], |acc, x| List.append(acc, compute(x)))
        \\    List.len(result)
        \\}
        ,
        .expected = .{ .u64_val = 2 },
        .skip = .{ .interpreter = true, .dev = true, .wasm = true, .llvm = true },
    },
    .{
        .name = "known crash repro: polymorphic tag union payload substitution - extract payload",
        .source =
        \\{
        \\    second : [Left(a), Right(b)] -> b
        \\    second = |either| match either {
        \\        Left(_) => 0.I64
        \\        Right(val) => val
        \\    }
        \\
        \\    input : [Left(I64), Right(I64)]
        \\    input = Right(42.I64)
        \\    second(input)
        \\}
        ,
        .expected = .{ .i64_val = 42 },
        .skip = .{ .interpreter = true, .dev = true, .wasm = true, .llvm = true },
    },
    .{
        .name = "known crash repro: polymorphic tag union payload substitution - multiple type vars",
        .source =
        \\{
        \\    get_err : [Ok(a), Err(e)] -> e
        \\    get_err = |result| match result {
        \\        Ok(_) => ""
        \\        Err(e) => e
        \\    }
        \\
        \\    val : [Ok(I64), Err(Str)]
        \\    val = Err("hello")
        \\    get_err(val)
        \\}
        ,
        .expected = .{ .str_val = "hello" },
        .skip = .{ .interpreter = true, .dev = true, .wasm = true, .llvm = true },
    },

    // --- non-Dec numeric method dispatch (Gap #1, lines 17681-17729) ---
    .{
        .name = "I32 addition via method dispatch",
        .source = "1.I32 + 2.I32",
        .expected = .{ .i32_val = 3 },
    },
    .{
        .name = "I32 subtraction via method dispatch",
        .source = "10.I32 - 3.I32",
        .expected = .{ .i32_val = 7 },
    },
    .{
        .name = "I32 multiplication via method dispatch",
        .source = "4.I32 * 5.I32",
        .expected = .{ .i32_val = 20 },
    },
    .{
        .name = "I64 addition via method dispatch",
        .source = "100.I64 + 200.I64",
        .expected = .{ .i64_val = 300 },
    },
    .{
        .name = "U64 addition via method dispatch",
        .source = "10.U64 + 20.U64",
        .expected = .{ .u64_val = 30 },
    },
    .{
        .name = "U32 addition via method dispatch",
        .source = "7.U32 + 3.U32",
        .expected = .{ .u32_val = 10 },
    },
    .{
        .name = "I32 greater than comparison",
        .source = "5.I32 > 3.I32",
        .expected = .{ .bool_val = true },
    },
    .{
        .name = "I32 less than comparison",
        .source = "2.I32 < 10.I32",
        .expected = .{ .bool_val = true },
    },
    .{
        .name = "I32 greater than or equal comparison",
        .source = "5.I32 >= 5.I32",
        .expected = .{ .bool_val = true },
    },
    .{
        .name = "I32 less than or equal comparison",
        .source = "3.I32 <= 5.I32",
        .expected = .{ .bool_val = true },
    },
    .{
        .name = "I32 equality comparison",
        .source = "42.I32 == 42.I32",
        .expected = .{ .bool_val = true },
    },
    .{
        .name = "I32 inequality comparison",
        .source = "42.I32 != 43.I32",
        .expected = .{ .bool_val = true },
    },
    .{
        .name = "I64 division via method dispatch",
        .source = "20.I64 // 4.I64",
        .expected = .{ .i64_val = 5 },
    },
    .{
        .name = "I64 remainder via method dispatch",
        .source = "17.I64 % 5.I64",
        .expected = .{ .i64_val = 2 },
    },

    // --- integer type conversions (Gaps #5-#12) ---
    .{
        .name = "I64 to I128",
        .source = "{ 42.I64.to_i128() }",
        .expected = .{ .i128_val = 42 },
    },
    .{
        .name = "I64 to F32",
        .source = "{ 42.I64.to_f32() }",
        .expected = .{ .f32_val = 42.0 },
    },
    .{
        .name = "I64 to F64",
        .source = "{ 42.I64.to_f64() }",
        .expected = .{ .f64_val = 42.0 },
    },
    .{
        .name = "U64 to I128",
        .source = "{ 42.U64.to_i128() }",
        .expected = .{ .i128_val = 42 },
    },
    .{
        .name = "U64 to F64",
        .source = "{ 42.U64.to_f64() }",
        .expected = .{ .f64_val = 42.0 },
    },
    .{
        .name = "I32 to I128",
        .source = "{ 42.I32.to_i128() }",
        .expected = .{ .i128_val = 42 },
    },
    .{
        .name = "I32 to F64",
        .source = "{ 42.I32.to_f64() }",
        .expected = .{ .f64_val = 42.0 },
    },
    .{
        .name = "U32 to I128",
        .source = "{ 42.U32.to_i128() }",
        .expected = .{ .i128_val = 42 },
    },
    .{
        .name = "U32 to F64",
        .source = "{ 42.U32.to_f64() }",
        .expected = .{ .f64_val = 42.0 },
    },
    .{
        .name = "I16 to I128",
        .source = "{ 42.I16.to_i128() }",
        .expected = .{ .i128_val = 42 },
    },
    .{
        .name = "I16 to F64",
        .source = "{ 42.I16.to_f64() }",
        .expected = .{ .f64_val = 42.0 },
    },
    .{
        .name = "U16 to I128",
        .source = "{ 42.U16.to_i128() }",
        .expected = .{ .i128_val = 42 },
    },
    .{
        .name = "U16 to F64",
        .source = "{ 42.U16.to_f64() }",
        .expected = .{ .f64_val = 42.0 },
    },
    .{
        .name = "I8 to I128",
        .source = "{ 42.I8.to_i128() }",
        .expected = .{ .i128_val = 42 },
    },
    .{
        .name = "I8 to F64",
        .source = "{ 42.I8.to_f64() }",
        .expected = .{ .f64_val = 42.0 },
    },
    .{
        .name = "I128 to F64",
        .source = "{ 42.I128.to_f64() }",
        .expected = .{ .f64_val = 42.0 },
    },
    .{
        .name = "U128 to F64",
        .source = "{ 42.U128.to_f64() }",
        .expected = .{ .f64_val = 42.0 },
    },
    // TODO: narrowing/wrapping conversions crash across all backends
    .{ .name = "U64 to U8 wrapping", .source = "{ 300.U64.to_u8() }", .expected = .{ .u8_val = 44 }, .skip = SKIP_ALL },
    .{ .name = "U64 to I8 wrapping", .source = "{ 200.U64.to_i8() }", .expected = .{ .i8_val = -56 }, .skip = SKIP_ALL },
    .{ .name = "I64 to U8 wrapping", .source = "{ 256.I64.to_u8() }", .expected = .{ .u8_val = 0 }, .skip = SKIP_ALL },
    .{ .name = "I64 to I8 wrapping", .source = "{ 300.I64.to_i8() }", .expected = .{ .i8_val = 44 }, .skip = SKIP_ALL },
    .{ .name = "U32 to U8 wrapping", .source = "{ 300.U32.to_u8() }", .expected = .{ .u8_val = 44 }, .skip = SKIP_ALL },
    .{
        .name = "U32 to U64",
        .source = "{ 42.U32.to_u64() }",
        .expected = .{ .u64_val = 42 },
    },
    .{
        .name = "U16 to U32",
        .source = "{ 42.U16.to_u32() }",
        .expected = .{ .u32_val = 42 },
    },
    .{ .name = "I128 to I8 wrapping", .source = "{ 300.I128.to_i8() }", .expected = .{ .i8_val = 44 }, .skip = SKIP_ALL },
    .{ .name = "U128 to U8 wrapping", .source = "{ 300.U128.to_u8() }", .expected = .{ .u8_val = 44 }, .skip = SKIP_ALL },
    // TODO: signed-to-unsigned conversions crash across all backends
    .{ .name = "I64 to U64", .source = "{ 42.I64.to_u64() }", .expected = .{ .u64_val = 42 }, .skip = SKIP_ALL },
    .{ .name = "I64 to U32", .source = "{ 42.I64.to_u32() }", .expected = .{ .u32_val = 42 }, .skip = SKIP_ALL },
    .{ .name = "I64 to U16", .source = "{ 42.I64.to_u16() }", .expected = .{ .u16_val = 42 }, .skip = SKIP_ALL },

    // --- shift operations (Gaps #10, #13) ---
    .{
        .name = "shift left I64",
        .source = "{ 1.I64.shift_left_by(4.U8) }",
        .expected = .{ .i64_val = 16 },
    },
    .{
        .name = "shift left U64",
        .source = "{ 1.U64.shift_left_by(8.U8) }",
        .expected = .{ .u64_val = 256 },
    },
    .{
        .name = "shift left I32",
        .source = "{ 1.I32.shift_left_by(3.U8) }",
        .expected = .{ .i32_val = 8 },
    },
    .{
        .name = "shift left U32",
        .source = "{ 1.U32.shift_left_by(5.U8) }",
        .expected = .{ .u32_val = 32 },
    },
    .{
        .name = "shift left I16",
        .source = "{ 1.I16.shift_left_by(2.U8) }",
        .expected = .{ .i16_val = 4 },
    },
    .{
        .name = "shift left U8",
        .source = "{ 1.U8.shift_left_by(7.U8) }",
        .expected = .{ .u8_val = 128 },
    },
    .{
        .name = "shift right zf I64",
        .source = "{ 128.I64.shift_right_zf_by(2.U8) }",
        .expected = .{ .i64_val = 32 },
    },
    .{
        .name = "shift right zf U64",
        .source = "{ 256.U64.shift_right_zf_by(4.U8) }",
        .expected = .{ .u64_val = 16 },
    },
    .{
        .name = "shift right zf I32",
        .source = "{ 64.I32.shift_right_zf_by(3.U8) }",
        .expected = .{ .i32_val = 8 },
    },
    .{
        .name = "shift right zf U32",
        .source = "{ 1024.U32.shift_right_zf_by(5.U8) }",
        .expected = .{ .u32_val = 32 },
    },
    .{
        .name = "shift right zf U16",
        .source = "{ 512.U16.shift_right_zf_by(4.U8) }",
        .expected = .{ .u16_val = 32 },
    },
    .{
        .name = "shift right zf U8",
        .source = "{ 240.U8.shift_right_zf_by(4.U8) }",
        .expected = .{ .u8_val = 15 },
    },

    // --- F32/F64 to int conversions (Gaps #3, #4) ---
    // TODO: float-to-int and float narrowing conversions crash across all backends
    .{ .name = "F64 to I64", .source = "{ 42.0.F64.to_i64() }", .expected = .{ .i64_val = 42 }, .skip = SKIP_ALL },
    .{ .name = "F64 to I32", .source = "{ 42.0.F64.to_i32() }", .expected = .{ .i32_val = 42 }, .skip = SKIP_ALL },
    .{ .name = "F64 to I16", .source = "{ 42.0.F64.to_i16() }", .expected = .{ .i16_val = 42 }, .skip = SKIP_ALL },
    .{ .name = "F64 to I8", .source = "{ 42.0.F64.to_i8() }", .expected = .{ .i8_val = 42 }, .skip = SKIP_ALL },
    .{ .name = "F64 to U64", .source = "{ 42.0.F64.to_u64() }", .expected = .{ .u64_val = 42 }, .skip = SKIP_ALL },
    .{ .name = "F64 to U32", .source = "{ 42.0.F64.to_u32() }", .expected = .{ .u32_val = 42 }, .skip = SKIP_ALL },
    .{ .name = "F64 to U16", .source = "{ 42.0.F64.to_u16() }", .expected = .{ .u16_val = 42 }, .skip = SKIP_ALL },
    .{ .name = "F64 to U8", .source = "{ 42.0.F64.to_u8() }", .expected = .{ .u8_val = 42 }, .skip = SKIP_ALL },
    .{ .name = "F64 to F32", .source = "{ 1.5.F64.to_f32() }", .expected = .{ .f32_val = 1.5 }, .skip = SKIP_ALL },
    .{ .name = "F32 to I64", .source = "{ 42.0.F32.to_i64() }", .expected = .{ .i64_val = 42 }, .skip = SKIP_ALL },
    .{ .name = "F32 to I32", .source = "{ 42.0.F32.to_i32() }", .expected = .{ .i32_val = 42 }, .skip = SKIP_ALL },
    .{ .name = "F32 to U64", .source = "{ 42.0.F32.to_u64() }", .expected = .{ .u64_val = 42 }, .skip = SKIP_ALL },
    .{ .name = "F32 to U32", .source = "{ 42.0.F32.to_u32() }", .expected = .{ .u32_val = 42 }, .skip = SKIP_ALL },
    .{
        .name = "F32 to F64",
        .source = "{ 1.5.F32.to_f64() }",
        .expected = .{ .f64_val = 1.5 },
    },

    // --- Dec to int/float conversions (Gap #2) ---
    // TODO: Dec-to-int and Dec-to-F32 conversions crash across all backends
    .{ .name = "Dec to I64", .source = "{ 42.to_i64() }", .expected = .{ .i64_val = 42 }, .skip = SKIP_ALL },
    .{ .name = "Dec to I32", .source = "{ 42.to_i32() }", .expected = .{ .i32_val = 42 }, .skip = SKIP_ALL },
    .{ .name = "Dec to I16", .source = "{ 42.to_i16() }", .expected = .{ .i16_val = 42 }, .skip = SKIP_ALL },
    .{ .name = "Dec to I8", .source = "{ 42.to_i8() }", .expected = .{ .i8_val = 42 }, .skip = SKIP_ALL },
    .{ .name = "Dec to U64", .source = "{ 42.to_u64() }", .expected = .{ .u64_val = 42 }, .skip = SKIP_ALL },
    .{ .name = "Dec to U32", .source = "{ 42.to_u32() }", .expected = .{ .u32_val = 42 }, .skip = SKIP_ALL },
    .{ .name = "Dec to U16", .source = "{ 42.to_u16() }", .expected = .{ .u16_val = 42 }, .skip = SKIP_ALL },
    .{ .name = "Dec to U8", .source = "{ 42.to_u8() }", .expected = .{ .u8_val = 42 }, .skip = SKIP_ALL },
    .{ .name = "Dec to I128", .source = "{ 42.to_i128() }", .expected = .{ .i128_val = 42 }, .skip = SKIP_ALL },
    .{ .name = "Dec to U128", .source = "{ 42.to_u128() }", .expected = .{ .u128_val = 42 }, .skip = SKIP_ALL },
    .{ .name = "Dec to F32", .source = "{ 1.5.to_f32() }", .expected = .{ .f32_val = 1.5 }, .skip = SKIP_ALL },
    .{
        .name = "Dec to F64",
        .source = "{ 1.5.to_f64() }",
        .expected = .{ .f64_val = 1.5 },
    },

    // --- typed int arithmetic (U8, U16, I8, I16) ---
    .{ .name = "U8 addition", .source = "1.U8 + 2.U8", .expected = .{ .u8_val = 3 } },
    .{ .name = "U8 subtraction", .source = "10.U8 - 3.U8", .expected = .{ .u8_val = 7 } },
    .{ .name = "U8 multiplication", .source = "5.U8 * 4.U8", .expected = .{ .u8_val = 20 } },
    .{ .name = "U16 addition", .source = "100.U16 + 200.U16", .expected = .{ .u16_val = 300 } },
    .{ .name = "U16 multiplication", .source = "10.U16 * 20.U16", .expected = .{ .u16_val = 200 } },
    .{ .name = "I8 addition", .source = "10.I8 + 20.I8", .expected = .{ .i8_val = 30 } },
    .{ .name = "I8 subtraction", .source = "50.I8 - 20.I8", .expected = .{ .i8_val = 30 } },
    .{ .name = "I16 addition", .source = "100.I16 + 200.I16", .expected = .{ .i16_val = 300 } },
    .{ .name = "I16 multiplication", .source = "10.I16 * 30.I16", .expected = .{ .i16_val = 300 } },
    .{ .name = "I128 addition", .source = "100.I128 + 200.I128", .expected = .{ .i128_val = 300 } },
    .{ .name = "U128 addition", .source = "100.U128 + 200.U128", .expected = .{ .u128_val = 300 } },

    // --- typed int comparisons on more types ---
    .{ .name = "U64 greater than", .source = "10.U64 > 5.U64", .expected = .{ .bool_val = true } },
    .{ .name = "U64 less than", .source = "3.U64 < 7.U64", .expected = .{ .bool_val = true } },
    .{ .name = "U64 equality", .source = "42.U64 == 42.U64", .expected = .{ .bool_val = true } },
    .{ .name = "I64 greater than", .source = "10.I64 > 5.I64", .expected = .{ .bool_val = true } },
    .{ .name = "I64 less than", .source = "3.I64 < 7.I64", .expected = .{ .bool_val = true } },
    .{ .name = "I64 equality", .source = "42.I64 == 42.I64", .expected = .{ .bool_val = true } },
    .{ .name = "U32 greater than", .source = "10.U32 > 5.U32", .expected = .{ .bool_val = true } },
    .{ .name = "U8 equality", .source = "42.U8 == 42.U8", .expected = .{ .bool_val = true } },
    .{ .name = "I8 less than", .source = "3.I8 < 7.I8", .expected = .{ .bool_val = true } },
    .{ .name = "I128 equality", .source = "42.I128 == 42.I128", .expected = .{ .bool_val = true } },
    .{ .name = "U128 greater than", .source = "100.U128 > 50.U128", .expected = .{ .bool_val = true } },

    // --- division and remainder on more int types ---
    .{ .name = "I32 truncating division", .source = "20.I32 // 3.I32", .expected = .{ .i32_val = 6 } },
    .{ .name = "I32 remainder", .source = "17.I32 % 5.I32", .expected = .{ .i32_val = 2 } },
    .{ .name = "U64 truncating division", .source = "100.U64 // 7.U64", .expected = .{ .u64_val = 14 } },
    .{ .name = "U64 remainder", .source = "100.U64 % 7.U64", .expected = .{ .u64_val = 2 } },
    .{ .name = "U32 truncating division", .source = "100.U32 // 3.U32", .expected = .{ .u32_val = 33 } },

    // --- to_str on typed ints (exercises render_helpers) ---
    .{ .name = "I32 to_str", .source = "42.I32.to_str()", .expected = .{ .str_val = "42" } },
    .{ .name = "U64 to_str", .source = "255.U64.to_str()", .expected = .{ .str_val = "255" } },
    .{ .name = "I8 to_str", .source = "42.I8.to_str()", .expected = .{ .str_val = "42" } },
    .{ .name = "U8 to_str", .source = "255.U8.to_str()", .expected = .{ .str_val = "255" } },
    .{ .name = "I16 to_str", .source = "1000.I16.to_str()", .expected = .{ .str_val = "1000" } },
    .{ .name = "F64 to_str", .source = "3.14.F64.to_str()", .expected = .{ .str_val = "3.14" } },
    .{ .name = "F32 to_str", .source = "1.5.F32.to_str()", .expected = .{ .str_val = "1.5" } },

    // --- list operations with typed elements ---
    // TODO: list of typed ints crashes across all backends
    .{
        .name = "list of I32 len",
        .source =
        \\{
        \\    xs = [1.I32, 2.I32, 3.I32]
        \\    xs.len().to_i64()
        \\}
        ,
        .expected = .{ .i64_val = 3 },
        .skip = SKIP_ALL,
    },
    .{
        .name = "list of U8 len",
        .source =
        \\{
        \\    xs = [10.U8, 20.U8, 30.U8]
        \\    xs.len().to_i64()
        \\}
        ,
        .expected = .{ .i64_val = 3 },
        .skip = SKIP_ALL,
    },

    // --- tag union with payload ---
    .{
        .name = "match Ok tag with int payload",
        .source =
        \\match Ok(42) {
        \\    Ok(n) => n
        \\    Err(_) => 0
        \\}
        ,
        .expected = .{ .dec_val = 42 * RocDec.one_point_zero_i128 },
    },
    .{
        .name = "match Err tag",
        .source =
        \\match Err("bad") {
        \\    Ok(_) => "good"
        \\    Err(msg) => msg
        \\}
        ,
        .expected = .{ .str_val = "bad" },
    },
    .{
        .name = "tag union with two-element payload",
        .source =
        \\match Pair(1, 2) {
        \\    Pair(a, b) => a + b
        \\    _ => 0
        \\}
        ,
        .expected = .{ .dec_val = 3 * RocDec.one_point_zero_i128 },
    },

    // --- F64 and F32 arithmetic ---
    .{ .name = "F64 addition", .source = "1.5.F64 + 2.5.F64", .expected = .{ .f64_val = 4.0 } },
    .{ .name = "F64 subtraction", .source = "10.0.F64 - 3.5.F64", .expected = .{ .f64_val = 6.5 } },
    .{ .name = "F64 multiplication", .source = "2.0.F64 * 3.0.F64", .expected = .{ .f64_val = 6.0 } },
    .{ .name = "F64 division", .source = "10.0.F64 / 4.0.F64", .expected = .{ .f64_val = 2.5 } },
    .{ .name = "F32 addition", .source = "1.5.F32 + 2.5.F32", .expected = .{ .f32_val = 4.0 } },
    .{ .name = "F32 multiplication", .source = "2.0.F32 * 3.0.F32", .expected = .{ .f32_val = 6.0 } },

    // --- F64/F32 comparisons ---
    .{ .name = "F64 greater than", .source = "3.14.F64 > 2.71.F64", .expected = .{ .bool_val = true } },
    // F64/F32 equality is intentionally unsupported — float == is a footgun (NaN, precision).
    // The type checker rejects it (F64 has no is_eq method), so this should produce a problem.
    .{ .name = "F64 equality is type error", .source = "1.0.F64 == 1.0.F64", .expected = .{ .problem = {} } },
    .{ .name = "F32 less than", .source = "1.0.F32 < 2.0.F32", .expected = .{ .bool_val = true } },

    // --- polymorphic functions with typed numerics (try to hit fallback numeric dispatch) ---
    .{
        .name = "closure returning I32 add",
        .source =
        \\{
        \\    id = |x| x
        \\    a = id(3.I32)
        \\    b = id(5.I32)
        \\    a + b
        \\}
        ,
        .expected = .{ .i32_val = 8 },
    },
    .{
        .name = "closure returning I64 comparison",
        .source =
        \\{
        \\    id = |x| x
        \\    a = id(10.I64)
        \\    b = id(5.I64)
        \\    a > b
        \\}
        ,
        .expected = .{ .bool_val = true },
    },
    .{
        .name = "I32 arithmetic through let binding chain",
        .source =
        \\{
        \\    x = 1.I32 + 2.I32
        \\    y = x * 3.I32
        \\    y + 1.I32
        \\}
        ,
        .expected = .{ .i32_val = 10 },
    },
    .{
        .name = "nested closure I64 subtraction",
        .source =
        \\{
        \\    apply = |f, x| f(x)
        \\    sub5 = |n| n - 5.I64
        \\    apply(sub5, 20.I64)
        \\}
        ,
        .expected = .{ .i64_val = 15 },
    },

    // --- more shift operations for wider coverage ---
    .{ .name = "shift right I64", .source = "{ 128.I64.shift_right_by(3.U8) }", .expected = .{ .i64_val = 16 } },
    .{ .name = "shift right U64", .source = "{ 256.U64.shift_right_by(4.U8) }", .expected = .{ .u64_val = 16 } },
    .{ .name = "shift right I32", .source = "{ 64.I32.shift_right_by(2.U8) }", .expected = .{ .i32_val = 16 } },
    .{ .name = "shift left I8", .source = "{ 1.I8.shift_left_by(3.U8) }", .expected = .{ .i8_val = 8 } },
    // TODO: I128/U128 shift crashes across all backends
    .{ .name = "shift left I128", .source = "{ 1.I128.shift_left_by(10.U8) }", .expected = .{ .i128_val = 1024 }, .skip = SKIP_ALL },
    .{ .name = "shift left U128", .source = "{ 1.U128.shift_left_by(16.U8) }", .expected = .{ .u128_val = 65536 }, .skip = SKIP_ALL },

    // --- negation on typed ints ---
    .{ .name = "I32 negation", .source = "{ -(5.I32) }", .expected = .{ .i32_val = -5 } },
    .{ .name = "I64 negation", .source = "{ -(10.I64) }", .expected = .{ .i64_val = -10 } },

    // --- F64 arithmetic through let bindings ---
    .{
        .name = "F64 arithmetic chain",
        .source =
        \\{
        \\    x = 10.0.F64 + 5.0.F64
        \\    y = x * 2.0.F64
        \\    y - 1.0.F64
        \\}
        ,
        .expected = .{ .f64_val = 29.0 },
    },

    // --- tag union with typed payload ---
    .{
        .name = "match custom tag returning I64",
        .source =
        \\match Val(42.I64) {
        \\    Val(n) => n
        \\    _ => 0.I64
        \\}
        ,
        .expected = .{ .i64_val = 42 },
    },
    .{
        .name = "match nested tags",
        .source =
        \\match Some(Ok(10)) {
        \\    Some(Ok(n)) => n
        \\    _ => 0
        \\}
        ,
        .expected = .{ .dec_val = 10 * RocDec.one_point_zero_i128 },
    },

    // --- Str operations for render_helpers coverage ---
    .{ .name = "Str.is_empty on empty", .source = "Str.is_empty(\"\")", .expected = .{ .bool_val = true } },
    .{ .name = "Str.is_empty on non-empty", .source = "Str.is_empty(\"hello\")", .expected = .{ .bool_val = false } },
    .{ .name = "Str.starts_with", .source = "Str.starts_with(\"hello world\", \"hello\")", .expected = .{ .bool_val = true } },
    .{ .name = "Str.ends_with", .source = "Str.ends_with(\"hello world\", \"world\")", .expected = .{ .bool_val = true } },
    .{ .name = "Str.count_utf8_bytes", .source = "Str.count_utf8_bytes(\"hello\")", .expected = .{ .u64_val = 5 } },
    .{ .name = "Str.trim leading and trailing", .source = "Str.trim(\"  hello  \")", .expected = .{ .str_val = "hello" } },
    .{ .name = "Str.trim_start", .source = "Str.trim_start(\"  hello\")", .expected = .{ .str_val = "hello" } },
    .{ .name = "Str.trim_end", .source = "Str.trim_end(\"hello  \")", .expected = .{ .str_val = "hello" } },

    // --- num from_str (Gap #7, #12, #13, #20) ---
    .{
        .name = "I64.from_str ok",
        .source =
        \\match I64.from_str("42") {
        \\    Ok(n) => n
        \\    Err(_) => 0.I64
        \\}
        ,
        .expected = .{ .i64_val = 42 },
    },
    .{
        .name = "I32.from_str ok",
        .source =
        \\match I32.from_str("100") {
        \\    Ok(n) => n
        \\    Err(_) => 0.I32
        \\}
        ,
        .expected = .{ .i32_val = 100 },
    },
    .{
        .name = "U64.from_str ok",
        .source =
        \\match U64.from_str("255") {
        \\    Ok(n) => n
        \\    Err(_) => 0.U64
        \\}
        ,
        .expected = .{ .u64_val = 255 },
    },
    .{
        .name = "I64.from_str bad input",
        .source =
        \\match I64.from_str("abc") {
        \\    Ok(_) => 1.I64
        \\    Err(_) => 0.I64
        \\}
        ,
        .expected = .{ .i64_val = 0 },
    },
    .{
        .name = "U8.from_str ok",
        .source =
        \\match U8.from_str("200") {
        \\    Ok(n) => n
        \\    Err(_) => 0.U8
        \\}
        ,
        .expected = .{ .u8_val = 200 },
    },
    .{
        .name = "I8.from_str negative",
        .source =
        \\match I8.from_str("-42") {
        \\    Ok(n) => n
        \\    Err(_) => 0.I8
        \\}
        ,
        .expected = .{ .i8_val = -42 },
    },
    .{
        .name = "F64.from_str ok",
        .source =
        \\match F64.from_str("3.14") {
        \\    Ok(n) => n
        \\    Err(_) => 0.0.F64
        \\}
        ,
        .expected = .{ .f64_val = 3.14 },
    },

    // --- more tag union patterns ---
    .{
        .name = "match with three tags",
        .source =
        \\match Red {
        \\    Red => "red"
        \\    Green => "green"
        \\    Blue => "blue"
        \\}
        ,
        .expected = .{ .str_val = "red" },
    },
    .{
        .name = "match enum green",
        .source =
        \\match Green {
        \\    Red => "red"
        \\    Green => "green"
        \\    Blue => "blue"
        \\}
        ,
        .expected = .{ .str_val = "green" },
    },
    .{
        .name = "match enum blue",
        .source =
        \\match Blue {
        \\    Red => "red"
        \\    Green => "green"
        \\    Blue => "blue"
        \\}
        ,
        .expected = .{ .str_val = "blue" },
    },

    // --- I8/I16 to_str for render_helpers coverage ---
    .{ .name = "I128 to_str", .source = "42.I128.to_str()", .expected = .{ .str_val = "42" } },
    .{ .name = "U128 to_str", .source = "42.U128.to_str()", .expected = .{ .str_val = "42" } },
    .{ .name = "U16 to_str", .source = "1000.U16.to_str()", .expected = .{ .str_val = "1000" } },
    .{ .name = "U32 to_str", .source = "1000.U32.to_str()", .expected = .{ .str_val = "1000" } },
    .{ .name = "I64 to_str", .source = "42.I64.to_str()", .expected = .{ .str_val = "42" } },

    // --- Num.abs on typed ints ---
    // TODO: dev backend returns wrong sign for abs
    .{ .name = "I8 abs positive", .source = "{ (-42.I8).abs() }", .expected = .{ .i8_val = 42 }, .skip = .{ .dev = true } },
    .{ .name = "I32 abs negative", .source = "{ (-100.I32).abs() }", .expected = .{ .i32_val = 100 }, .skip = .{ .dev = true } },
    .{ .name = "I64 abs negative", .source = "{ (-50.I64).abs() }", .expected = .{ .i64_val = 50 } },

    // --- Num.is_zero / is_positive / is_negative ---
    .{ .name = "I64 is_zero true", .source = "0.I64.is_zero()", .expected = .{ .bool_val = true } },
    .{ .name = "I64 is_zero false", .source = "5.I64.is_zero()", .expected = .{ .bool_val = false } },
    .{ .name = "I8 is_negative", .source = "(-1.I8).is_negative()", .expected = .{ .bool_val = true } },
    .{ .name = "I8 is_positive", .source = "5.I8.is_positive()", .expected = .{ .bool_val = true } },

    // --- record field access ---
    .{
        .name = "record field access",
        .source =
        \\{
        \\    rec = { x: 10, y: 20 }
        \\    rec.x + rec.y
        \\}
        ,
        .expected = .{ .dec_val = 30 * RocDec.one_point_zero_i128 },
    },
    .{
        .name = "record update syntax",
        .source =
        \\{
        \\    rec = { x: 10, y: 20 }
        \\    updated = { ..rec, x: 100 }
        \\    updated.x + updated.y
        \\}
        ,
        .expected = .{ .dec_val = 120 * RocDec.one_point_zero_i128 },
    },

    // --- tuple destructuring ---
    .{
        .name = "tuple access",
        .source =
        \\{
        \\    t = (10, 20)
        \\    t.0 + t.1
        \\}
        ,
        .expected = .{ .dec_val = 30 * RocDec.one_point_zero_i128 },
    },
    .{
        .name = "match tuple destructure",
        .source =
        \\match (3, 7) {
        \\    (a, b) => a + b
        \\}
        ,
        .expected = .{ .dec_val = 10 * RocDec.one_point_zero_i128 },
    },

    // --- for loop ---
    .{
        .name = "for loop summing I64",
        .source =
        \\{
        \\    var $sum = 0.I64
        \\    for item in [10.I64, 20.I64, 30.I64] {
        \\        $sum = $sum + item
        \\    }
        \\    $sum
        \\}
        ,
        .expected = .{ .i64_val = 60 },
    },

    // --- List.concat ---
    .{ .name = "List.concat with strings", .source = "List.concat([\"hello\", \"world\"], [\"foo\", \"bar\"]).len()", .expected = .{ .i64_val = 4 } },
    .{ .name = "List.concat with ints", .source = "List.concat([1, 2], [3, 4]).len()", .expected = .{ .i64_val = 4 } },
    .{ .name = "string list literal len", .source = "[\"hello\", \"world\"].len()", .expected = .{ .i64_val = 2 } },
    .{ .name = "string list concat simple", .source = "List.concat([\"a\"], [\"b\"]).len()", .expected = .{ .i64_val = 2 } },

    // --- Str operations ---
    .{ .name = "Str.concat", .source = "Str.concat(\"hello \", \"world\")", .expected = .{ .str_val = "hello world" } },
    .{ .name = "Str.repeat", .source = "Str.repeat(\"ab\", 3)", .expected = .{ .str_val = "ababab" } },
    // Str.contains hangs in wasm backend only (interpreter and dev pass)
    .{ .name = "Str.contains", .source = "Str.contains(\"hello world\", \"world\")", .expected = .{ .bool_val = true }, .skip = .{ .wasm = true } },
    .{ .name = "Str.contains false", .source = "Str.contains(\"hello world\", \"xyz\")", .expected = .{ .bool_val = false }, .skip = .{ .wasm = true } },
    .{ .name = "Str.to_utf8 len", .source = "Str.to_utf8(\"hi\").len()", .expected = .{ .u64_val = 2 } },
};
