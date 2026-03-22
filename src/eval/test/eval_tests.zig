//! Data-driven eval test definitions for the parallel test runner.
//! Each entry corresponds to one `runExpect*` call from the original test files.
//! Start with example tests covering each Expected variant to prove the concept;
//! more will be migrated later.

const TestCase = @import("parallel_runner.zig").TestCase;
const RocDec = @import("builtins").dec.RocDec;

pub const tests = [_]TestCase{
    .{ .name = "i64: simple number", .source = "1", .expected = .{ .i64_val = 1 } },
    .{ .name = "i64: if-else true branch", .source = "if (1 == 1) 42 else 99", .expected = .{ .i64_val = 42 } },
    .{ .name = "i64: arithmetic", .source = "2 + 3 * 4", .expected = .{ .i64_val = 14 } },
    .{ .name = "bool: true literal", .source = "True", .expected = .{ .bool_val = true } },
    .{ .name = "bool: comparison", .source = "5 > 3", .expected = .{ .bool_val = true } },
    .{ .name = "str: hello", .source = "\"hello\"", .expected = .{ .str_val = "hello" } },
    .{ .name = "dec: 1.5", .source = "1.5", .expected = .{ .dec_val = 1500000000000000000 } },
    .{ .name = "f32: literal", .source = "1.5.F32", .expected = .{ .f32_val = 1.5 } },
    .{ .name = "f64: literal", .source = "2.5.F64", .expected = .{ .f64_val = 2.5 } },
    .{ .name = "err: crash", .source = "{ crash \"test feature\" 0 }", .expected = .{ .err_val = error.Crash } },
    .{ .name = "problem: undefined variable", .source = "undefinedVar", .expected = .{ .problem = {} } },
};
