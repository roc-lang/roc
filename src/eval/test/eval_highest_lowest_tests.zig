//! Highest/lowest numeric constant coverage ported to the inspect-only eval runner.

const TestCase = @import("parallel_runner.zig").TestCase;

/// Highest/lowest numeric constant and boundary parsing test cases.
pub const tests = [_]TestCase{
    .{
        .name = "highest_lowest: U8 boundaries",
        .source =
        \\{
        \\    U8.highest == 255
        \\        and U8.lowest == 0
        \\        and U8.from_str("255").is_ok()
        \\        and U8.from_str("256").is_err()
        \\        and U8.from_str("-1").is_err()
        \\}
        ,
        .expected = .{ .inspect_str = "True" },
    },
    .{
        .name = "highest_lowest: I8 boundaries",
        .source =
        \\{
        \\    I8.highest == 127
        \\        and I8.lowest == -128
        \\        and I8.from_str("127").is_ok()
        \\        and I8.from_str("128").is_err()
        \\        and I8.from_str("-128").is_ok()
        \\        and I8.from_str("-129").is_err()
        \\}
        ,
        .expected = .{ .inspect_str = "True" },
    },
    .{
        .name = "highest_lowest: U16 boundaries",
        .source =
        \\{
        \\    U16.highest == 65535
        \\        and U16.lowest == 0
        \\        and U16.from_str("65535").is_ok()
        \\        and U16.from_str("65536").is_err()
        \\        and U16.from_str("-1").is_err()
        \\}
        ,
        .expected = .{ .inspect_str = "True" },
    },
    .{
        .name = "highest_lowest: I16 boundaries",
        .source =
        \\{
        \\    I16.highest == 32767
        \\        and I16.lowest == -32768
        \\        and I16.from_str("32767").is_ok()
        \\        and I16.from_str("32768").is_err()
        \\        and I16.from_str("-32768").is_ok()
        \\        and I16.from_str("-32769").is_err()
        \\}
        ,
        .expected = .{ .inspect_str = "True" },
    },
    .{
        .name = "highest_lowest: U32 boundaries",
        .source =
        \\{
        \\    U32.highest == 4294967295
        \\        and U32.lowest == 0
        \\        and U32.from_str("4294967295").is_ok()
        \\        and U32.from_str("4294967296").is_err()
        \\        and U32.from_str("-1").is_err()
        \\}
        ,
        .expected = .{ .inspect_str = "True" },
    },
    .{
        .name = "highest_lowest: I32 boundaries",
        .source =
        \\{
        \\    I32.highest == 2147483647
        \\        and I32.lowest == -2147483648
        \\        and I32.from_str("2147483647").is_ok()
        \\        and I32.from_str("2147483648").is_err()
        \\        and I32.from_str("-2147483648").is_ok()
        \\        and I32.from_str("-2147483649").is_err()
        \\}
        ,
        .expected = .{ .inspect_str = "True" },
    },
    .{
        .name = "highest_lowest: U64 boundaries",
        .source =
        \\{
        \\    U64.highest == 18446744073709551615
        \\        and U64.lowest == 0
        \\        and U64.from_str("18446744073709551615").is_ok()
        \\        and U64.from_str("18446744073709551616").is_err()
        \\        and U64.from_str("-1").is_err()
        \\}
        ,
        .expected = .{ .inspect_str = "True" },
    },
    .{
        .name = "highest_lowest: I64 boundaries",
        .source =
        \\{
        \\    I64.highest == 9223372036854775807
        \\        and I64.lowest == -9223372036854775808
        \\        and I64.from_str("9223372036854775807").is_ok()
        \\        and I64.from_str("9223372036854775808").is_err()
        \\        and I64.from_str("-9223372036854775808").is_ok()
        \\        and I64.from_str("-9223372036854775809").is_err()
        \\}
        ,
        .expected = .{ .inspect_str = "True" },
    },
    .{
        .name = "highest_lowest: U128 boundaries",
        .source =
        \\{
        \\    U128.to_str(U128.highest) == "340282366920938463463374607431768211455"
        \\        and U128.lowest == 0
        \\        and U128.from_str("340282366920938463463374607431768211455").is_ok()
        \\        and U128.from_str("340282366920938463463374607431768211456").is_err()
        \\        and U128.from_str("-1").is_err()
        \\}
        ,
        .expected = .{ .inspect_str = "True" },
    },
    .{
        .name = "highest_lowest: I128 boundaries",
        .source =
        \\{
        \\    I128.to_str(I128.highest) == "170141183460469231731687303715884105727"
        \\        and I128.to_str(I128.lowest) == "-170141183460469231731687303715884105728"
        \\        and I128.from_str("170141183460469231731687303715884105727").is_ok()
        \\        and I128.from_str("170141183460469231731687303715884105728").is_err()
        \\        and I128.from_str("-170141183460469231731687303715884105728").is_ok()
        \\        and I128.from_str("-170141183460469231731687303715884105729").is_err()
        \\}
        ,
        .expected = .{ .inspect_str = "True" },
    },
    .{
        .name = "highest_lowest: Dec boundaries",
        .source =
        \\{
        \\    Dec.highest == 170141183460469231731.687303715884105727
        \\        and Dec.lowest == -170141183460469231731.687303715884105728
        \\        and Dec.from_str("170141183460469231731.687303715884105727").is_ok()
        \\        and Dec.from_str("170141183460469231731.687303715884105728").is_err()
        \\        and Dec.from_str("-170141183460469231731.687303715884105728").is_ok()
        \\        and Dec.from_str("-170141183460469231731.687303715884105729").is_err()
        \\}
        ,
        .expected = .{ .inspect_str = "True" },
    },
    .{
        .name = "highest_lowest: F32 boundaries",
        .source =
        \\{
        \\    parsed_highest = match F32.from_str("3.40282347e38") {
        \\        Ok(value) => F32.to_str(value) == F32.to_str(F32.highest)
        \\        Err(_) => False
        \\    }
        \\
        \\    parsed_lowest = match F32.from_str("-3.40282347e38") {
        \\        Ok(value) => F32.to_str(value) == F32.to_str(F32.lowest)
        \\        Err(_) => False
        \\    }
        \\
        \\    parsed_highest and parsed_lowest
        \\}
        ,
        .expected = .{ .inspect_str = "True" },
    },
    .{
        .name = "highest_lowest: F64 boundaries",
        .source =
        \\{
        \\    parsed_highest = match F64.from_str("1.7976931348623157e308") {
        \\        Ok(value) => F64.to_str(value) == F64.to_str(F64.highest)
        \\        Err(_) => False
        \\    }
        \\
        \\    parsed_lowest = match F64.from_str("-1.7976931348623157e308") {
        \\        Ok(value) => F64.to_str(value) == F64.to_str(F64.lowest)
        \\        Err(_) => False
        \\    }
        \\
        \\    parsed_highest and parsed_lowest
        \\}
        ,
        .expected = .{ .inspect_str = "True" },
    },
};
