//! Ported low-level eval coverage from origin/main into the inspect-only runner.

const TestCase = @import("parallel_runner.zig").TestCase;

/// Public value `tests`.
pub const tests = [_]TestCase{
    .{
        .name = "low_level - F32.from_bits to_bits roundtrip finite",
        .source =
        \\{
        \\bits = 1069547520
        \\F32.to_bits(F32.from_bits(bits))
        \\}
        ,
        .expected = .{ .inspect_str = "1069547520" },
    },
    .{
        .name = "low_level - F32.from_bits to_bits preserves quiet NaN payload",
        .source =
        \\{
        \\bits = 2143289345
        \\F32.to_bits(F32.from_bits(bits)) == bits
        \\}
        ,
        .expected = .{ .inspect_str = "True" },
    },
    .{
        .name = "low_level - F64.from_bits to_bits roundtrip finite",
        .source =
        \\{
        \\bits = 4609434218613702656
        \\F64.to_bits(F64.from_bits(bits))
        \\}
        ,
        .expected = .{ .inspect_str = "4609434218613702656" },
    },
    .{
        .name = "low_level - F64.from_bits to_bits preserves quiet NaN payload",
        .source =
        \\{
        \\bits = 9221120237041090561
        \\F64.to_bits(F64.from_bits(bits)) == bits
        \\}
        ,
        .expected = .{ .inspect_str = "True" },
    },
    .{
        .name = "low_level - F32 constants expose expected bits",
        .source =
        \\{
        \\F32.to_bits(F32.e) == 1076754516
        \\    and F32.to_bits(F32.pi) == 1078530011
        \\    and F32.to_bits(F32.tau) == 1086918619
        \\}
        ,
        .expected = .{ .inspect_str = "True" },
    },
    .{
        .name = "low_level - F32 classification handles NaN infinity and finite values",
        .source =
        \\{
        \\F32.is_nan(F32.nan)
        \\    and !F32.is_float_eq(F32.nan, F32.nan)
        \\    and !F32.is_float_eq(1.0, F32.nan)
        \\    and F32.is_float_eq(-0.0, 0.0)
        \\    and F32.is_float_eq(F32.infinity, F32.infinity)
        \\    and F32.is_float_eq(F32.negate(F32.infinity), F32.negate(F32.infinity))
        \\    and !F32.is_float_eq(F32.infinity, F32.negate(F32.infinity))
        \\    and F32.is_infinite(F32.infinity)
        \\    and F32.is_infinite(F32.negate(F32.infinity))
        \\    and F32.is_finite(1.0)
        \\    and !F32.is_finite(F32.nan)
        \\    and !F32.is_finite(F32.infinity)
        \\}
        ,
        .expected = .{ .inspect_str = "True" },
    },
    .{
        .name = "low_level - F64 constants expose expected bits",
        .source =
        \\{
        \\F64.to_bits(F64.e) == 4613303445314885481
        \\    and F64.to_bits(F64.pi) == 4614256656552045848
        \\    and F64.to_bits(F64.tau) == 4618760256179416344
        \\}
        ,
        .expected = .{ .inspect_str = "True" },
    },
    .{
        .name = "low_level - F64 classification handles NaN infinity and finite values",
        .source =
        \\{
        \\F64.is_nan(F64.nan)
        \\    and !F64.is_float_eq(F64.nan, F64.nan)
        \\    and !F64.is_float_eq(1.0, F64.nan)
        \\    and F64.is_float_eq(-0.0, 0.0)
        \\    and F64.is_float_eq(F64.infinity, F64.infinity)
        \\    and F64.is_float_eq(F64.negate(F64.infinity), F64.negate(F64.infinity))
        \\    and !F64.is_float_eq(F64.infinity, F64.negate(F64.infinity))
        \\    and F64.is_infinite(F64.infinity)
        \\    and F64.is_infinite(F64.negate(F64.infinity))
        \\    and F64.is_finite(1.0)
        \\    and !F64.is_finite(F64.nan)
        \\    and !F64.is_finite(F64.infinity)
        \\}
        ,
        .expected = .{ .inspect_str = "True" },
    },
    .{
        .name = "low_level - F32 sqrt and sqrt_try",
        .source =
        \\{
        \\match F32.sqrt_try(9.0) {
        \\    Ok(value) => F32.is_float_eq(value, 3.0)
        \\    Err(_) => False
        \\}
        \\    and match F32.sqrt_try(-1.0) {
        \\        Ok(_) => False
        \\        Err(SqrtOfNegative) => True
        \\    }
        \\    and F32.to_str(F32.sqrt(2.25)) == "1.5"
        \\}
        ,
        .expected = .{ .inspect_str = "True" },
    },
    .{
        .name = "low_level - F64 sqrt and sqrt_try",
        .source =
        \\{
        \\match F64.sqrt_try(9.0) {
        \\    Ok(value) => F64.is_float_eq(value, 3.0)
        \\    Err(_) => False
        \\}
        \\    and match F64.sqrt_try(-1.0) {
        \\        Ok(_) => False
        \\        Err(SqrtOfNegative) => True
        \\    }
        \\    and F64.to_str(F64.sqrt(2.25)) == "1.5"
        \\}
        ,
        .expected = .{ .inspect_str = "True" },
    },
    .{
        .name = "low_level - F32 pow",
        .source =
        \\{
        \\F32.pow(2.0, 3.0).to_str() == "8"
        \\    and F32.pow(9.0, 0.5).to_str() == "3"
        \\}
        ,
        .expected = .{ .inspect_str = "True" },
    },
    .{
        .name = "low_level - F64 pow",
        .source =
        \\{
        \\F64.pow(2.0, 3.0).to_str() == "8"
        \\    and F64.pow(9.0, 0.5).to_str() == "3"
        \\}
        ,
        .expected = .{ .inspect_str = "True" },
    },
    .{
        .name = "low_level - F32 trig",
        .source =
        \\{
        \\F32.sin(0.0).to_str() == "0"
        \\    and F32.cos(0.0).to_str() == "1"
        \\    and F32.tan(0.0).to_str() == "0"
        \\    and F32.asin(0.0).to_str() == "0"
        \\    and F32.acos(1.0).to_str() == "0"
        \\    and F32.atan(0.0).to_str() == "0"
        \\}
        ,
        .expected = .{ .inspect_str = "True" },
    },
    .{
        .name = "low_level - F32 tan matrix",
        .source =
        \\{
        \\within = |actual, expected, tolerance| F32.abs_diff(actual, expected) <= tolerance
        \\within(F32.tan(0.2), 0.20271003, 0.000001)
        \\    and within(F32.tan(F32.negate(0.2)), F32.negate(0.20271003), 0.000001)
        \\    and within(F32.tan(0.8923), 1.2404218, 0.000001)
        \\    and within(F32.tan(1.5), 14.10142, 0.00001)
        \\    and within(F32.tan(37.45), F32.negate(0.25439608), 0.000001)
        \\    and within(F32.tan(89.123), 2.2858377, 0.00001)
        \\    and F32.is_nan(F32.tan(F32.infinity))
        \\    and F32.is_nan(F32.tan(F32.negate(F32.infinity)))
        \\}
        ,
        .expected = .{ .inspect_str = "True" },
    },
    .{
        .name = "low_level - F64 trig",
        .source =
        \\{
        \\F64.sin(0.0).to_str() == "0"
        \\    and F64.cos(0.0).to_str() == "1"
        \\    and F64.tan(0.0).to_str() == "0"
        \\    and F64.asin(0.0).to_str() == "0"
        \\    and F64.acos(1.0).to_str() == "0"
        \\    and F64.atan(0.0).to_str() == "0"
        \\}
        ,
        .expected = .{ .inspect_str = "True" },
    },
    .{
        .name = "low_level - F64 tan matrix",
        .source =
        \\{
        \\within = |actual, expected, tolerance| F64.abs_diff(actual, expected) <= tolerance
        \\within(F64.tan(0.2), 0.2027100355086725, 0.000000000000001)
        \\    and within(F64.tan(F64.negate(0.2)), F64.negate(0.2027100355086725), 0.000000000000001)
        \\    and within(F64.tan(0.8923), 1.2404217445497098, 0.000000000000001)
        \\    and within(F64.tan(1.5), 14.101419947171719, 0.00000000000001)
        \\    and within(F64.tan(37.45), F64.negate(0.25439607116885656), 0.000000000001)
        \\    and within(F64.tan(89.123), 2.285837625135532, 0.000000000001)
        \\    and F64.is_nan(F64.tan(F64.infinity))
        \\    and F64.is_nan(F64.tan(F64.negate(F64.infinity)))
        \\}
        ,
        .expected = .{ .inspect_str = "True" },
    },
    .{
        .name = "low_level - F32 rounding to integers",
        .source =
        \\{
        \\F32.round_to_i32(3.4) == 3
        \\    and F32.round_to_i32(-3.6) == -4
        \\    and F32.round_to_i32(2.5) == 3
        \\    and F32.round_to_i32(-2.5) == -3
        \\    and F32.floor_to_i32(-3.2) == -4
        \\    and F32.ceiling_to_u32(3.2) == 4
        \\}
        ,
        .expected = .{ .inspect_str = "True" },
    },
    .{
        .name = "low_level - F64 rounding to integers",
        .source =
        \\{
        \\F64.round_to_i32(3.4) == 3
        \\    and F64.round_to_i32(-3.6) == -4
        \\    and F64.round_to_i32(2.5) == 3
        \\    and F64.round_to_i32(-2.5) == -3
        \\    and F64.floor_to_i32(-3.2) == -4
        \\    and F64.ceiling_to_u32(3.2) == 4
        \\}
        ,
        .expected = .{ .inspect_str = "True" },
    },
    // Single float->int conversions, kept separate from the compound tests
    // above so a regression in one conversion isn't masked by `and`
    // short-circuiting. These pin the float-to-int wrapper ABI (the dev
    // backend must pass the float `val` through its CallBuilder so the
    // following integer args land in the right registers on Windows x64).
    .{
        .name = "low_level - F32 floor_to_i32 returns signed value",
        .source =
        \\F32.floor_to_i32(-3.2)
        ,
        .expected = .{ .inspect_str = "-4" },
    },
    .{
        .name = "low_level - F32 ceiling_to_u32 returns unsigned value",
        .source =
        \\F32.ceiling_to_u32(3.2)
        ,
        .expected = .{ .inspect_str = "4" },
    },
    .{
        .name = "low_level - F32 round_to_i32 returns signed value",
        .source =
        \\F32.round_to_i32(2.5)
        ,
        .expected = .{ .inspect_str = "3" },
    },
    .{
        .name = "low_level - F64 floor_to_i32 returns signed value",
        .source =
        \\F64.floor_to_i32(-3.2)
        ,
        .expected = .{ .inspect_str = "-4" },
    },
    .{
        .name = "low_level - F64 ceiling_to_u32 returns unsigned value",
        .source =
        \\F64.ceiling_to_u32(3.2)
        ,
        .expected = .{ .inspect_str = "4" },
    },
    .{
        .name = "low_level - F64 round_to_i32 returns signed value",
        .source =
        \\F64.round_to_i32(2.5)
        ,
        .expected = .{ .inspect_str = "3" },
    },
    .{
        .name = "low_level - Dec rounding to integers",
        .source =
        \\{
        \\Dec.round_to_i32(3.4) == 3
        \\    and Dec.round_to_i32(-3.6) == -4
        \\    and Dec.round_to_i32(2.5) == 3
        \\    and Dec.round_to_i32(-2.5) == -3
        \\    and Dec.floor_to_i32(-3.2) == -4
        \\    and Dec.ceiling_to_u32(3.2) == 4
        \\}
        ,
        .expected = .{ .inspect_str = "True" },
    },
    .{
        .name = "low_level - Str.is_empty returns True for empty string",
        .source =
        \\{
        \\x = Str.is_empty("")
        \\x
        \\}
        ,
        .expected = .{ .inspect_str = "True" },
    },
    .{
        .name = "low_level - Str.is_empty returns False for non-empty string",
        .source =
        \\{
        \\x = Str.is_empty("hello")
        \\x
        \\}
        ,
        .expected = .{ .inspect_str = "False" },
    },
    .{
        .name = "low_level - Str.is_empty in conditional",
        .source =
        \\{
        \\x = if True {
        \\    Str.is_empty("")
        \\} else {
        \\    False
        \\}
        \\x
        \\}
        ,
        .expected = .{ .inspect_str = "True" },
    },
    .{
        .name = "low_level - Str.concat with two non-empty strings",
        .source =
        \\{
        \\x = Str.concat("hello", "world")
        \\x
        \\}
        ,
        .expected = .{ .inspect_str = "\"helloworld\"" },
    },
    .{
        .name = "low_level - Str.concat with empty and non-empty string",
        .source =
        \\{
        \\x = Str.concat("", "test")
        \\x
        \\}
        ,
        .expected = .{ .inspect_str = "\"test\"" },
    },
    .{
        .name = "low_level - Str.concat with non-empty and empty string",
        .source =
        \\{
        \\x = Str.concat("test", "")
        \\x
        \\}
        ,
        .expected = .{ .inspect_str = "\"test\"" },
    },
    .{
        .name = "low_level - Str.concat with two empty strings",
        .source =
        \\{
        \\x = Str.concat("", "")
        \\x
        \\}
        ,
        .expected = .{ .inspect_str = "\"\"" },
    },
    .{
        .name = "low_level - Str.concat with special characters",
        .source =
        \\{
        \\x = Str.concat("hello ", "world!")
        \\x
        \\}
        ,
        .expected = .{ .inspect_str = "\"hello world!\"" },
    },
    .{
        .name = "low_level - Str.concat with longer strings",
        .source =
        \\{
        \\x = Str.concat("This is a longer string that contains about one hundred characters for testing concatenation.", " This is the second string that also has many characters in it for testing longer string operations.")
        \\x
        \\}
        ,
        .expected = .{ .inspect_str = "\"This is a longer string that contains about one hundred characters for testing concatenation. This is the second string that also has many characters in it for testing longer string operations.\"" },
    },
    .{
        .name = "low_level - Str.contains with substring in middle",
        .source =
        \\{
        \\x = Str.contains("foobarbaz", "bar")
        \\x
        \\}
        ,
        .expected = .{ .inspect_str = "True" },
    },
    .{
        .name = "low_level - Str.contains with non-matching strings",
        .source =
        \\{
        \\x = Str.contains("apple", "orange")
        \\x
        \\}
        ,
        .expected = .{ .inspect_str = "False" },
    },
    .{
        .name = "low_level - Str.contains with empty needle",
        .source =
        \\{
        \\x = Str.contains("anything", "")
        \\x
        \\}
        ,
        .expected = .{ .inspect_str = "True" },
    },
    .{
        .name = "low_level - Str.contains with substring at start",
        .source =
        \\{
        \\x = Str.contains("hello world", "hello")
        \\x
        \\}
        ,
        .expected = .{ .inspect_str = "True" },
    },
    .{
        .name = "low_level - Str.contains with substring at end",
        .source =
        \\{
        \\x = Str.contains("hello world", "world")
        \\x
        \\}
        ,
        .expected = .{ .inspect_str = "True" },
    },
    .{
        .name = "low_level - Str.contains with empty haystack",
        .source =
        \\{
        \\x = Str.contains("", "hello")
        \\x
        \\}
        ,
        .expected = .{ .inspect_str = "False" },
    },
    .{
        .name = "low_level - Str.contains with identical strings",
        .source =
        \\{
        \\x = Str.contains("test", "test")
        \\x
        \\}
        ,
        .expected = .{ .inspect_str = "True" },
    },
    .{
        .name = "low_level - Str.caseless_ascii_equals with equal strings",
        .source =
        \\{
        \\x = Str.caseless_ascii_equals("hello", "hello")
        \\x
        \\}
        ,
        .expected = .{ .inspect_str = "True" },
    },
    .{
        .name = "low_level - Str.caseless_ascii_equals with different case",
        .source =
        \\{
        \\x = Str.caseless_ascii_equals("hello", "HELLO")
        \\x
        \\}
        ,
        .expected = .{ .inspect_str = "True" },
    },
    .{
        .name = "low_level - Str.caseless_ascii_equals with different strings",
        .source =
        \\{
        \\x = Str.caseless_ascii_equals("hello", "world")
        \\x
        \\}
        ,
        .expected = .{ .inspect_str = "False" },
    },
    .{
        .name = "low_level - Str.caseless_ascii_equals with empty strings",
        .source =
        \\{
        \\x = Str.caseless_ascii_equals("", "")
        \\x
        \\}
        ,
        .expected = .{ .inspect_str = "True" },
    },
    .{
        .name = "low_level - Str.caseless_ascii_equals with empty and non-empty string",
        .source =
        \\{
        \\x = Str.caseless_ascii_equals("", "test")
        \\x
        \\}
        ,
        .expected = .{ .inspect_str = "False" },
    },
    .{
        .name = "low_level - Str.caseless_ascii_equals with longer strings",
        .source =
        \\{
        \\x = Str.caseless_ascii_equals("This is a longer string that contains about one hundred characters for testing purposes.", "THIS IS A LONGER STRING THAT CONTAINS ABOUT ONE HUNDRED CHARACTERS FOR TESTING purposes.")
        \\x
        \\}
        ,
        .expected = .{ .inspect_str = "True" },
    },
    .{
        .name = "low_level - Str.caseless_ascii_equals long and small strings",
        .source =
        \\{
        \\x = Str.caseless_ascii_equals("THIS IS A LONGER STRING THAT CONTAINS ABOUT ONE HUNDRED CHARACTERS FOR TESTING purposes.", "This")
        \\x
        \\}
        ,
        .expected = .{ .inspect_str = "False" },
    },
    .{
        .name = "low_level - Str.caseless_ascii_equals small and long strings",
        .source =
        \\{
        \\x = Str.caseless_ascii_equals("This", "THIS IS A LONGER STRING THAT CONTAINS ABOUT ONE HUNDRED CHARACTERS FOR TESTING purposes.")
        \\x
        \\}
        ,
        .expected = .{ .inspect_str = "False" },
    },
    .{
        .name = "low_level - Str.caseless_ascii_equals eq with non-ascii chars",
        .source =
        \\{
        \\x = Str.caseless_ascii_equals("COFFÉ", "coffÉ")
        \\x
        \\}
        ,
        .expected = .{ .inspect_str = "True" },
    },
    .{
        .name = "low_level - Str.caseless_ascii_equals non-ascii casing difference",
        .source =
        \\{
        \\x = Str.caseless_ascii_equals("coffé", "coffÉ")
        \\x
        \\}
        ,
        .expected = .{ .inspect_str = "False" },
    },
    .{
        .name = "low_level - Str.drop_prefix_caseless_ascii removes matching prefix",
        .source =
        \\{
        \\x = match Str.drop_prefix_caseless_ascii("Cache-Control: 0", "cache-control") {
        \\    Ok(rest) => rest
        \\    Err(_) => "missing"
        \\}
        \\x
        \\}
        ,
        .expected = .{ .inspect_str = "\": 0\"" },
    },
    .{
        .name = "low_level - Str.drop_prefix_caseless_ascii rejects punctuation case-bit pairs",
        .source =
        \\{
        \\x = match Str.drop_prefix_caseless_ascii("X_Auth: 0", "x\u(007F)auth") {
        \\    Ok(_) => False
        \\    Err(NotFound) => True
        \\}
        \\x
        \\}
        ,
        .expected = .{ .inspect_str = "True" },
    },
    .{
        .name = "low_level - Str.with_ascii_lowercased with mixed case",
        .source =
        \\{
        \\x = Str.with_ascii_lowercased("HeLLo")
        \\x
        \\}
        ,
        .expected = .{ .inspect_str = "\"hello\"" },
    },
    .{
        .name = "low_level - Str.with_ascii_lowercased with already lowercase",
        .source =
        \\{
        \\x = Str.with_ascii_lowercased("hello")
        \\x
        \\}
        ,
        .expected = .{ .inspect_str = "\"hello\"" },
    },
    .{
        .name = "low_level - Str.with_ascii_lowercased with empty string",
        .source =
        \\{
        \\x = Str.with_ascii_lowercased("")
        \\x
        \\}
        ,
        .expected = .{ .inspect_str = "\"\"" },
    },
    .{
        .name = "low_level - Str.with_ascii_lowercased with non-ascii chars",
        .source =
        \\{
        \\x = Str.with_ascii_lowercased("COFFÉ")
        \\x
        \\}
        ,
        .expected = .{ .inspect_str = "\"coffÉ\"" },
    },
    .{
        .name = "low_level - Str.with_ascii_uppercased with mixed case",
        .source =
        \\{
        \\x = Str.with_ascii_uppercased("HeLLo")
        \\x
        \\}
        ,
        .expected = .{ .inspect_str = "\"HELLO\"" },
    },
    .{
        .name = "low_level - Str.with_ascii_uppercased with already uppercase",
        .source =
        \\{
        \\x = Str.with_ascii_uppercased("HELLO")
        \\x
        \\}
        ,
        .expected = .{ .inspect_str = "\"HELLO\"" },
    },
    .{
        .name = "low_level - Str.with_ascii_uppercased with empty string",
        .source =
        \\{
        \\x = Str.with_ascii_uppercased("")
        \\x
        \\}
        ,
        .expected = .{ .inspect_str = "\"\"" },
    },
    .{
        .name = "low_level - Str.with_ascii_uppercased with non-ascii chars",
        .source =
        \\{
        \\x = Str.with_ascii_uppercased("coffÉ")
        \\x
        \\}
        ,
        .expected = .{ .inspect_str = "\"COFFÉ\"" },
    },
    .{
        .name = "low_level - Str.with_ascii_uppercased long text",
        .source =
        \\{
        \\x = Str.with_ascii_uppercased("coffÉ coffÉ coffÉ coffÉ coffÉ coffÉ coffÉ coffÉ coffÉ coffÉ coffÉ coffÉ coffÉ")
        \\x
        \\}
        ,
        .expected = .{ .inspect_str = "\"COFFÉ COFFÉ COFFÉ COFFÉ COFFÉ COFFÉ COFFÉ COFFÉ COFFÉ COFFÉ COFFÉ COFFÉ COFFÉ\"" },
    },
    .{
        .name = "low_level - Str.trim with an empty string",
        .source =
        \\{
        \\x = Str.trim("")
        \\x
        \\}
        ,
        .expected = .{ .inspect_str = "\"\"" },
    },
    .{
        .name = "low_level - Str.trim with a whitespace string",
        .source =
        \\{
        \\x = Str.trim("   ")
        \\x
        \\}
        ,
        .expected = .{ .inspect_str = "\"\"" },
    },
    .{
        .name = "low_level - Str.trim with a non-whitespace string",
        .source =
        \\{
        \\x = Str.trim("  hello  ")
        \\x
        \\}
        ,
        .expected = .{ .inspect_str = "\"hello\"" },
    },
    .{
        .name = "low_level - Str.trim_start with an empty string",
        .source =
        \\{
        \\x = Str.trim_start("")
        \\x
        \\}
        ,
        .expected = .{ .inspect_str = "\"\"" },
    },
    .{
        .name = "low_level - Str.trim_start with a whitespace string",
        .source =
        \\{
        \\x = Str.trim_start("   ")
        \\x
        \\}
        ,
        .expected = .{ .inspect_str = "\"\"" },
    },
    .{
        .name = "low_level - Str.trim_start with a non-whitespace string",
        .source =
        \\{
        \\x = Str.trim_start("  hello  ")
        \\x
        \\}
        ,
        .expected = .{ .inspect_str = "\"hello  \"" },
    },
    .{
        .name = "low_level - Str.trim_end with an empty string",
        .source =
        \\{
        \\x = Str.trim_end("")
        \\x
        \\}
        ,
        .expected = .{ .inspect_str = "\"\"" },
    },
    .{
        .name = "low_level - Str.trim_end with a whitespace string",
        .source =
        \\{
        \\x = Str.trim_end("   ")
        \\x
        \\}
        ,
        .expected = .{ .inspect_str = "\"\"" },
    },
    .{
        .name = "low_level - Str.trim_end with a non-whitespace string",
        .source =
        \\{
        \\x = Str.trim_end("  hello  ")
        \\x
        \\}
        ,
        .expected = .{ .inspect_str = "\"  hello\"" },
    },
    .{
        .name = "low_level - List.concat with two non-empty lists",
        .source =
        \\{
        \\x = List.concat([1, 2], [3, 4])
        \\len = List.len(x)
        \\len
        \\}
        ,
        .expected = .{ .inspect_str = "4" },
    },
    .{
        .name = "low_level - List.concat with empty and non-empty list",
        .source =
        \\{
        \\x = List.concat([], [1, 2, 3])
        \\len = List.len(x)
        \\len
        \\}
        ,
        .expected = .{ .inspect_str = "3" },
    },
    .{
        .name = "low_level - List.concat with two empty lists",
        .source =
        \\{
        \\x : List(U64)
        \\x = List.concat([], [])
        \\len = List.len(x)
        \\len
        \\}
        ,
        .expected = .{ .inspect_str = "0" },
    },
    .{
        .name = "low_level - List.concat preserves order",
        .source =
        \\{
        \\x = List.concat([10, 20], [30, 40, 50])
        \\first = List.first(x)
        \\first
        \\}
        ,
        .expected = .{ .inspect_str = "Ok(10.0)" },
    },
    .{
        .name = "low_level - List.concat with Str.to_utf8 inside lambda (issue 8618)",
        .source =
        \\{
        \\test = |line| {
        \\    bytes = line.to_utf8()
        \\    List.concat([0], bytes)
        \\}
        \\
        \\x = test("abc")
        \\x
        \\}
        ,
        .expected = .{ .inspect_str = "[0, 97, 98, 99]" },
    },
    .{
        .name = "top-level List.concat with Str.to_utf8 (value restriction check)",
        .source =
        \\{
        \\line = "abc"
        \\result = line.to_utf8().concat([0])
        \\result
        \\}
        ,
        .expected = .{ .inspect_str = "[97, 98, 99, 0]" },
    },
    .{
        .name = "low_level - List.concat with strings (refcounted elements)",
        .source =
        \\{
        \\x = List.concat(["hello", "world"], ["foo", "bar"])
        \\len = List.len(x)
        \\len
        \\}
        ,
        .expected = .{ .inspect_str = "4" },
    },
    .{
        .name = "low_level - List.concat with nested lists (refcounted elements)",
        .source =
        \\{
        \\x = List.concat([[1, 2], [3]], [[4, 5, 6]])
        \\len = List.len(x)
        \\len
        \\}
        ,
        .expected = .{ .inspect_str = "3" },
    },
    .{
        .name = "low_level - List.concat preserves reused input list after consuming call",
        .source =
        \\{
        \\repeat_helper = |acc, list, n| match n {
        \\    0 => acc
        \\    _ => repeat_helper(List.concat(acc, list), list, n - 1)
        \\}
        \\
        \\repeat = |list, n| repeat_helper([], list, n)
        \\
        \\result = repeat([1, 2], 3)
        \\result == [1, 2, 1, 2, 1, 2]
        \\}
        ,
        .expected = .{ .inspect_str = "True" },
    },
    .{
        .name = "low_level - parse range with split_on and question",
        .source =
        \\{
        \\parse_range = |range_str| {
        \\    match range_str.split_on("-") {
        \\        [a, b] => Ok((I64.from_str(a)?, I64.from_str(b)?))
        \\        _ => Err(InvalidRangeFormat)
        \\    }
        \\}
        \\
        \\match parse_range("11-22") {
        \\    Ok((start, end)) => start + end == 33
        \\    Err(_) => False
        \\}
        \\}
        ,
        .expected = .{ .inspect_str = "True" },
    },
    .{
        .name = "low_level - repeating byte pattern detects duplicated digits",
        .source =
        \\{
        \\repeat_helper = |acc, list, n| match n {
        \\    0 => acc
        \\    _ => repeat_helper(acc.concat(list), list, n - 1)
        \\}
        \\
        \\repeat = |list, n| repeat_helper([], list, n)
        \\
        \\has_repeating_pattern : I64 -> Bool
        \\has_repeating_pattern = |x| {
        \\    s = x.to_str().to_utf8()
        \\    n = s.len()
        \\
        \\    var $d = 1
        \\    while $d <= n // 2 {
        \\        if n % $d == 0 {
        \\            slice = s.sublist({ start: 0, len: $d })
        \\            repeated = slice->repeat(n // $d)
        \\            if repeated == s { return True }
        \\        }
        \\        $d = $d + 1
        \\    }
        \\
        \\    False
        \\}
        \\
        \\if has_repeating_pattern(12) { False } else { has_repeating_pattern(11) }
        \\}
        ,
        .expected = .{ .inspect_str = "True" },
    },
    .{
        .name = "low_level - repeat helper concatenates byte lists",
        .source =
        \\{
        \\repeat_helper = |acc, list, n| match n {
        \\    0 => acc
        \\    _ => repeat_helper(acc.concat(list), list, n - 1)
        \\}
        \\
        \\repeat = |list, n| repeat_helper([], list, n)
        \\
        \\repeat([49], 2) == [49, 49]
        \\}
        ,
        .expected = .{ .inspect_str = "True" },
    },
    .{
        .name = "low_level - for loop parses split ranges through question",
        .source =
        \\{
        \\parse_range = |range_str| {
        \\    match range_str.split_on("-") {
        \\        [a, b] => Ok((I64.from_str(a)?, I64.from_str(b)?))
        \\        _ => Err(InvalidRangeFormat)
        \\    }
        \\}
        \\
        \\part2 = |input| {
        \\    var $sum = 0
        \\
        \\    for range_str in input.trim().split_on(",") {
        \\        (start, end) = parse_range(range_str)?
        \\        $sum = $sum + start + end
        \\    }
        \\
        \\    Ok($sum)
        \\}
        \\
        \\match part2("11-22") {
        \\    Ok(sum) => sum == 33
        \\    Err(_) => False
        \\}
        \\}
        ,
        .expected = .{ .inspect_str = "True" },
    },
    .{
        .name = "low_level - for loop parses literal ranges through question",
        .source =
        \\{
        \\parse_range = |range_str| {
        \\    match range_str.split_on("-") {
        \\        [a, b] => Ok((I64.from_str(a)?, I64.from_str(b)?))
        \\        _ => Err(InvalidRangeFormat)
        \\    }
        \\}
        \\
        \\part2 = |ranges| {
        \\    var $sum = 0
        \\
        \\    for range_str in ranges {
        \\        (start, end) = parse_range(range_str)?
        \\        $sum = $sum + start + end
        \\    }
        \\
        \\    Ok($sum)
        \\}
        \\
        \\match part2(["11-22"]) {
        \\    Ok(sum) => sum == 33
        \\    Err(_) => False
        \\}
        \\}
        ,
        .expected = .{ .inspect_str = "True" },
    },
    .{
        .name = "low_level - for loop propagates question over ok value",
        .source =
        \\{
        \\sum_oks = |numbers| {
        \\    var $sum = 0
        \\
        \\    for n in numbers {
        \\        value = Ok(n)?
        \\        $sum = $sum + value
        \\    }
        \\
        \\    Ok($sum)
        \\}
        \\
        \\match sum_oks([11, 22]) {
        \\    Ok(sum) => sum == 33
        \\    Err(_) => False
        \\}
        \\}
        ,
        .expected = .{ .inspect_str = "True" },
    },
    .{
        .name = "low_level - for loop destructures tuple from question",
        .source =
        \\{
        \\pair = |n| Ok((n, n))
        \\
        \\sum_pairs = |numbers| {
        \\    var $sum = 0
        \\
        \\    for n in numbers {
        \\        (a, b) = pair(n)?
        \\        $sum = $sum + a + b
        \\    }
        \\
        \\    Ok($sum)
        \\}
        \\
        \\match sum_pairs([11]) {
        \\    Ok(sum) => sum == 22
        \\    Err(_) => False
        \\}
        \\}
        ,
        .expected = .{ .inspect_str = "True" },
    },
    .{
        .name = "low_level - for loop question through split tuple parser",
        .source =
        \\{
        \\parse_pair = |range_str| {
        \\    match range_str.split_on("-") {
        \\        [a, b] => Ok((a, b))
        \\        _ => Err(InvalidRangeFormat)
        \\    }
        \\}
        \\
        \\part2 = |ranges| {
        \\    var $sum = 0
        \\
        \\    for range_str in ranges {
        \\        (start, end) = parse_pair(range_str)?
        \\        $sum = $sum + start.to_utf8().len() + end.to_utf8().len()
        \\    }
        \\
        \\    Ok($sum)
        \\}
        \\
        \\match part2(["11-22"]) {
        \\    Ok(sum) => sum == 4
        \\    Err(_) => False
        \\}
        \\}
        ,
        .expected = .{ .inspect_str = "True" },
    },
    .{
        .name = "low_level - repeating pattern parser returns accumulated sum",
        .source =
        \\{
        \\parse_range = |range_str| {
        \\    match range_str.split_on("-") {
        \\        [a, b] => Ok((I64.from_str(a)?, I64.from_str(b)?))
        \\        _ => Err(InvalidRangeFormat)
        \\    }
        \\}
        \\
        \\repeat_helper = |acc, list, n| match n {
        \\    0 => acc
        \\    _ => repeat_helper(acc.concat(list), list, n - 1)
        \\}
        \\
        \\repeat = |list, n| repeat_helper([], list, n)
        \\
        \\has_repeating_pattern : I64 -> Bool
        \\has_repeating_pattern = |x| {
        \\    s = x.to_str().to_utf8()
        \\    n = s.len()
        \\
        \\    var $d = 1
        \\    while $d <= n // 2 {
        \\        if n % $d == 0 {
        \\            slice = s.sublist({ start: 0, len: $d })
        \\            repeated = slice->repeat(n // $d)
        \\            if repeated == s { return True }
        \\        }
        \\        $d = $d + 1
        \\    }
        \\
        \\    False
        \\}
        \\
        \\part2 = |input| {
        \\    var $sum = 0
        \\
        \\    for range_str in input.trim().split_on(",") {
        \\        (start, end) = parse_range(range_str)?
        \\
        \\        var $x = start
        \\        while $x <= end {
        \\            if has_repeating_pattern($x) {
        \\                $sum = $sum + $x
        \\            }
        \\            $x = $x + 1
        \\        }
        \\    }
        \\
        \\    Ok($sum)
        \\}
        \\
        \\match part2("11-22") {
        \\    Ok(sum) => sum == 33
        \\    Err(_) => False
        \\}
        \\}
        ,
        .expected = .{ .inspect_str = "True" },
    },
    .{
        .name = "low_level - List.concat with empty string list",
        .source =
        \\{
        \\x = List.concat([], ["a", "b", "c"])
        \\len = List.len(x)
        \\len
        \\}
        ,
        .expected = .{ .inspect_str = "3" },
    },
    .{
        .name = "low_level - List.concat with zero-sized type",
        .source =
        \\{
        \\x : List({})
        \\x = List.concat([{}, {}], [{}, {}, {}])
        \\len = List.len(x)
        \\len
        \\}
        ,
        .expected = .{ .inspect_str = "5" },
    },
    .{
        .name = "low_level - List.with_capacity of non refcounted elements creates empty list",
        .source =
        \\{
        \\x : List(U64)
        \\x = List.with_capacity(10)
        \\len = List.len(x)
        \\len
        \\}
        ,
        .expected = .{ .inspect_str = "0" },
    },
    .{
        .name = "low_level - List.with_capacity of str (refcounted elements) creates empty list",
        .source =
        \\{
        \\x : List(Str)
        \\x = List.with_capacity(10)
        \\len = List.len(x)
        \\len
        \\}
        ,
        .expected = .{ .inspect_str = "0" },
    },
    .{
        .name = "low_level - List.with_capacity of non refcounted elements can concat",
        .source =
        \\{
        \\y : List(U64)
        \\y = List.with_capacity(10)
        \\x = List.concat(y, [1])
        \\len = List.len(x)
        \\len
        \\}
        ,
        .expected = .{ .inspect_str = "1" },
    },
    .{
        .name = "low_level - List.with_capacity of str (refcounted elements) can concat",
        .source =
        \\{
        \\y : List(Str)
        \\y = List.with_capacity(10)
        \\x = List.concat(y, ["hello", "world"])
        \\len = List.len(x)
        \\len
        \\}
        ,
        .expected = .{ .inspect_str = "2" },
    },
    .{
        .name = "low_level - List.with_capacity without capacity, of str (refcounted elements) can concat",
        .source =
        \\{
        \\y : List(Str)
        \\y = List.with_capacity(0)
        \\x = List.concat(y, ["hello", "world"])
        \\len = List.len(x)
        \\len
        \\}
        ,
        .expected = .{ .inspect_str = "2" },
    },
    .{
        .name = "low_level - List.with_capacity of zero-sized type creates empty list",
        .source =
        \\{
        \\x : List({})
        \\x = List.with_capacity(10)
        \\len = List.len(x)
        \\len
        \\}
        ,
        .expected = .{ .inspect_str = "0" },
    },
    .{
        .name = "low_level - List.append on non-empty list",
        .source =
        \\{
        \\x = List.append([0, 1, 2, 3], 4)
        \\len = List.len(x)
        \\len
        \\}
        ,
        .expected = .{ .inspect_str = "5" },
    },
    .{
        .name = "low_level - List.reserve is public and preserves contents",
        .source =
        \\{
        \\x = List.reserve([0, 1, 2], 4)
        \\tail = List.append(x, 3)
        \\List.get(tail, 3)
        \\}
        ,
        .expected = .{ .inspect_str = "Ok(3.0)" },
    },
    .{
        .name = "low_level - List.reserve supports repeated append",
        .source =
        \\{
        \\reserved = List.reserve([], 3)
        \\one = List.append(reserved, 1)
        \\two = List.append(one, 2)
        \\three = List.append(two, 3)
        \\List.len(three)
        \\}
        ,
        .expected = .{ .inspect_str = "3" },
    },
    .{
        .name = "low_level - List.release_excess_capacity is public",
        .source =
        \\{
        \\reserved = List.reserve([1, 2], 8)
        \\trimmed = List.release_excess_capacity(reserved)
        \\List.len(trimmed)
        \\}
        ,
        .expected = .{ .inspect_str = "2" },
    },
    .{
        .name = "low_level - List.append on empty list",
        .source =
        \\{
        \\x = List.append([], 0)
        \\got = List.get(x, 0)
        \\got
        \\}
        ,
        .expected = .{ .inspect_str = "Ok(0.0)" },
    },
    .{
        .name = "low_level - List.append a list on empty list",
        .source =
        \\{
        \\x = List.append([], [])
        \\len = List.len(x)
        \\got = List.get(x, 0)
        \\len
        \\}
        ,
        .expected = .{ .inspect_str = "1" },
    },
    .{
        .name = "low_level - List.get composite list element",
        .source =
        \\{
        \\got = List.get([[]], 0)
        \\got
        \\}
        ,
        .expected = .{ .inspect_str = "Ok([])" },
    },
    .{
        .name = "low_level - List.reserve composite empty list",
        .source =
        \\{
        \\x = List.reserve([], 1)
        \\List.len(x)
        \\}
        ,
        .expected = .{ .inspect_str = "0" },
    },
    .{
        .name = "low_level - List.append composite empty list without get",
        .source =
        \\{
        \\x = List.append([], [])
        \\List.len(x)
        \\}
        ,
        .expected = .{ .inspect_str = "1" },
    },
    .{
        .name = "low_level - List.append for strings",
        .source =
        \\{
        \\x = List.append(["cat", "chases"], "rat")
        \\len = List.len(x)
        \\got = List.get(x, 2)
        \\len
        \\}
        ,
        .expected = .{ .inspect_str = "3" },
    },
    .{
        .name = "low_level - List.append for list of lists",
        .source =
        \\{
        \\x = List.append([[0, 1], [2, 3, 4], [5, 6, 7]], [8,9])
        \\len = List.len(x)
        \\len
        \\}
        ,
        .expected = .{ .inspect_str = "4" },
    },
    .{
        .name = "low_level - List.append for list of tuples",
        .source =
        \\{
        \\x = List.append([(-1, 0, 1), (2, 3, 4), (5, 6, 7)], (-2, -3, -4))
        \\len = List.len(x)
        \\len
        \\}
        ,
        .expected = .{ .inspect_str = "4" },
    },
    .{
        .name = "low_level - List.append for list of records",
        .source =
        \\{
        \\x = List.append([{x:"1", y: "1"}, {x: "2", y: "4"}, {x: "5", y: "7"}], {x: "2", y: "4"})
        \\len = List.len(x)
        \\tail = match List.get(x, 3) { Ok(rec) => rec.x, _ => "wrong"}
        \\len
        \\}
        ,
        .expected = .{ .inspect_str = "4" },
    },
    .{
        .name = "low_level - List.append for already refcounted elt",
        .source =
        \\{
        \\new = [8, 9]
        \\w = [new, new, new, [10, 11]]
        \\x = List.append([[0, 1], [2, 3, 4], [5, 6, 7]], new)
        \\len = List.len(x)
        \\len
        \\}
        ,
        .expected = .{ .inspect_str = "4" },
    },
    .{
        .name = "low_level - List.append for list of tuples with strings (issue 8650)",
        .source =
        \\{
        \\x = List.append([("a", "b")], ("hello", "world"))
        \\len = List.len(x)
        \\len
        \\}
        ,
        .expected = .{ .inspect_str = "2" },
    },
    .{
        .name = "low_level - List.append tuple to empty list (issue 8758)",
        .source =
        \\{
        \\x = List.append([], ("hello", "world"))
        \\len = List.len(x)
        \\len
        \\}
        ,
        .expected = .{ .inspect_str = "1" },
    },
    .{
        .name = "low_level - List.drop_at on an empty list at index 0",
        .source =
        \\{
        \\x = List.drop_at([], 0)
        \\len = List.len(x)
        \\len
        \\}
        ,
        .expected = .{ .inspect_str = "0" },
    },
    .{
        .name = "low_level - List.drop_at on an empty list at index >0",
        .source =
        \\{
        \\x = List.drop_at([], 10)
        \\len = List.len(x)
        \\len
        \\}
        ,
        .expected = .{ .inspect_str = "0" },
    },
    .{
        .name = "low_level - List.drop_at on non-empty list",
        .source =
        \\{
        \\x = List.drop_at([1, 2, 3], 0)
        \\len = List.len(x)
        \\first = List.get(x, 0)
        \\len
        \\}
        ,
        .expected = .{ .inspect_str = "2" },
    },
    .{
        .name = "low_level - List.drop_at out of bounds on non-empty list",
        .source =
        \\{
        \\x = List.drop_at([1, 2, 3, 4, 5], 10)
        \\len = List.len(x)
        \\len
        \\}
        ,
        .expected = .{ .inspect_str = "5" },
    },
    .{
        .name = "low_level - List.drop_at on refcounted List(Str)",
        .source =
        \\{
        \\x = List.drop_at(["cat", "chases", "rat"], 1)
        \\len = List.len(x)
        \\second = List.get(x, 1)
        \\len
        \\}
        ,
        .expected = .{ .inspect_str = "2" },
    },
    .{
        .name = "low_level - List.drop_at on refcounted List(List(Str))",
        .source =
        \\{
        \\x = List.drop_at([["two", "words"], [], ["a", "four", "word", "list"]], 1)
        \\len = List.len(x)
        \\second = Try.ok_or(List.get(x, 1), [])
        \\elt_len =  List.len(second)
        \\len
        \\}
        ,
        .expected = .{ .inspect_str = "2" },
    },
    .{
        .name = "low_level - List.sublist on empty list",
        .source =
        \\{
        \\x = List.sublist([], {start: 0, len: 10})
        \\len = List.len(x)
        \\len
        \\}
        ,
        .expected = .{ .inspect_str = "0" },
    },
    .{
        .name = "low_level - List.sublist on non-empty list",
        .source =
        \\{
        \\x = List.sublist([0, 1, 2, 3, 4], {start: 1, len: 3})
        \\len = List.len(x)
        \\slice_start = List.get(x, 0)
        \\slice_end = List.get(x, 2)
        \\len
        \\}
        ,
        .expected = .{ .inspect_str = "3" },
    },
    .{
        .name = "low_level - List.sublist start out of bounds",
        .source =
        \\{
        \\x = List.sublist([0, 1, 2, 3, 4], {start: 100, len: 3})
        \\len = List.len(x)
        \\len
        \\}
        ,
        .expected = .{ .inspect_str = "0" },
    },
    .{
        .name = "low_level - List.sublist requesting beyond end of list gives you input list",
        .source =
        \\{
        \\x = List.sublist([0, 1, 2, 3, 4], {start: 0, len: 10000})
        \\len = List.len(x)
        \\len
        \\}
        ,
        .expected = .{ .inspect_str = "5" },
    },
    .{
        .name = "low_level - U8.from_str parses explicit unsigned width",
        .source =
        \\{
        \\match U8.from_str("42") {
        \\    Ok(value) => value
        \\    Err(_) => 0.U8
        \\}
        \\}
        ,
        .expected = .{ .inspect_str = "42" },
    },
    .{
        .name = "low_level - U128.from_str parses explicit 128-bit integer",
        .source =
        \\{
        \\match U128.from_str("340282366920938463463374607431768211455") {
        \\    Ok(value) => U128.to_str(value)
        \\    Err(_) => "bad"
        \\}
        \\}
        ,
        .expected = .{ .inspect_str = "\"340282366920938463463374607431768211455\"" },
    },
    .{
        .name = "low_level - F32.from_str parses explicit float width",
        .source =
        \\{
        \\match F32.from_str("3.5") {
        \\    Ok(value) => F32.to_str(value)
        \\    Err(_) => "bad"
        \\}
        \\}
        ,
        .expected = .{ .inspect_str = "\"3.5\"" },
    },
    .{
        .name = "low_level - Dec.from_str parses explicit decimal",
        .source =
        \\{
        \\match Dec.from_str("12.5") {
        \\    Ok(value) => Dec.to_str(value)
        \\    Err(_) => "bad"
        \\}
        \\}
        ,
        .expected = .{ .inspect_str = "\"12.5\"" },
    },
    .{
        .name = "low_level - I64.from_str preserves explicit error path",
        .source =
        \\{
        \\match I64.from_str("nope") {
        \\    Ok(_) => "wrong"
        \\    Err(_) => "err"
        \\}
        \\}
        ,
        .expected = .{ .inspect_str = "\"err\"" },
    },
    .{
        .name = "low_level - Dec.to_str returns string representation of decimal",
        .source =
        \\{
        \\a : Dec
        \\a = 123.45.Dec
        \\x = Dec.to_str(a)
        \\x
        \\}
        ,
        .expected = .{ .inspect_str = "\"123.45\"" },
    },
    .{
        .name = "low_level - Dec.to_str with negative decimal",
        .source =
        \\{
        \\a : Dec
        \\a = -456.78.Dec
        \\x = Dec.to_str(a)
        \\x
        \\}
        ,
        .expected = .{ .inspect_str = "\"-456.78\"" },
    },
    .{
        .name = "low_level - Dec.to_str with zero",
        .source =
        \\{
        \\a : Dec
        \\a = 0.0.Dec
        \\x = Dec.to_str(a)
        \\x
        \\}
        ,
        .expected = .{ .inspect_str = "\"0.0\"" },
    },
    .{
        .name = "low_level - U8.to_str",
        .source =
        \\{
        \\a : U8
        \\a = 42.U8
        \\x = U8.to_str(a)
        \\x
        \\}
        ,
        .expected = .{ .inspect_str = "\"42\"" },
    },
    .{
        .name = "low_level - I8.to_str with negative",
        .source =
        \\{
        \\a : I8
        \\a = -42.I8
        \\x = I8.to_str(a)
        \\x
        \\}
        ,
        .expected = .{ .inspect_str = "\"-42\"" },
    },
    .{
        .name = "low_level - I8.abs_diff uses signed operand layout",
        .source =
        \\{
        \\a : I8
        \\a = 120.I8
        \\b : I8
        \\b = -120.I8
        \\x = I8.abs_diff(a, b)
        \\x
        \\}
        ,
        .expected = .{ .inspect_str = "240" },
    },
    .{
        .name = "low_level - U16.to_str",
        .source =
        \\{
        \\a : U16
        \\a = 1000.U16
        \\x = U16.to_str(a)
        \\x
        \\}
        ,
        .expected = .{ .inspect_str = "\"1000\"" },
    },
    .{
        .name = "low_level - I16.to_str with negative",
        .source =
        \\{
        \\a : I16
        \\a = -500.I16
        \\x = I16.to_str(a)
        \\x
        \\}
        ,
        .expected = .{ .inspect_str = "\"-500\"" },
    },
    .{
        .name = "low_level - U32.to_str",
        .source =
        \\{
        \\a : U32
        \\a = 100000.U32
        \\x = U32.to_str(a)
        \\x
        \\}
        ,
        .expected = .{ .inspect_str = "\"100000\"" },
    },
    .{
        .name = "low_level - I32.to_str with negative",
        .source =
        \\{
        \\a : I32
        \\a = -12345.I32
        \\x = I32.to_str(a)
        \\x
        \\}
        ,
        .expected = .{ .inspect_str = "\"-12345\"" },
    },
    .{
        .name = "low_level - U64.to_str",
        .source =
        \\{
        \\a : U64
        \\a = 9876543210.U64
        \\x = U64.to_str(a)
        \\x
        \\}
        ,
        .expected = .{ .inspect_str = "\"9876543210\"" },
    },
    .{
        .name = "low_level - I64.to_str with negative",
        .source =
        \\{
        \\a : I64
        \\a = -9876543210.I64
        \\x = I64.to_str(a)
        \\x
        \\}
        ,
        .expected = .{ .inspect_str = "\"-9876543210\"" },
    },
    .{
        .name = "low_level - U128.to_str",
        .source =
        \\{
        \\a : U128
        \\a = 12345678901234567890.U128
        \\x = U128.to_str(a)
        \\x
        \\}
        ,
        .expected = .{ .inspect_str = "\"12345678901234567890\"" },
    },
    .{
        .name = "low_level - I128.to_str with negative",
        .source =
        \\{
        \\a : I128
        \\a = -12345678901234567890.I128
        \\x = I128.to_str(a)
        \\x
        \\}
        ,
        .expected = .{ .inspect_str = "\"-12345678901234567890\"" },
    },
    .{
        .name = "low_level - F32.to_str",
        .source =
        \\{
        \\a : F32
        \\a = 3.14.F32
        \\x = F32.to_str(a)
        \\x
        \\}
        ,
        .expected = .{ .inspect_str = "\"3.14\"" },
    },
    .{
        .name = "low_level - F64.to_str",
        .source =
        \\{
        \\a : F64
        \\a = 3.14159265359.F64
        \\x = F64.to_str(a)
        \\x
        \\}
        ,
        .expected = .{ .inspect_str = "\"3.14159265359\"" },
    },
    .{
        .name = "low_level - F32.to_str with negative",
        .source =
        \\{
        \\a : F32
        \\a = -2.5.F32
        \\x = F32.to_str(a)
        \\x
        \\}
        ,
        .expected = .{ .inspect_str = "\"-2.5\"" },
    },
    .{
        .name = "low_level - F64.to_str with negative",
        .source =
        \\{
        \\a : F64
        \\a = -123.456.F64
        \\x = F64.to_str(a)
        \\x
        \\}
        ,
        .expected = .{ .inspect_str = "\"-123.456\"" },
    },
    .{
        .name = "low_level - Str.starts_with returns True for matching prefix",
        .source =
        \\{
        \\x = Str.starts_with("hello world", "hello")
        \\x
        \\}
        ,
        .expected = .{ .inspect_str = "True" },
    },
    .{
        .name = "low_level - Str.starts_with returns False for non-matching prefix",
        .source =
        \\{
        \\x = Str.starts_with("hello world", "world")
        \\x
        \\}
        ,
        .expected = .{ .inspect_str = "False" },
    },
    .{
        .name = "low_level - Str.starts_with with empty prefix",
        .source =
        \\{
        \\x = Str.starts_with("hello", "")
        \\x
        \\}
        ,
        .expected = .{ .inspect_str = "True" },
    },
    .{
        .name = "low_level - Str.starts_with with empty string and empty prefix",
        .source =
        \\{
        \\x = Str.starts_with("", "")
        \\x
        \\}
        ,
        .expected = .{ .inspect_str = "True" },
    },
    .{
        .name = "low_level - Str.starts_with with prefix longer than string",
        .source =
        \\{
        \\x = Str.starts_with("hi", "hello")
        \\x
        \\}
        ,
        .expected = .{ .inspect_str = "False" },
    },
    .{
        .name = "low_level - Str.ends_with returns True for matching suffix",
        .source =
        \\{
        \\x = Str.ends_with("hello world", "world")
        \\x
        \\}
        ,
        .expected = .{ .inspect_str = "True" },
    },
    .{
        .name = "low_level - Str.ends_with returns False for non-matching suffix",
        .source =
        \\{
        \\x = Str.ends_with("hello world", "hello")
        \\x
        \\}
        ,
        .expected = .{ .inspect_str = "False" },
    },
    .{
        .name = "low_level - Str.ends_with with empty suffix",
        .source =
        \\{
        \\x = Str.ends_with("hello", "")
        \\x
        \\}
        ,
        .expected = .{ .inspect_str = "True" },
    },
    .{
        .name = "low_level - Str.ends_with with empty string and empty suffix",
        .source =
        \\{
        \\x = Str.ends_with("", "")
        \\x
        \\}
        ,
        .expected = .{ .inspect_str = "True" },
    },
    .{
        .name = "low_level - Str.ends_with with suffix longer than string",
        .source =
        \\{
        \\x = Str.ends_with("hi", "hello")
        \\x
        \\}
        ,
        .expected = .{ .inspect_str = "False" },
    },
    .{
        .name = "low_level - Str.repeat basic repetition",
        .source =
        \\{
        \\x = Str.repeat("ab", 3)
        \\x
        \\}
        ,
        .expected = .{ .inspect_str = "\"ababab\"" },
    },
    .{
        .name = "low_level - Str.repeat with zero count",
        .source =
        \\{
        \\x = Str.repeat("hello", 0)
        \\x
        \\}
        ,
        .expected = .{ .inspect_str = "\"\"" },
    },
    .{
        .name = "low_level - Str.repeat with one count",
        .source =
        \\{
        \\x = Str.repeat("hello", 1)
        \\x
        \\}
        ,
        .expected = .{ .inspect_str = "\"hello\"" },
    },
    .{
        .name = "low_level - Str.repeat empty string",
        .source =
        \\{
        \\x = Str.repeat("", 5)
        \\x
        \\}
        ,
        .expected = .{ .inspect_str = "\"\"" },
    },
    .{
        .name = "low_level - Str.with_prefix basic",
        .source =
        \\{
        \\x = Str.with_prefix("world", "hello ")
        \\x
        \\}
        ,
        .expected = .{ .inspect_str = "\"hello world\"" },
    },
    .{
        .name = "low_level - Str.with_prefix empty prefix",
        .source =
        \\{
        \\x = Str.with_prefix("hello", "")
        \\x
        \\}
        ,
        .expected = .{ .inspect_str = "\"hello\"" },
    },
    .{
        .name = "low_level - Str.with_prefix empty string",
        .source =
        \\{
        \\x = Str.with_prefix("", "prefix")
        \\x
        \\}
        ,
        .expected = .{ .inspect_str = "\"prefix\"" },
    },
    .{
        .name = "low_level - Str.with_prefix both empty",
        .source =
        \\{
        \\x = Str.with_prefix("", "")
        \\x
        \\}
        ,
        .expected = .{ .inspect_str = "\"\"" },
    },
    .{
        .name = "low_level - Str.drop_prefix removes matching prefix",
        .source =
        \\{
        \\x = Str.drop_prefix("hello world", "hello ")
        \\x
        \\}
        ,
        .expected = .{ .inspect_str = "\"world\"" },
    },
    .{
        .name = "low_level - Str.drop_prefix returns original when no match",
        .source =
        \\{
        \\x = Str.drop_prefix("hello world", "goodbye ")
        \\x
        \\}
        ,
        .expected = .{ .inspect_str = "\"hello world\"" },
    },
    .{
        .name = "low_level - Str.drop_prefix with empty prefix",
        .source =
        \\{
        \\x = Str.drop_prefix("hello", "")
        \\x
        \\}
        ,
        .expected = .{ .inspect_str = "\"hello\"" },
    },
    .{
        .name = "low_level - Str.drop_prefix removes entire string",
        .source =
        \\{
        \\x = Str.drop_prefix("hello", "hello")
        \\x
        \\}
        ,
        .expected = .{ .inspect_str = "\"\"" },
    },
    .{
        .name = "low_level - Str.drop_prefix prefix longer than string",
        .source =
        \\{
        \\x = Str.drop_prefix("hi", "hello")
        \\x
        \\}
        ,
        .expected = .{ .inspect_str = "\"hi\"" },
    },
    .{
        .name = "low_level - Str.drop_suffix removes matching suffix",
        .source =
        \\{
        \\x = Str.drop_suffix("hello world", " world")
        \\x
        \\}
        ,
        .expected = .{ .inspect_str = "\"hello\"" },
    },
    .{
        .name = "low_level - Str.drop_suffix returns original when no match",
        .source =
        \\{
        \\x = Str.drop_suffix("hello world", " goodbye")
        \\x
        \\}
        ,
        .expected = .{ .inspect_str = "\"hello world\"" },
    },
    .{
        .name = "low_level - Str.drop_suffix with empty suffix",
        .source =
        \\{
        \\x = Str.drop_suffix("hello", "")
        \\x
        \\}
        ,
        .expected = .{ .inspect_str = "\"hello\"" },
    },
    .{
        .name = "low_level - Str.drop_suffix removes entire string",
        .source =
        \\{
        \\x = Str.drop_suffix("hello", "hello")
        \\x
        \\}
        ,
        .expected = .{ .inspect_str = "\"\"" },
    },
    .{
        .name = "low_level - Str.drop_suffix suffix longer than string",
        .source =
        \\{
        \\x = Str.drop_suffix("hi", "hello")
        \\x
        \\}
        ,
        .expected = .{ .inspect_str = "\"hi\"" },
    },
    .{
        .name = "low_level - Str.find_first returns seamless before and after slices",
        .source =
        \\{
        \\x = match Str.find_first("alpha:beta", ":") {
        \\    Ok(parts) => Str.count_utf8_bytes(parts.before) * 100 + Str.count_utf8_bytes(parts.after)
        \\    Err(_) => 0
        \\}
        \\x
        \\}
        ,
        .expected = .{ .inspect_str = "504" },
    },
    .{
        .name = "low_level - U8.to_i16 safe widening",
        .source =
        \\{
        \\a : U8
        \\a = 200.U8
        \\x = U8.to_i16(a)
        \\x
        \\}
        ,
        .expected = .{ .inspect_str = "200" },
    },
    .{
        .name = "low_level - U8.to_i32 safe widening",
        .source =
        \\{
        \\a : U8
        \\a = 255.U8
        \\x = U8.to_i32(a)
        \\x
        \\}
        ,
        .expected = .{ .inspect_str = "255" },
    },
    .{
        .name = "low_level - U8.to_i64 safe widening",
        .source =
        \\{
        \\a : U8
        \\a = 128.U8
        \\x = U8.to_i64(a)
        \\x
        \\}
        ,
        .expected = .{ .inspect_str = "128" },
    },
    .{
        .name = "low_level - U8.to_i128 safe widening",
        .source =
        \\{
        \\a : U8
        \\a = 100.U8
        \\x = U8.to_i128(a)
        \\x
        \\}
        ,
        .expected = .{ .inspect_str = "100" },
    },
    .{
        .name = "low_level - U8.to_u16 safe widening",
        .source =
        \\{
        \\a : U8
        \\a = 200.U8
        \\x = U8.to_u16(a)
        \\x
        \\}
        ,
        .expected = .{ .inspect_str = "200" },
    },
    .{
        .name = "low_level - U8.to_u32 safe widening",
        .source =
        \\{
        \\a : U8
        \\a = 255.U8
        \\x = U8.to_u32(a)
        \\x
        \\}
        ,
        .expected = .{ .inspect_str = "255" },
    },
    .{
        .name = "low_level - U8.to_u64 safe widening",
        .source =
        \\{
        \\a : U8
        \\a = 128.U8
        \\x = U8.to_u64(a)
        \\x
        \\}
        ,
        .expected = .{ .inspect_str = "128" },
    },
    .{
        .name = "low_level - U8.to_u128 safe widening",
        .source =
        \\{
        \\a : U8
        \\a = 50.U8
        \\x = U8.to_u128(a)
        \\x
        \\}
        ,
        .expected = .{ .inspect_str = "50" },
    },
    .{
        .name = "low_level - U8.to_i8_wrap in range",
        .source =
        \\{
        \\a : U8
        \\a = 100.U8
        \\x = U8.to_i8_wrap(a)
        \\x
        \\}
        ,
        .expected = .{ .inspect_str = "100" },
    },
    .{
        .name = "low_level - U8.to_i8_wrap out of range wraps",
        .source =
        \\{
        \\a : U8
        \\a = 200.U8
        \\x = U8.to_i8_wrap(a)
        \\x
        \\}
        ,
        .expected = .{ .inspect_str = "-56" },
    },
    .{
        .name = "low_level - U8.to_i8_try in range returns Ok",
        .source =
        \\{
        \\a : U8
        \\a = 100.U8
        \\x = U8.to_i8_try(a)
        \\x
        \\}
        ,
        .expected = .{ .inspect_str = "Ok(100)" },
    },
    .{
        .name = "low_level - U8.to_i8_try out of range returns Err",
        .source =
        \\{
        \\a : U8
        \\a = 200.U8
        \\x = U8.to_i8_try(a)
        \\x
        \\}
        ,
        .expected = .{ .inspect_str = "Err(OutOfRange)" },
    },
    .{
        .name = "low_level - U8.to_f32",
        .source =
        \\{
        \\a : U8
        \\a = 42.U8
        \\x = U8.to_f32(a)
        \\x
        \\}
        ,
        .expected = .{ .inspect_str = "42" },
    },
    .{
        .name = "low_level - U8.to_f64",
        .source =
        \\{
        \\a : U8
        \\a = 255.U8
        \\x = U8.to_f64(a)
        \\x
        \\}
        ,
        .expected = .{ .inspect_str = "255" },
    },
    .{
        .name = "low_level - U8.to_dec",
        .source =
        \\{
        \\a : U8
        \\a = 123.U8
        \\x = U8.to_dec(a)
        \\y = Dec.to_str(x)
        \\y
        \\}
        ,
        .expected = .{ .inspect_str = "\"123.0\"" },
    },
    .{
        .name = "low_level - F32.to_i8_try truncates fractional part",
        .source =
        \\{
        \\x = F32.to_i8_try(42.7)
        \\x
        \\}
        ,
        .expected = .{ .inspect_str = "Ok(42)" },
    },
    .{
        .name = "low_level - F64.to_u64_try truncates fractional part",
        .source =
        \\{
        \\x = F64.to_u64_try(42.5)
        \\x
        \\}
        ,
        .expected = .{ .inspect_str = "Ok(42)" },
    },
    .{
        .name = "low_level - F64.to_u32_try accepts values above I32 max",
        .source =
        \\{
        \\x = F64.to_u32_try(3000000000.0)
        \\x
        \\}
        ,
        .expected = .{ .inspect_str = "Ok(3000000000)" },
    },
    .{
        .name = "low_level - F64.to_i64_try rejects exclusive upper bound",
        .source =
        \\{
        \\x = F64.to_i64_try(9223372036854775808.0)
        \\x
        \\}
        ,
        .expected = .{ .inspect_str = "Err(OutOfRange)" },
    },
    .{
        .name = "low_level - F64.to_i64_try accepts inclusive lower bound",
        .source =
        \\{
        \\x = F64.to_i64_try(-9223372036854775808.0)
        \\x
        \\}
        ,
        .expected = .{ .inspect_str = "Ok(-9223372036854775808)" },
    },
    .{
        .name = "low_level - F64.to_i128_try truncates negative fractional part",
        .source =
        \\{
        \\x = F64.to_i128_try(-42.5)
        \\x
        \\}
        ,
        .expected = .{ .inspect_str = "Ok(-42)" },
    },
    .{
        .name = "low_level - F64.to_u64_try rejects out-of-range without trapping",
        .source =
        \\{
        \\x = F64.to_u64_try(F64.highest)
        \\x
        \\}
        ,
        .expected = .{ .inspect_str = "Err(OutOfRange)" },
    },
    .{
        .name = "low_level - F64.to_u128_try rejects out-of-range without trapping",
        .source =
        \\{
        \\x = F64.to_u128_try(F64.highest)
        \\x
        \\}
        ,
        .expected = .{ .inspect_str = "Err(OutOfRange)" },
    },
    .{
        .name = "low_level - Dec.to_i8_try truncates fractional part",
        .source =
        \\{
        \\x = Dec.to_i8_try(42.7)
        \\x
        \\}
        ,
        .expected = .{ .inspect_str = "Ok(42)" },
    },
    .{
        .name = "low_level - Dec.to_u64_try accepts full U64 range after truncation",
        .source =
        \\{
        \\x = Dec.to_u64_try(18446744073709551615.9)
        \\x
        \\}
        ,
        .expected = .{ .inspect_str = "Ok(18446744073709551615)" },
    },
    .{
        .name = "low_level - Dec.to_u64_try rejects exclusive upper bound",
        .source =
        \\{
        \\x = Dec.to_u64_try(18446744073709551616.0)
        \\x
        \\}
        ,
        .expected = .{ .inspect_str = "Err(OutOfRange)" },
    },
    .{
        .name = "low_level - I8.to_i16 safe widening positive",
        .source =
        \\{
        \\a : I8
        \\a = 100.I8
        \\x = I8.to_i16(a)
        \\x
        \\}
        ,
        .expected = .{ .inspect_str = "100" },
    },
    .{
        .name = "low_level - I8.to_i16 safe widening negative",
        .source =
        \\{
        \\a : I8
        \\a = -50.I8
        \\x = I8.to_i16(a)
        \\x
        \\}
        ,
        .expected = .{ .inspect_str = "-50" },
    },
    .{
        .name = "low_level - I8.to_i32 safe widening",
        .source =
        \\{
        \\a : I8
        \\a = -128.I8
        \\x = I8.to_i32(a)
        \\x
        \\}
        ,
        .expected = .{ .inspect_str = "-128" },
    },
    .{
        .name = "low_level - I8.to_i64 safe widening",
        .source =
        \\{
        \\a : I8
        \\a = 127.I8
        \\x = I8.to_i64(a)
        \\x
        \\}
        ,
        .expected = .{ .inspect_str = "127" },
    },
    .{
        .name = "low_level - I8.to_i128 safe widening",
        .source =
        \\{
        \\a : I8
        \\a = -1.I8
        \\x = I8.to_i128(a)
        \\x
        \\}
        ,
        .expected = .{ .inspect_str = "-1" },
    },
    .{
        .name = "low_level - I8.to_u8_wrap in range",
        .source =
        \\{
        \\a : I8
        \\a = 50.I8
        \\x = I8.to_u8_wrap(a)
        \\x
        \\}
        ,
        .expected = .{ .inspect_str = "50" },
    },
    .{
        .name = "low_level - I8.to_u8_wrap negative wraps",
        .source =
        \\{
        \\a : I8
        \\a = -1.I8
        \\x = I8.to_u8_wrap(a)
        \\x
        \\}
        ,
        .expected = .{ .inspect_str = "255" },
    },
    .{
        .name = "low_level - I8.to_u8_try in range returns Ok",
        .source =
        \\{
        \\a : I8
        \\a = 100.I8
        \\x = I8.to_u8_try(a)
        \\x
        \\}
        ,
        .expected = .{ .inspect_str = "Ok(100)" },
    },
    .{
        .name = "low_level - I8.to_u8_try negative returns Err",
        .source =
        \\{
        \\a : I8
        \\a = -10.I8
        \\x = I8.to_u8_try(a)
        \\x
        \\}
        ,
        .expected = .{ .inspect_str = "Err(OutOfRange)" },
    },
    .{
        .name = "low_level - I8.to_u16_wrap positive",
        .source =
        \\{
        \\a : I8
        \\a = 100.I8
        \\x = I8.to_u16_wrap(a)
        \\x
        \\}
        ,
        .expected = .{ .inspect_str = "100" },
    },
    .{
        .name = "low_level - I8.to_u16_wrap negative wraps",
        .source =
        \\{
        \\a : I8
        \\a = -1.I8
        \\x = I8.to_u16_wrap(a)
        \\x
        \\}
        ,
        .expected = .{ .inspect_str = "65535" },
    },
    .{
        .name = "low_level - I8.to_u16_try in range returns Ok",
        .source =
        \\{
        \\a : I8
        \\a = 50.I8
        \\x = I8.to_u16_try(a)
        \\x
        \\}
        ,
        .expected = .{ .inspect_str = "Ok(50)" },
    },
    .{
        .name = "low_level - I8.to_u16_try negative returns Err",
        .source =
        \\{
        \\a : I8
        \\a = -5.I8
        \\x = I8.to_u16_try(a)
        \\x
        \\}
        ,
        .expected = .{ .inspect_str = "Err(OutOfRange)" },
    },
    .{
        .name = "low_level - I8.to_u32_try negative returns Err",
        .source =
        \\{
        \\a : I8
        \\a = -100.I8
        \\x = I8.to_u32_try(a)
        \\x
        \\}
        ,
        .expected = .{ .inspect_str = "Err(OutOfRange)" },
    },
    .{
        .name = "low_level - I8.to_u64_try positive returns Ok",
        .source =
        \\{
        \\a : I8
        \\a = 127.I8
        \\x = I8.to_u64_try(a)
        \\x
        \\}
        ,
        .expected = .{ .inspect_str = "Ok(127)" },
    },
    .{
        .name = "low_level - I8.to_u128_try zero returns Ok",
        .source =
        \\{
        \\a : I8
        \\a = 0.I8
        \\x = I8.to_u128_try(a)
        \\x
        \\}
        ,
        .expected = .{ .inspect_str = "Ok(0)" },
    },
    .{
        .name = "low_level - I8.to_f32 positive",
        .source =
        \\{
        \\a : I8
        \\a = 42.I8
        \\x = I8.to_f32(a)
        \\x
        \\}
        ,
        .expected = .{ .inspect_str = "42" },
    },
    .{
        .name = "low_level - I8.to_f64 negative",
        .source =
        \\{
        \\a : I8
        \\a = -100.I8
        \\x = I8.to_f64(a)
        \\x
        \\}
        ,
        .expected = .{ .inspect_str = "-100" },
    },
    .{
        .name = "low_level - I8.to_dec positive",
        .source =
        \\{
        \\a : I8
        \\a = 50.I8
        \\x = I8.to_dec(a)
        \\y = Dec.to_str(x)
        \\y
        \\}
        ,
        .expected = .{ .inspect_str = "\"50.0\"" },
    },
    .{
        .name = "low_level - I8.to_dec negative",
        .source =
        \\{
        \\a : I8
        \\a = -25.I8
        \\x = I8.to_dec(a)
        \\y = Dec.to_str(x)
        \\y
        \\}
        ,
        .expected = .{ .inspect_str = "\"-25.0\"" },
    },
    .{
        .name = "low_level - Str.count_utf8_bytes empty string",
        .source =
        \\{
        \\x = Str.count_utf8_bytes("")
        \\x
        \\}
        ,
        .expected = .{ .inspect_str = "0" },
    },
    .{
        .name = "low_level - Str.count_utf8_bytes ASCII string",
        .source =
        \\{
        \\x = Str.count_utf8_bytes("hello")
        \\x
        \\}
        ,
        .expected = .{ .inspect_str = "5" },
    },
    .{
        .name = "low_level - Str.count_utf8_bytes multi-byte UTF-8",
        .source =
        \\{
        \\x = Str.count_utf8_bytes("é")
        \\x
        \\}
        ,
        .expected = .{ .inspect_str = "2" },
    },
    .{
        .name = "low_level - Str.count_utf8_bytes emoji",
        .source =
        \\{
        \\x = Str.count_utf8_bytes("🎉")
        \\x
        \\}
        ,
        .expected = .{ .inspect_str = "4" },
    },
    .{
        .name = "low_level - Str.with_capacity returns empty string",
        .source =
        \\{
        \\x = Str.with_capacity(0)
        \\x
        \\}
        ,
        .expected = .{ .inspect_str = "\"\"" },
    },
    .{
        .name = "low_level - Str.with_capacity with capacity returns empty string",
        .source =
        \\{
        \\x = Str.with_capacity(100)
        \\x
        \\}
        ,
        .expected = .{ .inspect_str = "\"\"" },
    },
    .{
        .name = "low_level - Str.reserve preserves content",
        .source =
        \\{
        \\x = Str.reserve("hello", 100)
        \\x
        \\}
        ,
        .expected = .{ .inspect_str = "\"hello\"" },
    },
    .{
        .name = "low_level - Str.reserve empty string",
        .source =
        \\{
        \\x = Str.reserve("", 50)
        \\x
        \\}
        ,
        .expected = .{ .inspect_str = "\"\"" },
    },
    .{
        .name = "low_level - Str.release_excess_capacity preserves content",
        .source =
        \\{
        \\x = Str.release_excess_capacity("hello")
        \\x
        \\}
        ,
        .expected = .{ .inspect_str = "\"hello\"" },
    },
    .{
        .name = "low_level - Str.release_excess_capacity empty string",
        .source =
        \\{
        \\x = Str.release_excess_capacity("")
        \\x
        \\}
        ,
        .expected = .{ .inspect_str = "\"\"" },
    },
    .{
        .name = "low_level - Str.to_utf8 empty string",
        .source =
        \\{
        \\x = List.len(Str.to_utf8(""))
        \\x
        \\}
        ,
        .expected = .{ .inspect_str = "0" },
    },
    .{
        .name = "low_level - Str.to_utf8 ASCII string",
        .source =
        \\{
        \\x = List.len(Str.to_utf8("hello"))
        \\x
        \\}
        ,
        .expected = .{ .inspect_str = "5" },
    },
    .{
        .name = "low_level - Str.to_utf8 multi-byte UTF-8",
        .source =
        \\{
        \\x = List.len(Str.to_utf8("é"))
        \\x
        \\}
        ,
        .expected = .{ .inspect_str = "2" },
    },
    .{
        .name = "low_level - Str.from_utf8_lossy roundtrip ASCII",
        .source =
        \\{
        \\x = Str.from_utf8_lossy(Str.to_utf8("hello"))
        \\x
        \\}
        ,
        .expected = .{ .inspect_str = "\"hello\"" },
    },
    .{
        .name = "low_level - Str.from_utf8_lossy roundtrip empty",
        .source =
        \\{
        \\x = Str.from_utf8_lossy(Str.to_utf8(""))
        \\x
        \\}
        ,
        .expected = .{ .inspect_str = "\"\"" },
    },
    .{
        .name = "low_level - Str.from_utf8_lossy roundtrip UTF-8",
        .source =
        \\{
        \\x = Str.from_utf8_lossy(Str.to_utf8("hello 🎉 world"))
        \\x
        \\}
        ,
        .expected = .{ .inspect_str = "\"hello 🎉 world\"" },
    },
    .{
        .name = "low_level - Str.split_on basic split count",
        .source =
        \\{
        \\x = List.len(Str.split_on("hello world", " "))
        \\x
        \\}
        ,
        .expected = .{ .inspect_str = "2" },
    },
    .{
        .name = "low_level - Str.split_on basic split first element",
        .source =
        \\{
        \\parts = Str.split_on("hello world", " ")
        \\first = List.first(parts)
        \\first
        \\}
        ,
        .expected = .{ .inspect_str = "Ok(\"hello\")" },
    },
    .{
        .name = "low_level - Str.split_on multiple delimiters count",
        .source =
        \\{
        \\x = List.len(Str.split_on("a,b,c,d", ","))
        \\x
        \\}
        ,
        .expected = .{ .inspect_str = "4" },
    },
    .{
        .name = "low_level - Str.split_on multiple delimiters first element",
        .source =
        \\{
        \\parts = Str.split_on("a,b,c,d", ",")
        \\first = List.first(parts)
        \\first
        \\}
        ,
        .expected = .{ .inspect_str = "Ok(\"a\")" },
    },
    .{
        .name = "low_level - Str.split_on no match",
        .source =
        \\{
        \\parts = Str.split_on("hello", "x")
        \\first = List.first(parts)
        \\first
        \\}
        ,
        .expected = .{ .inspect_str = "Ok(\"hello\")" },
    },
    .{
        .name = "low_level - Str.split_on empty string",
        .source =
        \\{
        \\x = List.len(Str.split_on("", ","))
        \\x
        \\}
        ,
        .expected = .{ .inspect_str = "1" },
    },
    .{
        .name = "low_level - Str.join_with basic join",
        .source =
        \\{
        \\x = Str.join_with(["hello", "world"], " ")
        \\x
        \\}
        ,
        .expected = .{ .inspect_str = "\"hello world\"" },
    },
    .{
        .name = "low_level - Str.join_with multiple elements",
        .source =
        \\{
        \\x = Str.join_with(["a", "b", "c", "d"], ",")
        \\x
        \\}
        ,
        .expected = .{ .inspect_str = "\"a,b,c,d\"" },
    },
    .{
        .name = "low_level - Str.join_with single element",
        .source =
        \\{
        \\x = Str.join_with(["hello"], "-")
        \\x
        \\}
        ,
        .expected = .{ .inspect_str = "\"hello\"" },
    },
    .{
        .name = "low_level - Str.join_with empty list",
        .source =
        \\{
        \\x = Str.join_with([], ",")
        \\x
        \\}
        ,
        .expected = .{ .inspect_str = "\"\"" },
    },
    .{
        .name = "low_level - Str.join_with roundtrip with split_on",
        .source =
        \\{
        \\x = Str.join_with(Str.split_on("hello world", " "), " ")
        \\x
        \\}
        ,
        .expected = .{ .inspect_str = "\"hello world\"" },
    },
    .{
        .name = "low_level - U8.plus basic",
        .source =
        \\{
        \\a : U8
        \\a = 5
        \\b : U8
        \\b = 3
        \\x : U8
        \\x = U8.plus(a, b)
        \\x
        \\}
        ,
        .expected = .{ .inspect_str = "8" },
    },
    .{
        .name = "low_level - U8.plus method call syntax",
        .source =
        \\{
        \\a : U8
        \\a = 5
        \\b : U8
        \\b = 3
        \\x : U8
        \\x = a.plus(b)
        \\x
        \\}
        ,
        .expected = .{ .inspect_str = "8" },
    },
    .{
        .name = "low_level - U8.shift_left_by basic",
        .source =
        \\{
        \\a : U8
        \\a = 5
        \\x = a.shift_left_by(2)
        \\x
        \\}
        ,
        .expected = .{ .inspect_str = "20" },
    },
    .{
        .name = "low_level - U8.shift_right_by basic",
        .source =
        \\{
        \\a : U8
        \\a = 20
        \\x = a.shift_right_by(2)
        \\x
        \\}
        ,
        .expected = .{ .inspect_str = "5" },
    },
    .{
        .name = "low_level - U8.shift_right_zf_by basic",
        .source =
        \\{
        \\a : U8
        \\a = 128
        \\x = a.shift_right_zf_by(2)
        \\x
        \\}
        ,
        .expected = .{ .inspect_str = "32" },
    },
    .{
        .name = "low_level - I8.shift_left_by positive",
        .source =
        \\{
        \\a : I8
        \\a = 3
        \\x = a.shift_left_by(3)
        \\x
        \\}
        ,
        .expected = .{ .inspect_str = "24" },
    },
    .{
        .name = "low_level - I8.shift_right_by negative arithmetic",
        .source =
        \\{
        \\a : I8
        \\a = -8
        \\x = a.shift_right_by(1)
        \\x
        \\}
        ,
        .expected = .{ .inspect_str = "-4" },
    },
    .{
        .name = "low_level - I8.shift_right_zf_by negative zero_fill",
        .source =
        \\{
        \\a : I8
        \\a = -8
        \\x = a.shift_right_zf_by(1)
        \\x
        \\}
        ,
        .expected = .{ .inspect_str = "124" },
    },
    .{
        .name = "low_level - U16.shift_left_by",
        .source =
        \\{
        \\a : U16
        \\a = 1
        \\x = a.shift_left_by(4)
        \\x
        \\}
        ,
        .expected = .{ .inspect_str = "16" },
    },
    .{
        .name = "low_level - I16.shift_right_by positive",
        .source =
        \\{
        \\a : I16
        \\a = 64
        \\x = a.shift_right_by(3)
        \\x
        \\}
        ,
        .expected = .{ .inspect_str = "8" },
    },
    .{
        .name = "low_level - I16.shift_right_by negative",
        .source =
        \\{
        \\a : I16
        \\a = -16
        \\x = a.shift_right_by(2)
        \\x
        \\}
        ,
        .expected = .{ .inspect_str = "-4" },
    },
    .{
        .name = "low_level - U32.shift_left_by",
        .source =
        \\{
        \\a : U32
        \\a = 16
        \\x = a.shift_left_by(3)
        \\x
        \\}
        ,
        .expected = .{ .inspect_str = "128" },
    },
    .{
        .name = "low_level - I32.shift_right_by negative",
        .source =
        \\{
        \\a : I32
        \\a = -32
        \\x = a.shift_right_by(3)
        \\x
        \\}
        ,
        .expected = .{ .inspect_str = "-4" },
    },
    .{
        .name = "low_level - U64.shift_left_by",
        .source =
        \\{
        \\a : U64
        \\a = 255
        \\x = a.shift_left_by(8)
        \\x
        \\}
        ,
        .expected = .{ .inspect_str = "65280" },
    },
    .{
        .name = "low_level - I64.shift_right_by negative",
        .source =
        \\{
        \\a : I64
        \\a = -1024
        \\x = a.shift_right_by(2)
        \\x
        \\}
        ,
        .expected = .{ .inspect_str = "-256" },
    },
    .{
        .name = "low_level - U128.shift_left_by",
        .source =
        \\{
        \\a : U128
        \\a = 1
        \\x = a.shift_left_by(10)
        \\x
        \\}
        ,
        .expected = .{ .inspect_str = "1024" },
    },
    .{
        .name = "low_level - I128.shift_right_by negative",
        .source =
        \\{
        \\a : I128
        \\a = -256
        \\x = a.shift_right_by(4)
        \\x
        \\}
        ,
        .expected = .{ .inspect_str = "-16" },
    },
    .{
        .name = "low_level - shift_left_by with zero shift",
        .source =
        \\{
        \\a : U8
        \\a = 42
        \\x = a.shift_left_by(0)
        \\x
        \\}
        ,
        .expected = .{ .inspect_str = "42" },
    },
    .{
        .name = "low_level - shift_right_by with zero shift",
        .source =
        \\{
        \\a : I8
        \\a = -42
        \\x = a.shift_right_by(0)
        \\x
        \\}
        ,
        .expected = .{ .inspect_str = "-42" },
    },
    .{
        .name = "low_level - shift operations preserve type",
        .source =
        \\{
        \\a : U32
        \\a = 100
        \\b = a.shift_left_by(2)
        \\c = b.shift_right_by(1)
        \\x = c.shift_right_zf_by(1)
        \\x
        \\}
        ,
        .expected = .{ .inspect_str = "100" },
    },
    .{
        .name = "low_level - I8.shift_right_zf_by with -1",
        .source =
        \\{
        \\a : I8
        \\a = -1
        \\x = a.shift_right_zf_by(4)
        \\x
        \\}
        ,
        .expected = .{ .inspect_str = "15" },
    },
    .{
        .name = "low_level - U16.shift_right_zf_by equals shift_right_by for unsigned",
        .source =
        \\{
        \\a : U16
        \\a = 256
        \\b = a.shift_right_by(4)
        \\c = a.shift_right_zf_by(4)
        \\x = U16.is_eq(b, c)
        \\x
        \\}
        ,
        .expected = .{ .inspect_str = "True" },
    },
    .{
        .name = "low_level - U8.shift_left_by overflow wraps",
        .source =
        \\{
        \\a : U8
        \\a = 128
        \\x = a.shift_left_by(1)
        \\x
        \\}
        ,
        .expected = .{ .inspect_str = "0" },
    },
    .{
        .name = "low_level - I8.shift_left_by overflow wraps",
        .source =
        \\{
        \\a : I8
        \\a = 64
        \\x = a.shift_left_by(2)
        \\x
        \\}
        ,
        .expected = .{ .inspect_str = "0" },
    },
    .{
        .name = "low_level - I8.shift_left_by max value overflow",
        .source =
        \\{
        \\a : I8
        \\a = 127
        \\x = a.shift_left_by(1)
        \\x
        \\}
        ,
        .expected = .{ .inspect_str = "-2" },
    },
    .{
        .name = "low_level - U8.shift_right_by max value",
        .source =
        \\{
        \\a : U8
        \\a = 255
        \\x = a.shift_right_by(1)
        \\x
        \\}
        ,
        .expected = .{ .inspect_str = "127" },
    },
    .{
        .name = "low_level - I8.shift_right_by min value",
        .source =
        \\{
        \\a : I8
        \\a = -128
        \\x = a.shift_right_by(1)
        \\x
        \\}
        ,
        .expected = .{ .inspect_str = "-64" },
    },
    .{
        .name = "low_level - I8.shift_right_zf_by min value",
        .source =
        \\{
        \\a : I8
        \\a = -128
        \\x = a.shift_right_zf_by(1)
        \\x
        \\}
        ,
        .expected = .{ .inspect_str = "64" },
    },
    .{
        .name = "low_level - shift_left_by amount at bit width boundary",
        .source =
        \\{
        \\a : U8
        \\a = 1
        \\x = a.shift_left_by(7)
        \\x
        \\}
        ,
        .expected = .{ .inspect_str = "128" },
    },
    .{
        .name = "low_level - shift_right_by amount at bit width boundary",
        .source =
        \\{
        \\a : U8
        \\a = 128
        \\x = a.shift_right_by(7)
        \\x
        \\}
        ,
        .expected = .{ .inspect_str = "1" },
    },
    .{
        .name = "low_level - I8.shift_right_by negative all ones preserves",
        .source =
        \\{
        \\a : I8
        \\a = -1
        \\x = a.shift_right_by(7)
        \\x
        \\}
        ,
        .expected = .{ .inspect_str = "-1" },
    },
    .{
        .name = "low_level - I8.shift_right_by negative rounds toward negative infinity",
        .source =
        \\{
        \\a : I8
        \\a = -3
        \\x = a.shift_right_by(1)
        \\x
        \\}
        ,
        .expected = .{ .inspect_str = "-2" },
    },
    .{
        .name = "low_level - U8.shift_right_zf_by all ones pattern",
        .source =
        \\{
        \\a : U8
        \\a = 255
        \\x = a.shift_right_zf_by(1)
        \\x
        \\}
        ,
        .expected = .{ .inspect_str = "127" },
    },
    .{
        .name = "low_level - I8.shift_right_zf_by all ones from negative",
        .source =
        \\{
        \\a : I8
        \\a = -1
        \\x = a.shift_right_zf_by(1)
        \\x
        \\}
        ,
        .expected = .{ .inspect_str = "127" },
    },
    .{
        .name = "low_level - shift_left_by with zero value",
        .source =
        \\{
        \\a : U8
        \\a = 0
        \\x = a.shift_left_by(5)
        \\x
        \\}
        ,
        .expected = .{ .inspect_str = "0" },
    },
    .{
        .name = "low_level - shift_right_zf_by with zero value",
        .source =
        \\{
        \\a : I8
        \\a = 0
        \\x = a.shift_right_zf_by(3)
        \\x
        \\}
        ,
        .expected = .{ .inspect_str = "0" },
    },
    .{
        .name = "low_level - shift_left_by large shift amount clamped U8",
        .source =
        \\{
        \\a : U8
        \\a = 1
        \\x = a.shift_left_by(200)
        \\x
        \\}
        ,
        .expected = .{ .inspect_str = "0" },
    },
    .{
        .name = "low_level - shift_right_by large shift amount clamped",
        .source =
        \\{
        \\a : U8
        \\a = 255
        \\x = a.shift_right_by(200)
        \\x
        \\}
        ,
        .expected = .{ .inspect_str = "0" },
    },
    .{
        .name = "low_level - U16.shift_left_by to max representable",
        .source =
        \\{
        \\a : U16
        \\a = 1
        \\x = a.shift_left_by(15)
        \\x
        \\}
        ,
        .expected = .{ .inspect_str = "32768" },
    },
    .{
        .name = "low_level - U32.shift_left_by power of 2",
        .source =
        \\{
        \\a : U32
        \\a = 1
        \\x = a.shift_left_by(20)
        \\x
        \\}
        ,
        .expected = .{ .inspect_str = "1048576" },
    },
    .{
        .name = "low_level - U64.shift_left_by large power",
        .source =
        \\{
        \\a : U64
        \\a = 1
        \\x = a.shift_left_by(40)
        \\x
        \\}
        ,
        .expected = .{ .inspect_str = "1099511627776" },
    },
    .{
        .name = "low_level - U128.shift_left_by near max",
        .source =
        \\{
        \\a : U128
        \\a = 1
        \\x = a.shift_left_by(100)
        \\x
        \\}
        ,
        .expected = .{ .inspect_str = "1267650600228229401496703205376" },
    },
    .{
        .name = "low_level - I16.shift_right_by negative large magnitude",
        .source =
        \\{
        \\a : I16
        \\a = -1024
        \\x = a.shift_right_by(5)
        \\x
        \\}
        ,
        .expected = .{ .inspect_str = "-32" },
    },
    .{
        .name = "low_level - I32.shift_right_by min value",
        .source =
        \\{
        \\a : I32
        \\a = -2147483648
        \\x = a.shift_right_by(1)
        \\x
        \\}
        ,
        .expected = .{ .inspect_str = "-1073741824" },
    },
    .{
        .name = "low_level - I32.shift_right_zf_by min value",
        .source =
        \\{
        \\a : I32
        \\a = -2147483648
        \\x = a.shift_right_zf_by(1)
        \\x
        \\}
        ,
        .expected = .{ .inspect_str = "1073741824" },
    },
    .{
        .name = "low_level - shift single bit round trip",
        .source =
        \\{
        \\a : U8
        \\a = 1
        \\b = a.shift_left_by(5)
        \\x = b.shift_right_by(5)
        \\x
        \\}
        ,
        .expected = .{ .inspect_str = "1" },
    },
    .{
        .name = "low_level - I64.shift_right_by negative two",
        .source =
        \\{
        \\a : I64
        \\a = -2
        \\x = a.shift_right_by(1)
        \\x
        \\}
        ,
        .expected = .{ .inspect_str = "-1" },
    },
    .{
        .name = "low_level - U32.shift_left_by shift amount exactly at width",
        .source =
        \\{
        \\a : U32
        \\a = 1
        \\x = a.shift_left_by(32)
        \\x
        \\}
        ,
        .expected = .{ .inspect_str = "0" },
    },
    .{
        .name = "low_level - I8.shift_right_by negative by 7 bits",
        .source =
        \\{
        \\a : I8
        \\a = -127
        \\x = a.shift_right_by(6)
        \\x
        \\}
        ,
        .expected = .{ .inspect_str = "-2" },
    },
    .{
        .name = "low_level - U64.shift_right_zf_by max value by half",
        .source =
        \\{
        \\a : U64
        \\a = 18446744073709551615
        \\x = a.shift_right_zf_by(32)
        \\x
        \\}
        ,
        .expected = .{ .inspect_str = "4294967295" },
    },
    .{
        .name = "low_level - U8.bitwise_and basic",
        .source =
        \\{
        \\a : U8
        \\a = 0b1100
        \\x = a.bitwise_and(0b1010)
        \\x
        \\}
        ,
        .expected = .{ .inspect_str = "8" },
    },
    .{
        .name = "low_level - U8.bitwise_or basic",
        .source =
        \\{
        \\a : U8
        \\a = 0b1100
        \\x = a.bitwise_or(0b1010)
        \\x
        \\}
        ,
        .expected = .{ .inspect_str = "14" },
    },
    .{
        .name = "low_level - U8.bitwise_xor basic",
        .source =
        \\{
        \\a : U8
        \\a = 0b1100
        \\x = a.bitwise_xor(0b1010)
        \\x
        \\}
        ,
        .expected = .{ .inspect_str = "6" },
    },
    .{
        .name = "low_level - U8.bitwise_not basic",
        .source =
        \\{
        \\a : U8
        \\a = 0
        \\x = a.bitwise_not()
        \\x
        \\}
        ,
        .expected = .{ .inspect_str = "255" },
    },
    .{
        .name = "low_level - U8.bitwise_not high bits",
        .source =
        \\{
        \\a : U8
        \\a = 0b0000_1111
        \\x = a.bitwise_not()
        \\x
        \\}
        ,
        .expected = .{ .inspect_str = "240" },
    },
    .{
        .name = "low_level - I8.bitwise_and basic",
        .source =
        \\{
        \\a : I8
        \\a = 0b0110
        \\x = a.bitwise_and(0b0011)
        \\x
        \\}
        ,
        .expected = .{ .inspect_str = "2" },
    },
    .{
        .name = "low_level - I8.bitwise_xor with negative one inverts",
        .source =
        \\{
        \\a : I8
        \\a = 5
        \\x = a.bitwise_xor(-1)
        \\x
        \\}
        ,
        .expected = .{ .inspect_str = "-6" },
    },
    .{
        .name = "low_level - I8.bitwise_not equals negate minus one",
        .source =
        \\{
        \\a : I8
        \\a = 5
        \\x = a.bitwise_not()
        \\x
        \\}
        ,
        .expected = .{ .inspect_str = "-6" },
    },
    .{
        .name = "low_level - I8.bitwise_not of negative one is zero",
        .source =
        \\{
        \\a : I8
        \\a = -1
        \\x = a.bitwise_not()
        \\x
        \\}
        ,
        .expected = .{ .inspect_str = "0" },
    },
    .{
        .name = "low_level - U16.bitwise_or sets high byte",
        .source =
        \\{
        \\a : U16
        \\a = 0x00FF
        \\x = a.bitwise_or(0xFF00)
        \\x
        \\}
        ,
        .expected = .{ .inspect_str = "65535" },
    },
    .{
        .name = "low_level - U16.bitwise_not basic",
        .source =
        \\{
        \\a : U16
        \\a = 0
        \\x = a.bitwise_not()
        \\x
        \\}
        ,
        .expected = .{ .inspect_str = "65535" },
    },
    .{
        .name = "low_level - I16.bitwise_xor self is zero",
        .source =
        \\{
        \\a : I16
        \\a = 12345
        \\x = a.bitwise_xor(a)
        \\x
        \\}
        ,
        .expected = .{ .inspect_str = "0" },
    },
    .{
        .name = "low_level - U32.bitwise_xor swaps nibbles",
        .source =
        \\{
        \\a : U32
        \\a = 0xFFFF
        \\x = a.bitwise_xor(0x0F0F)
        \\x
        \\}
        ,
        .expected = .{ .inspect_str = "61680" },
    },
    .{
        .name = "low_level - U32.bitwise_not basic",
        .source =
        \\{
        \\a : U32
        \\a = 0
        \\x = a.bitwise_not()
        \\x
        \\}
        ,
        .expected = .{ .inspect_str = "4294967295" },
    },
    .{
        .name = "low_level - I32.bitwise_and masks low bits",
        .source =
        \\{
        \\a : I32
        \\a = -1
        \\x = a.bitwise_and(0xFF)
        \\x
        \\}
        ,
        .expected = .{ .inspect_str = "255" },
    },
    .{
        .name = "low_level - U64.bitwise_and overlap",
        .source =
        \\{
        \\a : U64
        \\a = 0xFF00
        \\x = a.bitwise_and(0x0FF0)
        \\x
        \\}
        ,
        .expected = .{ .inspect_str = "3840" },
    },
    .{
        .name = "low_level - U64.bitwise_not basic",
        .source =
        \\{
        \\a : U64
        \\a = 0
        \\x = a.bitwise_not()
        \\x
        \\}
        ,
        .expected = .{ .inspect_str = "18446744073709551615" },
    },
    .{
        .name = "low_level - I64.bitwise_not basic",
        .source =
        \\{
        \\a : I64
        \\a = 5
        \\x = a.bitwise_not()
        \\x
        \\}
        ,
        .expected = .{ .inspect_str = "-6" },
    },
    .{
        .name = "low_level - U128.bitwise_and high word",
        .source =
        \\{
        \\a : U128
        \\a = 0xFFFF_FFFF_FFFF_FFFF_0000_0000_0000_0000
        \\x = a.bitwise_and(0xFFFF_0000_0000_0000_0000_0000_0000_0000)
        \\x
        \\}
        ,
        .expected = .{ .inspect_str = "340277174624079928635746076935438991360" },
    },
    .{
        .name = "low_level - U128.bitwise_xor basic",
        .source =
        \\{
        \\a : U128
        \\a = 0b1100
        \\x = a.bitwise_xor(0b1010)
        \\x
        \\}
        ,
        .expected = .{ .inspect_str = "6" },
    },
    .{
        .name = "low_level - U128.bitwise_not basic",
        .source =
        \\{
        \\a : U128
        \\a = 0
        \\x = a.bitwise_not()
        \\x
        \\}
        ,
        .expected = .{ .inspect_str = "340282366920938463463374607431768211455" },
    },
    .{
        .name = "low_level - I128.bitwise_not basic",
        .source =
        \\{
        \\a : I128
        \\a = 5
        \\x = a.bitwise_not()
        \\x
        \\}
        ,
        .expected = .{ .inspect_str = "-6" },
    },
    .{
        .name = "low_level - I128.bitwise_and with negative one is identity",
        .source =
        \\{
        \\a : I128
        \\a = 1234567890123456789
        \\x = a.bitwise_and(-1)
        \\x
        \\}
        ,
        .expected = .{ .inspect_str = "1234567890123456789" },
    },
    .{
        .name = "low_level - U64.bitwise ops combine",
        .source =
        \\{
        \\a : U64
        \\a = 0b1010
        \\b = a.bitwise_or(0b0101)
        \\c = b.bitwise_and(0b1100)
        \\x = c.bitwise_xor(0b0001)
        \\x
        \\}
        ,
        .expected = .{ .inspect_str = "13" },
    },
    .{
        .name = "low_level - List.sort_with basic ascending sort",
        .source =
        \\{
        \\x = List.sort_with([3, 1, 2], |a, b| if a < b LT else if a > b GT else EQ)
        \\first = List.first(x)
        \\first
        \\}
        ,
        .expected = .{ .inspect_str = "Ok(1.0)" },
    },
    .{
        .name = "low_level - List.sort_with preserves length",
        .source =
        \\{
        \\x = List.sort_with([5, 2, 8, 1, 9], |a, b| if a < b LT else if a > b GT else EQ)
        \\len = List.len(x)
        \\len
        \\}
        ,
        .expected = .{ .inspect_str = "5" },
    },
    .{
        .name = "low_level - List.sort_with nested in len defaults literal item type",
        .source =
        \\{
        \\List.len(List.sort_with([3, 1, 2], |a, b| if a < b LT else if a > b GT else EQ))
        \\}
        ,
        .expected = .{ .inspect_str = "3" },
    },
    .{
        .name = "low_level - List.sort_with with larger list",
        .source =
        \\{
        \\x = List.sort_with([5, 2, 8, 1, 9, 3, 7, 4, 6], |a, b| if a < b LT else if a > b GT else EQ)
        \\first = List.first(x)
        \\first
        \\}
        ,
        .expected = .{ .inspect_str = "Ok(1.0)" },
    },
    .{
        .name = "low_level - List.sort_with with two elements",
        .source =
        \\{
        \\x = List.sort_with([2, 1], |a, b| if a < b LT else if a > b GT else EQ)
        \\first = List.first(x)
        \\first
        \\}
        ,
        .expected = .{ .inspect_str = "Ok(1.0)" },
    },
    .{
        .name = "low_level - List.sort_with descending order",
        .source =
        \\{
        \\x = List.sort_with([1, 3, 2], |a, b| if a > b LT else if a < b GT else EQ)
        \\first = List.first(x)
        \\first
        \\}
        ,
        .expected = .{ .inspect_str = "Ok(3.0)" },
    },
    .{
        .name = "low_level - List.sort_with empty list",
        .source =
        \\{
        \\x : List(U64)
        \\x = List.sort_with([], |a, b| if a < b LT else if a > b GT else EQ)
        \\len = List.len(x)
        \\len
        \\}
        ,
        .expected = .{ .inspect_str = "0" },
    },
    .{
        .name = "low_level - List.sort_with single element",
        .source =
        \\{
        \\x = List.sort_with([42], |a, b| if a < b LT else if a > b GT else EQ)
        \\first = List.first(x)
        \\first
        \\}
        ,
        .expected = .{ .inspect_str = "Ok(42.0)" },
    },
    .{
        .name = "low_level - List.sort_with already sorted",
        .source =
        \\{
        \\x = List.sort_with([1, 2, 3, 4, 5], |a, b| if a < b LT else if a > b GT else EQ)
        \\first = List.first(x)
        \\first
        \\}
        ,
        .expected = .{ .inspect_str = "Ok(1.0)" },
    },
    .{
        .name = "low_level - List.sort_with reverse sorted",
        .source =
        \\{
        \\x = List.sort_with([5, 4, 3, 2, 1], |a, b| if a < b LT else if a > b GT else EQ)
        \\first = List.first(x)
        \\first
        \\}
        ,
        .expected = .{ .inspect_str = "Ok(1.0)" },
    },
    .{
        .name = "low_level - U8.mod_by basic",
        .source =
        \\{
        \\a : U8
        \\a = 10
        \\b : U8
        \\b = 3
        \\x : U8
        \\x = U8.mod_by(a, b)
        \\x
        \\}
        ,
        .expected = .{ .inspect_str = "1" },
    },
    .{
        .name = "low_level - U8.mod_by zero remainder",
        .source =
        \\{
        \\a : U8
        \\a = 10
        \\b : U8
        \\b = 5
        \\x : U8
        \\x = U8.mod_by(a, b)
        \\x
        \\}
        ,
        .expected = .{ .inspect_str = "0" },
    },
    .{
        .name = "low_level - I8.mod_by positive positive",
        .source =
        \\{
        \\a : I8
        \\a = 10
        \\b : I8
        \\b = 3
        \\x : I8
        \\x = I8.mod_by(a, b)
        \\x
        \\}
        ,
        .expected = .{ .inspect_str = "1" },
    },
    .{
        .name = "low_level - I8.mod_by negative positive",
        .source =
        \\{
        \\a : I8
        \\a = -10
        \\b : I8
        \\b = 3
        \\x : I8
        \\x = I8.mod_by(a, b)
        \\x
        \\}
        ,
        .expected = .{ .inspect_str = "2" },
    },
    .{
        .name = "low_level - I8.mod_by positive negative",
        .source =
        \\{
        \\a : I8
        \\a = 10
        \\b : I8
        \\b = -3
        \\x : I8
        \\x = I8.mod_by(a, b)
        \\x
        \\}
        ,
        .expected = .{ .inspect_str = "-2" },
    },
    .{
        .name = "low_level - I8.mod_by negative negative",
        .source =
        \\{
        \\a : I8
        \\a = -10
        \\b : I8
        \\b = -3
        \\x : I8
        \\x = I8.mod_by(a, b)
        \\x
        \\}
        ,
        .expected = .{ .inspect_str = "-1" },
    },
    .{
        .name = "low_level - U64.mod_by large numbers",
        .source =
        \\{
        \\a : U64
        \\a = 1000000
        \\b : U64
        \\b = 7
        \\x : U64
        \\x = U64.mod_by(a, b)
        \\x
        \\}
        ,
        .expected = .{ .inspect_str = "1" },
    },
    .{
        .name = "low_level - I64.mod_by with zero result",
        .source =
        \\{
        \\a : I64
        \\a = 100
        \\b : I64
        \\b = 10
        \\x : I64
        \\x = I64.mod_by(a, b)
        \\x
        \\}
        ,
        .expected = .{ .inspect_str = "0" },
    },
    .{
        .name = "issue 8750: dbg in polymorphic debug function with List.len",
        .source =
        \\{
        \\debug = |v| {
        \\    dbg v
        \\    v
        \\}
        \\xs = [1, 2, 3]
        \\len = xs->debug()->List.len()
        \\len
        \\}
        ,
        .expected = .{ .inspect_str = "3" },
    },
    .{
        .name = "issue 8750: dbg in polymorphic debug function with List.first",
        .source =
        \\{
        \\debug = |v| {
        \\    dbg v
        \\    v
        \\}
        \\xs = [10, 20, 30]
        \\first = xs->debug()->List.first()
        \\first
        \\}
        ,
        .expected = .{ .inspect_str = "Ok(10.0)" },
    },
    .{
        .name = "issue 8750: dbg in polymorphic debug function chained multiple times",
        .source =
        \\{
        \\debug = |v| {
        \\    dbg v
        \\    v
        \\}
        \\xs = [1, 2, 3, 4, 5]
        \\result = xs->debug()->debug()->List.len()
        \\result
        \\}
        ,
        .expected = .{ .inspect_str = "5" },
    },
    .{
        .name = "issue 8750: dbg in polymorphic function with List.fold",
        .source =
        \\{
        \\debug = |v| {
        \\    dbg v
        \\    v
        \\}
        \\xs = [1, 2, 3]
        \\sum = xs->debug()->List.fold(0, |acc, x| acc + x)
        \\sum
        \\}
        ,
        .expected = .{ .inspect_str = "6.0" },
    },
    .{
        .name = "issue 8750: identity function (no dbg) with List.fold",
        .source =
        \\{
        \\identity = |v| v
        \\xs = [1, 2, 3]
        \\sum = xs->identity()->List.fold(0, |acc, x| acc + x)
        \\sum
        \\}
        ,
        .expected = .{ .inspect_str = "6.0" },
    },
    .{
        .name = "issue 8750: direct List.fold without wrapper",
        .source =
        \\{
        \\xs = [1, 2, 3]
        \\sum = xs->List.fold(0, |acc, x| acc + x)
        \\sum
        \\}
        ,
        .expected = .{ .inspect_str = "6.0" },
    },
    .{
        .name = "issue 8750: dbg in polymorphic function with List.len",
        .source =
        \\{
        \\debug = |v| {
        \\    dbg v
        \\    v
        \\}
        \\xs = [1, 2, 3]
        \\len = xs->debug()->List.len()
        \\len
        \\}
        ,
        .expected = .{ .inspect_str = "3" },
    },
    .{
        .name = "issue 8750: block without dbg before List.fold",
        .source =
        \\{
        \\wrap = |v| { v }
        \\xs = [1, 2, 3]
        \\sum = xs->wrap()->List.fold(0, |acc, x| acc + x)
        \\sum
        \\}
        ,
        .expected = .{ .inspect_str = "6.0" },
    },
    .{
        .name = "issue 8750: dbg of constant before returning v with List.fold",
        .source =
        \\{
        \\debug = |v| {
        \\    dbg 42
        \\    v
        \\}
        \\xs = [1, 2, 3]
        \\sum = xs->debug()->List.fold(0, |acc, x| acc + x)
        \\sum
        \\}
        ,
        .expected = .{ .inspect_str = "6.0" },
    },
    .{
        .name = "issue 8765: Box.unbox with record containing numeric literal",
        .source =
        \\{
        \\update = |boxed| {
        \\    { count } = Box.unbox(boxed)
        \\    count + 1
        \\}
        \\initial = Box.box({ count: 0 })
        \\result = update(initial)
        \\result
        \\}
        ,
        .expected = .{ .inspect_str = "1.0" },
    },
    .{
        .name = "boxed lambda round trip: non-capturing lambda called multiple times",
        .source =
        \\{
        \\f = Box.unbox(Box.box(|x| x + 1))
        \\f(1) + f(2) + f(3)
        \\}
        ,
        .expected = .{ .inspect_str = "9.0" },
    },
    .{
        .name = "boxed lambda round trip: capturing lambda called multiple times",
        .source =
        \\{
        \\capture1 = 10
        \\capture2 = 20
        \\boxed = Box.box(|a, b| a + b + capture1 + capture2)
        \\f = Box.unbox(boxed)
        \\f(1, 2) + f(3, 4)
        \\}
        ,
        .expected = .{ .inspect_str = "70.0" },
    },
    .{
        .name = "boxed lambda round trip: unboxed callable joins with original callable",
        .source =
        \\{
        \\x = 3
        \\id = |z| x + z
        \\g = |y| id(x + y)
        \\g_boxed = Box.box(g)
        \\g_unboxed = Box.unbox(g_boxed)
        \\choice = if Bool.True A else B
        \\match choice {
        \\    A => g_unboxed(2)
        \\    B => g(2)
        \\}
        \\}
        ,
        .expected = .{ .inspect_str = "8.0" },
    },
    .{
        .name = "boxed lambda round trip: promoted callable from interpreted erased value",
        .source =
        \\{
        \\make_boxed = |_| Box.box(|x| x + 1)
        \\add1 = Box.unbox(make_boxed({}))
        \\add1(41)
        \\}
        ,
        .expected = .{ .inspect_str = "42.0" },
    },
    .{
        .name = "boxed lambda round trip: branch-selected interpreted erased value",
        .source =
        \\{
        \\choose_boxed = |pick|
        \\    if pick {
        \\        Box.box(|x| x + 1)
        \\    } else {
        \\        Box.box(|x| x + 10)
        \\    }
        \\add = Box.unbox(choose_boxed(Bool.True))
        \\add(41)
        \\}
        ,
        .expected = .{ .inspect_str = "42.0" },
    },
    .{
        .name = "boxed lambda round trip: pass boxed lambda through helpers",
        .source =
        \\{
        \\make = |n| |x| x + n
        \\wrap = |boxed| { value: boxed }
        \\unwrap = |record| record.value
        \\f = Box.unbox(unwrap(wrap(Box.box(make(5)))))
        \\f(1) + f(2)
        \\}
        ,
        .expected = .{ .inspect_str = "13.0" },
    },
    .{
        .name = "boxed lambda round trip: rebox after helper chain",
        .source =
        \\{
        \\make = |n| |x| x + n
        \\wrap = |boxed| { value: boxed }
        \\unwrap = |record| record.value
        \\boxed = wrap(Box.box(make(3)))
        \\reboxed = Box.box(Box.unbox(unwrap(boxed)))
        \\f = Box.unbox(reboxed)
        \\f(1) + f(2)
        \\}
        ,
        .expected = .{ .inspect_str = "9.0" },
    },
    .{
        .name = "boxed lambda round trip: stored in record",
        .source =
        \\{
        \\make = |n| |x| x + n
        \\holder = { boxed: Box.box(make(4)) }
        \\f = Box.unbox(holder.boxed)
        \\f(2) + f(3)
        \\}
        ,
        .expected = .{ .inspect_str = "13.0" },
    },
    .{
        .name = "boxed lambda round trip: stored in tag union",
        .source =
        \\{
        \\make = |n| |x| x + n
        \\boxed = Box.box(make(6))
        \\tagged = if Bool.True Ok(boxed) else Err(boxed)
        \\f = Box.unbox(match tagged {
        \\    Ok(value) => value
        \\    Err(value) => value
        \\})
        \\f(1) + f(2)
        \\}
        ,
        .expected = .{ .inspect_str = "15.0" },
    },
    .{
        .name = "boxed lambda round trip: polymorphic identity specialized after unboxing",
        .source =
        \\{
        \\identity = |x| x
        \\id_num = Box.unbox(Box.box(identity))
        \\id_str = Box.unbox(Box.box(identity))
        \\{ n: id_num(41), s: id_str("ok") }
        \\}
        ,
        .expected = .{ .inspect_str = "{ n: 41.0, s: \"ok\" }" },
    },
    .{
        .name = "boxed lambda round trip: closes over polymorphic value",
        .source =
        \\{
        \\make_const = |value| |_| value
        \\num_f = Box.unbox(Box.box(make_const(41)))
        \\str_f = Box.unbox(Box.box(make_const("ok")))
        \\{ n: num_f({}), s: str_f({}) }
        \\}
        ,
        .expected = .{ .inspect_str = "{ n: 41.0, s: \"ok\" }" },
    },
    .{
        .name = "boxed lambda round trip: direct proc-value capture transform",
        .source_kind = .module,
        .source =
        \\make_boxed_runner : (I64 -> I64) -> Box((I64 -> I64))
        \\make_boxed_runner = |f| Box.box(|x| {
        \\        boxed = Box.box(f)
        \\        run = Box.unbox(boxed)
        \\        run(x)
        \\})
        \\
        \\main : I64
        \\main = {
        \\    boxed = make_boxed_runner(|n| n + 1)
        \\    run = Box.unbox(boxed)
        \\    run(41)
        \\}
        ,
        .expected = .{ .inspect_str = "42" },
    },
    .{
        .name = "boxed lambda round trip: proc value captures erased callable",
        .source_kind = .module,
        .source =
        \\make_runner : (I64 -> I64) -> (I64 -> I64)
        \\make_runner = |f| |x| {
        \\        boxed = Box.box(f)
        \\        run = Box.unbox(boxed)
        \\        run(x)
        \\}
        \\
        \\main : I64
        \\main = {
        \\    runner = make_runner(|n| n + 1)
        \\    runner(41)
        \\}
        ,
        .expected = .{ .inspect_str = "42" },
    },
    .{
        .name = "boxed lambda round trip: branch join packs finite closure into erased result",
        .source_kind = .module,
        .source =
        \\make_boxed : {} -> Box((I64 -> I64))
        \\make_boxed = |_| Box.box(|x| x + 1)
        \\
        \\choose : Bool -> (I64 -> I64)
        \\choose = |use_box| {
        \\    boxed = make_boxed({})
        \\    if use_box {
        \\        Box.unbox(boxed)
        \\    } else {
        \\        |n| n + 10
        \\    }
        \\}
        \\
        \\main : I64
        \\main = choose(Bool.True)(41) + choose(Bool.False)(41)
        ,
        .expected = .{ .inspect_str = "93" },
    },
    .{
        .name = "boxed lambda round trip: erased function argument transform",
        .source_kind = .module,
        .source =
        \\make_boxed : {} -> Box(((I64 -> I64) -> I64))
        \\make_boxed = |_| Box.box(|f| f(41))
        \\
        \\apply_boxed : (I64 -> I64) -> I64
        \\apply_boxed = Box.unbox(make_boxed({}))
        \\
        \\main : I64
        \\main = apply_boxed(|x| x + 1)
        ,
        .expected = .{ .inspect_str = "42" },
    },
    .{
        .name = "boxed lambda round trip: erased function result transform",
        .source_kind = .module,
        .source =
        \\make_boxed : {} -> Box((I64 -> (I64 -> I64)))
        \\make_boxed = |_| Box.box(|n| |x| x + n)
        \\
        \\make_adder : I64 -> (I64 -> I64)
        \\make_adder = Box.unbox(make_boxed({}))
        \\
        \\main : I64
        \\main = make_adder(5)(10)
        ,
        .expected = .{ .inspect_str = "15" },
    },
    .{
        .name = "boxed lambda round trip: erased record callable field transform",
        .source_kind = .module,
        .source =
        \\make_boxed : {} -> Box(({ f : (I64 -> I64) } -> I64))
        \\make_boxed = |_| Box.box(|r| (r.f)(1))
        \\
        \\apply_record : { f : (I64 -> I64) } -> I64
        \\apply_record = Box.unbox(make_boxed({}))
        \\
        \\main : I64
        \\main = apply_record({ f: |x| x + 1 })
        ,
        .expected = .{ .inspect_str = "2" },
    },
    .{
        .name = "boxed lambda round trip: erased list callable element transform",
        .source_kind = .module,
        .source =
        \\make_boxed : {} -> Box((List((I64 -> I64)) -> U64))
        \\make_boxed = |_| Box.box(|fs| List.len(fs))
        \\
        \\apply_list : List((I64 -> I64)) -> U64
        \\apply_list = Box.unbox(make_boxed({}))
        \\
        \\main : U64
        \\main = apply_list([|x| x + 1, |x| x + 10])
        ,
        .expected = .{ .inspect_str = "2" },
    },
    .{
        .name = "boxed lambda round trip: erased tag payload callable transform",
        .source_kind = .module,
        .source =
        \\make_boxed : {} -> Box(([Apply((I64 -> I64)), Keep(I64)] -> I64))
        \\make_boxed = |_| Box.box(|value| {
        \\    match value {
        \\        Apply(f) => f(1)
        \\        Keep(n) => n
        \\    }
        \\})
        \\
        \\apply_tag : [Apply((I64 -> I64)), Keep(I64)] -> I64
        \\apply_tag = Box.unbox(make_boxed({}))
        \\
        \\main : I64
        \\main = apply_tag(Apply(|x| x + 1)) + apply_tag(Keep(7))
        ,
        .expected = .{ .inspect_str = "9" },
    },
    .{
        .name = "boxed lambda round trip: boxed callable captures boxed callable",
        .source_kind = .module,
        .source =
        \\make_outer : {} -> Box((I64 -> I64))
        \\make_outer = |_| {
        \\    inner = Box.box(|x| x + 1)
        \\
        \\    Box.box(|x| Box.unbox(inner)(x) + 1)
        \\}
        \\
        \\main : I64
        \\main = Box.unbox(make_outer({}))(40)
        ,
        .expected = .{ .inspect_str = "42" },
    },
    .{
        .name = "boxed lambda round trip: nested box does not authorize unrelated erasure",
        .source_kind = .module,
        .source =
        \\make_boxed : {} -> Box(({ inner : Box((I64 -> I64)) } -> I64))
        \\make_boxed = |_| Box.box(|record| Box.unbox(record.inner)(1))
        \\
        \\apply_record : { inner : Box((I64 -> I64)) } -> I64
        \\apply_record = Box.unbox(make_boxed({}))
        \\
        \\main : I64
        \\main = apply_record({ inner: Box.box(|x| x + 1) })
        ,
        .expected = .{ .inspect_str = "2" },
    },
    .{
        .name = "non-boxed function containers stay finite callable values",
        .source =
        \\{
        \\record = { f: |x| x + 1 }
        \\tagged = if Bool.True Apply(record.f) else Keep(|x| x + 10)
        \\list = [record.f, |x| x + 100]
        \\first = match List.first(list) {
        \\    Ok(f) => f
        \\    Err(_) => |x| x
        \\}
        \\match tagged {
        \\    Apply(f) => f(1) + first(1)
        \\    Keep(f) => f(1)
        \\}
        \\}
        ,
        .expected = .{ .inspect_str = "4.0" },
    },
    .{
        .name = "host boundary: unboxed lambda is rejected",
        .source_kind = .module,
        .source =
        \\import Platform
        \\
        \\main = Platform.apply!(|x: I64| x)
        ,
        .imports = &.{.{
            .name = "Platform",
            .source =
            \\apply! : (I64 -> I64) -> I64 => {}
            ,
        }},
        .expected = .{ .problem = {} },
    },
    .{
        .name = "issue 8555: method call syntax list.first() with match on Result",
        .source =
        \\{
        \\list : List(U8)
        \\list = [8.U8, 7.U8]
        \\val = match list.first() {
        \\    Err(_) => 0.U8
        \\    Ok(first) => first
        \\}
        \\val
        \\}
        ,
        .expected = .{ .inspect_str = "8" },
    },
    .{
        .name = "low_level - polymorphic Try callback keeps inactive Err branch",
        .source =
        \\{
        \\keep_oks : List(a), (a -> Try(ok, _err)) -> List(ok)
        \\keep_oks = |list, fun| {
        \\    list.fold(
        \\        [],
        \\        |out_list, elem| {
        \\            match fun(elem) {
        \\                Ok(result) => out_list.append(result)
        \\                Err(_) => out_list
        \\            }
        \\        },
        \\    )
        \\}
        \\
        \\always_ok_n = |_| Ok(1)
        \\keep_oks([10], always_ok_n)
        \\}
        ,
        .expected = .{ .inspect_str = "[1.0]" },
    },
    .{
        .name = "low_level - polymorphic Try callback keeps active Err payload",
        .source =
        \\{
        \\keep_oks : List(a), (a -> Try(ok, _err)) -> List(ok)
        \\keep_oks = |list, fun| {
        \\    list.fold(
        \\        [],
        \\        |out_list, elem| {
        \\            match fun(elem) {
        \\                Ok(result) => out_list.append(result)
        \\                Err(_) => out_list
        \\            }
        \\        },
        \\    )
        \\}
        \\
        \\always_err = |_| Err("bad")
        \\keep_oks([10], always_err)
        \\}
        ,
        .expected = .{ .inspect_str = "[]" },
    },
    .{
        .name = "issue 8750: List.fold render value",
        .source =
        \\{
        \\    xs = [1, 2, 3]
        \\    sum = xs->List.fold(0, |acc, x| acc + x)
        \\    sum
        \\}
        ,
        .expected = .{ .inspect_str = "6.0" },
    },
    .{
        .name = "low_level - List.set replaces element at index",
        .source =
        \\{
        \\list = Try.ok_or(List.set([1, 2, 3], 1, 9), [])
        \\Try.ok_or(List.get(list, 1), 0)
        \\}
        ,
        .expected = .{ .inspect_str = "9.0" },
    },
    .{
        .name = "low_level - List.set preserves untouched elements",
        .source =
        \\{
        \\list = Try.ok_or(List.set([1, 2, 3], 1, 9), [])
        \\Try.ok_or(List.get(list, 2), 0)
        \\}
        ,
        .expected = .{ .inspect_str = "3.0" },
    },
    .{
        .name = "low_level - List.set on refcounted List(Str)",
        .source =
        \\{
        \\list = Try.ok_or(List.set(["cat", "chases", "rat"], 1, "loves"), [])
        \\Try.ok_or(List.get(list, 1), "")
        \\}
        ,
        .expected = .{ .inspect_str = "\"loves\"" },
    },
    .{
        .name = "low_level - List.set out of bounds returns Err",
        .source =
        \\{
        \\match List.set([1, 2, 3], 9, 0) {
        \\    Ok(_) => "ok"
        \\    Err(_) => "err"
        \\}
        \\}
        ,
        .expected = .{ .inspect_str = "\"err\"" },
    },
    .{
        .name = "low_level - List.replace returns updated list and prev",
        .source =
        \\{
        \\result = Try.ok_or(List.replace([10, 20, 30], 1, 99), { list: [], prev: 0 })
        \\result.prev
        \\}
        ,
        .expected = .{ .inspect_str = "20.0" },
    },
    .{
        .name = "low_level - List.replace updated list element",
        .source =
        \\{
        \\result = Try.ok_or(List.replace([10, 20, 30], 1, 99), { list: [], prev: 0 })
        \\Try.ok_or(List.get(result.list, 1), 0)
        \\}
        ,
        .expected = .{ .inspect_str = "99.0" },
    },
    .{
        .name = "low_level - List.replace on refcounted List(Str)",
        .source =
        \\{
        \\result = Try.ok_or(List.replace(["a", "b", "c"], 0, "z"), { list: [], prev: "" })
        \\result.prev
        \\}
        ,
        .expected = .{ .inspect_str = "\"a\"" },
    },
    .{
        .name = "low_level - List.update applies function at index",
        .source =
        \\{
        \\list = Try.ok_or(List.update([10, 20, 30], 1, |x| x + 5), [])
        \\Try.ok_or(List.get(list, 1), 0)
        \\}
        ,
        .expected = .{ .inspect_str = "25.0" },
    },
    .{
        .name = "low_level - List.swap exchanges two elements",
        .source =
        \\{
        \\list = Try.ok_or(List.swap([1, 2, 3, 4], 0, 3), [])
        \\Try.ok_or(List.get(list, 0), 0)
        \\}
        ,
        .expected = .{ .inspect_str = "4.0" },
    },
    .{
        .name = "low_level - List.swap preserves the other swapped index",
        .source =
        \\{
        \\list = Try.ok_or(List.swap([1, 2, 3, 4], 0, 3), [])
        \\Try.ok_or(List.get(list, 3), 0)
        \\}
        ,
        .expected = .{ .inspect_str = "1.0" },
    },
    .{
        .name = "low_level - List.swap on refcounted List(Str)",
        .source =
        \\{
        \\list = Try.ok_or(List.swap(["cat", "chases", "rat"], 0, 2), [])
        \\Try.ok_or(List.get(list, 0), "")
        \\}
        ,
        .expected = .{ .inspect_str = "\"rat\"" },
    },
};
