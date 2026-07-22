app [main!] { pf: platform "./static-lib-platform/main.roc" }

string_report : U64 -> { actual : Str, expected : Str }
string_report = |seed| {
    roc = if seed == 0 { "Roc" } else { "ROc" }
    trimmed = Str.trim("  ${roc}  ")
    trim_start = Str.trim_start("  ${roc}")
    trim_end = Str.trim_end("${roc}  ")
    lower = Str.with_ascii_lowercased(roc)
    upper = Str.with_ascii_uppercased(roc)
    dropped_prefix = Str.drop_prefix("prefix-${roc}", "prefix-")
    dropped_suffix = Str.drop_suffix("${roc}-suffix", "-suffix")
    repeated = Str.repeat(roc, 2)
    reserved = Str.reserve(roc, 8)
    released = Str.release_excess_capacity(reserved)
    capacity = Str.concat(Str.with_capacity(8), roc)
    joined = Str.join_with([roc, "!"], "")
    split = Str.split_on("${roc},x", ",")
    utf8 = if seed == 0 { [82.U8, 111.U8, 99.U8] } else { [82.U8, 79.U8, 99.U8] }
    decoded = Str.from_utf8(utf8)

    found = match Str.split_first("${roc}:lang", ":") {
        Ok({ before, after }) => { before, after }
        Err(_) => { before: "<missing>", after: "<missing>" }
    }
    caseless_prefix = match Str.drop_prefix_caseless_ascii("Content-${roc}", "content-") {
        Ok(after) => after
        Err(_) => "<missing>"
    }
    expected_inspect = Str.concat(Str.concat("\"", roc), "\"")

    actual =
        \\string trimmed: ${Str.inspect(trimmed)}
        \\string trim start: ${Str.inspect(trim_start)}
        \\string trim end: ${Str.inspect(trim_end)}
        \\string lower: ${Str.inspect(lower)}
        \\string upper: ${Str.inspect(upper)}
        \\string drop prefix: ${Str.inspect(dropped_prefix)}
        \\string drop suffix: ${Str.inspect(dropped_suffix)}
        \\string repeat: ${Str.inspect(repeated)}
        \\string reserve/release: ${Str.inspect(released)}
        \\string with capacity: ${Str.inspect(capacity)}
        \\string join: ${Str.inspect(joined)}
        \\string split count: ${Str.inspect(List.len(split))}
        \\string caseless equals: ${Str.inspect(Str.caseless_ascii_equals(roc, lower))}
        \\string inspect: ${Str.inspect(Str.inspect(roc))}
        \\string find first: ${Str.inspect(found)}
        \\string caseless prefix: ${Str.inspect(caseless_prefix)}
        \\string from utf8: ${Str.inspect(decoded)}

    expected =
        \\string trimmed: ${Str.inspect(roc)}
        \\string trim start: ${Str.inspect(roc)}
        \\string trim end: ${Str.inspect(roc)}
        \\string lower: "roc"
        \\string upper: "ROC"
        \\string drop prefix: ${Str.inspect(roc)}
        \\string drop suffix: ${Str.inspect(roc)}
        \\string repeat: ${Str.inspect(Str.concat(roc, roc))}
        \\string reserve/release: ${Str.inspect(roc)}
        \\string with capacity: ${Str.inspect(roc)}
        \\string join: ${Str.inspect(Str.concat(roc, "!"))}
        \\string split count: 2
        \\string caseless equals: True
        \\string inspect: ${Str.inspect(expected_inspect)}
        \\string find first: ${Str.inspect({ before: roc, after: "lang" })}
        \\string caseless prefix: ${Str.inspect(roc)}
        \\string from utf8: Ok(${Str.inspect(roc)})

    { actual, expected }
}

numeric_report : U64 -> { actual : Str, expected : Str }
numeric_report = |seed| {
    int_text = if seed == 0 { "-42" } else { "-43" }
    dec_text = if seed == 0 { "42.5" } else { "43.5" }
    float_text = if seed == 0 { "1.5" } else { "2.5" }
    parsed_i64 = I64.from_str(int_text) == Ok(if seed == 0 { -42 } else { -43 })
    parsed_dec = Dec.from_str(dec_text) == Ok(if seed == 0 { 42.5 } else { 43.5 })
    parsed_f64 = F64.from_str(float_text) == Ok(if seed == 0 { 1.5 } else { 2.5 })

    dec_base : Dec
    dec_base = if seed == 0 { 2.0 } else { 3.0 }
    dec_square : Dec
    dec_square = if seed == 0 { 9.0 } else { 16.0 }
    dec_zero : Dec
    dec_zero = if seed == 0 { 0.0 } else { 0.5 }
    dec_one : Dec
    dec_one = if seed == 0 { 1.0 } else { 0.5 }
    dec_epsilon : Dec
    dec_epsilon = 0.000000000000001
    dec_pow_ok = Dec.pow(dec_base, 3.0) == (if seed == 0 { 8.0 } else { 27.0 })
    dec_sqrt_ok = Dec.sqrt(dec_square) == (if seed == 0 { 3.0 } else { 4.0 })
    dec_sin_ok = seed != 0 or Dec.abs(Dec.sin(dec_zero)) < dec_epsilon
    dec_cos_ok = seed != 0 or Dec.abs(Dec.cos(dec_zero) - 1.0) < dec_epsilon
    dec_tan_ok = seed != 0 or Dec.abs(Dec.tan(dec_zero)) < dec_epsilon
    dec_asin_ok = seed != 0 or Dec.abs(Dec.asin(dec_zero)) < dec_epsilon
    dec_acos_ok = seed != 0 or Dec.abs(Dec.acos(dec_one)) < dec_epsilon
    dec_atan_ok = seed != 0 or Dec.abs(Dec.atan(dec_zero)) < dec_epsilon

    whole_i128 : I128
    whole_i128 = if seed == 0 { 42 } else { 43 }
    whole_u128 : U128
    whole_u128 = if seed == 0 { 42 } else { 43 }
    whole_dec : Dec
    whole_dec = if seed == 0 { 42.0 } else { 43.0 }
    wide_dec : Dec
    wide_dec = if seed == 0 { 42.5 } else { 43.5 }
    dec_to_i128_ok = Dec.to_i128_try(wide_dec) == Ok(whole_i128)
    dec_to_u128_ok = Dec.to_u128_try(wide_dec) == Ok(whole_u128)
    i128_to_dec_ok = I128.to_dec_try(whole_i128) == Ok(whole_dec)
    u128_to_dec_ok = U128.to_dec_try(whole_u128) == Ok(whole_dec)

    float_base : F64
    float_base = if seed == 0 { 2.0 } else { 3.0 }
    float_zero : F64
    float_zero = if seed == 0 { 0.0 } else { 0.5 }
    float_one : F64
    float_one = if seed == 0 { 1.0 } else { 0.5 }
    float_pow_ok = F64.pow(float_base, 3.0) == (if seed == 0 { 8.0 } else { 27.0 })
    float_sin_ok = seed != 0 or F64.sin(float_zero) == 0.0
    float_cos_ok = seed != 0 or F64.cos(float_zero) == 1.0
    float_tan_ok = seed != 0 or F64.tan(float_zero) == 0.0
    float_asin_ok = seed != 0 or F64.asin(float_zero) == 0.0
    float_acos_ok = seed != 0 or F64.acos(float_one) == 0.0
    float_atan_ok = seed != 0 or F64.atan(float_zero) == 0.0

    actual =
        \\numeric parse i64: ${Str.inspect(parsed_i64)}
        \\numeric parse dec: ${Str.inspect(parsed_dec)}
        \\numeric parse f64: ${Str.inspect(parsed_f64)}
        \\numeric dec pow: ${Str.inspect(dec_pow_ok)}
        \\numeric dec sqrt: ${Str.inspect(dec_sqrt_ok)}
        \\numeric dec sin: ${Str.inspect(dec_sin_ok)}
        \\numeric dec cos: ${Str.inspect(dec_cos_ok)}
        \\numeric dec tan: ${Str.inspect(dec_tan_ok)}
        \\numeric dec asin: ${Str.inspect(dec_asin_ok)}
        \\numeric dec acos: ${Str.inspect(dec_acos_ok)}
        \\numeric dec atan: ${Str.inspect(dec_atan_ok)}
        \\numeric dec to i128: ${Str.inspect(dec_to_i128_ok)}
        \\numeric dec to u128: ${Str.inspect(dec_to_u128_ok)}
        \\numeric i128 to dec: ${Str.inspect(i128_to_dec_ok)}
        \\numeric u128 to dec: ${Str.inspect(u128_to_dec_ok)}
        \\numeric f64 pow: ${Str.inspect(float_pow_ok)}
        \\numeric f64 sin: ${Str.inspect(float_sin_ok)}
        \\numeric f64 cos: ${Str.inspect(float_cos_ok)}
        \\numeric f64 tan: ${Str.inspect(float_tan_ok)}
        \\numeric f64 asin: ${Str.inspect(float_asin_ok)}
        \\numeric f64 acos: ${Str.inspect(float_acos_ok)}
        \\numeric f64 atan: ${Str.inspect(float_atan_ok)}

    expected =
        \\numeric parse i64: True
        \\numeric parse dec: True
        \\numeric parse f64: True
        \\numeric dec pow: True
        \\numeric dec sqrt: True
        \\numeric dec sin: True
        \\numeric dec cos: True
        \\numeric dec tan: True
        \\numeric dec asin: True
        \\numeric dec acos: True
        \\numeric dec atan: True
        \\numeric dec to i128: True
        \\numeric dec to u128: True
        \\numeric i128 to dec: True
        \\numeric u128 to dec: True
        \\numeric f64 pow: True
        \\numeric f64 sin: True
        \\numeric f64 cos: True
        \\numeric f64 tan: True
        \\numeric f64 asin: True
        \\numeric f64 acos: True
        \\numeric f64 atan: True

    { actual, expected }
}

main! : U64 => Str
main! = |seed| {
    strings = string_report(seed)
    numbers = numeric_report(seed)
    actual = Str.concat(strings.actual, numbers.actual)
    expected = Str.concat(strings.expected, numbers.expected)

    if actual == expected {
        "ok"
    } else {
        actual
    }
}
