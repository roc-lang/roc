app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout
import pf.Host

# Regression test: abs and abs_diff on values the compiler cannot see through.
# - float abs_diff subtracts as floats and keeps the fractional part
# - I128.abs_diff yields every bit of a difference wider than I128's range
# - the magnitude of negative zero is positive zero

main! = || {
    runtime = Host.get_greeting!(Host.new("abs"))
    z : I128
    z = if Str.count_utf8_bytes(runtime) > 0 { 0 } else { 1 }

    i128_highest : I128
    i128_highest = z + 170141183460469231731687303715884105727
    i128_lowest : I128
    i128_lowest = z - 170141183460469231731687303715884105727 - 1
    Stdout.line!("I128 highest abs_diff lowest: ${Str.inspect(i128_highest.abs_diff(i128_lowest))}")
    Stdout.line!("I128 lowest abs_diff highest: ${Str.inspect(i128_lowest.abs_diff(i128_highest))}")
    Stdout.line!("I128 highest abs_diff -1: ${Str.inspect(i128_highest.abs_diff(z - 1))}")

    a : F64
    a = z.to_f64() + 7.5
    b : F64
    b = z.to_f64() + 2.25
    Stdout.line!("F64 7.5 abs_diff 2.25: ${Str.inspect(a.abs_diff(b))}")
    Stdout.line!("F64 2.25 abs_diff 7.5: ${Str.inspect(b.abs_diff(a))}")
    Stdout.line!("F64 -0.5 abs_diff 2.25: ${Str.inspect((a - 8.0).abs_diff(b))}")
    Stdout.line!("F64 abs of -0: ${Str.inspect((z.to_f64() * -1.0).abs())}")

    c : F32
    c = z.to_f32() + 7.5
    d : F32
    d = z.to_f32() + 2.25
    Stdout.line!("F32 7.5 abs_diff 2.25: ${Str.inspect(c.abs_diff(d))}")
    Stdout.line!("F32 abs of -0: ${Str.inspect((z.to_f32() * -1.0).abs())}")
}
