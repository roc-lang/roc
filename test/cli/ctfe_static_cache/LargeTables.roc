app [main!] { pf: platform "../../fx/platform/main.roc" }

import pf.Stdin
import pf.Stdout

build : U64, (U64 -> a) -> List(a)
build = |len, item| {
    var $out = List.with_capacity(len)
    var $index = 0.U64
    while $index < len {
        $out = List.append($out, item($index))
        $index = $index + 1
    }
    $out
}

## A NaN whose sign and payload are not Roc's one NaN.
odd_nan : F64
odd_nan = F64.from_bits(18442240474082181121)

bytes : List(U8)
bytes = build(5000, |index| (index * 7).to_u8_wrap())

wides : List(I128)
wides = build(300, |index| (index.to_i128() - 150) * 1000000000000000000000000000000)

floats : List(F64)
floats = build(1000, |index| if index % 7 == 0 { odd_nan } else { index.to_f64() / 4.0 })

points : List({ x : F64, y : U32 })
points = build(1000, |index| { x: if index % 5 == 0 { odd_nan } else { index.to_f64() }, y: index.to_u32_wrap() * 3 })

literals : List(U16)
literals = [1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233, 377, 610, 987, 1597, 2584, 4181, 6765, 10946, 17711, 28657, 46368]

main! = || {
    start = Str.count_utf8_bytes(Stdin.line!())
    byte_sum = bytes.fold(start, |sum, byte| sum + byte.to_u64())
    wide_sum = wides.fold(0.I128, |sum, wide| sum + wide / 1000000000000000000000000000)
    float_nans = floats.fold(0.U64, |count, float| if float.is_nan() { count + 1 } else { count })
    float_bits = floats.fold(0.U64, |sum, float| sum + float.to_bits() % 1000)
    point_nans = points.fold(0.U64, |count, point| if point.x.is_nan() { count + 1 } else { count })
    point_sum = points.fold(0.U64, |sum, point| sum + point.x.to_bits() % 1000 + point.y.to_u64())
    literal_sum = literals.fold(0.U64, |sum, literal| sum + literal.to_u64())
    Stdout.line!("${byte_sum.to_str()} ${wide_sum.to_str()} ${float_nans.to_str()} ${float_bits.to_str()} ${point_nans.to_str()} ${point_sum.to_str()} ${literal_sum.to_str()}")
}
