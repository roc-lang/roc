app [main!] { pf: platform "../../fx/platform/main.roc" }

import Tags
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

## Evaluated `Bool`s and a literal table of tags in the reading module.
local_flags : List(Bool)
local_flags = build(4000, |index| index % 7 < 3)

local_lights : List([Red, Amber, Green])
local_lights = [Green, Green, Amber, Red, Green]

light_weight : [Red, Amber, Green] -> U64
light_weight = |light| match light {
    Red => 1
    Amber => 2
    Green => 3
}

local_marks : List([Hit({}), Miss])
local_marks = [Hit({}), Miss, Hit({})]

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
    flag_count = Tags.flags.fold(0.U64, |count, flag| if flag { count + 1 } else { count })
    local_flag_count = local_flags.fold(0.U64, |count, flag| if flag { count + 1 } else { count })
    heading_sum = Tags.headings.fold(0.U64, |sum, heading| sum + Tags.weight(heading))
    picked = match Tags.headings.get(start + 1234) {
        Ok(heading) => Tags.weight(heading)
        Err(_) => 0
    }
    light_sum = Tags.lights.fold(0.U64, |sum, light| sum + light_weight(light)) * 100 + local_lights.fold(0.U64, |sum, light| sum + light_weight(light))
    cell_sum = Tags.cells.fold(0.U64, |sum, cell| if cell.alive and cell.light == Red { sum + cell.age.to_u64() } else { sum })
    mark_sum = Tags.marks.fold(0.U64, |sum, mark| match mark {
        Hit(unit) => if unit == {} { sum + 1 } else { sum }
        Miss => sum + 10
        Skip(unit, only) => match only {
            Only => if unit == {} { sum + 100 } else { sum }
        }
    })
    mark_equal = match Tags.marks.get(start + 2) {
        Ok(mark) => if mark == Miss { "eq" } else { "ne" }
        Err(_) => "none"
    }
    Stdout.line!("${Str.inspect(Tags.marks.take_first(start + 1))} ${Str.inspect(local_marks)} ${mark_sum.to_str()} ${mark_equal}")
    Stdout.line!("${byte_sum.to_str()} ${wide_sum.to_str()} ${float_nans.to_str()} ${float_bits.to_str()} ${point_nans.to_str()} ${point_sum.to_str()} ${literal_sum.to_str()} ${flag_count.to_str()} ${local_flag_count.to_str()} ${heading_sum.to_str()} ${picked.to_str()} ${light_sum.to_str()} ${cell_sum.to_str()}")
}
