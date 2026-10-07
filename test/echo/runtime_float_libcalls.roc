# Float operations that some target has no instruction for, with operands
# driven by a runtime value so they reach the backends instead of being folded
# at compile time. No target has a float remainder instruction, and a baseline
# x86-64 CPU has none for rounding either, so the compiled program calls
# `fmod`, `floor`, `ceil` and `trunc` (and their `f`-suffixed F32 forms). A
# default-platform executable links no libc and gets them from the platform
# itself.
main! = |args| {
    count = args.len()

    wide : F64
    wide = count.to_f64() + 7.5

    wide_divisor : F64
    wide_divisor = count.to_f64() + 2.0

    narrow : F32
    narrow = count.to_f32() + 7.5

    narrow_divisor : F32
    narrow_divisor = count.to_f32() + 2.0

    rem = "${wide.rem_by(wide_divisor).to_str()} ${narrow.rem_by(narrow_divisor).to_str()} ${(0.0 - wide).rem_by(wide_divisor).to_str()}"
    div_trunc = "${wide.div_trunc_by(wide_divisor).to_str()} ${narrow.div_trunc_by(narrow_divisor).to_str()}"
    floor = "${Str.inspect(wide.floor_to_i64_try())} ${Str.inspect(narrow.floor_to_i64_try())}"
    ceiling = "${Str.inspect(wide.ceiling_to_i64_try())} ${Str.inspect(narrow.ceiling_to_i64_try())}"

    echo!("rem: ${rem}, div_trunc: ${div_trunc}, floor: ${floor}, ceiling: ${ceiling}")
    Ok({})
}
