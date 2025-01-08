platform "jvm-interop"
    requires {} { program : _ }
    exposes []
    packages {}
    imports []
    provides [program_for_host]

program_for_host : {
    interpolate_string : (Str -> Str) as InterpolateString,
    mul_arr_by_scalar : (List I32, I32 -> List I32) as MulArrByScalar,
    factorial : (I64 -> I64) as Factorial,
}
program_for_host = program
