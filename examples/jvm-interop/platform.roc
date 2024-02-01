platform "jvm-interop"
    requires {} { program : _ }
    exposes []
    packages {}
    provides [programForHost]

programForHost : {
    interpolateString : (Str -> Str) as InterpolateString,
    mulArrByScalar : (List I32, I32 -> List I32) as MulArrByScalar,
    factorial : (I64 -> I64) as Factorial,
}
programForHost = program
