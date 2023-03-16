platform "jvm-interop"
    requires {} { interpolateString : _}
    # uncomment to test record
    # requires {} { program : _ }
    exposes []
    packages {}
    imports []
    # uncomment to test record
    # provides [programForHost]
    provides [interpolateStringy]


# uncomment to test record
# programForHost : {
#     interpolateString : (Str -> Str) as InterpolateString,
#     # mulArrByScalar : (List I32 -> List I32) as MulArrByScalar,
# }
# programForHost = program


# interpolateStringy : Str -> Str
interpolateStringy = \arg -> interpolateString arg


# mulArrByScalar : List I32 -> List I32
# mulArrByScalar = \arr -> mulArrayByScalar arr
