platform "jvm-interop"
    requires {} { interpolate : arg -> ret }
    exposes []
    packages {}
    imports []
    provides [stringInterpolation]

stringInterpolation : I32 -> Str
stringInterpolation = \arg -> interpolate arg
