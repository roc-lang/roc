import IOErr exposing [IOErr]

A := [].{
    unit! : Str => Try({}, [AErr(IOErr)])
    str! : Str => Try(Str, [AErr(IOErr)])
    bytes! : Str => Try(List(U8), [AErr(IOErr)])
    record! : Str => Try({ stdout : List(U8), stderr : List(U8), code : I32 }, [AErr(IOErr)])
    nested! : Str => Try({ stdout : List(U8) }, Try({ stderr : List(U8), code : I32 }, IOErr))
}
