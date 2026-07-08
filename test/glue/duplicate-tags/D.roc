import IOErr exposing [IOErr]

D := [].{
    unit! : Str => Try({}, [DErr(IOErr)])
    str! : Str => Try(Str, [DErr(IOErr)])
    bytes! : Str => Try(List(U8), [DErr(IOErr)])
    record! : Str => Try({ stdout : List(U8), stderr : List(U8), code : I32 }, [DErr(IOErr)])
    nested! : Str => Try({ stdout : List(U8) }, Try({ stderr : List(U8), code : I32 }, IOErr))
}
