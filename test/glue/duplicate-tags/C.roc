import IOErr exposing [IOErr]

C := [].{
    unit! : Str => Try({}, [CErr(IOErr)])
    str! : Str => Try(Str, [CErr(IOErr)])
    bytes! : Str => Try(List(U8), [CErr(IOErr)])
    record! : Str => Try({ stdout : List(U8), stderr : List(U8), code : I32 }, [CErr(IOErr)])
    nested! : Str => Try({ stdout : List(U8) }, Try({ stderr : List(U8), code : I32 }, IOErr))
}
