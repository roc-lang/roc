import IOErr exposing [IOErr]

B := [].{
    unit! : Str => Try({}, [BErr(IOErr)])
    str! : Str => Try(Str, [BErr(IOErr)])
    bytes! : Str => Try(List(U8), [BErr(IOErr)])
    record! : Str => Try({ stdout : List(U8), stderr : List(U8), code : I32 }, [BErr(IOErr)])
    nested! : Str => Try({ stdout : List(U8) }, Try({ stderr : List(U8), code : I32 }, IOErr))
}
