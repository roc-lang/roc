IOErr := [
    NotFound,
    Other(Str),
]

Host := [].{
    fallible_unit! : Str => Try({}, [HostErr(IOErr)])
    fallible_str! : Str => Try(Str, [HostErr(IOErr)])
    fallible_bytes! : Str => Try(List(U8), [HostErr(IOErr)])
    fallible_record! : Str => Try({ stdout : List(U8), stderr : List(U8), code : I32 }, [HostErr(IOErr)])
    fallible_nested! : Str => Try({ stdout : List(U8) }, Try({ stderr : List(U8), code : I32 }, IOErr))
}
