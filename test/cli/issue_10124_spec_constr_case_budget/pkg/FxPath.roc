FxPath := [Utf8(Str), Unix(List(U8)), Windows(List(U16))].{
    from_raw : [Utf8(Str), UnixBytes(List(U8)), WindowsU16s(List(U16))] -> FxPath
    from_raw = |raw|
        match raw {
            Utf8(str) => Utf8(str)
            UnixBytes(bytes) => Unix(bytes)
            WindowsU16s(units) => Windows(units)
        }

    to_str : FxPath -> Try(Str, [InvalidStr(U64)])
    to_str = |path|
        match path {
            Utf8(str) => Ok(str)
            Unix(bytes) =>
                match Str.from_utf8(bytes) {
                    Ok(str) => Ok(str)
                    Err(BadUtf8({ index, problem: _ })) => Err(InvalidStr(index))
                }
            Windows(units) => utf16_to_str(units)
        }
}

utf16_to_str : List(U16) -> Try(Str, [InvalidStr(U64)])
utf16_to_str = |remaining|
    match remaining {
        [] => Ok("")
        [_, .. as rest] => utf16_to_str(rest)
    }
