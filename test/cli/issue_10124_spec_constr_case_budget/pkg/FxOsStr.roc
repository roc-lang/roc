import FxPath exposing [FxPath]

FxOsStr := [Utf8(Str), UnixBytes(List(U8)), WindowsU16s(List(U16))].{
    to_raw : FxOsStr -> [Utf8(Str), UnixBytes(List(U8)), WindowsU16s(List(U16))]
    to_raw = |value|
        match value {
            Utf8(str) => Utf8(str)
            UnixBytes(bytes) => UnixBytes(bytes)
            WindowsU16s(units) => WindowsU16s(units)
        }

    to_path : FxOsStr -> FxPath
    to_path = |value| FxPath.from_raw(to_raw(value))

    to_str_try : FxOsStr -> Try(Str, [InvalidStr(U64)])
    to_str_try = |value| FxPath.to_str(to_path(value))
}
