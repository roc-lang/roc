OsStr := [Utf8(Str)].{
    display : OsStr -> Str
    display = |self|
        match self {
            Utf8(str) => str
        }

    to_str_try : OsStr -> Try(Str, [InvalidStr(U64)])
    to_str_try = |self| Ok(OsStr.display(self))
}
