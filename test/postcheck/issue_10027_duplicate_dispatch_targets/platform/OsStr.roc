OsStr := [Utf8(Str)].{
    from_quote : Str -> Try(OsStr, [BadQuotedBytes(Str)])
    from_quote = |str| Ok(Utf8(str))
}
