Query := [].{
    Sql(a) := { text : Str }.{
        from_quote : Str -> Try(Sql(a), [BadQuotedBytes(Str)])
        from_quote = |_| {
            dbg "literal conversion obligation"
            Err(BadQuotedBytes("rejected"))
        }
    }

    get : U64 -> Sql(a)
    get = |_n| "select 1"
}
