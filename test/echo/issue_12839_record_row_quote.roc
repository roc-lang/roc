# A custom string literal whose conversion requires the generated JSON codec
# of an anonymous record type. `people!` returns that record, so a caller could
# lift it into a nominal type with its own encoder: the conversion depends on
# the instance of `people!` its caller fixes, and `main!` fixes it at the record
# type. A warm `roc check` must reuse the converted value under either strategy
# instead of specializing the program again to convert it.
# https://github.com/roc-lang/roc/issues/12839
Query(row) := { text : Str }.{
    from_quote : Str -> Try(Query(row), [BadQuotedBytes(Str)])
        where [row.Json.Encodable([])]
    from_quote = |raw| if Str.is_empty(raw) Err(BadQuotedBytes("empty query")) else Ok({ text: raw })

    text : Query(row) -> Str
    text = |query| query.text
}

Person : { name : Str, age : U64 }

Db := { url : Str }.{
    rows! : Db, Query(row) => Try(List(row), [Unreachable(Str)])
    rows! = |db, query| if Str.is_empty(db.url) Err(Unreachable(query.text())) else Ok([])
}

people! : Db => Try(List(Person), [Unreachable(Str)])
people! = |db| db.rows!("select name, age from people")

main! = |_args| {
    match people!({ url: "" }) {
        Ok(found) => echo!(Str.inspect(List.len(found)))
        Err(Unreachable(text)) => echo!(text)
    }
    Ok({})
}
