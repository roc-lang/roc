# repro for https://github.com/roc-lang/roc/issues/9827 (platform-free
# adaptation of the basic-cli Sqlite decoder shape; cf #8848 one level
# shallower). A nested lambda returned from another lambda interprets one
# value via `?` into a tag union and a second value via a non-`?` call into
# another tag union, with forward references to helpers defined below.
# Panicked with "trying to add var at rank 3, but current rank is 2".
Stmt : { id : U64 }

str_dec : Str -> (List(Str) -> (Stmt => Try(Str, [FieldNotFound(Str), ..])))
str_dec = |_name| |_cols| |_stmt| Ok("todo")

nullable_i64_dec : Str -> (List(Str) -> (Stmt => Try([Null, NotNull(I64)], [FieldNotFound(Str), ..])))
nullable_i64_dec = |_name| |_cols| |_stmt| Ok(NotNull(1))

main! = |_args| {
    dec = decode(["status", "edited"])
    row = dec({ id: 1 })?
    match row.status {
        Todo => {}
        _ => {
            crash "expected Todo status"
        }
    }
    Ok({})
}

decode = |cols|
    |stmt| {
        status_str = str_dec("status")(cols)(stmt)?
        status = decode_status(status_str)?
        edited_raw = nullable_i64_dec("edited")(cols)(stmt)?
        edited = decode_edited(edited_raw)
        Ok({ status, edited })
    }

decode_status = |s|
    match s {
        "todo" => Ok(Todo)
        _ => Err(ParseError("x"))
    }

decode_edited = |e|
    match e {
        NotNull(1) => Edited
        _ => Unknown
    }
