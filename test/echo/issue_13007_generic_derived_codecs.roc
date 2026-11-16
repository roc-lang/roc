# Records and tuples built from `where`-constrained type variables encode and
# parse through the generic functions that constrain them. Each derived codec
# reads its type variables through the function's own requirements, so one
# definition serves every call site's types. The values come from the
# program's arguments, so the codecs run in the built program. Two type
# variables instantiated at one type, a variable beside a field of the same
# concrete type, a tag payload, a dict, and a generic function forwarding its
# own evidence to another each keep their codecs apart.

app [main!] {}

line! = |text| echo!("${text}\n")

in_record : a -> Str where [a.Json.Encodable([])]
in_record = |x| Json.to_str({ value: x })

in_tuple : a, b -> Str where [a.Json.Encodable([]), b.Json.Encodable([])]
in_tuple = |x, y| Json.to_str((x, y))

twice : a -> Str where [a.Json.Encodable([])]
twice = |x| Json.to_str({ first: x, both: (x, x) })

nested : a -> Str where [a.Json.Encodable([])]
nested = |x| Json.to_str({ items: [{ v: x }, { v: x }] })

pair_of : a, b -> Str where [a.Json.Encodable([]), b.Json.Encodable([])]
pair_of = |x, y| Json.to_str({ left: x, right: [y], count: 2.U8 })

value_of : Str -> Try(a, [InvalidJson(Str)]) where [a.Json.Parseable([InvalidJson(Str)])]
value_of = |text| {
    parsed : Try({ value : a }, [InvalidJson(Str)])
    parsed = Json.parse(text)
    match parsed {
        Ok(record) => Ok(record.value)
        Err(e) => Err(e)
    }
}

pair_parsed : Str -> Try((a, b), [InvalidJson(Str)]) where [a.Json.Parseable([InvalidJson(Str)]), b.Json.Parseable([InvalidJson(Str)])]
pair_parsed = |text| Json.parse(text)

mixed : a -> Str where [a.Json.Encodable([])]
mixed = |x| Json.to_str({ x: x, y: 5.U64 })

tagged : a -> Str where [a.Json.Encodable([])]
tagged = |x| {
    v : [A(a), B]
    v = A(x)
    Json.to_str(v)
}

tag_of : Str -> Try([A(a), B], [InvalidJson(Str)]) where [a.Json.Parseable([InvalidJson(Str)])]
tag_of = |text| Json.parse(text)

outer : a -> Str where [a.Json.Encodable([])]
outer = |x| in_record(x)

main! = |args| {
    count = args.len()
    text = args.first() ?? "s"
    line!(in_record(count))
    line!(in_record(text))
    line!(in_record({ inner: count }))
    line!(in_record([text, text]))
    line!(in_tuple(count, text))
    line!(in_tuple(text, count))
    line!(in_tuple(Bool.True, { k: text }))
    line!(twice(count))
    line!(twice(text))
    line!(nested(count))
    line!(nested(text))
    line!(pair_of(count, text))
    line!(pair_of(text, { n: count }))
    line!(pair_of(count, count))
    line!(mixed(count))
    line!(mixed(text))
    line!(tagged(count))
    line!(tagged([text]))
    line!(outer({ deep: text }))
    line!(in_record(Dict.empty().insert(text, count)))
    tag : Try([A(U64), B], [InvalidJson(Str)])
    tag = tag_of("{\"A\":${count.to_str()}}")
    match tag {
        Ok(A(n)) => line!("tag ${n.to_str()}")
        Ok(B) => line!("tag B")
        Err(_) => line!("tag failed")
    }
    number : Try(U64, [InvalidJson(Str)])
    number = value_of("{\"value\":${count.to_str()}}")
    word : Try(Str, [InvalidJson(Str)])
    word = value_of("{\"value\":\"${text}\"}")
    both : Try((Str, U64), [InvalidJson(Str)])
    both = pair_parsed("[\"${text}\",7]")
    match (number, word, both) {
        (Ok(n), Ok(w), Ok((p, q))) => line!("parsed ${n.to_str()} ${w} ${p} ${q.to_str()}")
        _ => line!("parse failed")
    }
    Ok({})
}
