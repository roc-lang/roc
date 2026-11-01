# `List.map` and `List.update` reuse a unique list's buffer when the input and
# output items share a layout. A closure capturing a `Str` has the layout of a
# `Str`, so each of these reads a function out of a buffer it then overwrites.

make : Str -> (Str -> Str)
make = |prefix| {
    |suffix| "${prefix}${suffix}"
}

expect {
    functions = [make("shared:")]
    List.map(functions, |function| function("value")) == ["shared:value"]
}

expect {
    records = [{ render: make("shared:") }]
    rendered = List.map(records, |record| {
        render = record.render
        render("value")
    })
    rendered == ["shared:value"]
}

expect {
    tags = [Render(make("shared:"))]
    rendered = List.map(tags, |tag| {
        match tag {
            Render(render) => render("value")
        }
    })
    rendered == ["shared:value"]
}

expect {
    functions = [make("first:"), make("second:")]
    updated = List.update(functions, 0, |function| make(function("value")))
    match updated {
        Ok(list) => List.map(list, |function| function("!")) == ["first:value!", "second:!"]
        Err(_) => Bool.False
    }
}

expect {
    functions = [make("kept:")]
    kept = List.map(functions, |function| function)
    match List.first(kept) {
        Ok(function) => function("value") == "kept:value"
        Err(_) => Bool.False
    }
}
