# The appended items' function values reach the result only through
# `List.append_sublist` itself.

make : Str -> (Str -> Str)
make = |prefix| {
    |suffix| "${prefix}${suffix}"
}

render_all : List({ render : Str -> Str }) -> List(Str)
render_all = |records| {
    var $rendered = []
    for record in records {
        render = record.render
        $rendered = List.append($rendered, render("!"))
    }
    $rendered
}

expect {
    first = [{ render: make("a") }]
    second = [{ render: make("b") }, { render: make("c") }]
    render_all(List.append_sublist(first, second, { start: 1, len: 1 })) == ["a!", "c!"]
}

expect {
    first = []
    second = [{ render: make("b") }, { render: make("c") }]
    render_all(List.append_sublist(first, second, { start: 0, len: 2 })) == ["b!", "c!"]
}
