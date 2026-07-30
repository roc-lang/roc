app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

Choice := [
    Found(Str),
    Missing,
]

choose : List(Str), Str -> Choice
choose = |items, wanted| {
    match List.find_first(items, |item| item == wanted) {
        Ok(value) => Found(value)
        Err(NotFound) => Missing
    }
}

render : Choice -> Str
render = |choice| {
    match choice {
        Found(value) => Str.concat("found: ", value)
        Missing => "missing"
    }
}

main! = || {
    out = render(choose(["one", "two", "three"], "two"))
    Stdout.line!(out)
    out
}
