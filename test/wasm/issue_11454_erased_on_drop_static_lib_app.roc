app [main!] { pf: platform "./static-lib-platform/main.roc" }

# A boxed closure's captures are released through the `on_drop` callback stored
# in its allocation. Two capture layouts give two different callbacks, so the
# call stays a `call_indirect`, which checks the callee's signature. The seed
# comes from the host, so none of this is evaluated at compile time.
#
# Every captured string is too long to be a small string, so both captures hold
# heap allocations. The runner's allocation balance check then fails if either
# `on_drop` is skipped or releases nothing.
greet : Str -> Box(Str -> Str)
greet = |greeting| Box.box(|name| "${greeting} ${name}")

count : List(Str) -> Box(Str -> Str)
count = |items| Box.box(|name| "${name} ${items.len().to_str()}")

main! = |seed| {
    tag = seed.to_str()

    a = greet(tag.concat(" hello from a heap string"))
    b = count([tag.concat(" heap string number one"), tag.concat(" heap string number two")])

    called = "${Box.unbox(a)("x")}, ${Box.unbox(b)("y")}"

    if called == "0 hello from a heap string x, y 2" {
        "ok"
    } else {
        "bad"
    }
}
