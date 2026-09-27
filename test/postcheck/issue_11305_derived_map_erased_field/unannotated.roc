app [main!] { pf: platform "./platform/main.roc" }

# The same never-instantiated `map` receiver, with the returned record's type
# inferred rather than annotated.

hooks = |{}| { render: |values| values.map(|value| value) }

main! : List(Str) => Try({}, [Exit(I32), ..])
main! = |_args| {
    _ = hooks({})
    Ok({})
}
