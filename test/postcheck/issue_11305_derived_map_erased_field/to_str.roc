app [main!] { pf: platform "./platform/main.roc" }

# A never-instantiated receiver whose only dispatch is an ordinary method.

hooks : {} -> { render : _ }
hooks = |{}| { render: |values| values.to_str() }

main! : List(Str) => Try({}, [Exit(I32), ..])
main! = |_args| {
    _ = hooks({})
    Ok({})
}
