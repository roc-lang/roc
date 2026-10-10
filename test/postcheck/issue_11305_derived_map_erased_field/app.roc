app [main!] { pf: platform "./platform/main.roc" }

# `render`'s receiver type is never instantiated, so its `map` dispatch has
# no value to run on: the receiver settles on the empty tag union and the
# dispatch is statically unreachable.

hooks : {} -> { render : _ }
hooks = |{}| { render: |values| values.map(|value| value) }

main! : List(Str) => Try({}, [Exit(I32), ..])
main! = |_args| {
    _ = hooks({})
    Ok({})
}
