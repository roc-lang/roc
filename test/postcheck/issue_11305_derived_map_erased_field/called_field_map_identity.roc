app [main!] { pf: platform "./platform/main.roc" }

# A generic function stored in a record field whose body maps with an
# identity callback, projected out and called at a concrete type.

hooks : {} -> { render : _ }
hooks = |{}| { render: |values| values.map(|value| value) }

main! : List(Str) => Try({}, [Exit(I32), ..])
main! = |_args| {
    render = hooks({}).render
    if render([1, 2]) == [1, 2] {
        Ok({})
    } else {
        crash "render returned the wrong elements"
    }
}
