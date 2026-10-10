app [main!] { pf: platform "./platform/main.roc" }

# A generic function stored in a record field whose body calls `map`,
# projected out and called at a concrete type.

hooks : {} -> { render : _ }
hooks = |{}| { render: |values| values.map(|value| value + 1) }

main! : List(Str) => Try({}, [Exit(I32), ..])
main! = |_args| {
    render = hooks({}).render
    if render([1, 2]) == [2, 3] {
        Ok({})
    } else {
        crash "render returned the wrong elements"
    }
}
