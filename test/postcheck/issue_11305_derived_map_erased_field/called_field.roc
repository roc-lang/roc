app [main!] { pf: platform "./platform/main.roc" }

# A generic function stored in a record field, projected out and called at a
# concrete type.

hooks : {} -> { render : _ }
hooks = |{}| { render: |values| values.len() }

main! : List(Str) => Try({}, [Exit(I32), ..])
main! = |_args| {
    render = hooks({}).render
    if render([1, 2]) == 2 {
        Ok({})
    } else {
        crash "render returned the wrong length"
    }
}
