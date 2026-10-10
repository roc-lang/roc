app [main!] { pf: platform "./platform/main.roc" }

# A generic function stored in a record field, projected out and called at a
# concrete type, with a direct call in its body.

hooks = |{}| { render: |values| List.len(values) }

main! : List(Str) => Try({}, [Exit(I32), ..])
main! = |_args| {
    render = hooks({}).render
    if render([1, 2, 3]) == 3 {
        Ok({})
    } else {
        crash "render returned the wrong length"
    }
}
