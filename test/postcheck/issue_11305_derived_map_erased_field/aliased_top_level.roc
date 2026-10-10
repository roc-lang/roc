app [main!] { pf: platform "./platform/main.roc" }

# A top-level generic function that calls `map`, bound to a local and called
# at a concrete type.

render = |values| values.map(|value| value + 1)

main! : List(Str) => Try({}, [Exit(I32), ..])
main! = |_args| {
    f = render
    if f([1, 2]) == [2, 3] {
        Ok({})
    } else {
        crash "render returned the wrong elements"
    }
}
