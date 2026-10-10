app [main!] { pf: platform "./platform/main.roc" }

# A top-level generic function mapping with a numeric callback, called at a
# concrete type.

render = |values| values.map(|value| value + 1)

main! : List(Str) => Try({}, [Exit(I32), ..])
main! = |_args| {
    if render([1, 2]) == [2, 3] {
        Ok({})
    } else {
        crash "render returned the wrong elements"
    }
}
