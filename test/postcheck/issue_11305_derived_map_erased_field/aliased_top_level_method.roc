app [main!] { pf: platform "./platform/main.roc" }

# A top-level generic function dispatching a method, bound to a local and
# called at a concrete type.

render = |values| values.len()

main! : List(Str) => Try({}, [Exit(I32), ..])
main! = |_args| {
    f = render
    if f([1, 2]) == 2 {
        Ok({})
    } else {
        crash "render returned the wrong length"
    }
}
