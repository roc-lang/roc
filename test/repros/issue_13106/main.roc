import Foo exposing [Bool]

main! = |_args| {
    echo!(Bool.show(Bool.Yes))
    Ok({})
}
