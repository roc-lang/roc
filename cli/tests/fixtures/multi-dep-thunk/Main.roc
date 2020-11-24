app "multi-dep-thunk" imports [ Dep1 ] provides [ main ] to "./platform"

main : Str
main = Dep1.value1 {}
