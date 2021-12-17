app "multi-dep-thunk"
    packages { pf: "platform" }
    imports [ Dep1 ]
    provides [ main ] to pf

main : Str
main = Dep1.value1 {}
