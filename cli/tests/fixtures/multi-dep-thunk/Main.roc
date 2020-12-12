app "multi-dep-thunk"
    packages { base: "platform" }
    imports [ Dep1 ]
    provides [ main ] to base

main : Str
main = Dep1.value1 {}
