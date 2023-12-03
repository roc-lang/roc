app "multi-dep-thunk"
    packages { pf: "platform/main.roc" }
    imports [Dep1]
    provides [main] to pf

main : Str
main = Dep1.value1 {}
