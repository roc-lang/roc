app "multi-dep-thunk"
    packages { pf: "../../test-platform-simple-zig/main.roc" }
    imports [Dep1]
    provides [main] to pf

main : Str
main = Dep1.value1 {}
