app "multi-dep-str"
    packages { pf: "../../test-platform-simple-zig/main.roc" }
    imports [Dep1]
    provides [main] to pf

main : Str
main = Dep1.str1
