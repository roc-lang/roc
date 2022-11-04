app "multi-dep-str"
    packages { pf: "platform/main.roc" }
    imports [Dep1]
    provides [main] to pf

main : Str
main = Dep1.str1
