app "multi-dep-str"
    packages { base: "platform" }
    imports [ Dep1 ]
    provides [ main ] to base

main : Str
main = Dep1.str1
