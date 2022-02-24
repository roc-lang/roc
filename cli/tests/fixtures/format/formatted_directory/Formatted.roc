app "formatted"
    packages { pf: "platform" } imports []
    provides [ main ] to pf

main : Str
main = Dep1.value1 {}
