app "formatted"
    packages { pf: "platform/main.roc" } imports []
    provides [main] to pf

main : Str
main = Dep1.value1 {}
