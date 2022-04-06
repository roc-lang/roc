app "formatted"
    packages { pf: "platform" }
  provides [ main ] to pf

main : Str
main = Dep1.value1 {}
