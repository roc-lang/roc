app "test-app" provides [ main ] imports [ Dep1 ]

main : Str
main = Dep1.value1 {}
