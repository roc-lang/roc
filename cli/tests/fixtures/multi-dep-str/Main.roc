app "multi-dep-str" imports [ Dep1 ] provides [ main ] to "./platform"

main : Str
main = Dep1.str1
