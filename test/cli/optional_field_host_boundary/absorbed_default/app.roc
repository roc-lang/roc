app [main!] { pf: platform "./platform/platform.roc" }

main! : List(Str) => { a : U8, b : Str ?? "hi" }
main! = |_args| { a: 1 }
