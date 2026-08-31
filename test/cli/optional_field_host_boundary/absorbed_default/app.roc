app [main!] { pf: platform "./platform/platform.roc" }

HostRec := { a : U8, b : Str ?? "hi" }

main! : List(Str) => HostRec
main! = |_args| HostRec.{ a: 1 }
