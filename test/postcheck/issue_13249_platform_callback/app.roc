app [Model, main!] { pf: platform "./pf/main.roc" }

Model : { n : U64 }

main! : Model => Model
main! = |m| { n: m.n + 1 }
