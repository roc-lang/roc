app "rocLovesRust"
    packages { pf: "rust-platform/main.roc" }
    imports []
    provides [main] to pf

main : { a : U64, b : U128 }
main = { a: 0, b: 1, doStuff: \str -> "Roc <3 \(str)!\n" }
# main : Str -> Str
# main =
