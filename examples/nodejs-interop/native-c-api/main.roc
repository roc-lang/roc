app "libhello"
    packages { pf: "platform/main.roc" }
    imports []
    provides [main] to pf

main : Str -> Str
main = \message ->
    "TypeScript said to Roc: \(message)! 🎉"
