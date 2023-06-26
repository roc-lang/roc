app "app"
    packages { pf: "platform/main.roc" }
    imports []
    provides [main] to pf

main = \str -> "hi, \(str)!!"
