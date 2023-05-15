app "roc-app"
    packages { pf: "platform/main.roc" }
    imports []
    provides [main] to pf

main : Str
main = "Hello from Roc!"
