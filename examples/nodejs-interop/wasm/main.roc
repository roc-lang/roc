app "roc-app"
    packages { pf: "platform/main.roc" }
    provides [main] to pf

main : Str
main = "Hello from Roc!"
