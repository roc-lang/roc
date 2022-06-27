app "hello"
    packages { pf: "c-platform/main.roc" }
    imports []
    provides [main] to pf

main = "Hello, World!\n"
