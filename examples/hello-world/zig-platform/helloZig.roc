app "helloZig"
    packages { pf: "main.roc" }
    imports []
    provides [main] to pf

expect 1 == 1

main = "Hello, World!\n"
