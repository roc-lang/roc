app "helloZig"
    packages { pf: "main.roc" }
    imports []
    provides [main] to pf

expect 2 == 2

expect 3 == 2

main = "Hello, World!\n"
