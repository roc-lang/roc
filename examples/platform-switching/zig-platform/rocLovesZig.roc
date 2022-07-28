app "rocLovesZig"
    packages { pf: "main.roc" }
    imports []
    provides [main] to pf

main = "Roc <3 Zig!\n"

expect 
    a = 1
    b = 2 + 2


    a == b
