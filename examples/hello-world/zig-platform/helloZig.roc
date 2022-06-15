app "helloZig"
    packages { pf: "." }
    imports []
    provides [main] to pf

expect 1 == 1

main = "Hello, World!\n"
