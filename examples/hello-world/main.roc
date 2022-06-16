app "hello"
    packages { pf: "c-platform" }
    imports []
    provides [main] to pf

main = "Hello, World!\n"
