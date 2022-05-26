app "helloC"
    packages { pf: "." }
    imports []
    provides [main] to pf

main = "Hello, World!\n"
