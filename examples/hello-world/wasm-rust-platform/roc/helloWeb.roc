app "helloWeb"
    packages { pf: "../platform" }
    imports []
    provides [ main ] to pf

main = "Hello, World!\n"
