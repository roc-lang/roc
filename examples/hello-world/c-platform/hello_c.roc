app "hello_c"
    packages { pf: "." }
    imports []
    provides [ main ] to pf

main = "Hello, World!"
