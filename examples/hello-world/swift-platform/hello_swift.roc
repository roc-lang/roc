app "hello_swift"
    packages { pf: "." }
    imports []
    provides [ main ] to pf

main = "Hello, World!\n"
