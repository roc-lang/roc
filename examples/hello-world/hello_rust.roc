app "hello_rust"
    packages { pf: "rust-platform" }
    imports []
    provides [ main ] to pf

main = "Hello, World!"
