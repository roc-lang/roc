app "hello_zig"
    packages { pf: "zig-platform" }
    imports []
    provides [ main ] to pf

main = "Hello, World!"
