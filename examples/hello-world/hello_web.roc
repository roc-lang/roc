app "hello_web"
    packages { pf: "web-platform" }
    imports []
    provides [ main ] to pf

main = "Hello, World!"
