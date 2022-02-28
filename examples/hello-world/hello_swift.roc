app "hello_swift"
    packages { pf: "swift-platform" }
    imports []
    provides [ main ] to pf

main = "Hello, World!"
