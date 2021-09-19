app "hello-world"
    packages { base: "platform" }
    imports []
    provides [ main ] to base

main = "Hello, World!\n"
