app "hello-c"
    packages { pf: "platform" }
    imports []
    provides [ main ] to pf

main = "Hello, World!\n"
