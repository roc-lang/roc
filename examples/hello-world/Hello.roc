app "hello-world"
    packages { pf: "platform" }
    imports []
    provides [ main ] to pf

main = { content: "Hello, World!\n", other: "" }
