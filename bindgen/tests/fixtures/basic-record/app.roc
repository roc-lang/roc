app "basic-record"
    packages { pf: "platform" }
    imports []
    provides [ main ] to pf

main = { a: 1995, b: 42 }
