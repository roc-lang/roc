app "example"
    packages { pf : "path" }
    imports [ pf.Stdout ]
    provides [ main ] to pf

main = Stdout.line "Hello"