app "rocLovesRust"
    packages { pf: "rust-platform/main.roc" }
    imports []
    provides [main] to pf

main = 
    x = "4"
    doStuff =
        if Bool.true then
            \str -> "Roc <3 \(str)\(x)!\n"
        else
            \str -> "Roc does not like \(str)\(x)!\n"
        
    { a: 0, b: 1, doStuff }
