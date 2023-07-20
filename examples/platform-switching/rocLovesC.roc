app "rocLovesC"
    packages { pf: "c-platform/main.roc" }
    imports [pf.Base64]
    provides [main] to pf

main = Base64.toStr "blah"
