app "rocLovesC"
    packages { pf: "c-platform/main.roc" }
    imports [pf.Dep]
    provides [main] to pf

main = Dep.strIdentity "blah"
