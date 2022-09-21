app "app"
    packages { pf: "platform.roc" }
    imports [pf.Dep1, pf.Dep2]
    provides [main] to pf

main = {s1: Dep1.string "hello", s2: Dep2.string "world"}
