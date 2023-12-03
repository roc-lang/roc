app "app"
    packages { pf: "platform.roc" }
    imports []
    provides [main] to pf

main = Tree "root" [Tree "leaf1" [], Tree "leaf2" []]
