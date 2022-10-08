app "rocLovesWebAssembly"
    packages { pf: "web-assembly-platform/main.roc" }
    imports []
    provides [main] to pf

main = "Roc <3 Web Assembly!\n"
