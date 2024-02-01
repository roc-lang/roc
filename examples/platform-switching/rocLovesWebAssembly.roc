app "rocLovesWebAssembly"
    packages { pf: "web-assembly-platform/main.roc" }
    provides [main] to pf

main = "Roc <3 Web Assembly!\n"
