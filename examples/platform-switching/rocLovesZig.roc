app "rocLovesZig"
    packages { pf: "zig-platform/main.roc" }
    imports []
    provides [main] to pf

main = "Roc <3 Zig!\n"
