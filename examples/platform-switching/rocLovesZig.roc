app "rocLovesZig"
    packages { pf: "zig-platform/main.roc" }
    imports []
    provides [main] to pf

main = \{} -> 
    captureMe = Bool.true 
    \_input -> 
        if captureMe then
            "Roc <3 Zig!\n"
        else
            "Roc <3 Zig!\n"
