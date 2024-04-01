app "rocLovesZig"
    packages { pf: "zig-platform/main.roc" }
    imports []
    provides [main] to pf

helper = \a, b -> Str.concat a b

main = helper "Roc <" "3 Zig!\n"
