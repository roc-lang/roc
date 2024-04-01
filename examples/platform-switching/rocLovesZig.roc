app [main] { pf: platform "zig-platform/main.roc" }

helper = \a, b -> Str.concat a b

main = helper "Roc <" "3 Zig!\n"
