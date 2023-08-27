app "rocLovesZig"
    packages { pf: "zig-platform/main.roc" }
    imports []
    provides [main] to pf

main = 
    f = (\a,b -> Pair a b)
    l = List.map2 [1,2,3] [3,2,1] f

    when List.len l is
        _ -> "Roc <3 Zig!\n"
