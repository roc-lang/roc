platform "test-platform"
    requires {} { main : _ }
    exposes []
    packages {}
    imports []
    provides [main_for_host]

StrRoseTree : [Tree Str (List StrRoseTree)]

main_for_host : StrRoseTree
main_for_host = main
