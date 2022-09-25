platform "test-platform"
    requires {} { main : _ }
    exposes []
    packages {}
    imports []
    provides [mainForHost]

StrRoseTree : [Tree Str (List StrRoseTree)]

mainForHost : StrRoseTree
mainForHost = main
