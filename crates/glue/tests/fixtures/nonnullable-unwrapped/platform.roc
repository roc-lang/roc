platform "test-platform"
    requires {} { main : _ }
    exposes []
    packages {}
    provides [mainForHost]

StrRoseTree : [Tree Str (List StrRoseTree)]

mainForHost : StrRoseTree
mainForHost = main
