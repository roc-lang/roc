platform "test-platform"
    requires {} { main : _ }
    exposes []
    packages {}
    imports []
    provides [mainForHost]

Expr : [String Str, Concat Expr Expr]

mainForHost : {} -> Expr
mainForHost = \{} -> main
