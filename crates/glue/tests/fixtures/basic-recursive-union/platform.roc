platform "test-platform"
    requires {} { main : _ }
    exposes []
    packages {}
    provides [mainForHost]

Expr : [String Str, Concat Expr Expr]

mainForHost : {} -> Expr
mainForHost = \{} -> main
