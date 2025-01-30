platform "test-platform"
    requires {} { main : _ }
    exposes []
    packages {}
    imports []
    provides [main_for_host]

Expr : [String Str, Concat Expr Expr]

main_for_host : {} -> Expr
main_for_host = \{} -> main
