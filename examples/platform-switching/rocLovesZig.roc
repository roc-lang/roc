app "rocLovesZig"
    packages { pf: "zig-platform/main.roc" }
    imports []
    provides [main] to pf

Expr : [Var, Val I64, Add Expr Expr, Mul Expr Expr]

mkExpr : I64 -> Expr
mkExpr = \n ->
    when n is
        0 -> Var 
        _ -> Add (mkExpr (n-1)) Var 

main : Str
main = 
    when mkExpr 1 is
        Var -> "var"
        Add _ _ -> "add"
        _ -> "other"
        
    
