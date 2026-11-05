# A method whose inferred type is an anonymous recursive type, reached through
# a method call on a value whose type is still a flex variable, reports the
# anonymous recursion like a call by its qualified name does.
Expr := [Leaf(U64), Next(Expr)].{
    count = |e, n| match e {
        Leaf(_) => n
        Next(inner) => Expr.count(inner, n)
    }
}

go = |e, x| e.count(x)

expect {
    x : U64
    x = 2
    go(Expr.Leaf(3), x) == 2
}
