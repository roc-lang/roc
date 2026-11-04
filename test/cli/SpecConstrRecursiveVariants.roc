# A recursive union whose variants each carry the union itself. SpecConstr's
# productivity analysis must give `show` a summary bounded by the type's
# shape, however many variants lead back to it.

Expr := [Lit(Str), T0(Expr), T1(Expr, Expr), T2(Expr), T3(Expr, Expr), T4(Expr), T5(Expr, Expr)]

show : Expr -> Str
show = |expr| match expr {
    Lit(text) => text
    T0(a) => "T0(${show(a)})"
    T1(a, b) => "T1(${show(a)}, ${show(b)})"
    T2(a) => "T2(${show(a)})"
    T3(a, b) => "T3(${show(a)}, ${show(b)})"
    T4(a) => "T4(${show(a)})"
    T5(a, b) => "T5(${show(a)}, ${show(b)})"
}

build : List(U8), U64, U64 -> (Expr, U64)
build = |bytes, pos, depth| {
    c = U8.to_u64(bytes.get(pos) ?? 0)
    if depth == 0 {
        (Lit("x"), pos + 1)
    } else {
        (l, p) = build(bytes, pos + 1, depth - 1)
        if c == 0 {
            return (T0(l), p)
        }
        if c == 1 {
            (r, q) = build(bytes, p, depth - 1)
            return (T1(l, r), q)
        }
        if c == 2 {
            return (T2(l), p)
        }
        if c == 3 {
            (r, q) = build(bytes, p, depth - 1)
            return (T3(l, r), q)
        }
        if c == 4 {
            return (T4(l), p)
        }
        if c == 5 {
            (r, q) = build(bytes, p, depth - 1)
            return (T5(l, r), q)
        }
        (l, p)
    }
}

expect {
    (expr, _) = build([1, 0, 4], 0, 2)
    show(expr) == "T1(T0(x), T0(x))"
}

expect {
    (expr, _) = build([], 0, 0)
    show(expr) == "x"
}
