# Principal types of small functions: the model's printed type must match
# REPL `:t` up to renaming, and `roc check` must accept.
H := [H].{}

c_id = |x| x

c_tag = |x| A(x)

c_if = |b, x| if b A(x) else B

c_match = |t| match t { A => 1.U64, B(n) => n }

c_matchw = |t| match t { A => 1.U64, _ => 2.U64 }

c_fwd = |t| match t { A => t, _ => B }

c_acc = |r| r.foo.bar

c_rec = |x| { a: x, b: A }

c_list = |x| [x, A]

c_nested = |t| match t { A(B) => 1.U64, A(C) => 2.U64 }

c_nestedw = |t| match t { A(B) => 1.U64, _ => 2.U64 }

c_q = |t| {
    v = t?
    Ok(Wrap(v))
}

c_q2 = |t, b| {
    v = t?
    if b Err(Bad) else Ok(v)
}

c_ok = |x| Ok(x)

c_ann_open : [A, B] -> [A, B]
c_ann_open = |t| t

c_block = |x| {
    w = W(x)
    [w, Z]
}

c_letpoly = |x| {
    f = |z| P(z)
    { a: f(x), b: f(1.U64) }
}

c_alias = c_tag

c_eta = |q| c_tag(q)
