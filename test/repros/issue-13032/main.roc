app [] {}

W : { out : List(U8), level : U64 }

push : W -> Try(W, [Oops])
push = |w| Ok({ out: w.out.append(1), level: w.level })

slot : W, (W -> Try(W, [Oops])) -> Try(W, [Oops])
slot = |w, f| {
    level = w.level
    r = f(w)?
    if r.level == level Ok(r) else Ok(r)
}

main! = |args| {
    n = U64.from_str(args.get(0) ?? "0") ?? 0
    var $acc = Ok({ out: [], level: 0 })
    for _ in (0.U64).until(n) {
        $acc = match $acc {
            Ok(w) => slot(w, push)
            Err(e) => Err(e)
        }
    }
    match $acc {
        Ok(w) => echo!("${w.out.len().to_str()}\n")
        Err(_) => echo!("err\n")
    }
    Ok({})
}
