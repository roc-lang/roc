app [run!] { pf: platform "./platform/main.roc" }

import pf.Host

# Repro for https://github.com/roc-lang/roc/issues/13032.
# Keeping a copied scalar across f(w)? must not keep w.out shared.
W : { out : List(U8), level : U64 }

push : W -> Try(W, [Oops])
push = |w| Ok({ out: w.out.append(1), level: w.level })

slot : W, (W -> Try(W, [Oops])) -> Try(W, [Oops])
slot = |w, f| {
    level = w.level
    r = f(w)?
    if r.level == level Ok(r) else Ok(r)
}

grow : U64 -> U64
grow = |n| {
    var $acc = Ok({ out: [], level: 0 })
    for _ in (0.U64).until(n) {
        $acc = match $acc {
            Ok(w) => slot(w, push)
            Err(e) => Err(e)
        }
    }
    match $acc {
        Ok(w) => w.out.len()
        Err(_) => crash "unexpected push error"
    }
}

run! : Str => Str
run! = |input| {
    n = input.count_utf8_bytes() * 32
    before = Host.alloc_count!()
    len = grow(n)
    allocations = Host.alloc_count!() - before
    # Geometric list growth needs only a handful of allocations for 512
    # appends. Allow growth-policy variation, but not a copy on every append.
    bounded = allocations <= 32
    "length: ${len.to_str()}, bounded allocations: ${Str.inspect(bounded)}, allocations: ${allocations.to_str()}"
}
