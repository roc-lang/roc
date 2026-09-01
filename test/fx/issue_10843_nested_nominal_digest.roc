app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

W(a) := [W(a)].{
    mk : a -> W(a)
    mk = |x| W(x)

    get : W(a) -> a
    get = |w| match w {
        W(x) => x
    }
}

peel : W(W(W(W(W(W(W(W(U64)))))))) -> U64
peel = |v| W.get(W.get(W.get(W.get(W.get(W.get(W.get(W.get(v))))))))

main! = || {
    nested = W.mk(W.mk(W.mk(W.mk(W.mk(W.mk(W.mk(W.mk(0.U64))))))))
    Stdout.line!(peel(nested).to_str())
}
