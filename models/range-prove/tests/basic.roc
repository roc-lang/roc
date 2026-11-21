app [main!] { pf: platform "../../../test/fx/platform/main.roc" }

import pf.Stdin
import pf.Stdout

f : List(U8), U64 -> U64
f = |bs, k| {
    n = List.len(bs)
    if n > 0 {
        i = if k < n { k } else { n - 1 }
        b = match List.get(bs, i) { Ok(v) => v, Err(_) => 13 }
        x = b.to_u64() + 10
        y = if x > 300 { 1 } else { x // (x + 1) }
        z = if n >= 8 { n - 8 } else { 0 }
        y + z + (if x.plus_overflows(1) { 5 } else { 6 })
    } else {
        0
    }
}

main! = || {
    line = Stdin.line!()
    bs = line.to_utf8()
    k = match U64.from_str(line.trim()) { Ok(v) => v, Err(_) => 3 }
    Stdout.line!(f(bs, k).to_str())
}
