app [run!] { pf: platform "./platform/main.roc" }

import pf.Host
import TailCarry

run! : Str => Str
run! = |input| {
    x = Str.count_utf8_bytes(input)
    n = 32.U64
    wide = { a: x, b: 2.U64, c: 3.U64, d: 4.U64, e: 5.U64 }
    before = Host.alloc_count!()
    f = TailCarry.forward(x, n)
    after_forward = Host.alloc_count!()
    m = TailCarry.mixed(x, n)
    after_mixed = Host.alloc_count!()
    c = TailCarry.closed(x, n)
    after_closed = Host.alloc_count!()
    w = TailCarry.forward(wide, n)
    after_wide = Host.alloc_count!()

    if f == x and m == x * 65536 and c == x and w == wide {
        "forward ${(after_forward - before).to_str()}, mixed ${(after_mixed - after_forward).to_str()}, closed ${(after_closed - after_mixed).to_str()}, wide ${(after_wide - after_closed).to_str()}"
    } else {
        "wrong tail result"
    }
}
