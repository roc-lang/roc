app [run!] { pf: platform "./platform/main.roc" }

import pf.Host

Buffers : { items : List(U64), other : List(U64) }

push : Buffers, U64 -> Try(Buffers, [Full])
push = |buffers, item| {
    { items, other } = buffers
    if items.len() > 1000000000 {
        Err(Full)
    } else {
        Ok({ items: items.append(item), other })
    }
}

fill : U64 -> U64
fill = |n| {
    var $buffers = { items: [], other: [] }
    var $index = 0
    while $index < n {
        $buffers = match push($buffers, $index) {
            Ok(pushed) => pushed
            Err(Full) => crash "unreachable"
        }
        $index = $index + 1
    }
    $buffers.items.len()
}

fill_twice : U64 -> U64
fill_twice = |n| {
    var $buffers = { items: [], other: [] }
    var $index = 0
    while $index < n {
        $buffers = match push($buffers, $index) {
            Ok(pushed) => { ..pushed, items: pushed.items.append($index) }
            Err(Full) => crash "unreachable"
        }
        $index = $index + 1
    }
    $buffers.items.len()
}

run! : Str => Str
run! = |input| {
    n = Str.count_utf8_bytes(input).to_u64() * 64
    before = Host.alloc_count!()
    once = fill(n)
    middle = Host.alloc_count!()
    twice = fill_twice(n)
    after = Host.alloc_count!()
    once_allocs = middle - before
    twice_allocs = after - middle
    expect once_allocs < 32
    expect twice_allocs < 32

    "items: ${once.to_str()} ${twice.to_str()}, allocations: ${once_allocs.to_str()} ${twice_allocs.to_str()}"
}
