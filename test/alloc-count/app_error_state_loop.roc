app [run!] { pf: platform "./platform/main.roc" }

import pf.Host

# Loops that pass their list to a fallible call and keep the old list when the
# call fails. The callee fails before touching the list, so the old list is
# never needed alongside the appended one and each loop should allocate only
# as the list grows, not once per iteration.
consume : List(U64), U64 -> Try(List(U64), [TooLong])
consume = |list, item| {
    if list.len() > 1000000000 { Err(TooLong) } else { Ok(list.append(item)) }
}

error_state_read_always : U64 -> U64
error_state_read_always = |n| {
    var $list = []
    var $failed = Bool.False
    var $index = 0
    while $index < n and !$failed {
        match consume($list, $index) {
            Ok(next) => { $list = next }
            Err(TooLong) => { $failed = Bool.True }
        }
        $index = $index + 1
    }
    $list.len()
}

error_state_read_on_success : U64 -> U64
error_state_read_on_success = |n| {
    var $list = []
    var $failed = Bool.False
    var $index = 0
    while $index < n and !$failed {
        match consume($list, $index) {
            Ok(next) => { $list = next }
            Err(TooLong) => { $failed = Bool.True }
        }
        $index = $index + 1
    }
    if $failed { 0 } else { $list.len() }
}

fallback : U64 -> U64
fallback = |n| {
    var $list = []
    var $index = 0
    while $index < n {
        $list = match consume($list, $index) {
            Ok(next) => next
            Err(TooLong) => $list
        }
        $index = $index + 1
    }
    $list.len()
}

early_return : U64 -> Try(U64, [TooLong])
early_return = |n| {
    var $list = []
    var $index = 0
    while $index < n {
        $list = consume($list, $index)?
        $index = $index + 1
    }
    Ok($list.len())
}

count! : (U64 -> U64), U64 => U64
count! = |f, n| {
    before = Host.alloc_count!()
    _ = f(n)
    Host.alloc_count!() - before
}

run! : Str => Str
run! = |_input| {
    n = 1024
    counts = [
        count!(error_state_read_always, n),
        count!(error_state_read_on_success, n),
        count!(fallback, n),
        count!(|m| early_return(m) ?? 0, n),
    ]
    # Growing a list to 1024 elements takes a handful of allocations; copying
    # it on every iteration takes one per element.
    linear = counts.all(|allocs| allocs < 64)
    shown = Str.join_with(counts.map(|allocs| allocs.to_str()), " ")
    "allocations: ${shown}, linear: ${if linear "yes" else "no"}"
}
