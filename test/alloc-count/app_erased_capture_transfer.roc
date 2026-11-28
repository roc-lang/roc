app [run!] { pf: platform "./platform/main.roc" }

import pf.Host

# #13374: the unreachable Box paths force erasure. One has a reusable
# result slot; the list of machines in Spare makes its result ambiguous.
One := [Step((U64 -> (One, U64))), Idle]
Spare := [Step((U64 -> (Spare, U64))), Spare(List(Spare))]

one : List(U64) -> One
one = |xs| Step(|x| {
        next = xs.append(x)
        (one(next), next.len())
})

# Return the same single-slot shape as the machine, but make the captured
# values observable. Closing a successor returns a variant with no callable.
shared_one : List(U64) -> One
shared_one = |xs| Step(|x| {
        last = xs.last() ?? 0
        if x == U64.highest (Idle, last) else (shared_one(xs.append(x)), last)
})

spare : List(U64) -> Spare
spare = |xs| Step(|x| {
        next = xs.append(x)
        (spare(next), next.len())
})

tail_one : One, U64 -> (One, U64)
tail_one = |m, x| match m {
        Step(f) => f(x)
        Idle => tail_one(one([]), x)
}

bound_one : One, U64 -> (One, U64)
bound_one = |m, x| match m {
        Step(f) => {
                (next, len) = f(x)
                (next, len)
        }
        Idle => bound_one(one([]), x)
}

tail_spare : Spare, U64 -> (Spare, U64)
tail_spare = |m, x| match m {
        Step(f) => f(x)
        Spare(ms) => tail_spare(ms.first() ?? spare([]), x)
}

bound_spare : Spare, U64 -> (Spare, U64)
bound_spare = |m, x| match m {
        Step(f) => {
                (next, len) = f(x)
                (next, len)
        }
        Spare(ms) => bound_spare(ms.first() ?? spare([]), x)
}

run_one : U64, Bool -> U64
run_one = |n, tail| {
        var $m = if n == 0 Box.unbox(Box.box(one([]))) else one([])
        var $len = 0
        var $i = 0
        while $i < n {
                (next, len) = if tail tail_one($m, $i) else bound_one($m, $i)
                $m = next
                $len = len
                $i = $i + 1
        }
        $len
}

run_spare : U64, Bool -> U64
run_spare = |n, tail| {
        var $m = if n == 0 Box.unbox(Box.box(spare([]))) else spare([])
        var $len = 0
        var $i = 0
        while $i < n {
                (next, len) = if tail tail_spare($m, $i) else bound_spare($m, $i)
                $m = next
                $len = len
                $i = $i + 1
        }
        $len
}

measure! : U64, Bool, Bool => (U64, U64)
measure! = |n, single, tail| {
        before_bytes = Host.alloc_bytes!()
        before_allocs = Host.alloc_count!()
        len = if single run_one(n, tail) else run_spare(n, tail)
        allocs = Host.alloc_count!() - before_allocs
        bytes = Host.alloc_bytes!() - before_bytes
        if len == n (bytes, allocs) else crash "wrong list length"
}

check! : U64, Bool, Bool => Str
check! = |n, single, tail| {
        (small, _) = measure!(n, single, tail)
        (large, calls) = measure!(2 * n, single, tail)
        # Copying the list each step approaches 4x requested bytes. A fresh
        # shell each step is only linear, so check its count independently.
        if large >= 3 * small {
                crash "quadratic capture copies"
        } else if single and calls >= n {
                crash "result shell not reused"
        } else {
                "${small.to_str()}/${large.to_str()} bytes, ${calls.to_str()} calls"
        }
}

run! : Str => Str
run! = |input| {
        seed = input.count_utf8_bytes()
        # Taking a dying unit from a runtime-sized list does not imply that
        # the allocation is unique. Both successors must preserve their update.
        siblings = List.repeat(shared_one([seed]), 2 + U64.rem_by(seed, 2))
        { list, prev } = siblings.replace(0, Idle) ?? crash "no sibling"
        (left, old_left) = tail_one(prev, seed + 10)
        (right, old_right) = bound_one(list.get(1) ?? crash "no sibling", seed + 20)
        (_, left_last) = tail_one(left, U64.highest)
        (_, right_last) = bound_one(right, U64.highest)
        n = 2_032 + seed
        a = check!(n, True, True)
        b = check!(n, True, False)
        c = check!(n, False, True)
        d = check!(n, False, False)
        if old_left == seed and old_right == seed and left_last == seed + 10 and right_last == seed + 20 {
                "erased captures: ${a}, ${b}, ${c}, ${d}; distinct successors"
        } else {
                crash "shared closure corrupted"
        }
}
