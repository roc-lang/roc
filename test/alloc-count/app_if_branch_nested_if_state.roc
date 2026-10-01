app [run!] { pf: platform "./platform/main.roc" }

import pf.Host

# Repro for https://github.com/roc-lang/roc/issues/11933
#
# An `if` statement reassigns `$a` in one branch, and that branch ends in a
# nested `if` that reassigns `$b` in one arm. The append to `$b` after the
# outer `if` must update `$b` in place: both lists are allocated at runtime with
# room for every append before counting starts, so the loop performs no
# allocations.
run! : Str => Str
run! = |input| {
    rounds = 500
    capacity = 1024 + Str.count_utf8_bytes(input)
    var $a = List.with_capacity(capacity)
    var $b = List.with_capacity(capacity)

    before = Host.alloc_count!()

    var $k = 0
    while $k < rounds {
        var $emit = False
        if $k == rounds {
            $emit = True
        } else {
            $a = $a.append($k)
            if $k == rounds + 1 {
                $b = $b.append($k)
            } else {
                $emit = True
            }
        }
        if $emit {
            $b = $b.append($k)
        } else {
        }
        $k = $k + 1
    }

    loop_allocs = Host.alloc_count!() - before

    "lengths: ${List.len($a).to_str()} ${List.len($b).to_str()}, loop allocations: ${loop_allocs.to_str()}"
}
