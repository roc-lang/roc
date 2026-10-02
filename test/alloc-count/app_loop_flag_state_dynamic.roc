app [run!] { pf: platform "./platform/main.roc" }

import pf.Host

# A loop whose stepping function is chosen at runtime, so it cannot be inlined.
# `Done` sets a flag that ends the loop before the state is read again, so the
# state must not be retained across the call to `step`.
Step : [Done, Next({ items : List(U64), left : U64 })]

step_append : { items : List(U64), left : U64 } -> Step
step_append = |state| {
    if state.left == 0 {
        Done
    } else {
        Next({ items: state.items.append(state.left), left: state.left - 1 })
    }
}

step_prepend : { items : List(U64), left : U64 } -> Step
step_prepend = |state| {
    if state.left == 0 {
        Done
    } else {
        Next({ items: state.items.prepend(state.left), left: state.left - 1 })
    }
}

flag : ({ items : List(U64), left : U64 } -> Step), U64 -> U64
flag = |step, n| {
    var $state = { items: [], left: n }
    var $steps = 0
    var $done = Bool.False
    while !$done {
        match step($state) {
            Done => { $done = Bool.True }
            Next(next) => {
                $state = next
                $steps = $steps + 1
            }
        }
    }
    $steps
}

flag_return : ({ items : List(U64), left : U64 } -> Step), U64 -> U64
flag_return = |step, n| {
    var $state = { items: [], left: n }
    var $steps = 0
    while Bool.True {
        match step($state) {
            Done => { return $steps }
            Next(next) => {
                $state = next
                $steps = $steps + 1
            }
        }
    }
    $steps
}

run! : Str => Str
run! = |input| {
    n = 1024
    step = if input == "prepend" { step_prepend } else { step_append }
    before_flag = Host.alloc_count!()
    _ = flag(step, n)
    flag_allocs = Host.alloc_count!() - before_flag
    before_return = Host.alloc_count!()
    _ = flag_return(step, n)
    return_allocs = Host.alloc_count!() - before_return
    # Growing the list to 1024 elements takes a handful of allocations;
    # copying it on every step takes one per element. Negating here as well
    # as in `flag` means `!` is not used only once in the program.
    linear = !(flag_allocs >= 64 or return_allocs >= 64)
    "allocations: ${flag_allocs.to_str()} ${return_allocs.to_str()}, linear: ${if linear "yes" else "no"}"
}
