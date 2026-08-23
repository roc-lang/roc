app [main!] { pf: platform "./platform/main.roc" }

import pf.Host
import pf.Stdout

Seed : { path : I64 }

# Repro for https://github.com/roc-lang/roc/issues/10770
#
# `Cap(a)` never mentions `a` in its payload, so `a` reaches `new_with_eq` only
# as the declaration argument of the `Cap(a)` it returns. Dropping that argument
# at the call boundary leaves `a` unresolved, and an unresolved type variable
# finalizes as uninhabited, which makes every specialized body below reachable
# only by a runtime error.
Cap(a) := [Cap({ probe! : Box((I64 => I64)) })].{
    new : () -> Cap(a)
    new = || Cap.new_with_eq(|_left, _right| True)

    new_with_eq : (a, a -> Bool) -> Cap(a)
    new_with_eq = |is_equal| {
        split : Box(a) -> { keep : Box(a), out : Box(a) }
        split = |boxed| {
            value = Box.unbox(boxed)
            { keep: Box.box(value), out: Box.box(value) }
        }
        split_handle = Box.box(split)

        probe! : I64 => I64
        probe! = |offset| {
            taken : Box(a)
            taken = Host.take_seed!()
            parts = Box.unbox(split_handle)(taken)
            left = Box.unbox(parts.out)
            _ = parts.keep
            if is_equal(left, left) { offset } else { offset + 1 }
        }

        Cap({ probe!: Box.box(probe!) })
    }

    probe_of : Cap(a) -> Box((I64 => I64))
    probe_of = |Cap(handle)| handle.probe!
}

main! = || {
    seed : Seed
    seed = { path: 1 }
    Host.store_seed!(Box.box(seed))

    cap : Cap(Seed)
    cap = Cap.new()
    probe! = Box.unbox(Cap.probe_of(cap))

    Stdout.line!(probe!(41).to_str())
}
