app [
    make_boxed_callable,
    drop_boxed_callable,
    make_aliased_boxed_callables,
    make_shared_boxed_callables,
    drop_aliased_boxed_callables,
] { pf: platform "./platform/main.roc" }

AliasedCallables : { first : Box(U64 -> U64), second : Box(U64 -> U64) }

make_boxed_callable : U64 -> Box(U64 -> U64)
make_boxed_callable = |offset| Box.box(|value| value + offset)

drop_boxed_callable : Box(U64 -> U64) -> {}
drop_boxed_callable = |_callable| {}

identity_probe : () -> Box(U64 -> U64)
identity_probe = || {
    probe : U64 -> U64
    probe = |value| value

    Box.box(probe)
}

# Repro for https://github.com/roc-lang/roc/issues/10770
#
# A `Box` is a reference to one heap allocation, so both fields hold the same
# boxed callable and must reach the host as one pointer. Platforms use that
# pointer as the value's identity.
make_aliased_boxed_callables : () -> Box(AliasedCallables)
make_aliased_boxed_callables = || {
    boxed = identity_probe()

    Box.box({ first: boxed, second: boxed })
}

# The same requirement for a top-level binding, which every reference reads
# rather than rebuilding.
shared_probe : Box(U64 -> U64)
shared_probe = identity_probe()

make_shared_boxed_callables : () -> Box(AliasedCallables)
make_shared_boxed_callables = || Box.box({ first: shared_probe, second: shared_probe })

drop_aliased_boxed_callables : Box(AliasedCallables) -> {}
drop_aliased_boxed_callables = |_callables| {}
