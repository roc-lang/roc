## A nominal record with an unnamed padding field, used to verify end-to-end that
## a Roc nominal reaches the host with the same byte layout as the matching C /
## extern struct. The declared order (z, _, a) is deliberately not alphabetical
## and the padding is not implied by alignment, so the host only reads the right
## values if Roc both honors declared order and reserves the unnamed padding:
## z@0, four padding bytes, a@8.
Padded := {
    z : U32,
    _ : U32,
    a : U32,
}.{
    new : U32, U32 -> Padded
    new = |z, a| { z, a }

    ## Hosted: the host reads `z` and `a` from the passed-in record at their
    ## expected byte offsets and returns "<z*100 + a>" as a string, so any layout
    ## mismatch yields the wrong number (or garbage).
    check! : Padded => Str

    ## A generic padded nominal: `new` is one body at every `v`, so it builds the
    ## record from a descriptor in its type table. z@0, four padding bytes,
    ## value@8, where the structural order would put `value` first.
    Slot(v) := {
        z : U32,
        _ : U32,
        value : v,
    }.{
        new : U32, v -> Slot(v)
        new = |z, value| { z, value }
    }

    ## A padded nominal whose padding is its type parameter: first@0, the
    ## padding, second@(4 + size of `p`).
    Spaced(p) := {
        first : U32,
        _ : p,
        second : U32,
    }.{
        new : U32, U32 -> Spaced(p)
        new = |first, second| { first, second }
    }

    ## A padded nominal crossing the host generically, inside a box: z@0, four
    ## padding bytes, held@8.
    Held(v) := {
        z : U32,
        _ : U32,
        held : Box(v),
    }.{
        new : U32, v -> Held(v)
        new = |z, value| { z, held: Box.box(value) }

        unbox : Held(v) -> v
        unbox = |held| Box.unbox(held.held)

        doubled : Held(v) -> U32
        doubled = |held| held.z
    }

    ## A padded nominal that a C ABI splits by field class: a float at 0 and a
    ## pointer at 8, which the record's row would put the other way round.
    Mixed(v) := {
        z : F32,
        _ : {},
        held : Box(v),
    }.{
        new : F32, v -> Mixed(v)
        new = |z, value| { z, held: Box.box(value) }

        unbox : Mixed(v) -> v
        unbox = |mixed| Box.unbox(mixed.held)

        scaled : Mixed(v) -> F32
        scaled = |mixed| mixed.z
    }

    ## Hosted: the host returns the record with `z` doubled and its box unmoved.
    check_mixed! : Mixed(v) => Mixed(v)

    ## Hosted: the host reads the record at its declared offsets and returns
    ## "<z*100 + value>".
    check_slot! : Slot(U32) => Str

    ## Hosted: the host reads the record at its declared offsets and returns
    ## "<first*100 + second>".
    check_spaced! : Spaced(U64) => Str

    ## Hosted: the host returns the record with `z` doubled and its box unmoved.
    check_held! : Held(v) => Held(v)
}
