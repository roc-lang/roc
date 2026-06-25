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
}
