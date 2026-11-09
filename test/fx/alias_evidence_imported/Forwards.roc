# Aliases another module's uses reach, each of whose chains is read in this
# module's scopes.
Forwards := [F(U64)].{
    to_str : Forwards -> Str
    to_str = |F(n)| "forwards ${n.to_str()}"

    via = |value| "<${value.to_str()}>"

    stored : U64 -> Str
    stored = via

    again = via
    once_more = again

    at_u8 : U8 -> Str
    at_u8 = once_more

    bump = |x| x + 1

    at : U64 -> U64
    at = bump

    gen = bump
    gen_again = gen
}
