FrameCallee := [].{
    ## Calls `f` twice and keeps nothing of it, so a caller can build the
    ## callable it passes here in its own frame.
    apply_twice : (U64 -> U64), U64 -> U64
    apply_twice = |f, x| f(f(x))
}
