FrameCallee := [].{
    ## Adds `f` of `x` to `f` of `x + 1`, and keeps nothing of `f`, so a
    ## caller can build the callable it passes here in its own frame. Neither
    ## call of `f` is a tail call: a tail call is deferred until after the
    ## caller's frame is gone (design.md "Deferred Erased Calls").
    apply_both : (U64 -> U64), U64 -> U64
    apply_both = |f, x| f(x) + f(x + 1)
}
