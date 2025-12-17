# Test: nested tag union - Err containing different tags
main : [Err([TagA, TagB])]
main =
    if True {
        Err(TagA)
    } else {
        Err(TagB)
    }
