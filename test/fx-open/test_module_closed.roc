# Test: Module-level closed union - baseline comparison
main : [Err([TagA, TagB]), Ok({})]
main =
    if True {
        Err(TagA)
    } else {
        Err(TagB)
    }
