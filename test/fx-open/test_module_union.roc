# Test: Module-level open union without platform
main : [Err([TagA, TagB, ..]), Ok({})]
main =
    if True {
        Err(TagA)
    } else {
        Err(TagB)
    }
