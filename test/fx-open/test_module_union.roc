# Test: Module-level open union without platform
main : [Err([TagA, TagB, ..others]), Ok({})]
main =
    if True {
        Err(TagA)
    } else {
        Err(TagB)
    }
