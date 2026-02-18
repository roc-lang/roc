# Simplest test: direct tags in if-else without Err wrapper
main : [TagA, TagB]
main =
    if True {
        TagA
    } else {
        TagB
    }
