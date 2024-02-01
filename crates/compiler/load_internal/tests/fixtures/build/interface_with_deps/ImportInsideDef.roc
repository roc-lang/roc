interface ImportInsideDef exposes [dep1Str, dep2TwoDobuled]

dep1Str =
    import Dep1
    Dep1.str

dep2TwoDobuled =
    2
    * (
        import Dep2 exposing [two]
        two
    )
