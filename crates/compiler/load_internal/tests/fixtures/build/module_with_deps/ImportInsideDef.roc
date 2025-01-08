module [dep1_str, dep2_two_dobuled]

dep1_str =
    import Dep1
    Dep1.str

dep2_two_dobuled =
    2
    * (
        import Dep2 exposing [two]
        two
    )
