interface ExposedUsedOutsideScope exposes [good, bad]

good =
    import Dep2 exposing [two]
    two

bad =
    two
