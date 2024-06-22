interface ExposedUsedOutsideScope exposes [good, bad] imports []

good =
    import Dep2 exposing [two]
    two

bad =
    two
