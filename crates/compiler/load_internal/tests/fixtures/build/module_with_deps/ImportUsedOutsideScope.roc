interface ImportUsedOutsideScope exposes [good, bad] imports []

good =
    import Dep2
    Dep2.two

bad =
    Dep2.two
