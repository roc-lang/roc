module [good, bad]

good =
    import Dep2
    Dep2.two

bad =
    Dep2.two
