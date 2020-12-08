app "closure" provides [ makeClosure ] to "./platform/"

makeClosure : ({} -> I64) as MyClosure
makeClosure =
    x = 42
    y = 42

    \{} -> x + y
