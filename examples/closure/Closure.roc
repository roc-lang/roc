app "closure" provides [ makeClosure ] to "./platform/"

makeClosure : ({} -> Int) as MyClosure
makeClosure =
    x = 42
    y = 42

    \{} -> x + y
