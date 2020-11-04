app Closure provides [ makeClosure ] imports []

makeClosure : ({} -> Int) as MyClosure
makeClosure = 
    x = 42

    \{} -> x

