app Closure provides [ makeClosure ] imports []

makeClosure : ({} -> Int) as MyClosure
makeClosure = 
    x = 42
    y = 42

    \{} -> x + y

