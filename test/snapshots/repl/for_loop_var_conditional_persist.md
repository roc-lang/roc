# META
~~~ini
description=For loop with var that persists across iterations with conditional updates
type=repl
~~~
# SOURCE
~~~roc
» result = { var lastEven_ = 0; var evenCount_ = 0; for n in [1, 2, 3, 4, 5, 6, 7, 8] { if n % 2 == 0 { lastEven_ = n; evenCount_ = evenCount_ + 1 } else { {} } }; lastEven_ * evenCount_ }
~~~
# OUTPUT
assigned `result`
# PROBLEMS
NIL
