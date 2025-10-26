# META
~~~ini
description=For loop iterating over List Bool
type=repl
~~~
# SOURCE
~~~roc
» result = { var allTrue_ = Bool.True; for b in [Bool.True, Bool.True, Bool.False] { if b == Bool.False { allTrue_ = Bool.False } else { {} } }; allTrue_ }
~~~
# OUTPUT
assigned `result`
# PROBLEMS
NIL
