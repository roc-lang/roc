# META
~~~ini
description=Test to_str methods for Bool, List, Dict, and universal types
type=snippet
~~~
# SOURCE
~~~roc
boolTrue : Str
boolTrue = Bool.to_str(Bool.true)

boolFalse : Str
boolFalse = Bool.to_str(Bool.false)

listStr : Str
listStr = List.to_str(["a", "b", "c"])

dictStr : Str
dictStr = Dict.to_str(Dict.from_list([("key", "value")]))
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
