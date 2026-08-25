# META
~~~ini
description=An unannotated local function returning a two-element lambda set owns a dispatch scope for its uses' evidence
type=repl
~~~
# SOURCE
~~~roc
» { pick = |flag| if flag { |x| x + 1.I64 } else { |x| x * 2.I64 }
f = pick(True)
g = pick(False)
(f(10.I64), g(10.I64)) }
~~~
# OUTPUT
(11, 20)
# PROBLEMS
NIL
