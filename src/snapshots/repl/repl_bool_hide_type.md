# META
~~~ini
description=Boolean values (True/False) should not display type annotations in REPL
type=repl
~~~
# SOURCE
~~~roc
» True
» False
~~~
# OUTPUT
1
---
0
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(e-nominal @1.2-1.6 (nominal "Bool")
	(e-tag @1.2-1.6 (name "True")))
---
(e-nominal @1.2-1.7 (nominal "Bool")
	(e-tag @1.2-1.7 (name "False")))
~~~
