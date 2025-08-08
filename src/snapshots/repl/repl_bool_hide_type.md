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
True
---
False
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(e-nominal @1.1-1.5 (nominal "Bool")
	(e-tag @1.1-1.5 (name "True")))
---
(e-nominal @1.1-1.6 (nominal "Bool")
	(e-tag @1.1-1.6 (name "False")))
~~~
# TYPES
~~~clojure
(expr @1.1-1.5 (type "Bool"))
---
(expr @1.1-1.6 (type "Bool"))
~~~
