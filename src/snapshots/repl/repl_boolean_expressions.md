# META
~~~ini
description=Boolean expressions and operations
type=repl
~~~
# SOURCE
~~~roc
» Bool.true # incorrect, tags must be UPPERCASE
» Bool.false
» Bool.True
» Bool.False
» !Bool.True
» !Bool.False
» Bool.True && Bool.False
» !Bool.True || !Bool.True
~~~
# OUTPUT
Evaluation error: error.Crash
---
Evaluation error: error.Crash
---
True
---
False
---
False
---
True
---
True
---
False
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(e-nominal @1.1-1.10 (nominal "Bool")
	(e-tag @1.1-1.10 (name "True")))
---
(e-nominal @1.1-1.11 (nominal "Bool")
	(e-tag @1.1-1.11 (name "False")))
---
(e-unary-not @1.1-1.11
	(e-nominal @1.2-1.11 (nominal "Bool")
		(e-tag @1.2-1.11 (name "True"))))
---
(e-unary-not @1.1-1.12
	(e-nominal @1.2-1.12 (nominal "Bool")
		(e-tag @1.2-1.12 (name "False"))))
---
(e-nominal @1.1-1.10 (nominal "Bool")
	(e-tag @1.1-1.10 (name "True")))
---
(e-unary-not @1.1-1.11
	(e-nominal @1.2-1.11 (nominal "Bool")
		(e-tag @1.2-1.11 (name "True"))))
~~~
# TYPES
~~~clojure
(expr @1.1-1.10 (type "Bool"))
---
(expr @1.1-1.11 (type "Bool"))
---
(expr @1.1-1.11 (type "Bool"))
---
(expr @1.1-1.12 (type "Bool"))
---
(expr @1.1-1.10 (type "Bool"))
---
(expr @1.1-1.11 (type "Bool"))
~~~
