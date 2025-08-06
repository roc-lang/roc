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
1
---
0
---
0
---
1
---
1
---
0
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(e-runtime-error (tag "ident_not_in_scope"))
---
(e-runtime-error (tag "ident_not_in_scope"))
---
(e-nominal @1.2-1.11 (nominal "Bool")
	(e-tag @1.2-1.11 (name "True")))
---
(e-nominal @1.2-1.12 (nominal "Bool")
	(e-tag @1.2-1.12 (name "False")))
---
(e-unary-not @1.2-1.12
	(e-nominal @1.3-1.12 (nominal "Bool")
		(e-tag @1.3-1.12 (name "True"))))
---
(e-unary-not @1.2-1.13
	(e-nominal @1.3-1.13 (nominal "Bool")
		(e-tag @1.3-1.13 (name "False"))))
---
(e-nominal @1.2-1.11 (nominal "Bool")
	(e-tag @1.2-1.11 (name "True")))
---
(e-unary-not @1.2-1.12
	(e-nominal @1.3-1.12 (nominal "Bool")
		(e-tag @1.3-1.12 (name "True"))))
~~~
