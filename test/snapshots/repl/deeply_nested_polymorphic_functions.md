# META
~~~ini
description=Deeply nested polymorphic function calls with multiple levels
type=repl
~~~
# SOURCE
~~~roc
» identity = |x| x
» apply = |f, val| f(val)
» twice = |f, val| f(f(val))
» num1 = twice(identity, 42)
» str1 = twice(identity, "Hello")
» num2 = apply(|x| apply(identity, x), 100)
» { num1, num2, str1 }
~~~
# OUTPUT
<needs context>
---
<needs context>
---
<needs context>
---
<needs context>
---
<needs context>
---
<needs context>
---
<needs context>
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(e-runtime-error (tag "ident_not_in_scope"))
---
(e-runtime-error (tag "ident_not_in_scope"))
---
(e-runtime-error (tag "ident_not_in_scope"))
---
(e-runtime-error (tag "ident_not_in_scope"))
---
(e-runtime-error (tag "ident_not_in_scope"))
---
(e-runtime-error (tag "ident_not_in_scope"))
---
(e-record @1.2-1.22
	(fields
		(field (name "num1")
			(e-runtime-error (tag "ident_not_in_scope")))
		(field (name "num2")
			(e-runtime-error (tag "ident_not_in_scope")))
		(field (name "str1")
			(e-runtime-error (tag "ident_not_in_scope")))))
~~~
# TYPES
~~~clojure
(expr @1.2-1.10 (type "Error"))
---
(expr @1.2-1.7 (type "Error"))
---
(expr @1.2-1.7 (type "Error"))
---
(expr @1.2-1.6 (type "Error"))
---
(expr @1.2-1.6 (type "Error"))
---
(expr @1.2-1.6 (type "Error"))
---
(expr @1.2-1.22 (type "{ num1: Error, num2: Error, str1: Error }"))
~~~
