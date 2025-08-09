# META
~~~ini
description=two strings
type=repl
~~~
# SOURCE
~~~roc
» 1 + 1
» 0.1 + 0.2
» "Hello, World!"
» []
~~~
# OUTPUT
2
---
Evaluation error: error.TypeMismatch
---
"Hello, World!"
---
<list_of_zst>
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(e-binop @1.1-1.6 (op "add")
	(e-int @1.1-1.2 (value "1"))
	(e-int @1.5-1.6 (value "1")))
---
(e-string @1.1-1.16
	(e-literal @1.2-1.15 (string "Hello, World!")))
---
(e-empty_list @1.1-1.3)
~~~
# TYPES
~~~clojure
(expr @1.1-1.6 (type "Num(_size)"))
---
(expr @1.1-1.16 (type "Str"))
---
(expr @1.1-1.3 (type "List(_elem)"))
~~~
