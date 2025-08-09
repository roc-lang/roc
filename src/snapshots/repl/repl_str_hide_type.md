# META
~~~ini
description=String values should not display type annotations in REPL
type=repl
~~~
# SOURCE
~~~roc
» "hello"
» "world"
» "Hello, World!"
~~~
# OUTPUT
"hello"
---
"world"
---
"Hello, World!"
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(e-string @1.1-1.8
	(e-literal @1.2-1.7 (string "hello")))
---
(e-string @1.1-1.8
	(e-literal @1.2-1.7 (string "world")))
---
(e-string @1.1-1.16
	(e-literal @1.2-1.15 (string "Hello, World!")))
~~~
# TYPES
~~~clojure
(expr @1.1-1.8 (type "Str"))
---
(expr @1.1-1.8 (type "Str"))
---
(expr @1.1-1.16 (type "Str"))
~~~
