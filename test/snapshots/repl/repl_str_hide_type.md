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
(e-string @1.1-1.1
	(e-literal @1.1-1.1 (string "hello")))
(e-string @1.2-1.9
	(e-literal @1.3-1.8 (string "hello")))
---
(e-string @1.1-1.1
	(e-literal @1.1-1.1 (string "world")))
(e-string @1.2-1.9
	(e-literal @1.3-1.8 (string "world")))
---
(e-string @1.1-1.1
	(e-literal @1.1-1.1 (string "Hello, World!")))
(e-string @1.2-1.17
	(e-literal @1.3-1.16 (string "Hello, World!")))
~~~
# TYPES
~~~clojure
(expr @1.1-1.1 (type "Str"))
(expr @1.2-1.9 (type "Str"))
---
(expr @1.1-1.1 (type "Str"))
(expr @1.2-1.9 (type "Str"))
---
(expr @1.1-1.1 (type "Str"))
(expr @1.2-1.17 (type "Str"))
~~~
