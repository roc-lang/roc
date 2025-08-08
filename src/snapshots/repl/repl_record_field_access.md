# META
~~~ini
description=Record field access
type=repl
~~~
# SOURCE
~~~roc
» {}.foo
» {foo: "Hello"}.foo
» {foo: "Hello", bar: "World"}.bar
~~~
# OUTPUT
Evaluation error: error.ZeroSizedType
---
"Hello"
---
"World"
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(e-dot-access @1.1-1.19 (field "foo")
	(receiver
		(e-record @1.1-1.15
			(fields
				(field (name "foo")
					(e-string @1.7-1.14
						(e-literal @1.8-1.13 (string "Hello"))))))))
---
(e-dot-access @1.1-1.33 (field "bar")
	(receiver
		(e-record @1.1-1.29
			(fields
				(field (name "foo")
					(e-string @1.7-1.14
						(e-literal @1.8-1.13 (string "Hello"))))
				(field (name "bar")
					(e-string @1.21-1.28
						(e-literal @1.22-1.27 (string "World"))))))))
~~~
# TYPES
~~~clojure
(expr @1.1-1.19 (type "Str"))
---
(expr @1.1-1.33 (type "Str"))
~~~
