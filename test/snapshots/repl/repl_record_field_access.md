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
(e-dot-access @1.1-1.1 (field "foo")
	(receiver
		(e-record @1.1-1.1
			(fields
				(field (name "foo")
					(e-string @1.1-1.1
						(e-literal @1.1-1.1 (string "Hello"))))))))
(e-dot-access @1.2-1.8 (field "foo")
	(receiver
		(e-empty_record @1.2-1.4)))
---
(e-dot-access @1.1-1.1 (field "bar")
	(receiver
		(e-record @1.1-1.1
			(fields
				(field (name "foo")
					(e-string @1.1-1.1
						(e-literal @1.1-1.1 (string "Hello"))))
				(field (name "bar")
					(e-string @1.1-1.1
						(e-literal @1.1-1.1 (string "World"))))))))
(e-dot-access @1.2-1.20 (field "foo")
	(receiver
		(e-record @1.2-1.16
			(fields
				(field (name "foo")
					(e-string @1.8-1.15
						(e-literal @1.9-1.14 (string "Hello"))))))))
~~~
# TYPES
~~~clojure
(expr @1.1-1.1 (type "Str"))
(expr @1.2-1.8 (type "_a"))
---
(expr @1.1-1.1 (type "Str"))
(expr @1.2-1.20 (type "Str"))
~~~
