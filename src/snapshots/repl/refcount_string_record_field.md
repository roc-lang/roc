# META
~~~ini
description=string refcount behavior in record field access
type=repl
~~~
# SOURCE
~~~roc
» "short"
» "This is a very long string that definitely exceeds the small string optimization limit and will require heap allocation"
» { foo: "short" }.foo
» { foo: "This is a very long string that definitely exceeds the small string optimization limit and will require heap allocation" }.foo
~~~
# OUTPUT
"short"
---
"This is a very long string that definitely exceeds the small string optimization limit and will require heap allocation"
---
"short"
---
"This is a very long string that definitely exceeds the small string optimization limit and will require heap allocation"
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(e-string @1.1-1.8
	(e-literal @1.2-1.7 (string "short")))
---
(e-string @1.1-1.122
	(e-literal @1.2-1.121 (string "This is a very long string that definitely exceeds the small string optimization limit and will require heap allocation")))
---
(e-dot-access @1.1-1.21 (field "foo")
	(receiver
		(e-record @1.1-1.17
			(fields
				(field (name "foo")
					(e-string @1.8-1.15
						(e-literal @1.9-1.14 (string "short"))))))))
---
(e-dot-access @1.1-1.135 (field "foo")
	(receiver
		(e-record @1.1-1.131
			(fields
				(field (name "foo")
					(e-string @1.8-1.129
						(e-literal @1.9-1.128 (string "This is a very long string that definitely exceeds the small string optimization limit and will require heap allocation"))))))))
~~~
# TYPES
~~~clojure
(expr @1.1-1.8 (type "Str"))
---
(expr @1.1-1.122 (type "Str"))
---
(expr @1.1-1.21 (type "Str"))
---
(expr @1.1-1.135 (type "Str"))
~~~
