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
(e-string @1.2-1.9
	(e-literal @1.3-1.8 (string "short")))
---
(e-string @1.2-1.123
	(e-literal @1.3-1.122 (string "This is a very long string that definitely exceeds the small string optimization limit and will require heap allocation")))
---
(e-dot-access @1.2-1.22 (field "foo")
	(receiver
		(e-record @1.2-1.18
			(fields
				(field (name "foo")
					(e-string @1.9-1.16
						(e-literal @1.10-1.15 (string "short"))))))))
---
(e-dot-access @1.2-1.136 (field "foo")
	(receiver
		(e-record @1.2-1.132
			(fields
				(field (name "foo")
					(e-string @1.9-1.130
						(e-literal @1.10-1.129 (string "This is a very long string that definitely exceeds the small string optimization limit and will require heap allocation"))))))))
~~~
