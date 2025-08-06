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
Evaluation error: error.LayoutError
---
"Hello, World!"
---
<list_of_zst>
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(e-binop @1.2-1.7 (op "add")
	(e-int @1.2-1.3 (value "1"))
	(e-int @1.6-1.7 (value "1")))
---
(e-binop @1.2-1.11 (op "add")
	(e-dec-small @1.2-1.5 (numerator "1") (denominator-power-of-ten "1") (value "0.1"))
	(e-dec-small @1.8-1.11 (numerator "2") (denominator-power-of-ten "1") (value "0.2")))
---
(e-string @1.2-1.17
	(e-literal @1.3-1.16 (string "Hello, World!")))
---
(e-empty_list @1.2-1.4)
~~~
