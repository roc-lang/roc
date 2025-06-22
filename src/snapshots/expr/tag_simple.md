# META
~~~ini
description=Simple tag literal
type=expr
~~~
# SOURCE
~~~roc
Ok
~~~
# PROBLEMS
~~~txt
NIL
~~~
# TOKENS
~~~zig
UpperIdent(1:1-1:3),EndOfFile(1:3-1:3),
~~~
# PARSE
~~~clojure
(tag (1:1-1:3) "Ok")
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e_tag (1:1-1:3)
	(ext_var 0)
	(name "Ok")
	(args "TODO"))
~~~
# TYPES
~~~clojure
(expr 13 (type "[Ok, * *]"))
~~~