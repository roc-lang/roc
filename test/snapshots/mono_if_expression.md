# META
~~~ini
description=Mono test: if expression
type=mono
~~~
# SOURCE
~~~roc
if True 1 else 2
~~~
# MONO
~~~roc
if True 1 else 2 : a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
KwIf,UpperIdent,Int,KwElse,Int,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-if-then-else
	(e-tag (raw "True"))
	(e-int (raw "1"))
	(e-int (raw "2")))
~~~
# CANONICALIZE
~~~clojure
(e-if
	(if-branches
		(if-branch
			(e-tag (name "True"))
			(e-num (value "1"))))
	(if-else
		(e-num (value "2"))))
~~~
# TYPES
~~~clojure
(expr (type "a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]"))
~~~
