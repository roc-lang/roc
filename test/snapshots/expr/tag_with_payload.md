# META
~~~ini
description=Tag with payload
type=expr
~~~
# SOURCE
~~~roc
Some(42)
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
UpperIdent,NoSpaceOpenRound,Int,CloseRound,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-apply
	(e-tag (raw "Some"))
	(e-int (raw "42")))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-tag (name "Some")
	(args
		(e-num (value "42"))))
~~~
# TYPES
~~~clojure
(expr (type "[Some(_size)]_others where [_a.from_int_digits : _arg -> _ret, _b.from_dec_digits : _arg -> _ret]"))
~~~
