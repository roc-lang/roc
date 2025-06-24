# META
~~~ini
description=Tuple expression
type=expr
~~~
# SOURCE
~~~roc
(1, "hello", True)
~~~
# PROBLEMS
NIL
# TOKENS
~~~zig
OpenRound(1:1-1:2),Int(1:2-1:3),Comma(1:3-1:4),StringStart(1:5-1:6),StringPart(1:6-1:11),StringEnd(1:11-1:12),Comma(1:12-1:13),UpperIdent(1:14-1:18),CloseRound(1:18-1:19),EndOfFile(1:19-1:19),
~~~
# PARSE
~~~clojure
(e-tuple @1-1-1-19
	(e-int @1-2-1-3 (raw "1"))
	(e-string @1-5-1-12
		(e-string-part @1-6-1-11 (raw "hello")))
	(e-tag @1-14-1-18 (raw "True")))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-tuple @1-1-1-19 (tuple-var 79) (id 80)
	(elems
		(e-int @1-2-1-3 (int-var 73) (precision-var 72) (literal "1") (value "TODO") (bound "u8"))
		(e-string @1-5-1-12
			(e-literal @1-6-1-11 (string "hello")))
		(e-tag @1-14-1-18 (ext-var 0) (name "True") (args "TODO"))))
~~~
# TYPES
~~~clojure
(expr (id 80) (type "*"))
~~~