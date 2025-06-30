# META
~~~ini
description=multiline_str_after_newlines_in_pat
type=expr
~~~
# SOURCE
~~~roc
(4

)"""""":C
U
~~~
# PROBLEMS
NIL
# TOKENS
~~~zig
OpenRound(1:1-1:2),Int(1:2-1:3),Newline(1:1-1:1),
Newline(1:1-1:1),
CloseRound(3:1-3:2),MultilineStringStart(3:2-3:5),StringPart(3:5-3:5),MultilineStringEnd(3:5-3:8),OpColon(3:8-3:9),UpperIdent(3:9-3:10),Newline(1:1-1:1),
UpperIdent(4:1-4:2),EndOfFile(4:2-4:2),
~~~
# PARSE
~~~clojure
(e-tuple @1.1-3.2
	(e-int @1.2-1.3 (raw "4")))
~~~
# FORMATTED
~~~roc
(
	4,

)
~~~
# CANONICALIZE
~~~clojure
(e-tuple @1.1-3.2 (id 74)
	(elems
		(e-int @1.2-1.3 (value "4"))))
~~~
# TYPES
~~~clojure
(expr (id 74) (type "(Num(*))"))
~~~
