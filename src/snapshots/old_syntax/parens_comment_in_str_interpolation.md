# META
~~~ini
description=parens_comment_in_str_interpolation
type=expr
~~~
# SOURCE
~~~roc
"${(S#
)}"
~~~
# PROBLEMS
NIL
# TOKENS
~~~zig
StringStart(1:1-1:2),StringPart(1:2-1:2),OpenStringInterpolation(1:2-1:4),NoSpaceOpenRound(1:4-1:5),UpperIdent(1:5-1:6),Newline(1:7-1:7),
CloseRound(2:1-2:2),CloseStringInterpolation(2:2-2:3),StringPart(2:3-2:3),StringEnd(2:3-2:4),EndOfFile(2:4-2:4),
~~~
# PARSE
~~~clojure
(e-string @1.1-2.4
	(e-string-part @1.2-1.2 (raw ""))
	(e-tuple @1.4-2.2
		(e-tag @1.5-1.6 (raw "S")))
	(e-string-part @2.3-2.3 (raw "")))
~~~
# FORMATTED
~~~roc
"${
	(
		S,
	)
}"
~~~
# CANONICALIZE
~~~clojure
(e-string @1.1-2.4
	(e-literal @1.2-1.2 (string ""))
	(e-tag @1.5-1.6 (name "S") (args "TODO"))
	(e-literal @2.3-2.3 (string "")))
~~~
# TYPES
~~~clojure
(expr @1.1-2.4 (type "Str"))
~~~
