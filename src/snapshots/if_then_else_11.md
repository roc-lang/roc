# META
~~~ini
description=if_then_else (11)
type=expr
~~~
# SOURCE
~~~roc
if # Comment after if
	bool
		{
			1
		} else {
			2
		}
~~~
# PROBLEMS
**NOT IMPLEMENTED**
This feature is not yet implemented: canonicalize if_then_else expression

# TOKENS
~~~zig
KwIf(1:1-1:3),Newline(1:5-1:22),
LowerIdent(2:2-2:6),Newline(1:1-1:1),
OpenCurly(3:3-3:4),Newline(1:1-1:1),
Int(4:4-4:5),Newline(1:1-1:1),
CloseCurly(5:3-5:4),KwElse(5:5-5:9),OpenCurly(5:10-5:11),Newline(1:1-1:1),
Int(6:4-6:5),Newline(1:1-1:1),
CloseCurly(7:3-7:4),EndOfFile(7:4-7:4),
~~~
# PARSE
~~~clojure
(e-if-then-else @1-1-7-4
	(e-ident @2-2-2-6 (qaul "") (raw "bool"))
	(e-block @3-3-5-4
		(statements
			(e-int @4-4-4-5 (raw "1"))))
	(e-block @5-10-7-4
		(statements
			(e-int @6-4-6-5 (raw "2")))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-runtime-error (tag "not_implemented") (id 73))
~~~
# TYPES
~~~clojure
(expr (id 73) (type "Error"))
~~~