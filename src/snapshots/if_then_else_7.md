# META
~~~ini
description=if_then_else (7)
type=expr
~~~
# SOURCE
~~~roc
if bool {
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
KwIf(1:1-1:3),LowerIdent(1:4-1:8),OpenCurly(1:9-1:10),Newline(1:1-1:1),
Int(2:2-2:3),Newline(1:1-1:1),
CloseCurly(3:1-3:2),KwElse(3:3-3:7),OpenCurly(3:8-3:9),Newline(1:1-1:1),
Int(4:2-4:3),Newline(1:1-1:1),
CloseCurly(5:1-5:2),EndOfFile(5:2-5:2),
~~~
# PARSE
~~~clojure
(e-if-then-else @1-1-5-2
	(e-ident @1-4-1-8 (qaul "") (raw "bool"))
	(e-block @1-9-3-2
		(statements
			(e-int @2-2-2-3 (raw "1"))))
	(e-block @3-8-5-2
		(statements
			(e-int @4-2-4-3 (raw "2")))))
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