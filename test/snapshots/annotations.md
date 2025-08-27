# META
~~~ini
description=Example of a nominal tag union with a payload
type=file
~~~
# SOURCE
~~~roc
module []

test = [10u8, 15]

~~~
# EXPECTED
TYPE MISMATCH - annotations.md:18:28:18:28
INVALID NOMINAL TAG - annotations.md:21:22:21:41
INVALID NOMINAL TAG - annotations.md:24:24:24:39
TYPE MISMATCH - annotations.md:28:35:28:35
# PROBLEMS
NIL
# TOKENS
~~~zig
KwModule(1:1-1:7),OpenSquare(1:8-1:9),CloseSquare(1:9-1:10),
LowerIdent(3:1-3:5),OpAssign(3:6-3:7),OpenSquare(3:8-3:9),Int(3:9-3:13),Comma(3:13-3:14),Int(3:15-3:17),CloseSquare(3:17-3:18),
EndOfFile(5:1-5:1),
~~~
# PARSE
~~~clojure
(file @1.1-3.18
	(module @1.1-1.10
		(exposes @1.8-1.10))
	(statements
		(s-decl @3.1-3.18
			(p-ident @3.1-3.5 (raw "test"))
			(e-list @3.8-3.18
				(e-int @3.9-3.13 (raw "10u8"))
				(e-int @3.15-3.17 (raw "15"))))))
~~~
# FORMATTED
~~~roc
module []

test = [10u8, 15]
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @3.1-3.5 (ident "test"))
		(e-list @3.8-3.18
			(elems
				(e-int @3.9-3.13 (value "10") (suffix "u8"))
				(e-num @3.15-3.17 (value "15"))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @3.1-3.5 (type "List(Num(Int(_size)))")))
	(expressions
		(expr @3.8-3.18 (type "List(Num(Int(_size)))"))))
~~~
