# META
~~~ini
description=Singleline with comma formatting module
type=file
~~~
# SOURCE
~~~roc
module [a, b,]

a = 'a'
b = 'a'
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
KwModule(1:1-1:7),OpenSquare(1:8-1:9),LowerIdent(1:9-1:10),Comma(1:10-1:11),LowerIdent(1:12-1:13),Comma(1:13-1:14),CloseSquare(1:14-1:15),
LowerIdent(3:1-3:2),OpAssign(3:3-3:4),SingleQuote(3:5-3:8),
LowerIdent(4:1-4:2),OpAssign(4:3-4:4),SingleQuote(4:5-4:8),
EndOfFile(5:1-5:1),
~~~
# PARSE
~~~clojure
(file @1.1-4.8
	(module @1.1-1.15
		(exposes @1.8-1.15
			(exposed-lower-ident @1.9-1.10
				(text "a"))
			(exposed-lower-ident @1.12-1.13
				(text "b"))))
	(statements
		(s-decl @3.1-3.8
			(p-ident @3.1-3.2 (raw "a"))
			(e-single-quote @3.5-3.8 (raw "'a'")))
		(s-decl @4.1-4.8
			(p-ident @4.1-4.2 (raw "b"))
			(e-single-quote @4.5-4.8 (raw "'a'")))))
~~~
# FORMATTED
~~~roc
module [
	a,
	b,
]

a = 'a'
b = 'a'
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @3.1-3.2 (ident "a"))
		(e-int @3.5-3.8 (value "97")))
	(d-let
		(p-assign @4.1-4.2 (ident "b"))
		(e-int @4.5-4.8 (value "97"))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @3.1-3.2 (type "Num(_size)"))
		(patt @4.1-4.2 (type "Num(_size)")))
	(expressions
		(expr @3.5-3.8 (type "Num(_size)"))
		(expr @4.5-4.8 (type "Num(_size)"))))
~~~
