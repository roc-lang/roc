# META
~~~ini
description=Multiline formatting module
type=file
~~~
# SOURCE
~~~roc
module [
	a,
	b,
]

a = 'a'
b = 'a'
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
KwModule(1:1-1:7),OpenSquare(1:8-1:9),
LowerIdent(2:2-2:3),Comma(2:3-2:4),
LowerIdent(3:2-3:3),Comma(3:3-3:4),
CloseSquare(4:1-4:2),
LowerIdent(6:1-6:2),OpAssign(6:3-6:4),SingleQuote(6:5-6:8),
LowerIdent(7:1-7:2),OpAssign(7:3-7:4),SingleQuote(7:5-7:8),
EndOfFile(8:1-8:1),
~~~
# PARSE
~~~clojure
(file @1.1-7.8
	(module @1.1-4.2
		(exposes @1.8-4.2
			(exposed-lower-ident @2.2-2.3
				(text "a"))
			(exposed-lower-ident @3.2-3.3
				(text "b"))))
	(statements
		(s-decl @6.1-6.8
			(p-ident @6.1-6.2 (raw "a"))
			(e-single-quote @6.5-6.8 (raw "'a'")))
		(s-decl @7.1-7.8
			(p-ident @7.1-7.2 (raw "b"))
			(e-single-quote @7.5-7.8 (raw "'a'")))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @6.1-6.2 (ident "a"))
		(e-num @6.5-6.8 (value "97")))
	(d-let
		(p-assign @7.1-7.2 (ident "b"))
		(e-num @7.5-7.8 (value "97"))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @6.1-6.2 (type "Num(Int(_size))"))
		(patt @7.1-7.2 (type "Num(Int(_size))")))
	(expressions
		(expr @6.5-6.8 (type "Num(Int(_size))"))
		(expr @7.5-7.8 (type "Num(Int(_size))"))))
~~~
