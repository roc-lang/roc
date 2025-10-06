# META
~~~ini
description=Singleline with comma formatting module
type=file:Module.roc
~~~
# SOURCE
~~~roc
Module := {}

a = 'a'
b = 'a'
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
UpperIdent(1:1-1:7),OpColonEqual(1:8-1:10),OpenCurly(1:11-1:12),CloseCurly(1:12-1:13),
LowerIdent(3:1-3:2),OpAssign(3:3-3:4),SingleQuote(3:5-3:8),
LowerIdent(4:1-4:2),OpAssign(4:3-4:4),SingleQuote(4:5-4:8),
EndOfFile(5:1-5:1),
~~~
# PARSE
~~~clojure
(file @1.1-4.8
	(type-module @1.1-1.7)
	(statements
		(s-type-decl @1.1-1.13
			(header @1.1-1.7 (name "Module")
				(args))
			(ty-record @1.11-1.13))
		(s-decl @3.1-3.8
			(p-ident @3.1-3.2 (raw "a"))
			(e-single-quote @3.5-3.8 (raw "'a'")))
		(s-decl @4.1-4.8
			(p-ident @4.1-4.2 (raw "b"))
			(e-single-quote @4.5-4.8 (raw "'a'")))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @3.1-3.2 (ident "a"))
		(e-num @3.5-3.8 (value "97")))
	(d-let
		(p-assign @4.1-4.2 (ident "b"))
		(e-num @4.5-4.8 (value "97")))
	(s-nominal-decl @1.1-1.13
		(ty-header @1.1-1.7 (name "Module"))
		(ty-record @1.11-1.13)))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @3.1-3.2 (type "Num(Int(_size))"))
		(patt @4.1-4.2 (type "Num(Int(_size))")))
	(type_decls
		(nominal @1.1-1.13 (type "Module")
			(ty-header @1.1-1.7 (name "Module"))))
	(expressions
		(expr @3.5-3.8 (type "Num(Int(_size))"))
		(expr @4.5-4.8 (type "Num(Int(_size))"))))
~~~
