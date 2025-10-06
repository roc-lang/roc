# META
~~~ini
description=Hexadecimal integer literal type inference
type=file:CanHexInteger.roc
~~~
# SOURCE
~~~roc
CanHexInteger := {}

x = 0xFF
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
UpperIdent(1:1-1:14),OpColonEqual(1:15-1:17),OpenCurly(1:18-1:19),CloseCurly(1:19-1:20),
LowerIdent(3:1-3:2),OpAssign(3:3-3:4),Int(3:5-3:9),
EndOfFile(4:1-4:1),
~~~
# PARSE
~~~clojure
(file @1.1-3.9
	(type-module @1.1-1.14)
	(statements
		(s-type-decl @1.1-1.20
			(header @1.1-1.14 (name "CanHexInteger")
				(args))
			(ty-record @1.18-1.20))
		(s-decl @3.1-3.9
			(p-ident @3.1-3.2 (raw "x"))
			(e-int @3.5-3.9 (raw "0xFF")))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @3.1-3.2 (ident "x"))
		(e-num @3.5-3.9 (value "255")))
	(s-nominal-decl @1.1-1.20
		(ty-header @1.1-1.14 (name "CanHexInteger"))
		(ty-record @1.18-1.20)))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @3.1-3.2 (type "Num(Int(_size))")))
	(type_decls
		(nominal @1.1-1.20 (type "CanHexInteger")
			(ty-header @1.1-1.14 (name "CanHexInteger"))))
	(expressions
		(expr @3.5-3.9 (type "Num(Int(_size))"))))
~~~
