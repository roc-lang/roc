# META
~~~ini
description=A primitive
type=file:ExprInt.roc
~~~
# SOURCE
~~~roc
ExprInt := {}

foo = 42
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
UpperIdent(1:1-1:8),OpColonEqual(1:9-1:11),OpenCurly(1:12-1:13),CloseCurly(1:13-1:14),
LowerIdent(3:1-3:4),OpAssign(3:5-3:6),Int(3:7-3:9),
EndOfFile(4:1-4:1),
~~~
# PARSE
~~~clojure
(file @1.1-3.9
	(type-module @1.1-1.8)
	(statements
		(s-type-decl @1.1-1.14
			(header @1.1-1.8 (name "ExprInt")
				(args))
			(ty-record @1.12-1.14))
		(s-decl @3.1-3.9
			(p-ident @3.1-3.4 (raw "foo"))
			(e-int @3.7-3.9 (raw "42")))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @3.1-3.4 (ident "foo"))
		(e-num @3.7-3.9 (value "42")))
	(s-nominal-decl @1.1-1.14
		(ty-header @1.1-1.8 (name "ExprInt"))
		(ty-record @1.12-1.14)))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @3.1-3.4 (type "Num(_size)")))
	(type_decls
		(nominal @1.1-1.14 (type "ExprInt")
			(ty-header @1.1-1.8 (name "ExprInt"))))
	(expressions
		(expr @3.7-3.9 (type "Num(_size)"))))
~~~
