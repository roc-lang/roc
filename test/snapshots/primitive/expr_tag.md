# META
~~~ini
description=A primitive
type=file:ExprTag.roc
~~~
# SOURCE
~~~roc
ExprTag := {}

foo = FortyTwo
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
UpperIdent(1:1-1:8),OpColonEqual(1:9-1:11),OpenCurly(1:12-1:13),CloseCurly(1:13-1:14),
LowerIdent(3:1-3:4),OpAssign(3:5-3:6),UpperIdent(3:7-3:15),
EndOfFile(4:1-4:1),
~~~
# PARSE
~~~clojure
(file @1.1-3.15
	(type-module @1.1-1.8)
	(statements
		(s-type-decl @1.1-1.14
			(header @1.1-1.8 (name "ExprTag")
				(args))
			(ty-record @1.12-1.14))
		(s-decl @3.1-3.15
			(p-ident @3.1-3.4 (raw "foo"))
			(e-tag @3.7-3.15 (raw "FortyTwo")))))
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
		(e-tag @3.7-3.15 (name "FortyTwo")))
	(s-nominal-decl @1.1-1.14
		(ty-header @1.1-1.8 (name "ExprTag"))
		(ty-record @1.12-1.14)))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @3.1-3.4 (type "[FortyTwo]_others")))
	(type_decls
		(nominal @1.1-1.14 (type "ExprTag")
			(ty-header @1.1-1.8 (name "ExprTag"))))
	(expressions
		(expr @3.7-3.15 (type "[FortyTwo]_others"))))
~~~
