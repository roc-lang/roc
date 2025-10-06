# META
~~~ini
description=Debug expression stmt
type=file:ExpectStmtTopLevel.roc
~~~
# SOURCE
~~~roc
ExpectStmtTopLevel := {}

foo = Bool.True

expect foo != Bool.False
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
UpperIdent(1:1-1:19),OpColonEqual(1:20-1:22),OpenCurly(1:23-1:24),CloseCurly(1:24-1:25),
LowerIdent(3:1-3:4),OpAssign(3:5-3:6),UpperIdent(3:7-3:11),NoSpaceDotUpperIdent(3:11-3:16),
KwExpect(5:1-5:7),LowerIdent(5:8-5:11),OpNotEquals(5:12-5:14),UpperIdent(5:15-5:19),NoSpaceDotUpperIdent(5:19-5:25),
EndOfFile(6:1-6:1),
~~~
# PARSE
~~~clojure
(file @1.1-5.25
	(type-module @1.1-1.19)
	(statements
		(s-type-decl @1.1-1.25
			(header @1.1-1.19 (name "ExpectStmtTopLevel")
				(args))
			(ty-record @1.23-1.25))
		(s-decl @3.1-3.16
			(p-ident @3.1-3.4 (raw "foo"))
			(e-tag @3.7-3.16 (raw "Bool.True")))
		(s-expect @5.1-5.25
			(e-binop @5.8-5.25 (op "!=")
				(e-ident @5.8-5.11 (raw "foo"))
				(e-tag @5.15-5.25 (raw "Bool.False"))))))
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
		(e-nominal @3.7-3.16 (nominal "Bool")
			(e-tag @3.7-3.16 (name "True"))))
	(s-nominal-decl @1.1-1.25
		(ty-header @1.1-1.19 (name "ExpectStmtTopLevel"))
		(ty-record @1.23-1.25))
	(s-expect @5.1-5.25
		(e-binop @5.8-5.25 (op "ne")
			(e-lookup-local @5.8-5.11
				(p-assign @3.1-3.4 (ident "foo")))
			(e-nominal @5.15-5.25 (nominal "Bool")
				(e-tag @5.15-5.25 (name "False"))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @3.1-3.4 (type "Bool")))
	(type_decls
		(nominal @1.1-1.25 (type "ExpectStmtTopLevel")
			(ty-header @1.1-1.19 (name "ExpectStmtTopLevel"))))
	(expressions
		(expr @3.7-3.16 (type "Bool"))))
~~~
