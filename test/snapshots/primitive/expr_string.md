# META
~~~ini
description=A primitive
type=file:ExprString.roc
~~~
# SOURCE
~~~roc
ExprString := {}

name = "luc"
foo = "hello ${name}"
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
UpperIdent(1:1-1:11),OpColonEqual(1:12-1:14),OpenCurly(1:15-1:16),CloseCurly(1:16-1:17),
LowerIdent(3:1-3:5),OpAssign(3:6-3:7),StringStart(3:8-3:9),StringPart(3:9-3:12),StringEnd(3:12-3:13),
LowerIdent(4:1-4:4),OpAssign(4:5-4:6),StringStart(4:7-4:8),StringPart(4:8-4:14),OpenStringInterpolation(4:14-4:16),LowerIdent(4:16-4:20),CloseStringInterpolation(4:20-4:21),StringPart(4:21-4:21),StringEnd(4:21-4:22),
EndOfFile(5:1-5:1),
~~~
# PARSE
~~~clojure
(file @1.1-4.22
	(type-module @1.1-1.11)
	(statements
		(s-type-decl @1.1-1.17
			(header @1.1-1.11 (name "ExprString")
				(args))
			(ty-record @1.15-1.17))
		(s-decl @3.1-3.13
			(p-ident @3.1-3.5 (raw "name"))
			(e-string @3.8-3.13
				(e-string-part @3.9-3.12 (raw "luc"))))
		(s-decl @4.1-4.22
			(p-ident @4.1-4.4 (raw "foo"))
			(e-string @4.7-4.22
				(e-string-part @4.8-4.14 (raw "hello "))
				(e-ident @4.16-4.20 (raw "name"))
				(e-string-part @4.21-4.21 (raw ""))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @3.1-3.5 (ident "name"))
		(e-string @3.8-3.13
			(e-literal @3.9-3.12 (string "luc"))))
	(d-let
		(p-assign @4.1-4.4 (ident "foo"))
		(e-string @4.7-4.22
			(e-literal @4.8-4.14 (string "hello "))
			(e-lookup-local @4.16-4.20
				(p-assign @3.1-3.5 (ident "name")))
			(e-literal @4.21-4.21 (string ""))))
	(s-nominal-decl @1.1-1.17
		(ty-header @1.1-1.11 (name "ExprString"))
		(ty-record @1.15-1.17)))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @3.1-3.5 (type "Str"))
		(patt @4.1-4.4 (type "Str")))
	(type_decls
		(nominal @1.1-1.17 (type "ExprString")
			(ty-header @1.1-1.11 (name "ExprString"))))
	(expressions
		(expr @3.8-3.13 (type "Str"))
		(expr @4.7-4.22 (type "Str"))))
~~~
