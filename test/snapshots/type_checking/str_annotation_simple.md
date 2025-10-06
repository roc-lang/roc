# META
~~~ini
description=Simple Str type annotation
type=file:StrAnnotationSimple.roc
~~~
# SOURCE
~~~roc
StrAnnotationSimple := {}

x : Str
x = "hello"
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
UpperIdent(1:1-1:20),OpColonEqual(1:21-1:23),OpenCurly(1:24-1:25),CloseCurly(1:25-1:26),
LowerIdent(3:1-3:2),OpColon(3:3-3:4),UpperIdent(3:5-3:8),
LowerIdent(4:1-4:2),OpAssign(4:3-4:4),StringStart(4:5-4:6),StringPart(4:6-4:11),StringEnd(4:11-4:12),
EndOfFile(5:1-5:1),
~~~
# PARSE
~~~clojure
(file @1.1-4.12
	(type-module @1.1-1.20)
	(statements
		(s-type-decl @1.1-1.26
			(header @1.1-1.20 (name "StrAnnotationSimple")
				(args))
			(ty-record @1.24-1.26))
		(s-type-anno @3.1-3.8 (name "x")
			(ty @3.5-3.8 (name "Str")))
		(s-decl @4.1-4.12
			(p-ident @4.1-4.2 (raw "x"))
			(e-string @4.5-4.12
				(e-string-part @4.6-4.11 (raw "hello"))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @4.1-4.2 (ident "x"))
		(e-string @4.5-4.12
			(e-literal @4.6-4.11 (string "hello")))
		(annotation @4.1-4.2
			(declared-type
				(ty-lookup @3.5-3.8 (name "Str") (builtin)))))
	(s-nominal-decl @1.1-1.26
		(ty-header @1.1-1.20 (name "StrAnnotationSimple"))
		(ty-record @1.24-1.26)))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @4.1-4.2 (type "Str")))
	(type_decls
		(nominal @1.1-1.26 (type "StrAnnotationSimple")
			(ty-header @1.1-1.20 (name "StrAnnotationSimple"))))
	(expressions
		(expr @4.5-4.12 (type "Str"))))
~~~
