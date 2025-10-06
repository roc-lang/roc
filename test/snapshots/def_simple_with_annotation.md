# META
~~~ini
description=Simple definition with type annotation
type=file:DefSimpleWithAnnotation.roc
~~~
# SOURCE
~~~roc
DefSimpleWithAnnotation := {}

foo : Str
foo = "one"
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
UpperIdent(1:1-1:24),OpColonEqual(1:25-1:27),OpenCurly(1:28-1:29),CloseCurly(1:29-1:30),
LowerIdent(3:1-3:4),OpColon(3:5-3:6),UpperIdent(3:7-3:10),
LowerIdent(4:1-4:4),OpAssign(4:5-4:6),StringStart(4:7-4:8),StringPart(4:8-4:11),StringEnd(4:11-4:12),
EndOfFile(5:1-5:1),
~~~
# PARSE
~~~clojure
(file @1.1-4.12
	(type-module @1.1-1.24)
	(statements
		(s-type-decl @1.1-1.30
			(header @1.1-1.24 (name "DefSimpleWithAnnotation")
				(args))
			(ty-record @1.28-1.30))
		(s-type-anno @3.1-3.10 (name "foo")
			(ty @3.7-3.10 (name "Str")))
		(s-decl @4.1-4.12
			(p-ident @4.1-4.4 (raw "foo"))
			(e-string @4.7-4.12
				(e-string-part @4.8-4.11 (raw "one"))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @4.1-4.4 (ident "foo"))
		(e-string @4.7-4.12
			(e-literal @4.8-4.11 (string "one")))
		(annotation @4.1-4.4
			(declared-type
				(ty-lookup @3.7-3.10 (name "Str") (builtin)))))
	(s-nominal-decl @1.1-1.30
		(ty-header @1.1-1.24 (name "DefSimpleWithAnnotation"))
		(ty-record @1.28-1.30)))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @4.1-4.4 (type "Str")))
	(type_decls
		(nominal @1.1-1.30 (type "DefSimpleWithAnnotation")
			(ty-header @1.1-1.24 (name "DefSimpleWithAnnotation"))))
	(expressions
		(expr @4.7-4.12 (type "Str"))))
~~~
