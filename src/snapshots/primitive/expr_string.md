# META
~~~ini
description=A primitive
type=file
~~~
# SOURCE
~~~roc
module [foo]
name = "luc"
foo = "hello ${name}"
~~~
# PROBLEMS
NIL
# TOKENS
~~~zig
KwModule(1:1-1:7),OpenSquare(1:8-1:9),LowerIdent(1:9-1:12),CloseSquare(1:12-1:13),Newline(1:1-1:1),
LowerIdent(2:1-2:5),OpAssign(2:6-2:7),StringStart(2:8-2:9),StringPart(2:9-2:12),StringEnd(2:12-2:13),Newline(1:1-1:1),
LowerIdent(3:1-3:4),OpAssign(3:5-3:6),StringStart(3:7-3:8),StringPart(3:8-3:14),OpenStringInterpolation(3:14-3:16),LowerIdent(3:16-3:20),CloseStringInterpolation(3:20-3:21),StringPart(3:21-3:21),StringEnd(3:21-3:22),EndOfFile(3:22-3:22),
~~~
# PARSE
~~~clojure
(file @1.1-3.22
	(module @1.1-1.13
		(exposes @1.8-1.13
			(exposed-lower-ident (text "foo"))))
	(statements
		(s-decl @2.1-2.13
			(p-ident @2.1-2.5 (raw "name"))
			(e-string @2.8-2.13
				(e-string-part @2.9-2.12 (raw "luc"))))
		(s-decl @3.1-3.22
			(p-ident @3.1-3.4 (raw "foo"))
			(e-string @3.7-3.22
				(e-string-part @3.8-3.14 (raw "hello "))
				(e-ident @3.16-3.20 (qaul "") (raw "name"))
				(e-string-part @3.21-3.21 (raw ""))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let (id 76)
		(p-assign @2.1-2.5 (ident "name") (id 73))
		(e-string @2.8-2.13 (id 75)
			(e-literal @2.9-2.12 (string "luc"))))
	(d-let (id 82)
		(p-assign @3.1-3.4 (ident "foo") (id 77))
		(e-string @3.7-3.22 (id 81)
			(e-literal @3.8-3.14 (string "hello "))
			(e-lookup-local @3.16-3.20
				(pattern (id 73)))
			(e-literal @3.21-3.21 (string "")))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(d_assign (name "name") (def_var 76) (type "Str"))
		(d_assign (name "foo") (def_var 82) (type "Str")))
	(expressions
		(expr @2.8-2.13 (type "Str"))
		(expr @3.7-3.22 (type "Str"))))
~~~
