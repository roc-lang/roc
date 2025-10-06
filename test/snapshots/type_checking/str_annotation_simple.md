# META
~~~ini
description=Simple Str type annotation
type=snippet
~~~
# SOURCE
~~~roc
x : Str
x = "hello"
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
LowerIdent(1:1-1:2),OpColon(1:3-1:4),UpperIdent(1:5-1:8),
LowerIdent(2:1-2:2),OpAssign(2:3-2:4),StringStart(2:5-2:6),StringPart(2:6-2:11),StringEnd(2:11-2:12),
EndOfFile(3:1-3:1),
~~~
# PARSE
~~~clojure
(file @1.1-2.12
	(type-module @1.1-1.2)
	(statements
		(s-type-anno @1.1-1.8 (name "x")
			(ty @1.5-1.8 (name "Str")))
		(s-decl @2.1-2.12
			(p-ident @2.1-2.2 (raw "x"))
			(e-string @2.5-2.12
				(e-string-part @2.6-2.11 (raw "hello"))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @2.1-2.2 (ident "x"))
		(e-string @2.5-2.12
			(e-literal @2.6-2.11 (string "hello")))
		(annotation @2.1-2.2
			(declared-type
				(ty-lookup @1.5-1.8 (name "Str") (builtin))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @2.1-2.2 (type "Str")))
	(expressions
		(expr @2.5-2.12 (type "Str"))))
~~~
