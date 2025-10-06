# META
~~~ini
description=Simple definition with type annotation
type=snippet
~~~
# SOURCE
~~~roc
foo : Str
foo = "one"
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
LowerIdent(1:1-1:4),OpColon(1:5-1:6),UpperIdent(1:7-1:10),
LowerIdent(2:1-2:4),OpAssign(2:5-2:6),StringStart(2:7-2:8),StringPart(2:8-2:11),StringEnd(2:11-2:12),
EndOfFile(3:1-3:1),
~~~
# PARSE
~~~clojure
(file @1.1-2.12
	(type-module @1.1-1.4)
	(statements
		(s-type-anno @1.1-1.10 (name "foo")
			(ty @1.7-1.10 (name "Str")))
		(s-decl @2.1-2.12
			(p-ident @2.1-2.4 (raw "foo"))
			(e-string @2.7-2.12
				(e-string-part @2.8-2.11 (raw "one"))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @2.1-2.4 (ident "foo"))
		(e-string @2.7-2.12
			(e-literal @2.8-2.11 (string "one")))
		(annotation @2.1-2.4
			(declared-type
				(ty-lookup @1.7-1.10 (name "Str") (builtin))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @2.1-2.4 (type "Str")))
	(expressions
		(expr @2.7-2.12 (type "Str"))))
~~~
