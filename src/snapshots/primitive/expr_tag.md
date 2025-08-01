# META
~~~ini
description=A primitive
type=file
~~~
# SOURCE
~~~roc
module [foo]
foo = FortyTwo
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
KwModule(1:1-1:7),OpenSquare(1:8-1:9),LowerIdent(1:9-1:12),CloseSquare(1:12-1:13),
LowerIdent(2:1-2:4),OpAssign(2:5-2:6),UpperIdent(2:7-2:15),EndOfFile(2:15-2:15),
~~~
# PARSE
~~~clojure
(file @1.1-2.15
	(module @1.1-1.13
		(exposes @1.8-1.13
			(exposed-lower-ident @1.9-1.12
				(text "foo"))))
	(statements
		(s-decl @2.1-2.15
			(p-ident @2.1-2.4 (raw "foo"))
			(e-tag @2.7-2.15 (raw "FortyTwo")))))
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
		(e-tag @2.7-2.15 (name "FortyTwo"))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @2.1-2.4 (type "[FortyTwo]_others")))
	(expressions
		(expr @2.7-2.15 (type "[FortyTwo]_others"))))
~~~
