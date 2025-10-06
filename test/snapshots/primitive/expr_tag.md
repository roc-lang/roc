# META
~~~ini
description=A primitive
type=snippet
~~~
# SOURCE
~~~roc
foo = FortyTwo
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
LowerIdent(1:1-1:4),OpAssign(1:5-1:6),UpperIdent(1:7-1:15),
EndOfFile(2:1-2:1),
~~~
# PARSE
~~~clojure
(file @1.1-1.15
	(type-module @1.1-1.4)
	(statements
		(s-decl @1.1-1.15
			(p-ident @1.1-1.4 (raw "foo"))
			(e-tag @1.7-1.15 (raw "FortyTwo")))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @1.1-1.4 (ident "foo"))
		(e-tag @1.7-1.15 (name "FortyTwo"))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @1.1-1.4 (type "[FortyTwo]_others")))
	(expressions
		(expr @1.7-1.15 (type "[FortyTwo]_others"))))
~~~
