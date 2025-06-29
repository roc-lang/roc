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
# PROBLEMS
NIL
# TOKENS
~~~zig
KwModule(1:1-1:7),OpenSquare(1:8-1:9),LowerIdent(1:9-1:12),CloseSquare(1:12-1:13),Newline(1:1-1:1),
LowerIdent(2:1-2:4),OpAssign(2:5-2:6),UpperIdent(2:7-2:15),EndOfFile(2:15-2:15),
~~~
# PARSE
~~~clojure
(file @1.1-2.15
	(module @1.1-1.13
		(exposes @1.8-1.13
			(exposed-lower-ident (text "foo"))))
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
	(d-let (id 76)
		(p-assign @2.1-2.4 (ident "foo") (id 73))
		(e-tag @2.7-2.15 (ext-var 0) (name "FortyTwo") (args "TODO") (id 75))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(d_assign (name "foo") (def_var 76) (type "[FortyTwo]*")))
	(expressions
		(expr @2.7-2.15 (type "[FortyTwo]*"))))
~~~
