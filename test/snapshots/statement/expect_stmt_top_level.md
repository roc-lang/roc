# META
~~~ini
description=Debug expression stmt
type=snippet
~~~
# SOURCE
~~~roc
foo = Bool.True

expect foo != Bool.False
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
LowerIdent,OpAssign,UpperIdent,NoSpaceDotUpperIdent,
KwExpect,LowerIdent,OpNotEquals,UpperIdent,NoSpaceDotUpperIdent,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-decl
			(p-ident (raw "foo"))
			(e-tag (raw "Bool.True")))
		(s-expect
			(e-binop (op "!=")
				(e-ident (raw "foo"))
				(e-tag (raw "Bool.False"))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "foo"))
		(e-nominal (nominal "Bool")
			(e-tag (name "True"))))
	(s-expect
		(e-binop (op "ne")
			(e-lookup-local
				(p-assign (ident "foo")))
			(e-nominal (nominal "Bool")
				(e-tag (name "False"))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Bool")))
	(expressions
		(expr (type "Bool"))))
~~~
