# META
~~~ini
description=Test Bool.True == Bool.True equality
type=snippet
~~~
# SOURCE
~~~roc
test = Bool.True == Bool.True
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
LowerIdent,OpAssign,UpperIdent,NoSpaceDotUpperIdent,OpEquals,UpperIdent,NoSpaceDotUpperIdent,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-decl
			(p-ident (raw "test"))
			(e-binop (op "==")
				(e-tag (raw "Bool.True"))
				(e-tag (raw "Bool.True"))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "test"))
		(e-binop (op "eq")
			(e-nominal-external
				(builtin)
				(e-tag (name "True")))
			(e-nominal-external
				(builtin)
				(e-tag (name "True"))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Bool")))
	(expressions
		(expr (type "Bool"))))
~~~
