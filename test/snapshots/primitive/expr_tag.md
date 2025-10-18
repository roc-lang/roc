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
LowerIdent,OpAssign,UpperIdent,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-decl
			(p-ident (raw "foo"))
			(e-tag (raw "FortyTwo")))))
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
		(e-tag (name "FortyTwo"))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "[FortyTwo]_others")))
	(expressions
		(expr (type "[FortyTwo]_others"))))
~~~
