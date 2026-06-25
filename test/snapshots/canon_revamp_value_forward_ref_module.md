# META
~~~ini
description=Top-level value forward-references another top-level value
type=snippet
~~~
# SOURCE
~~~roc
foo = bar
bar = 42
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
LowerIdent,OpAssign,LowerIdent,
LowerIdent,OpAssign,Int,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-decl
			(p-ident (raw "foo"))
			(e-ident (raw "bar")))
		(s-decl
			(p-ident (raw "bar"))
			(e-int (raw "42")))))
~~~
# FORMATTED
~~~roc
foo = bar

bar = 42
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "foo"))
		(e-lookup-local
			(p-assign (ident "bar"))))
	(d-let
		(p-assign (ident "bar"))
		(e-num (value "42"))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Dec"))
		(patt (type "Dec")))
	(expressions
		(expr (type "Dec"))
		(expr (type "Dec"))))
~~~
