# META
~~~ini
description=Mono test: empty list at top-level
type=mono
~~~
# SOURCE
~~~roc
items = []
~~~
# MONO
~~~roc
items : List(_a)
items = []
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
LowerIdent,OpAssign,OpenSquare,CloseSquare,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-decl
			(p-ident (raw "items"))
			(e-list))))
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "items"))
		(e-empty_list)))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "List(_a)")))
	(expressions
		(expr (type "List(_a)"))))
~~~
