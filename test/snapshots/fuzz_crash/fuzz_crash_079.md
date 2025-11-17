# META
~~~ini
description=formatter instability with leading newline
type=file
~~~
# SOURCE
~~~roc

b:r
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
LowerIdent,OpColon,LowerIdent,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-type-anno (name "b")
			(ty-var (raw "r")))))
~~~
# FORMATTED
~~~roc

b : r
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "b"))
		(e-anno-only)
		(annotation
			(ty-rigid-var (name "r")))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "r")))
	(expressions
		(expr (type "r"))))
~~~
