# META
~~~ini
description=Mono test: True tag at top-level
type=mono
~~~
# SOURCE
~~~roc
flag = True
~~~
# MONO
~~~roc
flag : [True, .._others]
flag = True
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
LowerIdent,OpAssign,UpperIdent,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-decl
			(p-ident (raw "flag"))
			(e-tag (raw "True")))))
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "flag"))
		(e-zero-argument-tag (closure "True") (name "True"))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "[True, .._others]")))
	(expressions
		(expr (type "[True, .._others]"))))
~~~
