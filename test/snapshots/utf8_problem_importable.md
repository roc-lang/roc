# META
~~~ini
description=Utf8Problem type should be directly importable without qualification
type=snippet
~~~
# SOURCE
~~~roc
x : Utf8Problem
x = InvalidStartByte
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
LowerIdent,OpColon,UpperIdent,
LowerIdent,OpAssign,UpperIdent,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-type-anno (name "x")
			(ty (name "Utf8Problem")))
		(s-decl
			(p-ident (raw "x"))
			(e-tag (raw "InvalidStartByte")))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "x"))
		(e-tag (name "InvalidStartByte"))
		(annotation
			(ty-lookup (name "Utf8Problem") (builtin)))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Str.Utf8Problem")))
	(expressions
		(expr (type "Str.Utf8Problem"))))
~~~
