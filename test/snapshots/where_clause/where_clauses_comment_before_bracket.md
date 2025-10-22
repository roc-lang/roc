# META
~~~ini
description=where_clauses comment before bracket (silently ignored)
type=snippet
~~~
# SOURCE
~~~roc
foo : a
    where # This comment is silently ignored - not preserved in formatted output
        [a.Bar]
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
LowerIdent,OpColon,LowerIdent,
KwWhere,
OpenSquare,LowerIdent,NoSpaceDotUpperIdent,CloseSquare,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-type-anno (name "foo")
			(ty-var (raw "a"))
			(where
				(alias (module-of "a") (name "Bar"))))))
~~~
# FORMATTED
~~~roc
foo : a
	where [
		a.Bar
	]
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(s-type-anno (name "foo")
		(ty-rigid-var (name "a"))
		(where
			(alias (module-of "a") (ident "Bar"))))
	(ext-decl (ident "a.Bar") (kind "type")))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs)
	(expressions))
~~~
