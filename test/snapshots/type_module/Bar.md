# META
~~~ini
description=Valid type module with type alias
type=file
~~~
# SOURCE
~~~roc
Bar : I64
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
UpperIdent(1:1-1:4),OpColon(1:5-1:6),UpperIdent(1:7-1:10),
EndOfFile(2:1-2:1),
~~~
# PARSE
~~~clojure
(file @1.1-1.10
	(type-module @1.1-1.4)
	(statements
		(s-type-decl @1.1-1.10
			(header @1.1-1.4 (name "Bar")
				(args))
			(ty @1.7-1.10 (name "I64")))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(s-alias-decl @1.1-1.10
		(ty-header @1.1-1.4 (name "Bar"))
		(ty @1.7-1.10 (name "I64"))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs)
	(type_decls
		(alias @1.1-1.10 (type "Bar")
			(ty-header @1.1-1.4 (name "Bar"))))
	(expressions))
~~~
