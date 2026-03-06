# META
~~~ini
description=Type alias used in annotation-only declaration should resolve correctly
type=snippet
~~~
# SOURCE
~~~roc
MyType : Str

hey : MyType
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
UpperIdent,OpColon,UpperIdent,
LowerIdent,OpColon,UpperIdent,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-type-decl
			(header (name "MyType")
				(args))
			(ty (name "Str")))
		(s-type-anno (name "hey")
			(ty (name "MyType")))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "hey"))
		(e-anno-only)
		(annotation
			(ty-lookup (name "MyType") (local))))
	(s-alias-decl
		(ty-header (name "MyType"))
		(ty-lookup (name "Str") (builtin))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "MyType")))
	(type_decls
		(alias (type "MyType")
			(ty-header (name "MyType"))))
	(expressions
		(expr (type "MyType"))))
~~~
