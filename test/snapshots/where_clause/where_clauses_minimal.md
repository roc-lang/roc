# META
~~~ini
description=Minimal where clause test
type=snippet
~~~
# SOURCE
~~~roc
convert_me : a -> b
	where [a.convert : a -> b]
convert_me = ...
~~~
# EXPECTED
NOT IMPLEMENTED - where_clauses_minimal.md:1:1:1:1
# PROBLEMS
**NOT IMPLEMENTED**
This feature is not yet implemented: ellipsis expression

**where_clauses_minimal.md:1:1:1:1:**
```roc
convert_me : a -> b
```
^

This error doesn't have a proper diagnostic report yet. Let us know if you want to help improve Roc's error messages!


# TOKENS
~~~zig
LowerIdent,OpColon,LowerIdent,OpArrow,LowerIdent,
KwWhere,OpenSquare,LowerIdent,NoSpaceDotLowerIdent,OpColon,LowerIdent,OpArrow,LowerIdent,CloseSquare,
LowerIdent,OpAssign,TripleDot,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-type-anno (name "convert_me")
			(ty-fn
				(ty-var (raw "a"))
				(ty-var (raw "b")))
			(where
				(method (module-of "a") (name "convert")
					(args
						(ty-var (raw "a")))
					(ty-var (raw "b")))))
		(s-decl
			(p-ident (raw "convert_me"))
			(e-ellipsis))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "convert_me"))
		(e-runtime-error (tag "not_implemented"))
		(annotation
			(ty-fn (effectful false)
				(ty-rigid-var (name "a"))
				(ty-rigid-var (name "b")))
			(where
				(method (ty-rigid-var-lookup (ty-rigid-var (name "a"))) (name "convert")
					(args
						(ty-rigid-var-lookup (ty-rigid-var (name "a"))))
					(ty-rigid-var-lookup (ty-rigid-var (name "b"))))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "a -> b where [a.convert : a -> b]")))
	(expressions
		(expr (type "a -> b where [a.convert : a -> b]"))))
~~~
