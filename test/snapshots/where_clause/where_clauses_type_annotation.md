# META
~~~ini
description=Simple type annotation with where clause
type=file
~~~
# SOURCE
~~~roc
convert : a -> b where module(a).to_b : a -> b
convert = |a| a.to_b()
~~~
# EXPECTED
MISSING MAIN! FUNCTION - where_clauses_type_annotation.md:1:1:2:23
# PROBLEMS
**MISSING MAIN! FUNCTION**
Default app modules must have a `main!` function.

No `main!` function was found.

Add a main! function like:
`main! = |arg| { ... }`
**where_clauses_type_annotation.md:1:1:2:23:**
```roc
convert : a -> b where module(a).to_b : a -> b
convert = |a| a.to_b()
```


# TOKENS
~~~zig
LowerIdent(1:1-1:8),OpColon(1:9-1:10),LowerIdent(1:11-1:12),OpArrow(1:13-1:15),LowerIdent(1:16-1:17),KwWhere(1:18-1:23),KwModule(1:24-1:30),NoSpaceOpenRound(1:30-1:31),LowerIdent(1:31-1:32),CloseRound(1:32-1:33),NoSpaceDotLowerIdent(1:33-1:38),OpColon(1:39-1:40),LowerIdent(1:41-1:42),OpArrow(1:43-1:45),LowerIdent(1:46-1:47),
LowerIdent(2:1-2:8),OpAssign(2:9-2:10),OpBar(2:11-2:12),LowerIdent(2:12-2:13),OpBar(2:13-2:14),LowerIdent(2:15-2:16),NoSpaceDotLowerIdent(2:16-2:21),NoSpaceOpenRound(2:21-2:22),CloseRound(2:22-2:23),
EndOfFile(3:1-3:1),
~~~
# PARSE
~~~clojure
(file @1.1-2.23
	(type-module @1.1-1.8)
	(statements
		(s-type-anno @1.1-1.47 (name "convert")
			(ty-fn @1.11-1.17
				(ty-var @1.11-1.12 (raw "a"))
				(ty-var @1.16-1.17 (raw "b")))
			(where
				(method @1.24-1.47 (module-of "a") (name "to_b")
					(args
						(ty-var @1.41-1.42 (raw "a")))
					(ty-var @1.46-1.47 (raw "b")))))
		(s-decl @2.1-2.23
			(p-ident @2.1-2.8 (raw "convert"))
			(e-lambda @2.11-2.23
				(args
					(p-ident @2.12-2.13 (raw "a")))
				(e-field-access @2.15-2.23
					(e-ident @2.15-2.16 (raw "a"))
					(e-apply @2.16-2.23
						(e-ident @2.16-2.21 (raw "to_b"))))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @2.1-2.8 (ident "convert"))
		(e-lambda @2.11-2.23
			(args
				(p-assign @2.12-2.13 (ident "a")))
			(e-dot-access @2.15-2.23 (field "to_b")
				(receiver
					(e-lookup-local @2.15-2.16
						(p-assign @2.12-2.13 (ident "a"))))
				(args)))
		(annotation @2.1-2.8
			(declared-type
				(ty-fn @1.11-1.17 (effectful false)
					(ty-var @1.11-1.12 (name "a"))
					(ty-var @1.16-1.17 (name "b"))))))
	(s-type-anno @1.1-1.47 (name "convert")
		(ty-fn @1.11-1.17 (effectful false)
			(ty-var @1.11-1.12 (name "a"))
			(ty-var @1.16-1.17 (name "b")))
		(where
			(method @1.24-1.47 (module-of "a") (ident "to_b")
				(args
					(ty-var @1.41-1.42 (name "a")))
				(ty-var @1.46-1.47 (name "b")))))
	(ext-decl @1.24-1.47 (ident "module(a).to_b") (kind "value")))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @2.1-2.8 (type "a -> b")))
	(expressions
		(expr @2.11-2.23 (type "a -> b"))))
~~~
