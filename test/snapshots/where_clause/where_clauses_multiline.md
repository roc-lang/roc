# META
~~~ini
description=Where clause with multiline constraints
type=file
~~~
# SOURCE
~~~roc
process : a, b -> c
	where
		module(a).convert : a -> c,
		module(b).transform : b -> c
process = ...
~~~
# EXPECTED
MISSING MAIN! FUNCTION - where_clauses_multiline.md:1:1:5:14
# PROBLEMS
**MISSING MAIN! FUNCTION**
Default app modules must have a `main!` function.

No `main!` function was found.

Add a main! function like:
`main! = |arg| { ... }`
**where_clauses_multiline.md:1:1:5:14:**
```roc
process : a, b -> c
	where
		module(a).convert : a -> c,
		module(b).transform : b -> c
process = ...
```


# TOKENS
~~~zig
LowerIdent(1:1-1:8),OpColon(1:9-1:10),LowerIdent(1:11-1:12),Comma(1:12-1:13),LowerIdent(1:14-1:15),OpArrow(1:16-1:18),LowerIdent(1:19-1:20),
KwWhere(2:2-2:7),
KwModule(3:3-3:9),NoSpaceOpenRound(3:9-3:10),LowerIdent(3:10-3:11),CloseRound(3:11-3:12),NoSpaceDotLowerIdent(3:12-3:20),OpColon(3:21-3:22),LowerIdent(3:23-3:24),OpArrow(3:25-3:27),LowerIdent(3:28-3:29),Comma(3:29-3:30),
KwModule(4:3-4:9),NoSpaceOpenRound(4:9-4:10),LowerIdent(4:10-4:11),CloseRound(4:11-4:12),NoSpaceDotLowerIdent(4:12-4:22),OpColon(4:23-4:24),LowerIdent(4:25-4:26),OpArrow(4:27-4:29),LowerIdent(4:30-4:31),
LowerIdent(5:1-5:8),OpAssign(5:9-5:10),TripleDot(5:11-5:14),
EndOfFile(6:1-6:1),
~~~
# PARSE
~~~clojure
(file @1.1-5.14
	(type-module @1.1-1.8)
	(statements
		(s-type-anno @1.1-4.31 (name "process")
			(ty-fn @1.11-1.20
				(ty-var @1.11-1.12 (raw "a"))
				(ty-var @1.14-1.15 (raw "b"))
				(ty-var @1.19-1.20 (raw "c")))
			(where
				(method @3.3-3.29 (module-of "a") (name "convert")
					(args
						(ty-var @3.23-3.24 (raw "a")))
					(ty-var @3.28-3.29 (raw "c")))
				(method @4.3-4.31 (module-of "b") (name "transform")
					(args
						(ty-var @4.25-4.26 (raw "b")))
					(ty-var @4.30-4.31 (raw "c")))))
		(s-decl @5.1-5.14
			(p-ident @5.1-5.8 (raw "process"))
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
		(p-assign @5.1-5.8 (ident "process"))
		(e-not-implemented @1.1-1.1)
		(annotation @5.1-5.8
			(declared-type
				(ty-fn @1.11-1.20 (effectful false)
					(ty-var @1.11-1.12 (name "a"))
					(ty-var @1.14-1.15 (name "b"))
					(ty-var @1.19-1.20 (name "c"))))))
	(s-type-anno @1.1-4.31 (name "process")
		(ty-fn @1.11-1.20 (effectful false)
			(ty-var @1.11-1.12 (name "a"))
			(ty-var @1.14-1.15 (name "b"))
			(ty-var @1.19-1.20 (name "c")))
		(where
			(method @3.3-3.29 (module-of "a") (ident "convert")
				(args
					(ty-var @3.23-3.24 (name "a")))
				(ty-var @3.28-3.29 (name "c")))
			(method @4.3-4.31 (module-of "b") (ident "transform")
				(args
					(ty-var @4.25-4.26 (name "b")))
				(ty-var @4.30-4.31 (name "c")))))
	(ext-decl @3.3-3.29 (ident "module(a).convert") (kind "value"))
	(ext-decl @4.3-4.31 (ident "module(b).transform") (kind "value")))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @5.1-5.8 (type "a, b -> c")))
	(expressions
		(expr @1.1-1.1 (type "a, b -> c"))))
~~~
