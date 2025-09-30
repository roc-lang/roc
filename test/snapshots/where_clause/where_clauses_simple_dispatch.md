# META
~~~ini
description=Simple where clause with single constraint
type=file
~~~
# SOURCE
~~~roc
stringify : a -> Str where module(a).to_str : a -> Str
stringify = |value| value.to_str()
~~~
# EXPECTED
MISSING MAIN! FUNCTION - where_clauses_simple_dispatch.md:1:1:2:35
# PROBLEMS
**MISSING MAIN! FUNCTION**
Default app modules must have a `main!` function.

No `main!` function was found.

Add a main! function like:
`main! = |arg| { ... }`
**where_clauses_simple_dispatch.md:1:1:2:35:**
```roc
stringify : a -> Str where module(a).to_str : a -> Str
stringify = |value| value.to_str()
```


# TOKENS
~~~zig
LowerIdent(1:1-1:10),OpColon(1:11-1:12),LowerIdent(1:13-1:14),OpArrow(1:15-1:17),UpperIdent(1:18-1:21),KwWhere(1:22-1:27),KwModule(1:28-1:34),NoSpaceOpenRound(1:34-1:35),LowerIdent(1:35-1:36),CloseRound(1:36-1:37),NoSpaceDotLowerIdent(1:37-1:44),OpColon(1:45-1:46),LowerIdent(1:47-1:48),OpArrow(1:49-1:51),UpperIdent(1:52-1:55),
LowerIdent(2:1-2:10),OpAssign(2:11-2:12),OpBar(2:13-2:14),LowerIdent(2:14-2:19),OpBar(2:19-2:20),LowerIdent(2:21-2:26),NoSpaceDotLowerIdent(2:26-2:33),NoSpaceOpenRound(2:33-2:34),CloseRound(2:34-2:35),
EndOfFile(3:1-3:1),
~~~
# PARSE
~~~clojure
(file @1.1-2.35
	(type-module @1.1-1.10)
	(statements
		(s-type-anno @1.1-1.55 (name "stringify")
			(ty-fn @1.13-1.21
				(ty-var @1.13-1.14 (raw "a"))
				(ty @1.18-1.21 (name "Str")))
			(where
				(method @1.28-1.55 (module-of "a") (name "to_str")
					(args
						(ty-var @1.47-1.48 (raw "a")))
					(ty @1.52-1.55 (name "Str")))))
		(s-decl @2.1-2.35
			(p-ident @2.1-2.10 (raw "stringify"))
			(e-lambda @2.13-2.35
				(args
					(p-ident @2.14-2.19 (raw "value")))
				(e-field-access @2.21-2.35
					(e-ident @2.21-2.26 (raw "value"))
					(e-apply @2.26-2.35
						(e-ident @2.26-2.33 (raw "to_str"))))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @2.1-2.10 (ident "stringify"))
		(e-lambda @2.13-2.35
			(args
				(p-assign @2.14-2.19 (ident "value")))
			(e-dot-access @2.21-2.35 (field "to_str")
				(receiver
					(e-lookup-local @2.21-2.26
						(p-assign @2.14-2.19 (ident "value"))))
				(args)))
		(annotation @2.1-2.10
			(declared-type
				(ty-fn @1.13-1.21 (effectful false)
					(ty-var @1.13-1.14 (name "a"))
					(ty @1.18-1.21 (name "Str"))))))
	(s-type-anno @1.1-1.55 (name "stringify")
		(ty-fn @1.13-1.21 (effectful false)
			(ty-var @1.13-1.14 (name "a"))
			(ty @1.18-1.21 (name "Str")))
		(where
			(method @1.28-1.55 (module-of "a") (ident "to_str")
				(args
					(ty-var @1.47-1.48 (name "a")))
				(ty @1.52-1.55 (name "Str")))))
	(ext-decl @1.28-1.55 (ident "module(a).to_str") (kind "value")))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @2.1-2.10 (type "a -> Str")))
	(expressions
		(expr @2.13-2.35 (type "a -> Str"))))
~~~
