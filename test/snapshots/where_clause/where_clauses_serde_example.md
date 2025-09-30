# META
~~~ini
description=Module dispatch in where clause
type=file
~~~
# SOURCE
~~~roc
deserialize : List(U8) -> Result(a, [DecodeErr])
	where module(a).decode : List(U8) -> Result(a, [DecodeErr])
deserialize = |_| ...
~~~
# EXPECTED
MISSING MAIN! FUNCTION - where_clauses_serde_example.md:1:1:3:22
# PROBLEMS
**MISSING MAIN! FUNCTION**
Default app modules must have a `main!` function.

No `main!` function was found.

Add a main! function like:
`main! = |arg| { ... }`
**where_clauses_serde_example.md:1:1:3:22:**
```roc
deserialize : List(U8) -> Result(a, [DecodeErr])
	where module(a).decode : List(U8) -> Result(a, [DecodeErr])
deserialize = |_| ...
```


# TOKENS
~~~zig
LowerIdent(1:1-1:12),OpColon(1:13-1:14),UpperIdent(1:15-1:19),NoSpaceOpenRound(1:19-1:20),UpperIdent(1:20-1:22),CloseRound(1:22-1:23),OpArrow(1:24-1:26),UpperIdent(1:27-1:33),NoSpaceOpenRound(1:33-1:34),LowerIdent(1:34-1:35),Comma(1:35-1:36),OpenSquare(1:37-1:38),UpperIdent(1:38-1:47),CloseSquare(1:47-1:48),CloseRound(1:48-1:49),
KwWhere(2:2-2:7),KwModule(2:8-2:14),NoSpaceOpenRound(2:14-2:15),LowerIdent(2:15-2:16),CloseRound(2:16-2:17),NoSpaceDotLowerIdent(2:17-2:24),OpColon(2:25-2:26),UpperIdent(2:27-2:31),NoSpaceOpenRound(2:31-2:32),UpperIdent(2:32-2:34),CloseRound(2:34-2:35),OpArrow(2:36-2:38),UpperIdent(2:39-2:45),NoSpaceOpenRound(2:45-2:46),LowerIdent(2:46-2:47),Comma(2:47-2:48),OpenSquare(2:49-2:50),UpperIdent(2:50-2:59),CloseSquare(2:59-2:60),CloseRound(2:60-2:61),
LowerIdent(3:1-3:12),OpAssign(3:13-3:14),OpBar(3:15-3:16),Underscore(3:16-3:17),OpBar(3:17-3:18),TripleDot(3:19-3:22),
EndOfFile(4:1-4:1),
~~~
# PARSE
~~~clojure
(file @1.1-3.22
	(type-module @1.1-1.12)
	(statements
		(s-type-anno @1.1-2.61 (name "deserialize")
			(ty-fn @1.15-1.49
				(ty-apply @1.15-1.23
					(ty @1.15-1.19 (name "List"))
					(ty @1.20-1.22 (name "U8")))
				(ty-apply @1.27-1.49
					(ty @1.27-1.33 (name "Result"))
					(ty-var @1.34-1.35 (raw "a"))
					(ty-tag-union @1.37-1.48
						(tags
							(ty @1.38-1.47 (name "DecodeErr"))))))
			(where
				(method @2.8-2.61 (module-of "a") (name "decode")
					(args
						(ty-apply @2.27-2.35
							(ty @2.27-2.31 (name "List"))
							(ty @2.32-2.34 (name "U8"))))
					(ty-apply @2.39-2.61
						(ty @2.39-2.45 (name "Result"))
						(ty-var @2.46-2.47 (raw "a"))
						(ty-tag-union @2.49-2.60
							(tags
								(ty @2.50-2.59 (name "DecodeErr"))))))))
		(s-decl @3.1-3.22
			(p-ident @3.1-3.12 (raw "deserialize"))
			(e-lambda @3.15-3.22
				(args
					(p-underscore))
				(e-ellipsis)))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @3.1-3.12 (ident "deserialize"))
		(e-lambda @3.15-3.22
			(args
				(p-underscore @3.16-3.17))
			(e-not-implemented @1.1-1.1))
		(annotation @3.1-3.12
			(declared-type
				(ty-fn @1.15-1.49 (effectful false)
					(ty-apply @1.15-1.23 (symbol "List")
						(ty @1.20-1.22 (name "U8")))
					(ty-apply @1.27-1.49 (symbol "Result")
						(ty-var @1.34-1.35 (name "a"))
						(ty-tag-union @1.37-1.48
							(ty @1.38-1.47 (name "DecodeErr"))))))))
	(s-type-anno @1.1-2.61 (name "deserialize")
		(ty-fn @1.15-1.49 (effectful false)
			(ty-apply @1.15-1.23 (symbol "List")
				(ty @1.20-1.22 (name "U8")))
			(ty-apply @1.27-1.49 (symbol "Result")
				(ty-var @1.34-1.35 (name "a"))
				(ty-tag-union @1.37-1.48
					(ty @1.38-1.47 (name "DecodeErr")))))
		(where
			(method @2.8-2.61 (module-of "a") (ident "decode")
				(args
					(ty-apply @2.27-2.35 (symbol "List")
						(ty @2.32-2.34 (name "U8"))))
				(ty-apply @2.39-2.61 (symbol "Result")
					(ty-var @2.46-2.47 (name "a"))
					(ty-tag-union @2.49-2.60
						(ty @2.50-2.59 (name "DecodeErr")))))))
	(ext-decl @2.8-2.61 (ident "module(a).decode") (kind "value")))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @3.1-3.12 (type "List(U8) -> Result(a, [DecodeErr])")))
	(expressions
		(expr @3.15-3.22 (type "List(U8) -> Result(a, [DecodeErr])"))))
~~~
