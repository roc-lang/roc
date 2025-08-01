# META
~~~ini
description=Alias constraint usage in where clauses
type=file
~~~
# SOURCE
~~~roc
module [Sort, sort]

Sort(a) : a
	where module(a).order : (a, a) -> [LT, EQ, GT]

sort : List(elem) -> List(elem) where module(elem).Sort
sort = ...
~~~
# EXPECTED
WHERE CLAUSE NOT ALLOWED IN TYPE DECLARATION - where_clauses_alias_constraint.md:3:1:4:48
# PROBLEMS
**WHERE CLAUSE NOT ALLOWED IN TYPE DECLARATION**
You cannot define a `where` clause inside a type declaration.

You're attempting do this here:
**where_clauses_alias_constraint.md:3:1:4:48:**
```roc
Sort(a) : a
	where module(a).order : (a, a) -> [LT, EQ, GT]
```


# TOKENS
~~~zig
KwModule(1:1-1:7),OpenSquare(1:8-1:9),UpperIdent(1:9-1:13),Comma(1:13-1:14),LowerIdent(1:15-1:19),CloseSquare(1:19-1:20),
UpperIdent(3:1-3:5),NoSpaceOpenRound(3:5-3:6),LowerIdent(3:6-3:7),CloseRound(3:7-3:8),OpColon(3:9-3:10),LowerIdent(3:11-3:12),
KwWhere(4:2-4:7),KwModule(4:8-4:14),NoSpaceOpenRound(4:14-4:15),LowerIdent(4:15-4:16),CloseRound(4:16-4:17),NoSpaceDotLowerIdent(4:17-4:23),OpColon(4:24-4:25),OpenRound(4:26-4:27),LowerIdent(4:27-4:28),Comma(4:28-4:29),LowerIdent(4:30-4:31),CloseRound(4:31-4:32),OpArrow(4:33-4:35),OpenSquare(4:36-4:37),UpperIdent(4:37-4:39),Comma(4:39-4:40),UpperIdent(4:41-4:43),Comma(4:43-4:44),UpperIdent(4:45-4:47),CloseSquare(4:47-4:48),
LowerIdent(6:1-6:5),OpColon(6:6-6:7),UpperIdent(6:8-6:12),NoSpaceOpenRound(6:12-6:13),LowerIdent(6:13-6:17),CloseRound(6:17-6:18),OpArrow(6:19-6:21),UpperIdent(6:22-6:26),NoSpaceOpenRound(6:26-6:27),LowerIdent(6:27-6:31),CloseRound(6:31-6:32),KwWhere(6:33-6:38),KwModule(6:39-6:45),NoSpaceOpenRound(6:45-6:46),LowerIdent(6:46-6:50),CloseRound(6:50-6:51),NoSpaceDotUpperIdent(6:51-6:56),
LowerIdent(7:1-7:5),OpAssign(7:6-7:7),TripleDot(7:8-7:11),EndOfFile(7:11-7:11),
~~~
# PARSE
~~~clojure
(file @1.1-7.11
	(module @1.1-1.20
		(exposes @1.8-1.20
			(exposed-upper-ident @1.9-1.13 (text "Sort"))
			(exposed-lower-ident @1.15-1.19
				(text "sort"))))
	(statements
		(s-type-decl @3.1-4.48
			(header @3.1-3.8 (name "Sort")
				(args
					(ty-var @3.6-3.7 (raw "a"))))
			(ty-var @3.11-3.12 (raw "a"))
			(where
				(method @4.8-4.48 (module-of "a") (name "order")
					(args
						(ty-tuple @4.26-4.32
							(ty-var @4.27-4.28 (raw "a"))
							(ty-var @4.30-4.31 (raw "a"))))
					(ty-tag-union @4.36-4.48
						(tags
							(ty @4.37-4.39 (name "LT"))
							(ty @4.41-4.43 (name "EQ"))
							(ty @4.45-4.47 (name "GT")))))))
		(s-type-anno @6.1-6.56 (name "sort")
			(ty-fn @6.8-6.32
				(ty-apply @6.8-6.18
					(ty @6.8-6.12 (name "List"))
					(ty-var @6.13-6.17 (raw "elem")))
				(ty-apply @6.22-6.32
					(ty @6.22-6.26 (name "List"))
					(ty-var @6.27-6.31 (raw "elem"))))
			(where
				(alias @6.39-6.56 (module-of "elem") (name "Sort"))))
		(s-decl @7.1-7.11
			(p-ident @7.1-7.5 (raw "sort"))
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
		(p-assign @7.1-7.5 (ident "sort"))
		(e-not-implemented @1.1-1.1)
		(annotation @7.1-7.5
			(declared-type
				(ty-fn @6.8-6.32 (effectful false)
					(ty-apply @6.8-6.18 (symbol "List")
						(ty-var @6.13-6.17 (name "elem")))
					(ty-apply @6.22-6.32 (symbol "List")
						(ty-var @6.27-6.31 (name "elem")))))))
	(s-alias-decl @3.1-4.48
		(ty-header @3.1-3.8 (name "sort")
			(ty-args
				(ty-var @3.6-3.7 (name "a"))))
		(ty-var @3.11-3.12 (name "a")))
	(s-type-anno @6.1-6.56 (name "sort")
		(ty-fn @6.8-6.32 (effectful false)
			(ty-apply @6.8-6.18 (symbol "List")
				(ty-var @6.13-6.17 (name "elem")))
			(ty-apply @6.22-6.32 (symbol "List")
				(ty-var @6.27-6.31 (name "elem"))))
		(where
			(alias @6.39-6.56 (module-of "elem") (ident "sort"))))
	(ext-decl @6.39-6.56 (ident "module(elem).Sort") (kind "type")))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @7.1-7.5 (type "Error -> Error")))
	(type_decls
		(alias @3.1-4.48 (type "sort(a)")
			(ty-header @3.1-3.8 (name "sort")
				(ty-args
					(ty-var @3.6-3.7 (name "a"))))))
	(expressions
		(expr @1.1-1.1 (type "Error -> Error"))))
~~~
