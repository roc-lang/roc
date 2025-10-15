# META
~~~ini
description=where_clauses (4)
type=snippet
~~~
# SOURCE
~~~roc
import Decode exposing [Decode]

decodeThings : List(List(U8)) -> List(a)
	where [a.Decode]
decodeThings = ...
~~~
# EXPECTED
MODULE NOT FOUND - where_clauses_4.md:1:1:1:32
# PROBLEMS
**MODULE NOT FOUND**
The module `Decode` was not found in this Roc project.

You're attempting to use this module here:
**where_clauses_4.md:1:1:1:32:**
```roc
import Decode exposing [Decode]
```
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^


# TOKENS
~~~zig
KwImport(1:1-1:7),UpperIdent(1:8-1:14),KwExposing(1:15-1:23),OpenSquare(1:24-1:25),UpperIdent(1:25-1:31),CloseSquare(1:31-1:32),
LowerIdent(3:1-3:13),OpColon(3:14-3:15),UpperIdent(3:16-3:20),NoSpaceOpenRound(3:20-3:21),UpperIdent(3:21-3:25),NoSpaceOpenRound(3:25-3:26),UpperIdent(3:26-3:28),CloseRound(3:28-3:29),CloseRound(3:29-3:30),OpArrow(3:31-3:33),UpperIdent(3:34-3:38),NoSpaceOpenRound(3:38-3:39),LowerIdent(3:39-3:40),CloseRound(3:40-3:41),
KwWhere(4:2-4:7),OpenSquare(4:8-4:9),LowerIdent(4:9-4:10),NoSpaceDotUpperIdent(4:10-4:17),CloseSquare(4:17-4:18),
LowerIdent(5:1-5:13),OpAssign(5:14-5:15),TripleDot(5:16-5:19),
EndOfFile(6:1-6:1),
~~~
# PARSE
~~~clojure
(file @1.1-5.19
	(type-module @1.1-1.7)
	(statements
		(s-import @1.1-1.32 (raw "Decode")
			(exposing
				(exposed-upper-ident @1.25-1.31 (text "Decode"))))
		(s-type-anno @3.1-4.18 (name "decodeThings")
			(ty-fn @3.16-3.41
				(ty-apply @3.16-3.30
					(ty @3.16-3.20 (name "List"))
					(ty-apply @3.21-3.29
						(ty @3.21-3.25 (name "List"))
						(ty @3.26-3.28 (name "U8"))))
				(ty-apply @3.34-3.41
					(ty @3.34-3.38 (name "List"))
					(ty-var @3.39-3.40 (raw "a"))))
			(where
				(alias @4.9-4.17 (module-of "a") (name "Decode"))))
		(s-decl @5.1-5.19
			(p-ident @5.1-5.13 (raw "decodeThings"))
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
		(p-assign @5.1-5.13 (ident "decodeThings"))
		(e-not-implemented @1.1-1.1)
		(annotation @5.1-5.13
			(declared-type
				(ty-fn @3.16-3.41 (effectful false)
					(ty-apply @3.16-3.30 (name "List") (builtin)
						(ty-apply @3.21-3.29 (name "List") (builtin)
							(ty-lookup @3.26-3.28 (name "U8") (builtin))))
					(ty-apply @3.34-3.41 (name "List") (builtin)
						(ty-rigid-var @3.39-3.40 (name "a")))))))
	(s-import @1.1-1.32 (module "Decode")
		(exposes
			(exposed (name "Decode") (wildcard false))))
	(s-type-anno @3.1-4.18 (name "decodeThings")
		(ty-fn @3.16-3.41 (effectful false)
			(ty-apply @3.16-3.30 (name "List") (builtin)
				(ty-apply @3.21-3.29 (name "List") (builtin)
					(ty-lookup @3.26-3.28 (name "U8") (builtin))))
			(ty-apply @3.34-3.41 (name "List") (builtin)
				(ty-rigid-var @3.39-3.40 (name "a"))))
		(where
			(alias @4.9-4.17 (module-of "a") (ident "Decode"))))
	(ext-decl @4.9-4.17 (ident "a.Decode") (kind "type")))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @5.1-5.13 (type "List(List(Num(Int(Unsigned8)))) -> List(a)")))
	(expressions
		(expr @1.1-1.1 (type "List(List(Num(Int(Unsigned8)))) -> List(a)"))))
~~~
