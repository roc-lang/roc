# META
~~~ini
description=where_clauses (4)
type=file
~~~
# SOURCE
~~~roc
module [decodeThings]

import Decode exposing [Decode]

decodeThings : List(List(U8)) -> List(a)
	where module(a).Decode
decodeThings = ...
~~~
# EXPECTED
MODULE NOT FOUND - where_clauses_4.md:3:1:3:32
# PROBLEMS
**MODULE NOT FOUND**
The module `Decode` was not found in this Roc project.

You're attempting to use this module here:
**where_clauses_4.md:3:1:3:32:**
```roc
import Decode exposing [Decode]
```
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^


# TOKENS
~~~zig
KwModule(1:1-1:7),OpenSquare(1:8-1:9),LowerIdent(1:9-1:21),CloseSquare(1:21-1:22),
KwImport(3:1-3:7),UpperIdent(3:8-3:14),KwExposing(3:15-3:23),OpenSquare(3:24-3:25),UpperIdent(3:25-3:31),CloseSquare(3:31-3:32),
LowerIdent(5:1-5:13),OpColon(5:14-5:15),UpperIdent(5:16-5:20),NoSpaceOpenRound(5:20-5:21),UpperIdent(5:21-5:25),NoSpaceOpenRound(5:25-5:26),UpperIdent(5:26-5:28),CloseRound(5:28-5:29),CloseRound(5:29-5:30),OpArrow(5:31-5:33),UpperIdent(5:34-5:38),NoSpaceOpenRound(5:38-5:39),LowerIdent(5:39-5:40),CloseRound(5:40-5:41),
KwWhere(6:2-6:7),KwModule(6:8-6:14),NoSpaceOpenRound(6:14-6:15),LowerIdent(6:15-6:16),CloseRound(6:16-6:17),NoSpaceDotUpperIdent(6:17-6:24),
LowerIdent(7:1-7:13),OpAssign(7:14-7:15),TripleDot(7:16-7:19),
EndOfFile(8:1-8:1),
~~~
# PARSE
~~~clojure
(file @1.1-7.19
	(module @1.1-1.22
		(exposes @1.8-1.22
			(exposed-lower-ident @1.9-1.21
				(text "decodeThings"))))
	(statements
		(s-import @3.1-3.32 (raw "Decode")
			(exposing
				(exposed-upper-ident @3.25-3.31 (text "Decode"))))
		(s-type-anno @5.1-6.24 (name "decodeThings")
			(ty-fn @5.16-5.41
				(ty-apply @5.16-5.30
					(ty @5.16-5.20 (name "List"))
					(ty-apply @5.21-5.29
						(ty @5.21-5.25 (name "List"))
						(ty @5.26-5.28 (name "U8"))))
				(ty-apply @5.34-5.41
					(ty @5.34-5.38 (name "List"))
					(ty-var @5.39-5.40 (raw "a"))))
			(where
				(alias @6.8-6.24 (module-of "a") (name "Decode"))))
		(s-decl @7.1-7.19
			(p-ident @7.1-7.13 (raw "decodeThings"))
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
		(p-assign @7.1-7.13 (ident "decodeThings"))
		(e-not-implemented @1.1-1.1)
		(annotation @7.1-7.13
			(declared-type
				(ty-fn @5.16-5.41 (effectful false)
					(ty-apply @5.16-5.30 (name "List") (builtin)
						(ty-apply @5.21-5.29 (name "List") (builtin)
							(ty-lookup @5.26-5.28 (name "U8") (builtin))))
					(ty-apply @5.34-5.41 (name "List") (builtin)
						(ty-rigid-var @5.39-5.40 (name "a")))))))
	(s-import @3.1-3.32 (module "Decode")
		(exposes
			(exposed (name "Decode") (wildcard false))))
	(s-type-anno @5.1-6.24 (name "decodeThings")
		(ty-fn @5.16-5.41 (effectful false)
			(ty-apply @5.16-5.30 (name "List") (builtin)
				(ty-apply @5.21-5.29 (name "List") (builtin)
					(ty-lookup @5.26-5.28 (name "U8") (builtin))))
			(ty-apply @5.34-5.41 (name "List") (builtin)
				(ty-rigid-var @5.39-5.40 (name "a"))))
		(where
			(alias @6.8-6.24 (module-of "a") (ident "Decode"))))
	(ext-decl @6.8-6.24 (ident "module(a).Decode") (kind "type")))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @7.1-7.13 (type "List(List(Num(Int(Unsigned8)))) -> List(a)")))
	(expressions
		(expr @1.1-1.1 (type "List(List(Num(Int(Unsigned8)))) -> List(a)"))))
~~~
