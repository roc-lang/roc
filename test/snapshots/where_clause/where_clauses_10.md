# META
~~~ini
description=where_clauses (10)
type=file:WhereClauses10.roc
~~~
# SOURCE
~~~roc
WhereClauses10 := {}

import Decode exposing [Decode]

decodeThings # After member name
	: # After colon
		List(List(U8)) -> List(a) # After anno
			where # after where
				module(a).Decode
~~~
# EXPECTED
MODULE NOT FOUND - where_clauses_10.md:3:1:3:32
# PROBLEMS
**MODULE NOT FOUND**
The module `Decode` was not found in this Roc project.

You're attempting to use this module here:
**where_clauses_10.md:3:1:3:32:**
```roc
import Decode exposing [Decode]
```
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^


# TOKENS
~~~zig
UpperIdent(1:1-1:15),OpColonEqual(1:16-1:18),OpenCurly(1:19-1:20),CloseCurly(1:20-1:21),
KwImport(3:1-3:7),UpperIdent(3:8-3:14),KwExposing(3:15-3:23),OpenSquare(3:24-3:25),UpperIdent(3:25-3:31),CloseSquare(3:31-3:32),
LowerIdent(5:1-5:13),
OpColon(6:2-6:3),
UpperIdent(7:3-7:7),NoSpaceOpenRound(7:7-7:8),UpperIdent(7:8-7:12),NoSpaceOpenRound(7:12-7:13),UpperIdent(7:13-7:15),CloseRound(7:15-7:16),CloseRound(7:16-7:17),OpArrow(7:18-7:20),UpperIdent(7:21-7:25),NoSpaceOpenRound(7:25-7:26),LowerIdent(7:26-7:27),CloseRound(7:27-7:28),
KwWhere(8:4-8:9),
KwModule(9:5-9:11),NoSpaceOpenRound(9:11-9:12),LowerIdent(9:12-9:13),CloseRound(9:13-9:14),NoSpaceDotUpperIdent(9:14-9:21),
EndOfFile(10:1-10:1),
~~~
# PARSE
~~~clojure
(file @1.1-9.21
	(type-module @1.1-1.15)
	(statements
		(s-type-decl @1.1-1.21
			(header @1.1-1.15 (name "WhereClauses10")
				(args))
			(ty-record @1.19-1.21))
		(s-import @3.1-3.32 (raw "Decode")
			(exposing
				(exposed-upper-ident @3.25-3.31 (text "Decode"))))
		(s-type-anno @5.1-9.21 (name "decodeThings")
			(ty-fn @7.3-7.28
				(ty-apply @7.3-7.17
					(ty @7.3-7.7 (name "List"))
					(ty-apply @7.8-7.16
						(ty @7.8-7.12 (name "List"))
						(ty @7.13-7.15 (name "U8"))))
				(ty-apply @7.21-7.28
					(ty @7.21-7.25 (name "List"))
					(ty-var @7.26-7.27 (raw "a"))))
			(where
				(alias @9.5-9.21 (module-of "a") (name "Decode"))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(s-nominal-decl @1.1-1.21
		(ty-header @1.1-1.15 (name "WhereClauses10"))
		(ty-record @1.19-1.21))
	(s-import @3.1-3.32 (module "Decode")
		(exposes
			(exposed (name "Decode") (wildcard false))))
	(s-type-anno @5.1-9.21 (name "decodeThings")
		(ty-fn @7.3-7.28 (effectful false)
			(ty-apply @7.3-7.17 (name "List") (builtin)
				(ty-apply @7.8-7.16 (name "List") (builtin)
					(ty-lookup @7.13-7.15 (name "U8") (builtin))))
			(ty-apply @7.21-7.28 (name "List") (builtin)
				(ty-rigid-var @7.26-7.27 (name "a"))))
		(where
			(alias @9.5-9.21 (module-of "a") (ident "Decode"))))
	(ext-decl @9.5-9.21 (ident "module(a).Decode") (kind "type")))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs)
	(type_decls
		(nominal @1.1-1.21 (type "WhereClauses10")
			(ty-header @1.1-1.15 (name "WhereClauses10"))))
	(expressions))
~~~
