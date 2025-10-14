# META
~~~ini
description=where_clauses (10)
type=snippet
~~~
# SOURCE
~~~roc
import Decode exposing [Decode]

decode_things # After member name
	: # After colon
		List(List(U8)) -> List(a) # After anno
			where # after where
				[a.Decode]
~~~
# EXPECTED
MODULE NOT FOUND - where_clauses_10.md:1:1:1:32
# PROBLEMS
**MODULE NOT FOUND**
The module `Decode` was not found in this Roc project.

You're attempting to use this module here:
**where_clauses_10.md:1:1:1:32:**
```roc
import Decode exposing [Decode]
```
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^


# TOKENS
~~~zig
KwImport(1:1-1:7),UpperIdent(1:8-1:14),KwExposing(1:15-1:23),OpenSquare(1:24-1:25),UpperIdent(1:25-1:31),CloseSquare(1:31-1:32),
LowerIdent(3:1-3:14),
OpColon(4:2-4:3),
UpperIdent(5:3-5:7),NoSpaceOpenRound(5:7-5:8),UpperIdent(5:8-5:12),NoSpaceOpenRound(5:12-5:13),UpperIdent(5:13-5:15),CloseRound(5:15-5:16),CloseRound(5:16-5:17),OpArrow(5:18-5:20),UpperIdent(5:21-5:25),NoSpaceOpenRound(5:25-5:26),LowerIdent(5:26-5:27),CloseRound(5:27-5:28),
KwWhere(6:4-6:9),
OpenSquare(7:5-7:6),LowerIdent(7:6-7:7),NoSpaceDotUpperIdent(7:7-7:14),CloseSquare(7:14-7:15),
EndOfFile(8:1-8:1),
~~~
# PARSE
~~~clojure
(file @1.1-7.15
	(type-module @1.1-1.7)
	(statements
		(s-import @1.1-1.32 (raw "Decode")
			(exposing
				(exposed-upper-ident @1.25-1.31 (text "Decode"))))
		(s-type-anno @3.1-7.15 (name "decode_things")
			(ty-fn @5.3-5.28
				(ty-apply @5.3-5.17
					(ty @5.3-5.7 (name "List"))
					(ty-apply @5.8-5.16
						(ty @5.8-5.12 (name "List"))
						(ty @5.13-5.15 (name "U8"))))
				(ty-apply @5.21-5.28
					(ty @5.21-5.25 (name "List"))
					(ty-var @5.26-5.27 (raw "a"))))
			(where
				(alias @7.6-7.14 (module-of "a") (name "Decode"))))))
~~~
# FORMATTED
~~~roc
import Decode exposing [Decode]

decode_things # After member name
	: # After colon
		List(List(U8)) -> List(a) # After anno
			where
				[a.Decode]
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(s-import @1.1-1.32 (module "Decode")
		(exposes
			(exposed (name "Decode") (wildcard false))))
	(s-type-anno @3.1-7.15 (name "decode_things")
		(ty-fn @5.3-5.28 (effectful false)
			(ty-apply @5.3-5.17 (name "List") (builtin)
				(ty-apply @5.8-5.16 (name "List") (builtin)
					(ty-lookup @5.13-5.15 (name "U8") (builtin))))
			(ty-apply @5.21-5.28 (name "List") (builtin)
				(ty-rigid-var @5.26-5.27 (name "a"))))
		(where
			(alias @7.6-7.14 (module-of "a") (ident "Decode"))))
	(ext-decl @7.6-7.14 (ident "a.Decode") (kind "type")))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs)
	(expressions))
~~~
