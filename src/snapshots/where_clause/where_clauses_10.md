# META
~~~ini
description=where_clauses (10)
type=file
~~~
# SOURCE
~~~roc
module [decode]

import Decode exposing [Decode]

decodeThings # After member name
	: # After colon
		List(List(U8)) -> List(a) # After anno
			where # after where
				module(a).Decode,
~~~
# EXPECTED
EXPOSED BUT NOT DEFINED - where_clauses_10.md:1:9:1:15
# PROBLEMS
**EXPOSED BUT NOT DEFINED**
The module header says that ``decode`` is exposed, but it is not defined anywhere in this module.

**where_clauses_10.md:1:9:1:15:**
```roc
module [decode]
```
        ^^^^^^
You can fix this by either defining ``decode`` in this module, or by removing it from the list of exposed values.

# TOKENS
~~~zig
KwModule(1:1-1:7),OpenSquare(1:8-1:9),LowerIdent(1:9-1:15),CloseSquare(1:15-1:16),
KwImport(3:1-3:7),UpperIdent(3:8-3:14),KwExposing(3:15-3:23),OpenSquare(3:24-3:25),UpperIdent(3:25-3:31),CloseSquare(3:31-3:32),
LowerIdent(5:1-5:13),
OpColon(6:2-6:3),
UpperIdent(7:3-7:7),NoSpaceOpenRound(7:7-7:8),UpperIdent(7:8-7:12),NoSpaceOpenRound(7:12-7:13),UpperIdent(7:13-7:15),CloseRound(7:15-7:16),CloseRound(7:16-7:17),OpArrow(7:18-7:20),UpperIdent(7:21-7:25),NoSpaceOpenRound(7:25-7:26),LowerIdent(7:26-7:27),CloseRound(7:27-7:28),
KwWhere(8:4-8:9),
KwModule(9:5-9:11),NoSpaceOpenRound(9:11-9:12),LowerIdent(9:12-9:13),CloseRound(9:13-9:14),NoSpaceDotUpperIdent(9:14-9:21),Comma(9:21-9:22),EndOfFile(9:22-9:22),
~~~
# PARSE
~~~clojure
(file @1.1-9.22
	(module @1.1-1.16
		(exposes @1.8-1.16
			(exposed-lower-ident (text "decode"))))
	(statements
		(s-import @3.1-3.32 (raw "Decode")
			(exposing
				(exposed-upper-ident (text "Decode"))))
		(s-type-anno @5.1-9.22 (name "decodeThings")
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
				(alias @9.5-9.22 (module-of "a") (name "Decode"))))))
~~~
# FORMATTED
~~~roc
module [decode]

import Decode exposing [Decode]

decodeThings # After member name
	: # After colon
		List(List(U8)) -> List(a) # After anno
 where # after where
			module(a).Decode,
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(s-import @3.1-3.32 (module "Decode")
		(exposes
			(exposed (name "Decode") (wildcard false))))
	(s-type-anno @5.1-9.22 (name "decodeThings")
		(ty-fn @7.3-7.28 (effectful false)
			(ty-apply @7.3-7.17 (symbol "List")
				(ty-apply @7.8-7.16 (symbol "List")
					(ty @7.13-7.15 (name "U8"))))
			(ty-apply @7.21-7.28 (symbol "List")
				(ty-var @7.26-7.27 (name "a"))))
		(where
			(alias @9.5-9.22 (module-of "a") (ident "Decode"))))
	(ext-decl @9.5-9.22 (ident "module(a).Decode") (kind "type")))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs)
	(expressions))
~~~
