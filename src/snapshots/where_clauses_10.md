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
				a.Decode,
~~~
# PROBLEMS
NIL

~~~
# TOKENS
~~~zig
KwModule(1:1-1:7),OpenSquare(1:8-1:9),LowerIdent(1:9-1:15),CloseSquare(1:15-1:16),Newline(1:1-1:1),
Newline(1:1-1:1),
KwImport(3:1-3:7),UpperIdent(3:8-3:14),KwExposing(3:15-3:23),OpenSquare(3:24-3:25),UpperIdent(3:25-3:31),CloseSquare(3:31-3:32),Newline(1:1-1:1),
Newline(1:1-1:1),
LowerIdent(5:1-5:13),Newline(5:15-5:33),
OpColon(6:2-6:3),Newline(6:5-6:17),
UpperIdent(7:3-7:7),NoSpaceOpenRound(7:7-7:8),UpperIdent(7:8-7:12),NoSpaceOpenRound(7:12-7:13),UpperIdent(7:13-7:15),CloseRound(7:15-7:16),CloseRound(7:16-7:17),OpArrow(7:18-7:20),UpperIdent(7:21-7:25),NoSpaceOpenRound(7:25-7:26),LowerIdent(7:26-7:27),CloseRound(7:27-7:28),Newline(7:30-7:41),
KwWhere(8:4-8:9),Newline(8:11-8:23),
LowerIdent(9:5-9:6),NoSpaceDotUpperIdent(9:6-9:13),Comma(9:13-9:14),EndOfFile(9:14-9:14),
~~~
# PARSE
~~~clojure
(file @1-1-9-14
	(module @1-1-1-16
		(exposes @1-8-1-16
			(exposed-lower-ident (text "decode"))))
	(statements
		(s-import @3-1-3-32 (module "Decode")
			(exposing
				(exposed-upper-ident (text "Decode"))))
		(s-type-anno @5-1-9-14 (name "decodeThings")
			(ty-fn @7-3-7-28
				(ty-apply @7-3-7-17
					(ty (name "List"))
					(ty-apply @7-8-7-16
						(ty (name "List"))
						(ty (name "U8"))))
				(ty-apply @7-21-7-28
					(ty (name "List"))
					(ty-var @7-26-7-27 (raw "a")))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(s-import @3-1-3-32 (module "Decode") (id 73)
		(exposes
			(exposed (name "Decode") (wildcard false)))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs)
	(expressions))
~~~