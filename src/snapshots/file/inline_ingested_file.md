# META
~~~ini
description=inline_ingested_file
type=file
~~~
# SOURCE
~~~roc
module [foo]

import "users.json" as data : Str
import Json

foo = Json.parse(data)
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
KwModule(1:1-1:7),OpenSquare(1:8-1:9),LowerIdent(1:9-1:12),CloseSquare(1:12-1:13),Newline(1:1-1:1),
Newline(1:1-1:1),
KwImport(3:1-3:7),StringStart(3:8-3:9),StringPart(3:9-3:19),StringEnd(3:19-3:20),KwAs(3:21-3:23),LowerIdent(3:24-3:28),OpColon(3:29-3:30),UpperIdent(3:31-3:34),Newline(1:1-1:1),
KwImport(4:1-4:7),UpperIdent(4:8-4:12),Newline(1:1-1:1),
Newline(1:1-1:1),
LowerIdent(6:1-6:4),OpAssign(6:5-6:6),UpperIdent(6:7-6:11),NoSpaceDotLowerIdent(6:11-6:17),NoSpaceOpenRound(6:17-6:18),LowerIdent(6:18-6:22),CloseRound(6:22-6:23),EndOfFile(6:23-6:23),
~~~
# PARSE
~~~clojure
(file @1.1-6.23
	(module @1.1-1.13
		(exposes @1.8-1.13
			(exposed-lower-ident (text "foo"))))
	(statements))
~~~
# FORMATTED
~~~roc
module [foo]
~~~
# CANONICALIZE
~~~clojure
(can-ir (empty true))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs)
	(expressions))
~~~
