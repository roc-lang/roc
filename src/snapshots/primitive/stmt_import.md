# META
~~~ini
description=A primitive
type=file
~~~
# SOURCE
~~~roc
module []

import json.Json [foo, BAR]
~~~
# PROBLEMS
**INVALID STATEMENT**
The statement **expr** is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

# TOKENS
~~~zig
KwModule(1:1-1:7),OpenSquare(1:8-1:9),CloseSquare(1:9-1:10),Newline(1:1-1:1),
Newline(1:1-1:1),
KwImport(3:1-3:7),LowerIdent(3:8-3:12),NoSpaceDotUpperIdent(3:12-3:17),OpenSquare(3:18-3:19),LowerIdent(3:19-3:22),Comma(3:22-3:23),UpperIdent(3:24-3:27),CloseSquare(3:27-3:28),EndOfFile(3:28-3:28),
~~~
# PARSE
~~~clojure
(file @1-1-3-28
	(module @1-1-1-10
		(exposes @1-8-1-10))
	(statements
		(s-import @3-1-3-17 (module ".Json") (qualifier "json"))
		(e-list @3-18-3-28
			(e-ident @3-19-3-22 (qaul "") (raw "foo"))
			(e-tag @3-24-3-27 (raw "BAR")))))
~~~
# FORMATTED
~~~roc
module []

import json.Json[foo, BAR]
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(s-import @3-1-3-17 (module "json.Json") (qualifier "json") (id 72)
		(exposes)))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs)
	(expressions))
~~~
