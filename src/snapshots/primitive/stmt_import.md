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
**NOT IMPLEMENTED**
This feature is not yet implemented: top-level import

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
(file (1:1-3:28)
	(module (1:1-1:10) (exposes (1:8-1:10)))
	(statements
		(import (3:1-3:17) ".Json" (qualifier "json"))
		(list (3:18-3:28)
			(ident (3:19-3:22) "" "foo")
			(tag (3:24-3:27) "BAR"))))
~~~
# FORMATTED
~~~roc
module []

import json.Json[foo, BAR]
~~~
# CANONICALIZE
~~~clojure
(can_ir "empty")
~~~
# TYPES
~~~clojure
(inferred_types (defs) (expressions))
~~~