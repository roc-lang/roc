# META
~~~ini
description=two strings
type=file
~~~
# SOURCE
~~~roc
module []

"one"

"two"
~~~
# PROBLEMS
**INVALID STATEMENT**
The statement **expr** is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

**INVALID STATEMENT**
The statement **expr** is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

# TOKENS
~~~zig
KwModule(1:1-1:7),OpenSquare(1:8-1:9),CloseSquare(1:9-1:10),Newline(1:1-1:1),
Newline(1:1-1:1),
StringStart(3:1-3:2),StringPart(3:2-3:5),StringEnd(3:5-3:6),Newline(1:1-1:1),
Newline(1:1-1:1),
StringStart(5:1-5:2),StringPart(5:2-5:5),StringEnd(5:5-5:6),EndOfFile(5:6-5:6),
~~~
# PARSE
~~~clojure
(file @1-1-5-6
	(module @1-1-1-10
		(exposes @1-8-1-10))
	(statements
		(e-string @3-1-3-6
			(e-string-part @3-2-3-5 (raw "one")))
		(e-string @5-1-5-6
			(e-string-part @5-2-5-5 (raw "two")))))
~~~
# FORMATTED
~~~roc
NO CHANGE
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
