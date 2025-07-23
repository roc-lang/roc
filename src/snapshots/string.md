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
# EXPECTED
COMPILER DIAGNOSTIC - string.md:0:0:0:0
COMPILER DIAGNOSTIC - string.md:0:0:0:0
# PROBLEMS
**COMPILER DIAGNOSTIC**

**Compiler Diagnostic**
Diagnostic type 'invalid_top_level_statement' is not yet handled in report generation.
**string.md:0:0:0:0**

**COMPILER DIAGNOSTIC**

**Compiler Diagnostic**
Diagnostic type 'invalid_top_level_statement' is not yet handled in report generation.
**string.md:0:0:0:0**

# TOKENS
~~~zig
KwModule(1:1-1:7),OpenSquare(1:8-1:9),CloseSquare(1:9-1:10),
StringStart(3:1-3:2),StringPart(3:2-3:5),StringEnd(3:5-3:6),
StringStart(5:1-5:2),StringPart(5:2-5:5),StringEnd(5:5-5:6),EndOfFile(5:6-5:6),
~~~
# PARSE
~~~clojure
(file @1.1-5.6
	(module @1.1-1.10
		(exposes @1.8-1.10))
	(statements
		(e-string @3.1-3.6
			(e-string-part @3.2-3.5 (raw "one")))
		(e-string @5.1-5.6
			(e-string-part @5.2-5.5 (raw "two")))))
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
