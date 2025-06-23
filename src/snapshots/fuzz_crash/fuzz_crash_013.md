# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
0{
~~~
# PROBLEMS
PARSER: missing_header
**INVALID STATEMENT**
The statement **expr** is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.
# TOKENS
~~~zig
Int(1:1-1:2),OpenCurly(1:2-1:3),EndOfFile(1:3-1:3),
~~~
# PARSE
~~~clojure
(file (1:1-1:3)
	(malformed_header (1:1-1:2) "missing_header")
	(statements (block (1:2-1:3) (statements))))
~~~
# FORMATTED
~~~roc
{}
~~~
# CANONICALIZE
~~~clojure
(can_ir "empty")
~~~
# TYPES
~~~clojure
(inferred_types (defs) (expressions))
~~~